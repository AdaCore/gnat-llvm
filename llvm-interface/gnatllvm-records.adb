------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Interfaces.C;            use Interfaces.C;

with Get_Targ; use Get_Targ;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Sem_Aux;  use Sem_Aux;
with Snames;   use Snames;
with Stand;    use Stand;
with Table;    use Table;

with GNATLLVM.DebugInfo;   use GNATLLVM.DebugInfo;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package body GNATLLVM.Records is

   function Count_Entities (E : Entity_Id) return Nat
     with Pre => Present (E);
   --  Return the number of entities of E.  This value will be used only
   --  to allocate an array that we know is large enough to contain all
   --  the fields, so we can overestimate the number of fields (even
   --  greatly), but can't underestimate.

   function Get_Record_Size_So_Far
     (TE       : Entity_Id;
      V        : GL_Value;
      Idx      : Record_Info_Id;
      For_Type : Boolean := False) return GL_Value
     with Pre  => Present (TE),
          Post => Present (Get_Record_Size_So_Far'Result);

   --  Similar to Get_Record_Type_Size, but stop at record info segment Idx
   --  or the last segment, whichever comes first.

   --  We represent a record by one or more pieces of information
   --  describing the record.  Each piece points to the next piece, if
   --  any.  For non-variant records, each piece either contains an
   --  LLVM type, which contains one or more fields or an GNAT type,
   --  which is used when the field's type is of dynamic size.
   --  In the case where a record's type is a record type (as opposed to
   --  a record subtype), it means that we are to use the maximum type of
   --  that size for allocation purpose, so we need to flag that here.

   type Record_Info is record
      LLVM_Type    : Type_T;
      GNAT_Type    : Entity_Id;
      Next         : Record_Info_Id;
      Use_Max_Size : Boolean;
   end record
     with Dynamic_Predicate => (Present (LLVM_Type)
                                  or else Present (GNAT_Type))
                               and then not (Present (LLVM_Type)
                                               and then Present (GNAT_Type));

   package Record_Info_Table is new Table.Table
     (Table_Component_Type => Record_Info,
      Table_Index_Type     => Record_Info_Id'Base,
      Table_Low_Bound      => Record_Info_Low_Bound,
      Table_Initial        => 100,
      Table_Increment      => 50,
      Table_Name           => "Record_Info_Table");

   --  The information for a field is the index of the piece in the
   --  record information and optionally the index within the piece in the
   --  case when its an LLVM_type.

   type Field_Info is record
      Rec_Info_Idx  : Record_Info_Id;
      Field_Ordinal : Nat;
   end record;

   package Field_Info_Table is new Table.Table
     (Table_Component_Type => Field_Info,
      Table_Index_Type     => Field_Info_Id'Base,
      Table_Low_Bound      => Field_Info_Low_Bound,
      Table_Initial        => 1000,
      Table_Increment      => 100,
      Table_Name           => "Record_Info_Table");

   ---------------------
   --  Count_Entities --
   ---------------------

   function Count_Entities (E : Entity_Id) return Nat is
      Count   : Nat := 0;
      Elmt    : Entity_Id := First_Field (E);

   begin
      while Present (Elmt) loop
         Count := Count + (if Chars (Elmt) = Name_uParent
                              then Count_Entities (Full_Etype (Elmt)) else 1);
         Next_Field (Elmt);
      end loop;

      return Count;
   end Count_Entities;

   ------------------------
   -- Create_Record_Type --
   ------------------------

   function Create_Record_Type (TE : Entity_Id) return Type_T is
      Prev_Idx  : Record_Info_Id := Empty_Record_Info_Id;
      --  The previous index of the record table entry, if any

      Cur_Idx   : Record_Info_Id;
      --  The index of the record table entry we're building

      Types     : Type_Array (0 .. Count_Entities (TE));
      --  Array of all field types that are going into the current piece

      Next_Type : Nat := 0;
      --  Ordinal of next entry in Types

      Cur_Field : Entity_Id := First_Field (TE);
      --  The current field on the entity chain that we're processing
      --  to set the field info when we have a parent record.

      LLVM_Type : Type_T;

      procedure Add_RI
        (T            : Type_T := No_Type_T;
         TE           : Entity_Id := Empty;
         Use_Max_Size : Boolean := False)
        with Pre => (Present (T) or else Present (TE))
                    and then not (Present (T) and then Present (TE));
      --  Add a Record_Info into the table, chaining it as appropriate

      procedure Add_FI (E : Entity_Id; RI_Idx : Record_Info_Id; Ordinal : Nat)
        with Pre => Ekind_In (E, E_Discriminant, E_Component);
      --  Add a Field_Info info the table, if appropriate, and set
      --  the field to point to it.

      procedure Add_Field (E : Entity_Id)
        with Pre => Ekind_In (E, E_Discriminant, E_Component);
      --  Add one field to the above data

      procedure Add_Fields (Def_Ident : Entity_Id)
        with Pre => Is_Record_Type (Def_Ident);
      --  Add all fields of Def_Ident to the above data, either the component
      --  or the extension components, but recursively add parent components.

      ------------
      -- Add_RI --
      ------------

      procedure Add_RI
        (T            : Type_T := No_Type_T;
         TE           : Entity_Id := Empty;
         Use_Max_Size : Boolean := False) is
      begin
         --  It's tempting to set Next to the next entry that we'll be using,
         --  but we may not actually be using that one.

         Record_Info_Table.Table (Cur_Idx) :=
           (LLVM_Type    => T, GNAT_Type => TE,
            Next         => Empty_Record_Info_Id,
            Use_Max_Size => Use_Max_Size);

         if Present (Prev_Idx) then
            Record_Info_Table.Table (Prev_Idx).Next := Cur_Idx;
         end if;

         Prev_Idx := Cur_Idx;
         Record_Info_Table.Increment_Last;
         Cur_Idx := Record_Info_Table.Last;
      end Add_RI;

      ------------
      -- Add_FI --
      ------------

      procedure Add_FI
        (E : Entity_Id; RI_Idx : Record_Info_Id; Ordinal : Nat) is
         Initial_Cur_Field : constant Entity_Id := Cur_Field;
      begin
         --  If this field really isn't in the record we're working on, it
         --  must be in a parent.  So it was correct to allocate space for
         --  it, but let the record description be from the type that it's
         --  actually in.  The fields in the entity list for this type are
         --  almost, but not quite, win the same order as in the component
         --  list, so we have to search for a field in that list with the
         --  same Original_Record_Component as this field.  This is
         --  potentially quadratic in the number of fields, but we can
         --  optimize by caching where we last searched in Cur_Field
         --  of Create_Record_Type.

         Field_Info_Table.Append ((Rec_Info_Idx => RI_Idx,
                                   Field_Ordinal => Ordinal));
         if Full_Scope (E) = TE then
            Set_Field_Info (E, Field_Info_Table.Last);
         end if;

         --  Look from Cur_Field until the end of the list.  Then look from
         --  the beginning to its previous value.

         while Present (Cur_Field) loop
            if Original_Record_Component (Cur_Field) =
              Original_Record_Component (E)
            then
               Set_Field_Info (Cur_Field, Field_Info_Table.Last);
               return;
            end if;

            Next_Field (Cur_Field);
         end loop;

         Cur_Field := First_Field (TE);
         while Cur_Field /= Initial_Cur_Field loop
            if Original_Record_Component (Cur_Field) =
              Original_Record_Component (E)
            then
               Set_Field_Info (Cur_Field, Field_Info_Table.Last);
               return;
            end if;

            Next_Field (Cur_Field);
         end loop;
      end Add_FI;

      ----------------
      -- Add_Fields --
      ----------------

      procedure Add_Fields (Def_Ident : Entity_Id) is

         procedure Add_Component_List (List : Node_Id)
           with Pre => No (List) or else Nkind (List) = N_Component_List;
         --  Add all fields in List

         ------------------------
         -- Add_Component_List --
         ------------------------

         procedure Add_Component_List (List : Node_Id) is
            Component_Def : Node_Id;
            Component_Ent : Entity_Id;
            Variant       : Node_Id;

         begin
            if No (List) then
               return;
            end if;

            Component_Def := First_Non_Pragma (Component_Items (List));
            while Present (Component_Def) loop
               Component_Ent := Defining_Identifier (Component_Def);
               if Chars (Component_Ent) = Name_uParent then
                  Add_Fields (Full_Etype (Component_Ent));
               end if;

               Add_Field (Component_Ent);
               Next_Non_Pragma (Component_Def);
            end loop;

            --  Now process variants.  For now, just add them as
            --  regular fields.

            if Present (Variant_Part (List)) then
               Variant := First (Variants (Variant_Part (List)));
               while Present (Variant) loop
                  Add_Component_List (Component_List (Variant));
                  Next (Variant);
               end loop;
            end if;
         end Add_Component_List;

         Field             : Entity_Id;
         Record_Definition : Node_Id;

      --  Start of processing for Add_Fields

      begin

         --  If this is a subtype, we make fields from the entity chain.
         --  Otherwise, we walk the definition.

         if Ekind_In (Def_Ident, E_Record_Subtype, E_Class_Wide_Subtype) then
            Field := First_Field (Def_Ident);
            while Present (Field) loop
               Add_Field (Field);
               Next_Field (Field);
            end loop;

         else
            --  If there are discriminants, process them first

            if Has_Discriminants (Def_Ident) then
               Field := First_Stored_Discriminant (Def_Ident);
               while Present (Field) loop
                  Add_Field (Field);
                  Next_Stored_Discriminant (Field);
               end loop;
            end if;

               --  Now get the record definition and add the components there

            Record_Definition :=
              Type_Definition (Declaration_Node (Def_Ident));
            if Nkind (Record_Definition) = N_Derived_Type_Definition then
               Record_Definition := Record_Extension_Part (Record_Definition);
            end if;

            Add_Component_List (Component_List (Record_Definition));
         end if;

      end Add_Fields;

      ---------------
      -- Add_Field --
      ---------------

      procedure Add_Field (E : Entity_Id) is
         Typ : constant Entity_Id := Full_Etype (E);

      begin
         --  If this is the '_parent' field, we make a dummy entry and handle
         --  it specially later.

         if Chars (E) = Name_uParent then
            Add_FI (E, Get_Record_Info (TE), 0);
            return;

         --  If this field is dynamic size, we have to close out the last
         --  record info entry we're making, if there's anything in it
         --  and make a piece for this field.

         elsif Is_Dynamic_Size (Typ) then
            if Next_Type /= 0 then
               Add_RI (T => Build_Struct_Type (Types (0 .. Next_Type - 1)));
               Next_Type := 0;
            end if;

            Add_FI (E, Cur_Idx, 0);
            Add_RI (TE => Typ, Use_Max_Size => not Is_Constrained (Typ));

         --  If it's of fixed size, add it to the current set of fields
         --  and make a field descriptor.

         else

            Add_FI (E, Cur_Idx, Next_Type);
            Types (Next_Type) := Create_Type (Typ);
            Next_Type := Next_Type + 1;
         end if;

      end Add_Field;

   --  Start of processing for Create_Record_Type

   begin
      --  Because of the potential recursion between record and access types,
      --  make a dummy type for us and set it as our type right at the start.
      --  Then initialize our first record info table entry, which we know
      --  will be used.

      LLVM_Type := Struct_Create_Named (LLVM_Context, Get_Name (TE));
      Set_Type (TE, LLVM_Type);
      Record_Info_Table.Increment_Last;
      Cur_Idx := Record_Info_Table.Last;
      Set_Record_Info (TE, Cur_Idx);
      Add_Fields (TE);

      --  If we haven't yet made any record info entries, it means that
      --  this is a fixed-size record that can be just an LLVM type,
      --  so use the one we made.

      if No (Prev_Idx) then
         Struct_Set_Body (LLVM_Type, Types'Address,
                          unsigned (Next_Type), False);
         Add_RI (T => LLVM_Type);

      else
         --  Otherwise, close out the last record info if we have any
         --  fields and show this record is of dynamic size.  Note thast if
         --  we don't have any fields, the entry we allocated will remain
         --  unused, but trying to reclaim it is risky.

         if Next_Type /= 0 then
            Add_RI (T => Build_Struct_Type (Types (0 .. Next_Type - 1)));
         end if;

         Set_Dynamic_Size (TE, True);
      end if;

      return LLVM_Type;
   end Create_Record_Type;

   ----------------------------
   -- Get_Record_Size_So_Far --
   ----------------------------

   function Get_Record_Size_So_Far
     (TE       : Entity_Id;
      V        : GL_Value;
      Idx      : Record_Info_Id;
      For_Type : Boolean := False) return GL_Value
   is
      Total_Size : GL_Value := Size_Const_Null;
      Cur_Align  : unsigned := unsigned (Get_Maximum_Alignment);
      Cur_Idx    : Record_Info_Id := Get_Record_Info (TE);
      RI         : Record_Info;
      This_Size  : GL_Value;
      This_Align : unsigned;

   begin
      Push_Debug_Freeze_Pos;

      --  If we're passed V, add it to the list that Get_Matching_Value
      --  will search if we run into a discriminant in one of the computations
      --  below.

      if Present (V) then
         Add_To_LValue_List (V);
      end if;

      --  Look at each piece of the record and find its value and alignment

      while Present (Cur_Idx) and then Cur_Idx /= Idx loop
         RI := Record_Info_Table.Table (Cur_Idx);
         if Present (RI.LLVM_Type) then
            This_Size  := Get_LLVM_Type_Size (RI.LLVM_Type);
            This_Align := Get_Type_Alignment (RI.LLVM_Type);
         else
            This_Size  := Get_Type_Size (RI.GNAT_Type, V,
                                         For_Type or RI.Use_Max_Size);
            This_Align := Get_Type_Alignment (RI.GNAT_Type);
         end if;

         --  We have to align the size so far to the alignment of
         --  this type.

         if This_Align > Cur_Align then
            Total_Size := Build_And
              (NSW_Add (Total_Size, Size_Const_Int (This_Align - 1)),
               NSW_Neg (Size_Const_Int (This_Align)));
         end if;

         Total_Size := NSW_Add (Total_Size, This_Size);
         Cur_Align  := This_Align;
         Cur_Idx    := RI.Next;
      end loop;

      Pop_Debug_Freeze_Pos;
      return Total_Size;
   end Get_Record_Size_So_Far;

   ----------------------
   -- Get_Field_Offset --
   ----------------------

   function Get_Field_Offset (T : Type_T; Idx : Nat) return GL_Value is

      --  We do this at a low level because we don't always have GNAT types
      --  corresponding to the LLVM type T.  GEP on records is defined
      --  as being passed 32-bit indices.

      Int_32_T   : constant Type_T      := Int_Ty (32);
      Const_0    : constant Value_T     := Const_Int (Int_32_T, 0, False);
      Const_Idx  : constant Value_T     :=
        Const_Int (Int_32_T, unsigned_long_long (Idx), False);
      Idxs       : constant Value_Array := (1 => Const_0, 2 => Const_Idx);
      Null_Val   : constant Value_T     := Const_Null (Pointer_Type (T, 0));
      GEP_Result : constant Value_T     :=
        In_Bounds_GEP (IR_Builder, Null_Val, Idxs'Address, 2, "");

   begin
      return G (Ptr_To_Int (IR_Builder, GEP_Result, LLVM_Size_Type, ""),
                Size_Type);

   end Get_Field_Offset;

   -------------------------
   -- Emit_Field_Position --
   -------------------------

   function Emit_Field_Position
     (E : Entity_Id; V : GL_Value) return GL_Value
   is
      TE     : constant Entity_Id      := Full_Scope (E);
      F_Idx  : constant Field_Info_Id  := Get_Field_Info (E);
      FI     : constant Field_Info     := Field_Info_Table.Table (F_Idx);
      Idx    : constant Record_Info_Id := FI.Rec_Info_Idx;
      RI     : constant Record_Info    := Record_Info_Table.Table (Idx);
      Offset : constant GL_Value       := Get_Record_Size_So_Far (TE, V, Idx);

   begin
      --  Offset now gives the offset from the start of the record to the
      --  piece that this field is in.  If this piece has a GNAT type, then
      --  the field is the entire piece and we have the offset.  If it's an
      --  LLVM type, we need to compute the offset within that type.

      if Present (RI.GNAT_Type) then
         return Offset;
      else
         return NSW_Add (Offset,
                         Get_Field_Offset (RI.LLVM_Type, FI.Field_Ordinal));
      end if;
   end Emit_Field_Position;

   -------------------------
   -- Record_Field_Offset --
   -------------------------

   function Record_Field_Offset
     (V : GL_Value; Field : Entity_Id) return GL_Value
   is
      Rec_Type   : constant Entity_Id      := Full_Scope (Field);
      F_Type     : constant Entity_Id      := Full_Etype (Field);
      First_Idx  : constant Record_Info_Id := Get_Record_Info (Rec_Type);
      FI         : constant Field_Info     :=
        Field_Info_Table.Table (Get_Field_Info (Field));
      Our_Idx    : constant Record_Info_Id := FI.Rec_Info_Idx;
      Offset     : constant GL_Value       :=
        Get_Record_Size_So_Far (Rec_Type, V, Our_Idx);
      RI         : constant Record_Info    :=
        Record_Info_Table.Table (Our_Idx);
      Result     : GL_Value;

   begin

      --  If this is the "_parent" field, just do a conversion so we point
      --  to that type.  But add it to the LValue table in case there's
      --  a reference to its discrminant.

      if Chars (Field) = Name_uParent then
         Result := Ptr_To_Ref (V, F_Type);
         Add_To_LValue_List (Result);
         return Result;

      --  If the current piece is for a variable-sized object, we offset
      --  to that object and make a pointer to its type.  Otherwise,
      --  make sure we're pointing to Rec_Type.

      elsif Present (RI.GNAT_Type) then
         return Ptr_To_Ref (GEP (Standard_Short_Short_Integer,
                                 Pointer_Cast (V, Standard_A_Char),
                                 (1 => Offset)),
                            F_Type);
      end if;

      --  Otherwise, if this is not the first piece, we have to offset to
      --  the field (in bytes).

      if Our_Idx = First_Idx then
         Result := V;
      else
         Result := GEP (Standard_Short_Short_Integer,
                        Pointer_Cast (V, Standard_A_Char),
                        (1 => Offset));
      end if;

      --  If the type is dynamic size, we have to convert the pointer to
      --  the type of this piece (which has no corresponding GNAT type.)

      if Is_Dynamic_Size (Rec_Type) then
         Result := G_Ref (Pointer_Cast (IR_Builder, LLVM_Value (Result),
                                        Pointer_Type (RI.LLVM_Type, 0), ""),
                          Rec_Type);
      else
         Result := Convert_To_Access_To (Result, Rec_Type);
      end if;

      --  Finally, do a regular GEP for the field and we're done

      return GEP (F_Type, Result,
                  (1 => Const_Null_32,
                   2 => Const_Int_32 (unsigned (FI.Field_Ordinal))));

   end Record_Field_Offset;

   --------------------------------
   -- Get_Record_Size_Complexity --
   --------------------------------

   function Get_Record_Size_Complexity
     (TE : Entity_Id; For_Type : Boolean := False) return Natural
   is
      Complexity : Natural        := 0;
      Cur_Idx    : Record_Info_Id := Get_Record_Info (TE);
      RI         : Record_Info;

   begin
      while Present (Cur_Idx) loop
         RI := Record_Info_Table.Table (Cur_Idx);
         if Present (RI.GNAT_Type) then
            Complexity := Complexity + Get_Type_Size_Complexity
              (RI.GNAT_Type, For_Type or RI.Use_Max_Size);
         end if;

         Cur_Idx := RI.Next;
      end loop;

      return Complexity;
   end Get_Record_Size_Complexity;

   --------------------------
   -- Get_Record_Type_Size --
   --------------------------

   function Get_Record_Type_Size
     (TE       : Entity_Id;
      V        : GL_Value;
      For_Type : Boolean := False) return GL_Value is
   begin
      return Get_Record_Size_So_Far (TE, V, Empty_Record_Info_Id, For_Type);
   end Get_Record_Type_Size;

   ---------------------------
   -- Emit_Record_Aggregate --
   ---------------------------

   function Emit_Record_Aggregate
     (Node : Node_Id; Result_So_Far : GL_Value) return GL_Value
   is
      Result     : GL_Value := Result_So_Far;
      Agg_Type   : constant Entity_Id := Full_Etype (Node);
      Expr       : Node_Id;

      function Find_Matching_Field
        (TE : Entity_Id; Field : Entity_Id) return Entity_Id
      with Pre  => Is_Record_Type (TE)
                   and then Ekind_In (Field, E_Discriminant, E_Component),
           Post => Original_Record_Component (Field) =
                     Original_Record_Component (Find_Matching_Field'Result);
      --  Find a field corresponding to Fld in record type TE

      -------------------------
      -- Find_Matching_Field --
      -------------------------

      function Find_Matching_Field
        (TE : Entity_Id; Field : Entity_Id) return Entity_Id
      is
         ORC : constant Entity_Id := Original_Record_Component (Field);
         Ent : Entity_Id := First_Field (TE);

      begin
         while Present (Ent) loop
            if Original_Record_Component (Ent) = ORC then
               return Ent;
            end if;

            Next_Field (Ent);
         end loop;

         return Empty;
      end Find_Matching_Field;

   begin
      pragma Assert (not Is_Dynamic_Size (Agg_Type));

      --  The above assertion proved that Agg_Type is of fixed size.  This
      --  means that each of its components must be just a simple component
      --  into an LLVM structure, so we just go through each of the part of
      --  the aggregate and use the offset for that field, skipping
      --  a discriminant of an unchecked union.

      Expr := First (Component_Associations (Node));
      while Present (Expr) loop
         declare
            Ent    : constant Entity_Id     :=
              Find_Matching_Field (Agg_Type, Entity (First (Choices (Expr))));
            F_Type : constant Entity_Id     := Full_Etype (Ent);
            F_Idx  : constant Field_Info_Id := Get_Field_Info (Ent);
            F_Info : constant Field_Info    := Field_Info_Table.Table (F_Idx);

         begin
            if Ekind (Ent) = E_Discriminant
              and then Is_Unchecked_Union (Agg_Type)
            then
               null;
            elsif Chars (Ent) = Name_uParent then

               --  If this is "_parent", its fields are our fields too.
               --  Assume Expression is also an N_Aggregate.

               pragma Assert (Nkind_In (Expression (Expr),
                                        N_Aggregate, N_Extension_Aggregate));
               Result := Emit_Record_Aggregate (Expression (Expr), Result);
            else
               Result := Insert_Value
                 (Result,
                  Build_Type_Conversion (Expression (Expr), F_Type),
                  unsigned (F_Info.Field_Ordinal));
            end if;
         end;

         Next (Expr);
      end loop;

      return Result;
   end Emit_Record_Aggregate;

begin
   --  Make a dummy entry in the record table, so the "Empty" entry is
   --  never used.

   Record_Info_Table.Increment_Last;
end GNATLLVM.Records;
