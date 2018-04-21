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
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with Namet;  use Namet;
with Nlists; use Nlists;
with Snames; use Snames;
with Stand;  use Stand;
with Table;  use Table;

with LLVM.Core;  use LLVM.Core;

with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Types;       use GNATLLVM.Types;

package body GNATLLVM.Records is

   function Count_Entities (E : Entity_Id) return Nat
     with Pre => Present (E);
   --  Return the number of entities of E

   function Get_Record_Size_So_Far
     (TE       : Entity_Id;
      V        : GL_Value;
      Idx      : Record_Info_Id;
      For_Type : Boolean) return GL_Value
     with Pre  => Present (TE),
          Post => Present (Get_Record_Size_So_Far'Result);

   --  Similar to Get_Record_Type_Size, but stop at record info segment Idx
   --  or the last segment, whichever comes first.

   --  We represent a record by one or more pieces of information
   --  describing the record.  Each piece points to the next piece, if
   --  any.  For non-variant records, each piece either contains an
   --  LLVM type, which contains one or more fields or an GNAT type,
   --  which is used when the field's type is of dynamic size.

   type Record_Info is record
      LLVM_Type : Type_T;
      GNAT_Type : Entity_Id;
      Next      : Record_Info_Id;
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
      Elmt    : Entity_Id := First_Entity (E);

   begin
      while Present (Elmt) loop
         if Ekind_In (Elmt, E_Discriminant, E_Component) then
            Count := Count + 1;
         end if;

         Next_Entity (Elmt);
      end loop;

      return Count;
   end Count_Entities;

   ------------------------
   -- Create_Record_Type --
   ------------------------

   function Create_Record_Type (Def_Ident : Entity_Id) return Type_T is
      Prev_Idx  : Record_Info_Id := Empty_Record_Info_Id;
      --  The previous index of the record table entry, if any

      Cur_Idx   : Record_Info_Id;
      --  The index of the record table entry we're building

      Types     : Type_Array (0 .. Count_Entities (Def_Ident));
      --  Array of all field types that are going into the current piece

      Next_Type : Nat := 0;
      --  Ordinal of next entry in Types

      Field     : Entity_Id;
      LLVM_Type : Type_T;

      procedure Add_RI (LLVM_Type : Type_T; GNAT_Type : Entity_Id)
        with Pre => (Present (LLVM_Type) or else Present (GNAT_Type))
                    and then not (Present (LLVM_Type)
                                    and then Present (GNAT_Type));
      --  Add a Record_Info into the table, chaining it as appropriate

      procedure Add_FI (E : Entity_Id; RI_Idx : Record_Info_Id; Ordinal : Nat)
        with Pre => Ekind_In (E, E_Discriminant, E_Component);
      --  Add a Field_Info info the table, if appropriate, and set
      --  the field to point to it.

      procedure Add_Field (E : Entity_Id)
        with Pre => Ekind_In (E, E_Discriminant, E_Component);
      --  Add one field to the above data

      ------------
      -- Add_RI --
      ------------

      procedure Add_RI (LLVM_Type : Type_T; GNAT_Type : Entity_Id) is
      begin
         --  It's tempting to set Next to the next entry that we'll be using,
         --  but we may not actually be using that one.

         Record_Info_Table.Table (Cur_Idx) :=
           (LLVM_Type => LLVM_Type, GNAT_Type => GNAT_Type,
            Next      => Empty_Record_Info_Id);

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
      begin
         --  If this field really isn't in the record we're working on,
         --  it must be in a parent.  So it was correct to allocate
         --  space for it, but let the record description be from the
         --  type that it's actually in.

         if Get_Fullest_View (Scope (E)) = Def_Ident then
            Field_Info_Table.Append ((Rec_Info_Idx => RI_Idx,
                                      Field_Ordinal => Ordinal));
            Set_Field_Info (E, Field_Info_Table.Last);
         end if;
      end Add_FI;

      ---------------
      -- Add_Field --
      ---------------

      procedure Add_Field (E : Entity_Id) is
         Typ : constant Entity_Id := Full_Etype (E);

      begin
         --  If this is the '_parent' field, we make a dummy entry and handle
         --  it specially later.

         if Chars (E) = Name_uParent then
            Add_FI (E, Get_Record_Info (Def_Ident), 0);
            return;

         --  If this field is dynamic size, we have to close out the last
         --  record info entry we're making, if there's anything in it
         --  and make a piece for this field.

         elsif Is_Dynamic_Size (Typ) then
            if Next_Type /= 0 then
               Add_RI (Build_Struct_Type (Types (0 .. Next_Type - 1)), Empty);
               Next_Type := 0;
            end if;

            Add_FI (E, Cur_Idx, 0);
            Add_RI (No_Type_T, Typ);

         --  If it's of fixed size, add it to the current set of fields
         --  and make a field descriptor.

         else

            Add_FI (E, Cur_Idx, Next_Type);
            Types (Next_Type) := Create_Type (Typ);
            Next_Type := Next_Type + 1;
         end if;

      end Add_Field;

   begin
      --  Because of the potential recursion between record and access types,
      --  make a dummy type for us and set it as our type right at the start.
      --  Then initialize our first record info table entry, which we know
      --  will be used.

      LLVM_Type := Struct_Create_Named (Env.Ctx, Get_Name (Def_Ident));
      Set_Type (Def_Ident, LLVM_Type);
      Record_Info_Table.Increment_Last;
      Cur_Idx := Record_Info_Table.Last;
      Set_Record_Info (Def_Ident, Cur_Idx);

      Field := First_Entity (Def_Ident);
      while Present (Field) loop
         if Ekind_In (Field, E_Discriminant, E_Component) then
            Add_Field (Field);
         end if;

         Next_Entity (Field);
      end loop;

      --  If we haven't yet made any record info entries, it means that
      --  this is a fixed-size record that can be just an LLVM type,
      --  so use the one we made.

      if No (Prev_Idx) then
         Struct_Set_Body (LLVM_Type, Types'Address,
                          unsigned (Next_Type), False);
         Add_RI (LLVM_Type, Empty);

      else
         --  Otherwise, close out the last record info if we have any
         --  fields and show this record is of dynamic size.  Note thast if
         --  we don't have any fields, the entry we allocated will remain
         --  unused, but trying to reclaim it is risky.

         if Next_Type /= 0 then
            Add_RI (Build_Struct_Type (Types (0 .. Next_Type - 1)), Empty);
         end if;

         Set_Dynamic_Size (Def_Ident, True);
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
      For_Type : Boolean) return GL_Value
   is
      Total_Size : GL_Value := Size_Const_Int (0);
      Cur_Idx    : Record_Info_Id := Get_Record_Info (TE);
      RI         : Record_Info;
      This_Size  : GL_Value;
      This_Align : unsigned;

   begin
      while Present (Cur_Idx) and then Cur_Idx /= Idx loop
         RI := Record_Info_Table.Table (Cur_Idx);
         if Present (RI.LLVM_Type) then
            This_Size  := Get_LLVM_Type_Size (RI.LLVM_Type);
            This_Align := Get_Type_Alignment (RI.LLVM_Type);
         else
            This_Size  := Get_Type_Size (RI.GNAT_Type, V, For_Type);
            This_Align := Get_Type_Alignment (RI.GNAT_Type);
         end if;

         --  We have to align the size so far to the alignment of
         --  this type.

         if This_Align /= 1 then
            Total_Size := Build_And
              (NSW_Add (Total_Size,
                        Size_Const_Int (unsigned_long_long (This_Align) - 1)),
               NSW_Neg (Size_Const_Int (unsigned_long_long (This_Align))));
         end if;

         Total_Size := NSW_Add (Total_Size, This_Size);
         Cur_Idx := RI.Next;
      end loop;

      return Total_Size;
   end Get_Record_Size_So_Far;

   -------------------------
   -- Record_Field_Offset --
   -------------------------

   function Record_Field_Offset
     (Ptr : GL_Value; Field : Entity_Id) return GL_Value
   is
      Rec_Type  : constant Entity_Id      := Get_Fullest_View (Scope (Field));
      F_Type    : constant Entity_Id      := Full_Etype (Field);
      First_Idx : constant Record_Info_Id := Get_Record_Info (Rec_Type);
      FI        : constant Field_Info     :=
        Field_Info_Table.Table (Get_Field_Info (Field));
      Our_Idx   : constant Record_Info_Id := FI.Rec_Info_Idx;
      Offset    : constant GL_Value       :=
        Get_Record_Size_So_Far (Rec_Type, Ptr, Our_Idx, False);
      RI        : constant Record_Info    := Record_Info_Table.Table (Our_Idx);
      Result   : GL_Value;

   begin

      --  If this is the "_parent" field, just do a conversion so we point
      --  to that type.

      if Chars (Field) = Name_uParent then
         return Ptr_To_Ref (Ptr, F_Type);

      --  If the current piece is for a variable-sized object, we offset
      --  to that object and make a pointer to its type.

      elsif Present (RI.GNAT_Type) then
         return Ptr_To_Ref (GEP (Standard_Short_Short_Integer,
                                 Pointer_Cast (Ptr, Standard_A_Char),
                                 (1 => Offset)),
                            F_Type);
      end if;

      --  Otherwise, if this is not the first piece, we have to offset to
      --  the field (in bytes).  Then, if the type is dynamic size, we have
      --  to convert the pointer to the type of this piece (which has no
      --  corresponding GNAT type.)

      if Our_Idx = First_Idx then
         Result := Ptr;
      else
         Result := GEP (Standard_Short_Short_Integer,
                        Pointer_Cast (Ptr, Standard_A_Char),
                        (1 => Offset));
      end if;

      if Is_Dynamic_Size (Rec_Type) then
         Result := G (Pointer_Cast (Env.Bld, LLVM_Value (Result),
                                    Pointer_Type (RI.LLVM_Type, 0), ""),
                      Rec_Type, Is_Reference => True);
      end if;

      --  Finally, do a regular GEP for the field and we're done

      return GEP (F_Type, Result,
                  (1 => Const_Null_32,
                   2 => Const_Int_32
                     (unsigned_long_long (FI.Field_Ordinal))));

   end Record_Field_Offset;

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
        (TE : Entity_Id; Fld : Entity_Id) return Entity_Id
      with Pre  => Is_Record_Type (TE)
                   and then Ekind_In (Fld, E_Discriminant, E_Component),
           Post => Original_Record_Component (Fld) =
                     Original_Record_Component (Find_Matching_Field'Result);
      --  Find a field corresponding to Fld in record type TE

      -------------------------
      -- Find_Matching_Field --
      -------------------------

      function Find_Matching_Field
        (TE : Entity_Id; Fld : Entity_Id) return Entity_Id
      is
         Ent : Entity_Id := First_Entity (TE);

      begin
         while Present (Ent) loop
            if Ekind_In (Ent, E_Discriminant, E_Component)
              and then (Original_Record_Component (Ent) =
                          Original_Record_Component (Fld))
            then
               return Ent;
            end if;

            Next_Entity (Ent);
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
            Value  : GL_Value;
            Temp   : GL_Value;
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
               Value := Emit_Expression (Expression (Expr));

               --  If Value and the field have different types, we may need
               --  to do something.  If both types are elementary, just
               --  convert.  If both are array types, the LLVM type should
               --  be the same for both.  Unfortunately, that's not the
               --  case for records and the only way to fix that is to
               --  essentially do an unchecked conversion by writing the
               --  value to memory, converting the pointer, and loading it
               --  again.  But be careful to check access types first since
               --  Value may be a reference.

               if Is_Access_Type (Value)
                 and then (Full_Designated_Type (Value)
                             /= Full_Designated_Type (F_Type))
               then
                  Value := Convert_To_Access_To
                    (Value, Full_Designated_Type (F_Type));

               elsif not Is_Reference (Value)
                 and then Full_Etype (Value) /= F_Type
               then
                  if Is_Elementary_Type (F_Type) then
                     Value := Convert_To_Elementary_Type (Value, F_Type);
                  elsif Is_Record_Type (F_Type) then
                     Temp := Alloca (Full_Etype (Value));
                     Store (Value, Temp);
                     Value := Load (Ptr_To_Ref (Temp, F_Type));
                  end if;
               end if;

               Result := Insert_Value
                 (Result, Value, unsigned (F_Info.Field_Ordinal));
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
