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

with Ada.Containers.Generic_Constrained_Array_Sort;

with Elists;   use Elists;
with Get_Targ; use Get_Targ;
with Nlists;   use Nlists;
with Output;   use Output;
with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Snames;   use Snames;
with Sprint;   use Sprint;
with Stand;    use Stand;
with Table;    use Table;
with Uintp;    use Uintp;

with LLVM.Core;  use LLVM.Core;

with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Conditionals; use GNATLLVM.Conditionals;
with GNATLLVM.DebugInfo;    use GNATLLVM.DebugInfo;
with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.Utils;        use GNATLLVM.Utils;

package body GNATLLVM.Records is

   --  We represent a record by one or more fragments describing the
   --  record.  Each piece points to the next piece, if any.  Each can
   --  contain an LLVM type, which contains one or more fields or an GNAT
   --  type, which is used when the field's type is of dynamic size.  If
   --  neither is present, this fragment doesn't represent a component of
   --  the record, but is used for chaining purposes, for example for
   --  variant record.

   type Record_Info_Base is record
      LLVM_Type    : Type_T;
      --  The LLVM type corresponding to this fragment, if any

      GNAT_Type    : Entity_Id;
      --  The GNAT type corresponding to this fragment, if any

      Next         : Record_Info_Id;
      --  Link to the next Record_Info entry for this record or variant

      Variant_List : List_Id;
      --  List in GNAT tree of the variants for this fragment

      Variant_Expr : Node_Id;
      --  Expression to evaluate to determine which variant is present

      Variants     : access Record_Info_Id_Array;
      --  Pointer to array of Record_Info_Ids representing the variants,
      --  which must be in the same order as in Variant_List.

      Use_Max_Size : Boolean;
      --  In the case where a record's type is a record type (as opposed to
      --  a record subtype), True means that we are to use the maximum type
      --  of that size for allocation purpose, so we need to flag that
      --  here.
   end record;
   --  We want to put a Predicate on this, but can't, so we need to make
   --  a subtype for that purpose.

   function RI_Value_Is_Valid (RI : Record_Info_Base) return Boolean;
   --  Return whether a Record_Info value is valid or not

   subtype Record_Info is Record_Info_Base
     with Predicate => RI_Value_Is_Valid (Record_Info);

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

   function Count_Entities (E : Entity_Id) return Nat
     with Pre => Present (E);
   --  Return the number of entities of E.  This value will be used only
   --  to allocate an array that we know is large enough to contain all
   --  the fields, so we can overestimate the number of fields (even
   --  greatly), but can't underestimate.

   procedure Get_RI_Info
     (RI          : Record_Info;
      V           : GL_Value;
      For_Type    : Boolean;
      Size        : out GL_Value;
      Must_Align  : out GL_Value;
      Is_Align    : out GL_Value;
      Return_Size : Boolean := True)
     with Post => (not Return_Size or else Present (Size))
                  and then Present (Must_Align) and then Present (Is_Align);
     --  Return information about a record fragment RI.  This includes is
     --  size, the amount to which this fragment must be aligned, and the
     --  amout to which the resulting size is known to be aligned.  If the
     --  size isn't wanted, don't compute it.

   procedure Get_RI_Info_For_Variant
     (RI          : Record_Info;
      V           : GL_Value;
      Size        : out GL_Value;
      Must_Align  : out GL_Value;
      Is_Align    : out GL_Value;
      Return_Size : Boolean := True)
     with Pre  => RI.Variants /= null,
          Post => not Return_Size or else Present (Size);
   --  Like Get_RI_Info, but for a fragment known to be a variant and
   --  where we're not getting the maximum size.

   procedure Get_RI_Info_For_Max_Size_Variant
     (RI         : Record_Info;
      Size        : out GL_Value;
      Must_Align  : out GL_Value;
      Is_Align    : out GL_Value;
      Return_Size : Boolean := True)
     with Pre  => RI.Variants /= null,
          Post => (not Return_Size or else Present (Size))
                  and then Present (Must_Align) and then Present (Is_Align);
   --  Get informaton correspondind to the maxium size of the variant
   --  described by In_RI.

   function Get_Variant_For_RI
     (In_RI : Record_Info; Need_Idx : Record_Info_Id) return Record_Info_Id
     with Pre => Present (Need_Idx);
   --  We are at RI when walking the description for a record and
   --  it has variants.  We're looking for Need_Idx.  If Need_Idx is an
   --  index in one of the variants, return that variant.

   function Get_Record_Size_So_Far
     (TE        : Entity_Id;
      V         : GL_Value;
      Start_Idx : Record_Info_Id;
      Idx       : Record_Info_Id;
      For_Type  : Boolean := False) return GL_Value
     with Post => Present (Get_Record_Size_So_Far'Result);
   --  Similar to Get_Record_Type_Size, but stop at record info segment Idx
   --  or the last segment, whichever comes first.  If TE is Present, it
   --  provides the default for Start_Idx and also requests alignment to
   --  TE's alignment if we're looking for the size.

   ------------------------
   --  RI_Value_Is_Valid --
   ------------------------

   function RI_Value_Is_Valid (RI : Record_Info_Base) return Boolean is
   begin
      --  This must be an LLVM Type, which is a struct, a GNAT type, or a
      --  variant and only one of those.

      if Present (RI.LLVM_Type) then
         return No (RI.GNAT_Type) and then RI.Variants = null
           and then Get_Type_Kind (RI.LLVM_Type) = Struct_Type_Kind;
      elsif Present (RI.GNAT_Type) then
         --  We already know that LLVM_Type isn't Present

         return RI.Variants = null;
      else
         --  Here we know that neither type is Present

         return Present (RI.Variant_List) and then Present (RI.Variant_Expr)
           and then RI.Variants /= null;
      end if;
   end RI_Value_Is_Valid;

   ---------------------
   --  Count_Entities --
   ---------------------

   function Count_Entities (E : Entity_Id) return Nat is
      Count   : Nat := 0;
      Elmt    : Entity_Id := First_Component_Or_Discriminant (E);

   begin
      while Present (Elmt) loop
         Count := Count + (if Chars (Elmt) = Name_uParent
                              then Count_Entities (Full_Etype (Elmt)) else 1);
         Next_Component_Or_Discriminant (Elmt);
      end loop;

      return Count;
   end Count_Entities;

   ------------------------
   -- Create_Record_Type --
   ------------------------

   function Create_Record_Type (TE : Entity_Id) return Type_T is

      --  This function creates a record type and the description of that
      --  record.  Note that this function does not itself lay out the record.
      --  We don't actually lay out record in that sense.  Instead, we create
      --  Record_Info structures whose chaining describe the record structure
      --  and Field_Info structures, one for each field, showing where each
      --  field is located in the record.  We then compute any information
      --  we need on the fly, mostly in Get_Record_Size_So_Far.

      Prev_Idx    : Record_Info_Id := Empty_Record_Info_Id;
      --  The previous index of the record table entry, if any

      First_Idx   : Record_Info_Id := Empty_Record_Info_Id;
      --  The first index used by the current call

      Cur_Idx     : Record_Info_Id;
      --  The index of the record table entry we're building

      Types       : Type_Array (0 .. Count_Entities (TE));
      --  Array of all field types that are going into the current piece

      Next_Type   : Nat := 0;
      --  Ordinal of next entry in Types

      Cur_Field   : Entity_Id := Empty;
      --  Used for a cache in Find_Matching_Field to avoid quadratic
      --  behavior.

      Last_Align  : unsigned  := unsigned (Get_Maximum_Alignment);
      --  The last known alignment for this record

      Split_Align : unsigned  := unsigned (Get_Maximum_Alignment);
      --  We need to split an LLVM fragment type if the alignment of the next
      --  field is greater than both this and Last_Align.  This only occurs
      --  for variant records.  See details there.

      LLVM_Type : Type_T;
      --  The LLVM type for this record type
      function Find_Field_In_Entity_List
        (F         : Entity_Id;
         Rec_Type  : Entity_Id;
         Cur_Field : in out Entity_Id) return Entity_Id
        with Pre  => Ekind_In (F, E_Discriminant, E_Component)
                     and then Is_Record_Type (Rec_Type);
      --  Find a field in the entity list of Rec_Type that has the same
      --  Original_Record_Component as F and return it if so.  Cur_Field
      --  is used to cache the last field tested to avoid quadratic behavior
      --  since we'll be requesting fields in roughly (but not exactly!)
      --  the same order as they are in the list.

      procedure Add_RI
        (T            : Type_T                      := No_Type_T;
         Typ          : Entity_Id                   := Empty;
         Variant_List : List_Id                     := No_List;
         Variant_Expr : Node_Id                     := Empty;
         Variants     : access Record_Info_Id_Array := null;
         Use_Max_Size : Boolean                     := False);
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

      procedure Flush_Current_Types;
      --  If there are any types in the Types array, create a record
      --  description for them.

      -------------------------------
      -- Find_Field_In_Entity_List --
      -------------------------------

      function Find_Field_In_Entity_List
        (F         : Entity_Id;
         Rec_Type  : Entity_Id;
         Cur_Field : in out Entity_Id) return Entity_Id
      is

         function ORC (F : Entity_Id) return Entity_Id
           with Pre  => Ekind_In (F, E_Discriminant, E_Component),
                Post => Ekind_In (ORC'Result, E_Discriminant, E_Component);
         --  Get the Original_Record_Component, but also check
         --  Corresponding_Discriminant first;

         ---------
         -- ORC --
         ---------

         function ORC (F : Entity_Id) return Entity_Id is
            Field : Entity_Id := F;

         begin
            while Ekind (Field) = E_Discriminant loop
               exit when No (Corresponding_Discriminant (Field));
               Field := Corresponding_Discriminant (Field);
            end loop;

            return Original_Record_Component (Field);
         end ORC;

         Initial_Cur_Field : constant Entity_Id := Cur_Field;

      begin
         --  Look from Cur_Field until the end of the list.  Then look from
         --  the beginning to its previous value.

         while Present (Cur_Field) loop
            if ORC (Cur_Field) = ORC (F) then
               return Cur_Field;
            end if;

            Next_Component_Or_Discriminant (Cur_Field);
         end loop;

         Cur_Field := First_Component_Or_Discriminant (Rec_Type);
         while Cur_Field /= Initial_Cur_Field loop
            if ORC (Cur_Field) = ORC (F) then
               return Cur_Field;
            end if;

            Next_Component_Or_Discriminant (Cur_Field);
         end loop;

         return Empty;
      end Find_Field_In_Entity_List;

      ------------
      -- Add_RI --
      ------------

      procedure Add_RI
        (T            : Type_T                      := No_Type_T;
         Typ          : Entity_Id                   := Empty;
         Variant_List : List_Id                     := No_List;
         Variant_Expr : Node_Id                     := Empty;
         Variants     : access Record_Info_Id_Array := null;
         Use_Max_Size : Boolean                     := False) is
      begin
         --  It's tempting to set Next to the next entry that we'll be using,
         --  but we may not actually end up using that one.

         Record_Info_Table.Table (Cur_Idx) :=
           (LLVM_Type    => T,
            GNAT_Type    => Typ,
            Next         => Empty_Record_Info_Id,
            Variant_List => Variant_List,
            Variant_Expr => Variant_Expr,
            Variants     => Variants,
            Use_Max_Size => Use_Max_Size);

         if Present (Prev_Idx) then
            Record_Info_Table.Table (Prev_Idx).Next := Cur_Idx;
         else
            First_Idx := Cur_Idx;
         end if;

         Prev_Idx := Cur_Idx;
         Record_Info_Table.Increment_Last;
         Cur_Idx := Record_Info_Table.Last;
      end Add_RI;

      ------------
      -- Add_FI --
      ------------

      procedure Add_FI
        (E : Entity_Id; RI_Idx : Record_Info_Id; Ordinal : Nat)
      is
         Matching_Field : Entity_Id;

      begin
         --  If this field really isn't in the record we're working on, it
         --  must be in a parent.  So it was correct to allocate space for
         --  it, but let the record description be from the type that it's
         --  actually in.  The fields in the entity list for this type are
         --  almost, but not quite, win the same order as in the component
         --  list, so we have to search for a field in that list with the
         --  same Original_Record_Component as this field.

         Field_Info_Table.Append ((Rec_Info_Idx => RI_Idx,
                                   Field_Ordinal => Ordinal));
         if Full_Scope (E) = TE then
            Set_Field_Info (E, Field_Info_Table.Last);
         end if;

         Matching_Field := Find_Field_In_Entity_List (E, TE, Cur_Field);
         if Present (Matching_Field) and then Matching_Field /= E then
            Set_Field_Info (Matching_Field, Field_Info_Table.Last);
         end if;
      end Add_FI;

      ----------------
      -- Add_Fields --
      ----------------

      procedure Add_Fields (Def_Ident : Entity_Id) is
         Rec_Type     : constant Entity_Id :=
           Implementation_Base_Type (Def_Ident);
         --  The base type, which we use to get the record order from

         Sub_Rec_Type : constant Entity_Id :=
           (if Rec_Type = Def_Ident then Empty else Def_Ident);
         --  Present if we have to search the field list of a record subtype

         Rec_Field : Entity_Id := Empty;
         --  Cache used to limit quadratic behavior

         function Matches_Name (F : Entity_Id; Name : Name_Id) return Boolean
           with Pre => Ekind_In (F, E_Component, E_Discriminant);
         --  See if the field F matches Name, either because Name is
         --  specified and matches F or name is not specified and F is not
         --  a special name.

         function Get_Discriminant_Constraint
           (TE : Entity_Id; Part : Node_Id) return Node_Id
           with Pre  => Ekind (TE) = E_Record_Subtype
                       and then Nkind (Part) = N_Variant_Part,
                Post => Present (Get_Discriminant_Constraint'Result);
           --  Get the expression that constrains the discriminant of
           --  type TE that's specified in Part.

         function Find_Choice (N : Node_Id; Alts : List_Id) return Node_Id
           with Pre  => Is_Static_Expression (N) and then Present (Alts),
           Post => Present (Find_Choice'Result);
         --  N is a static expression and Alts is a list of alternatives.
         --  Return which alternate has a Choice that covers N.

         procedure Add_Component_List
           (List : Node_Id; From_Rec : Entity_Id; F_Name : Name_Id)
           with Pre => (No (List) or else Nkind (List) = N_Component_List)
                       and then (No (From_Rec)
                                   or else Is_Record_Type (From_Rec));
         --  Add all fields in List matching Name.  If From_Rec is Present,
         --  instead of adding the actual field, add the field of the same
         --  name from From_Rec.

         ------------------
         -- Matches_Name --
         ------------------

         function Matches_Name
           (F : Entity_Id; Name : Name_Id) return Boolean is
         begin

            if Chars (F) = Name then
               return True;

            else
               return (No (Name) and then Chars (F) /= Name_uTag
                         and then Chars (F) /= Name_uParent
                         and then Chars (F) /= Name_uController);
            end if;
         end Matches_Name;

         ---------------------------------
         -- Get_Discriminant_Constraint --
         ---------------------------------

         function Get_Discriminant_Constraint
           (TE : Entity_Id; Part : Node_Id) return Node_Id
         is
            Discrim     : constant Entity_Id := Entity (Name (Part));
            Discrim_Num : constant Uint      := Discriminant_Number (Discrim);
            Constraint  : constant Elist_Id  := Discriminant_Constraint (TE);
            Elmt        : Elmt_Id            := First_Elmt (Constraint);

         begin
            --  Skip to the proper entry in the list and see if it's static

            for J in 1 .. UI_To_Int (Discrim_Num) - 1 loop
               Next_Elmt (Elmt);
            end loop;

            return Node (Elmt);
         end Get_Discriminant_Constraint;

         -----------------
         -- Find_Choice --
         -----------------

         function Find_Choice (N : Node_Id; Alts : List_Id) return Node_Id is
            Value       : constant Uint := Expr_Value (N);
            Alt, Choice : Node_Id;
            Low, High   : Uint;

         begin
            Alt := First (Alts);
            while Present (Alt) loop
               Choice := First (Discrete_Choices (Alt));
               if Nkind (Choice) = N_Others_Choice then
                  Choice := First (Others_Discrete_Choices (Choice));
               end if;

               while Present (Choice) loop
                  Decode_Range (Choice, Low, High);
                  if Value >= Low and then Value <= High then
                     return Alt;
                  end if;

                  Next (Choice);
               end loop;

               Next (Alt);
            end loop;

            return Empty;
         end Find_Choice;

         ------------------------
         -- Add_Component_List --
         ------------------------

         procedure Add_Component_List
           (List : Node_Id; From_Rec : Entity_Id; F_Name : Name_Id)
         is
            Var_Part           : constant Node_Id :=
              (if Present (List) then Variant_Part (List) else Empty);
            Constraining_Expr  : constant Node_Id :=
              (if Present (From_Rec) and then Present (Var_Part)
               then Get_Discriminant_Constraint (From_Rec, Var_Part)
               else Empty);
            Static_Constraint  : constant Boolean :=
              Present (Constraining_Expr)
                and then Is_Static_Expression (Constraining_Expr);
            Var_Array          : access Record_Info_Id_Array;
            Saved_Cur_Idx      : Record_Info_Id;
            Saved_Prev_Idx     : Record_Info_Id;
            Saved_Align        : unsigned;
            Component_Def      : Node_Id;
            Field              : Entity_Id;
            Field_To_Add       : Entity_Id;
            Variant            : Node_Id;
            J                  : Nat;

         begin
            --  Return quickly if nothing to do.  Otherwise, walk the component
            --  list looking for a field matching the name given to us.

            if No (List) then
               return;
            end if;

            Component_Def := First_Non_Pragma (Component_Items (List));
            while Present (Component_Def) loop
               Field := Defining_Identifier (Component_Def);
               if Matches_Name (Field, F_Name) then
                  Field_To_Add := Field;
                  if Present (From_Rec) then
                     Field_To_Add :=
                       Find_Field_In_Entity_List (Field, From_Rec, Rec_Field);
                  end if;

                  if Present (Field_To_Add) then
                     if Chars (Field_To_Add) = Name_uParent then
                        Add_Fields (Full_Etype (Field_To_Add));
                     end if;

                     Add_Field (Field_To_Add);
                  end if;
               end if;

               Next_Non_Pragma (Component_Def);
            end loop;

            --  Done if either we're doing special names or there are no
            --  variants in this record.

            if Present (F_Name) or else No (Var_Part) then
               return;
            end if;

            --  Otherwise process variants.  If we statically constrain the
            --  variant, see which variant is being referenced and output
            --  that one.  walk the proper variant here to verify that
            --  ever.  Otherwise, set up for the variant, make the entres
            --  for each variant, and then create the RI for the variant.

            if Static_Constraint then
               Add_Component_List (Component_List
                                     (Find_Choice (Constraining_Expr,
                                                   Variants (Var_Part))),
                                   From_Rec, No_Name);
               return;
            end if;

            Variant := First (Variants (Var_Part));
            Flush_Current_Types;
            Set_Is_Dynamic_Size (TE);
            Saved_Cur_Idx  := Cur_Idx;
            Saved_Prev_Idx := Prev_Idx;
            Saved_Align    := Last_Align;
            J              := 1;
            Var_Array      := new
              Record_Info_Id_Array'(1 .. List_Length (Variants (Var_Part))
                                      => Empty_Record_Info_Id);

            while Present (Variant) loop

               --  Create the fields in each variant.
               --
               --  We have a potential alignment issue here.  Suppose the
               --  last fragment was { i32, i8 }.  And suppose we have a
               --  variant that starts with { i8, i32 }.  That type has an
               --  alignment of 4, so that fragment would as well.  But the
               --  subtype will be
               --
               --              { i32, i8, i8, i32 }
               --
               --  with the third field at and offset of 6, not 8. So we
               --  need to split that and make two fragments, one { i8 }
               --  and one { i32 }.  We do this via Split_Align.
               --
               --  If we have items in this variant, allocate a new
               --  Record_Info entry for this variant and create the fields.

               First_Idx := Empty_Record_Info_Id;
               if Present (Component_Items (Component_List (Variant))) then
                  Record_Info_Table.Increment_Last;
                  Prev_Idx      := Empty_Record_Info_Id;
                  Cur_Idx       := Record_Info_Table.Last;
                  Split_Align   := Saved_Align;
                  Add_Component_List (Component_List (Variant),
                                      From_Rec, No_Name);
                  Flush_Current_Types;
               end if;

               Var_Array (J) := First_Idx;
               J             := J + 1;
               Next (Variant);
            end loop;

            Prev_Idx := Saved_Prev_Idx;
            Cur_Idx  := Saved_Cur_Idx;
            Add_RI (Variant_List => Variants (Var_Part),
                    Variants     => Var_Array,
                    Variant_Expr =>
                      (if Present (Constraining_Expr) then Constraining_Expr
                       else Name (Var_Part)));

         end Add_Component_List;

         Field             : Entity_Id;
         Field_To_Add      : Entity_Id;
         Outer_Field       : Entity_Id;
         Record_Definition : Node_Id;
         Components        : Node_Id;

      --  Start of processing for Add_Fields

      begin
         --  Get the record definition

         Record_Definition :=
           Type_Definition (Declaration_Node (Rec_Type));
         if Nkind (Record_Definition) = N_Derived_Type_Definition then
            Record_Definition := Record_Extension_Part (Record_Definition);
         end if;

         --  Add special components

         Components := Component_List (Record_Definition);
         Add_Component_List (Components, Sub_Rec_Type, Name_uTag);
         Add_Component_List (Components, Sub_Rec_Type, Name_uParent);
         Add_Component_List (Components, Sub_Rec_Type, Name_uController);

         --  Next, if there are discriminants, process them.  But
         --  ignore discriminants that are already in a parent type.

         if Has_Discriminants (Rec_Type) then
            Field := First_Discriminant (Rec_Type);
            while Present (Field) loop
               Field_To_Add := Field;
               if Present (Sub_Rec_Type) then
                  Field_To_Add :=
                    Find_Field_In_Entity_List (Field, Sub_Rec_Type, Rec_Field);
               end if;

               Outer_Field
                 := Find_Field_In_Entity_List (Field_To_Add, TE, Cur_Field);
               if Present (Field_To_Add)
                 and then not Has_Field_Info (Field_To_Add)
                 and then (No (Outer_Field)
                             or else not Has_Field_Info (Outer_Field))
               then
                  Add_Field (Field);
               end if;

               Next_Discriminant (Field);
            end loop;
         end if;

         --  Then add everything else

         Add_Component_List (Components, Sub_Rec_Type, No_Name);

      end Add_Fields;

      -------------------------
      -- Flush_Current_Types --
      -------------------------

      procedure Flush_Current_Types is
      begin
         if Next_Type /= 0 then
            Add_RI (T => Build_Struct_Type (Types (0 .. Next_Type - 1)));
            Next_Type := 0;
         end if;
      end Flush_Current_Types;

      ---------------
      -- Add_Field --
      ---------------

      procedure Add_Field (E : Entity_Id) is
         Typ   : constant Entity_Id := Full_Etype (E);
         Align : constant unsigned  := Get_Type_Alignment (Typ);
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
            Flush_Current_Types;
            Add_FI (E, Cur_Idx, 0);
            Add_RI (Typ => Typ, Use_Max_Size => not Is_Constrained (Typ));
            Set_Is_Dynamic_Size (TE);

         --  If it's of fixed size, add it to the current set of fields
         --  and make a field descriptor.

         else

            --  We need to flush the previous types if required by the
            --  alignment.

            if Align > Last_Align and then Align > Split_Align then
               Flush_Current_Types;
            end if;

            Add_FI (E, Cur_Idx, Next_Type);
            Types (Next_Type) := Create_Type (Typ);
            Next_Type := Next_Type + 1;
         end if;

         Last_Align := Align;
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
         --  fields.  Note that if we don't have any fields, the entry we
         --  allocated will remain unused, but trying to reclaim it is
         --  risky.

         Flush_Current_Types;
      end if;

      return LLVM_Type;
   end Create_Record_Type;

   -----------------
   -- Get_RI_Info --
   -----------------

   procedure Get_RI_Info
     (RI          : Record_Info;
      V           : GL_Value;
      For_Type    : Boolean;
      Size        : out GL_Value;
      Must_Align  : out GL_Value;
      Is_Align    : out GL_Value;
      Return_Size : Boolean := True)
   is
      T         : constant Type_T      := RI.LLVM_Type;
      TE        : constant Entity_Id   := RI.GNAT_Type;
      This_Size : GL_Value;

   begin
         --  First check for zero length LLVM type since the code below will
         --  fail if we have no fields.

      if Present (T) and then Get_LLVM_Type_Size (T) = ULL (0) then
            This_Size  := Size_Const_Null;
            Must_Align := Size_Const_Int (Get_Type_Alignment (T));
            Is_Align   := Size_Const_Int (Get_Type_Alignment (T));

      elsif Present (T) then

         --  We have to be careful how we compute the size of this record
         --  fragment because we don't want to count the padding at the end
         --  in all cases.  If this is followed by a variable-sized
         --  fragment, we may have a subtype where the following field is
         --  fixed size and hence is part of the same LLVM struct in that
         --  subtype.  In such a case, if the alignment of that type is
         --  less than the alignment of our type, there'll be less padding.
         --
         --  For example, suppose we have:
         --
         --     type R (X : Integer) is record
         --        A : Integer;
         --        B : Short_Short_Integer;
         --        C : String (1 .. X);
         --     end record;
         --
         --  In the layout of the base type, which is of variable size, the
         --  LLVM struct will be { i32, i32, i8 } and the next fragment
         --  will be for field C.  The above struct has a size of 12
         --  because the size is always a multiple of itsq alignment.
         --  However, if we have a subtype of R with X = 10, the struct for
         --  that subtype (now containing all the fields) will be
         --
         --      { i32, i32, i8, [10 x i8] }
         --
         --  but that last array will be at an offset of 9, not 12, which
         --  is what we'd get by just positioning it from the size of the
         --  fragment from the base type.
         --
         --  Therefore, we need to compute the size of this struct by
         --  adding the position of the last field to its size.  If padding
         --  is needed for any reason, our caller will take care of that.
         --
         --  In addition, unlike in the GNAT type (variable sized) case,
         --  the alignment that we must receive and that we generate are
         --  not the same.
         --
         declare
            Num_Types   : constant unsigned := Count_Struct_Element_Types (T);
            Last_Type   : constant Type_T   :=
              Struct_Get_Type_At_Index (T, Num_Types - 1);
            Last_Size   : constant ULL      := Get_LLVM_Type_Size (Last_Type);
            Last_Offset : constant ULL      :=
              Offset_Of_Element (Module_Data_Layout, T, Num_Types - 1);

         begin
            Must_Align := Size_Const_Int (Get_Type_Alignment (T));
            Is_Align   := Size_Const_Int (Get_Type_Alignment (Last_Type));
            This_Size  := Size_Const_Int (Last_Offset + Last_Size);
         end;

      --  The GNAT type case is easy

      elsif Present (TE) then
         Must_Align := Size_Const_Int (Get_Type_Alignment (TE));
         Is_Align   := Must_Align;
         if Return_Size then
            This_Size  := Get_Type_Size (TE, V, For_Type or RI.Use_Max_Size);
         end if;

      elsif RI.Variants /= null then
         if For_Type then
            Get_RI_Info_For_Max_Size_Variant (RI, This_Size, Must_Align,
                                              Is_Align, Return_Size);
         else
            Push_Discriminant_Info (For_Type => False, Is_Low_Bound => False);
            Get_RI_Info_For_Variant (RI, V, This_Size, Must_Align, Is_Align,
                                     Return_Size);
            Pop_Discriminant_Info;
         end if;

      else
         Must_Align := Size_Const_Int (Uint_1);
         Is_Align   := Size_Const_Int (ULL (Get_Maximum_Alignment));
         This_Size  := Size_Const_Null;
      end if;

      if Return_Size then
         Size := This_Size;
      end if;
   end Get_RI_Info;

   ------------------------
   -- Get_Variant_For_RI --
   ------------------------

   function Get_Variant_For_RI
     (In_RI : Record_Info; Need_Idx : Record_Info_Id) return Record_Info_Id
   is
      Idx : Record_Info_Id;
      RI  : Record_Info;

   begin
      --  Look through each variant

      for Variant_Idx of In_RI.Variants.all loop

         --  Now look through each entry in the variant, looking into nested
         --  variants if necessary.  We start looking at the first chained
         --  entry of each variant, since that's where fields of that
         --  variant start.

         Idx := Variant_Idx;
         while Present (Idx) loop
            RI := Record_Info_Table.Table (Idx);
            if Idx = Need_Idx then
               return Variant_Idx;
            elsif RI.Variants /= null then
               return (if Present (Get_Variant_For_RI (RI, Need_Idx))
                       then Variant_Idx else Empty_Record_Info_Id);
            else
               Idx := RI.Next;
            end if;
         end loop;
      end loop;

      return Empty_Record_Info_Id;
   end Get_Variant_For_RI;

   -----------------------------
   -- Get_RI_Info_For_Variant --
   -----------------------------

   procedure Get_RI_Info_For_Variant
     (RI          : Record_Info;
      V           : GL_Value;
      Size        : out GL_Value;
      Must_Align  : out GL_Value;
      Is_Align    : out GL_Value;
      Return_Size : Boolean := True)
   is
      Our_BB      : constant Basic_Block_T             := Get_Insert_Block;
      End_BB      : constant Basic_Block_T             := Create_Basic_Block;
      Must_Aligns : GL_Value_Array (RI.Variants'Range) :=
        (others => No_GL_Value);
      Is_Aligns   : GL_Value_Array (RI.Variants'Range) :=
        (others => No_GL_Value);
      Sizes       : GL_Value_Array (RI.Variants'Range) :=
        (others => No_GL_Value);
      Junk1       : GL_Value                           := No_GL_Value;
      Junk2       : GL_Value                           := No_GL_Value;
      To_BBs      : Basic_Block_Array (RI.Variants'Range);
      From_BBs    : Basic_Block_Array (RI.Variants'Range);
      Idx         : Record_Info_Id;

   begin
      --  We first go through each variant and compute the alignments and
      --  sizes of each.  We store the GL_value's where we've computed
      --  those things along with the starting (for branching into the code)
      --  and ending (for use with Phi) basic blocks for each.

      for J in RI.Variants'Range loop
         To_BBs (J) := Create_Basic_Block;
         Position_Builder_At_End (To_BBs (J));

         --  If this variant is empty, trivially get the values.  Otherwise,
         --  compute each, computing the size only if needed.

         if No (RI.Variants (J)) then
            Must_Aligns (J) := Size_Const_Int (Uint_1);
            Is_Aligns   (J) := Size_Const_Int (ULL (Get_Maximum_Alignment));
            Sizes       (J) := Size_Const_Null;
         else
            --  Must_Align comes from the first fragment, Is_Align comes
            --  from the last, and the size is computed from all of them.

            Get_RI_Info (Record_Info_Table.Table (RI.Variants (J)),
                         V, False, Junk1, Must_Aligns (J), Junk2, False);
            Idx := RI.Variants (J);
            while Present (Record_Info_Table.Table (Idx).Next) loop
               Idx := Record_Info_Table.Table (Idx).Next;
            end loop;

            Get_RI_Info (Record_Info_Table.Table (RI.Variants (J)),
                         V, False, Junk1, Junk2, Is_Aligns (J), False);

            if Return_Size then
               Sizes (J) :=
                 Get_Record_Size_So_Far (Empty, V, RI.Variants (J),
                                         Empty_Record_Info_Id);
            end if;
         end if;

         From_BBs (J) := Get_Insert_Block;
         Build_Br (End_BB);
      end loop;

      --  Now emit the code to branch to the fragments we made above

      Position_Builder_At_End (Our_BB);
      Emit_Case_Code (RI.Variant_List, Emit_Expression (RI.Variant_Expr),
                      To_BBs);

      --  Now make the Phi's that hold all the values and return them.

      Position_Builder_At_End (End_BB);
      Must_Align := Build_Phi (Must_Aligns, From_BBs);
      Is_Align   := Build_Phi (Is_Aligns,    From_BBs);
      if Return_Size then
         Size    := Build_Phi (Sizes,       From_BBs);
      end if;
   end Get_RI_Info_For_Variant;

   --------------------------------------
   -- Get_RI_Info_For_Max_Size_Variant --
   --------------------------------------

   procedure Get_RI_Info_For_Max_Size_Variant
     (RI          : Record_Info;
      Size        : out GL_Value;
      Must_Align  : out GL_Value;
      Is_Align    : out GL_Value;
      Return_Size : Boolean := True)
   is
      First      : Boolean      := True;
      End_BB     : Basic_Block_T;
      Next_BB    : Basic_Block_T;
      This_BB    : Basic_Block_T;
      Max_So_Far : GL_Value;
      Var_Size   : GL_Value;
      Idx        : Record_Info_Id;

   begin
      --  We need to compute the maximum size of each discriminant.  We set
      --  Max_So_Far to the size of the first variant and then see if any
      --  is larger.  Handle the case where the variant is empty.  If there
      --  is only one variant, we just use its size.  We need to
      --  special-case this to avoid generating empty basic blocks.

      if RI.Variants'Length = 1 then
         if Return_Size then
            Size := Get_Record_Size_So_Far
              (Empty, No_GL_Value, RI.Variants (RI.Variants'First),
               Empty_Record_Info_Id, True);
         end if;

         Must_Align := Size_Const_Int (ULL (Get_Maximum_Alignment));
         Is_Align   := Size_Const_Int (Uint_1);
         return;
      end if;

      End_BB := Create_Basic_Block;
      for J in RI.Variants'Range loop
         Idx := RI.Variants (J);
         Var_Size  := Get_Record_Size_So_Far (Empty, No_GL_Value, Idx,
                                               Empty_Record_Info_Id, True);
         if First then
            Max_So_Far := Allocate_For_Type (Size_Type, Size_Type, Var_Size);
            First      := False;
         else
            Next_BB  := (if J = RI.Variants'Last
                         then End_BB else Create_Basic_Block);
            This_BB  := Create_Basic_Block;
            Build_Cond_Br (I_Cmp (Int_SGT, Var_Size, Get (Max_So_Far, Data)),
                           This_BB, Next_BB);
            Position_Builder_At_End (This_BB);
            Store (Var_Size, Max_So_Far);
            Build_Br (End_BB);
            Position_Builder_At_End (Next_BB);
         end if;
      end loop;

      --  Get and return final results.  We might be able to do better with
      --  alignments here, but that's not clear.

      Must_Align := Size_Const_Int (ULL (Get_Maximum_Alignment));
      Is_Align   := Size_Const_Int (Uint_1);
      if Return_Size then
         Size := Get (Max_So_Far, Data);
      end if;

   end Get_RI_Info_For_Max_Size_Variant;

   ----------------------------
   -- Get_Record_Size_So_Far --
   ----------------------------

   function Get_Record_Size_So_Far
     (TE        : Entity_Id;
      V         : GL_Value;
      Start_Idx : Record_Info_Id;
      Idx       : Record_Info_Id;
      For_Type  : Boolean := False) return GL_Value
   is
      Total_Size : GL_Value       := Size_Const_Null;
      Cur_Align  : GL_Value       :=
        Size_Const_Int (ULL (Get_Maximum_Alignment));
      Cur_Idx    : Record_Info_Id :=
        (if Present (Start_Idx) then Start_Idx elsif Present (TE)
         then Get_Record_Info (TE) else Empty_Record_Info_Id);
      This_Size  : GL_Value       := No_GL_Value;
      Must_Align : GL_Value       := Size_Const_Int (Uint_1);
      This_Align : GL_Value;
      New_Idx    : Record_Info_Id;
      RI         : Record_Info;

   begin
      Push_Debug_Freeze_Pos;

      --  If we're passed V, add it to the list that Get_Matching_Value
      --  will search if we run into a discriminant in one of the computations
      --  below.

      if Present (V) then
         Add_To_LValue_List (V);
      end if;

      --  Look at each piece of the record and find its value and alignment.
      --  Align to the needed alignment for this piece, add its size, and
      --  show what alignment we now have.

      while Present (Cur_Idx) and then Cur_Idx /= Idx loop
         New_Idx := Empty_Record_Info_Id;
         RI      := Record_Info_Table.Table (Cur_Idx);

         --  If we're reached a variant point, we have two cases.  We could
         --  be looking for a specific RI index, in which case we see which
         --  variant has that index and set it as next, or we're looking
         --  to compute the size of the record.

         if RI.Variants /= null and then Present (Idx) then
            New_Idx := Get_Variant_For_RI (RI, Idx);
         end if;

         if Present (New_Idx) then
            Cur_Idx := New_Idx;
         else
            Get_RI_Info (RI, V, For_Type, This_Size, Must_Align, This_Align);
            Total_Size := NSW_Add (Align_To (Total_Size, Cur_Align,
                                             Must_Align),
                                   This_Size);

            --  The resulting alignment is the minimum of this alignment
            --  and the maximum of the current alignment and what we had
            --  to align to.

            declare
               Cur_GT_Max : constant GL_Value :=
                 I_Cmp (Int_SGT, Cur_Align, Must_Align);
               Max_Cur_Must : constant GL_Value :=
                 Build_Select (Cur_GT_Max, Cur_Align, Must_Align);
               This_LT_Max : constant GL_Value :=
                 I_Cmp (Int_SLT, This_Align, Max_Cur_Must);

            begin
               Cur_Align :=
                 Build_Select (This_LT_Max, This_Align, Max_Cur_Must);
            end;

            Cur_Idx := RI.Next;
         end if;
      end loop;

      --  At this point, either Idx is not Present, meaning we were supposed
      --  to be at the end of the type, or it is, in which case we should
      --  have hit it.  If either is the case, we have an error where we're
      --  looking for a field in the wrong type.

      pragma Assert (Cur_Idx = Idx);

      --  Now we may have to do a final alignment.  If Idx is specified,
      --  use the alignment for that field.  Otherwise, use the alignment
      --  for the type.

      if Present (Idx) then
         Get_RI_Info (Record_Info_Table.Table (Idx), No_GL_Value, False,
                      This_Size, Must_Align, This_Align, Return_Size => False);
      elsif Present (TE) then
         Must_Align := Size_Const_Int (ULL (Get_Type_Alignment (TE)));
      end if;

      Pop_Debug_Freeze_Pos;
      return Align_To (Total_Size, Cur_Align, Must_Align);
   end Get_Record_Size_So_Far;

   -------------------------
   -- Emit_Field_Position --
   -------------------------

   function Emit_Field_Position
     (E : Entity_Id; V : GL_Value) return GL_Value
   is
      TE     : constant Entity_Id      := Full_Scope (E);
      R_Idx  : constant Record_Info_Id := Get_Record_Info (TE);
      F_Idx  : constant Field_Info_Id  := Get_Field_Info (E);
      FI     : Field_Info;
      Idx    : Record_Info_Id;
      RI     : Record_Info;
      Offset : GL_Value;

   begin
      --  If there's no field information for this field, the field
      --  position is undefined.

      if No (F_Idx) then
         return Get_Undef (Size_Type);
      end if;

      FI     := Field_Info_Table.Table (F_Idx);
      Idx    := FI.Rec_Info_Idx;
      RI     := Record_Info_Table.Table (Idx);
      Offset := Get_Record_Size_So_Far (TE, V, R_Idx, Idx);

      --  Offset now gives the offset from the start of the record to the
      --  piece that this field is in.  If this piece has a GNAT type, then
      --  the field is the entire piece and we have the offset.  If it's an
      --  LLVM type, we need to compute the offset within that type.

      if Present (RI.GNAT_Type) then
         return Offset;
      else
         declare
            Ordinal     : constant unsigned := unsigned (FI.Field_Ordinal);
            This_Offset : constant ULL      :=
              Offset_Of_Element (Module_Data_Layout, RI.LLVM_Type, Ordinal);

         begin
            return NSW_Add (Offset, Const_Int (Offset, This_Offset));
         end;
      end if;
   end Emit_Field_Position;

   -------------------------
   -- Record_Field_Offset --
   -------------------------

   function Record_Field_Offset
     (V : GL_Value; Field : Entity_Id) return GL_Value
   is
      F_Type     : constant Entity_Id      := Full_Etype (Field);
      CRC        : constant Entity_Id      :=
        Corresponding_Record_Component (Field);
      Our_Field  : constant Entity_Id      :=
        (if Present (CRC) and then Full_Etype (CRC) = F_Type
         then CRC else Field);
      Rec_Type   : constant Entity_Id      := Full_Scope (Our_Field);
      First_Idx  : constant Record_Info_Id := Get_Record_Info (Rec_Type);
      F_Idx      : constant Field_Info_Id  := Get_Field_Info (Our_Field);
      FI         : Field_Info;
      Our_Idx    : Record_Info_Id;
      Offset     : GL_Value;
      RI         : Record_Info;
      Result     : GL_Value;

   begin
      --  If the field information isn't present, this must be because we're
      --  referencing a field that's not in this variant and hence is a
      --  constraint error.  So return undefined.

      if No (F_Idx) then
         pragma Assert (Ekind (Rec_Type) = E_Record_Subtype);
         pragma Assert (Has_Discriminants (Rec_Type));
         return Get_Undef_Ref (Rec_Type);
      end if;

      FI       := Field_Info_Table.Table (F_Idx);
      Our_Idx  := FI.Rec_Info_Idx;
      Offset   := Get_Record_Size_So_Far (Rec_Type, V, First_Idx, Our_Idx);
      RI       := Record_Info_Table.Table (Our_Idx);

      --  If this is the "_parent" field, just do a conversion so we point
      --  to that type.  But add it to the LValue table in case there's
      --  a reference to its discrminant.

      if Chars (Our_Field) = Name_uParent then
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
     (TE : Entity_Id; For_Type : Boolean := False) return Nat
   is
      Complexity : Nat            := 0;
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
      return Get_Record_Size_So_Far (TE, V, Empty_Record_Info_Id,
                                     Empty_Record_Info_Id, For_Type);
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
         Ent : Entity_Id          := First_Component_Or_Discriminant (TE);

      begin
         while Present (Ent) loop
            if Original_Record_Component (Ent) = ORC then
               return Ent;
            end if;

            Next_Component_Or_Discriminant (Ent);
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
               --  We are to actually insert the field.  However, if we
               --  haven't set any information for this field, it may be
               --  a reference to a field that will cause Constraint_Error.
               --  If so, just don't do anything with it.

               if Present (F_Idx) then
                  Result := Insert_Value
                    (Result,
                     Build_Type_Conversion (Expression (Expr), F_Type),
                     unsigned (Field_Info_Table.Table (F_Idx).Field_Ordinal));
               else
                  --  Ensure we understand this case

                  pragma Assert (Ekind (Agg_Type) = E_Record_Subtype);
                  pragma Assert (Has_Discriminants (Agg_Type));
               end if;
            end if;
         end;

         Next (Expr);
      end loop;

      return Result;
   end Emit_Record_Aggregate;

   pragma Annotate (Xcov, Exempt_On, "Debug helpers");

   procedure Print_RI_Briefly (Ridx : Record_Info_Id);

   ----------------------
   -- Print_RI_Briefly --
   ----------------------

   procedure Print_RI_Briefly (Ridx : Record_Info_Id) is
      RI : constant Record_Info := Record_Info_Table.Table (Ridx);

   begin
      if Present (RI.GNAT_Type) then
         Write_Str ("GNAT Type = ");
         Write_Int (Nat (RI.GNAT_Type));
         Write_Str (": ");
         pg (Union_Id (RI.GNAT_Type));
      elsif Present (RI.LLVM_Type) then
         Dump_LLVM_Type (RI.LLVM_Type);
      end if;

   end Print_RI_Briefly;

   ----------------------
   -- Print_Field_Info --
   ----------------------

   procedure Print_Field_Info (E : Entity_Id) is
      F_Idx : constant Field_Info_Id  := Get_Field_Info (E);
      FI    : Field_Info;

   begin
      Write_Str ("Field ");
      Write_Int (Nat (E));
      Write_Str (": ");
      pg (Union_Id (E));
      Write_Str ("Scope = ");
      Write_Int (Nat (Full_Scope (E)));
      Write_Eol;
      if Present (F_Idx) then
         FI := Field_Info_Table.Table (F_Idx);
         Write_Str ("RI => ");
         Write_Int (Nat (FI.Rec_Info_Idx));
         Write_Str (", Ordinal = ");
         Write_Int (FI.Field_Ordinal);
         Write_Eol;
         Print_RI_Briefly (FI.Rec_Info_Idx);
      end if;
   end Print_Field_Info;

   -----------------------
   -- Print_Record_Info --
   -----------------------

   procedure Print_Record_Info (TE : Entity_Id) is

      function  Compare_FI     (E1, E2 : Entity_Id) return Boolean;

      ----------------
      -- Compare_FI --
      ----------------

      function Compare_FI (E1, E2 : Entity_Id) return Boolean is
         F1_Idx : constant Field_Info_Id  := Get_Field_Info (E1);
         F2_Idx : constant Field_Info_Id  := Get_Field_Info (E2);
         FI1    : Field_Info;
         FI2    : Field_Info;

      begin
         if Present (F1_Idx) and then No (F2_Idx) then
            return False;
         elsif No (F1_Idx) then
            return True;
         end if;

         FI1 := Field_Info_Table.Table (F1_Idx);
         FI2 := Field_Info_Table.Table (F2_Idx);

         if  FI1.Rec_Info_Idx < FI2.Rec_Info_Idx then
            return True;
         elsif FI1.Rec_Info_Idx > FI2.Rec_Info_Idx then
            return False;
         else
            return FI1.Field_Ordinal < FI2.Field_Ordinal;
         end if;
      end Compare_FI;

      type Entity_Array is array (Nat range <>) of Entity_Id;
      Fields         : Entity_Array (0 .. Count_Entities (TE));
      Next_Field_Idx : Nat       := Fields'First;
      Field          : Entity_Id := First_Component_Or_Discriminant (TE);

   begin
      while Present (Field) loop
         Fields (Next_Field_Idx) := Field;
         Next_Field_Idx          := Next_Field_Idx + 1;
         Next_Component_Or_Discriminant (Field);
      end loop;

      declare
         subtype Our_Index is Nat range 0 .. Next_Field_Idx - 1;
         subtype Our_Fields_Type is Entity_Array (Our_Index);
         Our_Fields : Our_Fields_Type := Fields (Our_Fields_Type'Range);

         procedure Sort is new Ada.Containers.Generic_Constrained_Array_Sort
           (Our_Index, Entity_Id, Our_Fields_Type, Compare_FI);

         procedure Print_One_RI
           (Ridx : Record_Info_Id; Prefix : String := "");
         procedure Print_RI_Chain
           (Start : Record_Info_Id; Prefix : String := "");

         ------------------
         -- Print_One_RI --
         ------------------
         procedure Print_One_RI
           (Ridx : Record_Info_Id; Prefix : String := "")
         is
            RI                 : constant Record_Info :=
              Record_Info_Table.Table (Ridx);
            F_Idx              : Field_Info_Id;
            FI                 : Field_Info;
            New_Prefix         : String (Prefix'First .. Prefix'Last + 4);
            Var_Node, Choice   : Node_Id;

         begin
            Write_Str (Prefix);
            Write_Str ("RI ");
            Write_Int (Nat (Ridx));
            Write_Eol;
            if Present (RI.GNAT_Type) then
               Write_Str (Prefix);
               Write_Str ("GNAT Type = ");
               Write_Int (Nat (RI.GNAT_Type));
               Write_Eol;
               Write_Str (Prefix);
               Sprint_Node (RI.GNAT_Type);
               Write_Eol;
            elsif Present (RI.LLVM_Type) then
               Dump_LLVM_Type (RI.LLVM_Type);
            end if;

            if RI.Use_Max_Size then
               Write_Str  (Prefix);
               Write_Line ("Use max size");
            end if;

            for F of Our_Fields loop
               F_Idx := Get_Field_Info (F);
               FI := Field_Info_Table.Table (F_Idx);
               if Ridx = FI.Rec_Info_Idx then
                  Write_Str (Prefix);
                  Write_Str ("    Field");
                  if Present (RI.LLVM_Type) then
                     Write_Str ("@");
                     Write_Int (FI.Field_Ordinal);
                  end if;

                  Write_Str (" ");
                  Write_Int (Nat (F));
                  Write_Str (": ");
                  Sprint_Node (F);
                  Write_Eol;
               end if;
            end loop;

            if RI.Variants /= null then
               Write_Eol;
               Write_Str   (Prefix);
               Write_Str   ("Variants for ");
               Sprint_Node (RI.Variant_Expr);
               Write_Eol;

               New_Prefix (Prefix'Range)   := Prefix;
               New_Prefix (Prefix'Last + 1) := ' ';
               New_Prefix (Prefix'Last + 2) := ' ';
               New_Prefix (Prefix'Last + 3) := ' ';
               New_Prefix (Prefix'Last + 4) := ' ';

               Var_Node := First (RI.Variant_List);
               for Variant of RI.Variants.all loop
                  Write_Str (New_Prefix);
                  Write_Str ("when ");
                  Choice := First (Discrete_Choices (Var_Node));
                  loop
                     Sprint_Node (Choice);
                     Next (Choice);
                     exit when No (Choice);
                     Write_Str (" | ");
                  end loop;

                  Write_Eol;
                  if Present (Variant) then
                     Print_RI_Chain (Variant, New_Prefix);
                  end if;

                  Next (Var_Node);
               end loop;
            end if;
         end Print_One_RI;

         --------------------
         -- Print_RI_Chain --
         --------------------

         procedure Print_RI_Chain
           (Start : Record_Info_Id; Prefix : String := "")
         is
            Idx : Record_Info_Id := Start;

         begin
            while Present (Idx) loop
               Print_One_RI (Idx, Prefix);
               Idx := Record_Info_Table.Table (Idx).Next;
            end loop;
         end Print_RI_Chain;

      begin
         Sort (Our_Fields);
         Print_RI_Chain (Get_Record_Info (TE));
      end;

   end Print_Record_Info;

begin
   --  Make a dummy entry in the record and field tables, so the
   --  "Empty" entry is never used.

   Record_Info_Table.Increment_Last;
   Field_Info_Table.Increment_Last;
end GNATLLVM.Records;
