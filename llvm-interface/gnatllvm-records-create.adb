------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

with Ada.Containers.Generic_Sort;

with Debug;      use Debug;
with Errout;     use Errout;
with Exp_Util;   use Exp_Util;
with Get_Targ;   use Get_Targ;
with Nlists;     use Nlists;
with Output;     use Output;
with Repinfo;    use Repinfo;
with Sem_Aux;    use Sem_Aux;
with Sem_Eval;   use Sem_Eval;
with Snames;     use Snames;
with Sprint;     use Sprint;
with Table;      use Table;
with Uintp.LLVM; use Uintp.LLVM;

with LLVM.Core;  use LLVM.Core;

with GNATLLVM.Types.Create; use GNATLLVM.Types.Create;
with GNATLLVM.Utils;        use GNATLLVM.Utils;

package body GNATLLVM.Records.Create is

   function Max_Discriminant (TE : Entity_Id) return Int
     with Pre => Is_Record_Type (TE);
   --  Return the highest value of Discriminant_Number

   function Find_Field_In_Entity_List
     (F         : Entity_Id;
      TE        : Entity_Id;
      Cur_Field : in out Entity_Id) return Entity_Id
     with Pre  => Ekind_In (F, E_Discriminant, E_Component)
     and then Is_Record_Type (TE);
   --  Find a field in the entity list of TE that has the same
   --  Original_Record_Component as F and return it if so.  Cur_Field
   --  is used to cache the last field tested to avoid quadratic behavior
   --  since we'll be requesting fields in roughly (but not exactly!)
   --  the same order as they are in the list.

   function Variant_Alignment (Var_Part : Node_Id) return Nat
     with Pre => Present (Var_Part);
   --  Compute the alignment of the variant at Var_Part, which is the
   --  maximum size of any field in the variant.  We recurse through
   --  any nested variants.

   -------------------------------
   -- Find_Field_In_Entity_List --
   -------------------------------

   function Find_Field_In_Entity_List
     (F         : Entity_Id;
      TE        : Entity_Id;
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
         exit when ORC (Cur_Field) = ORC (F);
         Next_Component_Or_Discriminant (Cur_Field);
      end loop;

      if No (Cur_Field) then
         Cur_Field := First_Component_Or_Discriminant (TE);
         while Cur_Field /= Initial_Cur_Field loop
            exit when ORC (Cur_Field) = ORC (F);
            Next_Component_Or_Discriminant (Cur_Field);
         end loop;
      end if;

      return (if   Present (Cur_Field) and then ORC (Cur_Field) = ORC (F)
              then Cur_Field else Empty);

   end Find_Field_In_Entity_List;

   -----------------------
   --  Max_Discriminant --
   -----------------------

   function Max_Discriminant (TE : Entity_Id) return Int is
      F     : Entity_Id := First_Component_Or_Discriminant (TE);

   begin
      return Max : Int := 0 do
         while Present (F) loop
            if Ekind (F) = E_Discriminant
              and then Discriminant_Number (F) > Max
            then
               Max := UI_To_Int (Discriminant_Number (F));
            end if;

            Next_Component_Or_Discriminant (F);
         end loop;
      end return;
   end Max_Discriminant;

   -----------------------
   -- Variant_Alignment --
   -----------------------

   function Variant_Alignment (Var_Part : Node_Id) return Nat is
      Variant : Node_Id := First_Non_Pragma (Variants (Var_Part));

   begin
      return Align : Nat := 0 do
         while Present (Variant) loop
            declare
               Comp_List      : constant Node_Id := Component_List (Variant);
               Nested_Variant : constant Node_Id := Variant_Part (Comp_List);
               Comp_Def       : Node_Id          :=
                 First_Non_Pragma (Component_Items (Component_List (Variant)));

            begin
               while Present (Comp_Def) loop
                  Align := Nat'Max (Align,
                                    Effective_Field_Alignment
                                      (Defining_Identifier (Comp_Def)));

                  Next_Non_Pragma (Comp_Def);
               end loop;

               if Present (Nested_Variant) then
                  Align := Nat'Max (Align, Variant_Alignment (Nested_Variant));
               end if;
            end;

            Next_Non_Pragma (Variant);
         end loop;
      end return;
   end Variant_Alignment;

   ------------------------
   -- Create_Record_Type --
   ------------------------

   function Create_Record_Type (TE : Entity_Id) return Type_T is

      type Field_Info_Id_Array is array (Nat range <>) of Field_Info_Id;

      --  This function creates a record type and the description of that
      --  record.  Note that this function does not itself lay out the record.
      --  We don't actually lay out record in that sense.  Instead, we create
      --  Record_Info structures whose chaining describe the record structure
      --  and Field_Info structures, one for each field, showing where each
      --  field is located in the record.  We then compute any information
      --  we need on the fly, mostly in Get_Record_Size_So_Far.

      --  For each "piece" of the record (for variant records, the common
      --  portion and each variant), we first list all the fields in that
      --  part, then sort the list to deal with record rep clauses and the
      --  few cases when we reorder records, then lay out the fields into
      --  Record_Info pieces.  We start with a simple data structure that
      --  records information about the field to add and its location
      --  within the record.

      type Added_Field is record
         F            : Entity_Id;
         --  Entity of the field that we're adding

         Seq          : Nat;
         --  An ordinal representing the sequence number in which we add
         --  fields because, all else being equal, we want to keep fields
         --  in that order.

         Par_Depth    : Nat;
         --  The depth of parent reference (how many parents this is inside)

         Var_Depth    : Nat;
         --  The depth of static variants (for the subtype case) that we
         --  are because we must not move non-repped field across that
         --  boundary.

         Var_Align    : Nat;
         --  The alignment of the variant, if that depth is nonzero.

         Bitpos       : Uint;
         --  The bit position specified (or to be used, in the case of
         --  packed records), or No_Uint if none.

         Size         : Uint;
         --  Size in bits specified (or to be used, in the case of packed
         --  records), or No_Uint if none.
      end record;

      package Added_Field_Table is new Table.Table
        (Table_Component_Type => Added_Field,
         Table_Index_Type     => Int,
         Table_Low_Bound      => 1,
         Table_Initial        => 20,
         Table_Increment      => 5,
         Table_Name           => "Added_Field_Table");

      --  We maintain a table of all the LLVM types that will be put in a
      --  LLVM struct type in an RI.  These are both for actual and padding
      --  fields.

      package LLVM_Types is new Table.Table
        (Table_Component_Type => Type_T,
         Table_Index_Type     => Int,
         Table_Low_Bound      => 0,
         Table_Initial        => 20,
         Table_Increment      => 5,
         Table_Name           => "LLVM_Types");

      --  We maintain a stack for the depth of variants that we're in.
      --  For each, we indicate whether we're in a dynamic or static variant.
      --  By "static", we mean the case where we have a static subtype,
      --  so we know which fields will be present in the record.

      type Variant_Stack_Info is record
         Align      : Nat;
         Is_Static  : Boolean;
      end record;

      package Variant_Stack is new Table.Table
        (Table_Component_Type => Variant_Stack_Info,
         Table_Index_Type     => Int,
         Table_Low_Bound      => 1,
         Table_Initial        => 2,
         Table_Increment      => 1,
         Table_Name           => "Variant_Stack");

      Use_Packed     : constant Boolean :=
        Is_Packed (TE) or else Has_Specified_Layout (TE);
      --  Says to use an LLVM packed struct

      Prev_Idx       : Record_Info_Id   := Empty_Record_Info_Id;
      --  The previous index of the record table entry, if any

      First_Idx      : Record_Info_Id   := Empty_Record_Info_Id;
      --  The first index used by the current record fragment construction

      Overlap_Idx    : Record_Info_Id   := Empty_Record_Info_Id;
      --  The index of the overlap component of this variant part, if any

      Cur_Idx        : Record_Info_Id;
      --  The index of the record table entry we're building

      Cur_RI_Pos     : ULL              := 0;
      --  Current position into this R

      Par_Depth      : Int              := 0;
      --  Nesting depth into parent records

      RI_Align       : Nat              := 0;
      --  If nonzero, an alignment to assign to the next RI built for an
      --  LLVM type.

      RI_Position    : ULL              := 0;
      --  If nonzero, an alignment to assign to the next RI built for an
      --  LLVM type.

      RI_Is_Overlap  : Boolean          := False;
      --  If True, the next RI built is an overlap RI for a variant

      Cur_Field      : Entity_Id        := Empty;
      --  Used for a cache in Find_Field_In_Entity_List to avoid quadratic
      --  behavior.

      Split_Align    : Nat              := Get_Maximum_Alignment;
      --  We need to split an LLVM fragment type if the alignment of the
      --  next field is greater than both this and Last_Align.  This occurs
      --  for variant records; see details there.  It also occurs for the
      --  same reason after a variable-size field.

      GT             :  GL_Type         :=
        Default_GL_Type (TE, Create => False);
      --  The GL_Type for this record type

      Aliased_Fields : Boolean          := False;
      --  Indicates that at least one field is aliased

      LLVM_Type      : Type_T;
      --  The LLVM type for this record type

      Field          : Entity_Id;
      --  Temporary for loop over components and discriminants

      Discrim_FIs    : Field_Info_Id_Array :=
        (1 .. Max_Discriminant (Full_Base_Type (TE)) => Empty_Field_Info_Id);
      --  In entry J, we record the Field_Info corresponding to the
      --  discriminant number J.  We use this for record subtypes of
      --  derived types.

      procedure Add_RI
        (T                : Type_T                      := No_Type_T;
         F_GT             : GL_Type                     := No_GL_Type;
         Align            : Nat                         := 0;
         Position         : ULL                         := 0;
         Variant_List     : List_Id                     := No_List;
         Variant_Expr     : Node_Id                     := Empty;
         Variants         : Record_Info_Id_Array_Access := null;
         Overlap_Variants : Record_Info_Id_Array_Access := null);
      --  Add a Record_Info into the table, chaining it as appropriate

      procedure Add_FI
        (E              : Entity_Id;
         RI_Idx         : Record_Info_Id;
         F_GT           : in out GL_Type;
         Ordinal        : Nat     := 0;
         First_Bit      : Uint    := No_Uint;
         Num_Bits       : Uint    := No_Uint;
         Array_Bitfield : Boolean := False)
        with Pre => Ekind_In (E, E_Discriminant, E_Component);
      --  Add a Field_Info info the table, if appropriate, and set
      --  the field to point to it.  Update F_GT if we used a matching field.

      procedure Add_Field (E : Entity_Id)
        with Pre => Ekind_In (E, E_Discriminant, E_Component);
      --  Add one field to the above data

      procedure Process_Fields_To_Add;
      --  Create RI entries for the fields we've added above

      procedure Add_Fields (Def_Ident : Entity_Id)
        with Pre => Is_Record_Type (Def_Ident);
      --  Add all fields of Def_Ident to the above data, either the component
      --  or the extension components, but recursively add parent components.

      procedure Flush_Current_Types;
      --  If there are any types in the Types array, create a record
      --  description for them.

      ------------
      -- Add_RI --
      ------------

      procedure Add_RI
        (T                : Type_T                      := No_Type_T;
         F_GT             : GL_Type                     := No_GL_Type;
         Align            : Nat                         := 0;
         Position         : ULL                         := 0;
         Variant_List     : List_Id                     := No_List;
         Variant_Expr     : Node_Id                     := Empty;
         Variants         : Record_Info_Id_Array_Access := null;
         Overlap_Variants : Record_Info_Id_Array_Access := null) is

      begin
         --  It's tempting to set Next to the next entry that we'll be using,
         --  but we may not actually end up using that one.

         Record_Info_Table.Table (Cur_Idx) :=
           (LLVM_Type        => T,
            GT               => F_GT,
            Align            => Align,
            Position         => Position,
            Next             => Empty_Record_Info_Id,
            Variant_List     => Variant_List,
            Variant_Expr     => Variant_Expr,
            Variants         => Variants,
            Overlap_Variants => Overlap_Variants);

         --  If we've had a previous RI for this part, link us to it.
         --  Otherwise, if this is an overlap RI, indicate it as so and
         --  start the chain over.  If not, indicate the first index in our
         --  chain.

         if Present (Prev_Idx) then
            Record_Info_Table.Table (Prev_Idx).Next := Cur_Idx;
         elsif RI_Is_Overlap then
            Overlap_Idx   := Cur_Idx;
            Cur_Idx       := Empty_Record_Info_Id;
            RI_Is_Overlap := False;
         else
            First_Idx     := Cur_Idx;
         end if;

         Prev_Idx := Cur_Idx;
         Record_Info_Table.Increment_Last;
         Cur_Idx := Record_Info_Table.Last;
      end Add_RI;

      ------------
      -- Add_FI --
      ------------

      procedure Add_FI
        (E              : Entity_Id;
         RI_Idx         : Record_Info_Id;
         F_GT           : in out GL_Type;
         Ordinal        : Nat     := 0;
         First_Bit      : Uint    := No_Uint;
         Num_Bits       : Uint    := No_Uint;
         Array_Bitfield : Boolean := False)
      is
         Matching_Field : Entity_Id;

      begin
         --  If this field really isn't in the record we're working on, it
         --  must be in a parent.  So it was correct to allocate space for
         --  it, but let the record description be from the type that it's
         --  actually in.  And finally,
         --
         --  If we're using a matching field, update F_GT to its type.

         Field_Info_Table.Append
           ((Rec_Info_Idx   => RI_Idx,
             GT             => F_GT,
             Field_Ordinal  => Ordinal,
             First_Bit      => First_Bit,
             Num_Bits       => Num_Bits,
             Array_Bitfield => Array_Bitfield));

         --  The fields in the entity list for this type are almost, but
         --  not quite, in the same order as in the component list, so we
         --  have to search for a field in that list with the same
         --  Original_Record_Component as this field.

         if Full_Scope (E) = TE then
            Set_Field_Info (E, Field_Info_Table.Last);
         else
            Matching_Field := Find_Field_In_Entity_List (E, TE, Cur_Field);
            if Present (Matching_Field) then
               Set_Field_Info (Matching_Field, Field_Info_Table.Last);
               F_GT := Full_GL_Type (Matching_Field);

            --  If this is a hidden discriminant and we haven't yet found a
            --  place to save the value, save it in Discriminant_FIs.

            elsif Ekind (E) = E_Discriminant
              and then Is_Completely_Hidden (E)
            then
               Discrim_FIs (UI_To_Int (Discriminant_Number (E))) :=
                 Field_Info_Table.Last;
            end if;
         end if;
      end Add_FI;

      ----------------
      -- Add_Fields --
      ----------------

      procedure Add_Fields (Def_Ident : Entity_Id) is
         Rec_Type     : constant Entity_Id := Full_Base_Type (Def_Ident);
         --  The base type, which we use to get the record order from

         Sub_Rec_Type : constant Entity_Id :=
           (if Rec_Type = Def_Ident then Empty else Def_Ident);
         --  Present if we have to search the field list of a record subtype

         Rec_Field    : Entity_Id          := Empty;
         --  Cache used to limit quadratic behavior

         function Find_Choice (N : Node_Id; Alts : List_Id) return Node_Id
           with Pre => Is_Static_Expression (N) and then Present (Alts);
         --  N is a static expression and Alts is a list of alternatives.
         --  Return which alternate has a Choice that covers N.

         procedure Add_Component_List
           (List : Node_Id; From_Rec : Entity_Id; Parent : Boolean := False)
           with Pre => (No (List) or else Nkind (List) = N_Component_List)
                       and then (No (From_Rec)
                                   or else Is_Record_Type (From_Rec));
         --  Add fields in List.  If From_Rec is Present, instead
         --  of adding the actual field, add the field of the same
         --  name from From_Rec.  If Parent is true, only add the
         --  parent record, otherwise, add all records except the parent.

         function Choices_To_SO_Ref
           (Variant : Node_Id; Discrim : Entity_Id) return SO_Ref
           with Pre => Present (Variant);
         --  Given an alternative for a variant record, return an SO_Ref
         --  corresponding to an expression that's True when that variant
         --  is present.  This is a function of the discriminant (Discrim)
         --  and constants.

         -----------------
         -- Find_Choice --
         -----------------

         function Find_Choice (N : Node_Id; Alts : List_Id) return Node_Id is
            Value       : constant Uint := Expr_Rep_Value (N);
            Alt, Choice : Node_Id;
            Low, High   : Uint;

         begin
            Alt := First_Non_Pragma (Alts);
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

               Next_Non_Pragma (Alt);
            end loop;

            return Empty;
         end Find_Choice;

         ------------------------
         -- Add_Component_List --
         ------------------------

         procedure Add_Component_List
           (List : Node_Id; From_Rec : Entity_Id; Parent : Boolean := False)
         is
            Var_Part           : constant Node_Id   :=
              (if Present (List) then Variant_Part (List) else Empty);
            Discrim            : constant Entity_Id :=
              (if Present (Var_Part) then Name (Var_Part) else Empty);
            Constraining_Expr  : constant Node_Id   :=
              (if   Present (From_Rec) and then Present (Var_Part)
               then (Get_Discriminant_Constraint (From_Rec, Entity (Discrim)))
               else Empty);
            Variant_Expr       : constant Entity_Id :=
              (if    Present (Constraining_Expr) then Constraining_Expr
               elsif Present (Discrim) then Discrim else Empty);
            Static_Constraint  : constant Boolean   :=
              Present (Constraining_Expr)
                and then Is_Static_Expression (Constraining_Expr);
            Variant_Align      : constant Nat       :=
              (if Present (Var_Part) then Variant_Alignment (Var_Part) else 0);
            Var_Array          : Record_Info_Id_Array_Access;
            Overlap_Var_Array  : Record_Info_Id_Array_Access;
            Saved_Cur_Idx      : Record_Info_Id;
            Saved_Prev_Idx     : Record_Info_Id;
            Saved_First_Idx    : Record_Info_Id;
            Saved_Overlap_Idx  : Record_Info_Id;
            Component_Def      : Node_Id;
            Field              : Entity_Id;
            Field_To_Add       : Entity_Id;
            Variant            : Node_Id;
            J                  : Nat;

         begin
            --  Return quickly if nothing to do.  Otherwise, walk the
            --  component list.

            if No (List) then
               return;
            end if;

            Component_Def := First_Non_Pragma (Component_Items (List));
            while Present (Component_Def) loop
               Field := Defining_Identifier (Component_Def);
               if Parent = (Chars (Field) = Name_uParent) then
                  Field_To_Add := Field;
                  if Present (From_Rec) then
                     Field_To_Add :=
                       Find_Field_In_Entity_List (Field, From_Rec, Rec_Field);
                  end if;

                  if Present (Field_To_Add) then
                     if Chars (Field_To_Add) = Name_uParent then
                        Par_Depth := Par_Depth + 1;
                        Add_Fields (Full_Etype (Field_To_Add));
                        Par_Depth := Par_Depth - 1;
                     end if;

                     Add_Field (Field_To_Add);
                  end if;
               end if;

               Next_Non_Pragma (Component_Def);
            end loop;

            --  Done if we're just asking for the parent field or if
            --  there are no variants in this record.

            if Parent or else No (Var_Part) then
               return;
            end if;

            --  Otherwise process variants. If we statically constrain the
            --  variant, see which variant is being referenced and output
            --  that one.  Walk the proper variant here to verify that
            --  ever.  Otherwise, set up for the variant, make the entres
            --  for each variant, and then create the RI for the variant.
            --  In the case of an error, there may not be a match.

            if Static_Constraint then
               Variant := Find_Choice (Constraining_Expr, Variants (Var_Part));
               if Present (Variant) then
                  Variant_Stack.Append ((Variant_Align, True));
                  Add_Component_List (Component_List (Variant), From_Rec);
                  Variant_Stack.Decrement_Last;
               end if;
               return;
            end if;

            Variant := First_Non_Pragma (Variants (Var_Part));
            Process_Fields_To_Add;
            Flush_Current_Types;
            Set_Is_Nonnative_Type (TE);
            Saved_Cur_Idx     := Cur_Idx;
            Saved_Prev_Idx    := Prev_Idx;
            Saved_First_Idx   := First_Idx;
            Saved_Overlap_Idx := Overlap_Idx;
            J                 := 1;
            Var_Array         := new
              Record_Info_Id_Array'(1 .. List_Length_Non_Pragma
                                      (Variants (Var_Part))
                                      => Empty_Record_Info_Id);
            Overlap_Var_Array := new
              Record_Info_Id_Array'(1 .. List_Length_Non_Pragma
                                      (Variants (Var_Part))
                                      => Empty_Record_Info_Id);

            while Present (Variant) loop
               Variant_Stack.Append ((Variant_Align, False));
               First_Idx   := Empty_Record_Info_Id;
               Overlap_Idx := Empty_Record_Info_Id;
               if Present (Component_Items (Component_List (Variant))) then
                  Record_Info_Table.Increment_Last;
                  Prev_Idx      := Empty_Record_Info_Id;
                  Cur_Idx       := Record_Info_Table.Last;
                  Split_Align   := Variant_Align;
                  Add_Component_List (Component_List (Variant), From_Rec);
                  Process_Fields_To_Add;
                  Flush_Current_Types;
               end if;

               Set_Present_Expr (Variant,
                                 Choices_To_SO_Ref (Variant, Variant_Expr));
               Var_Array (J)         := First_Idx;
               Overlap_Var_Array (J) := Overlap_Idx;
               J                     := J + 1;
               Variant_Stack.Decrement_Last;
               Next_Non_Pragma (Variant);
            end loop;

            Prev_Idx    := Saved_Prev_Idx;
            Cur_Idx     := Saved_Cur_Idx;
            First_Idx   := Saved_First_Idx;
            Overlap_Idx := Saved_Overlap_Idx;
            Add_RI (Variant_List     => Variants (Var_Part),
                    Variants         => Var_Array,
                    Overlap_Variants => Overlap_Var_Array,
                    Variant_Expr     => Variant_Expr,
                    Align            => Variant_Align);
         end Add_Component_List;

         -----------------------
         -- Choices_To_SO_Ref --
         -----------------------

         function Choices_To_SO_Ref
           (Variant : Node_Id; Discrim : Entity_Id) return SO_Ref
         is
            Discrim_SO : constant SO_Ref :=
              Annotated_Value (Emit_Expr (Discrim));
            Choice     : Node_Id;
            Expr       : SO_Ref;
            This_Expr  : SO_Ref;
            Low, High  : Uint;

         begin
            Choice := First (Discrete_Choices (Variant));

            --  For "others", this is always True

            if Nkind (Choice) = N_Others_Choice then
               return Uint_1;
            end if;

            --  Otherwise, start with an expression of False, then fill in
            --  each choice.

            Expr := Uint_0;
            while Present (Choice) loop
               Decode_Range (Choice, Low, High);
               if Low = High then
                  This_Expr := Create_Node (Eq_Expr, Discrim_SO, Low);
               elsif High > Low then
                  This_Expr :=
                    Create_Node (Truth_And_Expr,
                                 Create_Node (Ge_Expr, Discrim_SO, Low),
                                 Create_Node (Le_Expr, Discrim_SO, High));
               else
                  This_Expr := Uint_0;
               end if;

               Expr := (if    Expr = 0 then This_Expr
                        elsif This_Expr = 0 then Expr
                        else  Create_Node (Truth_Or_Expr, Expr, This_Expr));
               Next (Choice);
            end loop;

            return Expr;
         end Choices_To_SO_Ref;

         Field             : Entity_Id;
         Field_To_Add      : Entity_Id;
         Outer_Field       : Entity_Id;
         Record_Definition : Node_Id;
         Components        : Node_Id;

      --  Start of processing for Add_Fields

      begin
         --  Get the record definition and component list

         Record_Definition :=
           Type_Definition (Declaration_Node (Rec_Type));
         if Nkind (Record_Definition) = N_Derived_Type_Definition then
            Record_Definition := Record_Extension_Part (Record_Definition);
         end if;

         Components := Component_List (Record_Definition);

         --  Add the parent field, which means adding all subfields of the
         --  parent.  We can't just rely on field sorting to do this because
         --  the parent record might have variants.

         Add_Component_List (Components, Sub_Rec_Type, True);

         --  If there are discriminants, process them.  But
         --  ignore discriminants that are already in a parent type.

         if Has_Discriminants (Rec_Type)
           and then not Is_Unchecked_Union (Rec_Type)
         then
            Field := First_Stored_Discriminant (Rec_Type);
            while Present (Field) loop
               Field_To_Add := Field;
               if Present (Sub_Rec_Type)
                 and then not Is_Completely_Hidden (Field)
               then
                  Field_To_Add :=
                    Find_Field_In_Entity_List (Field, Sub_Rec_Type, Rec_Field);
               end if;

               Outer_Field :=
                 Find_Field_In_Entity_List (Field_To_Add, TE, Cur_Field);

               --  If this is a tagged type, ignore if this was already in
               --  a _parent field.

               if Is_Tagged_Type (Rec_Type)
                 and then Present (Corresponding_Discriminant (Field))
               then
                  null;
               elsif Present (Field_To_Add)
                 and then (No (Outer_Field)
                             or else No (Get_Field_Info (Outer_Field)))
               then
                  Add_Field (Field_To_Add);

               --  If this field is a hidden discriminant, we need to
               --  allow space for it in the record even though we
               --  won't have a field for it.  Handle that case here.
               --  The test for scope is testing whether we'll be
               --  setting the field info for the field.

               elsif Is_Completely_Hidden (Field_To_Add)
                 and then Scope (Field_To_Add) /= TE
               then
                  Add_Field (Field_To_Add);
               end if;

               Next_Stored_Discriminant (Field);
            end loop;
         end if;

         --  Then add everything else

         Add_Component_List (Components, Sub_Rec_Type);

      end Add_Fields;

      -------------------------
      -- Flush_Current_Types --
      -------------------------

      procedure Flush_Current_Types is
         Last_Type : constant Int := LLVM_Types.Last;

      begin
         if Last_Type >= 0 then
            Add_RI (T        => Build_Struct_Type
                      (Type_Array (LLVM_Types.Table (0 .. Last_Type)),
                       Packed => Use_Packed),
                    Align    => RI_Align,
                    Position => RI_Position);
            RI_Align    := 0;
            RI_Position := 0;
            LLVM_Types.Set_Last (-1);
         end if;

         Cur_RI_Pos := 0;
      end Flush_Current_Types;

      ---------------
      -- Add_Field --
      ---------------

      procedure Add_Field (E : Entity_Id) is
         Clause      : constant Node_Id   := Component_Clause (E);
         R_TE        : constant Entity_Id := Full_Scope (E);
         Def_GT      : constant GL_Type   := Default_GL_Type (Full_Etype (E));
         F_GT        : GL_Type            := Full_GL_Type (E);
         Align       : constant Nat       := Get_Type_Alignment (F_GT);
         Bit_Align   : constant Nat       := Align * BPU;
         Parent_TE   : constant Entity_Id :=
           (if   Present (Parent_Subtype (R_TE))
            then Full_Parent_Subtype (R_TE) else Empty);
         Atomic      : constant Boolean   :=
           Is_Atomic_Or_VFA (E) or else Is_Atomic_Or_VFA (F_GT);
         Independent : constant Boolean   :=
           Is_Independent (E) or else Is_Independent (F_GT);
         Error_Str   : constant String    :=
           (if    Atomic                  then "atomic &"
            elsif Is_Aliased (E)          then "aliased &"
            elsif Independent             then "independent &"
            elsif Strict_Alignment (F_GT) then "& with aliased or tagged part"
            else  "");
         Pos         : Uint               :=
           (if Present (Clause) then Component_Bit_Offset (E) else No_Uint);
         Size        : Uint               :=
           (if   Unknown_Esize (E) then No_Uint
            else Validate_Size (E, Def_GT, Esize (E),
                                Zero_Allowed => Present (Clause)));
         Var_Depth   : Int                := 0;
         Var_Align   : Nat                := 0;

      begin
         --  If we've pushed the variant stack and the top entry is static,
         --  record the depth and alignment.

         if Variant_Stack.Last /= 0
           and then Variant_Stack.Table (Variant_Stack.Last).Is_Static
         then
            Var_Depth := Variant_Stack.Last;
            Var_Align := Variant_Stack.Table (Variant_Stack.Last).Align;
         end if;

         --  If this is the '_parent' field, we make a dummy entry
         --  and handle it specially later.

         if Chars (E) = Name_uParent then
            Add_FI (E, Get_Record_Info_N (TE), F_GT);
            return;

         --  Ensure the position does not overlap with the parent subtype,
         --  if there is one.  This test is omitted if the parent of the
         --  tagged type has a full rep clause since, in this case,
         --  component clauses are allowed to overlay the space allocated
         --  for the parent type and the front-end has checked that there
         --  are no overlapping components.

         elsif Present (Clause) and then Present (Parent_TE)
           and then not Is_Fully_Repped_Tagged_Type (Parent_TE)
           and then not Is_Dynamic_Size (Default_GL_Type (Parent_TE))
           and then Pos < Esize (Parent_TE)
         then
            Error_Msg_NE_Num
              ("position for & must be beyond parent, minimum allowed is ^",
               Position (Clause), E, Esize (Parent_TE) / BPU);
            Pos := No_Uint;

          --  If the position is not a multiple of the storage unit, then
          --  error out and reset the position.

         elsif Present (Clause) and then Pos mod BPU /= 0
           and then Error_Str'Length > 0
         then
            Error_Msg_NE ("position for " & Error_Str &
                            " must be multiple of Storage_Unit",
                          First_Bit (Clause), E);
            Pos := No_Uint;

         --  Likewise, if a position is specified and it's not a multiple
         --  of the alignment of the type.

         elsif Present (Clause) and then Pos mod Bit_Align /= 0
           and then Error_Str'Length > 0
         then
            Error_Msg_NE_Num
              ("position for " & Error_Str & " must be multiple of ^",
                  First_Bit (Clause), E, Align);
            Error_Msg_NE_Num
              ("\\because alignment of its type& is ^",
                  First_Bit (Clause), Full_Etype (E), Align);
            Pos := No_Uint;
         end if;

         --  If this is to be an atomic field, verify that we can make it one

         if Is_Atomic_Or_VFA (E) then
            Check_OK_For_Atomic_Type (F_GT, E);
         end if;

         --  If the size is not a multiple of the storage unit, then error
         --  out and reset the size.

         if Present (Clause) and then Size /= No_Uint
           and then Size mod BPU /= 0 and then Error_Str'Length > 0
         then
            Error_Msg_NE ("size for " & Error_Str &
                            " must be multiple of Storage_Unit",
                          Last_Bit (Clause), E);
            Size := No_Uint;

         --  If a size is specified and constant and is lower than
         --  the size of the type or larger than the type and the field
         --  is atomic or aliased, we may have to give an error. (The
         --  variable case is handled above.)

         elsif Present (Clause) and then Size /= No_Uint
           and then not Is_Dynamic_Size (Def_GT)
           and then (Size_Const_Int (Size) < Get_Type_Size_In_Bits (Def_GT)
                       or else ((Is_Aliased (E) or else Atomic)
                                and then (Get_Type_Size_In_Bits (Def_GT)) <
                                  Size_Const_Int (Size)))
           and then Error_Str'Length > 0
         then
            if Atomic or else Is_Aliased (E) then
               Error_Msg_NE_Num
                 ("size for " & Error_Str & " must be ^",
                  Last_Bit (Clause), E, Esize (Full_Etype (E)));
            else
               Error_Msg_NE_Num
                 ("size for " & Error_Str & " must be at least ^",
                  Last_Bit (Clause), E, Esize (Full_Etype (E)));
            end if;

            Size := No_Uint;
         end if;

         --  Now add field to table

         Added_Field_Table.Append ((E, Added_Field_Table.Last + 1, Par_Depth,
                                    Var_Depth, Var_Align, Pos, Size));
      end Add_Field;

      ---------------------------
      -- Process_Fields_To_Add --
      ---------------------------

      procedure Process_Fields_To_Add is
         In_Variant          : constant Boolean := Variant_Stack.Last /= 0;
         --  True if we're processing inside a variant, either static
         --  or dynamic.

         In_Dynamic_Variant  : constant Boolean := In_Variant
           and then not Variant_Stack.Table (Variant_Stack.Last).Is_Static;
         --  True if we're inside a dynamic variant

         Last_Var_Depth      : Int              := 0;
         --  The last variant depth that we saw for a field; used to indicate
         --  when the depth changes.

         Had_Non_Repped      : Boolean          := False;
         --  True once we saw a non-repped field; used to ensure that all
         --  non-repped fields as positions after all repped fields.

         Packed_Field_Bitpos : Uint             := No_Uint;
         Forced_Pos          : ULL              := 0;
         --  If nonzero, a position to force the next field to

         Bitfield_Start_Pos  : Uint             := No_Uint;
         Bitfield_End_Pos    : Uint             := No_Uint;
         --  Starting and ending (last plus one) positions of an LLVM
         --  field being used to contain multiple bitfields, if not
         --  No_Uint.

         Bitfield_Is_Array   : Boolean;
         --  Set if the bitfield field we make is an array

         function Field_Before (L, R : Int) return Boolean;
         --  Determine the sort order of two fields in Added_Field_Table

         procedure Swap_Fields (L, R : Int);
         --  Swap the fields in Added_Field_Table with the above indices

         procedure Sort is new Ada.Containers.Generic_Sort
           (Index_Type => Int, Before => Field_Before, Swap => Swap_Fields);

         function Align_Pos (Pos : ULL; Align : Nat) return ULL is
           (((Pos + ULL (Align) - 1) / ULL (Align))  * ULL (Align));
         --  Given a position and an alignment, align the position

         function Max_Record_Rep (E : Entity_Id) return Uint
           with Pre => Ekind_In (E, E_Component, E_Discriminant);
         --  Return the next byte after the highest repped position of
         --  the base type of E.

         procedure Force_To_Pos (Needed_Pos, Aligned_Pos : ULL);
         --  If the position we need to be at is beyond where we'd be given
         --  the native alignment of the type (if any, and taking into
         --  account that packed record fields aren't aligned), make an
         --  explicit padding type or, if this is the first field for an RI
         --  that isn't the first one, set the position of the RI we're
         --  going to make.

         procedure Create_Bitfield_Field (J : Int);
         --  We're processing the component at table index J, which is known
         --  to be a bitfield.  Create an LLVM field to hold contents of
         --  the J'th field to process, which is known to be a bitfield.

         ------------------
         -- Field_Before --
         ------------------

         function Field_Before (L, R : Int) return Boolean is

            --  When we look at properties of a component, we need to
            --  look at them on the field of the base type to be sure that
            --  we sort fields the same way for base types and its subtypes.

            AF_Left   : constant Added_Field := Added_Field_Table.Table (L);
            AF_Right  : constant Added_Field := Added_Field_Table.Table (R);
            Left_F    : constant Entity_Id   :=
              Original_Record_Component (AF_Left.F);
            Right_F   : constant Entity_Id   :=
              Original_Record_Component (AF_Right.F);
            Left_GT   : constant GL_Type     := Full_GL_Type (Left_F);
            Right_GT  : constant GL_Type     := Full_GL_Type (Right_F);
            Left_BO   : constant Uint        := Component_Bit_Offset (Left_F);
            Right_BO  : constant Uint        := Component_Bit_Offset (Right_F);
            Is_Pos_L  : constant Boolean     :=
              Present (Component_Clause (Left_F));
            Is_Pos_R  : constant Boolean     :=
              Present (Component_Clause (Right_F));
            Pack_L    : constant Boolean     := Is_Packable_Field (Left_F);
            Pack_R    : constant Boolean     := Is_Packable_Field (Right_F);
            Dynamic_L : constant Boolean     :=
              Is_Dynamic_Size (Left_GT,  Is_Unconstrained_Record (Left_GT));
            Dynamic_R : constant Boolean     :=
              Is_Dynamic_Size (Right_GT, Is_Unconstrained_Record (Right_GT));

         begin
            --  This function must satisfy the conditions of A.18(5/3),
            --  specifically that it must define a "strict weak ordering",
            --  meaning that it's irreflexive, asymetric, transitive and
            --  that if Field_Before (X, Y) is true for any X and Y, then
            --  for any Z, either Field_Before (X, Z) or Field_Before (Z,
            --  Y) must be true.

            --  The tag field is always the first field

            if Chars (Left_F) = Name_uTag then
               return True;
            elsif Chars (Right_F) = Name_uTag then
               return False;

            --  Otherwise, don't move outside of a parent part

            elsif AF_Left.Par_Depth /= AF_Right.Par_Depth then
               return AF_Left.Par_Depth > AF_Right.Par_Depth;

            --  Otherwise, the controller field is always before any
            --  non-parent field.

            elsif Chars (Left_F) = Name_uController
              and then AF_Right.Par_Depth = 0
            then
               return True;
            elsif Chars (Right_F) = Name_uController
              and then AF_Left.Par_Depth = 0
            then
               return False;

            --  If one field has a specified bit position and the other
            --  doesn't, the field with the position is first.

            elsif Is_Pos_L and then not Is_Pos_R then
               return True;
            elsif Is_Pos_R and then not Is_Pos_L then
               return False;

            --  If both have positions, the one with the lower position is
            --  first.

            elsif Is_Pos_L and then Is_Pos_R then
               return Left_BO < Right_BO;

            --  For all other cases, don't move outside of a variant part

            elsif AF_Left.Var_Depth /= AF_Right.Var_Depth then
               return AF_Left.Var_Depth < AF_Right.Var_Depth;

            --  Unless reordering is disabled, put packable fields after
            --  non-packable, fixed-length fields.  But don't do this if we
            --  aren't to reorder fields or for tagged records (since an
            --  extension could add packing but we must have the same
            --  ordering in extensions).

            elsif not No_Reordering (TE) and then not Is_Tagged_Type (TE)
              and then Pack_L and then not Pack_R and then not Dynamic_R
            then
               return False;
            elsif not No_Reordering (TE) and then not Is_Tagged_Type (TE)
              and then Pack_R and then not Pack_L and then not Dynamic_L
            then
               return True;

            --  A discriminant is in front of a non-discriminant

            elsif Ekind (Left_F) = E_Discriminant
              and then Ekind (Right_F) = E_Component
            then
               return True;
            elsif Ekind (Right_F) = E_Discriminant
              and then Ekind (Left_F) = E_Component
            then
               return False;

            --  Fixed-size fields come before variable-sized ones if this
            --  record is packed or has aliased components.  But don't do
            --  this if we aren't to reorder fields or for tagged records
            --  (since an extension could add an aliased field but we must
            --  have the same ordering in extensions).

            elsif not No_Reordering (TE) and then not Is_Tagged_Type (TE)
              and then (Is_Packed (TE) or else Aliased_Fields)
              and then not Dynamic_L and then Dynamic_R
            then
               return True;
            elsif not No_Reordering (TE) and then not Is_Tagged_Type (TE)
              and then (Is_Packed (TE) or else Aliased_Fields)
              and then not Dynamic_R and then Dynamic_L
            then
               return False;

            --  Otherwise, keep the original sequence intact

            else
               return AF_Left.Seq < AF_Right.Seq;
            end if;
         end Field_Before;

         -----------------
         -- Swap_Fields --
         -----------------

         procedure Swap_Fields (L, R : Int) is
            Temp : constant Added_Field := Added_Field_Table.Table (L);

         begin
            Added_Field_Table.Table (L) := Added_Field_Table.Table (R);
            Added_Field_Table.Table (R) := Temp;
         end Swap_Fields;

         --------------------
         -- Max_Record_Rep --
         --------------------

         function Max_Record_Rep (E : Entity_Id) return Uint is
            TE      : constant Entity_Id := Full_Base_Type (Full_Scope (E));
            F       : Entity_Id          :=
              First_Component_Or_Discriminant (TE);
            End_Pos : Uint               := Uint_0;

         begin
            while Present (F) loop
               if Present (Component_Clause (F))
                 and then Component_Bit_Offset (F) + Esize (F) > End_Pos
               then
                  End_Pos := Component_Bit_Offset (F) + Esize (F);
               end if;

               Next_Component_Or_Discriminant (F);
            end loop;

            return (End_Pos + (BPU - 1)) / BPU;

         end Max_Record_Rep;

         ------------------
         -- Force_To_Pos --
         ------------------
         procedure Force_To_Pos (Needed_Pos, Aligned_Pos : ULL) is
         begin
            if Needed_Pos /= 0 and then LLVM_Types.Last = -1
              and then Present (Prev_Idx)
            then
               RI_Position := Needed_Pos;
               RI_Align    := 1;
               Cur_RI_Pos  := Needed_Pos;
            elsif Needed_Pos > Aligned_Pos then
               LLVM_Types.Append
                 (Array_Type (Byte_T, unsigned (Needed_Pos - Cur_RI_Pos)));
               Cur_RI_Pos := Needed_Pos;
            else
               Cur_RI_Pos := Aligned_Pos;
            end if;

         end Force_To_Pos;

         ---------------------------
         -- Create_Bitfield_Field --
         ---------------------------

         procedure Create_Bitfield_Field (J : Int) is
            AF           : constant Added_Field := Added_Field_Table.Table (J);
            Bitfield_Len : ULL;

            function Start_Position (Bitpos : Uint) return Uint is
              (Bitpos / BPU);

            function End_Position (Bitpos, Size : Uint) return Uint is
              ((Bitpos + Size + BPU - 1) / BPU);

         begin
            --  We need to create an LLVM field to use to represent one or
            --  more bitfields starting at location J in the
            --  Added_Field_Table.  We need to continue widening the field
            --  until we run into a component that no longer overlaps any
            --  of the bits in the field or we've reached the end of the
            --  field list.
            --
            --  Start by making a field just wide enough for this component.

            Bitfield_Start_Pos := Start_Position (AF.Bitpos);
            Bitfield_End_Pos   := End_Position   (AF.Bitpos, AF.Size);

            --  Now go through all the remaining components that start within
            --  the field we made and widen the bitfield field to include it.

            for K in J + 1 .. Added_Field_Table.Last loop
               declare
                  AF_K : constant Added_Field := Added_Field_Table.Table (K);
                  F    : constant Entity_Id   := AF_K.F;

               begin
                  exit when AF_K.Bitpos = No_Uint or else AF_K.Size = No_Uint
                    or else not Is_Bitfield_By_Rep (F, AF_K.Bitpos, AF_K.Size)
                    or else Start_Position (AF_K.Bitpos) >= Bitfield_End_Pos;

                  Bitfield_End_Pos := End_Position (AF_K.Bitpos, AF_K.Size);
               end;
            end loop;

            --  We can be the most efficient if we can use a normal integral
            --  type (i8, i16, i32, or i64) for this field.  If it's not
            --  the size already, the only option we have now is to see if
            --  we have padding between our current position and the start of
            --  the bitfield that we can use to widen the bitfield.

            Bitfield_Len := UI_To_ULL (Bitfield_End_Pos - Bitfield_Start_Pos);
            case Bitfield_Len is
               when 1 | 2 | 4 | 8 =>
                  null;

               when 3 =>
                  if Cur_RI_Pos < UI_To_ULL (Bitfield_Start_Pos) then
                     Bitfield_Start_Pos := Bitfield_Start_Pos - 1;
                     Bitfield_Len       := 4;
                  end if;

               when 5 .. 7 =>
                  if UI_To_ULL (Bitfield_Start_Pos) - Cur_RI_Pos >
                    8 - Bitfield_Len
                  then
                     Bitfield_Start_Pos :=
                       Bitfield_Start_Pos - Int (8 - Bitfield_Len);
                     Bitfield_Len       := 8;
                  end if;

               when others =>
                  null;
            end case;

            --  Now we know what we have to do for the bitfield field

            Force_To_Pos (UI_To_ULL (Bitfield_Start_Pos), Cur_RI_Pos);
            if Bitfield_Len in 1 | 2 | 4 | 8 then
               Bitfield_Is_Array := False;
               LLVM_Types.Append (Int_Ty (Nat (Bitfield_Len) * BPU));
            else
               Bitfield_Is_Array := True;
               LLVM_Types.Append (Array_Type (Byte_T,
                                              unsigned (Bitfield_Len)));
            end if;

            Cur_RI_Pos := UI_To_ULL (Bitfield_End_Pos);
         end Create_Bitfield_Field;

      begin  -- Start of processing for Process_Fields_To_Add

         Sort (1, Added_Field_Table.Last);

         for J in 1 .. Added_Field_Table.Last loop
            declare
               AF        : Added_Field renames Added_Field_Table.Table (J);
               F         : constant Entity_Id   := AF.F;
               --  The field to add

               Def_GT    : constant GL_Type     :=
                 Default_GL_Type (Full_Etype (F));
               --  The default GL_Type for that field

               Size      : Uint                 := AF.Size;
               --  An optional size to force the field to

               Max_Sz    : constant Boolean     :=
                 Is_Unconstrained_Record (Def_GT);
               --  True if this is an object for which we have to use
               --  the maximum possible size.

               Biased    : constant Boolean     :=
                 Has_Biased_Representation (F);
               --  True if we need a biased representation for this field

               F_GT      : GL_Type              :=
                   Make_GT_Alternative (Def_GT, F,
                                        Size          => Size,
                                        Align         => No_Uint,
                                        For_Type      => False,
                                        For_Component => False,
                                        Max_Size      => Max_Sz,
                                        Is_Biased     => Biased);
               --  The GL_Type that we'll use for this field, taking
               --  into account any specified size and if we have to
               --  use the max size.

               Bit_Pos     : Uint               := AF.Bitpos;
               --  Specified bit position of field, if any.

               Pos         : Uint               :=
                 (if Bit_Pos = No_Uint then No_Uint else Bit_Pos / BPU);

               --  If a position is specified, this is that position

               Need_Align  :  Nat                :=
                 (if   Pos /= No_Uint
                       or else (Is_Packed (TE) and then not Is_Aliased (F))
                  then 1 else Get_Type_Alignment (F_GT));
               --  The alignment we need this field to have

            begin
               --  If we've pushed into a new static variant, see if
               --  we need to align it.  But update our level anyway and
               --  clear out any starting location for packed fields.
               --  Ignore if there's a position specified.

               if AF.Var_Depth /= Last_Var_Depth and then Pos = No_Uint then
                  if AF.Var_Depth > Last_Var_Depth and then AF.Var_Align /= 0
                  then
                     Need_Align  := AF.Var_Align;
                  end if;

                  Last_Var_Depth := AF.Var_Depth;
                  Packed_Field_Bitpos := No_Uint;
               end if;

               --  If this isn't a packable field, but we've previously set
               --  up a location for them, clear out the location.

               if Packed_Field_Bitpos /= No_Uint
                 and then not Is_Packable_Field (F)
               then
                  Packed_Field_Bitpos := No_Uint;

               --  If this is a packable field and we haven't set up a
               --  location for them, set one up and initialize the position
               --  and size of this field as well as following ones.

               elsif Packed_Field_Bitpos = No_Uint
                 and then Is_Packable_Field (F)
               then
                  Pos                 := UI_From_ULL (Cur_RI_Pos);
                  Bit_Pos             := Pos * BPU;
                  Size                := RM_Size (F_GT);
                  Packed_Field_Bitpos := Bit_Pos + Size;
                  AF.Bitpos           := Bit_Pos;
                  AF.Size             := Size;
                  F_GT                :=
                    Make_GT_Alternative (Def_GT, F,
                                         Size          => Size,
                                         Align         => No_Uint,
                                         For_Type      => False,
                                         For_Component => False,
                                         Max_Size      => Max_Sz,
                                         Is_Biased     => Biased);

                  Set_Esize (F, Size);

                  for K in J + 1 .. Added_Field_Table.Last loop
                     declare
                        AF_K : Added_Field renames Added_Field_Table.Table (K);

                     begin
                        exit when not Is_Packable_Field (AF_K.F)
                          or else AF_K.Var_Depth /= Last_Var_Depth;

                        AF_K.Bitpos         := Packed_Field_Bitpos;
                        AF_K.Size           := RM_Size (Full_Etype (AF_K.F));
                        Packed_Field_Bitpos := Packed_Field_Bitpos + AF_K.Size;
                        Set_Esize (AF_K.F, AF_K.Size);
                     end;
                  end loop;
               end if;

               --  If we're not in a variant, this field has no rep clause,
               --  is not the _Tag field, and we haven't seen a non-repped
               --  field before, force the position of this record to be
               --  after the end of all repped fields (including those in
               --  a variant).

               if Pos = No_Uint and then Chars (F) /= Name_uTag then
                  if not In_Variant and then not Had_Non_Repped then
                     Forced_Pos :=
                       Align_Pos (UI_To_ULL (Max_Record_Rep (F)), Need_Align);
                  end if;

                  Had_Non_Repped := True;
               end if;

               --  If this field is a non-native type, we have to close out
               --  the last record info entry we're making, if there's
               --  anything in it, and make a piece for this field.

               if Is_Nonnative_Type (F_GT) then
                  --  ??  This is the only case where we use an F_GT that
                  --  might have been modified by Add_FI.  We need to be
                  --  sure that's OK.

                  Flush_Current_Types;

                  --  If we're forcing the position of this field, set that
                  --  as the starting position of the RI we're about to make.
                  --  ??? For now, set the alignment to unaligned, but we
                  --  need to clean this up at some point as well as set
                  --  the proper GT for the fields.

                  if Forced_Pos /= 0 then
                     RI_Position := Forced_Pos;
                     RI_Align    := 1;
                     Forced_Pos  := 0;
                  end if;

                  Add_FI (F, Cur_Idx, F_GT);
                  Add_RI (F_GT => F_GT, Align => Need_Align);
                  Set_Is_Nonnative_Type (TE);
                  Split_Align := Need_Align;

               --  If it's a native type, add it to the current set of
               --  fields and make a field descriptor.

               else
                  --  We need to flush the previous types if required
                  --  by the alignment.  We assume here that if Add_FI
                  --  updates our type that it has the same alignment.

                  if Need_Align > Split_Align and then not Use_Packed then
                     Flush_Current_Types;
                     Set_Is_Nonnative_Type (TE);
                     RI_Align    := Need_Align;
                     Split_Align := Need_Align;

                  --  If we're in the overlap section of a variant and we've
                  --  run out of components that have a position, end the
                  --  overlap section.

                  elsif Pos = No_Uint and then RI_Is_Overlap then
                     Flush_Current_Types;
                     RI_Is_Overlap := False;

                  --  If we're in a dynamic variant and have a component
                  --  clause,  show that we're building a overlap RI.

                  elsif In_Dynamic_Variant
                    and then Present (Component_Clause (F))
                  then
                     RI_Is_Overlap := True;
                  end if;

                  declare
                     T           : constant Type_T := Type_Of (F_GT);
                     --  LLVM type to use

                     T_Align     : constant Nat    :=
                       (if Use_Packed then 1 else Get_Type_Alignment (T));
                     --  The native alignment of the LLVM type

                     Pos_Aligned : constant ULL    :=
                       Align_Pos (Cur_RI_Pos, T_Align);
                     --  The position we'll be at when applying the natural
                     --  alignment of the type.

                     Needed_Pos  : constant ULL    :=
                       (if    Pos /= No_Uint then UI_To_ULL (Pos)
                        elsif Forced_Pos /= 0 then Forced_Pos
                        else  Align_Pos (Cur_RI_Pos, Need_Align));
                     --  The position we need to be at, either by virtue of
                     --  a specified position alignment or because it's
                     --  forced there in a complex partially-repped variant
                     --  case.

                  begin
                     --  If this is a bitfield, we'll be using the special
                     --  "bitfield field".  If we don't fit in the current
                     --  one or there isn't one, make one.  Otherwise, just
                     --  record this field.

                     if Is_Bitfield_By_Rep (F, Bit_Pos, Size) then
                        if Bitfield_Start_Pos = No_Uint
                          or else AF.Bitpos / BPU >= Bitfield_End_Pos
                        then
                           Create_Bitfield_Field (J);
                        end if;

                        Add_FI (F, Cur_Idx, F_GT,
                                Ordinal        => LLVM_Types.Last,
                                First_Bit      => Bit_Pos -
                                  (Bitfield_Start_Pos * BPU),
                                Num_Bits       => Size,
                                Array_Bitfield => Bitfield_Is_Array);
                     else
                        Force_To_Pos (Needed_Pos, Pos_Aligned);
                        LLVM_Types.Append (T);
                        Cur_RI_Pos := Cur_RI_Pos + Get_Type_Size (T);
                        Add_FI (F, Cur_Idx, F_GT, Ordinal => LLVM_Types.Last);
                        Forced_Pos := 0;
                     end if;
                  end;
               end if;
            end;
         end loop;

         Added_Field_Table.Set_Last (0);
      end Process_Fields_To_Add;

   --  Start of processing for Create_Record_Type

   begin
      --  Because of the potential recursion between record and access types,
      --  make a dummy type for us and set it as our type right at the start.
      --  Then initialize our first record info table entry, which we know
      --  will be used.  But first see if the dummy type was already made.

      if No (GT) then
         GT := New_GT (TE);
      end if;

      LLVM_Type := Type_Of (GT);
      if No (LLVM_Type) then
         pragma Assert (Is_Empty_GL_Type (GT));
         LLVM_Type := Struct_Create_Named (Context, Get_Name (TE));
      end if;

      --  See if the record has any aliased components.  We'll use that
      --  flag to see if we should move variable-sized fields after fixed
      --  fields.  Ignore the tag.

      Field := First_Component_Or_Discriminant (TE);
      while Present (Field) loop
         if Is_Aliased (Field) and then Chars (Field) /= Name_uTag then
            Aliased_Fields := True;
         end if;

         Next_Component_Or_Discriminant (Field);
      end loop;

      Update_GL_Type (GT, LLVM_Type, True);
      Record_Info_Table.Increment_Last;
      Cur_Idx := Record_Info_Table.Last;
      Set_Record_Info (TE, Cur_Idx);
      Add_Fields (TE);
      Process_Fields_To_Add;

      --  If we haven't yet made any record info entries, it means that
      --  this is a fixed-size record that can be just an LLVM type,
      --  so use the one we made.

      if No (Prev_Idx) then
         Struct_Set_Body (LLVM_Type, LLVM_Types.Table (0)'Address,
                          unsigned (LLVM_Types.Last + 1), Use_Packed);
         Add_RI (T => LLVM_Type, Align => RI_Align);
      else
         --  Otherwise, close out the last record info if we have any
         --  fields.  Note that if we don't have any fields, the entry we
         --  allocated will remain unused, but trying to reclaim it is
         --  risky.

         Flush_Current_Types;
      end if;

      --  If we have a new discriminant that renames one from our parent,
      --  we need to mark which field the discriminant corresponds to.  So
      --  make a pass over the discriminants of this type seeing if any
      --  haven't had field information set.  If we find any, copy it from
      --  the original field.

      if Has_Discriminants (Full_Base_Type (TE))
        and then not Is_Unchecked_Union (Full_Base_Type (TE))
      then
         Field := First_Discriminant (Full_Base_Type (TE));
         while Present (Field) loop
            declare
               ORC         : constant Entity_Id :=
                 Original_Record_Component (Field);
               Discrim_Num : constant Nat       :=
                 UI_To_Int (Discriminant_Number (ORC));
               Outer_Orig  : constant Entity_Id :=
                 Find_Field_In_Entity_List (ORC, TE, Cur_Field);
               Outer_Field : Entity_Id;

            begin
               Outer_Field := Find_Field_In_Entity_List (Field, TE, Cur_Field);
               if Present (Outer_Field)
                 and then No (Get_Field_Info (Outer_Field))
                 and then Scope (ORC) = Full_Base_Type (TE)
               then
                  if Present (Outer_Field) and then Present (Outer_Orig) then
                     Set_Field_Info (Outer_Field, Get_Field_Info (Outer_Orig));
                  elsif Present (Discrim_FIs (Discrim_Num)) then
                     Set_Field_Info (Outer_Field, Discrim_FIs (Discrim_Num));
                  end if;
               end if;
            end;

            Next_Discriminant (Field);
         end loop;
      end if;

      --  Show that the type is no longer a dummy

      Update_GL_Type (GT, LLVM_Type, False);

      --  Back-annotate all fields that exist in this record type

      Cur_Field := First_Component_Or_Discriminant (TE);
      while Present (Cur_Field) loop
         if Full_Scope (Cur_Field) = TE
           and then Present (Get_Field_Info (Cur_Field))
         then
            declare
               Byte_Position : constant BA_Data         :=
                 Field_Position (Cur_Field, No_GL_Value);
               Bit_Position  : constant BA_Data         :=
                 Byte_Position * Const (ULL (BPU)) + Const (Field_Bit_Offset
                                                              (Cur_Field));
               Bit_Offset    : constant Node_Ref_Or_Val :=
                 Annotated_Value (Bit_Position);

            begin
               if Unknown_Esize (Cur_Field) then
                  Set_Esize (Cur_Field,
                             Annotated_Object_Size (Get_Field_Type
                                                      (Cur_Field)));
               end if;

               Set_Component_Bit_Offset (Cur_Field, Bit_Offset);
               if Is_Static_SO_Ref (Bit_Offset) then
                  Set_Normalized_Position
                    (Cur_Field, Bit_Offset / BPU);

                  Set_Normalized_First_Bit
                    (Cur_Field, Bit_Offset mod BPU);
               else
                  Set_Normalized_Position (Cur_Field,
                                           Annotated_Value (Byte_Position));
                  Set_Normalized_First_Bit (Cur_Field, Uint_0);
               end if;
            end;
         end if;

         Next_Component_Or_Discriminant (Cur_Field);
      end loop;

      --  If requested, dump the structure of this record

      if Debug_Flag_Underscore_RR then
         pg (Union_Id (TE));
         Print_Record_Info (TE);
         Write_Eol;
      end if;

      return LLVM_Type;
   end Create_Record_Type;

begin
   --  Make a dummy entry in the record and field tables, so the
   --  "Empty" entry is never used.

   Record_Info_Table.Increment_Last;
   Field_Info_Table.Increment_Last;
end GNATLLVM.Records.Create;
