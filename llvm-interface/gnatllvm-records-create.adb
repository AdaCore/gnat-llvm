------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2023, AdaCore                     --
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
with Repinfo;    use Repinfo;
with Sem_Aux;    use Sem_Aux;
with Sem_Eval;   use Sem_Eval;
with Snames;     use Snames;
with Sprint;     use Sprint;
with Table;      use Table;
with Uintp.LLVM; use Uintp.LLVM;

with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Codegen;      use GNATLLVM.Codegen;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;
with GNATLLVM.Types.Create; use GNATLLVM.Types.Create;
with GNATLLVM.Utils;        use GNATLLVM.Utils;

with CCG; use CCG;

package body GNATLLVM.Records.Create is

   function Max_Discriminant (TE : Record_Kind_Id) return Int;
   --  Return the highest value of Discriminant_Number

   function Find_Field_In_Entity_List
     (F         : Record_Field_Kind_Id;
      TE        : Record_Kind_Id;
      Cur_Field : in out Opt_Record_Field_Kind_Id)
     return Opt_Record_Field_Kind_Id;
   --  Find a field in the entity list of TE that has the same
   --  Original_Record_Component as F and return it if so. Cur_Field
   --  is used to cache the last field tested to avoid quadratic behavior
   --  since we'll be requesting fields in roughly (but not exactly!)
   --  the same order as they are in the list.

   function Variant_Alignment (Var_Part : N_Variant_Part_Id) return Nat;
   --  Compute the alignment of the variant at Var_Part, which is the
   --  maximum size of any field in the variant. We recurse through
   --  any nested variants.

   function Align_Pos (Pos : ULL; Align : Nat) return ULL is
     (((Pos + ULL (Align - 1)) / ULL (Align)) * ULL (Align));
      --  Given a position and an alignment, align the position

   function Align_Pos (Pos : Uint; Align : Nat) return Uint is
     (((Pos + (Align - 1)) / Align) * Align);
   --  Given a position and an alignment, align the position

   function Truncate_Pos (Pos : Uint; Align : Nat) return Uint is
     ((Pos / Align) * Align);
   --  Given a position and an alignment (usually BPU), truncate that
   --  position to a multiple of the alignment.

   function Find_Choice
     (N : N_Subexpr_Id; Alts : List_Id) return Opt_N_Variant_Id
     with Pre => Is_Static_Expression (N) and then Present (Alts);
   --  N is a static expression and Alts is a list of alternatives. Return
   --  which alternate has a Choice that covers N.

   function Choices_To_SO_Ref
     (Variant : N_Variant_Id; Discrim : N_Subexpr_Id) return SO_Ref;
   --  Given an alternative for a variant record, return an SO_Ref
   --  corresponding to an expression that's True when that variant is
   --  present. This is a function of the discriminant (Discrim) and
   --  constants.

   function Uses_Discriminant (GT : GL_Type) return Boolean
     with Pre => Present (GT);
   --  Returns True if one of GT's bounds references a discriminant

   function Max_Record_Rep (E : Record_Field_Kind_Id) return Uint;
   --  Return the next byte after the highest repped position of the base
   --  type of E.

   -------------------------------
   -- Find_Field_In_Entity_List --
   -------------------------------

   function Find_Field_In_Entity_List
     (F         : Record_Field_Kind_Id;
      TE        : Record_Kind_Id;
      Cur_Field : in out Opt_Record_Field_Kind_Id)
     return Opt_Record_Field_Kind_Id
   is

      function ORC (F : Record_Field_Kind_Id) return Record_Field_Kind_Id;
      --  Get the Original_Record_Component, but also check
      --  Corresponding_Discriminant first;

      ---------
      -- ORC --
      ---------

      function ORC (F : Record_Field_Kind_Id) return Record_Field_Kind_Id is
         Field : Record_Field_Kind_Id := F;

      begin
         while Ekind (Field) = E_Discriminant loop
            exit when No (Corresponding_Discriminant (Field));
            Field := Corresponding_Discriminant (Field);
         end loop;

         return Original_Record_Component (Field);
      end ORC;

      Initial_Cur_Field : constant Opt_Record_Field_Kind_Id := Cur_Field;

   begin
      --  Look from Cur_Field until the end of the list. Then look from
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

   function Max_Discriminant (TE : Record_Kind_Id) return Int is
      F : Opt_Record_Field_Kind_Id := First_Component_Or_Discriminant (TE);

   begin
      return Max : Int := 0 do
         while Present (F) loop
            if Ekind (F) = E_Discriminant
              and then Discriminant_Number (F) > Max
            then
               Max := +Discriminant_Number (F);
            end if;

            Next_Component_Or_Discriminant (F);
         end loop;
      end return;
   end Max_Discriminant;

   -----------------------
   -- Variant_Alignment --
   -----------------------

   function Variant_Alignment (Var_Part : N_Variant_Part_Id) return Nat is
      Variant : Opt_N_Variant_Id := First_Non_Pragma (Variants (Var_Part));

   begin
      return Align : Nat := BPU do
         while Present (Variant) loop
            declare
               Comp_List      : constant N_Component_List_Id   :=
                 Component_List (Variant);
               Nested_Variant : constant Opt_N_Variant_Part_Id :=
                 Variant_Part (Comp_List);
               Comp_Decl      : Opt_N_Component_Declaration_Id :=
                 First_Non_Pragma (Component_Items (Comp_List));

            begin
               while Present (Comp_Decl) loop
                  Align := Nat'Max (Align,
                                    Effective_Field_Alignment
                                      (Defining_Identifier (Comp_Decl)));

                  Next_Non_Pragma (Comp_Decl);
               end loop;

               if Present (Nested_Variant) then
                  Align := Nat'Max (Align, Variant_Alignment (Nested_Variant));
               end if;
            end;

            Next_Non_Pragma (Variant);
         end loop;
      end return;
   end Variant_Alignment;

   -----------------
   -- Find_Choice --
   -----------------

   function Find_Choice
     (N : N_Subexpr_Id; Alts : List_Id) return Opt_N_Variant_Id
   is
      Value     : constant Uint := Expr_Rep_Value (N);
      Alt       : Opt_N_Variant_Id;
      Choice    : Node_Id;
      Low, High : Uint;

   begin
      Alt := First_Non_Pragma (Alts);
      while Present (Alt) loop
         Choice := First (Discrete_Choices (Alt));

         if Nkind (Choice) = N_Others_Choice then
            Choice := First (Others_Discrete_Choices (Choice));
         end if;

         while Present (Choice) loop
            Decode_Range (Choice, Low, High);

            if Present (Low) and then Present (High)
              and then Value >= Low and then Value <= High
            then
               return Alt;
            end if;

            Next (Choice);
         end loop;

         Next_Non_Pragma (Alt);
      end loop;

      return Empty;
   end Find_Choice;

   -----------------------
   -- Choices_To_SO_Ref --
   -----------------------

   function Choices_To_SO_Ref
     (Variant : N_Variant_Id; Discrim : N_Subexpr_Id) return SO_Ref
   is
      Discrim_SO : constant SO_Ref := Annotated_Value (Emit_Expr (Discrim));
      Choice     : Node_Id;
      Expr       : SO_Ref;
      This_Expr  : SO_Ref;
      Low, High  : Uint;

      function Protect_Neg (U : Uint) return SO_Ref is
         (if    Present (U) and then U < 0 then Create_Node (Negate_Expr, -U)
          else U);
         --  A negative value means an expression, so we have to make a
         --  Negate_Expr of the positive value.

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
         Low  := Protect_Neg (Low);
         High := Protect_Neg (High);

         if Present (Low) and then Present (High) and then Low = High then
            This_Expr := Create_Node (Eq_Expr, Discrim_SO, Low);
         elsif Present (Low) and then Present (High) and then High > Low then
            This_Expr := Create_Node (Truth_And_Expr,
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

   -----------------------
   -- Uses_Discriminant --
   -----------------------

   function Uses_Discriminant (GT : GL_Type) return Boolean is
      Index : Opt_N_Is_Index_Id;

   begin
      --  Constrained array types are all we're concerned with here

      if not Is_Array_Type (GT) or else Is_Unconstrained_Array (GT) then
         return False;
      end if;

      Index := First_Index (GT);
      while Present (Index) loop
         declare
            Idx_Range : constant Node_Id      := Simplify_Range (Index);
            LB        : constant N_Subexpr_Id := Low_Bound (Idx_Range);
            HB        : constant N_Subexpr_Id := High_Bound (Idx_Range);

         begin
            exit when Contains_Discriminant (LB);
            exit when Contains_Discriminant (HB);
            Next_Index (Index);
         end;
      end loop;

      return Present (Index);
   end Uses_Discriminant;

   --------------------
   -- Max_Record_Rep --
   --------------------

   function Max_Record_Rep (E : Record_Field_Kind_Id) return Uint is
      TE      : constant Record_Kind_Id  := Full_Base_Type (Full_Scope (E));
      F       : Opt_Record_Field_Kind_Id :=
        First_Component_Or_Discriminant (TE);

   begin
      return End_Pos : Uint := Uint_0 do
         while Present (F) loop
            if Present (Component_Clause (F))
              and then Known_Component_Bit_Offset (F)
              and then Known_Esize (F)
              and then Component_Bit_Offset (F) + Esize (F) > End_Pos
            then
               End_Pos := Component_Bit_Offset (F) + Esize (F);
            end if;

            Next_Component_Or_Discriminant (F);
         end loop;
      end return;
   end Max_Record_Rep;

   ------------------------
   -- Create_Record_Type --
   ------------------------

   function Create_Record_Type (TE : Record_Kind_Id) return Type_T is

      type Field_Info_Id_Array is array (Nat range <>) of Field_Info_Id;

      --  This function creates a record type and the description of that
      --  record. Note that this function does not itself lay out the record.
      --  We don't actually lay out record in that sense. Instead, we create
      --  Record_Info structures whose chaining describe the record structure
      --  and Field_Info structures, one for each field, showing where each
      --  field is located in the record. We then compute any information
      --  we need on the fly, mostly in Get_Record_Size_So_Far.

      --  For each "piece" of the record (for variant records, the common
      --  portion and each variant), we first list all the fields in that
      --  part, then sort the list to deal with record rep clauses and the
      --  few cases when we reorder records, then lay out the fields into
      --  Record_Info pieces. We start with a simple data structure that
      --  records information about the field to add and its location
      --  within the record.

      type Added_Field is record
         F            : Record_Field_Kind_Id;
         --  Entity of the field that we're adding

         AF           : Record_Field_Kind_Id;
         --  Ancestor of the field that we're adding

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

         Pos          : Uint;
         --  The bit position specified (or to be used, in the case of
         --  packed records), or No_Uint if none.

         Size         : Uint;
         --  Size in bits specified (or to be used, in the case of packed
         --  records), or No_Uint if none.
      end record;

      package Added_Fields is new Table.Table
        (Table_Component_Type => Added_Field,
         Table_Index_Type     => Int,
         Table_Low_Bound      => 1,
         Table_Initial        => 20,
         Table_Increment      => 5,
         Table_Name           => "Added_Fields");

      --  We maintain a table of all the LLVM types that will be put in a
      --  LLVM struct type in an RI. These are both for actual and padding
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

      BT             : constant Record_Kind_Id  := Full_Base_Type (TE);
      --  Base type of our record type

      Aliased_Fields : constant Boolean         :=
        Record_Has_Aliased_Components (BT);
      --  Indicates that at least one field is aliased

      Full_Access    : constant Boolean         := Is_Full_Access (BT);
      --  Indicates that the record requires full access.

      Has_NP_Fixed   : Boolean                  := False;
      --  Indicates that at least one field of fixed size isn't packable

      Prev_Idx       : Record_Info_Id           := Empty_Record_Info_Id;
      --  The previous index of the record table entry, if any

      First_Idx      : Record_Info_Id           := Empty_Record_Info_Id;
      --  The first index used by the current record fragment construction

      Overlap_Idx    : Record_Info_Id           := Empty_Record_Info_Id;
      --  The index of the overlap component of this variant part, if any

      Cur_RI_Pos     : ULL                      := 0;
      --  Current position into this RI

      Par_Depth      : Int                      := 0;
      --  Nesting depth into parent records

      RI_Align       : Nat                      := 0;
      --  If nonzero, an alignment to assign to the next RI built for an
      --  LLVM type.

      RI_Position    : ULL                      := 0;
      --  If nonzero, a position to assign to the next RI built for an
      --  LLVM type.

      RI_Is_Overlap  : Boolean                  := False;
      --  If True, the next RI built is an overlap RI for a variant

      RI_Unused_Bits : Uint                     := Uint_0;
      --  Number of unused bits at the end of this RI

      Cur_Field      : Opt_Record_Field_Kind_Id := Empty;
      --  Used for a cache in Find_Field_In_Entity_List to avoid quadratic
      --  behavior.

      Split_Align    : Nat                      := Max_Valid_Align;
      --  We need to split an LLVM fragment type if the alignment of the
      --  next field is greater than both this and Last_Align. This occurs
      --  for variant records; see details there. It also occurs for the
      --  same reason after a variable-size field.

      First_Field_Id : Field_Info_Id            := Empty_Field_Info_Id;
      --  First Field_Info_Id in this RI

      Last_Field_Id  : Field_Info_Id            := Empty_Field_Info_Id;
      --  Last Field_Info_Id created, if any

      UID            : Unique_Id                := New_Unique_Id;
      --  The Unique_Id for the struct that we're building

      GT             :  GL_Type                 :=
        Default_GL_Type (TE, Create => False);
      --  The GL_Type for this record type

      Discrim_FIs    : Field_Info_Id_Array      :=
        (1 .. Max_Discriminant (Full_Base_Type (TE)) => Empty_Field_Info_Id);
      --  In entry J, we record the Field_Info corresponding to the
      --  discriminant number J. We use this for record subtypes of
      --  derived types.

      Cur_Idx        : Record_Info_Id;
      --  The index of the record table entry we're building

      LLVM_Type      : Type_T;
      --  The LLVM type for this record type

      Field          : Opt_Record_Field_Kind_Id;
      --  Temporary for loop over components and discriminants

      procedure Add_RI
        (T                : Type_T                      := No_Type_T;
         F_GT             : GL_Type                     := No_GL_Type;
         Align            : Nat                         := 0;
         Position         : ULL                         := 0;
         Variant_List     : List_Id                     := No_List;
         Variant_Expr     : Opt_N_Subexpr_Id            := Empty;
         Variants         : Record_Info_Id_Array_Access := null;
         Overlap_Variants : Record_Info_Id_Array_Access := null;
         Unused_Bits      : Uint                        := Uint_0);
      --  Add a Record_Info into the table, chaining it as appropriate

      procedure Add_FI
        (E                    : Record_Field_Kind_Id;
         RI_Idx               : Record_Info_Id;
         F_GT                 : in out GL_Type;
         Ordinal              : Nat     := 0;
         First_Bit            : Uint    := No_Uint;
         Num_Bits             : Uint    := No_Uint;
         Array_Bitfield       : Boolean := False;
         Large_Array_Bitfield : Boolean := False);
      --  Add a Field_Info info the table, if appropriate, and set
      --  the field to point to it. Update F_GT if we used a matching field.

      procedure Push_Parent_Depth;
      procedure Pop_Parent_Depth (Par_TE : Record_Kind_Id);
      --  Indicate that we're starting/stopping (respectively) to process
      --  a parent record.

      procedure Add_Field (E : Record_Field_Kind_Id);
      --  Add one field to the above data

      procedure Process_Fields_To_Add;
      --  Create RI entries for the fields we've added above

      procedure Add_Fields (E : Record_Kind_Id);
      --  Add all fields of E to the above data, either the component or
      --  the extension components, but recursively add parent components.

      procedure Flush_Current_Types;
      --  If there are any types in the Types array, create a record
      --  description for them.

      procedure Force_To_Pos (Needed_Pos : ULL);
      --  If the position we need to be at is beyond where we are, make an
      --  explicit padding type or, if this is the first field for an RI
      --  that isn't the first one, set the position of the RI we're going
      --  to make.

      ------------
      -- Add_RI --
      ------------

      procedure Add_RI
        (T                : Type_T                      := No_Type_T;
         F_GT             : GL_Type                     := No_GL_Type;
         Align            : Nat                         := 0;
         Position         : ULL                         := 0;
         Variant_List     : List_Id                     := No_List;
         Variant_Expr     : Opt_N_Subexpr_Id            := Empty;
         Variants         : Record_Info_Id_Array_Access := null;
         Overlap_Variants : Record_Info_Id_Array_Access := null;
         Unused_Bits      : Uint                        := Uint_0) is

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
            Overlap_Variants => Overlap_Variants,
            First_Field      => First_Field_Id,
            Unused_Bits      => Unused_Bits);

         --  If we've had a previous RI for this part, link us to it.
         --  Otherwise, if this is an overlap RI, indicate it as so and
         --  start the chain over. If not, indicate the first index in our
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

         Record_Info_Table.Increment_Last;
         Prev_Idx := Cur_Idx;
         Cur_Idx  := Record_Info_Table.Last;

         --  Reset the chain info for FIs and get a new UID if we need a
         --  new struct.

         First_Field_Id := Empty_Field_Info_Id;
         Last_Field_Id  := Empty_Field_Info_Id;
         UID            := New_Unique_Id;
      end Add_RI;

      ------------
      -- Add_FI --
      ------------

      procedure Add_FI
        (E                    : Record_Field_Kind_Id;
         RI_Idx               : Record_Info_Id;
         F_GT                 : in out GL_Type;
         Ordinal              : Nat     := 0;
         First_Bit            : Uint    := No_Uint;
         Num_Bits             : Uint    := No_Uint;
         Array_Bitfield       : Boolean := False;
         Large_Array_Bitfield : Boolean := False)
      is
         Matching_Field : Opt_Record_Field_Kind_Id;

      begin
         --  Create an entry for this field and properly chain it

         Field_Info_Table.Append
           ((Field                => E,
             Next                 => Empty_Field_Info_Id,
             Rec_Info_Idx         => RI_Idx,
             GT                   => F_GT,
             Field_Ordinal        => Ordinal,
             First_Bit            => First_Bit,
             Num_Bits             => Num_Bits,
             TBAA_Type            => No_Metadata_T,
             Array_Bitfield       => Array_Bitfield,
             Large_Array_Bitfield => Large_Array_Bitfield));

         if No (Last_Field_Id) then
            First_Field_Id := Field_Info_Table.Last;
         else
            Field_Info_Table.Table (Last_Field_Id).Next :=
              Field_Info_Table.Last;
         end if;

         Last_Field_Id := Field_Info_Table.Last;

         --  If this field really isn't in the record we're working on, it
         --  must be in a parent. So it was correct to allocate space for
         --  it, but we let the record description be from the type that
         --  it's actually in.
         --
         --  If we're using a matching field, update F_GT to its type.
         --
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
               Discrim_FIs (+Discriminant_Number (E)) := Field_Info_Table.Last;
            end if;
         end if;
      end Add_FI;

      -----------------------
      -- Push_Parent_Depth --
      -----------------------

      procedure Push_Parent_Depth is
      begin
         Par_Depth := Par_Depth + 1;
      end Push_Parent_Depth;

      ----------------------
      -- Pop_Parent_Depth --
      ----------------------

      procedure Pop_Parent_Depth (Par_TE : Record_Kind_Id) is
      begin
         Par_Depth := Par_Depth - 1;

         --  If we're starting a new RI, we need to force alignment to
         --  that of the parent record.

         if LLVM_Types.Last < 0 then
            RI_Align := Get_Record_Type_Alignment (Par_TE);
         end if;
      end Pop_Parent_Depth;

      ----------------
      -- Add_Fields --
      ----------------

      procedure Add_Fields (E : Record_Kind_Id) is
         Rec_Type     : constant Record_Kind_Id     := Full_Base_Type (E);
         --  The base type, which we use to get the record order from

         Sub_Rec_Type : constant Opt_Record_Kind_Id :=
           (if Rec_Type = E then Empty else E);
         --  Present if we have to search the field list of a record subtype

         Rec_Field    : Opt_Record_Field_Kind_Id    := Empty;
         --  Cache used to limit quadratic behavior

         procedure Add_Component_List
           (List     : Opt_N_Component_List_Id;
            From_Rec : Opt_Record_Kind_Id;
            Parent   : Boolean := False);
         --  Add fields in List. If From_Rec is Present, instead
         --  of adding the actual field, add the field of the same
         --  name from From_Rec. If Parent is true, only add the
         --  parent record, otherwise, add all records except the parent.

         ------------------------
         -- Add_Component_List --
         ------------------------

         procedure Add_Component_List
           (List     : Opt_N_Component_List_Id;
            From_Rec : Opt_Record_Kind_Id;
            Parent   : Boolean := False)
         is
            Var_Part           : constant Opt_N_Variant_Part_Id :=
              (if Present (List) then Variant_Part (List) else Empty);
            Discrim            : constant Opt_N_Subexpr_Id      :=
              (if Present (Var_Part) then Name (Var_Part) else Empty);
            Constraining_Expr  : constant Opt_N_Subexpr_Id      :=
              (if   Present (From_Rec) and then Present (Var_Part)
               then Get_Discriminant_Constraint (From_Rec, Entity (Discrim))
               else Empty);
            Variant_Expr       : constant Opt_N_Subexpr_Id      :=
              (if    Present (Constraining_Expr) then Constraining_Expr
               elsif Present (Discrim) then Discrim else Empty);
            Static_Constraint  : constant Boolean               :=
              Present (Constraining_Expr)
                and then Is_Static_Expression (Constraining_Expr);
            Variant_Align      : constant Nat                   :=
              (if Present (Var_Part) then Variant_Alignment (Var_Part) else 0);
            Var_Array          : Record_Info_Id_Array_Access;
            Overlap_Var_Array  : Record_Info_Id_Array_Access;
            Saved_Cur_Idx      : Record_Info_Id;
            Saved_Prev_Idx     : Record_Info_Id;
            Saved_First_Idx    : Record_Info_Id;
            Saved_Overlap_Idx  : Record_Info_Id;
            Component_Decl     : Opt_N_Component_Declaration_Id;
            Field              : Record_Field_Kind_Id;
            Field_To_Add       : Opt_Record_Field_Kind_Id;
            Variant            : Opt_N_Variant_Id;
            J                  : Nat;

         begin
            --  Return quickly if nothing to do. Otherwise, walk the
            --  component list.

            if No (List) then
               return;
            end if;

            Component_Decl := First_Non_Pragma (Component_Items (List));
            while Present (Component_Decl) loop
               Field := Defining_Identifier (Component_Decl);

               if Parent = (Chars (Field) = Name_uParent) then
                  Field_To_Add := Field;

                  if Present (From_Rec) then
                     Field_To_Add :=
                       Find_Field_In_Entity_List (Field, From_Rec, Rec_Field);
                  end if;

                  if Present (Field_To_Add) then
                     if Chars (Field_To_Add) = Name_uParent then
                        Push_Parent_Depth;
                        Add_Fields (Full_Etype (Field_To_Add));
                        Pop_Parent_Depth (Full_Etype (Field_To_Add));
                     end if;

                     Add_Field (Field_To_Add);
                  end if;
               end if;

               Next_Non_Pragma (Component_Decl);
            end loop;

            --  Done if we're just asking for the parent field or if
            --  there are no variants in this record.

            if Parent or else No (Var_Part) then
               return;
            end if;

            --  Otherwise process variants. If we statically constrain the
            --  variant, see which variant is being referenced and output
            --  that one. Walk the proper variant here to verify that
            --  ever. Otherwise, set up for the variant, make the entres
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

               --  When we're processing subtypes, we're still looking at
               --  the variant entries for the type. So ensure we only set
               --  Present_Expr for it once.

               if No (Present_Expr (Variant)) then
                  Set_Present_Expr (Variant,
                                    Choices_To_SO_Ref (Variant, Variant_Expr));
               end if;

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

         Is_Derived        : Boolean   := False;
         Field             : Opt_Record_Field_Kind_Id;
         Field_To_Add      : Record_Field_Kind_Id;
         Outer_Field       : Opt_Record_Field_Kind_Id;
         Record_Definition : Node_Id;
         Components        : Opt_N_Component_List_Id;

      --  Start of processing for Add_Fields

      begin
         --  Get the record definition and component list

         Record_Definition :=
           Type_Definition (Declaration_Node (Rec_Type));

         if Nkind (Record_Definition) = N_Derived_Type_Definition then
            Record_Definition := Record_Extension_Part (Record_Definition);
            Is_Derived        := True;
         end if;

         Components := Component_List (Record_Definition);

         --  Add the parent field, which means adding all subfields of the
         --  parent. We can't just rely on field sorting to do this because
         --  the parent record might have variants. The way we do this
         --  depends on whether we're in normal mode or just elaborating
         --  types.

         if Decls_Only and then Is_Derived then
            Push_Parent_Depth;
            Add_Fields (Full_Etype (Base_Type (Rec_Type)));
            Pop_Parent_Depth (Full_Etype (Base_Type (Rec_Type)));
         else
            Add_Component_List (Components, Sub_Rec_Type, True);
         end if;

         --  If there are discriminants, process them. But ignore
         --  discriminants that are already in a parent type.

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

               --  If this field is a hidden discriminant, we need to allow
               --  space for it in the record even though we won't have a
               --  field for it. Handle that case here. The test for scope
               --  is testing whether we'll be setting the field info for
               --  the field.

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
            declare
               T : constant Type_T :=
                 Build_Struct_Type
                 (Type_Array (LLVM_Types.Table (0 .. Last_Type)),
                  Packed => True,
                  Name   => Get_Ext_Name (TE, "_I"));

            begin
               C_Set_Struct (UID, T);
               Add_RI (T           => T,
                       Align       => RI_Align,
                       Position    => RI_Position,
                       Unused_Bits => RI_Unused_Bits);
               RI_Align       := 0;
               RI_Position    := 0;
               RI_Unused_Bits := Uint_0;
               LLVM_Types.Set_Last (-1);
            end;
         end if;

         Cur_RI_Pos := 0;
      end Flush_Current_Types;

      ---------------
      -- Add_Field --
      ---------------

      procedure Add_Field (E : Record_Field_Kind_Id) is
         Clause      : constant Opt_N_Component_Clause_Id :=
           Component_Clause (E);
         R_TE        : constant Record_Kind_Id            := Full_Scope (E);
         Def_GT      : constant GL_Type                   :=
           Default_GL_Type (Full_Etype (E));
         F_GT        : GL_Type                            := Full_GL_Type (E);
         Align       : constant Nat                       :=
           Get_Type_Alignment (F_GT);
         Parent_TE   : constant Opt_Record_Kind_Id        :=
           (if   Present (Parent_Subtype (R_TE))
            then Full_Parent_Subtype (R_TE) else Empty);
         Atomic      : constant Boolean                   :=
           Is_Full_Access (E) or else Is_Full_Access (F_GT);
         Error_Str   : constant String                    :=
           Field_Error_Msg (E, F_GT, True);
         Pos         : Uint                               :=
           (if Present (Clause) then Component_Bit_Offset (E) else No_Uint);
         Size        : Uint                               :=
           (if not Known_Esize (E) then No_Uint
            else   Validate_Size (E, Def_GT, Esize (E),
                                  Zero_Allowed => Present (Clause)));
         Var_Depth   : Int                                := 0;
         Var_Align   : Nat                                := 0;

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
         --  if there is one. At one point, we omitted this test if the
         --  parent of the tagged type has a full rep clause since, in this
         --  case, component clauses are allowed to overlay the space
         --  allocated for the parent type and the front-end has checked
         --  that there are no overlapping components. But there are cases
         --  where this won't work. We do allow it with -gnatd.K for
         --  compatibility purposes.

         elsif Present (Clause) and then Present (Parent_TE)
           and then not (Is_Fully_Repped_Tagged_Type (Parent_TE)
                           and then Debug_Flag_Dot_KK)
           and then not Is_Dynamic_Size (Default_GL_Type (Parent_TE))
           and then Pos < Esize (Parent_TE)
         then
            Error_Msg_NE_Num
              ("position for & must be beyond parent, minimum allowed is ^",
               Position (Clause), E, To_Bytes (Esize (Parent_TE)));

          --  If the position is not a multiple of the storage unit, then
          --  give error.

         elsif Present (Clause) and then Pos mod BPU /= 0
           and then Error_Str'Length > 0
         then
            Error_Msg_NE ("position for" & Error_Str &
                            " must be multiple of Storage_Unit",
                          First_Bit (Clause), E);
            Pos := No_Uint;

         --  Likewise, if a position is specified and it's not a multiple
         --  of the alignment of the type.

         elsif Present (Clause) and then Pos mod Align /= 0
           and then Error_Str'Length > 0
         then
            Error_Msg_NE_Num
              ("position for" & Error_Str & " must be multiple of ^",
                  First_Bit (Clause), E, To_Bytes (Align));
            Error_Msg_NE_Num
              ("\\because alignment of its type& is ^",
                  First_Bit (Clause), Full_Etype (E), To_Bytes (Align));
            Pos := No_Uint;
         end if;

         --  If this is to be an atomic field, verify that we can make it one

         if Is_Full_Access (E) then
            Check_OK_For_Atomic_Type (F_GT, E);
         end if;

         --  If the size is not a multiple of the storage unit, issue error

         if Present (Clause) and then Present (Size)
           and then Size mod BPU /= 0 and then Error_Str'Length > 0
         then
            Error_Msg_NE ("size for" & Error_Str &
                            " must be multiple of Storage_Unit",
                          Last_Bit (Clause), E);
            Size := No_Uint;

         --  If a size is specified and constant and is lower than the size
         --  of the type or larger than the type and the field is atomic or
         --  aliased, we may have to give an error. (The variable case is
         --  handled above.)

         elsif Present (Clause) and then Present (Size)
           and then not Is_Dynamic_Size (Def_GT)
           and then (Size_Const_Int (Size) < Get_Type_Size (Def_GT)
                       or else ((Is_Aliased (E) or else Atomic)
                                and then (Get_Type_Size (Def_GT)) <
                                  Size_Const_Int (Size)))
           and then Error_Str'Length > 0
         then
            if Atomic or else Is_Aliased (E) then
               Error_Msg_NE_Num
                 ("size for" & Error_Str & " must be ^",
                  Last_Bit (Clause), E, Esize (Full_Etype (E)));
            else
               Error_Msg_NE_Num
                 ("size for" & Error_Str & " must be at least ^",
                  Last_Bit (Clause), E, Esize (Full_Etype (E)));
            end if;

            Size := No_Uint;
         end if;

         --  Now add field to table

         Added_Fields.Append ((E, Ancestor_Field (E), Added_Fields.Last + 1,
                               Par_Depth, Var_Depth, Var_Align, Pos, Size));
      end Add_Field;

      ------------------
      -- Force_To_Pos --
      ------------------

      procedure Force_To_Pos (Needed_Pos : ULL) is
         procedure Append_Padding (Size : Nat; Count : ULL)
           with Pre => Size in 8 | 16 | 32 | 64;
         --  Append padding to the record using Count objects of Size bits
         --  each.

         Left_To_Pad : ULL := Needed_Pos - Cur_RI_Pos;

         --------------------
         -- Append_Padding --
         --------------------

         procedure Append_Padding (Size : Nat; Count : ULL) is
            Base_T : constant Type_T := Int_Ty (Size);
            Use_T  : constant Type_T :=
              (if   Count > 1 then Array_Type (Base_T, unsigned (Count))
               else Base_T);

         begin
            if Count /= 0 then
               LLVM_Types.Append (Use_T);
               C_Set_Field_Info (UID, LLVM_Types.Last, Is_Padding => True);
               Cur_RI_Pos  := Cur_RI_Pos + Count * ULL (Size);
               Left_To_Pad := Left_To_Pad - Count * ULL (Size);
            end if;
         end Append_Padding;

      begin
         --  If we're going backwards, we need to flush the current RI
         --  and then start a new one.

         if Needed_Pos < Cur_RI_Pos then
            Flush_Current_Types;
            Set_Is_Nonnative_Type (TE);
         end if;

         --  If this is the start of a new RI, set its position

         if Needed_Pos /= 0 and then LLVM_Types.Last = -1
           and then Present (Prev_Idx)
         then
            RI_Position := Needed_Pos;
            RI_Align    := BPU;
            Cur_RI_Pos  := Needed_Pos;
         elsif Needed_Pos > Cur_RI_Pos then

            --  ??? LLVM's scalar optimization will try to break the record
            --  up into pieces. It's unfortunate that it will do this for
            --  padding fields, and maybe we can fix this someday, but for
            --  now, create as few components as possible, with each properly
            --  aligned, so as to be the most efficient. Normally, we're
            --  padding to align or make space, so first pad with increasing
            --  size pieces, then large pieces, then decreasing size pieces.

            if Left_To_Pad >= UBPU and then Cur_RI_Pos mod (2 * UBPU) /= 0 then
               Append_Padding (BPU, 1);
            end if;

            if Left_To_Pad >= UBPU * 2
              and then Cur_RI_Pos mod (4 * UBPU) /= 0
            then
               Append_Padding (BPU * 2, 1);
            end if;

            if Left_To_Pad >= UBPU * 4
              and then Cur_RI_Pos mod (8 * UBPU) /= 0
            then
               Append_Padding (BPU * 4, 1);
            end if;

            Append_Padding (BPU * 8, Left_To_Pad / (UBPU * 8));

            if Left_To_Pad >= UBPU * 4 then
               Append_Padding (BPU * 4, Left_To_Pad / (UBPU * 4));
            end if;

            if Left_To_Pad >= UBPU * 2 then
               Append_Padding (BPU * 2, Left_To_Pad / (UBPU * 2));
            end if;

            if Left_To_Pad >= UBPU then
               Append_Padding (BPU, Left_To_Pad / UBPU);
            end if;
         end if;

      end Force_To_Pos;

      ---------------------------
      -- Process_Fields_To_Add --
      ---------------------------

      procedure Process_Fields_To_Add is
         Reorder                : constant Boolean   :=
           Convention (BT) = Convention_Ada and then not No_Reordering (BT)
           and then not Debug_Flag_Dot_R and then not Is_Tagged_Type (BT)
           and then not (if   Is_Packed (BT) then Has_NP_Fixed
                         else Optimize_Alignment_Space (BT));
         --  Says that it's OK to reorder fields in this record. We don't
         --  reorder for tagged records since an extension could add an
         --  aliased field but we must have the same ordering in
         --  extensions.

         In_Variant             : constant Boolean   :=
           Variant_Stack.Last /= 0;
         --  True if we're processing inside a variant, either static
         --  or dynamic.

         In_Dynamic_Variant     : constant Boolean   :=
           In_Variant
           and then not Variant_Stack.Table (Variant_Stack.Last).Is_Static;
         --  True if we're inside a dynamic variant

         Last_Var_Depth         : Int                := 0;
         --  The last variant depth that we saw for a field; used to indicate
         --  when the depth changes.

         Last_Par_Depth         : Int                := 0;
         --  Likewise for the last parent depth that we saw for a field

         Parent_TE              : Opt_Record_Kind_Id := Empty;
         --  The type of the last parent record that we've seen

         Had_Non_Repped         : Boolean            := False;
         --  True once we saw a non-repped field; used to ensure that all
         --  non-repped fields are positioned after all repped fields.

         Packed_Field_Bitpos    : Uint               := No_Uint;
         --  Our current position in the packed field type

         Forced_Pos             : ULL                := 0;
         --  If nonzero, a position to force the next field to

         Bitfield_Start_Pos     : Uint               := No_Uint;
         Bitfield_End_Pos       : Uint;
         --  Starting and ending (last plus one) positions of an LLVM
         --  field being used to contain multiple bitfields, if not
         --  No_Uint.

         Bitfield_Is_Array       : Boolean;
         Bitfield_Is_Large_Array : Boolean;
         --  Show if the bitfield field we make is an array and if a large one

         Next_Align              : Nat                := 1;
         --  An alignment to impose on the next field even if stricter than
         --  that needed for that field, for example if the previous field
         --  is a strict-alignment type.

         procedure Move_Aliased_Fields;
         --  If we're not reordering, we still can't have any aliased fields
         --  after a field that depends on the discriminant, but we can't do
         --  this minimal reordering with a sort (since we can't make a strict
         --  weak ordering in that case), so we have to do it manually.

         function Field_Before (L, R : Int) return Boolean;
         --  Determine the sort order of two fields in Added_Fields

         procedure Swap_Fields (L, R : Int);
         --  Swap the fields in Added_Fields with the above indices

         procedure Sort is new Ada.Containers.Generic_Sort
           (Index_Type => Int, Before => Field_Before, Swap => Swap_Fields);

         function Start_Position (Pos : Uint) return Uint is
           (Truncate_Pos (Pos, BPU));

         function End_Position (Pos, Size : Uint) return Uint is
           (Align_Pos (Pos + Size, BPU));

         function Fits_In_Bitfield_Field (AF : Added_Field) return Boolean;
         --  True if the field denoted by AF fits in the current bitfield
         --  field.

         procedure Create_Bitfield_Field (J : Int);
         --  We're processing the component at table index J, which is known
         --  to be a bitfield. Create an LLVM field to hold contents of
         --  the J'th field to process, which is known to be a bitfield.

         procedure Flush_Types;
         --  Like Flush_Current_types, but also reset variables we maintain

         -------------------------
         -- Move_Aliased_Fields --
         -------------------------

         procedure Move_Aliased_Fields is
            type AF_Array is array (Nat range <>) of Added_Field;

            AFs               : AF_Array (1 .. Added_Fields.Last);
            Seq               : Nat := 0;
            First_Discrim_Use : Nat := 0;
            Has_Late_Aliased  : Boolean := False;
         begin
            --  Populate AFs and check if we have an aliased field after
            --  something that uses a discriminant.

            for J in 1 .. Added_Fields.Last loop
               declare
                  AF : constant Added_Field := Added_Fields.Table (J);

               begin
                  AFs (J) := AF;

                  if First_Discrim_Use = 0
                    and then Uses_Discriminant (Full_GL_Type (AF.AF))
                  then
                     First_Discrim_Use := J;
                  elsif First_Discrim_Use > 0
                    and then Is_Aliased (AF.AF)
                  then
                     Has_Late_Aliased := True;
                  end if;
               end;
            end loop;

            --  If we don't, we're done

            if not Has_Late_Aliased then
               return;
            end if;

            --  Otherwise rebuild the added field table. First write the
            --  fields before the one that references the discriminant,
            --  then those after that are aliased, then those after that
            --  aren't aliasd.

            Added_Fields.Set_Last (0);
            for J in 1 .. First_Discrim_Use - 1 loop
               AFs (J).Seq := Seq;
               Seq         := Seq + 1;
               Added_Fields.Append (AFs (J));
            end loop;

            for J in First_Discrim_Use .. AFs'Last loop
               if Is_Aliased (AFs (J).AF) then
                  AFs (J).Seq := Seq;
                  Seq         := Seq + 1;
                  Added_Fields.Append (AFs (J));
               end if;
            end loop;

            for J in First_Discrim_Use .. AFs'Last loop
               if not Is_Aliased (AFs (J).AF) then
                  AFs (J).Seq := Seq;
                  Seq         := Seq + 1;
                  Added_Fields.Append (AFs (J));
               end if;
            end loop;

         end Move_Aliased_Fields;

         ------------------
         -- Field_Before --
         ------------------

         function Field_Before (L, R : Int) return Boolean is

            --  When we look at properties of a component, we need to
            --  look at them on the field of the base type to be sure that
            --  we sort fields the same way for base types and its subtypes.

            AF_Left   : constant Added_Field          :=
              Added_Fields.Table (L);
            AF_Right  : constant Added_Field          :=
              Added_Fields.Table (R);
            Left_F    : constant Record_Field_Kind_Id := AF_Left.AF;
            Right_F   : constant Record_Field_Kind_Id := AF_Right.AF;
            Left_GT   : constant GL_Type              := Full_GL_Type (Left_F);
            Right_GT  : constant GL_Type              :=
              Full_GL_Type (Right_F);
            Left_BO   : constant Uint                 :=
              Component_Bit_Offset (Left_F);
            Right_BO  : constant Uint                 :=
              Component_Bit_Offset (Right_F);
            Is_Pos_L  : constant Boolean              :=
              Present (Component_Clause (Left_F));
            Is_Pos_R  : constant Boolean              :=
              Present (Component_Clause (Right_F));
            Self_L    : constant Boolean              :=
              Uses_Discriminant (Left_GT);
            Self_R    : constant Boolean              :=
              Uses_Discriminant (Right_GT);
            Dynamic_L : constant Boolean              :=
              Is_Dynamic_Size (Left_GT,  Is_Unconstrained_Record (Left_GT));
            Dynamic_R : constant Boolean              :=
              Is_Dynamic_Size (Right_GT, Is_Unconstrained_Record (Right_GT));
            Pack_L    : constant Pack_Kind            :=
              Field_Pack_Kind (Left_F);
            Pack_R    : constant Pack_Kind            :=
              Field_Pack_Kind (Right_F);
            Bit_L     : constant Boolean              :=
              (Pack_L = Bit and then RM_Size (Left_GT)  mod BPU /= 0);
            Bit_R     : constant Boolean              :=
              (Pack_R = Bit and then RM_Size (Right_GT) mod BPU /= 0);

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

            --  A discriminant is in front of a field that depends on it,
            --  which we implement more generally as in front of any
            --  variable-sized field.

            elsif Ekind (Left_F) = E_Discriminant and then Dynamic_R then
               return True;
            elsif Ekind (Right_F) = E_Discriminant and then Dynamic_L then
               return False;

               --  If we're to reorder fields, self-referential fields
               --  come after any that aren't, fixed-size fields come
               --  before variable-sized ones, and packable fields come
               --  after those are aren't.

            elsif Reorder and then Self_L and then not Self_R then
               return False;
            elsif Reorder and then not Self_L and then Self_R then
               return True;
            elsif Reorder  and then Dynamic_L and then not Dynamic_R then
               return False;
            elsif Reorder and then not Dynamic_L and then Dynamic_R then
               return True;
            elsif Reorder and then Bit_L and then not Bit_R then
               return False;
            elsif Reorder and then not Bit_L and then Bit_R then
               return True;

            --  Otherwise, keep the original sequence intact

            else
               return AF_Left.Seq < AF_Right.Seq;
            end if;
         end Field_Before;

         -----------------
         -- Swap_Fields --
         -----------------

         procedure Swap_Fields (L, R : Int) is
            Temp : constant Added_Field := Added_Fields.Table (L);

         begin
            Added_Fields.Table (L) := Added_Fields.Table (R);
            Added_Fields.Table (R) := Temp;
         end Swap_Fields;

         ----------------------------
         -- Fits_In_Bitfield_Field --
         ----------------------------

         function Fits_In_Bitfield_Field (AF : Added_Field) return Boolean is
            SP : constant Uint := Start_Position (AF.Pos);
            EP : constant Uint := End_Position (AF.Pos, AF.Size);

         begin
            return SP < Bitfield_End_Pos
              and then (not Emit_C
                        or else EP - Bitfield_Start_Pos <=
                                Get_Long_Long_Size);
         end Fits_In_Bitfield_Field;

         --  If we're emitting C, don't let the bitsize be wider than
         --  "long long" since we can't do arithmetic wider than that.

         ---------------------------
         -- Create_Bitfield_Field --
         ---------------------------

         procedure Create_Bitfield_Field (J : Int) is
            AF           : constant Added_Field := Added_Fields.Table (J);
            Bitfield_Len : ULL;

         begin
            --  We need to create an LLVM field to use to represent one or
            --  more bitfields starting at location J in the Added_Fields
            --  table. We need to continue widening the field until we run
            --  into a component that no longer overlaps any of the bits in
            --  the field or we've reached the end of the field list. If
            --  we're processing a full-access record, we aggregate all
            --  components into a single field because LLVM can split
            --  volatile load operations of records, but it guarantees that
            --  volatile loads of native integer types are never split; we
            --  know that the components will fit because we've checked
            --  when seeing the full-access attribute on the record
            --  declaration.
            --
            --  Start by making a field just wide enough for this component.

            Bitfield_Start_Pos := Start_Position (AF.Pos);
            Bitfield_End_Pos   := End_Position   (AF.Pos, AF.Size);

            --  Now go through all the remaining components that start within
            --  the field we made and widen the bitfield field to include it.
            --  If we're emitting C, don't let the bitsize be wider than
            --  "long long" since we can't do arithmetic wider than that.

            for K in J + 1 .. Added_Fields.Last loop
               declare
                  AF_K : constant Added_Field          :=
                    Added_Fields.Table (K);
                  F    : constant Record_Field_Kind_Id := AF_K.F;

               begin
                  exit when No (AF_K.Pos) or else No (AF_K.Size)
                    or else (not Is_Bitfield_By_Rep (F, AF_K.Pos, AF_K.Size,
                                                     Use_Pos_Size => True)
                             and then not Full_Access)
                    or else (not Fits_In_Bitfield_Field (AF_K)
                             and then not Full_Access);

                  Bitfield_End_Pos := End_Position (AF_K.Pos, AF_K.Size);
               end;
            end loop;

            --  We can be the most efficient if we can use a normal integral
            --  type (i8, i16, i32, or i64) for this field. If it's not
            --  the size already, the only option we have now is to see if
            --  we have padding between our current position and the start of
            --  the bitfield that we can use to widen the bitfield.
            --  Note that the comparisons and arithmetic below are done
            --  unsigned, so we have to write them to avoid wrapping.

            Bitfield_Len := +(Bitfield_End_Pos - Bitfield_Start_Pos);
            case Bitfield_Len is
               when 8 | 16 | 32 | 64 =>
                  null;

               when 24 =>
                  if Cur_RI_Pos + 8 < +Bitfield_Start_Pos then
                     Bitfield_Start_Pos := Bitfield_Start_Pos - 8;
                     Bitfield_Len       := 32;
                  end if;

               when 40 | 48 | 56 =>
                  if Cur_RI_Pos + (64 - Bitfield_Len) < +Bitfield_Start_Pos
                  then
                     Bitfield_Start_Pos :=
                       Bitfield_Start_Pos - Int (64 - Bitfield_Len);
                     Bitfield_Len       := 64;
                  end if;

               when others =>
                  null;
            end case;

            --  Now we know what we have to do for the bitfield field

            Force_To_Pos (+Bitfield_Start_Pos);

            if Bitfield_Len in 8 | 16 | 32 | 64 then
               Bitfield_Is_Array       := False;
               Bitfield_Is_Large_Array := False;
               LLVM_Types.Append (Int_Ty (Bitfield_Len));
            else
               Bitfield_Is_Array := True;
               Bitfield_Is_Large_Array :=
                 Bitfield_Len > ULL (Get_Bits_Per_Word);
               LLVM_Types.Append (Array_Type
                                    (Byte_T,
                                     unsigned (To_Bytes (Bitfield_Len))));
            end if;

            C_Set_Field_Info (UID, LLVM_Types.Last, Is_Bitfield => True);
            Cur_RI_Pos := +Bitfield_End_Pos;
         end Create_Bitfield_Field;

         -----------------
         -- Flush_Types --
         -----------------

         procedure Flush_Types is
         begin
            Flush_Current_Types;
            Packed_Field_Bitpos := No_Uint;
            Bitfield_Start_Pos  := No_Uint;
         end Flush_Types;

      begin  -- Start of processing for Process_Fields_To_Add

         --  If we're not to reorder, we have aliased components, and
         --  this is a discriminated record, we have to take steps to
         --  move aliased components further up, if needed.

         if not Reorder and then Has_Discriminants (BT) and then Aliased_Fields
         then
            Move_Aliased_Fields;
         end if;

         --  Then do any other required sorting

         Sort (1, Added_Fields.Last);

         --  If we're just elaborating types and this is a tagged record,
         --  we have to allow for the tag field because the front end
         --  won't create one in this mode.

         if Decls_Only and then Is_Tagged_Type (TE) and then No (Prev_Idx)
           and then Variant_Stack.Last = 0
         then
            LLVM_Types.Append (Void_Ptr_T);
            Cur_RI_Pos := Cur_RI_Pos + Get_Type_Size (Void_Ptr_T);
         end if;

         for J in 1 .. Added_Fields.Last loop
            declare
               AF        : Added_Field renames Added_Fields.Table (J);
               F         : constant Record_Field_Kind_Id := AF.F;
               --  The field to add

               Def_GT    : constant GL_Type              :=
                 Default_GL_Type (Full_Etype (F));
               --  The default GL_Type for that field

               Size      : Uint                           := AF.Size;
               --  An optional size to force the field to

               Max_Sz    : constant Boolean               :=
                 Is_Unconstrained_Record (Def_GT);
               --  True if this is an object for which we have to use
               --  the maximum possible size.

               Biased    : constant Boolean               :=
                 Has_Biased_Representation (F);
               --  True if we need a biased representation for this field

               Packed    : constant Pack_Kind             :=
                 Field_Pack_Kind (AF.AF);
               --  The kind of packing we need to do for this field

               F_GT      : GL_Type                        :=
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

               Pos     : Uint                             := AF.Pos;
               --  Specified bit position of field, if any.

               Need_Align  :  Nat                         :=
                 (if   Present (Pos)
                  then BPU else Effective_Field_Alignment (F));
               --  The alignment we need this field to have

            begin
               --  If the parent depth has decreased and the previous
               --  parent has strict alignment, align our position to that
               --  of its type. Then save the potential current parent.

               if AF.Par_Depth < Last_Par_Depth and then Present (Parent_TE)
                 and then Strict_Alignment (Parent_TE)
               then
                  Need_Align :=
                    Nat'Max (Need_Align,
                             Get_Record_Type_Alignment (Parent_TE));
               end if;

               Last_Par_Depth := AF.Par_Depth;

               if Last_Par_Depth /= 0 then
                  Parent_TE := Full_Scope (F);
               end if;

               --  If we had a field that forces this one to an alignment,
               --  do that and set that value if needed for this field.

               Need_Align := Nat'Max (Need_Align, Next_Align);
               Next_Align := (if   Strict_Alignment (F_GT)
                              then Effective_Field_Alignment (F) else 1);

               --  If we've pushed into a new static variant, see if
               --  we need to align it. But update our level anyway and
               --  clear out any starting location for packed fields.
               --  Ignore if there's a position specified.

               if AF.Var_Depth /= Last_Var_Depth and then No (Pos) then
                  if AF.Var_Depth > Last_Var_Depth and then AF.Var_Align /= 0
                  then
                     Need_Align  := AF.Var_Align;
                  end if;

                  Last_Var_Depth := AF.Var_Depth;
                  Packed_Field_Bitpos := No_Uint;
               end if;

               --  If we're at the start of a continuation RI, set its
               --  alignment to where we need to be aligned.

               if Cur_RI_Pos = 0 and then Present (Prev_Idx) then
                  RI_Align := Nat'Max (RI_Align, Need_Align);
               end if;

               --  If this isn't a packable field and we haven't already
               --  set its position, but we've previously set up a location
               --  for packable fields, clear out that location.

               if Present (Packed_Field_Bitpos) and then No (Pos)
                 and then Packed /= Bit
               then
                  Packed_Field_Bitpos := No_Uint;

               --  If this is a packable field and we haven't set up a
               --  location for them, set one up and initialize the position
               --  and size of this field as well as following ones.

               elsif No (Packed_Field_Bitpos) and then Packed = Bit then
                  Pos                 := +Cur_RI_Pos;
                  Size                := RM_Size (F_GT);
                  Packed_Field_Bitpos := Pos + Size;
                  AF.Pos              := Pos;
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

                  for K in J + 1 .. Added_Fields.Last loop
                     declare
                        AF_K : Added_Field renames Added_Fields.Table (K);

                     begin
                        exit when Bit /= Field_Pack_Kind
                          (AF_K.AF, Packed_Field_Bitpos mod BPU /= 0);
                        exit when AF_K.Var_Depth /= Last_Var_Depth;

                        AF_K.Pos            := Packed_Field_Bitpos;
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

               if No (Pos) and then Chars (F) /= Name_uTag then
                  if not In_Variant and then not Had_Non_Repped then
                     Forced_Pos := Align_Pos (+Max_Record_Rep (F), Need_Align);
                  end if;

                  Had_Non_Repped := True;
               end if;

               --  If this field is a non-native type, we have to close out
               --  the last record info entry we're making, if there's
               --  anything in it, and make a piece for this field.

               if Is_Nonnative_Type (F_GT) then
                  --  ??  This is the only case where we use an F_GT that
                  --  might have been modified by Add_FI. We need to be
                  --  sure that's OK.

                  Flush_Types;

                  --  If we're forcing the position of this field, set that
                  --  as the starting position of the RI we're about to make.

                  if Forced_Pos /= 0 then
                     RI_Position := Forced_Pos;
                     RI_Align    := BPU;
                     Forced_Pos  := 0;
                  end if;

                  Add_FI (F, Cur_Idx, F_GT);
                  Add_RI (F_GT         => F_GT,
                          Align       => Need_Align,
                          Unused_Bits => Get_Unused_Bits (F_GT));
                  Set_Is_Nonnative_Type (TE);
                  Split_Align := Nat'Max (Need_Align, BPU);
                  RI_Align    := 0;

               --  If it's a native type, add it to the current set of
               --  fields and make a field descriptor.

               else
                  --  We need to flush the previous types if required
                  --  by the alignment. We assume here that if Add_FI
                  --  updates our type that it has the same alignment.

                  if Need_Align > Split_Align then
                     Flush_Types;
                     Set_Is_Nonnative_Type (TE);
                     RI_Align    := Need_Align;
                     Split_Align := Nat'Max (Need_Align, BPU);

                  --  If we're in the overlap section of a variant and we've
                  --  run out of components that have a position, end the
                  --  overlap section.

                  elsif No (Pos) and then RI_Is_Overlap then
                     Flush_Types;
                     RI_Is_Overlap := False;

                  --  If we're in a dynamic variant and have a component
                  --  clause,  show that we're building a overlap RI.

                  elsif In_Dynamic_Variant
                    and then Present (Component_Clause (F))
                  then
                     RI_Is_Overlap := True;
                  end if;

                  declare
                     F_T          : constant Type_T := Type_Of (F_GT);
                     T            : constant Type_T :=
                       (if   Decls_Only
                             and then Get_Type_Kind (F_T) = Void_Type_Kind
                        then Byte_T else F_T);
                     --  LLVM type to use

                     Needed_Pos  : constant ULL    :=
                       (if    Present (Pos)   then +Pos
                        elsif Forced_Pos /= 0 then Forced_Pos
                        else  Align_Pos (Cur_RI_Pos, Need_Align));
                     --  The position we need to be at, either by virtue of
                     --  a specified position alignment or because it's
                     --  forced there in a complex partially-repped variant
                     --  case.

                  begin
                     --  If this is a bitfield, we'll be using the special
                     --  "bitfield field". If we don't fit in the current
                     --  one or there isn't one, make one. Otherwise, just
                     --  record this field. If we have a truncated type
                     --  that's not the last field, or a field in a
                     --  full-access type, also treat it as a bitfield.

                     if Is_Bitfield_By_Rep (F, Pos, Size, Use_Pos_Size => True)
                       or else (Is_Truncated_GL_Type (F_GT)
                                  and then J /= Added_Fields.Last)
                       or else (Present (Pos) and then Present (Size)
                                  and then Full_Access)
                     then
                        if No (Bitfield_Start_Pos)
                          or else not Fits_In_Bitfield_Field (AF)
                        then
                           Create_Bitfield_Field (J);
                        end if;

                        Add_FI
                          (F, Cur_Idx, F_GT,
                           Ordinal              => LLVM_Types.Last,
                           First_Bit            => Pos - Bitfield_Start_Pos,
                           Num_Bits             => Size,
                           Array_Bitfield       => Bitfield_Is_Array,
                           Large_Array_Bitfield => Bitfield_Is_Large_Array);
                        RI_Unused_Bits := Bitfield_End_Pos - (Pos + Size);
                     else
                        Force_To_Pos (Needed_Pos);
                        LLVM_Types.Append (T);
                        C_Set_Field_Info (UID, LLVM_Types.Last,
                                          Get_Ext_Name (F), F);

                        Cur_RI_Pos :=
                          Align_Pos (Cur_RI_Pos + Get_Type_Size (T), BPU);
                        Add_FI (F, Cur_Idx, F_GT, Ordinal => LLVM_Types.Last);
                        RI_Unused_Bits := Get_Unused_Bits (F_GT);
                        Forced_Pos := 0;
                     end if;
                  end;
               end if;
            end;
         end loop;

         Added_Fields.Set_Last (0);
      end Process_Fields_To_Add;

   --  Start of processing for Create_Record_Type

   begin
      --  Because of the potential recursion between record and access types,
      --  make a dummy type for us and set it as our type right at the start.
      --  Then initialize our first record info table entry, which we know
      --  will be used. But first see if the dummy type was already made.

      if No (GT) then
         GT := New_GT (TE);
      end if;

      LLVM_Type := Type_Of (GT);

      if No (LLVM_Type) then
         pragma Assert (Is_Empty_GL_Type (GT));
         LLVM_Type := Struct_Create_Named (Get_Ext_Name (TE));
      end if;

      Update_GL_Type (GT, LLVM_Type, True);

      --  See if we have any non-packable fields of fixed size

      Cur_Field := First_Component_Or_Discriminant (BT);
      while Present (Cur_Field) loop
         if None = Field_Pack_Kind (Cur_Field, Force => True,
                                    Ignore_Size => True)
           and then not Is_Nonnative_Type (Full_GL_Type (Cur_Field))
         then
            Has_NP_Fixed := True;
         end if;

         Next_Component_Or_Discriminant (Cur_Field);
      end loop;

      --  Add all the fields into the record

      Record_Info_Table.Increment_Last;
      Cur_Idx := Record_Info_Table.Last;
      Set_Record_Info (TE, Cur_Idx);
      Add_Fields (TE);
      Process_Fields_To_Add;

      --  If we haven't yet made any record info entries, it means that
      --  this is a fixed-size record that can be just an LLVM type,
      --  so use the one we made.

      if No (Prev_Idx) then

         --  If this is a strict-alignment type, pad this record to pad the
         --  primitive size.

         if Strict_Alignment (TE) then
            Force_To_Pos (Align_Pos (Cur_RI_Pos,
                                     Get_Record_Type_Alignment (BT)));
         end if;

         Struct_Set_Body (LLVM_Type,
                          Type_Array (LLVM_Types.Table (0 .. LLVM_Types.Last)),
                          Packed => True);
         C_Set_Struct (UID, LLVM_Type);
         Add_RI (T           => LLVM_Type,
                 Align       => RI_Align,
                 Unused_Bits => RI_Unused_Bits);
      else
         --  Otherwise, close out the last record info if we have any
         --  fields. Note that if we don't have any fields, the entry we
         --  allocated will remain unused, but trying to reclaim it is
         --  risky.

         Flush_Current_Types;
      end if;

      --  If we have a new discriminant that renames one from our parent,
      --  we need to mark which field the discriminant corresponds to. So
      --  make a pass over the discriminants of this type seeing if any
      --  haven't had field information set. If we find any, copy it from
      --  the original field.

      if Has_Discriminants (BT) and then not Is_Unchecked_Union (BT) then
         Field := First_Discriminant (BT);
         while Present (Field) loop
            declare
               ORC         : constant Record_Field_Kind_Id     :=
                 Original_Record_Component (Field);
               Discrim_Num : constant Nat                      :=
                 +Discriminant_Number (ORC);
               Outer_Orig  : constant Opt_Record_Field_Kind_Id :=
                 Find_Field_In_Entity_List (ORC, TE, Cur_Field);
               Outer_Field : Opt_Record_Field_Kind_Id;

            begin
               Outer_Field := Find_Field_In_Entity_List (Field, TE, Cur_Field);

               if Present (Outer_Field)
                 and then No (Get_Field_Info (Outer_Field))
                 and then Scope (ORC) = BT
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
               Byte_Position  : constant BA_Data         :=
                 Field_Position (Cur_Field, No_GL_Value) / UBPU;
               Bit_Offset     : constant Uint            :=
                 Field_Bit_Offset (Cur_Field);
               Bit_Position   : constant BA_Data         :=
                 To_Bits (Byte_Position) + Bit_Offset;
               Bit_Position_T : constant Node_Ref_Or_Val :=
                 Annotated_Value (Bit_Position);
               Field_Size     : constant Node_Ref_Or_Val :=
                 Annotated_Value (Get_Type_Size (Field_Type (Cur_Field)));

            begin
               if not Known_Esize (Cur_Field) and then Present (Field_Size)
               then
                  Set_Esize (Cur_Field, Field_Size);
               end if;

               if Present (Bit_Position_T) then
                  Set_Component_Bit_Offset (Cur_Field, Bit_Position_T);

                  if Is_Static_SO_Ref (Bit_Position_T) then
                     Set_Normalized_Position
                       (Cur_Field, Bit_Position_T / BPU);
                     Set_Normalized_First_Bit
                       (Cur_Field, Bit_Position_T mod BPU);
                  else
                     Set_Normalized_Position
                       (Cur_Field,
                        Annotated_Value (Byte_Position + Bit_Offset / BPU));
                     Set_Normalized_First_Bit (Cur_Field, Bit_Offset mod BPU);
                  end if;
               end if;
            end;
         end if;

         Next_Component_Or_Discriminant (Cur_Field);
      end loop;

      --  If requested, dump the structure of this record

      if Debug_Flag_Underscore_RR then
         pg (Union_Id (TE));
         Print_Record_Info (TE, Eol => True);
      end if;

      return LLVM_Type;
   end Create_Record_Type;

begin
   --  Make a dummy entry in the record and field tables, so the
   --  "Empty" entry is never used.

   Record_Info_Table.Increment_Last;
   Field_Info_Table.Increment_Last;
end GNATLLVM.Records.Create;
