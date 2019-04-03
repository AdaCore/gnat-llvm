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

with Ada.Containers.Generic_Constrained_Array_Sort;

with Elists;   use Elists;
with Get_Targ; use Get_Targ;
with Nlists;   use Nlists;
with Output;   use Output;
with Repinfo;  use Repinfo;
with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Snames;   use Snames;
with Sprint;   use Sprint;
with Table;    use Table;

with LLVM.Core;  use LLVM.Core;

with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Conditionals; use GNATLLVM.Conditionals;
with GNATLLVM.DebugInfo;    use GNATLLVM.DebugInfo;
with GNATLLVM.Exprs;        use GNATLLVM.Exprs;
with GNATLLVM.Subprograms;  use GNATLLVM.Subprograms;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Variables;    use GNATLLVM.Variables;

package body GNATLLVM.Records is

   --  We can't represent all records by a single native LLVM type, so we
   --  create two data structures to represent records and the positions of
   --  fields within the record.
   --
   --  The Record_Info type is the format of an entry in the
   --  Record_Info_Table, indexed by the Record_Info_Id type.  The
   --  Field_Info type is the format of an entry in the Field_Info_Table,
   --  indexed by the Field_Info_Id type.  Get_Record_Info applied to a
   --  record type points to a Record_Info_Id, which is the start of the
   --  description of the record. Get_Field_Info for each field points to a
   --  Field_Info_Id, which contains information about how to locate that
   --  field within the record.  Record_Info objects are chained.  For
   --  variant records, we use one chain for the common part of the record
   --  and chain for each variant.
   --
   --  The Record_Info data is used to compute the size of a record and, in
   --  conjunction with the Field_Info data, to determine the offset of a
   --  field from the start of an object of that record type.  We record
   --  information for each subtype separately.
   --
   --  A single Record_Info item can represent one of the following:
   --
   --      nothing, meaning that either the record or part of a variant
   --      record is empty
   --
   --      the variant part of a record
   --
   --      a single GL_Type, which must be a non-native (and hence usually
   --      of dynamic size)
   --
   --      a single LLVM type, which is a struct containing one or more
   --      fields
   --
   --  A Field_Info type locates a record by saying in which Record_Info
   --  piece it's located and, in the case where that piece contains an
   --  LLVM type, how to locate the field within that type.
   --
   --  A simple record (unpacked, with just scalar components) is
   --  represented by a single Record_Info item which points to the LLVM
   --  struct type corresponding to the Ada record.  More complex but
   --  non-variant cases containing variable-sized objects require a mix of
   --  Record_Info items corresponding to LLVM and GL types.  Note that a
   --  reference to a discriminant is handled within the description of
   --  array types.
   --
   --  For more complex records, the LLVM type generated may not directly
   --  correspond to that of the Ada type for two reasons.  First, the
   --  GL_Type of a field may have an alignment larger than the alignment
   --  of the native LLVM type of that field or there may be record rep
   --  clauses that creates holes either at the start of a record or
   --  between Ada fields.  In both of those cases, we add extra fields to
   --  the LLVM type to reflect the padding.
   --
   --  Secondly, LLVM doesn't support bitfields, so we have to do the work
   --  of generating the corresponding operations directly.  We make a
   --  field corresponding to a primitive scalar type with the proper size
   --  and alignments to represent one or more bit fields.  In the
   --  Field_Info item corresponding to each bitfield, we identify the
   --  ordinal of the field in the LLVM type as well as the starting bit
   --  position and bit size.
   --
   --  A fixed-size field may have an alignment requirement that's stricter
   --  than the alignment of the corresponding LLVM type, so we need to record
   --  the requested alignment in the Record_Info object.
   --
   --  For packed records, we use a packed LLVM struct type and also
   --  manually lay out fields that become bitfields.
   --
   --  For a variant part, we record the following in the corresponding
   --  Record_Info item:
   --
   --      A pointer to the GNAT tree for the variant part (to obtain the
   --      discriminant value corresponding to each variant)
   --
   --      The expression to be evaluated (which may be a reference to a
   --      discriminant) to determine which variant is present
   --
   --      An array of Record_Info chains (corresponding to the order in
   --      the GNAT tree) for each variant.  The offset of each of these
   --      chains starts at the offset of the variant Record_Info item.
   --
   --      An array of Record_Info items (in the same order) corresponding
   --      to any fields that are repped into a fixed position.  The
   --      relative offset of these fields is zero.

   type Record_Info_Base is record
      LLVM_Type        : Type_T;
      --  The LLVM type corresponding to this fragment, if any

      GT               : GL_Type;
      --  The GL_Type corresponding to this fragment, if any

      Align            : ULL;
      --  If specified, the alignment of this piece

      Next             : Record_Info_Id;
      --  Link to the next Record_Info entry for this record or variant

      Variant_List     : List_Id;
      --  List in GNAT tree of the variants for this fragment

      Variant_Expr     : Node_Id;
      --  Expression to evaluate to determine which variant is present

      Variants         : Record_Info_Id_Array_Access;
      --  Pointer to array of Record_Info_Ids representing the variants,
      --  which must be in the same order as in Variant_List.

      Overlap_Variants : Record_Info_Id_Array_Access;
      --  Likewise for any part of the variant who offset starts at
      --  the beginning of a record (for field with record rep
      --  clauses).
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

   --  The information for a field is the index of the piece in the record
   --  information and optionally the location within the piece in the case
   --  when the Record_Info is an LLVM_type.  We also record the GL_Type
   --  used to represent the field.

   type Field_Info is record
      Rec_Info_Idx  : Record_Info_Id;
      --  Index into the record info table that contains this field

      Field_Ordinal : Nat;
      --  Ordinal of this field within the contents of the record info table

      GT            : GL_Type;
      --  The GL_Type correspond to this field, which takes into account
      --  a possible change in size
   end record;

   package Field_Info_Table is new Table.Table
     (Table_Component_Type => Field_Info,
      Table_Index_Type     => Field_Info_Id'Base,
      Table_Low_Bound      => Field_Info_Low_Bound,
      Table_Initial        => 1000,
      Table_Increment      => 100,
      Table_Name           => "Record_Info_Table");

   --  When computing the size of a record subtype, we push the subtype so
   --  we can see if we run into a discriminant from its base type.  If we
   --  do, we substitute the expression that corresponds to the discriminant
   --  type.  In most cases, but not all, the front end already does this
   --  substitution for us.  However, we have to be sure that we don't use
   --  the same entry more than once since this could cause infinite recursion.

   type SS_Entry is record
     TE   : Entity_Id;
      Used : Boolean;
   end record;

   package Subtype_Stack is new Table.Table
     (Table_Component_Type => SS_Entry,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 2,
      Table_Name           => "Subtype_Stack");

   function Count_Entities (E : Entity_Id) return Nat
     with Pre => Present (E);
   --  Return the number of entities of E.  This value will be used only
   --  to allocate an array that we know is large enough to contain all
   --  the fields, so we can overestimate the number of fields (even
   --  greatly), but can't underestimate.

   function Variant_Alignment (Var_Part : Node_Id) return ULL
     with Pre => Present (Var_Part);
   --  Compute the alignment of the variant at Var_Part, which is the
   --  maximum size of any field in the variant.  We recurse through
   --  any nested variants.

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

   function Find_Matching_Field
     (TE : Entity_Id; Field : Entity_Id) return Entity_Id
     with Pre  => Is_Record_Type (TE)
     and then Ekind_In (Field, E_Discriminant, E_Component),
     Post => Chars (Field) = Chars (Find_Matching_Field'Result);
   --  Likewise, but without caching and compare by Chars

   function Get_Discriminant_Constraint
     (TE : Entity_Id; E : Entity_Id) return Node_Id
     with Pre  => Ekind (TE) = E_Record_Subtype,
          Post => Present (Get_Discriminant_Constraint'Result);
   --  Get the expression that constrains the discriminant E of type TE

   function Get_Variant_Size (RI : Record_Info; V : GL_Value) return GL_Value
     with Pre  => RI.Variants /= null or else RI.Overlap_Variants /= null,
          Post => Present (Get_Variant_Size'Result);
   --  Get the size of a fragment known to be a variant and
   --  where we're not getting the maximum size.

   function Variant_Size (RI : Record_Info; V : GL_Value) return IDS
     with Pre  => RI.Variants /= null or else RI.Overlap_Variants /= null,
          Post => Present (Variant_Size'Result);
   --  Version of above for Is_Dynamic_Size

   function Variant_Size (RI : Record_Info; V : GL_Value) return BA_Data
     with Pre  => RI.Variants /= null or else RI.Overlap_Variants /= null;
   --  Version of above for back-annotation

   function Field_Position (E : Entity_Id; V : GL_Value) return BA_Data
     with Pre => Ekind_In (E, E_Component, E_Discriminant);
   --  Back-annotation version of Emit_Field_Position

   --  We put the routines used to compute sizes into a generic so that we
   --  can instantiate them using various types of sizing.  The most common
   --  case is an actual size computation, where we produce a GL_Value.
   --  But we may also instantiate this package to generate the structure
   --  needed for back-annotation.

   generic
      type Result is private;
      Empty_Result : Result;
      with function Sz_Const
        (C : ULL; Sign_Extend : Boolean := False) return Result;
      with function Sz_Type_Size
        (GT         : GL_Type;
         V          : GL_Value := No_GL_Value;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result;
      with function Sz_Variant_Size
        (RI : Record_Info; V : GL_Value) return Result;
      with function  Sz_I_Cmp
        (Op : Int_Predicate_T;
         LHS : Result;
         RHS : Result;
         Name : String := "") return Result;
      with function  "+" (V1, V2 : Result) return Result;
      with function  "-" (V1, V2 : Result) return Result;
      with function  Sz_Neg (V : Result; Name : String := "") return Result;
      with function  Sz_And
        (V1, V2 : Result; Name : String := "") return Result;
      with function  Sz_Select
        (V_If, V_Then, V_Else : Result; Name : String := "") return Result;
      with function  Sz_Min
        (V1, V2 : Result; Name : String := "") return Result;
      with function  Sz_Max
        (V1, V2 : Result; Name : String := "") return Result;
      with function  Sz_Is_Const (V : Result) return Boolean;
      with function  Sz_Const_Val (V : Result) return ULL;
   package Size is

      function No      (V : Result) return Boolean is (V =  Empty_Result);
      function Present (V : Result) return Boolean is (V /= Empty_Result);

      procedure Get_RI_Info
        (RI          : Record_Info;
         V           : GL_Value;
         Max_Size    : Boolean;
         Size        : out Result;
         Must_Align  : out Result;
         Is_Align    : out Result;
         Return_Size : Boolean := True);
      --  Return information about a record fragment RI.  This
      --  includes its size, the amount to which this fragment must be
      --  aligned, and the amout to which the resulting size is known
      --  to be aligned.  If the size isn't wanted, don't compute it.

      function Get_Variant_For_RI
        (In_RI       : Record_Info;
         V           : GL_Value;
         Max_Size    : Boolean;
         Need_Idx    : Record_Info_Id;
         Use_Overlap : Boolean := False) return Record_Info_Id
        with Pre => Present (Need_Idx);
      --  We are at RI when walking the description for a record and
      --  it has variants.  We're looking for Need_Idx.  If Need_Idx is an
      --  index in one of the variants, return that variant.

      function Get_Variant_Max_Size (RI : Record_Info) return Result
        with Pre  => RI.Variants /= null or else RI.Overlap_Variants /= null,
             Post => Present (Get_Variant_Max_Size'Result);
      --  Get informaton corresponding to the maxium size of the variant
      --  described by In_RI.

      function Get_Record_Size_So_Far
        (TE         : Entity_Id;
         V          : GL_Value;
         Start_Idx  : Record_Info_Id;
         Idx        : Record_Info_Id;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result;
      --  Similar to Get_Record_Type_Size, but stop at record info segment Idx
      --  or the last segment, whichever comes first.  If TE is Present, it
      --  provides the default for Start_Idx and also requests alignment to
      --  TE's alignment if we're looking for the size.

      function Get_Record_Type_Size
        (TE         : Entity_Id;
         V          : GL_Value;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result
        with Pre  => Is_Record_Type (TE);
      --  Like Get_Type_Size, but only for record types

      function Emit_Field_Position (E : Entity_Id; V : GL_Value) return Result
        with Pre  => Ekind_In (E, E_Discriminant, E_Component);
      --  Compute and return the position in bytes of the field specified by E
      --  from the start of its type as a value of Size_Type.  If Present, V
      --  is a value of that type, which is used in the case of a
      --  discriminated record.

      function Align_To (V, Cur_Align, Must_Align : Result) return Result;
      --  V is a value aligned to Cur_Align.  Ensure that it's aligned to
      --  Align_To.

   end Size;

   ------------------------
   --  RI_Value_Is_Valid --
   ------------------------

   function RI_Value_Is_Valid (RI : Record_Info_Base) return Boolean is
   begin
      --  This must be an LLVM Type, which is a struct, a GL_Type, or a
      --  variant and only one of those.

      if Present (RI.LLVM_Type) then
         return No (RI.GT) and then RI.Variants = null
           and then Get_Type_Kind (RI.LLVM_Type) = Struct_Type_Kind;
      elsif Present (RI.GT) then
         --  We already know that LLVM_Type isn't Present

         return RI.Variants = null and then RI.Overlap_Variants = null;
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
      Elmt    : Entity_Id := First_Component_Or_Discriminant (E);

   begin
      return Count : Nat := 0 do
         while Present (Elmt) loop
            Count := Count + (if Chars (Elmt) = Name_uParent
                              then Count_Entities (Full_Etype (Elmt)) else 1);
                              Next_Component_Or_Discriminant (Elmt);
         end loop;
      end return;
   end Count_Entities;

   -----------------------
   -- Variant_Alignment --
   -----------------------

   function Variant_Alignment (Var_Part : Node_Id) return ULL is
      Align         : ULL := 1;
      Variant       : Node_Id := First_Non_Pragma (Variants (Var_Part));

   begin
      while Present (Variant) loop
         declare
            Comp_List      : constant Node_Id := Component_List (Variant);
            Nested_Variant : constant Node_Id := Variant_Part (Comp_List);
            Comp_Def       : Node_Id          :=
              First_Non_Pragma (Component_Items (Component_List (Variant)));

         begin
            while Present (Comp_Def) loop
               Align := ULL'Max (Align,
                                 Get_Type_Alignment
                                   (Full_GL_Type (Defining_Identifier
                                                    (Comp_Def))));
               Next_Non_Pragma (Comp_Def);
            end loop;

            if Present (Nested_Variant) then
               Align := ULL'Max (Align, Variant_Alignment (Nested_Variant));
            end if;
         end;

         Next_Non_Pragma (Variant);
      end loop;

      return Align;
   end Variant_Alignment;

   ---------------------------------
   --  Use_Discriminant_For_Bound --
   ---------------------------------

   function Use_Discriminant_For_Bound (E : Entity_Id) return GL_Value is
      Rec_Type   : constant Entity_Id := Full_Scope (E);
      TE         : constant Entity_Id := Full_Etype (E);

   begin
      --  See if we've pushed a subtype of this record type into our
      --  stack of record subtypes.  If so, get the discriminant constraint
      --  from that subtype.  But ignore a constraint on this discriminant
      --  that just repeats the discriminant.

      for J in reverse 1 .. Subtype_Stack.Last loop
         declare
            SSE : SS_Entry renames Subtype_Stack.Table (J);

         begin
            if Full_Base_Type (SSE.TE) = Rec_Type and then not SSE.Used then
               SSE.Used := True;
               return Emit_Convert_Value
                 (Get_Discriminant_Constraint (SSE.TE, E),
                  Default_GL_Type (TE));
            end if;
         end;
      end loop;

      --  Otherwise, use a value that we pushed onto the LValue stacka

      return
        Get (Record_Field_Offset (Get_Matching_Value (Rec_Type), E), Data);

   end Use_Discriminant_For_Bound;

   ---------------------------------
   -- Get_Discriminant_Constraint --
   ---------------------------------

   function Get_Discriminant_Constraint
     (TE : Entity_Id; E : Entity_Id) return Node_Id
   is
      Discrim_Num : constant Uint      := Discriminant_Number (E);
      Constraint  : constant Elist_Id  := Stored_Constraint (TE);
      Elmt        : Elmt_Id            := First_Elmt (Constraint);

   begin
      --  Skip to the proper entry in the list and see if it's static

      for J in 1 .. UI_To_Int (Discrim_Num) - 1 loop
         Next_Elmt (Elmt);
      end loop;

      return Node (Elmt);
   end Get_Discriminant_Constraint;

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

   -------------------------
   -- Find_Matching_Field --
   -------------------------

   function Find_Matching_Field
     (TE : Entity_Id; Field : Entity_Id) return Entity_Id
   is
      Ent : Entity_Id := First_Component_Or_Discriminant (TE);

   begin
      while Present (Ent) loop
         exit when Chars (Ent) = Chars (Field);
         Next_Component_Or_Discriminant (Ent);
      end loop;

      return Ent;
   end Find_Matching_Field;

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
      --  part, then sort the list to deal with record rep clauses and
      --  the few cases when we reorder records, then lay out the fields
      --  into Record_Info pieces.  We start with a simple data structure
      --  that records information about the field to add and its location
      --  within the record.  We record the sequence in which we add a
      --  field because, all else being equal, we want to keep fields in
      --  that order.  We also record the depth of static variants (for the
      --  subtype case) that we are because we must not move non-repped
      --  field across that boundary.  And we record the alignment of the
      --  variant, if that depth is nonzero.

      type Added_Field is record
         F         : Entity_Id;
         Seq       : Int;
         Var_Depth : Int;
         Var_Align : ULL;
      end record;

      package Added_Field_Table is new Table.Table
        (Table_Component_Type => Added_Field,
         Table_Index_Type     => Int,
         Table_Low_Bound      => 1,
         Table_Initial        => 20,
         Table_Increment      => 5,
         Table_Name           => "Added_Fieldd_Info_Table");

      Var_Depth   : Int            := 0;
      Var_Align   : ULL            := 0;

      Prev_Idx    : Record_Info_Id := Empty_Record_Info_Id;
      --  The previous index of the record table entry, if any

      First_Idx   : Record_Info_Id := Empty_Record_Info_Id;
      --  The first index used by the current call

      Cur_Idx     : Record_Info_Id;
      --  The index of the record table entry we're building

      Cur_RI_Pos  : ULL            := 0;
      --  Current position into this RI

      RI_Align    : ULL            := 0;
      --  If nonzero, an alignment to assign to the next RI built for an
      --  LLVM type.

      Types       : Type_Array (0 .. Count_Entities (TE));
      --  Array of all field types that are going into the current piece

      Next_Type   : Nat            := 0;
      --  Ordinal of next entry in Types

      Cur_Field   : Entity_Id      := Empty;
      --  Used for a cache in Find_Field_In_Entity_List to avoid quadratic
      --  behavior.

      Split_Align : ULL            := ULL (Get_Maximum_Alignment);
      --  We need to split an LLVM fragment type if the alignment of the
      --  next field is greater than both this and Last_Align.  This occurs
      --  for variant records; see details there.  It also occurs for the
      --  same reason after a variable-size field.

      GT          :  GL_Type       := Default_GL_Type (TE, Create => False);
      --  The GL_Type for this record type

      LLVM_Type   : Type_T;
      --  The LLVM type for this record type

      Field       : Entity_Id;
      --  Temporary for loop over discriminants

      Discrim_FIs : Field_Info_Id_Array :=
        (1 .. Count_Entities (TE) + 1 => Empty_Field_Info_Id);
      --  In entry J, we record the Field_Info corresponding to the
      --  discriminant number J.  We use this for record subtypes of
      --  derived types.

      procedure Add_RI
        (T                : Type_T                      := No_Type_T;
         F_GT             : GL_Type                     := No_GL_Type;
         Align            : ULL                         := 0;
         Variant_List     : List_Id                     := No_List;
         Variant_Expr     : Node_Id                     := Empty;
         Variants         : Record_Info_Id_Array_Access := null;
          Overlap_Variants : Record_Info_Id_Array_Access := null);
      --  Add a Record_Info into the table, chaining it as appropriate

      procedure Add_FI
        (E       : Entity_Id;
         RI_Idx  : Record_Info_Id;
         Ordinal : Nat;
         F_GT    : in out GL_Type)
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
         Align            : ULL                         := 0;
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
            Next             => Empty_Record_Info_Id,
            Variant_List     => Variant_List,
            Variant_Expr     => Variant_Expr,
            Variants         => Variants,
            Overlap_Variants => Overlap_Variants);

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
        (E       : Entity_Id;
         RI_Idx  : Record_Info_Id;
         Ordinal : Nat;
         F_GT    : in out GL_Type)
      is
         Matching_Field : Entity_Id;

      begin
         --  If this field really isn't in the record we're working on, it
         --  must be in a parent.  So it was correct to allocate space for
         --  it, but let the record description be from the type that it's
         --  actually in.  The fields in the entity list for this type are
         --  almost, but not quite, in the same order as in the component
         --  list, so we have to search for a field in that list with the
         --  same Original_Record_Component as this field.  And finally,
         --  if this is a hidden discriminant and we haven't yet found a
         --  place to save the value, save it in Discriminant_FIs.
         --
         --  If we're using a matching field, update F_GT to its type.

         Field_Info_Table.Append
           ((Rec_Info_Idx  => RI_Idx, Field_Ordinal => Ordinal, GT => F_GT));

         if Full_Scope (E) = TE then
            Set_Field_Info (E, Field_Info_Table.Last);
         else
            Matching_Field := Find_Field_In_Entity_List (E, TE, Cur_Field);
            if Present (Matching_Field) then
               Set_Field_Info (Matching_Field, Field_Info_Table.Last);
               F_GT := Full_GL_Type (Matching_Field);
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

         Rec_Field : Entity_Id := Empty;
         --  Cache used to limit quadratic behavior

         function Matches_Name (F : Entity_Id; Name : Name_Id) return Boolean
           with Pre => Ekind_In (F, E_Component, E_Discriminant);
         --  See if the field F matches Name, either because Name is
         --  specified and matches F or name is not specified and F is not
         --  a special name.

         function Find_Choice (N : Node_Id; Alts : List_Id) return Node_Id
           with Pre => Is_Static_Expression (N) and then Present (Alts);
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

         function Choices_To_SO_Ref
           (Variant : Node_Id; Discrim : Entity_Id) return SO_Ref
           with Pre => Present (Variant);
         --  Given an alternative for a variant record, return an SO_Ref
         --  corresponding to an expression that's True when that variant
         --  is present.  This is a function of the discriminant (Discrim)
         --  and constants.

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
           (List : Node_Id; From_Rec : Entity_Id; F_Name : Name_Id)
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
            Variant_Align      : constant ULL       :=
              (if Present (Var_Part) then Variant_Alignment (Var_Part) else 0);
            Var_Array          : Record_Info_Id_Array_Access;
            Saved_Cur_Idx      : Record_Info_Id;
            Saved_Prev_Idx     : Record_Info_Id;
            Saved_First_Idx    : Record_Info_Id;
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

            --  Otherwise process variants. If we statically constrain the
            --  variant, see which variant is being referenced and output
            --  that one.  Walk the proper variant here to verify that
            --  ever.  Otherwise, set up for the variant, make the entres
            --  for each variant, and then create the RI for the variant.
            --  In the case of an error, there may not be a match.

            if Static_Constraint then
               Variant := Find_Choice (Constraining_Expr, Variants (Var_Part));
               if Present (Variant) then
                  Var_Depth := Var_Depth + 1;
                  Var_Align := Variant_Align;
                  Add_Component_List (Component_List (Variant), From_Rec,
                                      No_Name);
               end if;
               return;
            end if;

            Variant := First_Non_Pragma (Variants (Var_Part));
            Process_Fields_To_Add;
            Flush_Current_Types;
            Set_Is_Nonnative_Type (TE);
            Saved_Cur_Idx   := Cur_Idx;
            Saved_Prev_Idx  := Prev_Idx;
            Saved_First_Idx := First_Idx;
            J               := 1;
            Var_Array       := new
              Record_Info_Id_Array'(1 .. List_Length_Non_Pragma
                                      (Variants (Var_Part))
                                      => Empty_Record_Info_Id);

            while Present (Variant) loop
               First_Idx := Empty_Record_Info_Id;
               if Present (Component_Items (Component_List (Variant))) then
                  Record_Info_Table.Increment_Last;
                  Prev_Idx      := Empty_Record_Info_Id;
                  Cur_Idx       := Record_Info_Table.Last;
                  Split_Align   := Variant_Align;
                  Add_Component_List (Component_List (Variant), From_Rec,
                                      No_Name);
                  Process_Fields_To_Add;
                  Flush_Current_Types;
               end if;

               Var_Array (J) := First_Idx;
               Set_Present_Expr (Variant,
                                 Choices_To_SO_Ref (Variant, Variant_Expr));
               J             := J + 1;
               Next_Non_Pragma (Variant);
            end loop;

            Prev_Idx  := Saved_Prev_Idx;
            Cur_Idx   := Saved_Cur_Idx;
            First_Idx := Saved_First_Idx;
            Add_RI (Variant_List => Variants (Var_Part),
                    Variants     => Var_Array,
                    Variant_Expr => Variant_Expr,
                    Align        => Variant_Align);
         end Add_Component_List;

         -----------------------
         -- Choices_To_SO_Ref --
         -----------------------

         function Choices_To_SO_Ref
           (Variant : Node_Id; Discrim : Entity_Id) return SO_Ref
         is
            Discrim_SO : constant SO_Ref
              := Annotated_Value (Emit_Expr (Discrim));
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

               Expr := (if    Expr = Uint_0 then This_Expr
                        elsif This_Expr = Uint_0 then Expr
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

               Outer_Field
                 := Find_Field_In_Entity_List (Field_To_Add, TE, Cur_Field);

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

         Add_Component_List (Components, Sub_Rec_Type, No_Name);

      end Add_Fields;

      -------------------------
      -- Flush_Current_Types --
      -------------------------

      procedure Flush_Current_Types is
      begin
         if Next_Type /= 0 then
            Add_RI (T => Build_Struct_Type (Types (0 .. Next_Type - 1)),
                    Align => RI_Align);
            RI_Align  := 0;
            Next_Type := 0;
         end if;

         Cur_RI_Pos := 0;
      end Flush_Current_Types;

      ---------------
      -- Add_Field --
      ---------------

      procedure Add_Field (E : Entity_Id) is
      begin
         Added_Field_Table.Append ((E, Added_Field_Table.Last + 1, Var_Depth,
                                    Var_Align));
      end Add_Field;

      ---------------------------
      -- Process_Fields_To_Add --
      ---------------------------

      procedure Process_Fields_To_Add is
         Last_Var_Depth : Int := 0;

      begin
         for J in 1 .. Added_Field_Table.Last loop
            declare
               AF       : constant Added_Field := Added_Field_Table.Table (J);
               E        : constant Entity_Id   := AF.F;
               Def_GT   : constant GL_Type     :=
                 Default_GL_Type (Full_Etype (E));
               Size     : constant Uint        :=
                 (if   Unknown_Esize (E) then No_Uint else Esize (E));
               Max_Sz   : constant Boolean     :=
                 Is_Unconstrained_Record (Def_GT);
               Biased   : constant Boolean     :=
                 Has_Biased_Representation (E);
               F_GT     : GL_Type              :=
                   Make_GT_Alternative (Def_GT, E,
                                        Size          => Size,
                                        Align         => No_Uint,
                                        For_Type      => False,
                                        For_Component => False,
                                        Max_Size      => Max_Sz,
                                        Is_Biased     => Biased);
               Align    : constant ULL     := Get_Type_Alignment (F_GT);

            begin
               --  If we've pushed into a new static variant, see if
               --  we need to align it.

               if AF.Var_Depth /= Last_Var_Depth then
                  Last_Var_Depth := AF.Var_Depth;
                  if Cur_RI_Pos mod AF.Var_Align /= 0 then
                     Flush_Current_Types;
                     RI_Align := AF.Var_Align;
                     Set_Is_Nonnative_Type (TE);
                  end if;
               end if;

               --  If this is the '_parent' field, we make a dummy entry
               --  and handle it specially later.

               if Chars (E) = Name_uParent then
                  Add_FI (E, Get_Record_Info_N (TE), 0, F_GT);

               --  If this field is a non-native type, we have to close out
               --  the last record info entry we're making, if there's
               --  anything in it, and make a piece for this field.

               elsif Is_Nonnative_Type (F_GT) then
                  --  ??  This is the only case where we use an F_GT that
                  --  might have been modified by Add_FI.  We need to be
                  --  sure that's OK.

                  Flush_Current_Types;
                  Add_FI (E, Cur_Idx, 0, F_GT);
                  Add_RI (F_GT => F_GT);
                  Set_Is_Nonnative_Type (TE);
                  Split_Align := Align;

               --  If it's a native type, add it to the current set of
               --  fields and make a field descriptor.

               else
                  --  We need to flush the previous types if required by
                  --  the alignment.  We assume here that if Add_FI updates
                  --  our type that it has the same alignment.

                  if Align > Split_Align then
                     Flush_Current_Types;
                     Set_Is_Nonnative_Type (TE);
                     Split_Align := Align;
                  end if;

                  Types (Next_Type) := Type_Of (F_GT);
                  Cur_RI_Pos        :=
                    Cur_RI_Pos + Get_Type_Size (Type_Of (F_GT));
                  Add_FI (E, Cur_Idx, Next_Type, F_GT);
                  Next_Type         := Next_Type + 1;
               end if;
            end;
         end loop;

         Var_Depth := 0;
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

      --  If we have a new discriminant that renames one from our parent,
      --  we need to mark which field the discriminant corresponds to.  So
      --  make a pass over the discriminants of this type seeing if any
      --  haven't had field information set.  If we find any, copy it from
      --  the orginal field.

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
               Outer_Field
                 := Find_Field_In_Entity_List (Field, TE, Cur_Field);

               if Present (Outer_Field)
                 and then No (Get_Field_Info (Outer_Field))
                 and then Scope (ORC) = Full_Base_Type (TE)
                 and then Is_Completely_Hidden (ORC)
               then
                  if Present (Outer_Field) and then Present (Outer_Orig) then
                     Set_Field_Info (Outer_Field,
                                     Get_Field_Info
                                       (Original_Record_Component
                                          (Outer_Orig)));
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
         declare
            ORC : constant Entity_Id := Original_Record_Component (Cur_Field);
            Typ : constant Entity_Id := Full_Etype (Cur_Field);

         begin
            if Full_Scope (Cur_Field) = TE
              and then (Ekind (Cur_Field) = E_Component
                          or else No (ORC)
                          or else not Is_Completely_Hidden (ORC))
            then
               declare
                  Byte_Position : constant BA_Data :=
                    Field_Position (Cur_Field, No_GL_Value);
                  Bit_Position  : constant BA_Data :=
                    Byte_Position * Const (Uint_Bits_Per_Unit);

               begin
                  if Unknown_Esize (Cur_Field) then
                     Set_Esize (Cur_Field,
                                Annotated_Object_Size (Default_GL_Type (Typ)));
                  end if;
                  if Component_Bit_Offset (Cur_Field) = No_Uint then
                     Set_Component_Bit_Offset (Cur_Field,
                                               Annotated_Value (Bit_Position));
                  end if;
                  if not Is_Const (Byte_Position)
                    and then Unknown_Normalized_Position (Cur_Field)
                  then
                     Set_Normalized_Position (Cur_Field,
                                              Annotated_Value (Byte_Position));
                     Set_Normalized_First_Bit (Cur_Field, Uint_0);
                  end if;
               end;
            end if;
         end;

         Next_Component_Or_Discriminant (Cur_Field);
      end loop;

      return LLVM_Type;
   end Create_Record_Type;

   --  These are the generic functions to compute the size of record and
   --  offsets of fields within them.

   package body Size is

      -----------------
      -- Get_RI_Info --
      -----------------

      procedure Get_RI_Info
        (RI          : Record_Info;
         V           : GL_Value;
         Max_Size    : Boolean;
         Size        : out Result;
         Must_Align  : out Result;
         Is_Align    : out Result;
         Return_Size : Boolean := True)
      is
         T         : constant Type_T  := RI.LLVM_Type;
         GT        : constant GL_Type := RI.GT;
         This_Size : Result           := Empty_Result;

      begin
         --  First check for zero length LLVM type since the code below will
         --  fail if we have no fields.

         if Present (T) and then Get_Type_Size (T) = ULL (0) then
            This_Size  := Sz_Const (ULL (0));
            Must_Align := Sz_Const (Get_Type_Alignment (T));
            Is_Align   := Sz_Const (Get_Type_Alignment (T));

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
               Num_Types   : constant unsigned :=
                 Count_Struct_Element_Types (T);
               Last_Type   : constant Type_T   :=
                 Struct_Get_Type_At_Index (T, Num_Types - 1);
               Last_Size   : constant ULL      := Get_Type_Size (Last_Type);
               Last_Offset : constant ULL      :=
                 Offset_Of_Element (Module_Data_Layout, T, Num_Types - 1);

            begin
               Must_Align := Sz_Const (Get_Type_Alignment (T));
               Is_Align   := Sz_Const (Get_Type_Alignment (Last_Type));
               This_Size  := Sz_Const (Last_Offset + Last_Size);
            end;

         --  The GNAT type case is easy

         elsif Present (GT) then
            Must_Align   := Sz_Const (Get_Type_Alignment (GT));
            Is_Align     := Must_Align;
            if Return_Size then
               This_Size := Sz_Type_Size (GT, V, Max_Size);
            end if;

         --  For a variant, we've already set the variant alignment, so use
         --  that for Must_Align.  We can have fields after the variants
         --  in the case of extension records, so we care about Is_Align.
         --  But pessimize it rather than calculate it since the saving
         --  isn't worth it in this obscure case.

         elsif RI.Variants /= null or else RI.Overlap_Variants /= null then

            --  ??? This isn't quite right.  If the variant is of zero size,
            --  the record size shouldn't be aligned to the variant alignment.
            --  This means passing the current size into the functions below.

            Must_Align := Sz_Const (RI.Align);
            Is_Align   := Sz_Const (ULL (1));
            if Return_Size then
               This_Size := (if   Max_Size then Get_Variant_Max_Size (RI)
                             else Sz_Variant_Size (RI, V));
            end if;

         --  Otherwise, this is a null entry

         else
            Must_Align := Sz_Const (ULL (1));
            Is_Align   := Sz_Const (ULL (Get_Maximum_Alignment));
            This_Size  := Sz_Const (ULL (0));
         end if;

         --  If we've set an alignment for this RI, it overrides any
         --  computation we did above.

         if RI.Align /= 0 then
            Must_Align := Sz_Const (RI.Align);
         end if;

         Size := (if Return_Size then This_Size else Empty_Result);
      end Get_RI_Info;

      ------------------------
      -- Get_Variant_For_RI --
      ------------------------

      function Get_Variant_For_RI
        (In_RI       : Record_Info;
         V           : GL_Value;
         Max_Size    : Boolean;
         Need_Idx    : Record_Info_Id;
         Use_Overlap : Boolean := False) return Record_Info_Id
      is
         Variants : constant Record_Info_Id_Array_Access :=
           (if Use_Overlap then In_RI.Overlap_Variants else In_RI.Variants);
         New_Idx  : Record_Info_Id                       :=
           Empty_Record_Info_Id;
         Idx      : Record_Info_Id;
         RI       : Record_Info;

      begin
         --  Look through each variant

         for Variant_Idx of Variants.all loop

            --  Now look through each entry in the variant, looking into nested
            --  variants if necessary.  We start looking at the first chained
            --  entry of each variant, since that's where fields of that
            --  variant start.

            Idx := Variant_Idx;
            while Present (Idx) loop
               RI := Record_Info_Table.Table (Idx);
               if Idx = Need_Idx then
                  return Variant_Idx;
               end if;

               if RI.Variants /= null then
                  New_Idx := Get_Variant_For_RI (RI, V, Max_Size,
                                                 Need_Idx, False);
               end if;
               if RI.Overlap_Variants /= null then
                  New_Idx := Get_Variant_For_RI (RI, V, Max_Size,
                                                 Need_Idx, True);
               end if;

               if Present (New_Idx) then
                  return Variant_Idx;
               end if;

               Idx := RI.Next;
            end loop;
         end loop;

         return Empty_Record_Info_Id;
      end Get_Variant_For_RI;

      ----------------------------------
      -- Get_Variant_Max_Size_Variant --
      ----------------------------------

      function Get_Variant_Max_Size (RI : Record_Info) return Result is
         Max_Const_Size : ULL    := 0;
         Max_Var_Size   : Result := Empty_Result;
         Our_Size       : Result;

      begin
         --  We need to compute the maximum size of each variant.  Most
         --  discriminant sizes are constant, so we use an algorithm
         --  that'll work best in that situation.  So we record the largest
         --  constant size and make a chain of Max operations to compute
         --  the largest non-constant.  Then we merge them.

         for J in RI.Variants'Range loop
            if Present (RI.Variants (J)) then
               Our_Size := Get_Record_Size_So_Far (Empty, No_GL_Value,
                                                   RI.Variants (J),
                                                   Empty_Record_Info_Id,
                                                   True);
               if Sz_Is_Const (Our_Size) then
                  if Sz_Const_Val (Our_Size) > Max_Const_Size then
                     Max_Const_Size := Sz_Const_Val (Our_Size);
                  end if;
               elsif No (Max_Var_Size) then
                  Max_Var_Size := Our_Size;
               else
                  Max_Var_Size := Sz_Max (Our_Size, Max_Var_Size);
               end if;
            end if;
         end loop;

         if No (Max_Var_Size) then
            return Sz_Const (Max_Const_Size);
         elsif Max_Const_Size = 0 then
            return Max_Var_Size;
         else
            return Sz_Max (Max_Var_Size, Sz_Const (Max_Const_Size));
         end if;
      end Get_Variant_Max_Size;

      ----------------------------
      -- Get_Record_Size_So_Far --
      ----------------------------

      function Get_Record_Size_So_Far
        (TE         : Entity_Id;
         V          : GL_Value;
         Start_Idx  : Record_Info_Id;
         Idx        : Record_Info_Id;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result
      is
         Total_Size   : Result         := Sz_Const (ULL (0));
         Cur_Align    : Result         :=
           Sz_Const (ULL (Get_Maximum_Alignment));
         Cur_Idx      : Record_Info_Id :=
           (if   Present (Start_Idx) then Start_Idx elsif Present (TE)
            then Get_Record_Info (TE) else Empty_Record_Info_Id);
         This_Size    : Result         := Empty_Result;
         Must_Align   : Result         := Sz_Const (ULL (1));
         Pushed_Stack : Boolean        := False;
         This_Align   : Result;
         New_Idx      : Record_Info_Id;
         RI           : Record_Info;

      begin
         Push_Debug_Freeze_Pos;

         --  If we're passed V, add it to the list that Get_Matching_Value
         --  will search if we run into a discriminant in one of the
         --  computations below.

         if Present (V) then
            Add_To_LValue_List (V);
         end if;

         --  If this is a subtype, push it onto the stack we use to search for
         --  discriminant values.

         if Present (TE) and then Ekind (TE) = E_Record_Subtype
           and then Has_Discriminants (Full_Base_Type (TE))
           and then Is_Constrained (TE)
         then
            Subtype_Stack.Append ((TE, False));
            Pushed_Stack := True;
         end if;

         --  Look at each piece of the record and find its value and alignment.
         --  Align to the needed alignment for this piece, add its size, and
         --  show what alignment we now have.

         while Present (Cur_Idx) and then Cur_Idx /= Idx loop
            New_Idx := Empty_Record_Info_Id;
            RI      := Record_Info_Table.Table (Cur_Idx);

            --  If we're reached a variant point, we have two cases.  We
            --  could be looking for a specific RI index, in which case we
            --  see which variant has that index and set it as next, or
            --  we're looking to compute the size of the record.

            if RI.Variants /= null and then Present (Idx) then
               New_Idx := Get_Variant_For_RI (RI, V, Max_Size, Idx);
               if Present (New_Idx) and then RI.Align /= 0 then
                  Total_Size := Align_To (Total_Size, Cur_Align,
                                          Sz_Const (RI.Align));
               end if;
            end if;

            if RI.Overlap_Variants /= null and then Present (Idx) then
               New_Idx    := Get_Variant_For_RI (RI, V, Max_Size, Idx);
               if Present (New_Idx) then
                  Total_Size := Sz_Const (ULL (0));
               end if;
            end if;

            if Present (New_Idx) then
               Cur_Idx := New_Idx;
            else
               Get_RI_Info (RI, V, Max_Size, This_Size,
                            Must_Align, This_Align);
               Total_Size := Align_To (Total_Size, Cur_Align, Must_Align) +
                               This_Size;

               --  The resulting alignment is the minimum of this alignment
               --  and the maximum of the current alignment and what we had
               --  to align to.

               Cur_Align := Sz_Min (This_Align,
                                    Sz_Max (Cur_Align, Must_Align));
               Cur_Idx := RI.Next;
            end if;
         end loop;

         --  At this point, either Idx is not Present, meaning we were
         --  supposed to be at the end of the type, or it is, in which case
         --  we should have hit it.  If either is the case, we have an
         --  error where we're looking for a field in the wrong type.

         pragma Assert (Cur_Idx = Idx);

         --  Now we may have to do a final alignment.  If Idx is specified,
         --  use the alignment for that field.  Otherwise, use the
         --  alignment for the type.

         if Present (Idx) then
            Get_RI_Info (Record_Info_Table.Table (Idx), No_GL_Value, False,
                         This_Size, Must_Align, This_Align,
                         Return_Size => False);
         elsif Present (TE) then
            Must_Align := Sz_Const (Get_Type_Alignment (Default_GL_Type (TE)));
         end if;

         if Pushed_Stack then
            Subtype_Stack.Decrement_Last;
         end if;

         Pop_Debug_Freeze_Pos;
         return (if   No_Padding then Total_Size
                 else Align_To (Total_Size, Cur_Align, Must_Align));
      end Get_Record_Size_So_Far;

      --------------------------
      -- Get_Record_Type_Size --
      --------------------------

      function Get_Record_Type_Size
        (TE         : Entity_Id;
         V          : GL_Value;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result is

      begin
         return Get_Record_Size_So_Far
           (TE, V, Empty_Record_Info_Id, Empty_Record_Info_Id,
            Max_Size   => Max_Size or else Is_Unchecked_Union (TE),
            No_Padding => No_Padding);
      end Get_Record_Type_Size;

      -------------------------
      -- Emit_Field_Position --
      -------------------------

      function Emit_Field_Position
        (E : Entity_Id; V : GL_Value) return Result
      is
         TE     : constant Entity_Id      := Full_Scope (E);
         R_Idx  : constant Record_Info_Id := Get_Record_Info (TE);
         F_Idx  : constant Field_Info_Id  := Get_Field_Info (E);
         FI     : Field_Info;
         Idx    : Record_Info_Id;
         RI     : Record_Info;
         Offset : Result;

      begin
         --  If there's no field information for this field, the field
         --  position is undefined.

         if No (F_Idx) then
            return Empty_Result;
         end if;

         FI     := Field_Info_Table.Table (F_Idx);
         Idx    := FI.Rec_Info_Idx;
         RI     := Record_Info_Table.Table (Idx);
         Offset := Get_Record_Size_So_Far (TE, V, R_Idx, Idx);

         --  Offset now gives the offset from the start of the record to the
         --  piece that this field is in.  If this piece has a GL_Type, then
         --  the field is the entire piece and we have the offset.  If it's an
         --  LLVM type, we need to compute the offset within that type.

         if Present (RI.GT) then
            return Offset;
         else
            declare
               Ordinal     : constant unsigned := unsigned (FI.Field_Ordinal);
               This_Offset : constant ULL      :=
                 Offset_Of_Element (Module_Data_Layout, RI.LLVM_Type, Ordinal);

            begin
               return Offset + Sz_Const (This_Offset);
            end;
         end if;
      end Emit_Field_Position;

      --------------
      -- Align_To --
      --------------

      function Align_To (V, Cur_Align, Must_Align : Result) return Result is
      begin
         --  If both alignments are constant and we can determine that we
         --  needn't do any alignment, do nothing.  Otherwise, align.

         if Sz_Is_Const (Cur_Align) and then Sz_Is_Const (Must_Align)
           and then Sz_Const_Val (Must_Align) <= Sz_Const_Val (Cur_Align)
         then
            return V;
         else
            return Sz_And (V + Must_Align - Sz_Const (ULL (1)),
                           Sz_Neg (Must_Align));
         end if;
      end Align_To;

   end Size;

   --  Here we instantiate the size routines with functions that compute
   --  the LLVM value the size and make those visible to clients.

   package LLVM_Size is
      new Size (Result          => GL_Value,
                Empty_Result    => No_GL_Value,
                Sz_Const        => Size_Const_Int,
                "+"             => "+",
                "-"             => "-",
                Sz_And          => Build_And,
                Sz_Neg          => Neg,
                Sz_I_Cmp        => I_Cmp,
                Sz_Select       => Build_Select,
                Sz_Min          => Build_Min,
                Sz_Max          => Build_Max,
                Sz_Is_Const     => Is_A_Const_Int,
                Sz_Const_Val    => Get_Const_Int_Value_ULL,
                Sz_Type_Size    => Get_Type_Size,
                Sz_Variant_Size => Get_Variant_Size);

   function Get_Record_Size_So_Far
     (TE         : Entity_Id;
      V          : GL_Value;
      Start_Idx  : Record_Info_Id;
      Idx        : Record_Info_Id;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return GL_Value
     renames LLVM_Size.Get_Record_Size_So_Far;

   function Get_Record_Type_Size
     (TE         : Entity_Id;
      V          : GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return GL_Value
     renames LLVM_Size.Get_Record_Type_Size;

   function Emit_Field_Position (E : Entity_Id; V : GL_Value) return GL_Value
     renames LLVM_Size.Emit_Field_Position;

   function Align_To (V, Cur_Align, Must_Align : GL_Value) return GL_Value
     renames LLVM_Size.Align_To;

   --  Here we instantiate the size routines with functions that compute
   --  whether a size is dynamic or not and make those visible to clients.

   package IDS_Size is
      new Size (Result          => IDS,
                Empty_Result    => No_IDS,
                Sz_Const        => Const,
                "+"             => "+",
                "-"             => "-",
                Sz_And          => Build_And,
                Sz_Neg          => Neg,
                Sz_I_Cmp        => I_Cmp,
                Sz_Select       => Build_Select,
                Sz_Min          => Build_Min,
                Sz_Max          => Build_Max,
                Sz_Is_Const     => Is_Const,
                Sz_Const_Val    => Const_Val_ULL,
                Sz_Type_Size    => Get_Type_Size,
                Sz_Variant_Size => Variant_Size);

   function Get_Record_Type_Size
     (TE         : Entity_Id;
      V          : GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return IDS
     renames IDS_Size.Get_Record_Type_Size;

   function Record_Size_So_Far
     (TE         : Entity_Id;
      V          : GL_Value;
      Start_Idx  : Record_Info_Id;
      Idx        : Record_Info_Id;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return IDS
     renames IDS_Size.Get_Record_Size_So_Far;

   --  Here we instantiate the size routines with functions that compute
   --  back-annotation trees.

   package BA_Size is
      new Size (Result          => BA_Data,
                Empty_Result    => No_BA,
                Sz_Const        => Const,
                "+"             => "+",
                "-"             => "-",
                Sz_And          => Build_And,
                Sz_Neg          => Neg,
                Sz_I_Cmp        => I_Cmp,
                Sz_Select       => Build_Select,
                Sz_Min          => Build_Min,
                Sz_Max          => Build_Max,
                Sz_Is_Const     => Is_Const,
                Sz_Const_Val    => Const_Val_ULL,
                Sz_Type_Size    => Get_Type_Size,
                Sz_Variant_Size => Variant_Size);

   function Record_Size_So_Far
     (TE         : Entity_Id;
      V          : GL_Value;
      Start_Idx  : Record_Info_Id;
      Idx        : Record_Info_Id;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return BA_Data
     renames BA_Size.Get_Record_Size_So_Far;

   function Get_Record_Type_Size
     (TE         : Entity_Id;
      V          : GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return BA_Data
     renames BA_Size.Get_Record_Type_Size;

   function Field_Position (E : Entity_Id; V : GL_Value) return BA_Data
     renames BA_Size.Emit_Field_Position;

   -----------------------
   -- Get_Field_Ordinal --
   -----------------------

   function Get_Field_Ordinal
     (F_Idx : Field_Info_Id; TE : Entity_Id) return unsigned
   is
      FI     : constant Field_Info := Field_Info_Table.Table (F_Idx);

   begin
      pragma Assert (FI.Rec_Info_Idx = Get_Record_Info (TE));
      return unsigned (FI.Field_Ordinal);
   end Get_Field_Ordinal;

   --------------------
   -- Get_Field_Type --
   --------------------

   function Get_Field_Type
     (F_Idx : Field_Info_Id; TE : Entity_Id) return GL_Type
   is
      FI     : constant Field_Info := Field_Info_Table.Table (F_Idx);

   begin
      pragma Assert (FI.Rec_Info_Idx = Get_Record_Info (TE));
      return FI.GT;
   end Get_Field_Type;

   ----------------------
   -- Get_Variant_Size --
   ----------------------

   function Get_Variant_Size (RI : Record_Info; V : GL_Value) return GL_Value
   is
      Our_BB      : constant Basic_Block_T             := Get_Insert_Block;
      End_BB      : constant Basic_Block_T             := Create_Basic_Block;
      Sizes       : GL_Value_Array (RI.Variants'Range) :=
        (others => No_GL_Value);
      To_BBs      : Basic_Block_Array (RI.Variants'Range);
      From_BBs    : Basic_Block_Array (RI.Variants'Range);

   begin
      --  We first go through each variant and compute the alignments and
      --  sizes of each.  We store the GL_Value's where we've computed
      --  those things along with the starting (for branching into the code)
      --  and ending (for use with Phi) basic blocks for each.

      for J in RI.Variants'Range loop
         Disable_LV_Append := Disable_LV_Append + 1;
         To_BBs (J) := Create_Basic_Block;
         Position_Builder_At_End (To_BBs (J));

         --  If this variant is empty, trivially get the values.  Otherwise,
         --  compute each, computing the size only if needed.

         if No (RI.Variants (J)) then
            Sizes (J) := Size_Const_Null;
         else
            Sizes (J) := Get_Record_Size_So_Far (Empty, V, RI.Variants (J),
                                                 Empty_Record_Info_Id, False);
         end if;

         From_BBs (J) := Get_Insert_Block;
         Build_Br (End_BB);
         Disable_LV_Append := Disable_LV_Append - 1;
      end loop;

      --  Now emit the code to branch to the fragments we made above

      Position_Builder_At_End (Our_BB);
      Emit_Case_Code (RI.Variant_List, Emit_Expression (RI.Variant_Expr),
                      To_BBs);

      --  Now make the Phi that holds the size and return it

      Position_Builder_At_End (End_BB);
      return Build_Phi (Sizes, From_BBs);
   end Get_Variant_Size;

   ------------------
   -- Variant_Size --
   ------------------

   function Variant_Size (RI : Record_Info; V : GL_Value) return IDS is
      Size     : IDS := No_IDS;
      Our_Size : IDS;

   begin
      --  We first go through each variant and compute the size of each,
      --  looking only at constant values.  If a size differs from a
      --  previous size, it's not a constant.

      for J in RI.Variants'Range loop
         --  If this variant is empty, trivially get the size.  Otherwise,
         --  compute it.

         if No (RI.Variants (J)) then
            Our_Size       := Const (ULL (0));
         else
            Our_Size := Record_Size_So_Far (Empty, V, RI.Variants (J),
                                            Empty_Record_Info_Id, False);
         end if;

         if No (Size) or else Size /= Our_Size then
            Size := Var_IDS;
         end if;
      end loop;

      --  Now handle case where there were no variants.

      if No (Size) then
         Size := Const (ULL (0));
      end if;

      return Size;
   end Variant_Size;

   ------------------
   -- Variant_Size --
   ------------------

   function Variant_Size (RI : Record_Info; V : GL_Value) return BA_Data is
      function Get_Variant_Expr
        (RI : Record_Info; In_Values : BA_Data_Array) return BA_Data
        with Pre => List_Length_Non_Pragma (RI.Variant_List) =
                    In_Values'Length;
      --  Given an RI and a set of values, corresponding to the values
      --  in the order of the variants in the RI, return a BA_Data that
      --  represents an expression to compute which value is correct
      --  for a specific record object.

      Sizes       : BA_Data_Array (RI.Variants'Range) := (others => No_BA);

      ----------------------
      -- Get_Variant_Expr --
      ----------------------

      function Get_Variant_Expr
        (RI : Record_Info; In_Values : BA_Data_Array) return BA_Data
      is
         Values : BA_Data_Array := In_Values;
         Expr   : BA_Data       := Values (Values'Last);

      begin
         --  We're building an expression of the form
         --
         --  (if C1 then V1 elsif C2 then V2 ... else VO)
         --
         --  where VO is the "others" alternative.  We rely on the fact that,
         --  especially for alignments, that many of the values above are
         --  the same and "strike out" those values that are the same as
         --  ones we've already processed.  We rely on the others alternative
         --  being last.
         --
         --  The initial expression is VO, so we start by removing any
         --  values equal to it.

         for J in Values'Range loop
            if Values (J) = Expr then
               Values (J) := No_BA;
            end if;
         end loop;

         --  Now iterate over the list of values until we've removed all of
         --  them.  This is a quadratic algorithm, but even in records with
         --  large numbers of variants, the number of different sizes and
         --  alignments should be small, so that shouldn't be an issue.

         while (for some V of Values => Present (V)) loop
            declare
               Variant    : Node_Id := First_Non_Pragma (RI.Variant_List);
               This_Cond  : BA_Data := Const (0);
               This_Value : BA_Data;

            begin
               --  Search backwards for the last value

               for J in reverse Values'Range loop
                  This_Value := Values (J);
                  exit when Present (This_Value);
               end loop;

               --  Now build an OR of all the possibilities for this value.
               --  We know this can't include the "others" choice because we've
               --  removed that one above.

               for J in Values'Range loop
                  if Values (J) = This_Value then
                     pragma Assert (Present_Expr (Variant) /= Uint_1);
                     Values (J) := No_BA;
                     This_Cond :=
                       Truth_Or (This_Cond,
                                 SO_Ref_To_BA (Present_Expr (Variant)));
                  end if;

                  Next_Non_Pragma (Variant);
               end loop;

               --  Finally, make the conditional expression

               Expr := Build_Select (This_Cond, This_Value, Expr);
            end;
         end loop;

         return Expr;
      end Get_Variant_Expr;

   begin
      --  We first go through each variant and compute the size of each

      for J in RI.Variants'Range loop

         --  If this variant is empty, trivially get the size.  Otherwise,
         --  compute it.

         if No (RI.Variants (J)) then
            Sizes (J) := Const (0);
         else
            Sizes (J) := Record_Size_So_Far (Empty, V, RI.Variants (J),
                                             Empty_Record_Info_Id, False);
         end if;
      end loop;

      --  Now compute the resulting size

      return Get_Variant_Expr (RI, Sizes);
   end Variant_Size;

   -------------------------
   -- Record_Field_Offset --
   -------------------------

   function Record_Field_Offset
     (V : GL_Value; Field : Entity_Id) return GL_Value
   is
      F_GT       : GL_Type                 := Full_GL_Type (Field);
      CRC        : constant Entity_Id      :=
        Corresponding_Record_Component (Field);
      Our_Field  : constant Entity_Id      :=
        (if   No (Get_Field_Info (Field)) and then Present (CRC)
              and then Full_Etype (CRC) = Full_Etype (F_GT)
         then CRC else Field);
      Rec_Type   : constant Entity_Id      := Full_Scope (Our_Field);
      Rec_GT     : constant GL_Type        := Primitive_GL_Type (Rec_Type);
      First_Idx  : constant Record_Info_Id := Get_Record_Info (Rec_Type);
      F_Idx      : Field_Info_Id           := Get_Field_Info (Our_Field);
      FI         : Field_Info;
      Our_Idx    : Record_Info_Id;
      Offset     : GL_Value;
      RI         : Record_Info;
      Result     : GL_Value;

   begin
      --  If the field information isn't present, this must be because we're
      --  referencing a field that's not in this variant and hence is a
      --  constraint error.  So return undefined.  ??? But first try something
      --  to see if we can come up with the right field.

      if No (F_Idx) then
         pragma Assert (Has_Discriminants (Rec_Type));

         if Rec_Type /= Scope (Field) then
            F_Idx := Get_Field_Info (Find_Matching_Field (Rec_Type, Field));
         end if;

         if No (F_Idx) then
            return Get_Undef_Ref (F_GT);
         end if;
      end if;

      FI       := Field_Info_Table.Table (F_Idx);
      F_GT     := FI.GT;
      Our_Idx  := FI.Rec_Info_Idx;
      Offset   := Get_Record_Size_So_Far (Rec_Type, V, First_Idx, Our_Idx);
      RI       := Record_Info_Table.Table (Our_Idx);

      --  If this is the "_parent" field, just do a conversion so we point
      --  to that type.  But add it to the LValue table in case there's
      --  a reference to its discrminant.

      if Chars (Our_Field) = Name_uParent then
         Result := Ptr_To_Ref (V, F_GT);
         Add_To_LValue_List (Result);
         return Result;

      --  If the current piece is for a variable-sized object, we offset
      --  to that object and make a pointer to its type.  Otherwise,
      --  make sure we're pointing to Rec_Type.

      elsif Present (RI.GT) then
         return Ptr_To_Ref (GEP (SSI_GL_Type, Pointer_Cast (V, A_Char_GL_Type),
                                 (1 => Offset)),
                            F_GT);
      end if;

      --  Otherwise, if this is not the first piece, we have to offset to
      --  the field (in bytes).

      if Our_Idx = First_Idx then
         Result := To_Primitive (V);
      else
         Result := GEP (SSI_GL_Type,
                        Pointer_Cast (To_Primitive (V), A_Char_GL_Type),
                        (1 => Offset));
      end if;

      --  If the type is not native, we have to convert the pointer to the
      --  type of this piece (which has no corresponding GNAT type).

      if Is_Nonnative_Type (Rec_Type) then
         Result := G_Ref (Pointer_Cast (IR_Builder, LLVM_Value (Result),
                                        Pointer_Type (RI.LLVM_Type, 0), ""),
                          Rec_GT);
      else
         Result := Convert_Ref (Result, Rec_GT);
      end if;

      --  Finally, do a regular GEP for the field and we're done

      return GEP (F_GT, Result,
                  (1 => Const_Null_32,
                   2 => Const_Int_32 (unsigned (FI.Field_Ordinal))));

   end Record_Field_Offset;

   --------------------------------
   -- Get_Record_Size_Complexity --
   --------------------------------

   function Get_Record_Size_Complexity
     (TE : Entity_Id; Max_Size : Boolean := False) return Nat
   is
      Cur_Idx    : Record_Info_Id := Get_Record_Info (TE);
      RI         : Record_Info;

   begin
      return Complexity : Nat := 0 do
         while Present (Cur_Idx) loop
            RI := Record_Info_Table.Table (Cur_Idx);
            if Present (RI.GT) then
               Complexity := Complexity +
                 Get_Type_Size_Complexity (RI.GT, Max_Size);
            end if;

            Cur_Idx := RI.Next;
         end loop;
      end return;
   end Get_Record_Size_Complexity;

   -----------------------------------
   -- Contains_Unconstrained_Record --
   -----------------------------------

   function Contains_Unconstrained_Record (GT : GL_Type) return Boolean is
      F : Entity_Id := First_Component_Or_Discriminant (GT);

   begin
      while Present (F) loop
         exit when Is_Unconstrained_Record (Full_Etype (F));
         Next_Component_Or_Discriminant (F);
      end loop;

      return Present (F);
   end Contains_Unconstrained_Record;

   ---------------------------
   -- Emit_Record_Aggregate --
   ---------------------------

   function Emit_Record_Aggregate
     (N : Node_Id; Result_So_Far : GL_Value) return GL_Value
   is
      GT       : constant GL_Type := Primitive_GL_Type (Full_GL_Type (N));
      Expr     : Node_Id;

   begin
      --  If we can use Data for the result, it means that each of its
      --  components must be just a simple component into an LLVM
      --  structure, so we just go through each of the part of the
      --  aggregate and use the offset for that field, skipping a
      --  discriminant of an unchecked union.  If not, we use
      --  Record_Field_Offset to do the reference.

      Expr := First (Component_Associations (N));
      return Result : GL_Value := Result_So_Far do

         --  If we haven't already made a value, do so now.  If this is a
         --  loadable type or not of dynamic size and we have a value, we
         --  start with an undef of that type.  Otherwise, it's a variable
         --  of that type.

         if No (Result) then
            if (Is_Loadable_Type (GT)
                  or else (not Is_Dynamic_Size (GT)
                             and then Is_No_Elab_Needed (N)))
              and then not Contains_Unconstrained_Record (GT)
              and then not Is_Nonnative_Type (GT)
            then
               Result := Get_Undef (GT);
            else
               Result := Allocate_For_Type (GT, GT, N);
            end if;
         end if;

         --  Now process each expression

         while Present (Expr) loop
            declare
               F     : constant Entity_Id     :=
                 Find_Matching_Field
                 (Full_Etype (GT), Entity (First (Choices (Expr))));
               F_Idx : constant Field_Info_Id := Get_Field_Info (F);

            begin
               if Ekind (F) = E_Discriminant and then Is_Unchecked_Union (GT)
               then
                  null;
               elsif Chars (F) = Name_uParent then

                  --  If this is "_parent", its fields are our fields too.
                  --  Assume Expression is also an N_Aggregate.

                  pragma Assert (Nkind_In (Expression (Expr),
                                           N_Aggregate,
                                           N_Extension_Aggregate));

                  Result := Emit_Record_Aggregate (Expression (Expr), Result);
               else
                  --  We are to actually insert the field.  However, if we
                  --  haven't set any information for this field, it may be
                  --  a reference to a field that will cause Constraint_Error.
                  --  If so, just don't do anything with it.

                  if Present (F_Idx) then
                     declare
                        FI    : constant Field_Info :=
                          Field_Info_Table.Table (F_Idx);
                        F_GT  : constant GL_Type    := FI.GT;
                        Idx : constant Nat          := FI.Field_Ordinal;
                        Val : constant GL_Value     :=
                          Emit_Convert_Value (Expression (Expr), F_GT);

                     begin
                        if Is_Data (Result) then
                           Result :=
                             Insert_Value (Result, Val, unsigned (Idx));
                        else
                           Emit_Assignment (Record_Field_Offset (Result, F),
                                            Empty, Val);
                        end if;
                     end;
                  else
                     --  Ensure we understand this case

                     pragma Assert (Ekind (GT) = E_Record_Subtype
                                      and then Has_Discriminants (GT)
                                      and then (Ekind (F) = E_Component));
                  end if;
               end if;
            end;

            Next (Expr);
         end loop;
      end return;
   end Emit_Record_Aggregate;

   pragma Annotate (Xcov, Exempt_On, "Debug helpers");

   procedure Print_RI_Briefly (Ridx : Record_Info_Id);

   ----------------------
   -- Print_RI_Briefly --
   ----------------------

   procedure Print_RI_Briefly (Ridx : Record_Info_Id) is
      RI : constant Record_Info := Record_Info_Table.Table (Ridx);

   begin
      if Present (RI.GT) then
         Dump_GL_Type (RI.GT);
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
         Dump_GL_Type (FI.GT);
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
            if Present (RI.Next) then
               Write_Str (" => ");
               Write_Int (Nat (RI.Next));
            end if;

            if RI.Align /= 0 then
               Write_Str (" align ");
               Write_Int (Nat (RI.Align));
            end if;

            Write_Eol;
            if Present (RI.GT) then
               Write_Str (Prefix);
               Dump_GL_Type (RI.GT);
            elsif Present (RI.LLVM_Type) then
               Dump_LLVM_Type (RI.LLVM_Type);
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

                  Write_Line (" =>");
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
