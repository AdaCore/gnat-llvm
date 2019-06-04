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

with Elists;     use Elists;
with Get_Targ;   use Get_Targ;
with Nlists;     use Nlists;
with Output;     use Output;
with Repinfo;    use Repinfo;
with Snames;     use Snames;
with Sprint;     use Sprint;
with Uintp.LLVM; use Uintp.LLVM;

with LLVM.Core;  use LLVM.Core;

with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Conditionals; use GNATLLVM.Conditionals;
with GNATLLVM.Conversions;  use GNATLLVM.Conversions;
with GNATLLVM.DebugInfo;    use GNATLLVM.DebugInfo;
with GNATLLVM.Exprs;        use GNATLLVM.Exprs;
with GNATLLVM.Subprograms;  use GNATLLVM.Subprograms;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Variables;    use GNATLLVM.Variables;

package body GNATLLVM.Records is

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

   type Write_Back is record
      LHS : GL_Value;
      F   : Entity_Id;
      RHS : GL_Value;
   end record;

   package Writeback_Stack is new Table.Table
     (Table_Component_Type => Write_Back,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 2,
      Table_Increment      => 1,
      Table_Name           => "Writeback_Stack");

   procedure Add_Write_Back (LHS : GL_Value; F : Entity_Id; RHS : GL_Value)
     with  Pre  => Is_Record_Type (Related_Type (LHS))
                   and then Present (RHS)
                   and then Ekind_In (F, E_Component, E_Discriminant);
   --  Like Build_Field_Store, but stack the operation to be performed
   --  later.  The operations are performed LIFO.

   function Get_Variant_Size
     (RI         : Record_Info;
      V          : GL_Value;
      In_Size    : GL_Value;
      Cur_Align  : Nat;
      No_Padding : Boolean := False) return GL_Value
     with Pre  => RI.Variants /= null and then RI.Overlap_Variants /= null
                  and then Present (In_Size),
          Post => Present (Get_Variant_Size'Result);
   --  Compute the total size of a record with the information from a fragment
   --  known to be a variant and where we're not getting the maximum size.

   function Get_Variant_Size
     (RI         : Record_Info;
      V          : GL_Value;
      In_Size    : IDS;
      Cur_Align  : Nat;
      No_Padding : Boolean := False) return IDS
     with Pre  => RI.Variants /= null and then RI.Overlap_Variants /= null
                  and then Present (In_Size),
          Post => Present (Get_Variant_Size'Result);
   --  Version of above for Is_Dynamic_Size

   function Get_Variant_Size
     (RI         : Record_Info;
      V          : GL_Value;
      In_Size    : BA_Data;
      Cur_Align  : Nat;
      No_Padding : Boolean := False) return BA_Data
     with Pre => RI.Variants /= null and then RI.Overlap_Variants /= null;
   --  Version of above for back-annotation

   function Variant_Part_Size
     (RI         : Record_Info;
      V          : GL_Value;
      J          : Int;
      In_Size    : GL_Value;
      Cur_Align  : Nat;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return GL_Value
     with Pre  => RI.Variants /= null and then RI.Overlap_Variants /= null
                  and then J in RI.Variants'Range and then Present (In_Size),
          Post => Present (Variant_Part_Size'Result);
   --  Computes the contribution to Total_Size of the variant part with index
   --  J in RI.  In_Size and Cur_Align are the total size not considering the
   --  variant and the known alignment at that point.

   function Variant_Part_Size
     (RI         : Record_Info;
      V          : GL_Value;
      J          : Int;
      In_Size    : IDS;
      Cur_Align  : Nat;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return IDS
     with Pre  => RI.Variants /= null and then RI.Overlap_Variants /= null
                  and then J in RI.Variants'Range and then Present (In_Size),
          Post => Present (Variant_Part_Size'Result);
   --  Version of above for computing if something is dynamic size

   function Variant_Part_Size
     (RI         : Record_Info;
      V          : GL_Value;
      J          : Int;
      In_Size    : BA_Data;
      Cur_Align  : Nat;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return BA_Data
     with Pre => RI.Variants /= null and then RI.Overlap_Variants /= null
                 and then J in RI.Variants'Range;
   --  Version of above for back-annotation

   function Align_To
     (V : IDS; Cur_Align, Must_Align : Nat) return IDS
     with Pre => Present (V), Post => Present (Align_To'Result);
   --  Version for computing whether something is dynamic size

   function Align_To
     (V : BA_Data; Cur_Align, Must_Align : Nat) return BA_Data
     with Pre => Present (V), Post => Present (Align_To'Result);
   --  Version for computing back-annotation

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
        (RI         : Record_Info;
         V          : GL_Value;
         In_Size    : Result;
         Cur_Align  : Nat;
         No_Padding : Boolean := False) return Result;
      with function  Sz_I_Cmp
        (Op : Int_Predicate_T;
         LHS : Result;
         RHS : Result;
         Name : String := "") return Result;
      with function "+" (V1, V2 : Result) return Result;
      with function "-" (V1, V2 : Result) return Result;
      with function Sz_Neg (V : Result; Name : String := "") return Result;
      with function Sz_And
        (V1, V2 : Result; Name : String := "") return Result;
      with function Sz_Select
        (V_If, V_Then, V_Else : Result; Name : String := "") return Result;
      with function Sz_Min
        (V1, V2 : Result; Name : String := "") return Result;
      with function Sz_Max
        (V1, V2 : Result; Name : String := "") return Result;
      with function Sz_Is_Const (V : Result) return Boolean;
      with function Sz_Const_Val (V : Result) return ULL;
      with function Sz_Replace_Val (O, N : Result) return Result;
   package Size is

      function No      (V : Result) return Boolean is (V =  Empty_Result);
      function Present (V : Result) return Boolean is (V /= Empty_Result);

      procedure Get_RI_Info
        (RI          : Record_Info;
         V           : GL_Value;
         Max_Size    : Boolean;
         Cur_Align   : Nat;
         Total_Size  : in out Result;
         Must_Align  : out Nat;
         Is_Align    : out Nat;
         Return_Size : Boolean := True;
         No_Padding  : Boolean := False);
      --  Return information about a record fragment RI.  This includes the
      --  amount to which this fragment must be aligned and the amout to
      --  which the resulting size is known to be aligned.  Also update the
      --  total size with the size of the fragment. If the size isn't
      --  wanted, don't compute it.  Cur_Align is the known alignment of
      --  the size so far.

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

      function Get_Variant_Max_Size
        (RI         : Record_Info;
         In_Size    : Result;
         Cur_Align  : Nat;
         No_Padding : Boolean := False) return Result
        with Pre  => (RI.Variants /= null or else RI.Overlap_Variants /= null)
                     and then Present (In_Size),
             Post => Present (Get_Variant_Max_Size'Result);
      --  Get informaton corresponding to the maxium size of the variant
      --  described by RI, In_Size, and Cur_Align.

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

      function Align_To
        (V : Result; Cur_Align, Must_Align : Nat) return Result;
      --  V is a value aligned to Cur_Align.  Ensure that it's aligned to
      --  Align_To.

      function Variant_Part_Size
        (RI         : Record_Info;
         V          : GL_Value;
         J          : Int;
         In_Size    : Result;
         Cur_Align  : Nat;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result
        with Pre => RI.Variants /= null and then RI.Overlap_Variants /= null
             and then J in RI.Variants'Range;
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

      --  Otherwise, use a value that we pushed onto the LValue stack

      return Get (Build_Field_Load (Get_Matching_Value (Rec_Type), E), Data);

   end Use_Discriminant_For_Bound;

   -------------------------
   -- Find_Matching_Field --
   -------------------------

   function Find_Matching_Field
     (TE : Entity_Id; Field : Entity_Id) return Entity_Id
   is
      Ent : Entity_Id := First_Component_Or_Discriminant (TE);

   begin
      while Present (Ent) loop
         exit when Chars (Ent) = Chars (Field)
           and then Present (Get_Field_Info (Ent));
         Next_Component_Or_Discriminant (Ent);
      end loop;

      --  If this didn't work and the Original_Record_Component isn't the
      --  same record, try it and its type.

      if No (Ent) and then Original_Record_Component (Field) /= Field then
         Ent := Original_Record_Component (Field);
         return Find_Matching_Field (Full_Scope (Ent), Ent);
      end if;

      return Ent;
   end Find_Matching_Field;

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
         Cur_Align   : Nat;
         Total_Size  : in out Result;
         Must_Align  : out Nat;
         Is_Align    : out Nat;
         Return_Size : Boolean := True;
         No_Padding  : Boolean := False)
      is
         T         : constant Type_T  := RI.LLVM_Type;
         GT        : constant GL_Type := RI.GT;
         This_Size : Result           := Empty_Result;

      begin
         --  If this piece has a starting position specified, move to it

         if RI.Position /= 0 then
            Total_Size := Sz_Replace_Val (Total_Size, Sz_Const (RI.Position));
         end if;

         --  Then check for zero length LLVM type since the code below will
         --  fail if we have no fields.

         if Present (T) and then Get_Type_Size (T) = ULL (0) then
            This_Size  := Sz_Const (0);
            Must_Align := Get_Type_Alignment (T);
            Is_Align   := Get_Type_Alignment (T);

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
               Must_Align := Get_Type_Alignment (T);
               Is_Align   := Get_Type_Alignment (Last_Type);
               This_Size  := Sz_Const (Last_Offset + Last_Size);
            end;

         --  The GNAT type case is easy

         elsif Present (GT) then
            Must_Align   := Get_Type_Alignment (GT);
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

            --  If we're looking for the maximum size, do our normal
            --  processing of aligning and adding the size.  But if looking
            --  for the actual size, pass in the total size and let it be
            --  updated.  That avoids aligning if the variant part chosen
            --  has no fields.  So we return in that case.

            Must_Align := RI.Align;
            Is_Align   := 1;
            if Return_Size then
               Total_Size :=
                 (if   Max_Size
                  then Get_Variant_Max_Size (RI, Total_Size, Cur_Align,
                                             No_Padding => No_Padding)
                  else Sz_Variant_Size (RI, V, Total_Size, Cur_Align,
                                        No_Padding => No_Padding));
               return;
            end if;

         --  Otherwise, this is a null entry

         else
            Must_Align := 1;
            Is_Align   := Get_Maximum_Alignment;
            This_Size  := Sz_Const (0);
         end if;

         --  If we've set an alignment for this RI, it overrides any
         --  computation we did above and setting a position for this
         --  RI also overrides the alignment.

         if RI.Align /= 0 then
            Must_Align := RI.Align;
         end if;

         --  Now update the total size given what we've computed above
         if Return_Size then
            Total_Size
              := Align_To (Total_Size, Cur_Align, Must_Align) + This_Size;
         end if;
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
                  if No (New_Idx) then
                     New_Idx := Get_Variant_For_RI (RI, V, Max_Size,
                                                    Need_Idx, True);
                  end if;

                  if Present (New_Idx) then
                     return Variant_Idx;
                  end if;
               end if;

               Idx := RI.Next;
            end loop;
         end loop;

         return Empty_Record_Info_Id;
      end Get_Variant_For_RI;

      --------------------------
      -- Get_Variant_Max_Size --
      --------------------------

      function Get_Variant_Max_Size
        (RI         : Record_Info;
         In_Size    : Result;
         Cur_Align  : Nat;
         No_Padding : Boolean := False) return Result
      is
         Max_Const_Size : ULL    := 0;
         Max_Var_Size   : Result := Empty_Result;
         Our_Size       : Result;

      begin
         --  We need to compute the maximum size of each variant.  Most
         --  discriminant sizes are constant, so we use an algorithm
         --  that'll work best in that situation. First record the largest
         --  constant size and make a chain of Max operations to compute
         --  the largest non-constant.

         for J in RI.Variants'Range loop
            Our_Size := Variant_Part_Size (RI, No_GL_Value, J, In_Size,
                                           Cur_Align,
                                           Max_Size   => True,
                                           No_Padding => No_Padding);
            if Sz_Is_Const (Our_Size) then
               if Sz_Const_Val (Our_Size) > Max_Const_Size then
                  Max_Const_Size := Sz_Const_Val (Our_Size);
               end if;
            elsif No (Max_Var_Size) then
               Max_Var_Size := Our_Size;
            else
               Max_Var_Size := Sz_Max (Our_Size, Max_Var_Size);
            end if;
         end loop;

         --  Now merge the variable and constant sizes

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
         Total_Size   : Result         := Sz_Const (0);
         Cur_Align    : Nat            := Get_Maximum_Alignment;
         Cur_Idx      : Record_Info_Id :=
           (if   Present (Start_Idx) then Start_Idx elsif Present (TE)
            then Get_Record_Info (TE) else Empty_Record_Info_Id);
         Must_Align   : Nat            := 1;
         Pushed_Stack : Boolean        := False;
         This_Align   : Nat;
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
               New_Idx := Get_Variant_For_RI (RI, V, Max_Size, Idx, False);
               if Present (New_Idx) and then RI.Align /= 0 then
                  Total_Size := Align_To (Total_Size, Cur_Align, RI.Align);
               end if;

               if No (New_Idx) then
                  New_Idx := Get_Variant_For_RI (RI, V, Max_Size, Idx, True);
                  if Present (New_Idx) then
                     Total_Size := Sz_Const (0);
                  end if;
               end if;
            end if;

            if Present (New_Idx) then
               Cur_Idx := New_Idx;
            else
               Get_RI_Info (RI, V, Max_Size, Cur_Align, Total_Size,
                            Must_Align, This_Align, No_Padding => No_Padding);

               --  The resulting alignment is the minimum of this alignment
               --  and the maximum of the current alignment and what we had
               --  to align to.

               Cur_Align := Nat'Min (This_Align,
                                     Nat'Max (Cur_Align, Must_Align));
               Cur_Idx   := RI.Next;
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
                         Cur_Align, Total_Size, Must_Align, This_Align,
                         Return_Size => False, No_Padding => No_Padding);
         elsif Present (TE) then
            Must_Align := Get_Type_Alignment (Default_GL_Type (TE));
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

      function Align_To
        (V : Result; Cur_Align, Must_Align : Nat) return Result is
      begin
         --  If we can determine that we needn't do any alignment, do
         --  nothing.  Otherwise, align.

         if Must_Align <= Cur_Align then
            return V;
         else
            return Sz_And (V + Sz_Const (ULL (Must_Align - 1)),
                           Sz_Neg (Sz_Const (ULL (Must_Align))));
         end if;
      end Align_To;

      -----------------------
      -- Variant_Part_Size --
      -----------------------

      function Variant_Part_Size
        (RI         : Record_Info;
         V          : GL_Value;
         J          : Int;
         In_Size    : Result;
         Cur_Align  : Nat;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result
      is
         Align        : constant Nat    := RI.Align;
         Variant_Size : constant Result :=
           Get_Record_Size_So_Far (Empty, V, RI.Variants (J),
                                   Empty_Record_Info_Id,
                                   Max_Size   => Max_Size,
                                   No_Padding => No_Padding);
         Overlap_Size : constant Result :=
           Get_Record_Size_So_Far (Empty, V, RI.Overlap_Variants (J),
                                   Empty_Record_Info_Id, False,
                                   No_Padding => No_Padding);
         Our_Size     : Result          := In_Size;

      begin
         --  If this variant isn't of zero size, align the input and add
         --  its size.

         if Variant_Size /= Sz_Const (0) then
            Our_Size := Align_To (Our_Size, Cur_Align, Align) + Variant_Size;
         end if;

         --  The resulting size is the maximum of that and the size of the
         --  overlap portion (but special-case zero).

         if Our_Size = Sz_Const (0) then
            return Overlap_Size;
         elsif Overlap_Size = Sz_Const (0) then
            return Our_Size;
         else
            return Sz_Max (Our_Size, Overlap_Size);
         end if;
      end Variant_Part_Size;

   end Size;

   --  Sz_Replace_Val is used to replace a size value with a new value.
   --  For both the normal and back-annotation cases, this is the second
   --  operand.  However, for the purpose of determining if this is of
   --  dynamic size, we need to conside the size variable if either the old
   --  or the new value is variable to avoid trying to compute something
   --  dynamic at the global level.  We could probably avoid this computation
   --  if worth it, but the case here is very unusual (involving partially-
   --  repped extension records), so it's not worth the trouble.

   function Replace_Val (Unused_O, N : GL_Value) return GL_Value is
      (N);

   function Replace_Val (Unused_O, N : BA_Data) return BA_Data is
      (N);

   function Replace_Val (O, N : IDS) return IDS is
      (if Is_Const (O) then N else Var_IDS);

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
                Sz_Variant_Size => Get_Variant_Size,
                Sz_Replace_Val  => Replace_Val);

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

   function Align_To
     (V : GL_Value; Cur_Align, Must_Align : Nat) return GL_Value
     renames LLVM_Size.Align_To;

   function Variant_Part_Size
     (RI         : Record_Info;
      V          : GL_Value;
      J          : Int;
      In_Size    : GL_Value;
      Cur_Align  : Nat;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return GL_Value
     renames LLVM_Size.Variant_Part_Size;

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
                Sz_Variant_Size => Get_Variant_Size,
                Sz_Replace_Val  => Replace_Val);

   function Get_Record_Type_Size
     (TE         : Entity_Id;
      V          : GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return IDS
     renames IDS_Size.Get_Record_Type_Size;

   function Get_Record_Size_So_Far
     (TE         : Entity_Id;
      V          : GL_Value;
      Start_Idx  : Record_Info_Id;
      Idx        : Record_Info_Id;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return IDS
     renames IDS_Size.Get_Record_Size_So_Far;

   function Align_To (V : IDS; Cur_Align, Must_Align : Nat) return IDS
     renames IDS_Size.Align_To;

   function Variant_Part_Size
     (RI         : Record_Info;
      V          : GL_Value;
      J          : Int;
      In_Size    : IDS;
      Cur_Align  : Nat;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return IDS
     renames IDS_Size.Variant_Part_Size;

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
                Sz_Variant_Size => Get_Variant_Size,
                Sz_Replace_Val  => Replace_Val);

   function Get_Record_Type_Size
     (TE         : Entity_Id;
      V          : GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return BA_Data
     renames BA_Size.Get_Record_Type_Size;

   function Field_Position (E : Entity_Id; V : GL_Value) return BA_Data
     renames BA_Size.Emit_Field_Position;

   function Align_To (V : BA_Data; Cur_Align, Must_Align : Nat) return BA_Data
     renames BA_Size.Align_To;

   function Variant_Part_Size
     (RI         : Record_Info;
      V          : GL_Value;
      J          : Int;
      In_Size    : BA_Data;
      Cur_Align  : Nat;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return BA_Data
     renames BA_Size.Variant_Part_Size;

   -------------------------------
   -- Effective_Field_Alignment --
   -------------------------------

   function Effective_Field_Alignment (F : Entity_Id) return Pos is
      Align : constant Nat       := Get_Type_Alignment (Full_GL_Type (F));
      Pos   : constant Uint      := Component_Bit_Offset (F);
      Size  : constant Uint      := Esize (F);
      TE    : constant Entity_Id := Full_Scope (F);

   begin
      --  If the record is packed and this field isn't aliased, its alignment
      --  doesn't contribute to the alignment.

      if Is_Packed (TE) and then not Is_Aliased (F) then
         return 1;

      --  If there's no component clause or the position and alignment
      --  of the clause are consistent with the alignment, use it.

      elsif No (Component_Clause (F))
        or else (Pos mod (Align * BPU) = 0 and then Size mod (Align * BPU) = 0)
      then
         return Align;

      --  Otherwise, this field doesn't contribute to the alignment

      else
         return 1;
      end if;

   end Effective_Field_Alignment;

   -------------------------------
   -- Get_Record_Type_Alignment --
   -------------------------------

   function Get_Record_Type_Alignment (TE : Entity_Id) return Nat is
      Field : Entity_Id;

   begin
      --  Use the largest effective alignment of any field

      return Largest_Align : Nat := 1 do
         Field := First_Component_Or_Discriminant (TE);
         while Present (Field) loop
            Largest_Align := Nat'Max (Largest_Align,
                                      Effective_Field_Alignment (Field));

            Next_Component_Or_Discriminant (Field);
         end loop;
      end return;
   end Get_Record_Type_Alignment;

   -------------------
   -- Field_Ordinal --
   -------------------

   function Field_Ordinal (F : Entity_Id) return unsigned is
     (unsigned (Field_Info_Table.Table (Get_Field_Info (F)).Field_Ordinal));

   --------------------
   -- Get_Field_Type --
   --------------------

   function Get_Field_Type (F : Entity_Id) return GL_Type is
     (Field_Info_Table.Table (Get_Field_Info (F)).GT);

   function Is_Packable_Field (F : Entity_Id) return Boolean is
      GT : constant GL_Type := Full_GL_Type (Original_Record_Component (F));
      --  We have to use the data from the base type of the record to be sure
      --  that we lay out a record and its subtype the same way.

      T  : constant Type_T  := Type_Of (GT);
      pragma Unreferenced (T);
      --  We need to be sure that the type of the record is elaborated

   begin
      --  If we have a rep clause, we'll use that rather than packing it.
      --  If the record isn't packed, neither is the field.  Aliased fields
      --  aren't packed either.

      if Present (Component_Clause (F)) or else not Is_Packed (Full_Scope (F))
        or else Is_Aliased (F)
      then
         return False;
      end if;

      --  If the type's size is variable or if the type is nonnative, we
      --  can't pack.  We do pack if the RM_Size of the field's type is
      --  smaller than the Esize of the type.  However, we avoid packing if
      --  the field would be larger than 64 bits since that's usually not
      --  worth it.  We rely here on back-annotation of types so we're sure
      --  that both sizes are set.

      return not Is_Nonnative_Type (GT)
        and then Is_Static_SO_Ref (RM_Size (GT))
        and then Is_Static_SO_Ref (Esize (GT))
        and then RM_Size (GT) < Esize (GT) and then RM_Size (GT) <= 64;

   end Is_Packable_Field;

   ------------------------
   -- Is_Bitfield_By_Rep --
   ------------------------

   function Is_Bitfield_By_Rep
     (F    : Entity_Id;
      Pos  : Uint := No_Uint;
      Size : Uint := No_Uint) return Boolean
   is
      TE       : constant Entity_Id := Full_Etype (F);
      Our_Pos  : constant Uint      :=
        (if   Pos /= No_Uint then Pos elsif Present (Component_Clause (F))
         then Component_Bit_Offset (F) else No_Uint);
      Our_Size : constant Uint      :=
        (if   Size /= No_Uint then Size elsif Is_Packable_Field (F)
         then RM_Size (TE)
         elsif Known_Esize (F) and then Is_Static_SO_Ref (Esize (F))
         then Esize (F) else No_Uint);

   begin
      --  If the position is specified and isn't byte-aligned, it's a bitfield

      if Our_Pos /= No_Uint and then Our_Pos mod BPU /= 0 then
         return True;

      --  If we have no specification of size, either explicitly or
      --  implicitly by packing, this isn't a bitfield

      elsif Our_Size = No_Uint then
         return False;

      --  For integral types, we can only have sizes that are a power of
      --  two due to the way that LLVM handles types like i24.

      elsif Is_Discrete_Or_Fixed_Point_Type (TE) then
         return Our_Size not in Uint_8 | Uint_16 | Uint_32 | Uint_64;
      else
         return Our_Size mod BPU /= 0;
      end if;

   end Is_Bitfield_By_Rep;

   -----------------
   -- Is_Bitfield --
   -----------------

   function Is_Bitfield (F : Entity_Id) return Boolean is
     (Field_Info_Table.Table (Get_Field_Info (F)).First_Bit /= No_Uint);

   -----------------------
   -- Is_Array_Bitfield --
   -----------------------

   function Is_Array_Bitfield (F : Entity_Id) return Boolean is
     (Field_Info_Table.Table (Get_Field_Info (F)).Array_Bitfield);

   ----------------------
   -- Field_Bit_Offset --
   ----------------------

   function Field_Bit_Offset (F : Entity_Id) return Uint is
     (if   not Is_Bitfield (F) then Uint_0
      else Field_Info_Table.Table (Get_Field_Info (F)).First_Bit);

   ----------------------
   -- Get_Variant_Size --
   ----------------------

   function Get_Variant_Size
     (RI         : Record_Info;
      V          : GL_Value;
      In_Size    : GL_Value;
      Cur_Align  : Nat;
      No_Padding : Boolean := False) return GL_Value
   is
      Sizes       : GL_Value_Array (RI.Variants'Range) :=
        (others => No_GL_Value);
      Our_BB      : Basic_Block_T;
      End_BB      : Basic_Block_T;
      To_BBs      : Basic_Block_Array (RI.Variants'Range);
      From_BBs    : Basic_Block_Array (RI.Variants'Range);

   begin
      --  We first go through each variant and compute the sizes of each.
      --  We store the GL_Value's where we've computed those things along
      --  with the starting (for branching into the code) and ending (for
      --  use with Phi) basic blocks for each.
      --
      --  However, if the input size is constant, see if any variants are
      --  of a constant size and avoid creating BB's for them.  We also need
      --  this logic to avoid trying to create a BB at top level for a
      --  record where each variant is the same size and hence isn't dynamic.

      if Is_A_Const_Int (In_Size) then
         for J in RI.Variants'Range loop
            if Is_Const (IDS'(Get_Record_Size_So_Far
                                (Empty, No_GL_Value, RI.Variants (J),
                                 Empty_Record_Info_Id,
                                 No_Padding => No_Padding)))
            then
               Sizes (J) := Variant_Part_Size (RI, No_GL_Value, J,
                                               In_Size, Cur_Align,
                                               No_Padding => No_Padding);
            end if;
         end loop;
      end if;

      --  If the above has computed all of the sizes and they're all
      --  equal, we know the size and needn't do anything else.

      if Present (Sizes (Sizes'First))
        and then (for all J in Sizes'Range =>
                    Sizes (J) = Sizes (Sizes'First))
      then
         return Sizes (Sizes'First);
      end if;

      --  Otherwise, get basic block info and compute any variable sizes

      Our_BB := Get_Insert_Block;
      End_BB := Create_Basic_Block;

      for J in RI.Variants'Range loop
         Disable_LV_Append := Disable_LV_Append + 1;
         To_BBs (J) := Create_Basic_Block;
         Position_Builder_At_End (To_BBs (J));

         --  If this variant is empty, the result is the total size so far.
         --  Otherwise, align the size coming in and add our size to it.

         if No (Sizes (J)) then
            Sizes (J) := Variant_Part_Size (RI, V, J, In_Size, Cur_Align);
         end if;

         From_BBs (J) := Get_Insert_Block;
         Build_Br (End_BB);
         Disable_LV_Append := Disable_LV_Append - 1;
      end loop;

      --  Now emit the code to branch to the fragments we made above.
      --  However, if all the sizes are the same, we can just use that size.

      Position_Builder_At_End (Our_BB);
      Emit_Case_Code (RI.Variant_List, Emit_Expression (RI.Variant_Expr),
                      To_BBs);

      --  Now make the Phi that holds the size and return it

      Position_Builder_At_End (End_BB);
      return Build_Phi (Sizes, From_BBs);
   end Get_Variant_Size;

   ----------------------
   -- Get_Variant_Size --
   ----------------------

   function Get_Variant_Size
     (RI         : Record_Info;
      V          : GL_Value;
      In_Size    : IDS;
      Cur_Align  : Nat;
      No_Padding : Boolean := False) return IDS
   is
      Size     : IDS := No_IDS;
      Our_Size : IDS;

   begin
      --  If we already have a variable size, we're done

      if not Is_Const (In_Size) then
         return Var_IDS;
      end if;

      --  Otherwise, first go through each variant and compute the size of
      --  each, looking only at constant values.  If a size differs from a
      --  previous size, it's not a constant.

      for J in RI.Variants'Range loop
         Our_Size := Variant_Part_Size (RI, V, J, In_Size, Cur_Align,
                                        No_Padding => No_Padding);

         --  If we haven't already set a size, set it.  Otherwise, if our
         --  size differs from the size we saved, the size is variable.

         if No (Size) then
            Size := Our_Size;
         elsif Size /= Our_Size then
            Size := Var_IDS;
         end if;
      end loop;

      --  If there were variants, return their size

      return (if Present (Size) then Size else In_Size);
   end Get_Variant_Size;

   ----------------------
   -- Get_Variant_Size --
   ----------------------

   function Get_Variant_Size
     (RI         : Record_Info;
      V          : GL_Value;
      In_Size    : BA_Data;
      Cur_Align  : Nat;
      No_Padding : Boolean := False) return BA_Data
   is
      function Get_Variant_Expr
        (RI : Record_Info; In_Values : BA_Data_Array) return BA_Data
        with Pre => List_Length_Non_Pragma (RI.Variant_List) =
                    In_Values'Length;
      --  Given an RI and a set of values, corresponding to the values
      --  in the order of the variants in the RI, return a BA_Data that
      --  represents an expression to compute which value is correct
      --  for a specific record object.

      Sizes : BA_Data_Array (RI.Variants'Range) := (others => No_BA);

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
                     This_Cond  :=
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
         Sizes (J) := Variant_Part_Size (RI, V, J, In_Size, Cur_Align,
                                        No_Padding => No_Padding);
      end loop;

      --  Now compute the resulting size

      return Get_Variant_Expr (RI, Sizes);
   end Get_Variant_Size;

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

      FI      := Field_Info_Table.Table (F_Idx);
      F_GT    := FI.GT;
      Our_Idx := FI.Rec_Info_Idx;
      Offset  := Get_Record_Size_So_Far (Rec_Type, V, First_Idx, Our_Idx);
      RI      := Record_Info_Table.Table (Our_Idx);

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
         pragma Assert (not Is_Bitfield (Field));
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

      return GEP_To_Relationship
        (F_GT,
         (if Is_Bitfield (Field) then Reference_To_Unknown else Reference),
         Result,
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
      GT   : constant GL_Type := Primitive_GL_Type (Full_GL_Type (N));
      Expr : Node_Id;

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
               In_F : constant Entity_Id := Entity (First (Choices (Expr)));
               Val  : constant Node_Id   := Expression (Expr);
               V    : GL_Value;
               F    : Entity_Id;

            begin
               if Ekind (In_F) = E_Discriminant
                 and then Is_Unchecked_Union (GT)
               then
                  null;
               elsif Chars (In_F) = Name_uParent then

                  --  If this is "_parent", its fields are our fields too.
                  --  Assume Expression is also an N_Aggregate.

                  pragma Assert (Nkind_In (Expression (Expr),
                                           N_Aggregate,
                                           N_Extension_Aggregate));

                  Result := Emit_Record_Aggregate (Val, Result);
               else
                  --  We are to actually insert the field.  However, if we
                  --  haven't set any information for this field, it may be
                  --  a reference to a field that will cause Constraint_Error.
                  --  If so, just don't do anything with it.

                  F := Find_Matching_Field (Full_Etype (GT), In_F);
                  if Present (Get_Field_Info (F)) then
                     V := Emit_Convert_Value (Val, Get_Field_Type (F));
                     V := Build_Field_Store (Result, F, V);
                     if Present (V) then
                        Result := V;
                     end if;
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

   ----------------------
   -- Build_Field_Load --
   ----------------------

   function Build_Field_Load
     (In_V       : GL_Value;
      In_F       : Entity_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False;
      Prefer_LHS : Boolean  := False) return GL_Value
   is
      R_TE   : constant Entity_Id := Full_Scope (In_F);
      Rec_T  : constant Type_T    := Type_Of (R_TE);
      F      : constant Entity_Id := Find_Matching_Field (R_TE, In_F);
      V      : constant GL_Value  := To_Primitive (In_V);
      F_GT   : constant GL_Type   := Get_Field_Type (F);
      Result : GL_Value;
      pragma Unreferenced (Rec_T);
      --  We had to force elaboration of the type of F's Scope here
      --  since there's a chance it wasn't yet elaborated.

   begin
      --  If we have something in a data form and we're not requiring or
      --  preferring an LHS, and we have information about the field, we
      --  can and should do this with an Extract_Value.

      if Is_Data (V) and then not For_LHS and then not Prefer_LHS
        and then Present (Get_Field_Info (F))
        and then not Is_Nonnative_Type (R_TE)
        and then not Is_Nonnative_Type (F_GT)
        and then not Is_Array_Bitfield (F)
        and then (Full_Etype (V) = R_TE
                    or else Is_Layout_Identical (V, Default_GL_Type (R_TE)))
      then
         Result := Extract_Value_To_Relationship
           (F_GT, V, Field_Ordinal (F),
            (if Is_Bitfield (F) then Unknown else Data));
      else
         Result := Record_Field_Offset (Get (V, Any_Reference), F);
      end if;

      --  If this is the parent field, we're done

      if Chars (F) = Name_uParent then
         return Result;

      --  Check for the trivial case of a zero-length field

      elsif Esize (F) = 0 then
         return (if    Is_Elementary_Type (F_GT) then Const_Null (F_GT)
                 elsif Is_Loadable_Type (F_GT)   then Get_Undef (F_GT)
                 else  Get_Undef_Ref (F_GT));

      --  If we have a bitfield, we need special processing.  Because we have
      --  a lot of intermediate values that don't correspond to Ada types,
      --  we do some low-level processing here when we can't avoid it.

      elsif Is_Bitfield (F) then

         --  If this is a bitfield array type, we need to pointer-pun it to
         --  an integral type that's the width of the bitfield field type.

         if Is_Array_Bitfield (F) then
            declare
               T     : constant Type_T := Get_Element_Type (Type_Of (Result));
               New_T : constant Type_T :=
                 Int_Ty (Nat (ULL'(Get_Type_Size_In_Bits (T))));

            begin
               Result := Ptr_To_Relationship (Result, Pointer_Type (New_T, 0),
                                              Reference_To_Unknown);
            end;
         end if;

         --  Next, do the extraction using two shifts

         declare
            type Opf is access function
              (V, Count : GL_Value; Name : String := "") return GL_Value;

            Loaded    : constant GL_Value := Get (Result, Unknown);
            T         : constant Type_T   := Type_Of (Loaded);
            Result_T  : constant Type_T   := Type_Of (F_GT);
            First_Bit : constant ULL      := UI_To_ULL (Field_Bit_Offset (F));
            Num_Bits  : constant ULL      := UI_To_ULL (Esize (F));
            Val_Width : constant ULL      := Get_Type_Size_In_Bits (T);
            Uns       : constant Boolean  :=
              Is_Unsigned_For_RM (F_GT)
              or else not Is_Discrete_Or_Fixed_Point_Type (F_GT);
            Shl_Count : constant ULL      := Val_Width - First_Bit - Num_Bits;
            Shr_Count : constant ULL      := Val_Width - Num_Bits;
            Shr       : constant Opf      :=
              (if Uns then L_Shr'Access else A_Shr'Access);
         begin
            Result := Shl (Loaded,
                           G (Const_Int (T, Shl_Count, False), F_GT, Unknown),
                           Allow_Overflow => True);
            Result := Shr (Result,
                           G (Const_Int (T, Shr_Count, False), F_GT, Unknown));

            --  If the result is an integral type, we can just convert to
            --  that type, if needed.

            if Get_Type_Kind (Result_T) = Integer_Type_Kind then
               return (if   Result_T = T
                       then G_Is_Relationship (Result, F_GT, Data)
                       else Trunc (Result, F_GT));

            --  Otherwise, truncate to the corresponding bit size

            else
               Result := Trunc (Result, Int_Ty (Nat (ULL'(Get_Type_Size_In_Bits
                                                            (Result_T)))));

               --  For a floating-point type we perform a bit cast

               if Is_Floating_Point_Type (F_GT) then
                  return Bit_Cast (Result, F_GT);

               --  For other types, we have to put into memory via pointer
               --  punning.

               else
                  declare
                     Memory         : constant GL_Value :=
                       (if   Present (LHS) then LHS
                        else Allocate_For_Type (F_GT, F_GT, Empty));
                     Mem_As_Int_Ptr : constant GL_Value :=
                       G (Bit_Cast (IR_Builder, LLVM_Value (Memory),
                                    Pointer_Type (Type_Of (Result), 0), ""),
                          F_GT, Reference_To_Unknown);

                  begin
                     Store (Result, Mem_As_Int_Ptr);

                     --  For aggregates, we need to queue up a write-back of
                     --  the aggregate if this is in an LHS context.

                     if For_LHS and then Is_Bitfield (F) then
                        Add_Write_Back (V, F, Memory);
                     end if;

                     return Memory;
                  end;
               end if;
            end if;
         end;
      end if;

      return Result;
   end Build_Field_Load;

   -----------------------
   -- Build_Field_Store --
   -----------------------

   function Build_Field_Store
     (LHS : GL_Value; In_F : Entity_Id; RHS : GL_Value) return GL_Value
   is
      LHS_GT    : constant GL_Type       := Related_Type (LHS);
      R_TE      : constant Entity_Id     := Full_Scope (In_F);
      Rec_T     : constant Type_T        := Type_Of (R_TE);
      F         : constant Entity_Id     := Find_Matching_Field (R_TE, In_F);
      F_Idx     : constant Field_Info_Id := Get_Field_Info (F);
      FI        : constant Field_Info    := Field_Info_Table.Table (F_Idx);
      F_GT      : constant GL_Type       := FI.GT;
      Idx       : constant Nat           := FI.Field_Ordinal;
      RHS_Cvt   : GL_Value               := Convert_GT (RHS, F_GT);
      Result    : GL_Value               := No_GL_Value;
      First_Bit : ULL;
      Num_Bits  : ULL;
      pragma Unreferenced (Rec_T);
      --  We had to force elaboration of the type of F's Scope here
      --  since there's a chance it wasn't yet elaborated.

   begin
      --  First handle the cases where F isn't a bitfield

      if not Is_Bitfield (F) then
         if Is_Data (LHS) then
            Result := Insert_Value (LHS, Get (RHS_Cvt, Data), unsigned (Idx));
         else
            Emit_Assignment (Record_Field_Offset (LHS, F), Empty, RHS);
         end if;

         return Result;
      end if;

      --  First check for the trivial case of a zero-length field

      if Esize (F) = 0 then
         return (if Is_Data (LHS) then LHS else No_GL_Value);
      end if;

      --  Now we handle the bitfield case.  Like the load case, we do our
      --  masking and shifting operations in an integral type.  This is
      --  either the type of the field or an integral type of the same
      --  width as the array used for the field.
      --
      --  We need to get both the field within LHS that contains F into this
      --  integral type (if it isn't already) and RHS.  Then we perform the
      --  masking and store the data back.

      declare
         LHS_For_Access : constant GL_Value :=
           (if Is_Array_Bitfield (F) and then Is_Data (LHS)
            then Allocate_For_Type (LHS_GT, LHS_GT, Empty, LHS) else LHS);
         RHS_T          : Type_T            :=
           Type_Of (Related_Type (RHS_Cvt));
         RHS_Width      : constant ULL      := Get_Type_Size_In_Bits (RHS_T);
         Data_LHS       : GL_Value          := No_GL_Value;
         Rec_Data       : GL_Value;
         Data_T         : Type_T;
         New_RHS_T      : Type_T;
         Data_Width     : ULL;
         Count          : GL_Value;
         Mask           : GL_Value;

      begin
         --  We first have to form an access to the appropriate field, either
         --  a reference or actual data.  The code above has ensured that
         --  we'll have a reference in the case of an array bitfield.

         if Is_Data (LHS_For_Access) then
            Rec_Data := Extract_Value_To_Relationship
              (F_GT, LHS_For_Access, Field_Ordinal (F), Unknown);
         else
            Data_LHS := Record_Field_Offset (LHS_For_Access, F);
            Rec_Data := Data_LHS;
         end if;

         --  If this is a bitfield array type, we need to pointer-pun it to
         --  an integral type that's the width of the bitfield field type.
         --  We've forced Rec_Data to be a reference in this case.

         if Is_Array_Bitfield (F) then
            declare
               T     : constant Type_T :=
                 Get_Element_Type (Type_Of (Rec_Data));
               New_T : constant Type_T :=
                 Int_Ty (Nat (ULL'(Get_Type_Size_In_Bits (T))));

            begin
               Data_LHS := Ptr_To_Relationship (Data_LHS,
                                                Pointer_Type (New_T, 0),
                                                Reference_To_Unknown);
               Rec_Data := Data_LHS;
            end;
         end if;

         --  Now get the field contents as actual data and get and verify
         --  its type.

         Rec_Data   := Get (Rec_Data, Unknown);
         Data_T     := Type_Of (Rec_Data);
         Data_Width := Get_Type_Size_In_Bits (Data_T);
         pragma Assert (Get_Type_Kind (Data_T) = Integer_Type_Kind);

         --  Our next step is to get RHS into the same type as the
         --  record data.  We have the same three cases as field load,
         --  but here we want to start with an integral value the same
         --  width as the converted input.

         New_RHS_T := Int_Ty (Nat (RHS_Width));
         if Is_Floating_Point_Type (RHS_Cvt) then
            RHS_Cvt := Bit_Cast_To_Relationship (Get (RHS_Cvt, Data),
                                                 New_RHS_T, Unknown);
         elsif Get_Type_Kind (RHS_T) /= Integer_Type_Kind then
            RHS_Cvt := Load (G (Bit_Cast (IR_Builder,
                                          LLVM_Value (Get (RHS_Cvt,
                                                           Reference)),
                                          Pointer_Type (New_RHS_T, 0), ""),
                                Related_Type (RHS_Cvt), Reference_To_Unknown));
         end if;

         --  Next, we do shifts, masks, and a logical "or" to compute the
         --  new value of the field.

         RHS_T     := Type_Of (RHS_Cvt);
         First_Bit := UI_To_ULL (Field_Bit_Offset (F));
         Num_Bits  := UI_To_ULL (Esize (F));

         --  If this is narrower than the field size, mask off the high bits.
         --  One might think that we don't have to do this in the unsigned case
         --  since a failed suppressed check is erroneous, but the value might
         --  also be out of range due to being uninitialized and that's a
         --  bounded error (13.9.1(9-11)).

         if Num_Bits /= RHS_Width then
            Count   := G (Const_Int (RHS_T, RHS_Width - Num_Bits, False),
                          F_GT, Unknown);
            RHS_Cvt := L_Shr (Shl (RHS_Cvt, Count, Allow_Overflow => True),
                              Count);
         end if;

         --  If needed, convert the RHS to the size of the bitfield field.
         --  This is usually an extension, but can be a truncation in some
         --  cases where RHS is a record type with padding.

         if RHS_Width < Data_Width then
            RHS_Cvt := Z_Ext (RHS_Cvt, Data_T);
         elsif RHS_Width > Data_Width then
            RHS_Cvt := Trunc (RHS_Cvt, Data_T);
         end if;

         --  Now form the mask, remove the old value, and insert the new value

         Count    := G (Const_Int (Data_T, First_Bit, False), F_GT, Unknown);
         Mask     := G (Const_Int (Data_T, ULL'Last, True), F_GT, Unknown);
         Mask     := L_Shr (Mask, G (Const_Int (Data_T, Data_Width - Num_Bits,
                                             False),
                                 F_GT, Unknown));
         Mask     := Build_Not (Shl (Mask, Count));
         Rec_Data := Build_Or (Build_And (Rec_Data, Mask),
                               Shl (RHS_Cvt, Count));
         --  If we're still working with data, then insert the new value into
         --  the field. Otherwise, store it where it belongs.

         if Is_Data (LHS_For_Access) then
            Result := Insert_Value (LHS_For_Access, Rec_Data,
                                    unsigned (Idx));
         else
            Store (Rec_Data, Data_LHS);
         end if;

         --  If our input was data, but we made an LHS above, we need to
         --  reload the data.

         if No (Result) and then Is_Data (LHS) then
            Result := Load (LHS_For_Access);
         end if;

         return Result;
      end;

   end Build_Field_Store;

   --------------------
   -- Add_Write_Back --
   --------------------

   procedure Add_Write_Back (LHS : GL_Value; F : Entity_Id; RHS : GL_Value) is
   begin
      Writeback_Stack.Append ((LHS => LHS, F => F, RHS => RHS));
   end Add_Write_Back;

   ------------------------
   -- Perform_Writebacks --
   ------------------------

   procedure Perform_Writebacks is
   begin
      for J in reverse 1 .. Writeback_Stack.Last loop
         declare
            WB : constant Write_Back := Writeback_Stack.Table (J);

         begin
            Discard (Build_Field_Store (WB.LHS, WB.F, WB.RHS));
         end;
      end loop;

      Writeback_Stack.Set_Last (0);
   end Perform_Writebacks;

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
         if FI.First_Bit /= No_Uint then
            Write_Str (", Bits = ");
            Write_Int (UI_To_Int (FI.First_Bit));
            Write_Str (" .. ");
            Write_Int (UI_To_Int (FI.First_Bit + FI.Num_Bits - 1));
         end if;

         if FI.Array_Bitfield then
            Write_Str (", Array Bitfield");
         end if;

         Write_Str (": ");
         Dump_GL_Type (FI.GT);
         Write_Eol;
         Print_RI_Briefly (FI.Rec_Info_Idx);
      end if;
   end Print_Field_Info;

   -----------------------
   -- Print_Record_Info --
   -----------------------

   procedure Print_Record_Info (TE : Entity_Id) is

      package Field_Table is new Table.Table
        (Table_Component_Type => Entity_Id,
         Table_Index_Type     => Int,
         Table_Low_Bound      => 1,
         Table_Initial        => 20,
         Table_Increment      => 5,
         Table_Name           => "Field_Table");

      function  FI_Before (J1, J2 : Int) return Boolean;
      --  Indicates the ordering of two entries in the above table

      procedure Swap_Fields (J1, J2 : Int);
      --  Swap fields in above table

      procedure Sort is new Ada.Containers.Generic_Sort
        (Index_Type => Int, Before => FI_Before, Swap => Swap_Fields);

      ---------------
      -- FI_Before --
      ---------------

      function FI_Before (J1, J2 : Int) return Boolean is
         E1     : constant Entity_Id      := Field_Table.Table (J1);
         E2     : constant Entity_Id      := Field_Table.Table (J2);
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

         if FI1.Rec_Info_Idx /= FI2.Rec_Info_Idx then
            return FI1.Rec_Info_Idx < FI2.Rec_Info_Idx;
         elsif FI1.Field_Ordinal /= FI2.Field_Ordinal then
            return FI1.Field_Ordinal < FI2.Field_Ordinal;
         else
            return FI1.First_Bit < FI2.First_Bit;
         end if;
      end FI_Before;

      -----------------
      -- Swap_Fields --
      -----------------

      procedure Swap_Fields (J1, J2 : Int) is
         Temp : constant Entity_Id := Field_Table.Table (J1);

      begin
         Field_Table.Table (J1) := Field_Table.Table (J2);
         Field_Table.Table (J2) := Temp;
      end Swap_Fields;

      Field : Entity_Id := First_Component_Or_Discriminant (TE);

   begin
      while Present (Field) loop
         Field_Table.Append (Field);
         Next_Component_Or_Discriminant (Field);
      end loop;

      declare
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
            F                  : Entity_Id;
            F_Idx              : Field_Info_Id;
            FI                 : Field_Info;
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
               Write_Int (RI.Align);
            end if;

            if RI.Position /= 0 then
               Write_Str (" position ");
               Write_Int (Nat (RI.Position));
            end if;

            Write_Eol;
            if Present (RI.GT) then
               Write_Str (Prefix);
               Dump_GL_Type (RI.GT);
            elsif Present (RI.LLVM_Type) then
               Dump_LLVM_Type (RI.LLVM_Type);
            end if;

            for J in 1 .. Field_Table.Last loop
               F     := Field_Table.Table (J);
               F_Idx := Get_Field_Info (F);
               FI    := Field_Info_Table.Table (F_Idx);
               if Ridx = FI.Rec_Info_Idx then
                  Write_Str (Prefix);
                  Write_Str ("    Field");
                  if Present (RI.LLVM_Type) then
                     Write_Str ("@");
                     Write_Int (FI.Field_Ordinal);
                     if FI.First_Bit /= No_Uint then
                        Write_Str ("[");
                        Write_Int (UI_To_Int (FI.First_Bit));
                        Write_Str (" .. ");
                        Write_Int (UI_To_Int (FI.First_Bit + FI.Num_Bits - 1));
                        Write_Str ("]");
                     end if;
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

               Var_Node := First (RI.Variant_List);
               for J in RI.Variants'Range loop
                  Write_Str (Prefix & "    ");
                  Write_Str ("when ");
                  Choice := First (Discrete_Choices (Var_Node));
                  loop
                     Sprint_Node (Choice);
                     Next (Choice);
                     exit when No (Choice);
                     Write_Str (" | ");
                  end loop;

                  Write_Line (" =>");
                  if Present (RI.Variants (J)) then
                     Print_RI_Chain (RI.Variants (J), Prefix & "    ");
                  end if;
                  if Present (RI.Overlap_Variants (J)) then
                     Print_One_RI (RI.Overlap_Variants (J),
                                   Prefix & "   overlap ");
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
         Sort (1, Field_Table.Last);
         Print_RI_Chain (Get_Record_Info (TE));
      end;

   end Print_Record_Info;

end GNATLLVM.Records;
