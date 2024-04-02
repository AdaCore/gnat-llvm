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

with Elists;     use Elists;
with Nlists;     use Nlists;
with Output;     use Output;
with Repinfo;    use Repinfo;
with Sem_Ch13;   use Sem_Ch13;
with Snames;     use Snames;
with Sprint;     use Sprint;
with Uintp.LLVM; use Uintp.LLVM;

with GNATLLVM.Aliasing;      use GNATLLVM.Aliasing;
with GNATLLVM.Compile;       use GNATLLVM.Compile;
with GNATLLVM.Conditionals;  use GNATLLVM.Conditionals;
with GNATLLVM.Conversions;   use GNATLLVM.Conversions;
with GNATLLVM.DebugInfo;     use GNATLLVM.DebugInfo;
with GNATLLVM.Exprs;         use GNATLLVM.Exprs;
with GNATLLVM.Instructions;  use GNATLLVM.Instructions;
with GNATLLVM.Subprograms;   use GNATLLVM.Subprograms;
with GNATLLVM.Utils;         use GNATLLVM.Utils;
with GNATLLVM.Variables;     use GNATLLVM.Variables;
with GNATLLVM.Wrapper;       use GNATLLVM.Wrapper;

package body GNATLLVM.Records is

   --  When computing the size of a record subtype, we push the subtype so
   --  we can see if we run into a discriminant from its base type. If we
   --  do, we substitute the expression that corresponds to the discriminant
   --  type. In most cases, but not all, the front end already does this
   --  substitution for us. However, we have to be sure that we don't use
   --  the same entry more than once since this could cause infinite recursion.

   type SS_Entry is record
      TE   : E_Record_Subtype_Id;
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
      F   : Opt_Record_Field_Kind_Id;
      RHS : GL_Value;
   end record;

   package Writeback_Stack is new Table.Table
     (Table_Component_Type => Write_Back,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 2,
      Table_Increment      => 1,
      Table_Name           => "Writeback_Stack");

   function RI_Size_Complexity
     (Idx : Record_Info_Id; Max_Size : Boolean) return Nat
     with Pre => Present (Idx);
   --  Return the size complexity of the RI designated by Idx

   function Get_Variant_Size
     (RI          : Record_Info;
      V           : GL_Value;
      In_Size     : GL_Value;
      Force_Align : Nat;
      No_Padding : Boolean := False) return GL_Value
     with Pre  => RI.Variants /= null and then RI.Overlap_Variants /= null
                  and then Present (In_Size),
          Post => Present (Get_Variant_Size'Result);
   --  Compute the total size of a record with the information from a fragment
   --  known to be a variant and where we're not getting the maximum size.

   function Get_Variant_Size
     (RI          : Record_Info;
      V           : GL_Value;
      In_Size     : IDS;
      Force_Align : Nat;
      No_Padding : Boolean := False) return IDS
     with Pre  => RI.Variants /= null and then RI.Overlap_Variants /= null
                  and then Present (In_Size),
          Post => Present (Get_Variant_Size'Result);
   --  Version of above for Is_Dynamic_Size

   function Get_Variant_Size
     (RI          : Record_Info;
      V           : GL_Value;
      In_Size     : BA_Data;
      Force_Align : Nat;
      No_Padding  : Boolean := False) return BA_Data
     with Pre => RI.Variants /= null and then RI.Overlap_Variants /= null;
   --  Version of above for back-annotation

   function Variant_Part_Size
     (RI          : Record_Info;
      V           : GL_Value;
      J           : Int;
      In_Size     : GL_Value;
      Force_Align : Nat;
      Max_Size    : Boolean := False;
      No_Padding  : Boolean := False) return GL_Value
     with Pre  => RI.Variants /= null and then RI.Overlap_Variants /= null
                  and then J in RI.Variants'Range and then Present (In_Size),
          Post => Present (Variant_Part_Size'Result);
   --  Computes the contribution to Total_Size of the variant part with
   --  index J in RI. In_Size is the total size not considering the
   --  variant and the known alignment at that point.

   function Variant_Part_Size
     (RI          : Record_Info;
      V           : GL_Value;
      J           : Int;
      In_Size     : IDS;
      Force_Align : Nat;
      Max_Size    : Boolean := False;
      No_Padding  : Boolean := False) return IDS
     with Pre  => RI.Variants /= null and then RI.Overlap_Variants /= null
                  and then J in RI.Variants'Range and then Present (In_Size),
          Post => Present (Variant_Part_Size'Result);
   --  Version of above for computing if something is dynamic size

   function Variant_Part_Size
     (RI          : Record_Info;
      V           : GL_Value;
      J           : Int;
      In_Size     : BA_Data;
      Force_Align : Nat;
      Max_Size    : Boolean := False;
      No_Padding  : Boolean := False) return BA_Data
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

   function Record_Type_For_Field
     (GT : GL_Type; F : Record_Field_Kind_Id) return Record_Kind_Id
     with Pre  => Present (GT);
   --  We have an object of type GT and want to reference field F. Return
   --  the record type that we have to use for the reference.

   --  We put the routines used to compute sizes into a generic so that we
   --  can instantiate them using various types of sizing. The most common
   --  case is an actual size computation, where we produce a GL_Value.
   --  But we may also instantiate this package to generate the structure
   --  needed for back-annotation.

   generic
      type Result is private;
      No_Result : Result;
      with function Size_Const_Int
        (C : ULL; Sign_Extend : Boolean := False) return Result;
      with function Get_Type_Size
        (GT         : GL_Type;
         V          : GL_Value := No_GL_Value;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result;
      with function Get_Variant_Size
        (RI          : Record_Info;
         V           : GL_Value;
         In_Size     : Result;
         Force_Align : Nat;
         No_Padding  : Boolean := False) return Result;
      with function "+" (V1, V2 : Result) return Result;
      with function "-" (V1, V2 : Result) return Result;
      with function Neg (V : Result; Name : String := "") return Result;
      with function Build_And
        (V1, V2 : Result; Name : String := "") return Result;
      with function Build_Max
        (V1, V2 : Result; Name : String := "") return Result;
      with function Is_A_Constant_Int       (V : Result)    return Boolean;
      with function Get_Const_Int_Value_ULL (V : Result)    return ULL;
      with function Replace_Val             (O, N : Result) return Result;
   package Size is

      function No      (V : Result) return Boolean is (V =  No_Result);
      function Present (V : Result) return Boolean is (V /= No_Result);

      function Only_Overlap_RIs (Idx : Record_Info_Id) return Boolean;
      --  Return True if Idx is null or if the only RIs after it are RIs
      --  for variants with only overlap parts.

      procedure Get_RI_Info
        (RI          : Record_Info;
         V           : GL_Value;
         Max_Size    : Boolean;
         Cur_Align   : Nat;
         Force_Align : Nat;
         Total_Size  : in out Result;
         Must_Align  : out Nat;
         Is_Align    : out Nat;
         Return_Size : Boolean := True;
         No_Padding  : Boolean := False);
      --  Return information about a record fragment RI. This includes the
      --  amount to which this fragment must be aligned and the amout to
      --  which the resulting size is known to be aligned. Also update the
      --  total size with the size of the fragment. If the size isn't
      --  wanted, don't compute it. Cur_Align is the known alignment of
      --  the size so far.

      function Get_Variant_For_RI
        (In_RI       : Record_Info;
         V           : GL_Value;
         Max_Size    : Boolean;
         Need_Idx    : Record_Info_Id;
         Use_Overlap : Boolean := False) return Record_Info_Id
        with Pre => Present (Need_Idx);
      --  We are at RI when walking the description for a record and
      --  it has variants. We're looking for Need_Idx. If Need_Idx is an
      --  index in one of the variants, return that variant.

      function Get_Variant_Max_Size
        (RI          : Record_Info;
         In_Size     : Result;
         Force_Align : Nat;
         No_Padding  : Boolean := False) return Result
        with Pre  => (RI.Variants /= null or else RI.Overlap_Variants /= null)
                     and then Present (In_Size),
             Post => Present (Get_Variant_Max_Size'Result);
      --  Get information corresponding to the maxium size of the variant
      --  described by RI, In_Size, and Force_Align.

      function Get_Record_Size_So_Far
        (TE          : Opt_Record_Kind_Id;
         V           : GL_Value;
         Start_Idx   : Record_Info_Id := Empty_Record_Info_Id;
         Idx         : Record_Info_Id := Empty_Record_Info_Id;
         In_Size     : Result         := No_Result;
         Force_Align : Nat            := BPU;
         Max_Size    : Boolean        := False;
         No_Padding  : Boolean        := False) return Result;
      --  Similar to Get_Record_Type_Size, but stop at record info segment Idx
      --  or the last segment, whichever comes first. If TE is Present, it
      --  provides the default for Start_Idx and also requests alignment to
      --  TE's alignment if we're looking for the size. Total_Size is the

      function Get_Record_Type_Size
        (TE         : Record_Kind_Id;
         V          : GL_Value;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result;
      --  Like Get_Type_Size, but only for record types

      function Emit_Field_Position
        (E : Record_Field_Kind_Id; V : GL_Value) return Result;
      --  Compute and return the position in bytes of the field specified by E
      --  from the start of its type as a value of Size_Type. If Present, V
      --  is a value of that type, which is used in the case of a
      --  discriminated record.

      function Align_To
        (V : Result; Cur_Align, Must_Align : Nat) return Result;
      --  V is a value aligned to Cur_Align. Ensure that it's aligned to
      --  Align_To.

      function Variant_Part_Size
        (RI          : Record_Info;
         V           : GL_Value;
         J           : Int;
         In_Size     : Result;
         Force_Align : Nat;
         Max_Size    : Boolean := False;
         No_Padding  : Boolean := False) return Result
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

   function Use_Discriminant_For_Bound (E : E_Discriminant_Id) return GL_Value
   is
      Rec_Type   : constant Record_Kind_Id := Full_Scope (E);
      TE         : constant Type_Kind_Id   := Full_Etype (E);

   begin
      --  If we're just elaborating decls, return undef

      if Decls_Only then
         return Get_Undef (Full_GL_Type (E));
      end if;

      --  See if we've pushed a subtype of this record type into our
      --  stack of record subtypes. If so, get the discriminant constraint
      --  from that subtype. But ignore a constraint on this discriminant
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

   ---------------------------
   -- Record_Type_For_Field --
   ---------------------------

   function Record_Type_For_Field
     (GT : GL_Type; F : Record_Field_Kind_Id) return Record_Kind_Id
   is
      TE    : constant Record_Kind_Id           := Full_Etype (GT);
      New_F : constant Opt_Record_Field_Kind_Id := Find_Matching_Field (TE, F);

   begin
      --  We'd prefer to use GT's type, but only if we can find a match for
      --  F. If not, we have to use F's type.

      return (if Present (New_F) then TE else Full_Scope (F));

   end Record_Type_For_Field;

   -------------------------
   -- Find_Matching_Field --
   -------------------------

   function Find_Matching_Field
     (TE    : Record_Kind_Id;
      Field : Record_Field_Kind_Id) return Opt_Record_Field_Kind_Id
   is
      Ent : Opt_Record_Field_Kind_Id := First_Component_Or_Discriminant (TE);

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
     (TE : E_Record_Subtype_Id; E : E_Discriminant_Id) return N_Subexpr_Id
   is
      Discrim_Num : constant Uint      := Discriminant_Number (E);
      Constraint  : constant Elist_Id  := Stored_Constraint (TE);
      Elmt        : Elmt_Id            := First_Elmt (Constraint);

   begin
      --  Skip to the proper entry in the list and see if it's static

      for J in Nat (1) .. +Discrim_Num - 1 loop
         Next_Elmt (Elmt);
      end loop;

      return Node (Elmt);
   end Get_Discriminant_Constraint;

   --  These are the generic functions to compute the size of record and
   --  offsets of fields within them.

   package body Size is

      ----------------------
      -- Only_Overlap_RIs --
      ----------------------

      function Only_Overlap_RIs (Idx : Record_Info_Id) return Boolean is
         J : Record_Info_Id := Idx;

      begin
         while Present (J) loop
            declare
               RI : constant Record_Info := Record_Info_Table.Table (J);

            begin
               exit when Present (RI.LLVM_Type) or else Present (RI.GT);
               exit when RI.Variants /= null
                 and then (for some K of RI.Variants.all => Present (K));

               J := RI.Next;
            end;
         end loop;

         return No (J);
      end Only_Overlap_RIs;

      -----------------
      -- Get_RI_Info --
      -----------------

      procedure Get_RI_Info
        (RI          : Record_Info;
         V           : GL_Value;
         Max_Size    : Boolean;
         Cur_Align   : Nat;
         Force_Align : Nat;
         Total_Size  : in out Result;
         Must_Align  : out Nat;
         Is_Align    : out Nat;
         Return_Size : Boolean := True;
         No_Padding  : Boolean := False)
      is
         T         : constant Type_T  := RI.LLVM_Type;
         GT        : constant GL_Type := RI.GT;
         This_Size : Result           := No_Result;

      begin
         --  If this piece has a starting position specified, move to it.

         if RI.Position /= 0 then
            Total_Size := Replace_Val (Total_Size,
                                       Size_Const_Int (RI.Position));
         end if;

         --  If we have an LLVM type, it's packed record, so our size will
         --  be that of the record and we aren't forcing an alignment. If
         --  our total size is a constant, we can say what our alignment is.

         if Present (T) then
            This_Size  := Size_Const_Int (Get_Type_Size (T));
            Must_Align := BPU;
            Is_Align   :=
              (if   Is_A_Constant_Int (Total_Size)
               then ULL_Align (Get_Const_Int_Value_ULL
                                 (Total_Size + This_Size))
               else Nat'(BPU));

         --  For a GNAT type, do similar, except that we know more about
         --  our alignment.

         elsif Present (GT) then
            Must_Align   := Get_Type_Alignment (GT);
            Is_Align     := Must_Align;

            if Return_Size then
               This_Size := Get_Type_Size (GT, V, Max_Size);
            end if;

         --  For a variant, we've already set the variant alignment, so use
         --  that for Must_Align. We can have fields after the variants
         --  in the case of extension records, so we care about Is_Align.
         --  But pessimize it rather than calculate it since the saving
         --  isn't worth it in this obscure case.

         elsif RI.Variants /= null or else RI.Overlap_Variants /= null then

            --  If we're looking for the maximum size, do our normal
            --  processing of aligning and adding the size. But if looking
            --  for the actual size, pass in the total size and let it be
            --  updated. That avoids aligning if the variant part chosen
            --  has no fields. So we return in that case.

            Must_Align := Nat'Max (Force_Align, RI.Align);
            Is_Align   := BPU;

            if Return_Size then
               Total_Size :=
                 (if   Max_Size
                  then Get_Variant_Max_Size (RI, Total_Size, Must_Align,
                                             No_Padding => No_Padding)
                  else Get_Variant_Size (RI, V, Total_Size, Must_Align,
                                         No_Padding => No_Padding));
               return;
            end if;

         --  Otherwise, this is a null entry

         else
            Must_Align := BPU;
            Is_Align   := Max_Align;
            This_Size  := Size_Const_Int (0);
         end if;

         --  Take into account any alignment we set for this RI. We
         --  needn't force alignment in the case of a forced position.

         if RI.Align /= 0 and then RI.Position = 0 then
            Must_Align := Nat'Max (Must_Align, RI.Align);
         end if;

         --  Now update the total size given what we've computed above. If
         --  this is the last RI (or subsequent RI's only have overlap RIs
         --  in variants), and we're asking for the size without padding,
         --  subtract the unused bits from the size of this piece.

         Must_Align := Nat'Max (Force_Align, Must_Align);

         if Return_Size then
            if Only_Overlap_RIs (RI.Next) and then No_Padding then
               This_Size :=
                 This_Size - Size_Const_Int (+RI.Unused_Bits);
            end if;

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
            --  variants if necessary. We start looking at the first chained
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
        (RI          : Record_Info;
         In_Size     : Result;
         Force_Align : Nat;
         No_Padding  : Boolean := False) return Result
      is
         Max_Const_Size : ULL    := 0;
         Max_Var_Size   : Result := No_Result;
         Our_Size       : Result;

      begin
         --  We need to compute the maximum size of each variant. Most
         --  discriminant sizes are constant, so we use an algorithm
         --  that'll work best in that situation. First record the largest
         --  constant size and make a chain of Max operations to compute
         --  the largest non-constant.

         for J in RI.Variants'Range loop
            Our_Size := Variant_Part_Size (RI, No_GL_Value, J, In_Size,
                                           Force_Align,
                                           Max_Size   => True,
                                           No_Padding => No_Padding);
            if Is_A_Constant_Int (Our_Size) then
               if Get_Const_Int_Value_ULL (Our_Size) > Max_Const_Size then
                  Max_Const_Size := Get_Const_Int_Value_ULL (Our_Size);
               end if;
            elsif No (Max_Var_Size) then
               Max_Var_Size := Our_Size;
            else
               Max_Var_Size := Build_Max (Our_Size, Max_Var_Size);
            end if;
         end loop;

         --  Now merge the variable and constant sizes

         if No (Max_Var_Size) then
            return Size_Const_Int (Max_Const_Size);
         elsif Max_Const_Size = 0 then
            return Max_Var_Size;
         else
            return Build_Max (Max_Var_Size, Size_Const_Int (Max_Const_Size));
         end if;
      end Get_Variant_Max_Size;

      ----------------------------
      -- Get_Record_Size_So_Far --
      ----------------------------

      function Get_Record_Size_So_Far
        (TE          : Opt_Record_Kind_Id;
         V           : GL_Value;
         Start_Idx   : Record_Info_Id := Empty_Record_Info_Id;
         Idx         : Record_Info_Id := Empty_Record_Info_Id;
         In_Size     : Result         := No_Result;
         Force_Align : Nat            := BPU;
         Max_Size    : Boolean        := False;
         No_Padding  : Boolean        := False) return Result
      is
         Total_Size   : Result         :=
           (if Present (In_Size) then In_Size else Size_Const_Int (0));
         Cur_Align    : Nat            :=
           (if   Present (In_Size) then BPU else Max_Align);
         Cur_Idx      : Record_Info_Id :=
           (if    Present (Start_Idx) then Start_Idx
            elsif Present (TE) then Get_Record_Info (TE)
            else  Empty_Record_Info_Id);
         Must_Align   : Nat            := BPU;
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

            --  If we're reached a variant point, we have two cases. We
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
                     Total_Size := Size_Const_Int (0);
                  end if;
               end if;
            end if;

            if Present (New_Idx) then
               Cur_Idx := New_Idx;
            else
               Get_RI_Info (RI, V, Max_Size, Cur_Align, Force_Align,
                            Total_Size, Must_Align, This_Align,
                            No_Padding => No_Padding);

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
         --  we should have hit it. If either is the case, we have an
         --  error where we're looking for a field in the wrong type.

         pragma Assert (Cur_Idx = Idx);

         --  Now we may have to do a final alignment. If Idx is specified,
         --  use the alignment for that field. Otherwise, use the
         --  alignment for the type.

         if Present (Idx) then
            Get_RI_Info (Record_Info_Table.Table (Idx), No_GL_Value, False,
                         Cur_Align, BPU, Total_Size, Must_Align, This_Align,
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
        (TE         : Record_Kind_Id;
         V          : GL_Value;
         Max_Size   : Boolean := False;
         No_Padding : Boolean := False) return Result is

      begin
         return Get_Record_Size_So_Far
           (TE, V,
            Max_Size   => Max_Size or else Is_Unchecked_Union (TE),
            No_Padding => No_Padding);
      end Get_Record_Type_Size;

      -------------------------
      -- Emit_Field_Position --
      -------------------------

      function Emit_Field_Position
        (E : Record_Field_Kind_Id; V : GL_Value) return Result
      is
         TE     : constant Record_Kind_Id := Full_Scope (E);
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
            return No_Result;
         end if;

         FI     := Field_Info_Table.Table (F_Idx);
         Idx    := FI.Rec_Info_Idx;
         RI     := Record_Info_Table.Table (Idx);
         Offset := Get_Record_Size_So_Far (TE, V,
                                           Start_Idx => R_Idx,
                                           Idx       => Idx);

         --  Offset now gives the offset from the start of the record to the
         --  piece that this field is in. If this piece has a GL_Type, then
         --  the field is the entire piece and we have the offset. If it's an
         --  LLVM type, we need to compute the offset within that type.

         if Present (RI.GT) then
            return Offset;
         else
            declare
               Ordinal     : constant unsigned := unsigned (FI.Field_Ordinal);
               This_Offset : constant ULL      :=
                 Offset_Of_Element (Module_Data_Layout, RI.LLVM_Type, Ordinal);

            begin
               return Offset + Size_Const_Int (This_Offset * UBPU);
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
         --  nothing. Otherwise, align.

         if Must_Align <= Cur_Align then
            return V;
         else
            return Build_And (V + Size_Const_Int (ULL (Must_Align - 1)),
                              Neg (Size_Const_Int (ULL (Must_Align))));
         end if;
      end Align_To;

      -----------------------
      -- Variant_Part_Size --
      -----------------------

      function Variant_Part_Size
        (RI          : Record_Info;
         V           : GL_Value;
         J           : Int;
         In_Size     : Result;
         Force_Align : Nat;
         Max_Size    : Boolean := False;
         No_Padding  : Boolean := False) return Result
      is
         Variant_Size : constant Result :=
           Get_Record_Size_So_Far (Empty, V,
                                   Start_Idx   => RI.Variants (J),
                                   In_Size     => In_Size,
                                   Force_Align => Force_Align,
                                   Max_Size    => Max_Size,
                                   No_Padding  => No_Padding);
         Overlap_Size : constant Result :=
           Get_Record_Size_So_Far (Empty, V,
                                   Start_Idx => RI.Overlap_Variants (J),
                                   No_Padding => No_Padding);

      begin
         --  The resulting size is the maximum of that and the size of the
         --  overlap portion (but special-case zero).

         if Variant_Size = Size_Const_Int (0) then
            return Overlap_Size;
         elsif Overlap_Size = Size_Const_Int (0) then
            return Variant_Size;
         else
            return Build_Max (Variant_Size, Overlap_Size);
         end if;
      end Variant_Part_Size;

   end Size;

   --  Replace_Val is used to replace a size value with a new value.
   --  For both the normal and back-annotation cases, this is the second
   --  operand. However, for the purpose of determining if this is of
   --  dynamic size, we need to conside the size variable if either the old
   --  or the new value is variable to avoid trying to compute something
   --  dynamic at the global level. We could probably avoid this computation
   --  if worth it, but the case here is very unusual (involving partially-
   --  repped extension records), so it's not worth the trouble.

   function Replace_Val (Unused_O, N : GL_Value) return GL_Value is
      (N);

   function Replace_Val (Unused_O, N : BA_Data) return BA_Data is
      (N);

   function Replace_Val (O, N : IDS) return IDS is
      (if Is_Const (O) then N else Var_IDS);

   --  Here we instantiate the size routines with functions that compute
   --  the LLVM value the size and make those visible to clients. Since the
   --  addition and subtraction functions as well as binary "and" can be
   --  called with both addresses and integers, we define wrappers that
   --  either call our abstractions for address arithmetic or directly
   --  generate code for regular integer operations.

   function Address_Or_Integer_Add (V1, V2 : GL_Value) return GL_Value is
     (if Is_Address (V1) then Address_Add (V1, V2) else V1 + V2);

   function Address_Or_Integer_Sub (V1, V2 : GL_Value) return GL_Value is
     (if Is_Address (V1) then Address_Sub (V1, V2) else V1 - V2);

   function Address_Or_Integer_And
     (V1, V2 : GL_Value; Name : String := "") return GL_Value
   is
     (if Tagged_Pointers and then Is_Address (V1) then
        Set_Pointer_Address
          (V1, Build_And (Get_Pointer_Address (V1), V2, Name))
      else Build_And (V1, V2, Name));

   package LLVM_Size is
      new Size (Result                  => GL_Value,
                No_Result               => No_GL_Value,
                Size_Const_Int          => Size_Const_Int,
                "+"                     => Address_Or_Integer_Add,
                "-"                     => Address_Or_Integer_Sub,
                Neg                     => Neg,
                Build_And               => Address_Or_Integer_And,
                Build_Max               => Build_Max,
                Is_A_Constant_Int       => Is_A_Constant_Int,
                Get_Const_Int_Value_ULL => Get_Const_Int_Value_ULL,
                Get_Type_Size           => Get_Type_Size,
                Get_Variant_Size        => Get_Variant_Size,
                Replace_Val             => Replace_Val);

   function Get_Record_Size_So_Far
     (TE          : Opt_Record_Kind_Id;
      V           : GL_Value;
      Start_Idx   : Record_Info_Id := Empty_Record_Info_Id;
      Idx         : Record_Info_Id := Empty_Record_Info_Id;
      In_Size     : GL_Value       := No_GL_Value;
      Force_Align : Nat            := BPU;
      Max_Size    : Boolean        := False;
      No_Padding  : Boolean        := False) return GL_Value
     renames LLVM_Size.Get_Record_Size_So_Far;

   function Get_Record_Type_Size
     (TE         : Record_Kind_Id;
      V          : GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return GL_Value
     renames LLVM_Size.Get_Record_Type_Size;

   function Emit_Field_Position
     (E : Record_Field_Kind_Id; V : GL_Value) return GL_Value
     renames LLVM_Size.Emit_Field_Position;

   function Align_To
     (V : GL_Value; Cur_Align, Must_Align : Nat) return GL_Value
     renames LLVM_Size.Align_To;

   function Variant_Part_Size
     (RI          : Record_Info;
      V           : GL_Value;
      J           : Int;
      In_Size     : GL_Value;
      Force_Align : Nat;
      Max_Size    : Boolean := False;
      No_Padding  : Boolean := False) return GL_Value
     renames LLVM_Size.Variant_Part_Size;

   --  Here we instantiate the size routines with functions that compute
   --  whether a size is dynamic or not and make those visible to clients.

   package IDS_Size is
      new Size (Result                  => IDS,
                No_Result               => No_IDS,
                Size_Const_Int          => Const,
                "+"                     => "+",
                "-"                     => "-",
                Neg                     => Neg,
                Build_And               => Build_And,
                Build_Max               => Build_Max,
                Is_A_Constant_Int       => Is_Const,
                Get_Const_Int_Value_ULL => Const_Val_ULL,
                Get_Type_Size           => Get_Type_Size,
                Get_Variant_Size        => Get_Variant_Size,
                Replace_Val             => Replace_Val);

   function Get_Record_Type_Size
     (TE         : Record_Kind_Id;
      V          : GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return IDS
     renames IDS_Size.Get_Record_Type_Size;

   function Get_Record_Size_So_Far
     (TE          : Opt_Record_Kind_Id;
      V           : GL_Value;
      Start_Idx   : Record_Info_Id := Empty_Record_Info_Id;
      Idx         : Record_Info_Id := Empty_Record_Info_Id;
      In_Size     : IDS            := No_IDS;
      Force_Align : Nat            := BPU;
      Max_Size    : Boolean        := False;
      No_Padding  : Boolean        := False) return IDS
     renames IDS_Size.Get_Record_Size_So_Far;

   function Align_To (V : IDS; Cur_Align, Must_Align : Nat) return IDS
     renames IDS_Size.Align_To;

   function Variant_Part_Size
     (RI          : Record_Info;
      V           : GL_Value;
      J           : Int;
      In_Size     : IDS;
      Force_Align : Nat;
      Max_Size    : Boolean := False;
      No_Padding  : Boolean := False) return IDS
     renames IDS_Size.Variant_Part_Size;

   --  Here we instantiate the size routines with functions that compute
   --  back-annotation trees.

   package BA_Size is
      new Size (Result                  => BA_Data,
                No_Result               => No_BA,
                Size_Const_Int          => Const,
                "+"                     => "+",
                "-"                     => "-",
                Neg                     => Neg,
                Build_And               => Build_And,
                Build_Max               => Build_Max,
                Is_A_Constant_Int       => Is_Const,
                Get_Const_Int_Value_ULL => Const_Val_ULL,
                Get_Type_Size           => Get_Type_Size,
                Get_Variant_Size        => Get_Variant_Size,
                Replace_Val             => Replace_Val);

   function Get_Record_Type_Size
     (TE         : Record_Kind_Id;
      V          : GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return BA_Data
     renames BA_Size.Get_Record_Type_Size;

   function Field_Position
     (E : Record_Field_Kind_Id; V : GL_Value) return BA_Data
     renames BA_Size.Emit_Field_Position;

   function Align_To (V : BA_Data; Cur_Align, Must_Align : Nat) return BA_Data
     renames BA_Size.Align_To;

   function Variant_Part_Size
     (RI          : Record_Info;
      V           : GL_Value;
      J           : Int;
      In_Size     : BA_Data;
      Force_Align : Nat;
      Max_Size    : Boolean := False;
      No_Padding  : Boolean := False) return BA_Data
     renames BA_Size.Variant_Part_Size;

   ----------------------
   -- Get_Variant_Size --
   ----------------------

   function Get_Variant_Size
     (RI          : Record_Info;
      V           : GL_Value;
      In_Size     : GL_Value;
      Force_Align : Nat;
      No_Padding  : Boolean := False) return GL_Value
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
      --  of a constant size and avoid creating BB's for them. We also need
      --  this logic to avoid trying to create a BB at top level for a
      --  record where each variant is the same size and hence isn't dynamic.

      if Is_A_Constant_Int (In_Size) then
         for J in RI.Variants'Range loop
            if Is_Const (IDS'(Get_Record_Size_So_Far
                                (Empty, No_GL_Value,
                                 Force_Align => Force_Align,
                                 Start_Idx   => RI.Variants (J),
                                 No_Padding  => No_Padding)))
            then
               Sizes (J) := Variant_Part_Size (RI, No_GL_Value, J, In_Size,
                                               Force_Align,
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
            Sizes (J) := Variant_Part_Size (RI, V, J, In_Size, Force_Align);
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
     (RI          : Record_Info;
      V           : GL_Value;
      In_Size     : IDS;
      Force_Align : Nat;
      No_Padding  : Boolean := False) return IDS
   is
      Size     : IDS := No_IDS;
      Our_Size : IDS;

   begin
      --  If we already have a variable size, we're done

      if not Is_Const (In_Size) then
         return Var_IDS;
      end if;

      --  Otherwise, first go through each variant and compute the size of
      --  each, looking only at constant values. If a size differs from a
      --  previous size, it's not a constant.

      for J in RI.Variants'Range loop
         Our_Size := Variant_Part_Size (RI, V, J, In_Size, Force_Align,
                                        No_Padding => No_Padding);

         --  If we haven't already set a size, set it. Otherwise, if our
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
     (RI          : Record_Info;
      V           : GL_Value;
      In_Size     : BA_Data;
      Force_Align : Nat;
      No_Padding  : Boolean := False) return BA_Data
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
         --  where VO is the "others" alternative. We rely on the fact that,
         --  especially for alignments, that many of the values above are
         --  the same and "strike out" those values that are the same as
         --  ones we've already processed. We rely on the others alternative
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
         --  them. This is a quadratic algorithm, but even in records with
         --  large numbers of variants, the number of different sizes
         --  should be small, so that shouldn't be an issue.

         while (for some V of Values => Present (V)) loop
            declare
               Variant    : Opt_N_Variant_Id :=
                 First_Non_Pragma (RI.Variant_List);
               This_Cond  : BA_Data          := Const (0);
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
         Sizes (J) := Variant_Part_Size (RI, V, J, In_Size, Force_Align,
                                         No_Padding => No_Padding);
      end loop;

      --  Now compute the resulting size

      return Get_Variant_Expr (RI, Sizes);
   end Get_Variant_Size;

   -------------------------------
   -- Effective_Field_Alignment --
   -------------------------------

   function Effective_Field_Alignment (F : Record_Field_Kind_Id) return Nat is
      AF      : constant Record_Field_Kind_Id := Original_Record_Component (F);
      GT      : constant GL_Type              := Full_GL_Type (AF);
      F_Align : constant Nat                  := Get_Type_Alignment (GT);
      Pos     : constant Uint                 := Component_Bit_Offset (AF);
      Size    : constant Uint                 := Esize (AF);
      TE      : constant Record_Kind_Id       := Full_Scope (AF);
      R_Align : constant Nat                  :=
        (if    Known_Alignment (TE) then +Alignment (TE) * BPU
         elsif Known_RM_Size (TE) and then Strict_Alignment (TE)
         then  ULL_Align (+RM_Size (TE))
         elsif Known_Esize (TE) then ULL_Align (+Esize (TE))
         else  Nat'Max (Max_Align, F_Align));
      --  If an alignment is specified for the record, use it. If not, but
      --  a size is specified for the record and we require strict
      --  alignment, derive the alignment from that. Similarly if an
      --  Object_Size clause has been specified. Otherwise, the only
      --  contributions to alignment are the alignment of the fields.

      E_Align : constant Nat                  := Nat'Min (F_Align, R_Align);

   begin
      --  If the field can't be misaligned, its alignment always contributes
      --  directly to the alignment of the record.

      if Cant_Misalign_Field (AF, GT) then
         return F_Align;

      --  Otherwise, if the field is packable its alignment doesn't
      --  contribute to the alignment.

      elsif Field_Pack_Kind (AF, Force => True, Ignore_Size => True) /= None
      then
         return BPU;

      --  If there's no component clause use this field's alignment. But
      --  we can't use an alignment smaller than that of the record

      elsif No (Component_Clause (AF)) then
         return Nat'Min (F_Align, R_Align);

      --  Otherwise, if the alignment is consistent with the size and
      --  position of the component, use it.

      else
         return (if   Pos mod E_Align = 0 and then Size mod E_Align = 0
                 then E_Align else BPU);
      end if;

   end Effective_Field_Alignment;

   -----------------------------------
   -- Record_Has_Aliased_Components --
   -----------------------------------

   function Record_Has_Aliased_Components
     (TE : Record_Kind_Id) return Boolean
   is
      F : Opt_Record_Field_Kind_Id := First_Component_Or_Discriminant (TE);

   begin
      --  We ignore the tag since no user code can take its address

      while Present (F) loop
         exit when Is_Aliased (F) and then Chars (F) /= Name_uTag;
         Next_Component_Or_Discriminant (F);
      end loop;

      return Present (F);
   end Record_Has_Aliased_Components;

   -------------------------------
   -- Get_Record_Type_Alignment --
   -------------------------------

   function Get_Record_Type_Alignment (TE : Record_Kind_Id) return Nat is
      Field : Opt_Record_Field_Kind_Id;

   begin
      --  Use the largest effective alignment of any field

      return Largest_Align : Nat := BPU do

         --  If we're just elaborating types and this is a tagged record,
         --  we have to allow for the tag field because the front end
         --  won't create one in this mode.

         if Decls_Only and then Is_Tagged_Type (TE) then
            Largest_Align := Get_Type_Alignment (Void_Ptr_T);
         end if;

         --  Now go through each field looking for the highest effective
         --  alignment.

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

   function Field_Ordinal (F : Record_Field_Kind_Id) return unsigned is
     (unsigned (Field_Info_Table.Table (Get_Field_Info (F)).Field_Ordinal));

   ----------------
   -- Field_Type --
   ----------------

   function Field_Type (F : Record_Field_Kind_Id) return GL_Type is
      GT : constant GL_Type := Field_Info_Table.Table (Get_Field_Info (F)).GT;
   begin
      --  GT may be a dummy type. In that case, we need to get the
      --  replacement default type.

      return (if Is_Dummy_Type (GT) then Default_GL_Type (GT) else GT);
   end Field_Type;

   ---------------
   -- TBAA_Type --
   ---------------

   function TBAA_Type (Fidx : Field_Info_Id) return Metadata_T is
     (Field_Info_Table.Table (Fidx).TBAA_Type);

   -------------------
   -- Set_TBAA_Type --
   -------------------

   procedure Set_TBAA_Type (Fidx : Field_Info_Id; M : Metadata_T) is
   begin
      Field_Info_Table.Table (Fidx).TBAA_Type := M;
   end Set_TBAA_Type;

   ------------------
   -- Parent_Field --
   ------------------

   function Parent_Field
     (F : Record_Field_Kind_Id) return Opt_Record_Field_Kind_Id
   is
      R_TE : constant Record_Kind_Id           := Full_Scope (F);
      ORC  : constant Opt_Record_Field_Kind_Id :=
        Original_Record_Component (F);
      CRC  : constant Opt_Record_Field_Kind_Id :=
        Corresponding_Record_Component (F);

   begin
      if Present (ORC) and then ORC /= F
        and then Has_Compatible_Representation (R_TE, Full_Scope (ORC))
      then
         return ORC;
      elsif Present (CRC) and then CRC /= F
        and then Has_Compatible_Representation (R_TE, Full_Scope (CRC))
      then
         return CRC;
      else
         return Empty;
      end if;

   end Parent_Field;

   --------------------
   -- Ancestor_Field --
   --------------------

   function Ancestor_Field
     (F : Record_Field_Kind_Id) return Record_Field_Kind_Id
   is
      PF : Opt_Record_Field_Kind_Id;

   begin
      return AF : Record_Field_Kind_Id := F do
         loop
            PF := Parent_Field (AF);

            if Present (PF) then
               AF := PF;
            else
               exit;
            end if;
         end loop;
      end return;
   end Ancestor_Field;

   ------------------------------
   -- RI_To_Struct_Field_Array --
   ------------------------------

   function RI_To_Struct_Field_Array
     (Ridx : Record_Info_Id) return Struct_Field_Array
   is
      package Fields is new Table.Table
        (Table_Component_Type => Struct_Field,
         Table_Index_Type     => Nat,
         Table_Low_Bound      => 1,
         Table_Initial        => 20,
         Table_Increment      => 5,
         Table_Name           => "Fields");

      Last_Ord : Int                  := -1;
      RI       : constant Record_Info := Record_Info_Table.Table (Ridx);
      F_Idx    : Field_Info_Id        := RI.First_Field;
      FI       : Field_Info;

   begin
      --  If this doesn't contain an LLVM type, this is not a native LLVM
      --  structure, so we can't do anything.

      if No (RI.LLVM_Type) then
         return Struct_Field_Array'(1 .. 0 => <>);
      end if;

      --  Otherwise, loop through all the fields in this record. There may
      --  be multiple fields corresponding to one ordinal, so just look at
      --  one of them.

      while Present (F_Idx) loop
         FI := Field_Info_Table.Table (F_Idx);

         if FI.Field_Ordinal /= Last_Ord then
            Last_Ord := FI.Field_Ordinal;
            declare
               F_Type  : constant Type_T        :=
                 Struct_Get_Type_At_Index (RI.LLVM_Type, unsigned (Last_Ord));
               Offset  : constant ULL           :=
                 Get_Element_Offset (RI.LLVM_Type, Last_Ord);
               GT      : constant GL_Type       :=
                 (if Is_Bitfield_By_Rep (FI.Field) then No_GL_Type else FI.GT);

            begin
               Fields.Append ((FI.Field, Offset, F_Type, GT));
            end;
         end if;

         F_Idx := FI.Next;
      end loop;

      declare
         Result : Struct_Field_Array (1 .. Fields.Last);

      begin
         for J in Result'Range loop
            Result (J) := Fields.Table (J);
         end loop;

         return Result;
      end;
   end RI_To_Struct_Field_Array;

   ---------------------
   -- Field_Pack_Kind --
   ---------------------

   function Field_Pack_Kind
     (F           : Record_Field_Kind_Id;
      Force       : Boolean := False;
      Ignore_Size : Boolean := False) return Pack_Kind
   is
      AF : constant Record_Field_Kind_Id := Ancestor_Field (F);
      GT : constant GL_Type              := Full_GL_Type (AF);
      TE : constant Record_Kind_Id       := Full_Scope (AF);
      T  : constant Type_T               := Type_Of (GT);
      pragma Unreferenced (T);
      --  We need to be sure that the type of the field is elaborated

   begin
      --  If we have a rep clause, we'll use that rather than packing it.
      --  If the record isn't packed, neither is the field. Aliased fields
      --  or fields whose types are strictly aligned aren't packed either.
      --  If the type's size is variable or if the type is nonnative, we
      --  can't pack, even to a byte boundary.

      if Present (Component_Clause (AF))
        or else (Component_Alignment (TE) /= Calign_Storage_Unit
                   and then not Is_Packed (TE))
        or else Cant_Misalign_Field (AF, GT)
        or else Is_Nonnative_Type (GT)
        or else not Is_Static_SO_Ref (Esize (GT))
      then
         return None;

      --  We can pack to a bit boundary if the record is packed, the
      --  field's RM Size is known and smaller than the Esize (unless we're
      --  forcing packing), and the field isn't too large (unless we're to
      --  ignore that test). Otherwise, we pack only to a byte boundary.
      --  We rely here on back-annotation of types so we're sure that both
      --  sizes are set.

      else
         return (if   Is_Packed (TE) and then Is_Static_SO_Ref (RM_Size (GT))
                      and then (Ignore_Size or else RM_Size (GT) <= 64)
                      and then (Force or else RM_Size (GT) < Esize (GT))
                 then Bit else Byte);
      end if;

   end Field_Pack_Kind;

   ------------------------
   -- Is_Bitfield_By_Rep --
   ------------------------

   function Is_Bitfield_By_Rep
     (F            : Record_Field_Kind_Id;
      Pos          : Uint    := No_Uint;
      Size         : Uint    := No_Uint;
      Use_Pos_Size : Boolean := False) return Boolean
   is
      TE       : constant Type_Kind_Id := Full_Etype (F);
      Our_Pos  : constant Uint         :=
        (if    Use_Pos_Size then Pos
         elsif Known_Static_Component_Bit_Offset (F)
         then  Component_Bit_Offset (F) else No_Uint);
      Our_Size : constant Uint         :=
        (if    Use_Pos_Size then Size
         elsif Known_Static_Esize (F)    then Esize (F)
         elsif Field_Pack_Kind (F) = Bit then RM_Size (TE) else No_Uint);

   begin
      --  If the position is specified and isn't byte-aligned, it's a bitfield

      if Present (Our_Pos) and then Our_Pos mod BPU /= 0 then
         return True;

      --  If we have no specification of size, either explicitly or
      --  implicitly by packing, this isn't a bitfield

      elsif No (Our_Size) then
         return False;

      --  For integral types, we can only have sizes that are a power of
      --  two due to the way that LLVM handles types like i24.

      elsif Is_Discrete_Or_Fixed_Point_Type (TE) then
         return Our_Size not in
           Uint_8 | Uint_16 | Uint_32 | Uint_64 | Uint_128;
      else
         return Our_Size mod BPU /= 0;
      end if;

   end Is_Bitfield_By_Rep;

   -----------------
   -- Is_Bitfield --
   -----------------

   function Is_Bitfield (F : Record_Field_Kind_Id) return Boolean is
     (Present (Field_Info_Table.Table (Get_Field_Info (F)).First_Bit));

   -----------------------
   -- Is_Array_Bitfield --
   -----------------------

   function Is_Array_Bitfield (F : Record_Field_Kind_Id) return Boolean is
     (Field_Info_Table.Table (Get_Field_Info (F)).Array_Bitfield);

   -----------------------------
   -- Is_Large_Array_Bitfield --
   -----------------------------

   function Is_Large_Array_Bitfield
     (F : Record_Field_Kind_Id) return Boolean
   is
     (Field_Info_Table.Table (Get_Field_Info (F)).Large_Array_Bitfield);

   ----------------------
   -- Field_Bit_Offset --
   ----------------------

   function Field_Bit_Offset (F : Record_Field_Kind_Id) return Uint is
     (if   not Is_Bitfield (F) then Uint_0
      else Field_Info_Table.Table (Get_Field_Info (F)).First_Bit);

   ------------------------
   -- RI_Size_Complexity --
   ------------------------

   function RI_Size_Complexity
     (Idx : Record_Info_Id; Max_Size : Boolean) return Nat
   is
      RI : constant Record_Info := Record_Info_Table.Table (Idx);

   begin
      --  If this represents a GL_Type, use the complexity of that size

      if Present (RI.GT) then
         return Get_Type_Size_Complexity (RI.GT, Max_Size);

      --  If we have a variant expression, we start with the complexity
      --  of evaluating the variant (or zero if we're looking for the
      --  max size) and then add the complexity of each variant (we add
      --  because even if looking for the max size, we have to consider each
      --  variant).

      elsif Present (RI.Variant_Expr) then
         return Complexity : Nat := (if Max_Size then 0 else 2) do
            for Vidx of RI.Variants.all loop
               if Present (Vidx) then
                  Complexity :=
                    Complexity + RI_Size_Complexity (Vidx, Max_Size);
               end if;
            end loop;
         end return;

      --  Otherwise this is fixed size

      else
         return 0;
      end if;
   end RI_Size_Complexity;

   --------------------------------
   -- Get_Record_Size_Complexity --
   --------------------------------

   function Get_Record_Size_Complexity
     (TE : Record_Kind_Id; Max_Size : Boolean := False) return Nat
   is
      Cur_Idx : Record_Info_Id := Get_Record_Info (TE);

   begin
      return Complexity : Nat := 0 do
         while Present (Cur_Idx) loop
            Complexity :=
              Complexity + RI_Size_Complexity (Cur_Idx, Max_Size);
            Cur_Idx := Record_Info_Table.Table (Cur_Idx).Next;
         end loop;
      end return;
   end Get_Record_Size_Complexity;

   -------------------------
   -- Record_Field_Offset --
   -------------------------

   function Record_Field_Offset
     (V : GL_Value; Field : Record_Field_Kind_Id) return GL_Value
   is
      F_GT       : GL_Type                           := Full_GL_Type (Field);
      CRC        : constant Opt_Record_Field_Kind_Id :=
        Corresponding_Record_Component (Field);
      Our_Field  : constant Record_Field_Kind_Id     :=
        (if   No (Get_Field_Info (Field)) and then Present (CRC)
              and then Full_Etype (CRC) = Full_Etype (F_GT)
         then CRC else Field);
      Rec_Type   : constant Record_Kind_Id           := Full_Scope (Our_Field);
      Rec_GT     : constant GL_Type                  :=
        Primitive_GL_Type (Rec_Type);
      First_Idx  : constant Record_Info_Id           :=
        Get_Record_Info (Rec_Type);
      F_Idx      : Field_Info_Id                     :=
        Get_Field_Info (Our_Field);
      FI         : Field_Info;
      Our_Idx    : Record_Info_Id;
      Offset     : GL_Value;
      RI         : Record_Info;
      Result     : GL_Value;
      Result_T   : Type_T;

   begin
      --  If the field information isn't present, this must be because
      --  we're referencing a field that's not in this variant and hence is
      --  a constraint error. So return undefined. But first try to see
      --  if we can come up with the right field.

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
      Offset  := To_Bytes (Get_Record_Size_So_Far (Rec_Type, V,
                                                   Start_Idx => First_Idx,
                                                   Idx       => Our_Idx));
      RI      := Record_Info_Table.Table (Our_Idx);

      --  If this is the "_parent" field, just do a conversion so we point
      --  to that type. But add it to the LValue table in case there's
      --  a reference to its discrminant.

      if Chars (Our_Field) = Name_uParent then
         Result := Ptr_To_Ref (V, F_GT);
         Add_To_LValue_List (Result);
         return Result;

      --  If the current piece is for a variable-sized object, we offset
      --  to that object and make a pointer to its type. Otherwise,
      --  make sure we're pointing to Rec_Type.

      elsif Present (RI.GT) then
         pragma Assert (not Is_Bitfield (Field));
         Result := GEP (SSI_GL_Type, Pointer_Cast (V, A_Char_GL_Type),
                        (1 => Offset));
         Set_Alignment (Result, Nat'Min (Alignment (V), Alignment (Offset)));
         return Ptr_To_Ref (Result, F_GT);
      end if;

      --  Get the primitive form of V and make sure that it's not a fat or
      --  thin pointer, but we don't a copy because we'll be indexing into
      --  it (and the copy would be incorrect if we want this offset as an
      --  LValue).

      Result := Get (To_Primitive (V, No_Copy => True), Any_Reference);

      --  Otherwise, if this is not the first piece, we have to offset to
      --  the field (in bytes).

      if Our_Idx /= First_Idx then
         Result := Set_Alignment
           (GEP (SSI_GL_Type, Pointer_Cast (Result, A_Char_GL_Type),
                 (1 => Offset)),
            Nat'Min (Alignment (V), Alignment (Offset)));
      end if;

      --  If the type is not native, we have to convert the pointer to the
      --  type of this piece (which has no corresponding GNAT type).

      if Is_Nonnative_Type (Rec_Type) then
         Result :=
           Ptr_To_Relationship
             (Result, Pointer_Type (RI.LLVM_Type, Address_Space),
              Rec_GT, Reference_To_Unknown);
         Set_Unknown_T (Result, RI.LLVM_Type);
         Result_T := RI.LLVM_Type;
      else
         Result   := Convert_Ref (Result, Rec_GT);
         Result_T := Type_Of (Rec_GT);
      end if;

      --  Finally, do a regular GEP for the field

      Result := GEP_To_Relationship
        (F_GT,
         (if Is_Bitfield (Field) then Reference_To_Unknown else Reference),
         Result, Result_T,
         (1 => Const_Null_32, 2 => Const_Int_32 (FI.Field_Ordinal)));

      --  If we've set this to a an unknown reference, set the type so that
      --  we know what we're pointing to.

      Maybe_Initialize_TBAA_For_Field (Result, Field, F_GT);

      if Is_Bitfield (Field) then
         Set_Unknown_T (Result,
                        Struct_Get_Type_At_Index
                          (Result_T, unsigned (FI.Field_Ordinal)));
      end if;

      return Result;

   end Record_Field_Offset;

   ---------------------------
   -- Emit_Record_Aggregate --
   ---------------------------

   function Emit_Record_Aggregate
     (N : N_Subexpr_Id; Result_So_Far : GL_Value) return GL_Value
   is
      GT   : constant GL_Type := Primitive_GL_Type (Full_GL_Type (N));
      Expr : Opt_N_Component_Association_Id;

   begin
      --  If we can use Data for the result, it means that each of its
      --  components must be just a simple component into an LLVM
      --  structure, so we just go through each of the part of the
      --  aggregate and use the offset for that field, skipping a
      --  discriminant of an unchecked union. If not, we use
      --  Record_Field_Offset to do the reference.

      Expr := First (Component_Associations (N));
      return Result : GL_Value := Result_So_Far do

         --  If we haven't already made a value, do so now. If this is a
         --  loadable type or not of dynamic size and we have a value, we
         --  start with an undef of that type. Otherwise, it's a variable
         --  of that type.

         if No (Result) then
            if (Is_Loadable_Type (GT)
                  or else (not Is_Dynamic_Size (GT)
                             and then Is_No_Elab_Needed (N)))
              and then not Is_Nonnative_Type (GT)
            then
               Result := Get_Undef (GT);
            else
               Result := Allocate_For_Type (GT, N => N);
            end if;
         end if;

         --  Now process each expression

         while Present (Expr) loop
            declare
               In_F : constant Record_Field_Kind_Id :=
                 Entity (First (Choices (Expr)));
               Val  : constant Opt_N_Subexpr_Id     := Expression (Expr);
               V    : GL_Value;
               F    : Record_Field_Kind_Id;

            begin
               if (Ekind (In_F) = E_Discriminant
                     and then Is_Unchecked_Union (GT))
                 or else Decls_Only
               then
                  if Present (Val) then
                     Discard (Emit_Expression (Val));
                  end if;

               elsif Chars (In_F) = Name_uParent then

                  --  If this is "_parent", its fields are our fields too.
                  --  Assume Expression is also an N_Aggregate.

                  pragma Assert
                    (Nkind (Expression (Expr))
                       in N_Aggregate | N_Extension_Aggregate);

                  Result := Emit_Record_Aggregate (Val, Result);

               else
                  --  We are to actually insert the field. However, if we
                  --  haven't set any information for this field, it may be
                  --  a reference to a field that will cause Constraint_Error.
                  --  If so, just don't do anything with it.

                  F := Find_Matching_Field (Full_Etype (GT), In_F);

                  if Present (Get_Field_Info (F)) then
                     V := Emit_Convert_Value (Val, Field_Type (F));
                     V := Build_Field_Store (Result, F, V);

                     if Present (V) then
                        Result := V;
                     end if;
                  else
                     --  Ensure we understand this case

                     pragma Assert (Ekind (GT) = E_Record_Subtype
                                      and then Has_Discriminants (GT)
                                      and then Ekind (F) = E_Component);
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
      In_F       : Record_Field_Kind_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False;
      Prefer_LHS : Boolean  := False;
      VFA        : Boolean  := False) return GL_Value
   is
      R_GT   : constant GL_Type              := Related_Type (In_V);
      R_TE   : constant Record_Kind_Id       :=
        Record_Type_For_Field (R_GT, In_F);
      F      : constant Record_Field_Kind_Id :=
        Find_Matching_Field (R_TE, In_F);
      F_GT   : constant GL_Type              := Field_Type (F);
      V      : GL_Value                      := To_Primitive (In_V);
      Result : GL_Value;

   begin
      --  If V is Volatile_Full_Access, we have to try to load the full record
      --  into memory. If we did, and this is for an LHS, we also need to
      --  set up a writeback.

      if VFA then
         V  := Get (V, Object);

         if Is_Data (V) and For_LHS then
            V := Get (V, Any_Reference);
            Add_Write_Back (In_V, Empty, V);
         end if;
      end if;

      --  If we have something in a data form and we're not requiring or
      --  preferring an LHS, and we have information about the field, we
      --  can and should do this with an Extract_Value.

      if Is_Data (V) and then not For_LHS and then not Prefer_LHS
        and then Present (Get_Field_Info (F))
        and then not Is_Nonnative_Type (F_GT)
        and then not Is_Nonnative_Type (R_TE)
        and then (not Is_Array_Bitfield (F)
                    or else (Is_Nonsymbolic_Constant (V)
                               and then not Is_Large_Array_Bitfield (F)))
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

      elsif Known_Esize (F) and then Esize (F) = 0 then
         return (if    Is_Elementary_Type (F_GT) then Const_Null (F_GT)
                 elsif Is_Loadable_Type (F_GT)   then Get_Undef (F_GT)
                 else  Get_Undef_Ref (F_GT));

      --  If we have a bitfield, we need special processing. Because we have
      --  a lot of intermediate values that don't correspond to Ada types,
      --  we do some low-level processing here when we can't avoid it.

      elsif Is_Bitfield (F) then

         --  If this is a bitfield array type, we need to pointer-pun or
         --  convert it to an integral type that's the width of the
         --  bitfield field type.

         if Is_Array_Bitfield (F) then
            if Relationship (Result) = Unknown then
               declare
                  T : constant Type_T := Int_Ty (Get_Type_Size (Result));

               begin
                  Result := G (Convert_Aggregate_Constant (+Result, T),
                               F_GT, Unknown);
               end;
            else
               declare
                  T     : constant Type_T := Element_Type_Of (Result);
                  New_T : constant Type_T := Int_Ty (Get_Scalar_Bit_Size (T));

               begin
                  Result :=
                    Ptr_To_Relationship
                      (Result,
                       Pointer_Type (New_T, Address_Space),
                       Reference_To_Unknown);
                  Set_Unknown_T (Result, New_T);
               end;
            end if;
         end if;

         --  Next, do the extraction using two shifts

         declare
            type Opf is access function
              (V, Count : GL_Value; Name : String := "") return GL_Value;

            Loaded    : constant GL_Value := Get (Result, Unknown);
            T         : constant Type_T   := Type_Of (Loaded);
            Result_T  : constant Type_T   := Type_Of (F_GT);
            First_Bit : constant ULL      := +Field_Bit_Offset (F);
            Num_Bits  : constant ULL      := +Esize (F);
            Val_Width : constant ULL      := Get_Scalar_Bit_Size (T);
            Uns       : constant Boolean  :=
              Is_Unsigned_For_RM (F_GT)
              or else not Is_Discrete_Or_Fixed_Point_Type (F_GT);
            Shl_Count : constant ULL      := Val_Width - First_Bit - Num_Bits;
            Shr_Count : constant ULL      := Val_Width - Num_Bits;
            Shr       : constant Opf      :=
              (if Uns then L_Shr'Access else A_Shr'Access);
            Res_Width : constant ULL      :=
                +Align_To (GT_Size (F_GT), 1, BPU);

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

            --  Otherwise, truncate to the corresponding bit size if needed

            else

               if Val_Width /= Res_Width then
                  Result := Trunc_To_Relationship (Result, Int_Ty (Res_Width),
                                                   Unknown);
               end if;

               --  For a floating-point type we perform a bit cast

               if Is_Floating_Point_Type (F_GT) then
                  return Bit_Cast (Result, F_GT);

               --  For other types, we have to put into memory via pointer
               --  punning.

               else
                  declare
                     Memory         : constant GL_Value :=
                       (if   Present (LHS) then LHS
                        else Allocate_For_Type (F_GT));
                     Mem_As_Int_Ptr : GL_Value          :=
                       Ptr_To_Relationship
                         (Memory,
                          Pointer_Type
                            (Type_Of (Result),
                             Address_Space),
                          F_GT, Reference_To_Unknown);

                  begin
                     Set_Unknown_T (Mem_As_Int_Ptr, Type_Of (Result));
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

   ------------------
   -- Field_To_Use --
   ------------------

   function Field_To_Use
     (LHS : GL_Value; F : Record_Field_Kind_Id) return Record_Field_Kind_Id
   is
      GT      : constant GL_Type        := Related_Type (LHS);
      TE      : constant Record_Kind_Id := Record_Type_For_Field (GT, F);

   begin
      --  Ensure TE is elaborated since we may need info about this field

      Discard (Type_Of (TE));
      return Find_Matching_Field (TE, F);
   end Field_To_Use;

   -----------------------
   -- Build_Field_Store --
   -----------------------

   function Build_Field_Store
     (In_LHS : GL_Value;
      In_F   : Record_Field_Kind_Id;
      RHS    : GL_Value;
      VFA    : Boolean := False) return GL_Value
   is
      R_GT      : constant GL_Type              := Related_Type (In_LHS);
      R_TE      : constant Record_Kind_Id       :=
        Record_Type_For_Field (R_GT, In_F);
      F         : constant Record_Field_Kind_Id :=
        Find_Matching_Field (R_TE, In_F);
      F_Idx     : constant Field_Info_Id        := Get_Field_Info (F);
      FI        : constant Field_Info           :=
        Field_Info_Table.Table (F_Idx);
      F_GT      : constant GL_Type              := FI.GT;
      Idx       : constant Nat                  := FI.Field_Ordinal;
      LHS       : GL_Value                      := In_LHS;
      RHS_Cvt   : GL_Value                      := Convert_GT (RHS, F_GT);
      Result    : GL_Value                      := No_GL_Value;
      First_Bit : ULL;
      Num_Bits  : ULL;

   begin
      --  First check for the trivial case of a zero-length field

      if Known_Esize (F) and then Esize (F) = 0 then
         return (if Is_Data (LHS) then LHS else No_GL_Value);

      --  If this is for a volatile full access object, load that object

      elsif VFA and then not Is_Data (LHS) then
         LHS := Get (To_Primitive (LHS), Object);
      end if;

      --  Handle the cases where F isn't a bitfield. If we're dealing with
      --  data, just insert the data. Otherwise, emit an assignment. We
      --  convert RHS to the type of the field except in the case where the
      --  field is an unconstrained record. In that case, the size of the
      --  field shouldn't be used for the copy since it's the maximum size
      --  of the record and hence almost always larger than the amount of
      --  data to be copied.

      if not Is_Bitfield (F) then
         if Is_Data (LHS) and then not Is_Nonnative_Type (R_TE) then
            Result := Insert_Value (LHS, Get (RHS_Cvt, Data), unsigned (Idx));
         else
            Emit_Assignment (Record_Field_Offset (LHS, F), Empty,
                             (if   Is_Unconstrained_Record (F_GT) then RHS
                              else Convert_GT (RHS, F_GT)));
         end if;

         return Result;
      end if;

      --  Now we handle the bitfield case. Like the load case, we do our
      --  masking and shifting operations in an integral type. This is
      --  either the type of the field or an integral type of the same
      --  width as the array used for the field.
      --
      --  We need to get both the field within LHS that contains F into this
      --  integral type (if it isn't already) and RHS. Then we perform the
      --  masking and store the data back.

      declare
         LHS_For_Access : constant GL_Value :=
           (if   Is_Array_Bitfield (F) and then Is_Data (LHS)
                 and then (not Is_Nonsymbolic_Constant (LHS)
                             or else not Is_Nonsymbolic_Constant (RHS_Cvt)
                             or else Is_Large_Array_Bitfield (F))
            then Allocate_For_Type (R_GT, V => LHS) else LHS);
         RHS_GT         : constant GL_Type  := Related_Type (RHS_Cvt);
         RHS_T          : Type_T            := Type_Of (RHS_GT);
         RHS_Width      : constant ULL      := Get_Scalar_Bit_Size (RHS_T);
         Data_LHS       : GL_Value          := No_GL_Value;
         Rec_Data       : GL_Value;
         Orig_Data_T    : Type_T;
         Data_T         : Type_T;
         New_RHS_T      : Type_T;
         Data_Width     : ULL;
         Count          : GL_Value;
         Mask           : GL_Value;

      begin
         --  We first have to form an access to the appropriate field, either
         --  a reference or actual data. The code above has ensured that
         --  we'll have a reference in the case of an array bitfield in
         --  the non-constant case.

         if Is_Data (LHS_For_Access) and then not Is_Nonnative_Type (R_TE) then
            Rec_Data := Extract_Value_To_Relationship
              (F_GT, LHS_For_Access, Field_Ordinal (F), Unknown);
         else
            Data_LHS := Record_Field_Offset (LHS_For_Access, F);
            Rec_Data := Data_LHS;
         end if;

         --  If this is a bitfield array type, we need to pointer-pun or
         --  convert it to an integral type that's the width of the
         --  bitfield field type. We've forced Rec_Data to be a reference
         --  in this case.

         if Is_Array_Bitfield (F) then
            if Is_Data (LHS_For_Access) then
               Orig_Data_T := Type_Of (Rec_Data);
               Rec_Data    := G (Convert_Aggregate_Constant
                                   (+Rec_Data,
                                    Int_Ty (Get_Type_Size (Rec_Data))),
                                 F_GT, Unknown);
            else
               declare
                  T     : constant Type_T := Element_Type_Of (Rec_Data);
                  New_T : constant Type_T := Int_Ty (Get_Type_Size (T));

               begin
                  Data_LHS :=
                    Ptr_To_Relationship
                      (Data_LHS,
                       Pointer_Type (New_T, Address_Space),
                       Reference_To_Unknown);
                  Set_Unknown_T (Data_LHS, New_T);
                  Rec_Data := Data_LHS;
               end;
            end if;
         end if;

         --  Now get the field contents as actual data and get and verify
         --  its type.

         Rec_Data   := Get (Rec_Data, Unknown);
         Data_T     := Type_Of (Rec_Data);
         Data_Width := Get_Scalar_Bit_Size (Data_T);
         pragma Assert (Get_Type_Kind (Data_T) = Integer_Type_Kind);

         --  Our next step is to get RHS into the same type as the
         --  record data. We have the same three cases as field load,
         --  but here we want to start with an integral value the same
         --  width as the converted input.

         New_RHS_T := Int_Ty (Nat (RHS_Width));

         if Is_Floating_Point_Type (RHS_Cvt) then
            RHS_Cvt := Bit_Cast_To_Relationship (Get (RHS_Cvt, Data),
                                                 New_RHS_T, Unknown);
         elsif Get_Type_Kind (RHS_T) /= Integer_Type_Kind then
            if Is_Nonsymbolic_Constant (RHS_Cvt) then
               RHS_Cvt := G (Convert_Aggregate_Constant (+RHS, New_RHS_T),
                             Related_Type (RHS_Cvt), Unknown);
            else
               RHS_Cvt := Get (RHS_Cvt, Reference);
               RHS_Cvt :=
                 Ptr_To_Relationship
                   (RHS_Cvt,
                    Pointer_Type (New_RHS_T, Address_Space),
                    Reference_To_Unknown);
               Set_Unknown_T (RHS_Cvt, New_RHS_T);
               RHS_Cvt := Load (RHS_Cvt);
            end if;
         else
            RHS_Cvt := Get (RHS_Cvt, Data);
         end if;

         --  Next, we do shifts, masks, and a logical "or" to compute the
         --  new value of the field.

         RHS_T     := Type_Of (RHS_Cvt);
         First_Bit := +Field_Bit_Offset (F);
         Num_Bits  := +Esize (F);

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
            RHS_Cvt := Z_Ext_To_Relationship (RHS_Cvt, Data_T, Unknown);
         elsif RHS_Width > Data_Width then
            RHS_Cvt := Trunc_To_Relationship (RHS_Cvt, Data_T, Unknown);
         end if;

         --  Now form the mask, remove the old value, and insert the new value

         Count    := G (Const_Int (Data_T, First_Bit, False), F_GT, Unknown);
         Mask     := G (Const_Int (Data_T, ULL'Last, True), F_GT, Unknown);
         Mask     := L_Shr (Mask, G (Const_Int (Data_T, Data_Width - Num_Bits,
                                             False),
                                 F_GT, Unknown));
         Mask     := Build_Not (Shl (Mask, Count));
         RHS_Cvt  := Shl (RHS_Cvt, Count);
         Rec_Data := Build_Or (Build_And (Rec_Data, Mask), RHS_Cvt);

         --  If we're still working with data, then insert the new value into
         --  the field. Otherwise, store it where it belongs. In the array
         --  bitfield case, we have to convert the constant back.

         if Is_Data (LHS_For_Access) then
            if Is_Array_Bitfield (F) then
               Rec_Data := G (Convert_Aggregate_Constant
                                (+Rec_Data, Orig_Data_T),
                              F_GT, Unknown);
            end if;

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

   -----------------------
   -- Build_Field_Store --
   -----------------------

   procedure Build_Field_Store
     (LHS  : GL_Value;
      In_F : Record_Field_Kind_Id;
      RHS  : GL_Value;
      VFA  : Boolean := False)
   is
      Result : constant GL_Value := Build_Field_Store (LHS, In_F, RHS, VFA);

   begin
      --  If we have a value, copy it back into LHS

      if Present (Result) then
         Emit_Assignment (LHS, Value => Result);
      end if;
   end Build_Field_Store;

   --------------------
   -- Add_Write_Back --
   --------------------

   procedure Add_Write_Back
     (LHS : GL_Value; F : Opt_Record_Field_Kind_Id; RHS : GL_Value) is
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
            if Present (WB.F) then
               Discard (Build_Field_Store (WB.LHS, WB.F, WB.RHS));
            else
               Emit_Assignment (WB.LHS, Value => WB.RHS);
            end if;
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

   procedure Print_Field_Info (E : Record_Field_Kind_Id) is
      F_Idx : constant Field_Info_Id  := Get_Field_Info (E);
      FI    : Field_Info;

   begin
      Push_Output;
      Set_Standard_Error;
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

         if Present (FI.First_Bit) then
            Write_Str (", Bits = ");
            Write_Int (+FI.First_Bit);
            Write_Str (" .. ");
            Write_Int (+(FI.First_Bit + FI.Num_Bits) - 1);
         end if;

         if FI.Array_Bitfield then
            Write_Str (", Array Bitfield");
         end if;

         Write_Str (": ");
         Dump_GL_Type (FI.GT);
         Write_Eol;
         Print_RI_Briefly (FI.Rec_Info_Idx);
      end if;

      Pop_Output;
   end Print_Field_Info;

   -----------------------
   -- Print_Record_Info --
   -----------------------

   procedure Print_Record_Info (TE : Record_Kind_Id; Eol : Boolean := False) is

   begin
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

            if RI.Unused_Bits /= 0 then
               Write_Str (" unused bits ");
               Write_Int (+RI.Unused_Bits);
            end if;

            Write_Eol;

            if Present (RI.GT) then
               Write_Str (Prefix);
               Dump_GL_Type (RI.GT);
            elsif Present (RI.LLVM_Type) then
               Dump_LLVM_Type (RI.LLVM_Type);
            end if;

            F_Idx := RI.First_Field;
            while Present (F_Idx) loop
               FI := Field_Info_Table.Table (F_Idx);
               Write_Str (Prefix);
               Write_Str ("    Field");

               if Present (RI.LLVM_Type) then
                  Write_Str ("@");
                  Write_Int (FI.Field_Ordinal);

                  if Present (FI.First_Bit) then
                     Write_Str ("[");
                     Write_Int (+FI.First_Bit);
                     Write_Str (" .. ");
                     Write_Int (+(FI.First_Bit + FI.Num_Bits) - 1);
                     Write_Str ("]");
                  end if;
               end if;

               Write_Str (" ");
               Write_Int (Nat (FI.Field));
               Write_Str (": ");
               Sprint_Node (FI.Field);
               Write_Eol;
               F_Idx := FI.Next;
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
         Push_Output;
         Set_Standard_Error;
         Print_RI_Chain (Get_Record_Info (TE));

         if Eol then
            Write_Eol;
         end if;

         Pop_Output;
      end;

   end Print_Record_Info;

end GNATLLVM.Records;
