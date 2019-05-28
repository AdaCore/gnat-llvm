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

with Errout; use Errout;
with Sinfo;  use Sinfo;
with Snames; use Snames;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Arrays;    use GNATLLVM.Arrays;
with GNATLLVM.Blocks;    use GNATLLVM.Blocks;
with GNATLLVM.Compile;   use GNATLLVM.Compile;
with GNATLLVM.Exprs;     use GNATLLVM.Exprs;
with GNATLLVM.Types;     use GNATLLVM.Types;
with GNATLLVM.Variables; use GNATLLVM.Variables;

package body GNATLLVM.Conversions is

   function Are_Arrays_With_Different_Index_Types
     (GT1, GT2 : GL_Type) return Boolean
     with Pre => Present (GT1) and then Present (GT2);
   --  Return True iff GT1 and GT2 are array types that have at least
   --  one index for whose LLVM types are different.  GT1 must be
   --  unconstrained.

   function Is_In_LHS_Context (N : Node_Id) return Boolean;
   --  Return True if N's parent (if N is Present) is such that we need a
   --  LValue.

   function Is_Nop_Conversion (V : GL_Value; GT : GL_Type) return Boolean
     with Pre => Is_Reference (V) and then Present (GT);
   --  Return True if converting V to type GT won't change any bits

   -----------------------------------------------
   -- Are_Arrays_With_Different_Index_Types --
   -----------------------------------------------

   function Are_Arrays_With_Different_Index_Types
     (GT1, GT2 : GL_Type) return Boolean
   is
      Idx1, Idx2 : Entity_Id;

   begin

      --  The front end should not have gotten us here if the number
      --  of dimensions differ.

      pragma Assert (Number_Dimensions (GT1) = Number_Dimensions (GT2));

      --  We don't need to do anything if the index types differ unless the
      --  corresponding LLVM types differ, so that's all we check.

      if Ekind (GT2) = E_String_Literal_Subtype then
         return (Type_Of (Full_Etype (First_Index (GT1)))
                   /= Type_Of (Integer_GL_Type));
      end if;

      Idx1 := First_Index (GT1);
      Idx2 := First_Index (GT2);
      while Present (Idx1) loop
         exit when
           Type_Of (Full_Base_Type (Full_Etype (Idx1))) /=
           Type_Of (Full_Base_Type (Full_Etype (Idx2)));
         Next_Index (Idx1);
         Next_Index (Idx2);
      end loop;

      return Present (Idx1);
   end Are_Arrays_With_Different_Index_Types;

   -----------------------
   -- Is_Nop_Conversion --
   -----------------------

   function Is_Nop_Conversion (V : GL_Value; GT : GL_Type) return Boolean is
   begin
      --  This is a no-op if the two LLVM types are the same or if both
      --  GNAT types aren't scalar types.

      return Type_Of (V) = Pointer_Type (Type_Of (GT), 0)
        or else (not Is_Scalar_Type (GT)
                   and then not Is_Scalar_Type (Related_Type (V)));

   end Is_Nop_Conversion;

   -----------------------------
   -- Is_Unsigned_For_Convert --
   -----------------------------

   function Is_Unsigned_For_Convert (GT : GL_Type) return Boolean is
      BT : constant GL_Type := Base_GL_Type (GT);

   begin
      --  If GT is narrower than BT, use its signedness, otherwise use BT's

      return (if   ULL'(Get_Type_Size_In_Bits (Type_Of (GT))) <
                   ULL'(Get_Type_Size_In_Bits (Type_Of (BT)))
              then Is_Unsigned_Type (GT)
              else Is_Unsigned_Type (BT));
   end Is_Unsigned_For_Convert;

   ------------------------
   -- Is_Unsigned_For_RM --
   ------------------------

   function Is_Unsigned_For_RM (GT : GL_Type) return Boolean is
   begin
      --  If biased, say yes

      if Is_Biased_GL_Type (GT) then
         return True;

      --  If not scalar type or no range, say no; if unsigned say yes.

      elsif not Is_Scalar_Type (GT) or else No (Scalar_Range (GT)) then
         return False;
      elsif Is_Unsigned_Type (GT) then
         return True;

      --  Otherwise it's unsigned iff the low bound is known to be
      --  nonnegative.

      elsif Is_No_Elab_Needed (Type_Low_Bound (GT)) then
         declare
            LB : constant GL_Value := Emit_Expression (Type_Low_Bound (GT));

         begin
            return Is_A_Const_Int (LB) and then Get_Const_Int_Value (LB) >= 0;
         end;

      else
         return False;
      end if;

   end Is_Unsigned_For_RM;

   -----------------------
   -- Is_In_LHS_Context --
   -----------------------

   function Is_In_LHS_Context (N : Node_Id) return Boolean is
   begin
      if No (N) or No (Parent (N)) then
         return False;
      end if;

      --  The only cases that are LValue contexts are the LHS of an
      --  assignment statement or a parameter to a subprogram call.

      case Nkind (Parent (N)) is
         when N_Assignment_Statement =>
            return N = Name (Parent (N));

         when N_Attribute_Reference =>
            case Get_Attribute_Id (Attribute_Name (Parent (N))) is

               when Attribute_Access
                 | Attribute_Unchecked_Access
                 | Attribute_Unrestricted_Access
                 | Attribute_Address
                 | Attribute_Pool_Address =>
                  return True;

               when others =>
                  return False;
            end case;

         when N_Procedure_Call_Statement | N_Function_Call =>

            --  We treat any operation that's an actual as if it's an LHS
            --  even though this is a bit conservative.

            return N /= Name (Parent (N));

         when N_Parameter_Association =>
            return True;

         when others =>
            return False;

      end case;

   end Is_In_LHS_Context;

   ------------------
   -- Is_Parent_Of --
   ------------------

   function Is_Parent_Of (T_Need, T_Have : Entity_Id) return Boolean is
      BT_Need : constant Entity_Id := Full_Base_Type (T_Need);
      BT_Have : constant Entity_Id := Full_Base_Type (T_Have);

   begin
      --  If the two types are the same return True.  Likewise if
      --  T_Have has a parent different than itself and that and this
      --  relation holds for that.

      return BT_Need = BT_Have
        or else (Full_Etype (BT_Have) /= BT_Have
                   and then Is_Parent_Of (BT_Need, Full_Etype (BT_Have)))
        or else (Present (Parent_Subtype (BT_Have))
                   and then Is_Parent_Of (BT_Need,
                                          Full_Parent_Subtype (BT_Have)));

   end Is_Parent_Of;

   ---------------------
   -- Emit_Conversion --
   ---------------------

   function Emit_Conversion
     (N                   : Node_Id;
      GT                  : GL_Type;
      From_N              : Node_Id := Empty;
      For_LHS             : Boolean := False;
      Is_Unchecked        : Boolean := False;
      Need_Overflow_Check : Boolean := False;
      Float_Truncate      : Boolean := False;
      No_Truncation       : Boolean := False) return GL_Value
   is
      Result      : GL_Value                 := Emit (N, For_LHS => For_LHS);
      Orig_Result : constant GL_Value        := Result;
      Prim_GT     : constant GL_Type         := Primitive_GL_Type (GT);
      In_GT       : constant GL_Type         := Related_Type (Result);
      R           : constant GL_Relationship := Relationship (Result);
      GT_Uns      : constant Boolean         := Is_Unsigned_For_RM (GT);
      In_GT_Uns   : constant Boolean         := Is_Unsigned_For_RM (In_GT);

   begin
      --  We have to be careful here.  There isn't as clear a distinction
      --  between unchecked conversion and regular conversion as we might
      --  like.  Both the front-end and RTS have code of the form "Type
      --  (LValue)'Unrestricted_Access" and expect this to produce a
      --  reference to the address of LValue.  This code is called in both
      --  the LValue and value case.  If we're starting with a reference,
      --  we want to keep it as a reference unless we're sure that this
      --  needs an actual conversion.
      --
      --  We test two things: first, we see if we're being used in a
      --  context where an LValue is definitely or likely needed and also
      --  look for a conversion that won't actually change any bits.  If
      --  both, do this as an unchecked conversion.  On the other hand, if
      --  an overflow check is required, we know this is NOT an unchecked
      --  conversion.

      --  First, if we're converting between two access subprogram access
      --  types and one is a foreign convention and one isn't, issue a
      --  warning since that can cause issues with nested subprograms.

      if Is_Access_Subprogram_Type (GT)
        and then Is_Access_Subprogram_Type (In_GT)
        and then Has_Foreign_Convention (GT) /= Has_Foreign_Convention (In_GT)
      then
         Error_Msg_Node_1 := Full_Etype (In_GT);
         Error_Msg_Node_2 := Full_Etype (GT);
         Error_Msg
           ("??conversion between subprogram access types of different",
            Sloc (N));
         Error_Msg
           ("\conventions, & and &, will not work if the former points ",
            Sloc (N));
         Error_Msg
           ("\to a subprogram that references parent variables.", Sloc (N));
      end if;

      --  If we're converting to an elementary type and need an overflow
      --  check, do that.

      if Is_Elementary_Type (GT) and then Need_Overflow_Check then
         Result := To_Primitive (Get (Result, Data));
         Emit_Overflow_Check (Result, From_N);
         Result := Convert (Result, GT,
                            Float_Truncate => Float_Truncate,
                            Is_Unchecked   => Is_Unchecked);

      --  If we have a reference in a LHS context and the conversion won't
      --  do anything, just convert the pointer.

      elsif Is_Reference (Result) and then Is_In_LHS_Context (From_N)
        and then Is_Nop_Conversion (Result, GT)
      then
         Result := Convert_Ref (Get (Result, Any_Reference), GT);

      --  For unchecked conversion between pointer and integer, just copy
      --  the bits.  But use Size_Type and generic pointers to make sure
      --  that any size changes are taken into account (they shouldn't be
      --  because of the rules of UC, but let's be conservative).

      elsif Is_Unchecked and then Is_Access_Type (In_GT)
        and then Is_Discrete_Or_Fixed_Point_Type (GT)
      then
         Result := Get (From_Access (Get (Result, Data)),
                        Reference_For_Integer);
         Result := Convert (Ptr_To_Int (Result, Size_GL_Type), GT);
      elsif Is_Unchecked and then Is_Discrete_Or_Fixed_Point_Type (In_GT)
        and then Is_Access_Type (GT)
      then
         --  If GT is an access to unconstrained, this means that the
         --  address is to be taken as a thin pointer.  We also need special
         --  code in the case of access to subprogram.

         if Is_Unconstrained_Array (Full_Designated_Type (GT)) then
            Result :=
              Int_To_Relationship (Get (Result, Data),
                                   Full_Designated_GL_Type (GT), Thin_Pointer);
         elsif Ekind (GT) = E_Access_Subprogram_Type then
            Result := Int_To_Relationship (Get (Result, Data),
                                           Full_Designated_GL_Type (GT),
                                           Reference);
         else
            Result := Int_To_Ref (Get (Result, Data), SSI_GL_Type);
         end if;

         Result := Convert_To_Access (Result, GT);

      --  We can unchecked convert floating point of the same width
      --  (the only way that UC is formally defined) with a "bitcast"
      --  instruction.

      elsif Is_Unchecked
        and then ((Is_Floating_Point_Type (GT)
                     and then Is_Discrete_Or_Fixed_Point_Type (In_GT))
                  or else (Is_Discrete_Or_Fixed_Point_Type (GT)
                             and then Is_Floating_Point_Type (In_GT)))
        and then (ULL'(Get_Type_Size_In_Bits (Type_Of (GT))) =
                    ULL'(Get_Type_Size_In_Bits (Type_Of (In_GT))))
      then
         Result := Bit_Cast (Get (Result, Data), GT);

      --  If both types are elementary, hand that off to our helper, but
      --  raise a Constraint_Error if this conversion overflowed by producing
      --  an undef.

      elsif Is_Elementary_Type (In_GT)
        and then Is_Elementary_Type (GT)
      then
         Result := Convert (Get (Result, Data), GT,
                            Float_Truncate => Float_Truncate,
                            Is_Unchecked   => Is_Unchecked);
         if Is_Undef (Result) and then not Is_Undef (Orig_Result) then
            Error_Msg_N ("?`Constraint_Error` will be raised at run time",
                         From_N);
            Emit_Raise_Call (From_N, CE_Overflow_Check_Failed);
         end if;

      --  Otherwise, convert to the primitive type, do any require
      --  conversion (as an unchecked conversion, meaning pointer
      --  punning or equivalent) and then convert to the result type.
      --  Some of these operations will likely be nops.

      else
         Result := To_Primitive (Result);

         --  If both types are the same, just change the type of the result.
         --  Avoid confusing [0 x T] as both a zero-size constrained type and
         --  the type used for a variable-sized type.

         if Is_Data (Result) and then not Is_Nonnative_Type (Prim_GT)
           and then Type_Of (Result) = Type_Of (Prim_GT)
         then
            Result := G_Is (Result, Prim_GT);

         --  If we have an undefined value that we're converting to another
         --  type, just get an undefined value of that type.  But watch for
         --  the case where we have Data of some fixed-size type and we're
         --  converting to a dynamic-sized type.  We handle the reference
         --  cases below since we may have to deal with materializing
         --  bounds.

         elsif Is_Undef (Result) and then R = Data
           and then Is_Loadable_Type (Prim_GT)
         then
            Result := Get_Undef (Prim_GT);

         --  If we have a constant of a struct type that we're converting
         --  to a struct of the same layout, we can make a new constant.

         elsif Is_Data (Result) and then Is_Constant (Result)
           and then Get_Type_Kind (Type_Of (Result)) = Struct_Type_Kind
           and then Is_Record_Type (Prim_GT)
           and then not Is_Nonnative_Type (Prim_GT)
           and then Is_Layout_Identical (Result, Prim_GT)
         then
            Result := Convert_Struct_Constant (Result, Prim_GT);

         --  Otherwise, do an actual pointer pun

         else
            Result := Convert_Ref (Get (Result, Any_Reference), Prim_GT);
         end if;

         Result := From_Primitive (Result, GT);
      end if;

      --  For unchecked conversion, if the result is a non-biased
      --  integral type whose precision is not equal to its size, sign-
      --  or zero-extend the result.  But we need not do this if the
      --  input is also an integral type and both are unsigned or both
      --  are signed and the output is not narrower than the input and
      --  we can't do this in the case of nonbinary modulus.  We can't do
      --  this for modular integer types since LLVM already did it and
      --  we'll generate bad shifts if we try to do it again.

      if Is_Unchecked and then not No_Truncation
        and then Is_Discrete_Or_Fixed_Point_Type (GT)
        and then not Is_Modular_Integer_Type (GT)
        and then not Non_Binary_Modulus (GT)
        and then RM_Size (GT) < Esize (GT)
        and then not (Is_Discrete_Or_Fixed_Point_Type (In_GT)
                        and then GT_Uns = In_GT_Uns
                        and then (GT_Uns
                                    or else RM_Size (GT) = RM_Size (In_GT)))
      then
         declare
            Prim_GT     : constant GL_Type  := Primitive_GL_Type (GT);
            Shift_Count : constant GL_Value :=
              Const_Int (Prim_GT, Esize (Prim_GT) - RM_Size (Prim_GT));
            Left_Shift  : constant GL_Value :=
              Shl (Convert (Get (Result, Data), Prim_GT), Shift_Count);

         begin
            Result := From_Primitive ((if   Is_Unsigned_Type (Prim_GT)
                                       then L_Shr (Left_Shift, Shift_Count)
                                       else A_Shr (Left_Shift, Shift_Count)),
                                      GT);
         end;
      end if;

      return Result;

   end Emit_Conversion;

   -------------
   -- Convert --
   -------------

   function Convert
     (V              : GL_Value;
      GT             : GL_Type;
      Float_Truncate : Boolean := False;
      Is_Unchecked   : Boolean := False) return GL_Value
   is
      type Cvtf is access function
        (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value;

      In_V        : constant GL_Value :=
        (if   Is_Unchecked and then not Has_Padding (V) then V
         else To_Primitive (V));
      In_GT       : constant GL_Type  := Related_Type (In_V);
      Is_Unc_Bias : constant Boolean  :=
        Is_Unchecked and then (Is_Biased_GL_Type (GT)
                                 or else Is_Biased_GL_Type (In_GT));
      Prim_GT     : constant GL_Type  :=
        (if   Is_Unchecked and then not Has_Padding (GT) then GT
         else Primitive_GL_Type (GT));
      Value       : GL_Value          := In_V;
      Src_Access  : constant Boolean  := Is_Access_Type (V);
      Dest_Access : constant Boolean  := Is_Access_Type (Prim_GT);
      Src_FP      : constant Boolean  := Is_Floating_Point_Type (V);
      Dest_FP     : constant Boolean  := Is_Floating_Point_Type (Prim_GT);
      Src_Uns     : constant Boolean  := Is_Unsigned_For_Convert (In_GT);
      Dest_Uns    : constant Boolean  := Is_Unsigned_For_Convert (Prim_GT);
      Src_Size    : constant Nat      :=
        Nat (ULL'(Get_Type_Size_In_Bits (In_V)));
      Dest_Usize  : constant Uint     :=
        (if   Is_Modular_Integer_Type (Prim_GT) then RM_Size (Prim_GT)
         else Esize (Prim_GT));
      Dest_Size   : constant Nat      := UI_To_Int (Dest_Usize);
      Is_Trunc    : constant Boolean  := Dest_Size < Src_Size;
      Subp        : Cvtf              := null;
      Result      : GL_Value;

   begin
      --  If the input is undefined, so is our output

      if Is_Undef (In_V) then
         return Get_Undef (GT);

      --  If the value is already of the desired LLVM type, we're done
      --  unless one type is biased.

      elsif Type_Of (In_V) = Type_Of (GT) and then not Is_Biased_GL_Type (In_V)
        and then not Is_Biased_GL_Type (GT)
      then
         return G_Is (In_V, GT);

      --  If we're converting between two GL_Types corresponding to the same
      --  GNAT type, convert to the primitive type and the to the desired
      --  GL_Type (one of those will likely be a nop).  Don't do this if
      --  we have a UC to or from a biased type.

      elsif Full_Etype (In_GT) = Full_Etype (GT) and then not Is_Unc_Bias then
         return From_Primitive (In_V, GT);

      --  If converting pointer to/from integer, copy the bits using the
      --  appropriate instruction.

      elsif Dest_Access and then Is_Integer_Type (In_V) then
         Subp := Int_To_Ptr'Access;
      elsif Is_Integer_Type (GT) and then Src_Access then
         Subp := Ptr_To_Int'Access;

      --  For pointer to pointer, call our helper

      elsif Src_Access and then Dest_Access then
         return Convert_To_Access (Value, GT);

      --  Having dealt with pointers, we have four cases: FP to FP, FP to
      --  Int, Int to FP, and Int to Int.  We already know that this isn't
      --  a noop case because we've checked above for the same type.

      elsif Src_FP and then Dest_FP then
         Subp := (if Is_Trunc then FP_Trunc'Access else FP_Ext'Access);

      elsif Src_FP and then not Dest_FP then
         Subp := (if Dest_Uns then FP_To_UI'Access else FP_To_SI'Access);

         if not Float_Truncate then

            --  In the FP to Integer case, the LLVM instructions round to
            --  zero, but the Ada semantics round away from zero, so we have
            --  to adjust the input.  We first compute Type'Pred (0.5).  If
            --  the input is strictly negative, subtract this value and
            --  otherwise add it from the input.  For 0.5, the result is
            --  exactly between 1.0 and the machine number preceding 1.0.
            --  Since the last bit of 1.0 is even, this 0.5 will round to 1.0,
            --  while all other number with an absolute value less than 0.5
            --  round to 0.0.  For larger numbers exactly halfway between
            --  integers, rounding will always be correct as the true
            --  mathematical result will be closer to the higher integer
            --  compared to the lower one.  So, this constant works for all
            --  floating-point numbers.  We compute this using an LLVM
            --  function ("next") that operates on an APFloat and returns
            --  either a value epsilon higher or lower than the original.
            --
            --  The reason to use the same constant with subtract/add instead
            --  of a positive and negative constant is to allow the comparison
            --  to be scheduled in parallel with retrieval of the constant and
            --  conversion of the input to the calc_type (if necessary).

            declare
               Half       : constant GL_Value :=
                 Pred_FP (Const_Real (In_V, 0.5));
               Val_Is_Neg : constant GL_Value :=
                 F_Cmp (Real_OLT, In_V, Const_Null (In_V));
               Add_Amt    : constant GL_Value := F_Add (In_V, Half, "round");
               Sub_Amt    : constant GL_Value := F_Sub (In_V, Half, "round");

            begin
               Value := Build_Select (Val_Is_Neg, Sub_Amt, Add_Amt);
            end;
         end if;

      elsif not Src_FP and then Dest_FP then
         Subp := (if Src_Uns then UI_To_FP'Access else SI_To_FP'Access);

      --  Remaining case is integer to integer

      elsif Is_Trunc then
         Subp := Trunc'Access;
      else
         Subp := (if Src_Uns then Z_Ext'Access else S_Ext'Access);
      end if;

      --  Here all that's left to do is deal with non-primitive types and
      --  generate the IR instruction.

      Result := Subp (Value, Prim_GT);
      if Related_Type (Result) /= GT then
         Result := From_Primitive (Result, GT);
      end if;

      return Result;

   end Convert;

   -----------------------
   -- Convert_To_Access --
   -----------------------

   function Convert_To_Access (V : GL_Value; GT : GL_Type) return GL_Value is
      DT     : constant GL_Type         := Full_Designated_GL_Type (GT);
      In_R   : constant GL_Relationship := Relationship (V);
      In_GT  : constant GL_Type         := Related_Type (V);
      As_Ref : constant GL_Value        :=
        (if   Is_Data (V) and then Is_Access_Type (In_GT)
         then From_Access (V) else V);
      R      : constant GL_Relationship := Relationship_For_Access_Type (GT);
      Result : GL_Value;

   begin
      --  The normal process is to convert the input from an access type,
      --  if that's what it is, to a reference to the designated type.
      --  Then we convert it to the relationship to that type that's reflected
      --  by the result type and finally we actually convert the pointer
      --  to the destination type.
      --
      --  One case that has to be handled specially is converting to an
      --  access-to-subprogram type because if we have a
      --  Reference_To_Subprogram, we can't convert to Reference if it's a
      --  procedure because the type is Void, so we have to handle that
      --  specially here.

      if In_R = Reference_To_Subprogram
        and then Ekind (GT) = E_Access_Subprogram_Type
      then
         Result := Get (Ptr_To_Relationship (As_Ref, DT, Reference), R);
      else
         Result := Convert_Pointer (Get (As_Ref, R), DT);
      end if;

      return To_Access (Result, GT);
   end Convert_To_Access;

   ----------------
   -- Convert_GT --
   ----------------

   function Convert_GT (V : GL_Value; GT : GL_Type) return GL_Value is
      In_GT : constant GL_Type := Related_Type (V);

   begin
      --  If V is already of the desired type, we're done

      if In_GT = GT then
         return V;

      --  If this is an elementary type, the GT's may be of a different
      --  size, so pointer-punning will give the wrong result.  Instead,
      --  we have to ensure the value is Data and do a normal conversion.
      --  However, we don't need to do the load if the LLVM types are the
      --  same.

      elsif Is_Elementary_Type (In_GT) and then Is_Elementary_Type (GT) then
         if Type_Of (In_GT) = Type_Of (GT) then
            return G_Is (V, GT);
         else
            return Convert (Get (V, Data), GT);
         end if;

      --  If this is a reference (now known to be a composite type), use
      --  pointer punning.

      elsif Is_Reference (V) then
         return Convert_Ref (V, GT);

      --  We now have Data of a composite type.  If they're the same GNAT
      --  type, convert via the primitive type.

      elsif Full_Etype (In_GT) = Full_Etype (GT) then
         return From_Primitive (To_Primitive (V), GT);

      --  Otherwise, it must be a case where this is a type whose layout is
      --  identical to GT or where the base types are the same.  If it's a
      --  constant record with identical layout, we can convert it.
      --  Otherwise, leave it alone and it'll be sorted out downstream.

      elsif Is_Constant (V) and then Is_Record_Type (GT)
        and then Is_Layout_Identical (V, GT)
      then
         return Convert_Struct_Constant (V, GT);
      else
         return V;
      end if;

   end Convert_GT;

   -----------------
   -- Convert_Ref --
   -----------------

   function Convert_Ref (V : GL_Value; GT : GL_Type) return GL_Value is
      V_GT     : constant GL_Type := Related_Type (V);
      Unc_Src  : constant Boolean := Is_Access_Unconstrained_Array (V);
      Unc_Dest : constant Boolean := Is_Unconstrained_Array (GT);

   begin
      --  V is some type of reference to some type.  We want to
      --  convert it to be some type of reference to GT, which may be
      --  some other type (if it's the same, we have no work to do).  The
      --  relationship of the result to GT may or may not be the same as
      --  the relationship of V to its type.
      --
      --  We want to do as little work here as possible because we don't
      --  know what our caller will be doing with the result and want to
      --  avoid a situation where what we do has to be undone by our caller.
      --  However, the following must be true:
      --
      --  (1) The result must be SOME valid representation of GT
      --  (2) We must not lose any information, especially information that
      --      we can't recover, but should also not discard any information
      --      that we might conceivable need later if we can keep it
      --
      --  These principles dictate our behavior in all cases.  For example,
      --  if the input is a fat pointer, we should try to retain it as a
      --  fat pointer even if GT is constrained because we may want those
      --  bounds if we later convert to an unconstrained type.  However, if
      --  we're converting to a constrained array with an index type that
      --  has a different LLVM type, we discard the bounds rather than
      --  recomputing them since we may NOT need them and hence may be
      --  wasting that computation.  On the other hand, if the input is a a
      --  constrained array type and the output is unconstrained, we MUST
      --  materialize the bounds because they come from the bounds of the
      --  constrained array and would be lost if we were to just return a
      --  pointer to the data with an unconstrained type.
      --
      --  ??? Need to rewrite to implement the above

      --  First deal with the case where we're converting between two arrays
      --  with different index types and GT is unconstrained.  In that case,
      --  we have to materialize the bounds in the new index types.

      if Unc_Dest and then Is_Array_Type (V_GT)
        and then Are_Arrays_With_Different_Index_Types (GT, V_GT)
      then
         declare
            New_FP : constant GL_Value :=
              Get_Undef_Relationship (GT, Fat_Pointer);
            Bounds : constant GL_Value := Get_Array_Bounds (GT, V_GT, V);
            Data   : constant GL_Value :=
              Ptr_To_Relationship (Get (V, Reference), GT, Reference);

         begin
            return Insert_Value (Insert_Value (New_FP, Data, 0),
                                 Get (Bounds, Reference_To_Bounds), 1);
         end;
      end if;

      --  Next have the case where we previously had a reference to a
      --  subprogram and all we knew was the return type and we're converting
      --  it to an actual subprogram access type.  We have little to do, but
      --  it simplifies the tests below since Full_Designated_Type is
      --  undefined on such objects.

      if Is_Subprogram_Reference (V) then
         return Ptr_To_Relationship (V, GT, Reference);

      --  If neither is constrained, but they aren't the same type, just do
      --  a pointer cast unless we have to convert between function access
      --  types that do and don't have static links.  If both are
      --  constrained, we return the input unchanged (the front end is
      --  responsible for this making sense).  Otherwise, we have to handle
      --  converting between fat and raw pointers.

      elsif not Unc_Src and not Unc_Dest then
         if Related_Type (V) = GT then
            return Get (V, Any_Reference);
         else
            --  If what we have is a reference to bounds and data or a
            --  thin pointer and have an array type that needs bounds,
            --  convert to the same relationship of that type.  Otherwise,
            --  convert to a Reference and then to the new type.

            if Relationship (V) in Reference_To_Bounds_And_Data | Thin_Pointer
              and then Type_Needs_Bounds (GT)
            then
               return Ptr_To_Relationship (V, GT, Relationship (V));
            else
               return Ptr_To_Ref (Get (V, Reference), GT);
            end if;
         end if;

      elsif Unc_Src and then Unc_Dest then
         return Convert_Pointer (Get (V, Fat_Pointer), GT);

      elsif Unc_Src and then not Unc_Dest then
         return Convert_Ref (Get (V, Reference), GT);
      else
         pragma Assert (not Unc_Src and then Unc_Dest);

         --  ???  The code here is wrong.  See, e.g., c46104a.  If we're
         --  converting between arrays with different types for bounds,
         --  we need to use the new bounds and not just UC the bound
         --  reference, since that won't work.  But there doesn't seem
         --  to be an obvious way to fix it at the moment and this
         --  whole function needs to be rewritten anyway.

         return Convert_Pointer (Get (To_Primitive (V), Fat_Pointer), GT);
      end if;
   end Convert_Ref;

   ---------------------
   -- Convert_Pointer --
   ---------------------

   function Convert_Pointer (V : GL_Value; GT : GL_Type) return GL_Value is
      R     : constant GL_Relationship := Relationship (V);
      T     : constant Type_T          := Type_For_Relationship (GT, R);
      Value : Value_T;

   begin
      if Type_Of (V) = T then
         return G_Is_Relationship (V, GT, R);

      --  If the input is an actual pointer, convert it

      elsif Get_Type_Kind (T) = Pointer_Type_Kind then
         return G (Pointer_Cast (IR_Builder, LLVM_Value (V), T, ""), GT, R);
      end if;

      --  Otherwise, we have a composite pointer and must make a new
      --  structure corresponding to converting each pointer individually.

      Value := Get_Undef (T);
      for J in 0 .. Count_Struct_Element_Types (T) - 1 loop
         declare
            Out_Type : constant Type_T   := Struct_Get_Type_At_Index (T, J);
            In_Value : constant Value_T  :=
              Extract_Value (IR_Builder, LLVM_Value (V), J, "");
            Cvt_Value : constant Value_T :=
              Pointer_Cast (IR_Builder, In_Value, Out_Type, "");

         begin
            Value := Insert_Value (IR_Builder, Value, Cvt_Value, J, "");
         end;
      end loop;

      return G (Value, GT, R);
   end Convert_Pointer;

   -------------------------------
   -- Strip_Complex_Conversions --
   -------------------------------

   function Strip_Complex_Conversions (N : Node_Id) return Node_Id is
      E : Node_Id := N;

   begin
      while Present (E) loop
         exit when not Nkind_In (E, N_Type_Conversion,
                                 N_Unchecked_Type_Conversion,
                                 N_Qualified_Expression);
         exit when Is_Elementary_Type (Full_Etype (E));
         exit when Is_Elementary_Type (Full_Etype (Expression (E)));
         exit when Get_Type_Size_Complexity (Full_GL_Type (E))
           <= Get_Type_Size_Complexity (Full_GL_Type (Expression (E)));
         E := Expression (E);
      end loop;

      return E;
   end Strip_Complex_Conversions;

   -----------------------
   -- Strip_Conversions --
   -----------------------

   function Strip_Conversions (N : Node_Id) return Node_Id is
      E : Node_Id := N;

   begin
      while Present (E) loop
         exit when not Nkind_In (E, N_Type_Conversion,
                                 N_Unchecked_Type_Conversion,
                                 N_Qualified_Expression);
         E := Expression (E);
      end loop;

      return E;
   end Strip_Conversions;

end GNATLLVM.Conversions;
