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

with Einfo.Utils; use Einfo.Utils;
with Errout;      use Errout;
with Snames;      use Snames;

with LLVM.Transforms_Inst_Combine; use LLVM.Transforms_Inst_Combine;

with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Exprs;        use GNATLLVM.Exprs;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Wrapper;      use GNATLLVM.Wrapper;

package body GNATLLVM.Conversions is

   function Are_Arrays_With_Different_Index_Types
     (GT1, GT2 : GL_Type) return Boolean
     with Pre => Present (GT1) and then Present (GT2);
   --  Return True iff GT1 and GT2 are array types that have at least
   --  one index for whose LLVM types are different.

   function Is_In_LHS_Context (N : Opt_N_Subexpr_Id) return Boolean;
   --  Return True if N's parent (if N is Present) is such that we need a
   --  LValue.

   function Is_Nop_Conversion (V : GL_Value; GT : GL_Type) return Boolean
     with Pre => Is_Reference (V) and then Present (GT);
   --  Return True if converting V to type GT won't change any bits

   function Is_Nonsymbolic_Constant_Internal (V : Value_T) return Boolean
     with Pre => Present (V);
   --  Return True iff V is a constant and that constant contains no
   --  symbolic pointer values.

   function Contains_Restricted_Type (T : Type_T) return Boolean
     with Pre => Present (T);
   --  Return True iff T is either a pointer type, a wide FP type, or
   --  a composite type that contains one of those.

   function Convert_Nonsymbolic_Constant
     (V : Value_T; T : Type_T) return Value_T
     with Pre  => Present (V) and then Present (T)
                  and then Is_Nonsymbolic_Constant (V),
          Post => Type_Of (Convert_Nonsymbolic_Constant'Result) = T;
   --  Convert V, a constant, to T

   function Convert_Nonsymbolic_Constant_Internal
     (V : Value_T; T : Type_T) return Value_T
     with Pre  => Present (V) and then Present (T)
                  and then Is_Nonsymbolic_Constant (V),
          Post => Type_Of (Convert_Nonsymbolic_Constant_Internal'Result) = T;
   --  Convert V, a constant, to T. This version only works if one side
   --  is an array of bytes.

   -----------------------------------------------
   -- Are_Arrays_With_Different_Index_Types --
   -----------------------------------------------

   function Are_Arrays_With_Different_Index_Types
     (GT1, GT2 : GL_Type) return Boolean
   is
      FLB1       : constant Boolean :=
        Is_Fixed_Lower_Bound_Array_Subtype (Full_Base_Type (GT1));
      FLB2       : constant Boolean :=
        Is_Fixed_Lower_Bound_Array_Subtype (Full_Base_Type (GT2));
      Idx1, Idx2 : Opt_N_Is_Index_Id;

   begin
      --  If either isn't an array type, we don't have this case

      if not Is_Array_Type (GT1) or else not Is_Array_Type (GT2) then
         return False;

      --  The string literal case is easy

      elsif Ekind (GT2) = E_String_Literal_Subtype then
         return (Type_Of (Full_Etype (First_Index (GT1)))
                   /= Type_Of (Integer_GL_Type));
      end if;

      --  The front end should not have gotten us here if the number
      --  of dimensions differ.

      pragma Assert (Number_Dimensions (GT1) = Number_Dimensions (GT2));

      --  We need to reconstruct the bounds if one type is a fixed lower bound
      --  type and the other isn't or if both are and is more than one
      --  dimension (since which dimension is has a fixed lower bound may
      --  differ.

      if FLB1 /= FLB2
        or else (FLB1 and then FLB2 and then Number_Dimensions (GT1) /= 1)
      then
         return True;
      end if;

      --  We don't need to do anything if the index types differ unless the
      --  corresponding LLVM types differ, so that's all we check.

      Idx1 := First_Index (GT1);
      Idx2 := First_Index (GT2);
      while Present (Idx1) loop
         exit when Type_Of (Full_Base_Type (Full_Etype (Idx1))) /=
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
      --  If either type is biased, this isn't a no-op

      if Is_Biased_GL_Type (V) or else Is_Biased_GL_Type (GT) then
         return False;

      --  This is a no-op if the two LLVM types are the same or if both
      --  GNAT types aren't scalar types.

      else
         return (Is_Reference (V) and then Relationship (V) /= Fat_Pointer
                 and then Element_Type_Of (V) = Type_Of (GT))
           or else (not Is_Scalar_Type (GT)
                      and then not Is_Scalar_Type (Related_Type (V)));
      end if;

   end Is_Nop_Conversion;

   -----------------------------
   -- Is_Unsigned_For_Convert --
   -----------------------------

   function Is_Unsigned_For_Convert (GT : GL_Type) return Boolean is
      BT : constant GL_Type := Base_GL_Type (GT);

   begin
      --  If biased, say yes

      if Is_Biased_GL_Type (GT) then
         return True;

      --  If GT is narrower than BT, use its signedness, otherwise use BT's

      else
         return (if   ULL'(Get_Type_Size (Type_Of (GT))) <
                        ULL'(Get_Type_Size (Type_Of (BT)))
                 then Is_Unsigned_Type (GT) else Is_Unsigned_Type (BT));
      end if;

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

      else
         declare
            LB : constant Uint := Get_Uint_Value (Type_Low_Bound (GT));

         begin
            return Present (LB) and then LB >= 0;
         end;
      end if;

   end Is_Unsigned_For_RM;

   -----------------------
   -- Is_In_LHS_Context --
   -----------------------

   function Is_In_LHS_Context (N : Opt_N_Subexpr_Id) return Boolean is
   begin
      if No (N) or else No (Parent (N)) then
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

   function Is_Parent_Of (T_Need, T_Have : Record_Kind_Id) return Boolean is
      BT_Need : constant Record_Kind_Id := Full_Base_Type (T_Need);
      BT_Have : constant Record_Kind_Id := Full_Base_Type (T_Have);

   begin
      --  If the two types are the same return True. Likewise if
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
     (N                   : N_Subexpr_Id;
      GT                  : GL_Type;
      From_N              : Opt_N_Subexpr_Id := Empty;
      For_LHS             : Boolean          := False;
      Is_Unchecked        : Boolean          := False;
      Need_Overflow_Check : Boolean          := False;
      Float_Truncate      : Boolean          := False;
      No_Truncation       : Boolean          := False) return GL_Value
   is
      Result      : GL_Value                  := Emit (N, For_LHS => For_LHS);
      Prim_GT     : constant GL_Type          := Primitive_GL_Type (GT);
      In_GT       : constant GL_Type          := Related_Type (Result);
      R           : constant GL_Relationship  := Relationship (Result);
      GT_Uns      : constant Boolean          := Is_Unsigned_For_RM (GT);
      In_GT_Uns   : constant Boolean          := Is_Unsigned_For_RM (In_GT);
      Error_N     : constant Opt_N_Subexpr_Id :=
        (if Present (From_N) then From_N else N);

   begin
      --  We have to be careful here. There isn't as clear a distinction
      --  between unchecked conversion and regular conversion as we might
      --  like. Both the front-end and RTS have code of the form "Type
      --  (LValue)'Unrestricted_Access" and expect this to produce a
      --  reference to the address of LValue. This code is called in both
      --  the LValue and value case. If we're starting with a reference,
      --  we want to keep it as a reference unless we're sure that this
      --  needs an actual conversion.
      --
      --  We test two things: first, we see if we're being used in a
      --  context where an LValue is definitely or likely needed and also
      --  look for a conversion that won't actually change any bits. If
      --  both, do this as an unchecked conversion. On the other hand, if
      --  an overflow check is required, we know this is NOT an unchecked
      --  conversion.

      --  First, if we're converting from an subprogram access type that's
      --  convention Ada to one that's a foreign convention, give a warning
      --  since that can cause issues with nested subprograms because we
      --  don't have a way of making a trampoline at this point. However,
      --  the other direction works fine since if an activation record is
      --  needed, the "subprogram address" will be a trampoline that will load
      --  it (the value in the second part of the fat subprogram pointer will
      --  be unused.
      --  ??? Even in the "bad" direction, if we have a constant, we may be
      --  able to know more about whether it needs an activation record, for
      --  example if it's not a nested function.

      if Is_Access_Subprogram_Type (GT)
        and then Is_Access_Subprogram_Type (In_GT)
        and then (Has_Foreign_Convention (GT)
                    or else not Can_Use_Internal_Rep (GT))
        and then not (Has_Foreign_Convention (In_GT)
                       or else not Can_Use_Internal_Rep (In_GT))
      then
         Error_Msg_Node_1 := Full_Etype (In_GT);
         Error_Msg_Node_2 := Full_Etype (GT);
         Error_Msg
           ("??conversion between & and & may not work if", Sloc (Error_N));
         Error_Msg
           ("\the former is a subprogram referencing parent variables.",
            Sloc (Error_N));
      end if;

      --  If our type is already what's needed, we're done

      if In_GT = GT then
         return Result;

      --  If we're converting to an elementary type and need an overflow
      --  check, do that.

      elsif Is_Elementary_Type (GT) and then Need_Overflow_Check then
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
      --  the bits. But use Size_Type and generic pointers to make sure
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
         --  address is to be taken as a thin pointer. We also need special
         --  code in the case of access to subprogram and we also need
         --  to show that the alignment is of the designated type.

         if Is_Unconstrained_Array (Full_Designated_Type (GT)) then
            Result := Int_To_Relationship (Get (Result, Data),
                                           Full_Designated_GL_Type (GT),
                                           Thin_Pointer);
         elsif Ekind (GT) in E_Access_Subprogram_Type |
                             E_Anonymous_Access_Subprogram_Type
         then
            Result := Int_To_Relationship (Get (Result, Data),
                                           Full_Designated_GL_Type (GT),
                                           Reference);
         else
            Result := Int_To_Ref (Get (Result, Data), SSI_GL_Type);
         end if;

         Result := Convert_To_Access (Result, GT);

      --  We can unchecked convert floating point of the same width
      --  (the only way that UC is formally defined) with a "bitcast"
      --  instruction. But we can't do this if either type is X86_Fp80
      --  because the sizes aren't well-defined there.

      elsif Is_Unchecked
        and then ((Is_Floating_Point_Type (GT)
                     and then Is_Discrete_Or_Fixed_Point_Type (In_GT))
                  or else (Is_Discrete_Or_Fixed_Point_Type (GT)
                             and then Is_Floating_Point_Type (In_GT)))
        and then ULL'(Get_Type_Size (Type_Of (GT))) =
                   ULL'(Get_Type_Size (Type_Of (In_GT)))
        and then Get_Type_Kind (GT) /= X86_FP80_Type_Kind
        and then Get_Type_Kind (In_GT) /= X86_FP80_Type_Kind
      then
         Result := Bit_Cast (Get (Result, Data), GT);

      --  If both types are elementary, hand that off to our helper, but
      --  raise a Constraint_Error if this conversion overflowed by producing
      --  an undef.

      elsif Is_Elementary_Type (In_GT) and then Is_Elementary_Type (GT) then

         --  If the aim of the conversion is one integral type to the same
         --  result type, just show that the type has changed. There may
         --  have been smaller types involved in primitive types of the
         --  conversion, but converting to and from them is not only
         --  unnecessary code, but can break 'Valid of a widened field.
         --  However, we always have to do something if there's a biased or
         --  packed array implementation type involved. But first be sure
         --  we have data.

         Result := Get (Result, Data);

         if Is_Discrete_Or_Fixed_Point_Type (GT)
           and then Type_Of (GT) = Type_Of (Result)
           and then not Is_Biased_GL_Type (GT)
           and then not Is_Biased_GL_Type (Result)
           and then not Is_Packed_Array_Impl_Type (GT)
           and then not Is_Packed_Array_Impl_Type (Result)
           and then not Is_Unchecked
         then
            Result := G_Is (Result, GT);

         --  If we have a reference to an access type and we're converting
         --  to another access type, we can just convert the reference.
         --  This avoids loading the data, including in cases where we
         --  can't load the data, such as at library level.

         elsif Is_Reference (Result) and then Is_Access_Type (Result) then
            Result := Convert_Ref (Result, GT);

         else
            Result := Convert (Get (Result, Data), GT,
                               Float_Truncate => Float_Truncate,
                               Is_Unchecked   => Is_Unchecked,
                               No_Truncation  => No_Truncation);
         end if;

      --  If we have a constant (which we know is an aggregate), we
      --  can make a new constant. This is the desired result for both
      --  checked and unchecked conversion.

      elsif Can_Convert_Aggregate_Constant (Result, GT) then
         Result := Convert_Aggregate_Constant (Result, GT);

      --  If we have an undefined value that we're converting to another
      --  type, just get an undefined value of that type. But watch for
      --  the case where we have Data of some fixed-size type and we're
      --  converting to a dynamic-sized type. We handle the reference
      --  cases below since we may have to deal with materializing bounds.

      elsif Is_Undef (Result) and then R = Data and then Is_Loadable_Type (GT)
      then
         Result := Get_Undef (GT);

      --  Otherwise, convert to the primitive type, do any required
      --  conversion (as an unchecked conversion, meaning pointer
      --  punning or equivalent) and then convert to the result type.
      --  Some of these operations will likely be nops.

      else
         --  If the result type is an unconstrained record, don't convert
         --  to the primitive type since this will often result in
         --  copying from beyond the size of the object.

         if not Is_Unconstrained_Record (GT) then
            Result := To_Primitive (Result, No_Copy => Is_Unchecked);
         end if;

         --  If both types are the same, just change the type of the result.
         --  Avoid confusing [0 x T] as both a zero-size constrained type and
         --  the type used for a variable-sized type.

         if Is_Data (Result) and then not Is_Nonnative_Type (Prim_GT)
           and then Type_Of (Result) = Type_Of (Prim_GT)
         then
            Result := G_Is (Result, Prim_GT);

         --  If this is a zero-sized input and our result is an
         --  integer type, we don't want to load anything, so set
         --  the result as undefined.

         elsif Is_Integer_Type (GT) and then Is_Zero_Size (Result) then
            Result := Emit_Undef (GT);

         --  If the result type is an integer and is wider than our
         --  input width, we need to do an integer load of exactly the
         --  width of the input so we don't overrun memory and to avoid
         --  any endianness issues. But don't get confused by fat pointers
         --  or variable-sized objects.

         elsif Is_Integer_Type (GT)
           and then Relationship (Result) /= Fat_Pointer
           and then not Is_Nonnative_Type (Result)
           and then Get_Scalar_Bit_Size (GT) >
           Get_Scalar_Bit_Size (Data_Type_Of (Result))
         then
            declare
               T : constant Type_T :=
                 Int_Ty (Get_Scalar_Bit_Size (Data_Type_Of (Result)));

            begin
               Result :=
                 Ptr_To_Relationship
                   (Get (Result, Any_Reference),
                    Pointer_Type (T, Address_Space), Reference_To_Unknown);
               Set_Unknown_T (Result, T);
               Result := Load (Result);
               Result := GM (+Result, GT, GV => Result);
               if Is_Unsigned_Type (GT) then
                  Result := Z_Ext (Result, GT);
               else
                  Result := S_Ext (Result, GT);
               end if;
            end;

         --  Otherwise, do an actual pointer pun. But if we have a modular
         --  integer type that's a packed array implementation type, we
         --  can't use the primitive since that's i1 and the data will have
         --  been stored as i8.

         else
            Result :=
              Convert_Ref (Get (Result, Any_Reference),
                           (if   (Is_Modular_Integer_Type (GT)
                                   and then Is_Packed_Array_Impl_Type (GT))
                                 or else Is_Unchecked
                            then GT else Prim_GT));
         end if;

         if Related_Type (Result) /= GT then
            Result := From_Primitive (Result, GT, No_Copy => Is_Unchecked);
         end if;
      end if;

      --  For unchecked conversion, if the result is a non-biased
      --  integral type whose precision is not equal to its size, sign-
      --  or zero-extend the result. But we need not do this if the
      --  input is also an integral type and both are unsigned or both
      --  are signed and the output is not narrower than the input and
      --  we can't do this in the case of nonbinary modulus. We can't do
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
              Shl (Convert (Get (Result, Data), Prim_GT), Shift_Count,
                   Allow_Overflow => True);

         begin
            Result := From_Primitive ((if   Is_Unsigned_Type (Prim_GT)
                                       then L_Shr (Left_Shift, Shift_Count)
                                       else A_Shr (Left_Shift, Shift_Count)),
                                      GT);
         end;
      end if;

      --  For unchecked conversion, we know that the alignment is at least
      --  that of the type (or the type pointed to if this is an access type).

      if Is_Unchecked then
         Initialize_Alignment (Result);
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
      Is_Unchecked   : Boolean := False;
      No_Truncation  : Boolean := False) return GL_Value
   is
      type Cvtf is access function
        (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value;

      Is_Unc_Bias : constant Boolean  :=
        Is_Unchecked and then (Is_Biased_GL_Type (GT)
                                 or else Is_Biased_GL_Type (Related_Type (V)));
      In_V        : constant GL_Value :=
        (if   Is_Unchecked or else Related_Type (V) = GT then V
         else To_Primitive (V));
      In_GT       : constant GL_Type  := Related_Type (In_V);
      Prim_GT     : constant GL_Type  :=
        (if Is_Unc_Bias then GT else Primitive_GL_Type (GT));
      Value       : GL_Value          := In_V;
      Src_Access  : constant Boolean  := Is_Access_Type (V);
      Dest_Access : constant Boolean  := Is_Access_Type (Prim_GT);
      Src_FP      : constant Boolean  := Is_Floating_Point_Type (V);
      Dest_FP     : constant Boolean  := Is_Floating_Point_Type (Prim_GT);
      Src_Uns     : constant Boolean  := Is_Unsigned_For_Convert (In_GT);
      Dest_Uns    : constant Boolean  := Is_Unsigned_For_Convert (Prim_GT);
      Src_Size    : constant Nat      :=
        Nat (ULL'(Get_Scalar_Bit_Size (In_V)));
      Dest_Size   : constant Nat      :=
        Nat (ULL'(Get_Scalar_Bit_Size (Prim_GT)));
      Is_Trunc    : constant Boolean  := Dest_Size < Src_Size;
      Subp        : Cvtf              := null;

   begin
      --  If the input is undefined, so is our output

      if Is_Undef (In_V) then
         return Get_Undef (GT);

      --  If we want the same type as we have, we're done

      elsif In_GT = GT then
         return In_V;

      --  If the value is already of the desired LLVM type, we're done
      --  unless one type is biased or if we're converting an unsigned
      --  constant to signed and the result will be negative or if this is
      --  an unchecked (but not non-truncating) conversion.

      elsif Type_Of (In_V) = Type_Of (GT) and then not Is_Biased_GL_Type (In_V)
        and then not Is_Biased_GL_Type (GT)
        and then not (Is_Unchecked and then not No_Truncation)
      then
         return Mark_Overflowed (G_Is (In_V, GT),
                                 not Dest_Uns and then Src_Uns
                                   and then Is_A_Constant_Int (In_V)
                                   and then +In_V < ULL (0));

      --  If we're converting between two GL_Types corresponding to the same
      --  GNAT type, convert to the primitive type and the to the desired
      --  GL_Type (one of those will likely be a nop). Don't do this if
      --  we have a UC to or from a biased type.

      elsif Full_Etype (In_GT) = Full_Etype (GT) and then not Is_Unc_Bias then
         return From_Primitive (In_V, GT);

      --  If converting pointer to/from integer, copy the bits using the
      --  appropriate instruction.

      elsif not Tagged_Pointers
        and then Dest_Access
        and then Is_Integer_Type (In_V)
      then
         Subp := Int_To_Ptr'Access;
      elsif not Tagged_Pointers
        and then Is_Integer_Type (GT)
        and then Src_Access
      then
         Subp := Ptr_To_Int'Access;

      --  For pointer to pointer, call our helper

      elsif Src_Access and then Dest_Access then
         return Convert_To_Access (Value, GT, Is_Unchecked => Is_Unchecked);

      --  With tagged pointers, we store addresses as LLVM pointers; when
      --  converting between different address types, we therefore don't do
      --  anything beyond changing the Ada type.

      elsif Tagged_Pointers
        and then Is_Address (In_V)
        and then Is_Address (GT)
      then
         return G_Is (In_V, GT);

      --  Conversions from integer to tagged-pointer address yield a result
      --  derived from the null pointer; it can't be dereferenced, but it
      --  can be combined with valid pointers, inheriting tags.

      elsif Tagged_Pointers
        and then Is_Integer_Type (In_V)
        and then (Dest_Access or else Is_Address (GT))
      then
         Subp := Null_Derived_Ptr'Access;

      --  Conversely, when converting tagged-pointer addresses to integers,
      --  we need to extract the pointer's address. Afterwards, we may need
      --  to truncate or zero-extend the resulting integer.

      elsif Tagged_Pointers
        and then (Src_Access or else Is_Address (In_V))
        and then Is_Integer_Type (GT)
      then
         Value := Get_Pointer_Address (Value);

         declare
            Addr_GT   : constant GL_Type := Related_Type (Value);
            Addr_Size : constant Nat :=
              Nat (ULL'(Get_Scalar_Bit_Size (Addr_GT)));
         begin
            if GT = Addr_GT then
               return Value;
            else
               Subp :=
                 (if   Dest_Size < Addr_Size
                  then Trunc'Access
                  else Z_Ext'Access);
            end if;
         end;

      --  Having dealt with pointers, we have four cases: FP to FP, FP to
      --  Int, Int to FP, and Int to Int. We already know that this isn't
      --  a noop case because we've checked above for the same type.

      elsif Src_FP and then Dest_FP then
         Subp := (if Is_Trunc then FP_Trunc'Access else FP_Ext'Access);

      elsif Src_FP and then not Dest_FP then
         Subp := (if Dest_Uns then FP_To_UI'Access else FP_To_SI'Access);

         if not Float_Truncate then

            --  In the FP to Integer case, the LLVM instructions round to
            --  zero, but the Ada semantics round away from zero, so we
            --  have to adjust the input. We first compute Type'Pred
            --  (0.5). If the input is strictly negative, subtract this
            --  value and otherwise add it from the input. For 0.5, the
            --  result is exactly between 1.0 and the machine number
            --  preceding 1.0. Since the last bit of 1.0 is even, this 0.5
            --  will round to 1.0, while all other number with an absolute
            --  value less than 0.5 round to 0.0. For larger numbers
            --  exactly halfway between integers, rounding will always be
            --  correct as the true mathematical result will be closer to
            --  the higher integer compared to the lower one. So, this
            --  constant works for all floating-point numbers. We compute
            --  this using an LLVM function ("next") that operates on an
            --  APFloat and returns either a value epsilon higher or lower
            --  than the original.
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
               Add_Amt    : constant GL_Value := F_Add (In_V, Half, "ROUND");
               Sub_Amt    : constant GL_Value := F_Sub (In_V, Half, "ROUND");

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

      --  We've verified at the start that our input isn't an undef. If we
      --  see an undef here, it means that LLVM has flagged a conversion as
      --  overflowing (which only happens for FP to int). Be consistent
      --  with the rest of our infrastructure in that case and mark it as
      --  overflowed. Then all that's left to do is deal with
      --  non-primitive types and generate the IR instruction.

      return Result : GL_Value := Subp (Value, Prim_GT) do
         Result := Mark_Overflowed (Result, Is_Undef (Result));

         if Related_Type (Result) /= GT then
            Result := From_Primitive (Result, GT);
         end if;
      end return;

   end Convert;

   -----------------------
   -- Convert_To_Access --
   -----------------------

   function Convert_To_Access
     (V            : GL_Value;
      GT           : GL_Type;
      Is_Unchecked : Boolean := False) return GL_Value
   is
      DT     : constant GL_Type         := Full_Designated_GL_Type (GT);
      In_GT  : constant GL_Type         := Related_Type (V);
      As_Ref : constant GL_Value        :=
        (if   Is_Data (V) and then Is_Access_Type (In_GT)
         then From_Access (V) else V);
      In_R   : constant GL_Relationship := Relationship (As_Ref);
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

      --  If we have an unchecked conversion to a fat pointer, we can have
      --  all sorts of weird stuff as input. In that case, we want to do
      --  whatever is as reasonable as we can, but not crash

      elsif Is_Unchecked and then R = Fat_Pointer
        and then not Contains_Bounds (As_Ref)
      then
         --  Start making a fat pointer with both parts initial undefined.
         --  If As_Ref is a pointer, cast it into the proper type and insert
         --  it into the fat pointer. If it's an integer of the proper width,
         --  convert it to a pointer (it may be System.Address). Otherwise,
         --  leave it as undef.

         Result := Get_Undef_Relationship (DT, R);

         if Is_Pointer (As_Ref) then
            Result := Insert_Value (Result, Ptr_To_Ref (As_Ref, DT), 0);
         elsif Is_Integer_Type (As_Ref)
           and then Get_Type_Size (Type_Of (As_Ref)) = ULL (Thin_Pointer_Size)
         then
            Result := Insert_Value (Result, Int_To_Ref (As_Ref, DT), 0);
         end if;

       --  If this is a unchecked conversion to a thin pointer from a
       --  non-array type, this isn't going to work if we look at bounds,
       --  but just copy the pointer to avoid blowing up below looking for
       --  bounds.

      elsif Is_Unchecked and then R = Thin_Pointer
        and then not Is_Array_Type (As_Ref)
      then
         --  If this is a fat pointer (a pointer isn't viewed as an array
         --  type above), extract the data first.

         Result := As_Ref;

         if In_R = Fat_Pointer then
            Result := Get (Result, Reference);
         end if;

         --  And then set it as a thin pointer

         Result := Ptr_To_Relationship (Result, DT, Thin_Pointer);

      --  Otherwise, get the input in the desired relationship and then
      --  convert the pointer.

      else
         Result := Convert_Pointer (Get (As_Ref, R), DT);
      end if;

      return From_Primitive (To_Access (Result, Primitive_GL_Type (GT)), GT);
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
      --  size, so pointer-punning will give the wrong result. Instead,
      --  we have to ensure the value is Data and do a normal conversion.
      --  However, we don't need to do the load if the LLVM types are the
      --  same.

      elsif Is_Elementary_Type (In_GT) and then Is_Elementary_Type (GT) then
         if Type_Of (In_GT) = Type_Of (GT) then
            return G_Is (V, GT);
         else
            return Convert (Get (V, Data), GT);
         end if;

      --  If this is an aggregate constant, we may be able to convert it

      elsif Can_Convert_Aggregate_Constant (V, GT) then
         return Convert_Aggregate_Constant (V, GT);

      --  If this is a reference (now known to be a composite type), use
      --  pointer punning.

      elsif Is_Reference (V) then
         return Convert_Ref (V, GT);

      --  We now have Data of a composite type. If they're the same GNAT
      --  type, convert via the primitive type.

      elsif Full_Etype (In_GT) = Full_Etype (GT) then
         return From_Primitive (To_Primitive (V), GT);

      --  Otherwise, leave it alone and it'll be sorted out downstream.

      else
         return V;
      end if;

   end Convert_GT;

   -----------------
   -- Convert_Ref --
   -----------------

   function Convert_Ref (V : GL_Value; GT : GL_Type) return GL_Value is
      V_GT       : constant GL_Type := Related_Type (V);
      Has_Bounds : constant Boolean := Contains_Bounds (V);
      Unc_Dest   : constant Boolean := Is_Unconstrained_Array (GT);
      In_V       : GL_Value;

   begin
      --  V is some type of reference to some type. We want to
      --  convert it to be some type of reference to GT, which may be
      --  some other type (if it's the same, we have no work to do). The
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
      --  These principles dictate our behavior in all cases. For example,
      --  if the input is a fat pointer, we should try to retain it as a
      --  fat pointer even if possible because we may want those bounds if
      --  we later convert to an unconstrained type. However, if we're
      --  converting to an array with an index type that has a different
      --  LLVM type, we discard the bounds rather than recomputing them
      --  since we may NOT need them and hence may be wasting that
      --  computation. On the other hand, if the input is a a constrained
      --  array type and the output is unconstrained, we MUST materialize
      --  the bounds because they come from the bounds of the constrained
      --  array and would be lost if we were to just return a pointer to
      --  the data with an unconstrained type.
      --
      --  First is the case where we previously had a reference to a
      --  subprogram, all we knew was the return type, and we're converting
      --  it to an actual subprogram access type.

      if Is_Subprogram_Reference (V) then
         return Ptr_To_Relationship (V, GT, Reference);

      --  If the types are the same, we're done except that we know
      --  nothing about Reference_To_Unknown

      elsif GT = V_GT and then Relationship (V) /= Reference_To_Unknown then
         return V;

      --  Next handle the case with a source that has bounds (which is not
      --  necessarily an unconstrained source because we may be keeping
      --  bounds around in case we need them) or when we have an
      --  unconstrained destination.

      elsif Has_Bounds or else Unc_Dest then

         --  If the result is an unconstrained array and the index types
         --  are the same, just convert the pointer and keep it in the same
         --  representation, making one if we have an unconstrained result
         --  type but don't already have bounds. Otherwise, recreate the
         --  bounds for an unconstrained destination and discard them for a
         --  constrained destination.

         In_V := Remove_Padding (V);

         if Unc_Dest
           and then not Are_Arrays_With_Different_Index_Types (GT, V_GT)
         then
            if not Has_Bounds then
               In_V := Get (In_V, Fat_Pointer);
            end if;

            return Convert_Pointer (In_V, GT);
         elsif Unc_Dest then
            declare
               New_FP : constant GL_Value :=
                 Get_Undef_Relationship (GT, Fat_Pointer);
               Bounds : constant GL_Value := Get_Array_Bounds (GT, V_GT, In_V);
               Data   : constant GL_Value :=
                 Ptr_To_Relationship (Get (In_V, Reference), GT, Reference);

            begin
               return Insert_Value (Insert_Value (New_FP, Data, 0),
                                    Get (Bounds, Reference_To_Bounds), 1);
            end;
         else
            return Ptr_To_Ref (Get (In_V, Reference), GT);
         end if;

      --  At this point, we know that the result isn't an unconstrained
      --  array and that the input doesn't have bounds, so just convert
      --  to a reference.

      else
         return Ptr_To_Ref (Remove_Padding (V), GT);
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
         return GM (Pointer_Cast (IR_Builder, +V, T, ""), GT, R, V);
      end if;

      --  Otherwise, we have a composite pointer and must make a new
      --  structure corresponding to converting each pointer individually.

      Value := Get_Undef (T);
      for J in 0 .. Count_Struct_Element_Types (T) - 1 loop
         declare
            Out_Type : constant Type_T   := Struct_Get_Type_At_Index (T, J);
            In_Value : constant Value_T  :=
              Extract_Value (IR_Builder, +V, J, "");
            Cvt_Value : constant Value_T :=
              Pointer_Cast (IR_Builder, In_Value, Out_Type, "");

         begin
            Value := Insert_Value (IR_Builder, Value, Cvt_Value, J, "");
         end;
      end loop;

      return GM (Value, GT, R, V);
   end Convert_Pointer;

   ------------------------------
   -- Contains_Restricted_Type --
   ------------------------------

   function Contains_Restricted_Type (T : Type_T) return Boolean is
   begin
      case Get_Type_Kind (T) is
         when Pointer_Type_Kind | X86_FP80_Type_Kind | FP128_Type_Kind
            | PPC_FP128_Type_Kind =>
            return True;

         when Array_Type_Kind =>
            return Contains_Restricted_Type (Get_Element_Type (T));

         when Struct_Type_Kind =>
            for J in 0 .. Int (Count_Struct_Element_Types (T)) - 1 loop
               if Contains_Restricted_Type
                 (Struct_Get_Type_At_Index (T, unsigned (J)))
               then
                  return True;
               end if;
            end loop;

            return False;

         when others =>
            return False;
      end case;
   end Contains_Restricted_Type;

   --------------------------------------
   -- Is_Nonsymbolic_Constant_Internal --
   --------------------------------------

   function Is_Nonsymbolic_Constant_Internal (V : Value_T) return Boolean is
      T : constant Type_T := Type_Of (V);

   begin
      --  If this isn't a constant, it isn't a nonsymbolic constant.
      --  If it's a ConstantData, it is.
      --  ??? The LLVM optimizer also has issues with larger than 64 bit
      --  integers, so don't consider them as convertable this way.

      if not Is_Constant (V) then
         return False;
      elsif Present (Is_Constant_Data (V))
        and then not (Get_Type_Kind (T) = Integer_Type_Kind
                        and then Get_Int_Type_Width (T) > 64)
      then
         return True;

      --  Otherwise, this is some other type of constant. We're only concerned
      --  about structs and arrays and we set an upper bound on the size
      --  since LLVM's combiner does too.

      elsif Get_Type_Kind (T) in Struct_Type_Kind | Array_Type_Kind
        and then Get_Type_Size (T) < 1024 * BPU
      then
         declare
            Nelts : constant unsigned :=
              (if   Get_Type_Kind (T) = Struct_Type_Kind
               then Count_Struct_Element_Types (T)
               else Get_Array_Length (T));

         begin
            if Nelts /= 0 then
               for J in 0 .. Nelts - 1 loop
                  if not Is_Nonsymbolic_Constant_Internal (Get_Operand (V, J))
                  then
                     return False;
                  end if;
               end loop;
            end if;
         end;

         return True;

      --  Otherwise, this isn't a non-symbolic constant

      else
         return False;
      end if;

   end Is_Nonsymbolic_Constant_Internal;

   -----------------------------
   -- Is_Nonsymbolic_Constant --
   -----------------------------

   function Is_Nonsymbolic_Constant (V : Value_T) return Boolean is
     (not Contains_Restricted_Type (Type_Of (V))
        and then Is_Nonsymbolic_Constant_Internal (V));

   ------------------------------------
   -- Can_Convert_Aggregate_Constant --
   ------------------------------------

   function Can_Convert_Aggregate_Constant
     (V : GL_Value; GT : GL_Type) return Boolean
   is
      V_GT   : constant GL_Type := Related_Type (V);
      In_GT  : constant GL_Type :=
        (if Is_Padded_GL_Type (V_GT) then Primitive_GL_Type (V_GT) else V_GT);
      Out_GT : constant GL_Type :=
        (if Is_Padded_GL_Type (GT) then Primitive_GL_Type (GT) else GT);
      In_T   : constant Type_T := Type_Of (In_GT);
      Out_T  : constant Type_T := Type_Of (Out_GT);

   begin
      --  If this isn't data, isn't a constant, we have a nonnative
      --  type or both are an elementary type, we can't use this form
      --  of conversion. Likewise if this isn't a constant.

      if not Is_Data (V) or else not Is_Constant (V)
        or else Is_Nonnative_Type (GT)
        or else (not Is_Aggregate_Type (GT) and then not Is_Aggregate_Type (V))
      then
         return False;

      --  If the layout of the types are identical, we can do the conversion.
      --  Likewise if the input is undefined or all zero

      elsif Is_Layout_Identical (In_T, Out_T)
        or else Is_Undef (V) or else Is_A_Constant_Aggregate_Zero (V)
      then
         return True;

      --  The only other way is if we have a nonsymbolic constant (which
      --  checks for the length of the input), the output doesn't contain
      --  any restricted type, and the length of the output is small enough.

      else
         return Is_Nonsymbolic_Constant (V)
           and then not Contains_Restricted_Type (Out_T)
           and then Get_Type_Size (Out_T) < 1024 * BPU;
      end if;
   end Can_Convert_Aggregate_Constant;

   -------------------------------------------
   -- Convert_Nonsymbolic_Constant_Internal --
   -------------------------------------------

   function Convert_Nonsymbolic_Constant_Internal
     (V : Value_T; T : Type_T) return Value_T
   is
      BB       : constant Basic_Block_T  := Get_Insert_Block (IR_Builder);
      Ptr_Ty   : constant Type_T         := Pointer_Type (T, Address_Space);
      Our_Func : constant Value_T        :=
        Add_Function (Convert_Module, "__CC", Fn_Ty ((1 .. 0 => <>), T));
      Our_BB   : constant Basic_Block_T  := Append_Basic_Block (Our_Func, "");
      G_C      : constant Value_T        :=
        Add_Global (Convert_Module, Type_Of (V), "_CC");
      Our_PM   : constant Pass_Manager_T := Create_Pass_Manager;
      Changed  : Boolean;
      New_Ret  : Value_T;
      Result   : Value_T;

   begin
      --  We want to build a function that contains a return of a load from
      --  a constant global corresponding to V that's cast to a reference
      --  to GT and pass that to the instruction combiner. It should
      --  produce just one instruction, which is the return whose argument
      --  is the converted constant.
      --
      --  For the optimizer to properly handle this case, the following must
      --  be true and are guaranteed by our callers.
      --
      --  - either the input or output type must be of the form [N x i8]
      --
      --  - no scalar component can be a pointer or a type wider than
      --    a word
      --
      --  - all scalar components must be either constant integers or constant
      --    FP values.
      --
      --  - if the input or output is an integer type, its bitsize must be
      --    a multiple of the byte size

      Set_Initializer     (G_C, V);
      Set_Linkage         (G_C, Private_Linkage);
      Set_Global_Constant (G_C, True);
      Set_Unnamed_Addr    (G_C, True);
      Position_Builder_At_End (Our_BB);
      Discard (Build_Ret (IR_Builder,
                          Load_2 (IR_Builder, T,
                                  Pointer_Cast (IR_Builder, G_C, Ptr_Ty, ""),
                                  "")));
      Add_Instruction_Combining_Pass (Our_PM);
      Changed := Run_Pass_Manager (Our_PM, Convert_Module);
      New_Ret := Get_First_Instruction (Our_BB);
      pragma Assert (Changed and then Get_Last_Instruction (Our_BB) = New_Ret
                       and then Present (Is_A_Return_Inst (New_Ret)));
      Result  := Get_Operand (New_Ret, 0);

      --  Now delete everything we made here

      Delete_Basic_Block (Our_BB);
      Delete_Function (Our_Func);
      Dispose_Pass_Manager (Our_PM);

      --  If we were in a block previously, switch back to it. Then return
      --  our value.

      if Present (BB) then
         Position_Builder_At_End (BB);
      end if;

      return Result;
   end Convert_Nonsymbolic_Constant_Internal;

   ----------------------------------
   -- Convert_Nonsymbolic_Constant --
   ----------------------------------

   function Convert_Nonsymbolic_Constant
     (V : Value_T; T : Type_T) return Value_T
   is
      In_T      : constant Type_T  := Type_Of (V);
      In_Size   : constant ULL     := Get_Type_Size (In_T);
      In_Bytes  : constant ULL     := To_Bytes (In_Size);
      Cvt_T     : constant Type_T  :=
        (if   Get_Type_Kind (In_T) /= Integer_Type_Kind
              or else Get_Scalar_Bit_Size (In_T) = In_Size
         then In_T else Int_Ty (In_Bytes * UBPU));
      --  If we have a non-byte-width input type, make one that rounds up the
      --  size. We do the same for the output type below.

      Out_Size  : constant ULL     := Get_Type_Size (T);
      Out_Bytes : constant ULL     := To_Bytes (Out_Size);
      Out_T     : constant Type_T  :=
        (if   Get_Type_Kind (T) /= Integer_Type_Kind
              or else Get_Scalar_Bit_Size (T) = Out_Size
         then T else Int_Ty (Out_Bytes * UBPU));
      In_V      : constant Value_T :=
        (if Cvt_T = In_T then V else Z_Ext (IR_Builder, V, Cvt_T, ""));
      --  Zero extension is always correct because this can only happen
      --  for modular types.

      Result    : Value_T;

   begin
      --  If one type is a byte array, we can do this in one step

      if (Get_Type_Kind (In_T) = Array_Type_Kind
            and then Get_Element_Type (In_T) = Byte_T)
        or else (Get_Type_Kind (T) = Array_Type_Kind
                   and then Get_Element_Type (T) = Byte_T)
      then
         Result := Convert_Nonsymbolic_Constant_Internal (In_V, Out_T);

      --  Otherwise, make a byte array type and do two conversions

      else
         declare
            Bytes     : constant ULL     := ULL'Min (In_Bytes, Out_Bytes);
            Int_T     : constant Type_T  :=
              Array_Type (Byte_T, unsigned (Bytes));
            Int_V     : constant Value_T :=
              Convert_Nonsymbolic_Constant_Internal (In_V, Int_T);

         begin
            Result := Convert_Nonsymbolic_Constant_Internal (Int_V, Out_T);
         end;
      end if;

      --  If the result type was a non-byte-wide, we have to truncate what
      --  we have to it.

      return (if T = Out_T then Result else Trunc (IR_Builder, Result, T, ""));
   end Convert_Nonsymbolic_Constant;

   --------------------------------
   -- Convert_Aggregate_Constant --
   --------------------------------

   function Convert_Aggregate_Constant
     (V : Value_T; T : Type_T) return Value_T
   is
      In_T  : constant Type_T := Type_Of (V);

   begin
      --  There are many ways we can do this conversion. If the types
      --  are the same, we're done.

      if In_T = T then
         return V;

      --  If the input is undefined, so is the output

      elsif Is_Undef (V) then
         return Get_Undef (T);

      --  If the input it a zeroinitializer, so is the output

      elsif Present (Is_A_Constant_Aggregate_Zero (V)) then
         return  Const_Null (T);

      --  If they're structures and the layouts are identical, we can
      --  convert the constant that way.

      elsif Get_Type_Kind (In_T) = Struct_Type_Kind
        and then Is_Layout_Identical (In_T, T)
      then
         return Convert_Struct_Constant (V, T);

      --  Otherwise, assume this is a nonsymbolic constant and convert
      --  that way. (If it isn't, we'll get an assertion failure.)

      else
         return Convert_Nonsymbolic_Constant (V, T);
      end if;

   end Convert_Aggregate_Constant;

   --------------------------------
   -- Convert_Aggregate_Constant --
   --------------------------------

   function Convert_Aggregate_Constant
     (V : GL_Value; GT : GL_Type) return GL_Value
   is
      In_GT  : constant GL_Type  := Related_Type (V);
      In_V   : constant GL_Value :=
        (if Is_Padded_GL_Type (In_GT) then To_Primitive (V) else V);
      Out_GT : constant GL_Type  :=
        (if Is_Padded_GL_Type (GT) then Primitive_GL_Type (GT) else GT);
      Cvt_V  : constant GL_Value :=
          G (Convert_Aggregate_Constant (+In_V, Type_Of (Out_GT)), Out_GT);

   begin
      return (if Out_GT = GT then Cvt_V else From_Primitive (Cvt_V, GT));
   end Convert_Aggregate_Constant;

   -------------------------------
   -- Strip_Complex_Conversions --
   -------------------------------

   function Strip_Complex_Conversions
     (N : Opt_N_Subexpr_Id) return Opt_N_Subexpr_Id
   is
   begin
      return E : Opt_N_Subexpr_Id := N do
         while Present (E) loop
            exit when
              Nkind (E) not in N_Type_Conversion
                             | N_Unchecked_Type_Conversion
                             | N_Qualified_Expression
              or else Is_Elementary_Type (Full_Etype (E))
              or else Is_Elementary_Type (Full_Etype (Expression (E)))
              or else
                Get_Type_Size_Complexity (Full_GL_Type (E))
                  <= Get_Type_Size_Complexity (Full_GL_Type (Expression (E)));

            E := Expression (E);
         end loop;
      end return;
   end Strip_Complex_Conversions;

   -----------------------
   -- Strip_Conversions --
   -----------------------

   function Strip_Conversions (N : Opt_N_Subexpr_Id) return Opt_N_Subexpr_Id is
   begin
      return E : Opt_N_Subexpr_Id := N do
         while Present (E) loop
            exit when Nkind (E) not in N_Type_Conversion
                                     | N_Unchecked_Type_Conversion
                                     | N_Qualified_Expression;
            E := Expression (E);
         end loop;
      end return;
   end Strip_Conversions;

end GNATLLVM.Conversions;
