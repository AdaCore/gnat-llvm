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

with Errout;     use Errout;
with Snames;     use Snames;
with Stand;      use Stand;
with Table;      use Table;

with GNATLLVM.Arrays;      use GNATLLVM.Arrays;
with GNATLLVM.Blocks;      use GNATLLVM.Blocks;
with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.Exprs;       use GNATLLVM.Exprs;
with GNATLLVM.Records;     use GNATLLVM.Records;
with GNATLLVM.Subprograms; use GNATLLVM.Subprograms;
with GNATLLVM.Wrapper;     use GNATLLVM.Wrapper;

package body GNATLLVM.Types is

   --  We save pairs of GNAT type and LLVM Value_T for each level of
   --  processing of an Emit_LValue so we can find it if we have a
   --  self-referential item (a discriminated record).

   package LValue_Pair_Table is new Table.Table
     (Table_Component_Type => GL_Value,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "LValue_Pair_Table");
   --  Table of intermediate results for Emit_LValue

   LValue_Pair_First : Nat := 1;
   --  The current first entry in the above table.  See the below table.

   --  In the process of computing an LValue, we may need to compute
   --  another expression, e.g., an index or a bound, which may, in turn,
   --  compute another LValue.  So we need to have a stack to save and restore
   --  a starting pointer to the above table.

   package LValue_Stack is new Table.Table
     (Table_Component_Type => Nat,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 3,
      Table_Increment      => 2,
      Table_Name           => "LValue_Stack");

   function Is_In_LHS_Context (N : Node_Id) return Boolean;
   --  Return True if N's parent (if N is Present) is such that we need a
   --  LValue.

   function Is_Nop_Conversion (V : GL_Value; TE : Entity_Id) return Boolean
     with Pre => Is_Reference (V) and then Is_Type (TE);
   --  Return True if converting V to type TE won't change any bits

   function Is_Parent_Of (T_Need, T_Have : Entity_Id) return Boolean
     with Pre => Is_Type (T_Need) and then Is_Type (T_Have);
   --  True if T_Have is a parent type of T_Need

   function Get_Alloc_Size
     (TE, Alloc_Type : Entity_Id; V : GL_Value) return GL_Value
     with Pre => Is_Type (TE), Post => Present (Get_Alloc_Size'Result);
   --  Like Get_Type_Size, but used for the size to be allocated, so we
   --  include the size of the bounds in some array cases.

   function Move_Into_Memory
     (Temp       : GL_Value;
      V          : GL_Value;
      TE         : Entity_Id;
      Alloc_Type : Entity_Id) return GL_Value
     with Pre  => Present (Temp) and then Is_Type (TE)
                  and then Is_Type (Alloc_Type),
          Post => Is_Access_Type (Move_Into_Memory'Result);
   --  Temp is memory that was recently allocated.  Move Value, if
   --  present, into that allocated memory and return the allocated
   --  memory as a reference to type TE.  This is used by both type of
   --  memory allocators.  Temp can be of any type, either an integer
   --  or pointer to anything.  Alloc_Type is the type that was used
   --  to allocate the memory.

   -----------------------
   -- Build_Struct_Type --
   -----------------------

   function Build_Struct_Type
     (Types : Type_Array; Packed : Boolean := False) return Type_T is
   begin
      return Struct_Type_In_Context
        (Context, Types'Address, Types'Length, Packed);
   end Build_Struct_Type;

   -----------------------------------------------
   -- Are_Arrays_With_Different_Index_Types --
   -----------------------------------------------

   function Are_Arrays_With_Different_Index_Types
     (T1, T2 : Entity_Id) return Boolean
   is
      Idx1, Idx2 : Entity_Id;

   begin

      --  The front end should not have gotten us here if the number
      --  of dimensions differ.

      pragma Assert (Number_Dimensions (T1) = Number_Dimensions (T2));

      --  We don't need to do anything if the index types differ unless the
      --  corresponding LLVM types differ, so that's all we check.

      if Ekind (T2) = E_String_Literal_Subtype then
         return (Create_Type (Full_Etype (First_Index (T1)))
                   /= Create_Type (Standard_Integer));
      end if;

      Idx1 := First_Index (T1);
      Idx2 := First_Index (T2);
      while Present (Idx1) loop
         exit when
           Create_Type (Full_Etype (Idx1)) /= Create_Type (Full_Etype (Idx2));
         Next_Index (Idx1);
         Next_Index (Idx2);
      end loop;

      return Present (Idx1);
   end Are_Arrays_With_Different_Index_Types;

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

   -----------------------
   -- Is_Nop_Conversion --
   -----------------------

   function Is_Nop_Conversion (V : GL_Value; TE : Entity_Id) return Boolean is
   begin
      --  This is a no-op if the two LLVM types are the same or if both
      --  GNAT types aren't scalar types.

      return Type_Of (V) = Pointer_Type (Create_Type (TE), 0)
        or else (not Is_Scalar_Type (TE)
                   and then not Is_Scalar_Type (Related_Type (V)));

   end Is_Nop_Conversion;

   ---------------------
   -- Emit_Conversion --
   ---------------------

   function Emit_Conversion
     (N                   : Node_Id;
      TE                  : Entity_Id;
      From_N              : Node_Id := Empty;
      Is_Unchecked        : Boolean := False;
      Need_Overflow_Check : Boolean := False;
      Float_Truncate      : Boolean := False) return GL_Value
   is
      Result : GL_Value                 := Emit (N);
      In_TE  : constant Entity_Id       := Related_Type (Result);
      R      : constant GL_Relationship := Relationship (Result);

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

      --  If we're converting between two access subprogram access types
      --  and one is a foreign convention and one isn't, issue a warning
      --  since that can cause issues with nested subprograms.

      if Is_Access_Subprogram_Type (TE)
        and then Is_Access_Subprogram_Type (In_TE)
        and then Has_Foreign_Convention (TE) /= Has_Foreign_Convention (In_TE)
      then
         Error_Msg_Node_1 := In_TE;
         Error_Msg_Node_2 := TE;
         Error_Msg_N
           ("??conversion between subprogram access types of different , ", N);
         Error_Msg_N
           ("\conventions, & and &, will not work if the former points ", N);
         Error_Msg_N
           ("\to a subprogram that references parent variables.", N);
      end if;

      if Is_Elementary_Type (TE) and then Need_Overflow_Check then
         Result := Get (Result, Data);
         Emit_Overflow_Check (Result, From_N);
         Result := Convert (Result, TE);

      elsif Is_Reference (Result) and then Is_In_LHS_Context (From_N)
        and then Is_Nop_Conversion (Result, TE)
      then
         Result := Convert_Ref (Get (Result, Any_Reference), TE);

      --  For unchecked conversion between pointer and integer, just copy
      --  the bits.  But use Size_Type and generic pointers to make sure
      --  that any size changes are taken into account (they shouldn't be
      --  because of the rules of UC, but let's be conservative).

      elsif Is_Unchecked and then Is_Access_Type (In_TE)
        and then Is_Discrete_Or_Fixed_Point_Type (TE)
      then
         Result := Get (From_Access (Get (Result, Data)),
                        Reference_For_Integer);
         Result := Convert (Ptr_To_Int (Result, Size_Type), TE);
      elsif Is_Unchecked and then Is_Discrete_Or_Fixed_Point_Type (In_TE)
        and then Is_Access_Type (TE)
      then
         --  If TE is an access to unconstrained, this means that the
         --  address is to be taken as a thin pointer.  We also need special
         --  code in the case of access to subprogram.

         if Is_Unconstrained_Array (Full_Designated_Type (TE)) then
            Result :=
              Int_To_Relationship (Get (Result, Data),
                                   Full_Designated_Type (TE), Thin_Pointer);
         elsif Ekind (TE) = E_Access_Subprogram_Type then
            Result := Int_To_Relationship (Get (Result, Data),
                                           Full_Designated_Type (TE),
                                           Reference);
         else
            Result := Int_To_Ref (Get (Result, Data),
                                  Standard_Short_Short_Integer);
         end if;

         Result := Convert_To_Access (Result, TE);

      --  We can unchecked convert floating point of the same width
      --  (the only way that UC is formally defined) with a "bitcast"
      --  instruction.

      elsif Is_Unchecked
        and then ((Is_Floating_Point_Type (TE)
                     and then Is_Discrete_Or_Fixed_Point_Type (In_TE))
                  or else (Is_Discrete_Or_Fixed_Point_Type (TE)
                             and then Is_Floating_Point_Type (In_TE)))
        and then (ULL'(Get_LLVM_Type_Size_In_Bits (Create_Type (TE))) =
                    ULL'(Get_LLVM_Type_Size_In_Bits (Create_Type (In_TE))))
      then
         return Bit_Cast (Get (Result, Data), TE);

      --  If both types are elementary, hand that off to our helper

      elsif Is_Elementary_Type (In_TE)
        and then Is_Elementary_Type (TE)
      then
         Result := Convert (Get (Result, Data), TE,
                            Float_Truncate => Float_Truncate);

      --  If both types are the same, just change the type of the result.
      --  Avoid confusing [0 x T] as both a zero-size constrained type and
      --  the type used for a variable-sized type.

      elsif not Is_Reference (Result) and then not Is_Dynamic_Size (TE)
        and then Type_Of (Result) = Create_Type (TE)
      then
         Result := G_Is (Result, TE);

      --  If we have an undefined value that we're converting to another
      --  type, just get an undefined value of that type.  But watch for
      --  the case where we have Data of some fixed-size type and we're
      --  converting to a dynamic-sized type.  We handle the reference
      --  cases below since we may have to deal with materializing bounds.

      elsif Is_Undef (Result) and then R = Data
        and then not Is_Dynamic_Size (TE)
      then
         return Get_Undef (TE);

      --  If we have a constant of a struct type that we're converting to
      --  a struct of the same layout, we can make a new constant.

      elsif R = Data and then Is_Constant (Result)
        and then Get_Type_Kind (Type_Of (Result)) = Struct_Type_Kind
        and then Is_Record_Type (TE) and then not Is_Dynamic_Size (TE)
        and then Is_Layout_Identical (Result, TE)
      then
         return Convert_Struct_Constant (Result, TE);

      --  Otherwise, we do the same as an unchecked conversion.

      else
         Result := Convert_Ref (Get (Result, Any_Reference), TE);
      end if;

      --  For unchecked conversion, if the result is a non-biased
      --  integral type whose precision is not equal to its size, sign-
      --  or zero-extend the result.  But we need not do this if the
      --  input is also an integral type and both are unsigned or both
      --  are signed and the output is not narrower than the input and
      --  we can't do this in the case of nonbinary modulus.

      if Is_Unchecked and then Is_Discrete_Type (TE)
        and then not Non_Binary_Modulus (TE)
        and then RM_Size (TE) /= Esize (TE)
        and then not (Is_Discrete_Or_Fixed_Point_Type (In_TE)
                        and then Is_Unsigned_Type (TE)
                        and then Is_Unsigned_Type (In_TE))
        and then not (Is_Discrete_Or_Fixed_Point_Type (In_TE)
                        and then not Is_Unsigned_Type (In_TE)
                        and then not Is_Unsigned_Type (TE)
                        and then RM_Size (TE) >= RM_Size (In_TE))
      then
         declare
            Shift_Count : constant GL_Value  :=
              Const_Int (TE, Esize (TE) - RM_Size (TE));
            Left_Shift  : constant GL_Value :=
              Shl (Convert (Get (Result, Data), TE), Shift_Count);

         begin
            Result := (if Is_Unsigned_Type (TE)
                       then L_Shr (Left_Shift, Shift_Count)
                       else A_Shr (Left_Shift, Shift_Count));
         end;
      end if;

      return Result;

   end Emit_Conversion;

   -------------
   -- Convert --
   -------------

   function Convert
     (V              : GL_Value;
      TE             : Entity_Id;
      Float_Truncate : Boolean := False) return GL_Value
   is
      type Cvtf is access function
        (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value;

      Value       : GL_Value         := V;
      T           : constant Type_T  := Create_Type (TE);
      Src_Access  : constant Boolean := Is_Access_Type (V);
      Dest_Access : constant Boolean := Is_Access_Type (TE);
      Src_FP      : constant Boolean := Is_Floating_Point_Type (V);
      Dest_FP     : constant Boolean := Is_Floating_Point_Type (TE);
      Src_Uns     : constant Boolean := Is_Unsigned_Type (V);
      Dest_Uns    : constant Boolean := Is_Unsigned_Type (TE);
      Src_Size    : constant Nat     :=
        Nat (ULL'(Get_LLVM_Type_Size_In_Bits (V)));
      Dest_Usize  : constant Uint    :=
        (if Is_Modular_Integer_Type (TE) or else TE = Standard_Boolean
         then RM_Size (TE) else Esize (TE));
      Dest_Size   : constant Nat     := UI_To_Int (Dest_Usize);
      Is_Trunc    : constant Boolean := Dest_Size < Src_Size;
      Subp        : Cvtf             := null;

   begin
      --  If the value is already of the desired LLVM type, we're done.

      if Type_Of (V) = T then
         return G_Is (V, TE);

      --  If converting pointer to/from integer, copy the bits using the
      --  appropriate instruction.

      elsif Dest_Access and then Is_Integer_Type (V) then
         Subp := Int_To_Ptr'Access;
      elsif Is_Integer_Type (TE) and then Src_Access then
         Subp := Ptr_To_Int'Access;

      --  For pointer to pointer, call our helper

      elsif Src_Access and then Dest_Access then
         return Convert_To_Access (V, TE);

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
               Pred_Half  : constant GL_Value := Pred_FP (Const_Real (V, 0.5));
               Val_Is_Neg : constant GL_Value :=
                 F_Cmp (Real_OLT, V, Const_Null (V));
               Add_Amt    : constant GL_Value := F_Add (V, Pred_Half, "round");
               Sub_Amt    : constant GL_Value := F_Sub (V, Pred_Half, "round");

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

      --  Here all that's left to do is generate the IR instruction

      return Subp (Value, TE);

   end Convert;

   -----------------------
   -- Convert_To_Access --
   -----------------------

   function Convert_To_Access (V : GL_Value; TE : Entity_Id) return GL_Value is
      DT     : constant Entity_Id       := Full_Designated_Type (TE);
      As_Ref : constant GL_Value        :=
        (if Is_Access_Type (Related_Type (V)) then From_Access (V) else V);
      R      : constant GL_Relationship :=
        Relationship_For_Access_Type (TE);
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

      if Relationship (As_Ref) = Reference_To_Subprogram
        and then Ekind (TE) = E_Access_Subprogram_Type
      then
         Result := Get (Ptr_To_Relationship (As_Ref, DT, Reference), R);
      else
         Result := Convert_Pointer (Get (As_Ref, R), DT);
      end if;

      return To_Access (Result, TE);
   end Convert_To_Access;

   -----------------
   -- Convert_Ref --
   -----------------

   function Convert_Ref (V : GL_Value; TE : Entity_Id) return GL_Value is
      V_Type   : constant Entity_Id := Related_Type (V);
      Unc_Src  : constant Boolean   := Is_Access_Unconstrained (V);
      Unc_Dest : constant Boolean   := Is_Unconstrained_Array (TE);

   begin
      --  V is some type of reference to some type.  We want to
      --  convert it to be some type of reference to TE, which may be
      --  some other type (if it's the same, we have no work to do).  The
      --  relationship of the result to TE may or may not be the same as
      --  the relationship of V to its type.
      --
      --  We want to do as little work here as possible because we don't
      --  know what our caller will be doing with the result and want to
      --  avoid a situation where what we do has to be undone by our caller.
      --  However, the following must be true:
      --
      --  (1) The result must be SOME valid representation of TE
      --  (2) We must not lose any information, especially information that
      --      we can't recover, but should also not discard any information
      --      that we might conceivable need later if we can keep it
      --
      --  These principles dictate our behavior in all cases.  For example,
      --  if the input is a fat pointer, we should try to retain it as a
      --  fat pointer even if TE is constrained because we may want those
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
      --  with different index types and TE is unconstrained.  In that case,
      --  we have to materialize the bounds in the new index types.

      if Unc_Dest and then Is_Array_Type (V_Type)
        and then Are_Arrays_With_Different_Index_Types (TE, V_Type)
      then
         declare
            New_FP : constant GL_Value :=
              Get_Undef_Relationship (TE, Fat_Pointer);
            Bounds : constant GL_Value := Get_Array_Bounds (TE, V_Type, V);
            Data   : constant GL_Value :=
              Ptr_To_Relationship (Get (V, Reference), TE, Reference);

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
         return Ptr_To_Relationship (V, TE, Reference);

      --  If neither is constrained, but they aren't the same type, just do
      --  a pointer cast unless we have to convert between function access
      --  types that do and don't have static links.  If both are
      --  constrained, we return the input unchanged (the front end is
      --  responsible for this making sense).  Otherwise, we have to handle
      --  converting between fat and raw pointers.

      elsif not Unc_Src and not Unc_Dest then
         if Full_Designated_Type (V) = TE then
            return V;
         else
            return Ptr_To_Ref (V, TE);
         end if;

      elsif Unc_Src and then Unc_Dest then
         return Get (V, Fat_Pointer);

      elsif Unc_Src and then not Unc_Dest then
         return Convert_Ref (Get (V, Reference), TE);
      else
         pragma Assert (not Unc_Src and then Unc_Dest);

         --  ???  The code here is wrong.  See, e.g., c46104a.  If we're
         --  converting between arrays with different types for bounds,
         --  we need to use the new bounds and not just UC the bound
         --  reference, since that won't work.  But there doesn't seem
         --  to be an obvious way to fix it at the moment and this
         --  whole function needs to be rewritten anyway.

         return Convert_Pointer (Get (V, Fat_Pointer), TE);
      end if;
   end Convert_Ref;

   ---------------------
   -- Convert_Pointer --
   ---------------------

   function Convert_Pointer (V : GL_Value; TE : Entity_Id) return GL_Value is
      R     : constant GL_Relationship := Relationship (V);
      T     : constant Type_T          := Type_For_Relationship (TE, R);
      Value : Value_T;

   begin
      if Type_Of (V) = T then
         return V;

      --  If the input is an actual pointer, convert it

      elsif Get_Type_Kind (T) = Pointer_Type_Kind then
         return Ptr_To_Relationship (V, TE, R);
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

      return G (Value, TE, R);
   end Convert_Pointer;

   ----------------------
   -- Push_LValue_List --
   ----------------------

   procedure Push_LValue_List is
   begin
      LValue_Stack.Append (LValue_Pair_First);
      LValue_Pair_First := LValue_Pair_Table.Last + 1;
   end Push_LValue_List;

   ---------------------
   -- Pop_LValue_List --
   ---------------------

   procedure Pop_LValue_List is
   begin
      LValue_Pair_Table.Set_Last (LValue_Pair_First - 1);
      LValue_Pair_First := LValue_Stack.Table (LValue_Stack.Last);
      LValue_Stack.Decrement_Last;
   end Pop_LValue_List;

   ------------------------
   --  Clear_LValue_List --
   ------------------------

   procedure Clear_LValue_List is
   begin
      LValue_Pair_Table.Set_Last (LValue_Pair_First - 1);
   end Clear_LValue_List;

   -------------------------
   --  Add_To_LValue_List --
   -------------------------

   procedure Add_To_LValue_List (V : GL_Value) is
   begin
      LValue_Pair_Table.Append (V);
   end Add_To_LValue_List;

   -------------------------
   --  Add_To_LValue_List --
   -------------------------

   function Add_To_LValue_List (V : GL_Value) return GL_Value is
   begin
      Add_To_LValue_List (V);
      return V;
   end Add_To_LValue_List;

   ------------------
   -- Is_Parent_Of --
   ------------------

   function Is_Parent_Of (T_Need, T_Have : Entity_Id) return Boolean is
   begin
      --  If the two types are the same return True.  Likewise if
      --  T_Have has a parent different than itself and that and this
      --  relation holds for that.

      if T_Need = T_Have then
         return True;
      elsif Ekind (T_Have) = E_Record_Type
        and then Full_Etype (T_Have) /= T_Have
      then
         return Is_Parent_Of (T_Need, Full_Etype (T_Have));
      else
         return False;
      end if;

   end Is_Parent_Of;

   ------------------------
   -- Get_Matching_Value --
   ------------------------

   function Get_Matching_Value (TE : Entity_Id) return GL_Value is
   begin
      --  Check in the opposite order of what we push.  We may, for example
      --  be finding the size of an object of that size, in which case the
      --  object will have been added last.

      for J in reverse LValue_Pair_First .. LValue_Pair_Table.Last loop
         if Is_Parent_Of (T_Need => Implementation_Base_Type (TE),
                          T_Have => Implementation_Base_Type
                            (LValue_Pair_Table.Table (J).Typ))
           or else Is_Parent_Of (T_Have => Implementation_Base_Type (TE),
                                 T_Need => Implementation_Base_Type
                                   (LValue_Pair_Table.Table (J).Typ))
         then
            return Convert_Ref (LValue_Pair_Table.Table (J), TE);
         end if;
      end loop;

      --  Should never get here and postcondition verifies

      return No_GL_Value;
   end Get_Matching_Value;

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
         exit when Get_Type_Size_Complexity (Full_Etype (E))
           <= Get_Type_Size_Complexity (Full_Etype (Expression (E)));
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

   ----------------------
   -- Bounds_To_Length --
   ----------------------

   function Bounds_To_Length
     (In_Low, In_High : GL_Value; TE : Entity_Id) return GL_Value
   is
      Low      : constant GL_Value := Convert (In_Low, TE);
      High     : constant GL_Value := Convert (In_High, TE);
      Cmp_Kind : constant Int_Predicate_T :=
        (if Is_Unsigned_Type (TE) then Int_UGT else Int_SGT);
      Is_Empty : constant GL_Value := I_Cmp (Cmp_Kind, Low, High, "is-empty");
      Const_1  : constant GL_Value := Const_Int (TE, Uint_1);
   begin
      return Build_Select
        (C_If   => Is_Empty,
         C_Then => Const_Null (TE),
         C_Else =>
           (if Low = Const_1 then High
            else NSW_Add (NSW_Sub (High, Low), Const_1)));
   end Bounds_To_Length;

   --------------------------------
   -- Get_LLVM_Type_Size_In_Bits --
   --------------------------------

   function Get_LLVM_Type_Size_In_Bits (TE : Entity_Id) return GL_Value
   is
      LLVM_Type : constant Type_T := Create_Type (TE);

   begin
      pragma Assert (not Is_Dynamic_Size (TE));
      return Get_LLVM_Type_Size_In_Bits (LLVM_Type);
   end Get_LLVM_Type_Size_In_Bits;

   ------------------------
   -- Ultimate_Base_Type --
   ------------------------

   function Ultimate_Base_Type (TE : Entity_Id) return Entity_Id is
   begin
      return Typ : Entity_Id := TE do
         while Etype (Typ) /= Typ loop
            Typ := Etype (Typ);
         end loop;
      end return;
   end Ultimate_Base_Type;

   ----------------------
   -- Get_Fullest_View --
   ----------------------

   function Get_Fullest_View
     (TE : Entity_Id; Include_PAT : Boolean := True) return Entity_Id is
   begin
      --  Strictly speaking, the recursion below isn't necessary, but
      --  it's both simplest and safest.

      case Ekind (TE) is
         when Incomplete_Kind =>
            if From_Limited_With (TE) then
               return Get_Fullest_View (Non_Limited_View (TE), Include_PAT);
            elsif Present (Full_View (TE)) then
               return Get_Fullest_View (Full_View (TE), Include_PAT);
            end if;

         when Private_Kind =>
            if Present (Underlying_Full_View (TE)) then
               return
                 Get_Fullest_View (Underlying_Full_View (TE), Include_PAT);
            elsif Present (Full_View (TE)) then
               return Get_Fullest_View (Full_View (TE), Include_PAT);
            else
               return Get_Fullest_View (Etype (TE), Include_PAT);
            end if;

         when Array_Kind =>
            if Include_PAT and then Present (Packed_Array_Impl_Type (TE)) then
               return Get_Fullest_View (Packed_Array_Impl_Type (TE));
            end if;

         when E_Record_Subtype =>
            if Present (Cloned_Subtype (TE)) then
               return Get_Fullest_View (Cloned_Subtype (TE), Include_PAT);
            end if;

         when E_Class_Wide_Type =>
            return Get_Fullest_View (Root_Type (TE), Include_PAT);

         when  E_Class_Wide_Subtype =>
            if Present (Equivalent_Type (TE)) then
               return Get_Fullest_View (Equivalent_Type (TE), Include_PAT);
            elsif Present (Cloned_Subtype (TE)) then
               return Get_Fullest_View (Cloned_Subtype (TE), Include_PAT);
            end if;

         when E_Protected_Type | E_Protected_Subtype
            | E_Task_Type |  E_Task_Subtype =>
            if Present (Corresponding_Record_Type (TE)) then
               return Get_Fullest_View (Corresponding_Record_Type (TE),
                                        Include_PAT);
            end if;

         when E_Access_Protected_Subprogram_Type
            | E_Anonymous_Access_Protected_Subprogram_Type =>
            if Present (Equivalent_Type (TE)) then
               return Get_Fullest_View (Equivalent_Type (TE), Include_PAT);
            end if;

         when E_Access_Subtype =>
            return Get_Fullest_View (Base_Type (TE), Include_PAT);

         when others =>
            null;
      end case;

      return TE;
   end Get_Fullest_View;

   ------------------------
   -- Create_Access_Type --
   ------------------------

   function Create_Access_Type (TE : Entity_Id) return Type_T
   is
      T : constant Type_T := Create_Type (TE);

   begin
      if Is_Unconstrained_Array (TE) then
         return Create_Array_Fat_Pointer_Type (TE);
      elsif Ekind (TE) = E_Subprogram_Type then
         return Create_Subprogram_Access_Type;
      else
         return Pointer_Type (T, 0);
      end if;
   end Create_Access_Type;

   -----------------------
   -- GNAT_To_LLVM_Type --
   -----------------------

   function GNAT_To_LLVM_Type
     (TE : Entity_Id; Definition : Boolean) return Type_T
   is
      T         : Type_T     := No_Type_T;
      TBAA      : Metadata_T := No_Metadata_T;
      Def_Ident : Entity_Id;
      pragma Unreferenced (Definition);

   begin
      --  See if we already have a type.  If so, we must not be defining
      --  this type.  ??? But we can't add that test just yet.

      T := Get_Type (TE);
      if Present (T) then
         --  ??? pragma Assert (not Definition);
         return T;
      end if;

      --  See if we can get the type from the fullest view.
      --  ??? This isn't quite right in the case where we're not
      --  defining the type, or where there's a Freeze_Node, but add this
      --  logic later.

      Def_Ident := Get_Fullest_View (TE);
      if Def_Ident /= TE then
         T := GNAT_To_LLVM_Type (Def_Ident, False);
         Copy_Type_Info (Def_Ident, TE);
         if Is_Record_Type (Def_Ident)
           and then Is_Incomplete_Or_Private_Type (TE)
         then
            Copy_Field_Info (Def_Ident, TE);
         end if;

         return T;
      end if;

      --  ??? This probably needs to be cleaned up, but before we do anything,
      --  see if this isn't a base type and process that if so.

      if Base_Type (Def_Ident) /= Def_Ident then
         Discard (GNAT_To_LLVM_Type (Base_Type (Def_Ident), False));
      end if;

      case Ekind (Def_Ident) is
         when Discrete_Kind =>

            --  LLVM is expecting boolean expressions to be of size 1
            --  ??? will not work properly if there is a size clause
            --  Also avoid using 0-sized type for "mod 1" type (c420001).

            if Is_Boolean_Type (Def_Ident) then
               T := Int_Ty (1);
            elsif Is_Modular_Integer_Type (Def_Ident)
              and then RM_Size (Def_Ident) /= Uint_0
            then
               T := Int_Ty (RM_Size (Def_Ident));
            elsif Esize (Def_Ident) /= Uint_0 then
               T := Int_Ty (Esize (Def_Ident));
            else
               T := Int_Ty (8);
            end if;

         when E_Floating_Point_Type | E_Floating_Point_Subtype =>
            declare
               Float_Type : constant Node_Id
                 := Full_Etype (Full_Etype (Def_Ident));
               Size       : constant Uint := Esize (Float_Type);

            begin
               case Float_Rep (Float_Type) is
                  when IEEE_Binary =>
                     pragma Assert (UI_Is_In_Int_Range (Size));
                     case UI_To_Int (Size) is
                        when 32 =>
                           T := Float_Type_In_Context (Context);
                        when 64 =>
                           T := Double_Type_In_Context (Context);
                        when 128 =>
                           --  Extended precision; not IEEE_128
                           T := X86_F_P80_Type_In_Context (Context);
                        when 80 | 96 =>
                           T := X86_F_P80_Type_In_Context (Context);
                        when others =>
                           --  ??? Double check that
                           T := F_P128_Type_In_Context (Context);
                     end case;

                  when AAMP =>
                     --  Not supported
                     Error_Msg_N ("unsupported floating point type", TE);
                     T := Void_Type;
               end case;
            end;

         when Access_Kind =>
            T := Type_For_Relationship
              (Full_Designated_Type (Def_Ident),
               Relationship_For_Access_Type (Def_Ident));

         when Record_Kind =>
            T := Create_Record_Type (Def_Ident);

         when Array_Kind =>
            T := Create_Array_Type (Def_Ident);

         when E_Subprogram_Type =>
            T := Create_Subprogram_Type (Def_Ident);

         when Fixed_Point_Kind =>
            T := Int_Ty (Esize (Def_Ident));

         when E_Incomplete_Type =>
            --  This is a Taft Amendment type, return a dummy type that
            --  we can take a pointer to.

            T := Struct_Create_Named (Context, Get_Name (Def_Ident));

         when E_Private_Type
            | E_Private_Subtype
            | E_Limited_Private_Type
            | E_Limited_Private_Subtype
         =>
            T := Create_Type (Full_Etype (Def_Ident));

         when others =>
            Error_Msg_N
              ("unsupported type kind: `"
               & Ekind (Def_Ident)'Image & "`", Def_Ident);
            raise Program_Error;
      end case;

      --  Now save the result, if we have one, and compute any TBAA
      --  information.

      if Present (T) then
         Set_Type (TE, T);
         TBAA := Create_TBAA (TE);
         if Present (TBAA) then
            Set_TBAA (TE, TBAA);
         end if;
      end if;

      --  If this is a packed array implementation type and the original
      --  type is an array, set information about the bounds of the
      --  original array.

      if Is_Packed_Array_Impl_Type (Def_Ident) then
         Discard (Create_Array_Type (Def_Ident, For_Orig => True));
      end if;

      return T;
   end GNAT_To_LLVM_Type;

   -----------------
   -- Create_TBAA --
   -----------------

   function Create_TBAA (TE : Entity_Id) return Metadata_T is
   begin
      if Ekind_In (TE, E_Signed_Integer_Type, E_Modular_Integer_Type,
                   E_Floating_Point_Type)
      then
         return Create_TBAA_Scalar_Type_Node
           (MD_Builder, Get_Name (TE), TBAA_Root);
      else
         return No_Metadata_T;
      end if;
   end Create_TBAA;

   ----------------------
   -- Bounds_From_Type --
   ----------------------

   procedure Bounds_From_Type (TE : Entity_Id; Low, High : out GL_Value)
   is
      SRange : constant Node_Id := Scalar_Range (TE);

   begin
      pragma Assert (Nkind_In (SRange, N_Range,
                               N_Signed_Integer_Type_Definition));

      Low  := Emit_Convert_Value (Low_Bound (SRange), TE);
      High := Emit_Convert_Value (High_Bound (SRange), TE);

   end Bounds_From_Type;

   ----------------------
   -- Move_Into_Memory --
   ----------------------

   function Move_Into_Memory
     (Temp       : GL_Value;
      V          : GL_Value;
      TE         : Entity_Id;
      Alloc_Type : Entity_Id) return GL_Value
   is
      R      : constant GL_Relationship := Relationship_For_Alloc (TE);
      Copied : Boolean                  := False;
      Memory : GL_Value                 :=
        (if Is_Access_Type (Temp)
         then Ptr_To_Relationship (Temp, Alloc_Type, R)
         else Int_To_Relationship (Temp, Alloc_Type, R));
      New_V  : GL_Value;

   begin
      --  If this is to get bounds and data and we have a value to store
      --  which contains data, convert it to bounds and data and store it.
      --  Otherwise, we have two cases, depending on the reason that we
      --  have bounds because Emit_Assignment only can handle the
      --  nominal type for alias to unconstrained case.

      if R = Reference_To_Bounds_And_Data then
         if Present (V) and then not Is_Reference (V) then
            New_V := Get (V, Bounds_And_Data);
            Store (New_V, Ptr_To_Relationship (Memory, New_V, R));
            Copied := True;
         elsif not Is_Constrained (TE) then
            Store (Get_Array_Bounds (TE, Alloc_Type, V),
                   Get (Memory, Reference_To_Bounds));
         end if;

         Memory := Get (Memory, Thin_Pointer);
      end if;

      --  If we have a value to move into memory, move it

      if Present (V) and then not Copied then
         Emit_Assignment (Memory, Empty, V, True, True);
      end if;

      return Convert_Ref (Memory, TE);
   end Move_Into_Memory;

   -----------------------
   -- Allocate_For_Type --
   -----------------------

   function Allocate_For_Type
     (TE         : Entity_Id;
      Alloc_Type : Entity_Id;
      N          : Node_Id;
      V          : GL_Value := No_GL_Value;
      Name       : String := "") return GL_Value
   is
      Max_Alloc   : constant ULL := 10_000_000;
      Element_Typ : Entity_Id;
      Num_Elts    : GL_Value;
   begin
      --  We have three cases.  If the object is not of a dynamic size,
      --  we just do the alloca and that's all.

      if not Is_Dynamic_Size (Alloc_Type) then
         if Do_Stack_Check
           and then Get_LLVM_Type_Size (Create_Type (Alloc_Type)) > Max_Alloc
         then
            Emit_Raise_Call (N, SE_Object_Too_Large);
            return Get_Undef_Ref (TE);
         else
            return
              Move_Into_Memory (Alloca (Alloc_Type, Name), V, TE, Alloc_Type);
         end if;
      end if;

      --  Otherwise, we probably have to do some sort of dynamic
      --  allocation.  If this is an array of a component that's not of
      --  dynamic size, then we can allocate an array of the component type
      --  corresponding to the array type and cast it to a pointer to the
      --  actual type.  If not, we have to allocate it as an array of
      --  bytes.  We must use an array of bytes if we have to include bounds.

      if Is_Array_Type (Alloc_Type)
        and then not Is_Dynamic_Size (Full_Component_Type (Alloc_Type))
        and then not Is_Constr_Subt_For_UN_Aliased (Alloc_Type)
      then
         Element_Typ := Full_Component_Type (Alloc_Type);
         Num_Elts    := Get_Array_Elements (V, Alloc_Type, For_Type => No (V));
      else
         Element_Typ := Standard_Short_Short_Integer;
         Num_Elts    := Get_Alloc_Size (Alloc_Type, Alloc_Type, V);
      end if;

      --  Check that we aren't trying to allocate too much memory.  Raise
      --  Storage_Error if so.  We don't try to support local exception
      --  labels and -fstack-check at the same time.  The divide below
      --  will constant-fold, but make sure we aren't dividing by zero.

      if Do_Stack_Check
        and then Get_Type_Size (Element_Typ) /= Size_Const_Null
      then
         Emit_Raise_Call_If (I_Cmp (Int_UGT, Num_Elts,
                                    U_Div (Size_Const_Int (Max_Alloc),
                                           Get_Type_Size (Element_Typ))),
                             N, SE_Object_Too_Large);
      end if;

      return Move_Into_Memory
        (Array_Alloca (Element_Typ, Num_Elts, Name), V, TE, Alloc_Type);

   end Allocate_For_Type;

   ----------------------------
   -- Heap_Allocate_For_Type --
   ----------------------------

   function Heap_Allocate_For_Type
     (TE         : Entity_Id;
      Alloc_Type : Entity_Id;
      V          : GL_Value  := No_GL_Value;
      Proc       : Entity_Id := Empty;
      Pool       : Entity_Id := Empty) return GL_Value
   is
      Size       : constant GL_Value  := Get_Alloc_Size (TE, Alloc_Type, V);
      Align      : constant unsigned  := Get_Type_Alignment (Alloc_Type);
      Align_V    : constant GL_Value  := Size_Const_Int (Align);
      Result     : GL_Value;

   begin
      --  If no function was specified, use the default memory allocation
      --  function, where we just pass a size.

      if No (Proc) then
         Result := Call (Get_Default_Alloc_Fn, Standard_A_Char, (1 => Size));

      --  If a procedure was specified (meaning that a pool must also have
      --  been specified) and the pool is a record, then it's a storage
      --  pool and we pass the pool, size, and alignment. Be sure that we
      --  convert the pool to actual type of the formal of the deallocator
      --  function: it may be a derived type.

      elsif Is_Record_Type (Full_Etype (Pool)) then
         Result :=
           Call_Alloc (Proc,
                       (1 => Ptr_To_Ref (Emit_Safe_LValue (Pool),
                                         Full_Etype (First_Formal (Proc))),
                        2 => Size, 3 => Align_V));

      --  Otherwise, this is the secondary stack and we just call with size

      else
         Result := Call_Alloc (Proc, (1 => Size));
      end if;

      --  If we're doing this for an unconstrained array, we have the pointer
      --  to the raw array, not a fat pointer.

      return Move_Into_Memory (Result, V, TE, Alloc_Type);
   end Heap_Allocate_For_Type;

   ---------------------
   -- Heap_Deallocate --
   ---------------------

   procedure Heap_Deallocate (V : GL_Value; Proc : Entity_Id; Pool : Entity_Id)
   is
      DT          : constant Entity_Id := Full_Designated_Type (V);
      Size        : constant GL_Value  := Get_Type_Size (DT, From_Access (V));
      Align       : constant unsigned  := Get_Type_Alignment (DT);
      Align_V     : constant GL_Value  := Size_Const_Int (Align);
      Converted_V : GL_Value           := V;

   begin
      --  If V is an access type, convert it to a reference to the
      --  underlying data.

      if Is_Access_Type (V) and then Relationship (V) = Data then
         Converted_V := From_Access (V);
      end if;

      --  If V is an unconstrained array, we want a pointer to the bounds
      --  and data.  Otherwise just a Reference.  We'll then either convert
      --  it to a generic pointer or to an integer (System.Address).

      Converted_V := Get (Converted_V,
                          Relationship_For_Alloc (Related_Type (Converted_V)));

      --  If no subprogram was specified, use the default memory deallocation
      --  procedure, where we just pass the object.

      if No (Proc) then
         Call (Get_Default_Free_Fn,
               (1 => Pointer_Cast (Converted_V, Standard_A_Char)));

      --  If a procedure was specified (meaning that a pool must also
      --  have been specified) and the pool is a record, then it's a
      --  storage pool and we pass the pool, size, and alignment.  Be
      --  sure that we convert the pool to actual type of the formal of
      --  the deallocator function: it may be a derived type.

      elsif Is_Record_Type (Full_Etype (Pool)) then
         Call_Dealloc (Proc,
               (1 => Ptr_To_Ref (Emit_Safe_LValue (Pool),
                                 Full_Etype (First_Formal (Proc))),
                2 => Ptr_To_Size_Type (Converted_V),
                3 => Size, 4 => Align_V));

      --  Otherwise, this is the secondary stack and we just call with size

      else
         Call_Dealloc (Proc, (1 => Ptr_To_Size_Type (Converted_V), 2 => Size));
      end if;
   end Heap_Deallocate;

   --------------
   -- Align_To --
   --------------

   function Align_To (V, Cur_Align, Must_Align : GL_Value) return GL_Value is
   begin
      --  If both alignments are constant and we can determine that we
      --  needn't do any alignment, do nothing.  Otherwise, align.

      if Is_A_Const_Int (Cur_Align) and then Is_A_Const_Int (Must_Align)
        and then (Get_Const_Int_Value (Must_Align)
                    <= Get_Const_Int_Value (Cur_Align))
      then
         return V;
      else
         return Build_And (NSW_Add (V,
                                    NSW_Sub (Must_Align,
                                             Size_Const_Int (Uint_1))),
                           NSW_Neg (Must_Align));
      end if;
   end Align_To;

   ------------------------
   -- Get_Type_Alignment --
   ------------------------

   function Get_Type_Alignment (TE : Entity_Id) return unsigned is
      Largest_Align : unsigned  := 1;
      Field         : Entity_Id;

   begin
      --  If it's an array, it's the alignment of the component type

      if Is_Array_Type (TE) then
         return Get_Type_Alignment (Full_Component_Type (TE));

      --  Otherwise, if a record, use the highest alignment of any field

      elsif Is_Record_Type (TE) then
         Field := First_Entity (TE);
         while Present (Field) loop
            if Ekind_In (Field, E_Discriminant, E_Component) then
               Largest_Align
                 := unsigned'Max (Largest_Align,
                                  Get_Type_Alignment (Full_Etype (Field)));
            end if;

            Next_Entity (Field);
         end loop;

         return Largest_Align;

      --  Otherwise, it must be an elementary type, so get the LLVM type's
      --  alignment

      else
         return Get_Type_Alignment (Create_Type (TE));

      end if;
   end Get_Type_Alignment;

   -------------------
   -- Get_Type_Size --
   -------------------

   function Get_Type_Size
     (TE       : Entity_Id;
      V        : GL_Value := No_GL_Value;
      For_Type : Boolean  := False) return GL_Value
   is
   begin
      --  If a value was specified and it's not a reference, then it
      --  must be of a fixed size.  That's the size we're looking for.

      if Present (V) and then not Is_Access_Type (V) then
         return Get_LLVM_Type_Size (Type_Of (V));
      elsif Is_Record_Type (TE) then
         return Get_Record_Type_Size (TE, V, For_Type);
      elsif Is_Array_Type (TE) and then Is_Dynamic_Size (TE) then
         return Get_Array_Type_Size (TE, V, For_Type);
      else
         return Get_LLVM_Type_Size (Create_Type (TE));
      end if;

   end Get_Type_Size;

   --------------------
   -- Get_Alloc_Size --
   --------------------

   function Get_Alloc_Size
     (TE, Alloc_Type : Entity_Id; V : GL_Value) return GL_Value
   is
      Size : GL_Value :=
        Get_Type_Size (Alloc_Type,
                       (if   Is_Class_Wide_Equivalent_Type (Alloc_Type)
                        then No_GL_Value else V),
                       For_Type => No (V) and then not Is_Constrained (TE));

   begin
      --  Adjust size if constrained subtype for aliased unconstrained or
      --  for unconstrained itself.

      if Is_Unconstrained_Array (TE)
        or else Type_Needs_Bounds (Alloc_Type)
      then
         Size := NSW_Add (Size, Get_Bound_Size (TE));
      end if;

      return Size;
   end Get_Alloc_Size;

   ------------------
   -- Compute_Size --
   ------------------

   function Compute_Size
     (Left_Type, Right_Type   : Entity_Id;
      Left_Value, Right_Value : GL_Value) return GL_Value is

   begin
      --  Use the type of right side unless its complexity is more
      --  than that of the size of the type on the left side.  If the
      --  LHS is a class wide equivalent type, we must use it.

      if Get_Type_Size_Complexity (Right_Type) >
        Get_Type_Size_Complexity (Left_Type)
        or else Is_Class_Wide_Equivalent_Type (Left_Type)
      then
         return Get_Type_Size (Left_Type, Left_Value);
      else
         return Get_Type_Size (Right_Type, Right_Value);
      end if;

   end Compute_Size;

   ------------------------------
   -- Get_Type_Size_Complexity --
   ------------------------------

   function Get_Type_Size_Complexity
     (TE : Entity_Id; For_Type : Boolean := False) return Nat is
   begin

      if Is_Record_Type (TE) then
         return Get_Record_Size_Complexity (TE, For_Type);
      elsif Is_Array_Type (TE) then
         return Get_Array_Size_Complexity (TE, For_Type);

      else
         --  All other types are constant size

         return 0;

      end if;
   end Get_Type_Size_Complexity;

   ----------------------------------
   -- Add_Type_Data_To_Instruction --
   ----------------------------------

   procedure Add_Type_Data_To_Instruction (Inst : Value_T; TE : Entity_Id)
   is
      TBAA : constant Metadata_T := Get_TBAA (Implementation_Base_Type (TE));
   begin
      if Is_Volatile (TE) then
         Set_Volatile (Inst);
      end if;

      if Present (TBAA) then
         Add_TBAA_Access
           (Inst, Create_TBAA_Access_Tag (MD_Builder, TBAA, TBAA, 0));
      end if;
   end Add_Type_Data_To_Instruction;

end GNATLLVM.Types;
