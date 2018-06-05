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

with Errout; use Errout;
with Stand;  use Stand;
with Table;  use Table;

with GNATLLVM.Arrays;      use GNATLLVM.Arrays;
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
        (LLVM_Context, Types'Address, Types'Length, Packed);
   end Build_Struct_Type;

   -----------------------------------------------
   -- Are_Arrays_With_Different_Index_Types --
   -----------------------------------------------

   function Are_Arrays_With_Different_Index_Types
     (T1, T2 : Entity_Id) return Boolean
   is
      Idx1, Idx2 : Entity_Id;

   begin

      --  The front end should not have gotten us here if the component
      --  types or number of dimensions differ.

      pragma Assert (Full_Component_Type (T1) = Full_Component_Type (T2)
                       and then (Number_Dimensions (T1) =
                                   Number_Dimensions (T2)));

      --  We don't need to do anything if the index types differ unless the
      --  corresponding LLVM types differ, so that's all we check.

      if Ekind (T2) = E_String_Literal_Subtype then
         return (Create_Type (Full_Etype (First_Index (T1)))
                   /= Create_Type (Standard_Integer));
      end if;

      Idx1 := First_Index (T1);
      Idx2 := First_Index (T2);
      while Present (Idx1) loop
         if Create_Type (Full_Etype (Idx1)) /= Create_Type (Full_Etype (Idx2))
         then
            return True;
         end if;

         Next_Index (Idx1);
         Next_Index (Idx2);
      end loop;

      return False;
   end Are_Arrays_With_Different_Index_Types;

   ---------------------------
   -- Build_Type_Conversion --
   ---------------------------

   function Build_Type_Conversion
     (N : Node_Id; TE : Entity_Id) return GL_Value is

   begin
      --  If both types are elementary, hand that off to our helper.

      if Is_Elementary_Type (Full_Etype (N))
        and then Is_Elementary_Type (TE)
      then
         return Convert_To_Elementary_Type (Emit_Expression (N), TE);

      --  Otherwise, we do the same as an unchecked conversion.

      else
         return Build_Unchecked_Conversion (N, TE);

      end if;
   end Build_Type_Conversion;

   --------------------------------
   -- Convert_To_Elementary_Type --
   --------------------------------

   function Convert_To_Elementary_Type
     (V : GL_Value; TE : Entity_Id) return GL_Value
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
         --  floating-point numbers.

         --  The reason to use the same constant with subtract/add instead
         --  of a positive and negative constant is to allow the comparison
         --  to be scheduled in parallel with retrieval of the constant and
         --  conversion of the input to the calc_type (if necessary).

         --  The easiest way of computing the constant is to do it at
         --  compile-time by finding the correct floating-point type to use.

         declare
            Size_In_Bits : constant ULL   := Get_LLVM_Type_Size_In_Bits (V);
            PredHalf     : constant Long_Long_Float :=
              (if Long_Long_Float'Size = Size_In_Bits
               then Long_Long_Float'Pred (0.5)
               elsif Long_Float'Size = Size_In_Bits
               then Long_Long_Float (Long_Float'Pred (0.5))
               elsif Float'Size = Size_In_Bits
               then Long_Long_Float (Float'Pred (0.5))
               else Long_Long_Float (Short_Float'Pred (0.5)));
            Val_Neg    : constant GL_Value :=
              F_Cmp (Real_OLT, V, Const_Real (V, 0.0));
            Adjust_Amt : constant GL_Value :=
                Const_Real (V, Interfaces.C.double (PredHalf));
            --  ??? The conversion to "double" above may be problematic,
            --  but it's not clear how else to get the constant to LLVM.

            Add_Amt    : constant GL_Value := F_Add (V, Adjust_Amt, "round");
            Sub_Amt    : constant GL_Value := F_Sub (V, Adjust_Amt, "round");

         begin
            Value := Build_Select (Val_Neg, Sub_Amt, Add_Amt);
         end;

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

   end Convert_To_Elementary_Type;

   -----------------------
   -- Convert_To_Access --
   -----------------------

   function Convert_To_Access (V : GL_Value; TE : Entity_Id) return GL_Value is
      DT     : constant Entity_Id       := Full_Designated_Type (TE);
      As_Ref : constant GL_Value        :=
        (if Is_Access_Type (Related_Type (V)) then From_Access (V) else V);
      R      : constant GL_Relationship :=
        Relationship_For_Access_Type (TE);

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
         return To_Access (Ptr_To_Relationship (As_Ref,
                                                Full_Designated_Type (TE),
                                                Reference),
                           TE);
      else
         return To_Access (Convert_Pointer (Get (As_Ref, R), DT), TE);
      end if;
   end Convert_To_Access;

   --------------------------
   -- Convert_To_Access_To --
   --------------------------

   function Convert_To_Access_To
     (V : GL_Value; TE : Entity_Id) return GL_Value
   is
      V_Type   : constant Entity_Id := Related_Type (V);
      Unc_Src  : constant Boolean   := Is_Access_Unconstrained (V);
      Unc_Dest : constant Boolean   := Is_Unconstrained_Array (TE);

   begin
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
         return Ptr_To_Ref (V, TE);

      --  If neither is constrained, but they aren't the same type, just do
      --  a pointer cast unless we have to convert between function access
      --  types that do and don't have static links.  If both are
      --  constrained, we return the input unchanged (the front end is
      --  responsible for this making sense).  Otherwise, we have to handle
      --  converting between fat and raw pointers.

      elsif not Unc_Src and not Unc_Dest then
         if Full_Designated_Type (V) = TE then
            return V;
         elsif Needs_Activation_Record (Full_Designated_Type (V))
           and then not Needs_Activation_Record (TE)
         then
            return Ptr_To_Ref (Extract_Value (Standard_A_Char, V, 1), TE);
         elsif not Needs_Activation_Record (Full_Designated_Type (V))
           and then Needs_Activation_Record (TE)
         then
            return Insert_Value
              (Insert_Value (Get_Undef_Ref (TE),
                             Const_Null (Standard_A_Char), 1),
               Pointer_Cast (V, Standard_A_Char), 0);
         else
            return Ptr_To_Ref (V, TE);
         end if;

      elsif Unc_Src and then Unc_Dest then
         return Get (V, Fat_Pointer);

      elsif Unc_Src and then not Unc_Dest then
         return Convert_To_Access_To (Get (V, Reference), TE);
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
   end Convert_To_Access_To;

   --------------------------------
   -- Build_Unchecked_Conversion --
   --------------------------------

   function Build_Unchecked_Conversion
     (N : Node_Id; TE : Entity_Id) return GL_Value
   is
      T : constant Type_T := Create_Type (TE);
      V :  GL_Value       := Emit_Expression (N);

   begin
      --  If the value is already of the desired LLVM type, we're done.

      if Type_Of (V) = Create_Type (TE) then
         return V;

      --  If converting pointer to pointer or pointer to/from integer, we
      --  just copy the bits using the appropriate instruction.

      elsif Is_Access_Type (TE) and then not Is_Access_Unconstrained (TE)
        and then Is_Scalar_Type (V)
      then
         return Int_To_Ptr (V, TE);
      elsif Is_Scalar_Type (TE) and then Is_Access_Type (V)
        and then not Is_Access_Unconstrained (V)
      then
         return Ptr_To_Int (V, TE);
      elsif Is_Access_Type (TE) and then Is_Access_Type (V)
        and then not Is_Access_Unconstrained (V)
        and then not Is_Access_Unconstrained (TE)
      then
         return Pointer_Cast (V, TE);

      --  If these are both integral types, we handle this as a normal
      --  conversion.  Unchecked conversion is only defined if the sizes
      --  are the same, which is handled above by checking for the same
      --  LLVM type, but the front-end generates it, meaning to do
      --  a normal conversion.

      elsif Is_Discrete_Or_Fixed_Point_Type (TE)
        and then Is_Discrete_Or_Fixed_Point_Type (V)
      then
         return Convert_To_Elementary_Type (V, TE);

      --  We can unchecked convert floating point of the same width
      --  (the only way that UC is formally defined) with a "bitcast"
      --  instruction.

      elsif ((Is_Floating_Point_Type (TE)
                and then Is_Discrete_Or_Fixed_Point_Type (V))
             or else (Is_Discrete_Or_Fixed_Point_Type (TE)
                        and then Is_Floating_Point_Type (V)))
        and then (ULL'(Get_LLVM_Type_Size_In_Bits (T)) =
                    ULL'(Get_LLVM_Type_Size_In_Bits (V)))
      then
         return Bit_Cast (V, TE);

      --  If we have an unconstrained array that we're constraining,
      --  convert to an access to the result and then see if we can
      --  get it as a value (which will only be the case for constant
      --  size. ??? Review this code.

      elsif Is_Access_Unconstrained (V)
        and then Is_Array_Type (TE) and then Is_Constrained (TE)
      then
         return Get (Convert_To_Access_To (V, TE), Object);

      --  If we're converting to an unconstrained array, keep things the
      --  way they are so we preserve bounds.

      elsif Is_Unconstrained_Array (TE) then
         return V;

      --  Otherwise, these must be cases where we have to convert by
      --  pointer punning.  We need the LValue of the expression
      --  first.  If the type is a dynamic size, we know that's what
      --  we have already.  Otherwise, get it for an LValue (which
      --  will throw away the previous computation).  If we have an
      --  unconstrained array, point to the array data.  Then
      --  dereference in the proper type.

      else
         if not Is_Dynamic_Size (V)
           and then (not Is_Reference (V)
                       or else Full_Designated_Type (V) /= Full_Etype (N))
         then
            V := Emit_LValue (N);
         end if;

         if Is_Access_Unconstrained (V) then
            V := Get (V, Reference);
         end if;

         return Get (Ptr_To_Ref (V, TE, "unc-ptr-cvt"), Object);
      end if;
   end Build_Unchecked_Conversion;

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
         then
            return Convert_To_Access_To (LValue_Pair_Table.Table (J), TE);
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
         exit when not  Nkind_In (E, N_Type_Conversion,
                                  N_Unchecked_Type_Conversion,
                                  N_Qualified_Expression);
         exit when Is_Elementary_Type (Full_Etype (E));
         exit when Get_Type_Size_Complexity (Full_Etype (E))
           <= Get_Type_Size_Complexity (Full_Etype (Expression (E)));
         E := Expression (E);
      end loop;

      return E;
   end Strip_Complex_Conversions;

   ----------------------
   -- Bounds_To_Length --
   ----------------------

   function Bounds_To_Length
     (In_Low, In_High : GL_Value; TE : Entity_Id) return GL_Value
   is
      Low      : constant GL_Value := Convert_To_Elementary_Type (In_Low, TE);
      High     : constant GL_Value := Convert_To_Elementary_Type (In_High, TE);
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
      Typ : Entity_Id := TE;
   begin
      while Etype (Typ) /= Typ loop
         Typ := Etype (Typ);
      end loop;

      return Typ;
   end Ultimate_Base_Type;

   ----------------------
   -- Get_Fullest_View --
   ----------------------

   function Get_Fullest_View (TE : Entity_Id) return Entity_Id is
   begin
      --  Strictly speaking, the recursion below isn't necessary, but
      --  it's both simplest and safest.

      case Ekind (TE) is
         when Incomplete_Kind =>
            if From_Limited_With (TE) then
               return Get_Fullest_View (Non_Limited_View (TE));
            elsif Present (Full_View (TE)) then
               return Get_Fullest_View (Full_View (TE));
            end if;

         when Private_Kind =>
            if Present (Underlying_Full_View (TE)) then
               return Get_Fullest_View (Underlying_Full_View (TE));
            elsif Present (Full_View (TE)) then
               return Get_Fullest_View (Full_View (TE));
            else
               return Get_Fullest_View (Etype (TE));
            end if;

         when Array_Kind =>
            if Present (Packed_Array_Impl_Type (TE)) then
               return Get_Fullest_View (Packed_Array_Impl_Type (TE));
            end if;

         when E_Record_Subtype =>
            if Present (Cloned_Subtype (TE)) then
               return Get_Fullest_View (Cloned_Subtype (TE));
            end if;

         when E_Class_Wide_Type =>
            return Get_Fullest_View (Root_Type (TE));

         when  E_Class_Wide_Subtype =>
            if Present (Equivalent_Type (TE)) then
               return Get_Fullest_View (Equivalent_Type (TE));
            elsif Present (Cloned_Subtype (TE)) then
               return Get_Fullest_View (Cloned_Subtype (TE));
            end if;

         when E_Protected_Type | E_Protected_Subtype
            | E_Task_Type |  E_Task_Subtype =>
            if Present (Corresponding_Record_Type (TE)) then
               return Get_Fullest_View (Corresponding_Record_Type (TE));
            end if;

         when E_Access_Protected_Subprogram_Type
            | E_Anonymous_Access_Protected_Subprogram_Type =>
            if Present (Equivalent_Type (TE)) then
               return Get_Fullest_View (Equivalent_Type (TE));
            end if;

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
      elsif Needs_Activation_Record (TE) then
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
      Discard   : Type_T;
      pragma Unreferenced (Discard);
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
         return T;
      end if;

      --  ??? This probably needs to be cleaned up, but before we do anything,
      --  see if this isn't a base type and process that if so.

      if Base_Type (Def_Ident) /= Def_Ident then
         Discard := GNAT_To_LLVM_Type (Base_Type (Def_Ident), False);
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
                           T := Float_Type_In_Context (LLVM_Context);
                        when 64 =>
                           T := Double_Type_In_Context (LLVM_Context);
                        when 128 =>
                           --  Extended precision; not IEEE_128
                           T := X86_F_P80_Type_In_Context (LLVM_Context);
                        when 80 | 96 =>
                           T := X86_F_P80_Type_In_Context (LLVM_Context);
                        when others =>
                           --  ??? Double check that
                           T := F_P128_Type_In_Context (LLVM_Context);
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

            T := Struct_Create_Named (LLVM_Context, Get_Name (Def_Ident));

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

      Low := Build_Type_Conversion (Low_Bound (SRange), TE);
      High := Build_Type_Conversion (High_Bound (SRange), TE);

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

   begin
      --  If this is to get bounds and data and we have a value to store
      --  which contains data, convert it to bounds and data and store it.
      --  Otherwise, we have two cases, depending on the reason that we
      --  have bounds because Emit_Assignment only can handle the
      --  nominal type for alias to unconstrained case.

      if R = Reference_To_Bounds_And_Data then
         if Present (V) and then not Is_Reference (V) then
            Store (Get (V, Bounds_And_Data), Memory);
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

      return Convert_To_Access_To (Memory, TE);
   end Move_Into_Memory;

   -----------------------
   -- Allocate_For_Type --
   -----------------------

   function Allocate_For_Type
     (TE         : Entity_Id;
      Alloc_Type : Entity_Id;
      V          : GL_Value := No_GL_Value;
      Name       : String := "") return GL_Value
   is
      Element_Typ : Entity_Id;
      Num_Elts    : GL_Value;

   begin
      --  We have three cases.  If the object is not of a dynamic size,
      --  we just do the alloca and that's all.

      if not Is_Dynamic_Size (Alloc_Type) then
         return
           Move_Into_Memory (Alloca (Alloc_Type, Name), V, TE, Alloc_Type);
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
      Ret_Loc    : constant GL_Value  :=
        (if No (Proc) then No_GL_Value
         else Allocate_For_Type (Size_Type, Size_Type));

   begin
      --  If no function was specified, use the default memory allocation
      --  function, where we just pass a size.

      if No (Proc) then
         return Move_Into_Memory
           (Call (Get_Default_Alloc_Fn, Standard_A_Char, (1 => Size)),
            V, TE, Alloc_Type);

      --  If a procedure was specified (meaning that a pool must also have
      --  been specified) and the pool is a record, then it's a storage
      --  pool and we pass the pool, size, and alignment. Be sure that we
      --  convert the pool to actual type of the formal of the deallocator
      --  function: it may be a derived type.  ???  This is a procedure
      --  whose first parameter is an OUT parameter where it puts the
      --  address.  We should be converting procedures with OUT parameters
      --  to functions, which would make the below a lot easier, but we
      --  don't yet (because there's no good place to indicate that we
      --  have).

      elsif Is_Record_Type (Full_Etype (Pool)) then
         Call_Alloc_Dealloc (Proc,
               (1 => Ptr_To_Ref (Emit_LValue (Pool, Clear => False),
                                 Full_Etype (First_Formal (Proc))),
                2 => Ret_Loc, 3 => Size, 4 => Align_V));

      --  Otherwise, this is the secondary stack and we just call with size

      else
         Call_Alloc_Dealloc (Proc, (1 => Ret_Loc, 2 => Size));
      end if;

      --  If we're doing this for an unconstrained array, we have the pointer
      --  to the raw array, not a fat pointer.

      return Move_Into_Memory (Load (Ret_Loc), V, TE, Alloc_Type);
   end Heap_Allocate_For_Type;

   ---------------------
   -- Heap_Deallocate --
   ---------------------

   procedure Heap_Deallocate (V : GL_Value; Proc : Entity_Id; Pool : Entity_Id)
   is
      Size        : constant GL_Value := Get_Type_Size (V);
      Align       : constant unsigned := Get_Type_Alignment (V);
      Align_V     : constant GL_Value := Size_Const_Int (Align);
      Converted_V : GL_Value          := V;

   begin
      --  If V is an access type, convert it to a reference to the
      --  underlying data.

      if Is_Access_Type (V) and then Relationship (V) = Data then
         Converted_V := From_Access (V);
      end if;

      --  If V is a fat pointer, get just the array data.  We'll then either
      --  convert it to a generic pointer or to an integer (System.Address).

      Converted_V := Get (Converted_V, Reference);

      --  If no subprogram was specified, use the default memory deallocation
      --  procedure, where we just pass the object and a size a size.

      if No (Proc) then
         Call (Get_Default_Free_Fn,
               (1 => Pointer_Cast (Converted_V, Standard_A_Char), 2 => Size));

      --  If a procedure was specified (meaning that a pool must also
      --  have been specified) and the pool is a record, then it's a
      --  storage pool and we pass the pool, size, and alignment.  Be
      --  sure that we convert the pool to actual type of the formal of
      --  the deallocator function: it may be a derived type.

      elsif Is_Record_Type (Full_Etype (Pool)) then
         Call_Alloc_Dealloc (Proc,
               (1 => Ptr_To_Ref (Emit_LValue (Pool, Clear => False),
                                 Full_Etype (First_Formal (Proc))),
                2 => Ptr_To_Size_Type (Converted_V),
                3 => Size, 4 => Align_V));

      --  Otherwise, this is the secondary stack and we just call with size

      else
         Call_Alloc_Dealloc (Proc,
               (1 => Ptr_To_Size_Type (Converted_V), 2 => Size));
      end if;
   end Heap_Deallocate;

   ---------------------------
   --  Convert_To_Size_Type --
   ---------------------------

   function Convert_To_Size_Type (V : GL_Value) return GL_Value is
   begin
      return Convert_To_Elementary_Type (V, Size_Type);
   end Convert_To_Size_Type;

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
        Get_Type_Size (Alloc_Type, V,
                       For_Type => No (V) and then not Is_Constrained (TE));

   begin
      --  Adjust size if constrained subtype for aliased unconstrained or
      --  for unconstrained itself

      if Is_Unconstrained_Array (TE)
        or else (Is_Constr_Subt_For_UN_Aliased (Alloc_Type)
                   and then Is_Array_Type (Alloc_Type))
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
