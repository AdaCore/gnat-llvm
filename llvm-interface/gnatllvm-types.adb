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

with Errout;   use Errout;
with Output;   use Output;
with Restrict; use Restrict;
with Snames;   use Snames;
with Stand;    use Stand;
with Table;    use Table;

with GNATLLVM.Arrays;      use GNATLLVM.Arrays;
with GNATLLVM.Blocks;      use GNATLLVM.Blocks;
with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.Exprs;       use GNATLLVM.Exprs;
with GNATLLVM.Records;     use GNATLLVM.Records;
with GNATLLVM.Subprograms; use GNATLLVM.Subprograms;
with GNATLLVM.Variables;   use GNATLLVM.Variables;
with GNATLLVM.Wrapper;     use GNATLLVM.Wrapper;

package body GNATLLVM.Types is

   --  We save pairs of GNAT types and LLVM Value_T for each level of
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

   Var_Idx_For_BA : Int := 1;
   --  Index of variable used for Dynamic_Val in back-annotation.

   function Depends_On_Being_Elaborated (TE : Entity_Id) return Boolean
     with Pre => Is_Type_Or_Void (TE);
   --  Return True if TE or any type it depends on is being elaborated

   function Create_Discrete_Type (TE : Entity_Id) return Type_T
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (TE),
          Post => Present (Create_Discrete_Type'Result);
   function Create_Floating_Point_Type (TE : Entity_Id) return Type_T
     with Pre  => Is_Floating_Point_Type (TE),
          Post => Present (Create_Floating_Point_Type'Result);
   function Create_Access_Type (TE : Entity_Id) return Type_T
     with Pre  => Is_Access_Type (TE),
          Post => Present (Create_Access_Type'Result);
   --  Create an LLVM type for various GNAT types

   function Convert_Pointer
     (V  : GL_Value;
      TE : Entity_Id;
      R  : GL_Relationship;
      T  : Type_T) return GL_Value
     with Pre  => Present (V) and then Is_Type (TE) and then Present (T),
          Post => Present (Convert_Pointer'Result);
   --  Internal function for Convert_Pointer and Convert_Pointer_To_Dummy
   --  to do actual conversion.

   function Is_In_LHS_Context (N : Node_Id) return Boolean;
   --  Return True if N's parent (if N is Present) is such that we need a
   --  LValue.

   function Is_Nop_Conversion (V : GL_Value; TE : Entity_Id) return Boolean
     with Pre => Is_Reference (V) and then Is_Type (TE);
   --  Return True if converting V to type TE won't change any bits

   function Is_Parent_Of (T_Need, T_Have : Entity_Id) return Boolean
     with Pre => Is_Record_Type (T_Need) and then Is_Record_Type (T_Have);
   --  True if T_Have is a parent type of T_Need

   function Get_Alloc_Size
     (TE       : Entity_Id;
      Alloc_TE : Entity_Id;
      V        : GL_Value;
      Max_Size : Boolean := False) return GL_Value
     with Pre  => Is_Type (TE) and then Is_Type (Alloc_TE),
          Post => Present (Get_Alloc_Size'Result);
   --  Like Get_Type_Size, but used for the size to be allocated, so we
   --  include the size of the bounds in some array cases.

   function Get_Alloc_Alignment
     (TE       : Entity_Id;
      Alloc_TE : Entity_Id) return GL_Value
     with Pre  => Is_Type (TE) and then Is_Type (Alloc_TE),
          Post => Full_Etype (Get_Alloc_Alignment'Result) = Size_Type;
   --  Like Get_Type_Size, but used for the alignment in an allocateor, so we
   --  include the alignment of the bounds in some array cases.

   function Move_Into_Memory
     (Temp     : GL_Value;
      V        : GL_Value;
      Expr     : Node_Id;
      TE       : Entity_Id;
      Alloc_TE : Entity_Id) return GL_Value
     with Pre  => Present (Temp) and then Is_Type (TE)
                  and then Is_Type (Alloc_TE),
          Post => Is_Access_Type (Move_Into_Memory'Result);
   --  Temp is memory that was recently allocated.  Move V, if Present, or
   --  the evaluation of Expr if Present and V isn't, into that allocated
   --  memory and return the allocated memory as a reference to type TE.
   --  This is used by both type of memory allocators.  Temp can be of any
   --  type, either an integer or pointer to anything.  Alloc_TE is the
   --  type that was used to allocate the memory.

   function IDS_From_Const (V : GL_Value) return IDS is
     (if   Is_A_Const_Int (V) then (False, V) else Var_IDS)
     with Pre => Is_Constant (V), Post => Present (IDS_From_Const'Result);
   --  V is a constant.  If it's a constant integer, return that value.
   --  Otherwise, don't treat it as a constant.

   function BA_From_Const (V : GL_Value) return BA_Data is
     (if   Is_A_Const_Int (V) then (False, V, No_Uint) else No_BA)
     with Pre => Is_Constant (V);
   --  Likewise, for back-annotation

   ----------------------
   --  Is_Dynamic_Size --
   ----------------------

   function Is_Dynamic_Size
     (TE : Entity_Id; Max_Size : Boolean := False) return Boolean
   is
      Size : IDS;
   begin
      --  If this is of elementary type, it's not of dynamic size.  We have
      --  to do this test not just for efficiency, but also to avoid
      --  infinite recursion if we are passed Size_Type.

      if Is_Elementary_Type (TE) then
         return False;
      end if;

      --  Otherwise get the size for our purposes.  If not a constant or not
      --  something LLVM can use natively as an array bound, this is dynamic.

      Size := IDS_Type_Size (TE, No_GL_Value, Max_Size);
      return not IDS_Is_Const (Size)
        or else IDS_Const_Int (Size) < 0
        or else IDS_Const_Int (Size) > LLI (unsigned'Last);

   end Is_Dynamic_Size;

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

   ----------------------
   -- Is_Loadable_Type --
   ----------------------

   function Is_Loadable_Type (T : Type_T) return Boolean is
   begin
      --  A type isn't loadable if it's too larged

      if Get_Type_Size (T) / Get_Type_Alignment (T) > Max_Load_Size then
         return False;

      --  If a structure, it isn't loadable if any component isn't

      elsif Get_Type_Kind (T) = Struct_Type_Kind
        and then Count_Struct_Element_Types (T) /= 0
      then
         for J in 0 .. Count_Struct_Element_Types (T) - 1 loop
            if not Is_Loadable_Type (Struct_Get_Type_At_Index (T, J)) then
               return False;
            end if;
         end loop;

      --  Likewise for an array (its component might be an array that has
      --  a non-loadable type as a component).

      elsif Get_Type_Kind (T) = Array_Type_Kind
        and then not Is_Loadable_Type (Get_Element_Type (T))
      then
         return False;
      end if;

      --  If nothing prevented this from being a loadable type, it is

      return True;
   end Is_Loadable_Type;

   ---------------------
   -- Emit_Conversion --
   ---------------------

   function Emit_Conversion
     (N                   : Node_Id;
      TE                  : Entity_Id;
      From_N              : Node_Id := Empty;
      For_LHS             : Boolean := False;
      Is_Unchecked        : Boolean := False;
      Need_Overflow_Check : Boolean := False;
      Float_Truncate      : Boolean := False) return GL_Value
   is
      Result      : GL_Value                 := Emit (N, For_LHS => For_LHS);
      Orig_Result : constant GL_Value        := Result;
      In_TE       : constant Entity_Id       := Related_Type (Result);
      R           : constant GL_Relationship := Relationship (Result);

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
         Error_Msg
           ("??conversion between subprogram access types of different",
            Sloc (N));
         Error_Msg
           ("\conventions, & and &, will not work if the former points ",
            Sloc (N));
         Error_Msg
           ("\to a subprogram that references parent variables.", Sloc (N));
      end if;

      if Is_Elementary_Type (TE) and then Need_Overflow_Check then
         Result := Get (Result, Data);
         Emit_Overflow_Check (Result, From_N);
         Result := Convert (Result, TE, Float_Truncate => Float_Truncate);

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
        and then (ULL'(Get_Type_Size_In_Bits (Create_Type (TE))) =
                    ULL'(Get_Type_Size_In_Bits (Create_Type (In_TE))))
      then
         return Bit_Cast (Get (Result, Data), TE);

      --  If both types are elementary, hand that off to our helper, but
      --  raise a Constraint_Error if this conversion overflowed by producing
      --  an undef.

      elsif Is_Elementary_Type (In_TE)
        and then Is_Elementary_Type (TE)
      then
         Result := Convert (Get (Result, Data), TE,
                            Float_Truncate => Float_Truncate);
         if Is_Undef (Result) and then not Is_Undef (Orig_Result) then
            Error_Msg_N ("?`Constraint_Error` will be raised at run time",
                         From_N);
            Emit_Raise_Call (From_N, CE_Overflow_Check_Failed);
         end if;

      --  If both types are the same, just change the type of the result.
      --  Avoid confusing [0 x T] as both a zero-size constrained type and
      --  the type used for a variable-sized type.

      elsif Is_Data (Result) and then not Is_Nonnative_Type (TE)
        and then Type_Of (Result) = Create_Type (TE)
      then
         Result := G_Is (Result, TE);

      --  If we have an undefined value that we're converting to another
      --  type, just get an undefined value of that type.  But watch for
      --  the case where we have Data of some fixed-size type and we're
      --  converting to a dynamic-sized type.  We handle the reference
      --  cases below since we may have to deal with materializing bounds.

      elsif Is_Undef (Result) and then R = Data
        and then Is_Loadable_Type (TE)
      then
         return Get_Undef (TE);

      --  If we have a constant of a struct type that we're converting to
      --  a struct of the same layout, we can make a new constant.

      elsif R = Data and then Is_Constant (Result)
        and then Get_Type_Kind (Type_Of (Result)) = Struct_Type_Kind
        and then Is_Record_Type (TE) and then not Is_Nonnative_Type (TE)
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

   -------------------------------
   -- Normalize_LValue_Refrence --
   -------------------------------

   function Normalize_LValue_Reference (V : GL_Value) return GL_Value is
      New_V : constant GL_Value        :=
        Get (V, (if   Is_Double_Reference (V) then Reference_To_Reference
                 else Any_Reference));
      TE    : constant Entity_Id       := Related_Type (New_V);
      R     : constant GL_Relationship := Relationship (New_V);
      T     : constant Type_T          := Type_For_Relationship (TE, R);

   begin
      --  If this is a reference and the LLVM type doesn't agree with the
      --  proper type for TE and the relationship, convert it.

      if Relationship (New_V) = Reference
        and then Get_Element_Type (Type_Of (New_V)) /= T
      then
         return Ptr_To_Relationship (New_V, TE, R);

      --  If it's an actual access type and the type differs, unpack it,
      --  convert to the proper type, and repack.

      elsif Type_Of (New_V) /= T then
         return To_Access (Convert_Pointer (From_Access (New_V),
                                            Full_Designated_Type (TE)),
                           TE);

      else
         return New_V;
      end if;

   end Normalize_LValue_Reference;

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
      Src_Size    : constant Nat     := Nat (ULL'(Get_Type_Size_In_Bits (V)));
      Dest_Usize  : constant Uint    :=
        (if   Is_Modular_Integer_Type (TE) then RM_Size (TE) else Esize (TE));
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
      In_R   : constant GL_Relationship := Relationship (V);
      In_TE  : constant Entity_Id       := Related_Type (V);
      As_Ref : constant GL_Value        :=
        (if   Is_Data (V) and then Is_Access_Type (In_TE)
         then From_Access (V) else V);
      R      : constant GL_Relationship := Relationship_For_Access_Type (TE);
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
      Unc_Src  : constant Boolean   := Is_Access_Unconstrained_Array (V);
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
            return Get (V, Any_Reference);
         else
            --  If what we have is a reference to bounds and data or a
            --  thin pointer and have an array type that needs bounds,
            --  convert to the same relationship of that type.  Otherwise,
            --  convert to a Reference and then to the new type.

            if Relationship (V) in Reference_To_Bounds_And_Data | Thin_Pointer
              and then Type_Needs_Bounds (TE)
            then
               return Ptr_To_Relationship (V, TE, Relationship (V));
            else
               return Ptr_To_Ref (Get (V, Reference), TE);
            end if;
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

   begin
      return Convert_Pointer (V, TE, R, T);
   end Convert_Pointer;

   ------------------------------
   -- Convert_Pointer_To_Dummy --
   ------------------------------

   function Convert_Pointer_To_Dummy (V : GL_Value) return GL_Value is
      TE    : constant Entity_Id       := Related_Type (V);
      R     : constant GL_Relationship := Relationship (V);
      T     : constant Type_T          := Create_Dummy_Access_Type (TE);

   begin
      return Convert_Pointer (V, TE, R, T);
   end Convert_Pointer_To_Dummy;

   ---------------------
   -- Convert_Pointer --
   ---------------------

   function Convert_Pointer
     (V  : GL_Value;
      TE : Entity_Id;
      R  : GL_Relationship;
      T  : Type_T) return GL_Value
   is
      Value : Value_T;

   begin
      if Type_Of (V) = T then
         return G_Is_Relationship (V, TE, R);

      --  If the input is an actual pointer, convert it

      elsif Get_Type_Kind (T) = Pointer_Type_Kind then
         return G (Pointer_Cast (IR_Builder, LLVM_Value (V), T, ""), TE, R);
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
      TE : constant Entity_Id := Related_Type (V);

   begin
      --  Only add to the LValue list if this is a record type.  We might
      --  be tempted to do this only if the type has discriminants, but
      --  that doesn't work because a parent might and it's not worth
      --  checking.

      if Is_Record_Type (TE) and then Disable_LV_Append = 0 then
         LValue_Pair_Table.Append (V);
      end if;
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

   ------------------------
   -- Get_Matching_Value --
   ------------------------

   function Get_Matching_Value (TE : Entity_Id) return GL_Value is
   begin
      --  Check in the opposite order of what we push.  We may, for example
      --  be finding the size of an object of that size, in which case the
      --  object will have been added last.

      for J in reverse LValue_Pair_First .. LValue_Pair_Table.Last loop
         if Is_Parent_Of (TE, LValue_Pair_Table.Table (J).Typ)
           or else Is_Parent_Of (LValue_Pair_Table.Table (J).Typ, TE)
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
         exit when Is_Elementary_Type (Full_Etype (Expression (E)));
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

   ---------------------------
   -- Get_Type_Size_In_Bits --
   ---------------------------

   function Get_Type_Size_In_Bits (TE : Entity_Id) return GL_Value is
   begin
      pragma Assert (not Is_Nonnative_Type (TE));
      return Get_Type_Size_In_Bits (Create_Type (TE));
   end Get_Type_Size_In_Bits;

   ------------------------
   -- Ultimate_Base_Type --
   ------------------------

   function Ultimate_Base_Type (TE : Entity_Id) return Entity_Id is
   begin
      return Typ : Entity_Id := TE do
         while Full_Etype (Typ) /= Typ loop
            Typ := Full_Etype (Typ);
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
            elsif Ekind (TE) = E_Incomplete_Subtype then
               return Get_Fullest_View (Etype (TE));
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

   ---------------------------------
   -- Depends_On_Being_Elaborated --
   ---------------------------------

   function Depends_On_Being_Elaborated (TE : Entity_Id) return Boolean is
      BT : constant Entity_Id := Full_Base_Type (TE);
      F  : Entity_Id;

   begin
      --  If this is a void type, it doesn't depend on anything.

      if Ekind (TE) = E_Void then
         return False;

      --  This depends on something being elaborated if the type is being
      --  elaborated, its base type depends on something being elaborated,
      --  or this is an array with an aggregate component type that depends
      --  on something being elaborated.

      elsif Is_Being_Elaborated (TE)
           or else (BT /= TE and then Depends_On_Being_Elaborated (BT))
           or else (Is_Array_Type (TE)
                      and then Is_Aggregate_Type (Full_Component_Type (TE))
                      and then (Depends_On_Being_Elaborated
                                  (Full_Component_Type (TE))))
      then
         return True;

      --  If this is a record type, it depends on the type of each of
      --  the fields which have aggregate types.

      elsif Is_Record_Type (TE) then
         F := First_Component_Or_Discriminant (TE);
         while Present (F) loop
            exit when Is_Aggregate_Type (Full_Etype (F))
              and then Depends_On_Being_Elaborated (Full_Etype (F));
            Next_Component_Or_Discriminant (F);
         end loop;

         return Present (F);

      else
         --  Otherwise, this doesn't depend on something being elaborated

         return False;
      end if;
   end Depends_On_Being_Elaborated;

   --------------------------
   -- Create_Discrete_Type --
   --------------------------

   function Create_Discrete_Type (TE : Entity_Id) return Type_T is
   begin
      --  It's tempting to use i1 for boolean types, but that causes issue.
      --  First, we'd have to handle booleans with rep clauses specially,
      --  but, perhaps more importantly, LLVM treats a boolean as being true
      --  if it's 1 (interpreted as an 8-bit value) and zero otherwise, but
      --  the more natural interpretaton is that it's false if zero and
      --  true otherwise and this can become visible when using overlays
      --  with 'Address.
      --
      --  So we only use i1 for the internal boolean object (e.g., the result
      --  of a comparison) and for a 1-bit modular type.

      if Is_Modular_Integer_Type (TE) and then RM_Size (TE) /= Uint_0 then
         return Int_Ty (RM_Size (TE));
      elsif Esize (TE) /= Uint_0 then
         return Int_Ty (Esize (TE));
      else
         return Int_Ty (8);
      end if;
   end Create_Discrete_Type;

   --------------------------------
   -- Create_Floating_Point_Type --
   --------------------------------

   function Create_Floating_Point_Type (TE : Entity_Id) return Type_T is
      Size : constant Uint := Esize (Full_Base_Type (TE));
      T    : Type_T;
      pragma Assert (UI_Is_In_Int_Range (Size));

   begin
      case Float_Rep (TE) is
         when IEEE_Binary =>
            case UI_To_Int (Size) is
               when 32 =>
                  T := Float_Type_In_Context (Context);
               when 64 =>
                  T := Double_Type_In_Context (Context);
               when 80 | 96 | 128 =>
                  --  Extended precision; not IEEE_128
                  T := X86_F_P80_Type_In_Context (Context);
               when others =>
                  T := Void_Type;
            end case;

         when AAMP =>
            T := Void_Type;
      end case;

      if T = Void_Type then
         Error_Msg_N ("unsupported floating point type", TE);
      end if;

      return T;
   end Create_Floating_Point_Type;

   ------------------------
   -- Create_Access_Type --
   ------------------------

   function Create_Access_Type (TE : Entity_Id) return Type_T is
      DT : constant Entity_Id       := Full_Designated_Type (TE);
      R  : constant GL_Relationship := Relationship_For_Access_Type (TE);

   begin
      --  If DT is a subprogram type (since the access type to it is always
      --  the same type) or if it doesn't depend on something that's being
      --  elaborated, handle this normally.

      if Ekind (DT) = E_Subprogram_Type
        or else not Depends_On_Being_Elaborated (DT)
      then
         Set_Is_Dummy_Type (TE, False);
         return Type_For_Relationship (DT, R);

      --  If this is a record type, we can get the actual type that will be
      --  used here. If it hasn't been done yet, set it for the record
      --  type, and mark it dummy.

      elsif Is_Record_Type (DT) then
         if No (Get_Type (DT)) then
            Set_Type (DT, Struct_Create_Named (Context, Get_Name (DT)));
            Set_Is_Dummy_Type (DT, True);
         end if;

         return Pointer_Type (Get_Type (DT), 0);

      --  Otherwise, if DT is currently being elaborated, we have to make a
      --  dummy type that we know will be the same width of an access to
      --  the actual object and we'll convert to the actual type when we
      --  try to access an object of this access type.  The only types
      --  where there's an elaboration that can recurse are record, array,
      --  and access types (though access types whose designated types are
      --  other access types are quite rare).

      else
         Set_Is_Dummy_Type (TE, True);
         return Create_Dummy_Access_Type (TE);
      end if;
   end Create_Access_Type;

   ------------------------------
   -- Create_Dummy_Access_Type --
   ------------------------------

   function Create_Dummy_Access_Type (TE : Entity_Id) return Type_T is
      DT : constant Entity_Id       := Full_Designated_Type (TE);
      R  : constant GL_Relationship := Relationship_For_Access_Type (TE);

   begin
      if Is_Array_Type (DT) then

         --  For arrays, a pointer to void will work for all but a fat
         --  pointer.  For a fat pointer, use two pointers to void (we
         --  could make an array bound type without actually fully
         --  elaborating the array type, but it's not worth the trouble).

         return (if   R /= Fat_Pointer then Void_Ptr_Type
                 else Build_Struct_Type ((1 => Void_Ptr_Type,
                                          2 => Void_Ptr_Type)));

      elsif Ekind (DT) = E_Subprogram_Type then
         return Void_Ptr_Type;

      else
         --  Access type is the only case left.  We use a void pointer.

         pragma Assert (Is_Access_Type (DT) and then R = Reference);
         return Void_Ptr_Type;
      end if;

   end Create_Dummy_Access_Type;

   -----------------
   -- Create_Type --
   -----------------

   function Create_Type (TE : Entity_Id) return Type_T is
      T    : Type_T := Get_Type (TE);
      TBAA : Metadata_T;

   begin
      --  See if we already have a non-dummy type or have a dummy type but
      --  we're elaborating this type.

      if Present (T)
        and then (not Is_Dummy_Type (TE) or Is_Being_Elaborated (TE))
      then
         return T;
      end if;

      --  Set that we're elaborating the type.  Note that we have to do this
      --  here rather than right before the case statement because we may
      --  have two different types being elaborated that have the same
      --  base type.

      Set_Is_Being_Elaborated (TE, True);

      --  Before we do anything, see if this isn't a base type and
      --  process that if so.

      if Full_Base_Type (TE) /= TE then
         Discard (Create_Type (Full_Base_Type (TE)));
      end if;

      case Ekind (TE) is
         when Discrete_Or_Fixed_Point_Kind =>
            T := Create_Discrete_Type (TE);

         when Float_Kind =>
            T := Create_Floating_Point_Type (TE);

         when Access_Kind =>
            T := Create_Access_Type (TE);

         when Record_Kind =>
            T := Create_Record_Type (TE);

         when Array_Kind =>
            T := Create_Array_Type (TE);

         when E_Subprogram_Type =>
            T := Create_Subprogram_Type (TE);

         when E_Incomplete_Type =>
            --  This is normally a Taft Amendment type, so return a
            --  dummy type that we can take a pointer to.  But it may also
            --  be an actual type in the case of an error, so use something
            --  that we can take the size an alignment of.

            T := Int_Ty (8);

         when others =>
            Error_Msg_N
              ("unsupported type kind: `" & Ekind (TE)'Image & "`", TE);
            T := Void_Type;
      end case;

      --  Now save the result and any TBAA information

      Set_Type (TE, T);
      TBAA := Create_TBAA (TE);
      if Present (TBAA) then
         Set_TBAA (TE, TBAA);
      end if;

      --  If this is a packed array implementation type and the original
      --  type is an array, set information about the bounds of the
      --  original array.

      if Is_Packed_Array_Impl_Type (TE) then
         Discard (Create_Array_Type (TE, For_Orig => True));
      end if;

      --  Back-annotate sizes of non-scalar types if there isn't one.
      --  ??? Don't do anything for access subprogram since this will cause
      --  warnings for UC's in g-thread and g-spipat.

      if not Is_Access_Subprogram_Type (TE)
        and then not Is_Scalar_Type (TE)
      then
         if Unknown_Esize (TE) then
            Set_Esize   (TE, Annotated_Object_Size (TE));
         end if;
         if Unknown_RM_Size (TE) then
            Set_RM_Size (TE, Annotated_Value
                           (BA_Mul (BA_Type_Size (TE, No_Padding => True),
                                    BA_Const (8))));
         end if;
      end if;

      if Unknown_Alignment (TE) then
         Set_Alignment (TE, UI_From_Int (Int (ULL'(Get_Type_Alignment (TE)))));
      end if;

      Set_Is_Being_Elaborated (TE, False);
      return T;
   end Create_Type;

   -------------------------------
   -- Create_Type_For_Component --
   -------------------------------

   function Create_Type_For_Component (TE : Entity_Id) return Type_T is
      T    : constant Type_T := Create_Type (TE);
      Size : GL_Value;
   begin
      --  If this is a dynamic size, even when looking at the maximum
      --  size (when applicable), on the one hand, or already a native
      --  type, on the other, return T.  Otherwise, make an array type
      --  corresponding to the size.

      if Is_Dynamic_Size (TE, Max_Size => Is_Unconstrained_Record (TE))
        or else not Is_Nonnative_Type (TE)
      then
         return T;
      end if;

      Size := Get_Type_Size (TE, No_GL_Value,
                             Max_Size => Is_Unconstrained_Record (TE));
      pragma Assert (Is_A_Const_Int (Size));
      return Array_Type (Int_Ty (8), unsigned (Get_Const_Int_Value (Size)));
   end Create_Type_For_Component;

   -----------------
   -- Create_TBAA --
   -----------------

   function Create_TBAA (TE : Entity_Id) return Metadata_T is
      BT   : constant Entity_Id  := Full_Base_Type (TE);
      TBAA : constant Metadata_T := Get_TBAA (BT);

   begin
      --  If the base type has a TBAA, use it for us.  If it doesn't, it's
      --  probably because this is the base type, in which case, make a
      --  new entry for it.  If it's a type that we don't currently make
      --  TBAA information for, return none.

      if Present (TBAA) then
         return TBAA;
      elsif Is_Scalar_Type (BT) then
         return Create_TBAA_Scalar_Type_Node (MD_Builder, Get_Name (BT),
                                              TBAA_Root);
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
     (Temp     : GL_Value;
      V        : GL_Value;
      Expr     : Node_Id;
      TE       : Entity_Id;
      Alloc_TE : Entity_Id) return GL_Value
   is
      R        : constant GL_Relationship := Relationship_For_Alloc (TE);
      New_Expr : constant Node_Id         := Strip_Complex_Conversions (Expr);
      Memory : GL_Value                   :=
        (if Is_Access_Type (Temp)
         then Ptr_To_Relationship (Temp, Alloc_TE, R)
         else Int_To_Relationship (Temp, Alloc_TE, R));
      New_V  : GL_Value                   :=
        (if   Present (V) then V elsif Present (New_Expr)
         then Emit (New_Expr, LHS => Memory) else No_GL_Value);

   begin
      --  If this is to get bounds and data and we have a value to store
      --  which contains data, convert it to bounds and data and store it.
      --  Otherwise, we have two cases, depending on the reason that we
      --  have bounds because Emit_Assignment only can handle the
      --  nominal type for alias to unconstrained case.

      if R = Reference_To_Bounds_And_Data then
         if Present (New_V) and then Is_Data (New_V) then
            New_V  := Get (New_V, Bounds_And_Data);
            Memory := Ptr_To_Relationship (Memory, New_V, R);
         else
            if not Is_Constrained (TE) or else No (New_V) then
               Store (Get_Array_Bounds (TE, Alloc_TE, New_V),
                      Get (Memory, Reference_To_Bounds));
               Memory := Get (Memory, Thin_Pointer);
            end if;
         end if;
      end if;

      --  If we have a value to move into memory, move it

      if Present (New_V) and then New_V /= Memory then
         Emit_Assignment (Memory, Value => New_V);
      end if;

      return Convert_Ref (Memory, TE);
   end Move_Into_Memory;

   -----------------------
   -- Allocate_For_Type --
   -----------------------

   function Allocate_For_Type
     (TE       : Entity_Id;
      Alloc_TE : Entity_Id;
      N        : Node_Id;
      V        : GL_Value := No_GL_Value;
      Expr     : Node_Id  := Empty;
      Name     : String   := "";
      Max_Size : Boolean  := False) return GL_Value
   is
      Max_Alloc  : constant ULL := 10_000_000;
      Value      : GL_Value     := V;
      Element_TE : Entity_Id;
      Num_Elts   : GL_Value;

   begin
      --  We have three cases.  If the object has a native type, we just do
      --  the alloca and that's all.

      if not Is_Nonnative_Type (Alloc_TE) then
         if Do_Stack_Check
           and then Get_Type_Size (Create_Type (Alloc_TE)) > Max_Alloc
         then
            Emit_Raise_Call (N, SE_Object_Too_Large);
            return Get_Undef_Ref (TE);
         else
            return Move_Into_Memory (Alloca (Alloc_TE, Name),
                                     Value, Expr, TE, Alloc_TE);
         end if;
      end if;

      --  Otherwise, we probably have to do some sort of dynamic
      --  allocation.  If this is an array of a component that's not of
      --  dynamic size, then we can allocate an array of the component type
      --  corresponding to the array type and cast it to a pointer to the
      --  actual type.  If not, we have to allocate it as an array of
      --  bytes.  We must use an array of bytes if we have to include bounds.
      --  If this is an unconstrained array, we need to find the bounds, so
      --  evaluate Expr if Present and there's no Value.

      if Is_Unconstrained_Array (Alloc_TE) and then No (Value)
        and then Present (Expr)
      then
         Value := Emit (Expr);
      end if;

      if Is_Array_Type (Alloc_TE)
        and then not Is_Dynamic_Size (Full_Component_Type (Alloc_TE),
                                      not Is_Constrained
                                        (Full_Component_Type (Alloc_TE)))
        and then not Is_Constr_Subt_For_UN_Aliased (Alloc_TE)
      then
         Element_TE := Full_Component_Type (Alloc_TE);
         Num_Elts   := Get_Array_Elements (Value, Alloc_TE);
      else
         Element_TE := Standard_Short_Short_Integer;
         Num_Elts   := Get_Alloc_Size (Alloc_TE, Alloc_TE, Value, Max_Size);
      end if;

      --  Check that we aren't trying to allocate too much memory.  Raise
      --  Storage_Error if so.  We don't try to support local exception
      --  labels and -fstack-check at the same time.  The divide below
      --  will constant-fold, but make sure we aren't dividing by zero.

      if Do_Stack_Check
        and then Get_Type_Size (Element_TE) /= Size_Const_Null
      then
         Emit_Raise_Call_If (I_Cmp (Int_UGT, Num_Elts,
                                    U_Div (Size_Const_Int (Max_Alloc),
                                           Get_Type_Size (Element_TE))),
                             N, SE_Object_Too_Large);
      end if;

      return Move_Into_Memory (Array_Alloca (Element_TE, Num_Elts, Name),
                               Value, Expr, TE, Alloc_TE);

   end Allocate_For_Type;

   ----------------------------
   -- Heap_Allocate_For_Type --
   ----------------------------

   function Heap_Allocate_For_Type
     (TE       : Entity_Id;
      Alloc_TE : Entity_Id;
      V        : GL_Value  := No_GL_Value;
      N        : Node_Id   := Empty;
      Expr     : Node_Id   := Empty;
      Proc     : Entity_Id := Empty;
      Pool     : Entity_Id := Empty;
      Max_Size : Boolean   := False) return GL_Value
   is
      Value   : constant GL_Value  :=
        (if    Present (V) then V
         elsif Is_Unconstrained_Type (Alloc_TE) and then Present (Expr)
         then  Emit (Expr) else No_GL_Value);
      Size    : constant GL_Value  :=
         Get_Alloc_Size (TE, Alloc_TE, Value, Max_Size);
      Align   : constant GL_Value  := Get_Alloc_Alignment (TE, Alloc_TE);
      Result  : GL_Value;

   begin
      --  Check that we aren't violating any restrictions

      if Present (N)
        and then not (Nkind (N) = N_Allocator and then Comes_From_Source (N))
      then
         Check_No_Implicit_Heap_Alloc (N);
         if Has_Task (TE) then
            Check_No_Implicit_Task_Alloc (N);
         end if;

         if Has_Protected (TE) then
            Check_No_Implicit_Protected_Alloc (N);
         end if;
      end if;

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
                        2 => Size, 3 => Align));

      --  Otherwise, this is the secondary stack and we just call with size

      else
         Result := Call_Alloc (Proc, (1 => Size));
      end if;

      --  If we're doing this for an unconstrained array, we have the pointer
      --  to the raw array, not a fat pointer.

      return Move_Into_Memory (Result, Value, Expr, TE, Alloc_TE);
   end Heap_Allocate_For_Type;

   ---------------------
   -- Heap_Deallocate --
   ---------------------

   procedure Heap_Deallocate
     (V        : GL_Value;
      Desig_TE : Entity_Id;
      Proc     : Entity_Id;
      Pool     : Entity_Id)
   is
      Conv_V   : GL_Value  := V;
      DT       : Entity_Id := Related_Type (V);
      Alloc_TE : Entity_Id := DT;

   begin
      --  If V is an access type, convert it to a reference to the
      --  underlying data.  We also want to record the actual designated
      --  type in this case since it may contain bound information and
      --  we need to record the bounds as well as their size.

      if Is_Access_Type (V) and then Is_Data (V) then
         Conv_V   := From_Access (V);
         DT       := Full_Designated_Type (V);
         Alloc_TE := DT;
      end if;

      --  If we have a designated type, that's the type we use for
      --  computing size and alignment.

      if Present (Desig_TE) then
         Alloc_TE := Get_Fullest_View (Desig_TE);
      end if;

      --  If V is an unconstrained array, we want a pointer to the bounds
      --  and data.  Otherwise just a Reference.  We'll then either convert
      --  it to a generic pointer or to an integer (System.Address).

      Conv_V := Get (Conv_V, Relationship_For_Alloc (DT));

      declare
         Align : constant GL_Value := Get_Alloc_Alignment (DT, Alloc_TE);
         Size  : constant GL_Value := Get_Alloc_Size (DT, Alloc_TE, Conv_V);

      begin
         --  If no subprogram was specified, use the default memory
         --  deallocation procedure, where we just pass the object.

         if No (Proc) then
            Call (Get_Default_Free_Fn,
                  (1 => Pointer_Cast (Conv_V, Standard_A_Char)));

            --  If a procedure was specified (meaning that a pool must also
            --  have been specified) and the pool is a record, then it's a
            --  storage pool and we pass the pool, size, and alignment.  Be
            --  sure that we convert the pool to actual type of the formal
            --  of the deallocator function: it may be a derived type.

         elsif Is_Record_Type (Full_Etype (Pool)) then
            Call_Dealloc (Proc,
                          (1 => Ptr_To_Ref (Emit_Safe_LValue (Pool),
                                            Full_Etype (First_Formal (Proc))),
                           2 => Ptr_To_Size_Type (Conv_V),
                           3 => Size, 4 => Align));

            --  Otherwise, this is the secondary stack and we just call
            --  it with the size.

         else
            Call_Dealloc (Proc, (1 => Ptr_To_Size_Type (Conv_V), 2 => Size));
         end if;
      end;
   end Heap_Deallocate;

   ------------------------
   -- Get_Type_Alignment --
   ------------------------

   function Get_Type_Alignment (TE : Entity_Id) return ULL is
      Largest_Align : ULL  := 1;
      Field         : Entity_Id;

   begin
      --  If it's an array, it's the alignment of the component type

      if Is_Array_Type (TE) then
         return Get_Type_Alignment (Full_Component_Type (TE));

      --  If a record, use the highest alignment of any field

      elsif Is_Record_Type (TE) then
         Field := First_Entity (TE);
         while Present (Field) loop
            if Ekind_In (Field, E_Discriminant, E_Component) then
               Largest_Align
                 := ULL'Max (Largest_Align,
                             Get_Type_Alignment (Full_Etype (Field)));
            end if;

            Next_Entity (Field);
         end loop;

         return Largest_Align;

      --  If it's a subprogram type, there really isn't an alignment, but
      --  indicate that code can be anywhere.

      elsif Ekind (TE) = E_Subprogram_Type then
         return 1;

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
     (TE         : Entity_Id;
      V          : GL_Value := No_GL_Value;
      Max_Size   : Boolean  := False;
      No_Padding : Boolean := False) return GL_Value is
   begin
      --  If a value was specified and it's data, then it must be of a
      --  fixed size.  That's the size we're looking for.

      if Present (V) and then Relationship (V) = Data then
         return Get_Type_Size (Type_Of (V));
      elsif Is_Record_Type (TE) then
         return Get_Record_Type_Size (TE, V,
                                      Max_Size   => Max_Size,
                                      No_Padding => No_Padding);
      elsif Is_Array_Type (TE) and then Is_Nonnative_Type (TE) then
         return Get_Array_Type_Size (TE, V, Max_Size);
      else
         return Get_Type_Size (Create_Type (TE));
      end if;

   end Get_Type_Size;

   --------------------
   -- Get_Alloc_Size --
   --------------------

   function Get_Alloc_Size
     (TE       : Entity_Id;
      Alloc_TE : Entity_Id;
      V        : GL_Value;
      Max_Size : Boolean := False) return GL_Value
   is
      Size : GL_Value :=
        Get_Type_Size (Alloc_TE,
                       (if   Is_Class_Wide_Equivalent_Type (Alloc_TE)
                        then No_GL_Value else V),
                       Max_Size => Max_Size);

   begin
      --  Adjust size if constrained subtype for aliased unconstrained or
      --  for unconstrained itself.

      if Is_Unconstrained_Array (TE) or else Type_Needs_Bounds (Alloc_TE) then
         Size := Align_To (Add (Size, Get_Bound_Size (TE)),
                           Get_Type_Alignment (TE), Get_Bound_Alignment (TE));
      end if;

      return Size;
   end Get_Alloc_Size;

   -------------------------
   -- Get_Alloc_Alignment --
   -------------------------

   function Get_Alloc_Alignment
     (TE       : Entity_Id;
      Alloc_TE : Entity_Id) return GL_Value
   is
      Align : GL_Value := Get_Type_Alignment (Alloc_TE);

   begin
      if Is_Unconstrained_Array (TE) or else Type_Needs_Bounds (Alloc_TE) then
         Align := Build_Max (Align, Get_Bound_Alignment (TE));
      end if;

      return Align;
   end Get_Alloc_Alignment;

   ------------------
   -- Compute_Size --
   ------------------

   function Compute_Size
     (Left_Type, Right_Type   : Entity_Id;
      Left_Value, Right_Value : GL_Value) return GL_Value
   is
      LHS_Complex : constant Nat     := Get_Type_Size_Complexity (Left_Type);
      RHS_Complex : constant Nat     := Get_Type_Size_Complexity (Right_Type);
      Class_Wide  : constant Boolean :=
        Is_Class_Wide_Equivalent_Type (Left_Type);

   begin
      --  Use the type of right side unless its complexity is more
      --  than that of the size of the type on the left side.  If the
      --  LHS is a class wide equivalent type, we must use it.

      if RHS_Complex > LHS_Complex or else Class_Wide then
         return Get_Type_Size (Left_Type, Left_Value,
                               No_Padding => not Class_Wide);
      else
         return Get_Type_Size (Right_Type, Right_Value, No_Padding => True);
      end if;

   end Compute_Size;

   ------------------------------
   -- Get_Type_Size_Complexity --
   ------------------------------

   function Get_Type_Size_Complexity
     (TE : Entity_Id; Max_Size : Boolean := False) return Nat is
   begin

      if Is_Record_Type (TE) then
         return Get_Record_Size_Complexity (TE, Max_Size);
      elsif Is_Array_Type (TE) then
         return Get_Array_Size_Complexity  (TE, Max_Size);

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
      TBAA : constant Metadata_T := Get_TBAA (TE);
   begin
      Set_Volatile (Inst, Is_Volatile (TE));

      if Present (TBAA) then
         Add_TBAA_Access
           (Inst, Create_TBAA_Access_Tag (MD_Builder, TBAA, TBAA, 0));
      end if;
   end Add_Type_Data_To_Instruction;

   -------------
   -- IDS_Min --
   -------------

   function IDS_Min (V1, V2 : IDS; Name : String := "") return IDS is
     (if   IDS_Is_Const (V1) and then IDS_Is_Const (V2)
      then (False, Build_Min (V1.Value, V2.Value, Name)) else Var_IDS);

   -------------
   -- IDS_Max --
   -------------

   function IDS_Max (V1, V2 : IDS; Name : String := "") return IDS is
     (if   IDS_Is_Const (V1) and then IDS_Is_Const (V2)
      then (False, Build_Max (V1.Value, V2.Value, Name)) else Var_IDS);

   -------------------
   -- IDS_Type_Size --
   -------------------

   function IDS_Type_Size
     (TE         : Entity_Id;
      V          : GL_Value := No_GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return IDS is
   begin
      --  If a value was specified and it's data, then it must be of a
      --  fixed size.  That's the size we're looking for.

      if Present (V) and then Relationship (V) = Data
        and then not Max_Size and then not No_Padding
      then
         return IDS_From_Const (Get_Type_Size (Type_Of (V)));
      elsif Is_Record_Type (TE) then
         return IDS_Record_Type_Size (TE, V,
                                      Max_Size   => Max_Size,
                                      No_Padding => No_Padding);
      elsif Is_Array_Type (TE) and then not Is_Constrained (TE) then
         return Var_IDS;
      elsif Is_Array_Type (TE) then
         return IDS_Array_Type_Size (TE, V, Max_Size);
      else
         return IDS_From_Const (Get_Type_Size (Create_Type (TE)));
      end if;
   end IDS_Type_Size;

   -------------------
   -- IDS_Emit_Expr --
   -------------------

   function IDS_Emit_Expr (V : Node_Id; LHS : IDS := No_IDS) return IDS is
      pragma Unreferenced (LHS);
   begin
      return (if   Is_No_Elab_Needed (V)
              then IDS_From_Const (Emit_Expression (V)) else Var_IDS);
   end IDS_Emit_Expr;

   ---------------------------
   -- BA_To_Node_Ref_Or_Val --
   ---------------------------

   function BA_To_Node_Ref_Or_Val (V : BA_Data) return Node_Ref_Or_Val is
      function UI_From_LLI is new UI_From_Integral (LLI);
      Ret : Uint;

   begin
      --  If this isn't valid, return an invalid value

      if No (V) then
         return No_Uint;

      --  If we already have a Node_Ref, return it.

      elsif V.T_Value /= No_Uint then
         return V.T_Value;

      --  Otherwise, we have a constant.  If negative, make a Negate_Expr.

      else
         Ret := UI_From_LLI (Get_Const_Int_Value (V.C_Value));
         return (if   Ret < 0 then Create_Node (Negate_Expr, UI_Negate (Ret))
                 else Ret);
      end if;
   end BA_To_Node_Ref_Or_Val;

   ---------------------
   -- Annotated_Value --
   ---------------------

   function Annotated_Value (V : BA_Data) return Node_Ref_Or_Val is
      U : constant Uint := BA_To_Node_Ref_Or_Val (V);

   begin
      return (if U = No_Uint then Uint_0 else U);
   end Annotated_Value;

   ---------------------------
   -- Annotated_Object_Size --
   ---------------------------

   function Annotated_Object_Size (TE : Entity_Id) return Node_Ref_Or_Val is
      Use_Max      : constant Boolean := Is_Unconstrained_Record (TE);
      TE_Byte_Size : constant BA_Data :=
        BA_Type_Size (TE, Max_Size => Use_Max);
      TE_Bit_Size  : constant BA_Data := BA_Mul (TE_Byte_Size, BA_Const (8));

   begin
      return Annotated_Value (TE_Bit_Size);
   end Annotated_Object_Size;

   -------------
   -- BA_Unop --
   -------------

   function BA_Unop
     (V    : BA_Data;
      F    : Unop_Access;
      C    : TCode;
      Name : String := "") return BA_Data is

   begin
      --  If we don't have an input, propagate that to the output.
      if No (V) then
         return V;

      --  If we have a constant, perform the operation on the constant and
      --  return it.

      elsif BA_Is_Const (V) then
         return (False, F (V.C_Value, Name), No_Uint);

      --  Otherwise, create a new representation tree node

      else
         return (False, No_GL_Value, Create_Node (C, V.T_Value));
      end if;

   end BA_Unop;

   --------------
   -- BA_Binop --
   --------------

   function BA_Binop
     (V1, V2 : BA_Data;
      F      : Binop_Access;
      C      : TCode;
      Name   : String := "") return BA_Data
   is
      Op1, Op2 : Node_Ref_Or_Val;

   begin
      --  If both are constants, do the operation as a constant and return
      --  that value.  Unfortunately, LLVM doesn't check for overflow in
      --  the constant case, so we have to do it.

      if BA_Is_Const (V1) and then BA_Is_Const (V2) then
         declare
            Res     : constant GL_Value := F (V1.C_Value, V2.C_Value, Name);
            V1_Neg  : constant Boolean  := BA_Const_Int (V1) < 0;
            V2_Neg  : constant Boolean  := BA_Const_Int (V2) < 0;
            Res_Neg : constant Boolean  := Get_Const_Int_Value (Res) < 0;

         begin
            case C is
               when Plus_Expr =>
                  if V1_Neg = V2_Neg and then Res_Neg /= V1_Neg then
                     return No_BA;
                  end if;
               when Minus_Expr =>
                  if V1_Neg = not V2_Neg and then Res_Neg /= V1_Neg then
                     return No_BA;
                  end if;
               when Mult_Expr =>
                  if Res_Neg /= V1_Neg xor V2_Neg then
                     return No_BA;
                  end if;
               when others =>
                  null;
            end case;

            return (False, Res, No_Uint);
         end;
      end if;

      --  Otherwise, get our two operands as a node reference or Uint

      Op1 :=  BA_To_Node_Ref_Or_Val (V1);
      Op2 :=  BA_To_Node_Ref_Or_Val (V2);

      --  If either isn't valid, return invalid

      if Op1 = No_Uint or else Op2 = No_Uint then
         return No_BA;

         --  Otherwise build and return a node.  If there's a constant,
         --  it should be in the second position.

      else
         return (False, No_GL_Value,
                 Create_Node (C, (if Is_Static_SO_Ref (Op1) then Op2 else Op1),
                              (if Is_Static_SO_Ref (Op1) then Op1 else Op2)));
      end if;

   end BA_Binop;

   --------------
   -- BA_I_Cmp --
   --------------

   function BA_I_Cmp
     (Op       : Int_Predicate_T;
      LHS, RHS : BA_Data;
      Name     : String := "") return BA_Data
   is
      LHS_Op, RHS_Op : Node_Ref_Or_Val;
      TC             : TCode;

   begin
      --  If both are constants, do the operation as a constant and return
      --  that value.

      if BA_Is_Const (LHS) and then BA_Is_Const (RHS) then
         return (False, I_Cmp (Op, LHS.C_Value, RHS.C_Value, Name), No_Uint);
      end if;

      --  Otherwise, get our two operands as a node reference or Uint

      LHS_Op :=  BA_To_Node_Ref_Or_Val (LHS);
      RHS_Op :=  BA_To_Node_Ref_Or_Val (RHS);

      --  If either isn't valid, return invalid

      if LHS_Op = No_Uint or else RHS_Op = No_Uint then
         return No_BA;

      --  Otherwise, build and return a node

      else
         case Op is
            when Int_EQ =>
               TC := Eq_Expr;
            when Int_NE =>
               TC := Ne_Expr;
            when Int_UGT | Int_SGT =>
               TC := Gt_Expr;
            when Int_UGE | Int_SGE =>
               TC := Ge_Expr;
            when Int_ULT | Int_SLT =>
               TC := Lt_Expr;
            when Int_ULE | Int_SLE =>
               TC := Le_Expr;
            when others =>
               pragma Assert (False);
         end case;

         return (False, No_GL_Value, Create_Node (TC, LHS_Op, RHS_Op));
      end if;

   end BA_I_Cmp;

   ------------
   -- BA_Min --
   ------------

   function BA_Min (V1, V2 : BA_Data; Name : String := "") return BA_Data is
     (BA_Binop (V1, V2, Build_Min'Access, Min_Expr, Name));

   ------------
   -- BA_Max --
   ------------

   function BA_Max (V1, V2 : BA_Data; Name : String := "") return BA_Data is
     (BA_Binop (V1, V2, Build_Max'Access, Max_Expr, Name));

   ---------------
   -- BA_Select --
   ---------------

   function BA_Select
     (V_If, V_Then, V_Else : BA_Data; Name : String := "") return BA_Data
   is
      pragma Unreferenced (Name);
      If_Op, Then_Op, Else_Op : Node_Ref_Or_Val;

   begin
      --  If the first is a constant, return the appropriate leg

      if BA_Is_Const (V_If) then
         return (if BA_Is_Const_0 (V_If) then V_Else else V_Then);
      end if;

      --  Otherwise, get our operands as a node reference or Uint

      If_Op   :=  BA_To_Node_Ref_Or_Val (V_If);
      Then_Op :=  BA_To_Node_Ref_Or_Val (V_Then);
      Else_Op :=  BA_To_Node_Ref_Or_Val (V_Else);

      --  If any isn't valid, return invalid

      if If_Op = No_Uint or else Then_Op = No_Uint
        or else Else_Op = No_Uint
      then
         return No_BA;

      --  Otherwise, build and return a node

      else
         return (False, No_GL_Value,
                 Create_Node (Cond_Expr, If_Op, Then_Op, Else_Op));
      end if;

   end BA_Select;

   ------------------
   -- BA_Type_Size --
   ------------------

   function BA_Type_Size
     (TE         : Entity_Id;
      V          : GL_Value := No_GL_Value;
      Max_Size   : Boolean := False;
      No_Padding : Boolean := False) return BA_Data is
   begin
      --  If a value was specified and it's data, then it must be of a
      --  fixed size.  That's the size we're looking for.

      if Present (V) and then Relationship (V) = Data
        and then not Max_Size and then not No_Padding
      then
         return BA_From_Const (Get_Type_Size (Type_Of (V)));
      elsif Is_Record_Type (TE) then
         return BA_Record_Type_Size (TE, V,
                                     Max_Size   => Max_Size,
                                     No_Padding => No_Padding);
      elsif Is_Array_Type (TE) and then not Is_Constrained (TE) then
         return No_BA;
      elsif Is_Array_Type (TE) then
         return BA_Array_Type_Size (TE, V, Max_Size);
      elsif Ekind (TE) = E_Subprogram_Type then
         return No_BA;
      else
         return BA_From_Const (Get_Type_Size (Create_Type (TE)));
      end if;

   end BA_Type_Size;

   ------------------
   -- BA_Emit_Expr --
   ------------------

   function BA_Emit_Expr
     (V : Node_Id; LHS : BA_Data := No_BA) return BA_Data
   is
      pragma Unreferenced (LHS);
      SO_Info : Dynamic_SO_Ref := Get_SO_Ref (V);

   begin
      --  If we didn't already get an SO_Ref for this expression, get one

      if SO_Info = No_Uint then
         --  If this expression contains a discriminant, see if it's just the
         --  discriminant.  If so, return a tree node for it.
         --  ??? If not, for now return no value.

         if Contains_Discriminant (V) then
            if Nkind (V) = N_Identifier
              and then Ekind (Entity (V)) = E_Discriminant
            then
               SO_Info := Create_Discrim_Ref (Entity (V));
            else
               return No_BA;
            end if;

         --  Otherwise, see if this is a constant

         elsif Is_No_Elab_Needed (V) then
            return (False, Emit_Expression (V), No_Uint);
         else
            SO_Info := Create_Node (Dynamic_Val, UI_From_Int (Var_Idx_For_BA));
            Var_Idx_For_BA := Var_Idx_For_BA + 1;
         end if;

         Set_SO_Ref (V, SO_Info);
      end if;

      --  And now return the value

      return (False, No_GL_Value, SO_Info);
   end BA_Emit_Expr;

   procedure Dump_BA_Data (V : BA_Data) is
   begin
      if No (V) then
         Write_Line ("None");
      elsif BA_Is_Const (V) then
         Dump_LLVM_Value (LLVM_Value (V.C_Value));
      else
         lgx (V.T_Value);
      end if;
   end  Dump_BA_Data;

end GNATLLVM.Types;
