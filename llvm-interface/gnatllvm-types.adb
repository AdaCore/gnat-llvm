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
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;

with GNATLLVM.Arrays;  use GNATLLVM.Arrays;
with GNATLLVM.Compile; use GNATLLVM.Compile;
with GNATLLVM.Records; use GNATLLVM.Records;
with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

package body GNATLLVM.Types is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Create_Subprogram_Type
     (Param_Ident  : Entity_Id;
      Return_Type  : Entity_Id;
      Takes_S_Link : Boolean) return Type_T
     with Pre  => Present (Param_Ident) and then Is_Type_Or_Void (Return_Type),
          Post => Present (Create_Subprogram_Type'Result);
   --  Helper for public Create_Subprogram_Type functions: the public
   --  ones harmonize input and this one actually creates the LLVM
   --  type for subprograms.  Return_Type will be of Ekind E_Void if
   --  this is a procedure.

   function Create_Subprogram_Access_Type (T : Type_T) return Type_T
     with Pre  => Present (T),
          Post => Present (Create_Subprogram_Access_Type'Result);
   --  Return a structure type that embeds Subp_Type and a static link pointer

   function GNAT_Equivalent_Type (TE : Entity_Id) return Entity_Id
     with Pre => Is_Type (TE), Post => Is_Type (GNAT_Equivalent_Type'Result);
   --  Returns a type that we consider equivalent to TE, which may be
   --  TE itself if we are to use that type.

   -----------------------
   -- Build_Struct_Type --
   -----------------------

   function Build_Struct_Type
     (Types : Type_Array; Packed : Boolean := False) return Type_T is
   begin
      return Struct_Type_In_Context
        (LLVM_Context, Types'Address, Types'Length, Packed);
   end Build_Struct_Type;

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
      Dest_Uns    : constant Boolean := Is_Unsigned_Type (V);
      Src_Size    : constant unsigned_long_long :=
        Get_LLVM_Type_Size_In_Bits (V);
      Dest_Usize  : constant Uint :=
        (if Is_Modular_Integer_Type (TE) or else TE = Standard_Boolean
         then RM_Size (TE) else Esize (TE));
      Dest_Size   : constant unsigned_long_long :=
        unsigned_long_long (UI_To_Int (Dest_Usize));
      Is_Trunc    : constant Boolean := Dest_Size < Src_Size;
      Subp        : Cvtf := null;

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
         return Convert_To_Access_To (V, Full_Designated_Type (TE));

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
            Size_In_Bits : constant unsigned_long_long :=
              Get_LLVM_Type_Size_In_Bits (V);
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
                Const_Real (V, double (PredHalf));
            --  ?? The conversion to "double" above may be problematic,
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

   --------------------------
   -- Convert_To_Access_To --
   --------------------------

   function Convert_To_Access_To
     (V : GL_Value; TE : Entity_Id) return GL_Value
   is
      Unc_Src  : constant Boolean := Is_Access_Unconstrained (V);
      Unc_Dest : constant Boolean :=
        Is_Array_Type (TE) and then not Is_Constrained (TE);

   begin
      --  If neither is constrained, but they aren't the same type, just do
      --  a pointer cast.  If both are constrained, we return the input
      --  unchanged (the front end is responsible for this making sense).
      --  Otherwise, we have to handle converting between fat and raw
      --  pointers.

      if not Unc_Src and not Unc_Dest then
         if Full_Designated_Type (V) = TE then
            return V;
         else
            return Ptr_To_Ref (V, TE);
         end if;

      elsif Unc_Src and then Unc_Dest then
         return V;

      elsif Unc_Src and then not Unc_Dest then
         return Convert_To_Access_To (Array_Data (V), TE);
      else
         pragma Assert (not Unc_Src and then Unc_Dest);
         return Array_Fat_Pointer (TE, V);
      end if;
   end Convert_To_Access_To;

   --------------------------------
   -- Build_Unchecked_Conversion --
   --------------------------------

   function Build_Unchecked_Conversion
     (N : Node_Id; TE : Entity_Id) return GL_Value
   is
      type Opf is access function (V : GL_Value; TE : Entity_Id; Name : String)
        return GL_Value;

      T        : constant Type_T    := Create_Type (TE);
      V        : constant GL_Value  := Emit_Expression (N);
      Subp     : Opf                := null;

   begin
      --  If the value is already of the desired LLVM type, we're done.

      if Type_Of (V) = T then
         return V;

      --  If converting pointer to pointer or pointer to/from integer, we
      --  just copy the bits using the appropriate instruction.

      elsif Is_Access_Type (TE) and then Is_Scalar_Type (V) then
         Subp := Int_To_Ptr'Access;
      elsif Is_Scalar_Type (TE) and then Is_Access_Type (V) then
         Subp := Ptr_To_Int'Access;
      elsif Is_Access_Type (TE) and then Is_Access_Type (V)
        and then not Is_Access_Unconstrained (V)
        and then not Is_Access_Unconstrained (TE)
      then
         Subp := Pointer_Cast'Access;

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
        and then (unsigned_long_long'(Get_LLVM_Type_Size_In_Bits (T)) =
                    unsigned_long_long'(Get_LLVM_Type_Size_In_Bits (V)))
      then
         return Bit_Cast (V, TE);

      --  Otherwise, these must be cases where we have to convert by
      --  pointer punning.  If the source is a type of dynamic size, the
      --  value is already a pointer.  Otherwise, we have to make it a
      --  pointer.  ??? This code has a problem in that it calls Emit_LValue
      --  on an expression that's already been elaborated, but let's fix
      --  that double-elaboration issue later.

      else
         declare
            Addr : constant GL_Value :=
              (if Is_Dynamic_Size (V) then V else Emit_LValue (N));
         begin
            return Need_Value (Ptr_To_Ref (Addr, TE, "unc-ptr-cvt"), TE);
         end;
      end if;

      --  If we get here, we should have set Subp to point to the function
      --  to call to do the conversion.

      return Subp (V, TE, "unchecked-conv");
   end Build_Unchecked_Conversion;

   --------------------
   -- Coerce_To_Type --
   --------------------
   function Coerce_To_Type (V : GL_Value; TE : Entity_Id) return GL_Value is
      Value : GL_Value := V;
      Temp  : GL_Value;

   begin
      --  Because V might be a reference, we have to test for that case first.
      --  If TE is an access type, then we just have to convert it to that
      --  type.

      if Is_Access_Type (Value) then
         if Is_Access_Type (TE) then
            Value := Convert_To_Access_To (Value, Full_Designated_Type (TE));
         else
            --  If not, the only case where this should happen is if its
            --  type is of dynamic size, making it a reference.  In that
            --  case, we have to convert it to the type we need, then
            --  de-reference to get a value, which will be of the proper
            --  type.

            pragma Assert (Is_Dynamic_Size (Full_Designated_Type (Value)));
            Value := Load (Convert_To_Access_To (Value, TE));
         end if;

      elsif Full_Etype (Value) /= TE then

         --  If both types are elementary, just convert.  If both are array
         --  types, the LLVM type should be the same for both.
         --  Unfortunately, that's not the case for records and the only
         --  way to fix that is to essentially do an unchecked conversion
         --  by writing the value to memory, converting the pointer, and
         --  loading it again.

         if Is_Elementary_Type (TE) then
            Value := Convert_To_Elementary_Type (Value, TE);
         elsif Is_Record_Type (TE) then
            Temp := Alloca (Full_Etype (Value));
            Store (Value, Temp);
            Value := Load (Ptr_To_Ref (Temp, TE));
         end if;
      end if;

      return Value;
   end Coerce_To_Type;

   ------------------
   -- Count_Params --
   ------------------

   function Count_Params (E : Entity_Id) return Nat is
      Cnt   : Nat := 0;
      Param : Entity_Id := First_Formal_With_Extras (E);

   begin
      while Present (Param) loop
         Cnt := Cnt + 1;
         Param := Next_Formal_With_Extras (Param);
      end loop;

      return Cnt;
   end Count_Params;

   ----------------------
   -- Bounds_To_Length --
   ----------------------

   function Bounds_To_Length
     (Low, High : GL_Value; TE : Entity_Id) return GL_Value
   is
      Cmp_Kind : constant Int_Predicate_T :=
        (if Is_Unsigned_Type (Low) then Int_UGT else Int_SGT);
      Is_Empty : constant GL_Value :=
      I_Cmp (Cmp_Kind, Low, High, "is-empty");
      Out_Low  : constant GL_Value := Convert_To_Elementary_Type (Low, TE);
      Out_High : constant GL_Value := Convert_To_Elementary_Type (High, TE);
      Const_1  : constant GL_Value := Const_Int (TE, Uint_1);
   begin
      return Build_Select
        (C_If   => Is_Empty,
         C_Then => Const_Null (TE),
         C_Else =>
           (if Out_Low = Const_1 then Out_High
            else NSW_Add (NSW_Sub (Out_High, Out_Low), Const_1)));
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

   --------------------------
   -- GNAT_Equivalent_Type --
   --------------------------

   function GNAT_Equivalent_Type (TE : Entity_Id) return Entity_Id is
   begin
      case Ekind (TE) is
         when E_Class_Wide_Subtype =>
            if Present (Equivalent_Type (TE)) then
               return Equivalent_Type (TE);
            end if;

         when E_Access_Protected_Subprogram_Type |
           E_Anonymous_Access_Protected_Subprogram_Type =>
            if Present (Equivalent_Type (TE)) then
               return Equivalent_Type (TE);
            end if;

         when E_Class_Wide_Type =>
            return Root_Type (TE);

         when E_Protected_Type | E_Protected_Subtype |
           E_Task_Type |  E_Task_Subtype =>
            if Present (Corresponding_Record_Type (TE)) then
               return Corresponding_Record_Type (TE);
            end if;

         when others =>
            null;
      end case;

      return TE;
   end GNAT_Equivalent_Type;

   ------------------------
   -- Create_Access_Type --
   ------------------------

   function Create_Access_Type (TE : Entity_Id) return Type_T
   is
      T : constant Type_T := Create_Type (TE);

   begin
      if Is_Array_Type (TE) and then not Is_Constrained (TE) then
         return Create_Array_Fat_Pointer_Type (TE);
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
         --  ?? pragma Assert (not Definition);
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

      --  See if we can get this from the equivalent type

      Def_Ident := GNAT_Equivalent_Type (TE);
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

            if Is_Boolean_Type (Def_Ident) then
               T := Int_Ty (1);
            elsif Is_Modular_Integer_Type (Def_Ident) then
               T := Int_Ty (RM_Size (Def_Ident));

            else
               T := Int_Ty (Esize (Def_Ident));
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

         when E_Access_Type .. E_General_Access_Type
           | E_Anonymous_Access_Type | E_Access_Subprogram_Type
           | E_Anonymous_Access_Subprogram_Type =>
            if Needs_Activation_Record (Full_Designated_Type (Def_Ident)) then
               T := Create_Subprogram_Access_Type
                 (Create_Type (Full_Designated_Type (Def_Ident)));
            else
               T := Create_Access_Type (Full_Designated_Type (Def_Ident));
            end if;

         when Record_Kind =>
            T := Create_Record_Type (Def_Ident);

         when Array_Kind =>
            --  Handle packed arrays

            if Present (Packed_Array_Impl_Type (Def_Ident)) then
               T := Create_Type (Packed_Array_Impl_Type (Def_Ident));
               Copy_Type_Info (Packed_Array_Impl_Type (Def_Ident), Def_Ident);
            else
               T := Create_Array_Type (Def_Ident);
            end if;

         when E_Subprogram_Type =>
            --  An anonymous access to a subprogram can point to any subprogram
            --  (nested or not), so it must accept a static link.

            T := Create_Subprogram_Type_From_Entity
              (Def_Ident, Takes_S_Link => Needs_Activation_Record (Def_Ident));

         when Fixed_Point_Kind =>
            T := Int_Ty (Esize (Def_Ident));

         when E_Incomplete_Type =>
            --  This is a taft amendment type, return a dummy type

            T := Void_Type;

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
      if Ekind (TE) in E_Signed_Integer_Type  |
                       E_Modular_Integer_Type |
                       E_Floating_Point_Type
      then
         return Create_TBAA_Scalar_Type_Node
           (MD_Builder, Get_Name (TE), TBAA_Root);
      else
         return No_Metadata_T;
      end if;
   end Create_TBAA;

   --------------------------
   -- Create_Discrete_Type --
   --------------------------

   procedure Create_Discrete_Type
     (TE : Entity_Id; T : out Type_T; Low, High : out GL_Value)
   is
      SRange : constant Node_Id := Scalar_Range (TE);

   begin
      --  Delegate LLVM Type creation to Create_Type

      T := Create_Type (TE);

      --  Compute the bounds

      pragma Assert (Nkind (SRange) = N_Range);
      Low := Build_Type_Conversion (Low_Bound (SRange), TE);
      High := Build_Type_Conversion (High_Bound (SRange), TE);

   end Create_Discrete_Type;

   --------------------------------------
   -- Create_Subprogram_Type_From_Spec --
   --------------------------------------

   function Create_Subprogram_Type_From_Spec (N : Node_Id) return Type_T
   is
      Def_Ident : constant Entity_Id := Defining_Entity (N);

   begin
      return Create_Subprogram_Type (Def_Ident, Full_Etype (Def_Ident), False);
   end Create_Subprogram_Type_From_Spec;

   ----------------------------------------
   -- Create_Subprogram_Type_From_Entity --
   ----------------------------------------

   function Create_Subprogram_Type_From_Entity
     (TE : Entity_Id; Takes_S_Link  : Boolean) return Type_T is
   begin
      return Create_Subprogram_Type (TE, Full_Etype (TE), Takes_S_Link);
   end Create_Subprogram_Type_From_Entity;

   ----------------------------
   -- Create_Subprogram_Type --
   ----------------------------

   function Create_Subprogram_Type
     (Param_Ident   : Entity_Id;
      Return_Type   : Entity_Id;
      Takes_S_Link  : Boolean) return Type_T
   is
      LLVM_Return_Typ : Type_T :=
        (if Ekind (Return_Type) = E_Void
         then Void_Type else Create_Type (Return_Type));
      Orig_Arg_Count  : constant Nat := Count_Params (Param_Ident);
      Args_Count      : constant Nat :=
        Orig_Arg_Count + (if Takes_S_Link then 1 else 0) +
          (if Ekind (Return_Type) /= E_Void
             and then Is_Dynamic_Size (Return_Type)
           then 1 else 0);
      Arg_Types       : Type_Array (1 .. Args_Count);
      Param_Ent       : Entity_Id := First_Formal_With_Extras (Param_Ident);
      J               : Nat := 1;

   begin
      --  First, Associate an LLVM type for each Ada subprogram parameter

      while Present (Param_Ent) loop
         declare
            Param_Type : constant Node_Id := Full_Etype (Param_Ent);
         begin
            --  If this is an out parameter, or a parameter whose type is
            --  unconstrained, take a pointer to the actual parameter.

            Arg_Types (J) :=
              (if Param_Needs_Ptr (Param_Ent)
               then Create_Access_Type (Param_Type)
               else Create_Type (Param_Type));
         end;

         J := J + 1;
         Param_Ent := Next_Formal_With_Extras (Param_Ent);
      end loop;

      --  Set the argument for the static link, if any

      if Takes_S_Link then
         Arg_Types (Orig_Arg_Count + 1) := Void_Ptr_Type;
      end if;

      --  If the return type has dynamic size, we need to add a parameter
      --  to which we pass the address for the return to be placed in.

      if Ekind (Return_Type) /= E_Void
        and then Is_Dynamic_Size (Return_Type)
      then
         Arg_Types (Arg_Types'Last) := Create_Access_Type (Return_Type);
         LLVM_Return_Typ := Void_Type;
      end if;

      return Fn_Ty (Arg_Types, LLVM_Return_Typ);
   end Create_Subprogram_Type;

   -----------------------------------
   -- Create_Subprogram_Access_Type --
   -----------------------------------

   function Create_Subprogram_Access_Type (T : Type_T) return Type_T is
      pragma Unreferenced (T);

   begin
      return
        Build_Struct_Type ((1 => Void_Ptr_Type, 2 => Void_Ptr_Type));
   end Create_Subprogram_Access_Type;

   -----------------------
   -- Allocate_For_Type --
   -----------------------

   function Allocate_For_Type
     (TE   : Entity_Id;
      V    : GL_Value := No_GL_Value;
      Name : String := "") return GL_Value
   is
      Element_Typ : Entity_Id;
      Num_Elts    : GL_Value;

   begin
      --  We have three cases.  If the object is not of a dynamic size,
      --  we just do the alloca and that's all.

      if not Is_Dynamic_Size (TE) then
         return Alloca (TE, Name);
      end if;

      --  Otherwise, we have to do some sort of dynamic allocation.  If
      --  this is an array of a component that's not of dynamic size, then
      --  we can allocate an array of the component type corresponding to
      --  the array type and cast it to a pointer to the actual type.
      --  If not, we have to allocate it as an array of bytes.

      if Is_Array_Type (TE)
        and then not Is_Dynamic_Size (Full_Component_Type (TE))
      then
         Element_Typ := Full_Component_Type (TE);
         Num_Elts    := Get_Array_Elements (V, TE, For_Type => No (V));
      else
         Element_Typ := Standard_Short_Short_Integer;
         Num_Elts    := Get_Type_Size (TE, V, For_Type => No (V));
      end if;

      return Ptr_To_Ref
        (Array_Alloca (Element_Typ, Num_Elts, "dyn-array"), TE, Name);

   end Allocate_For_Type;

   ---------------------------
   --  Convert_To_Size_Type --
   ---------------------------

   function Convert_To_Size_Type (V : GL_Value) return GL_Value is
   begin
      return Convert_To_Elementary_Type (V, Size_Type);
   end Convert_To_Size_Type;

   ------------------------
   -- Get_Type_Alignment --
   ------------------------

   function Get_Type_Alignment (TE : Entity_Id) return unsigned is
   begin

      --  Easiest case is not dynamic size: then just LLVM type's alignment

      if not Is_Dynamic_Size (TE) then
         return Get_Type_Alignment (Create_Type (TE));

      --  If it's an array, it's the alignment of the component type

      elsif Is_Array_Type (TE) then
         return Get_Type_Alignment (Component_Type (TE));

      --  Otherwise, it must be a record.  Use the highest alignment of
      --  any field.

      else
         pragma Assert (Is_Record_Type (TE));
         declare
            Field         : Entity_Id := First_Entity (TE);
            Largest_Align : unsigned := 1;

         begin
            while Present (Field) loop
               if Ekind_In (Field, E_Discriminant, E_Component) then
                  Largest_Align
                    := unsigned'Max (Largest_Align,
                                     Get_Type_Alignment (Full_Etype (Field)));
               end if;

               Next_Entity (Field);
            end loop;

            return Largest_Align;
         end;
      end if;
   end Get_Type_Alignment;

   -------------------
   -- Get_Type_Size --
   -------------------

   function Get_Type_Size
     (TE       : Entity_Id;
      V        : GL_Value;
      For_Type : Boolean := False) return GL_Value
   is
   begin

      if Is_Record_Type (TE) then
         return Get_Record_Type_Size (TE, V, For_Type);

      elsif Is_Array_Type (TE) and then Is_Dynamic_Size (TE) then
         return Get_Array_Type_Size (TE, V, For_Type);

      else
         return Get_LLVM_Type_Size (Create_Type (TE));
      end if;

   end Get_Type_Size;

   ------------------
   -- Compute_Size --
   ------------------

   function Compute_Size
     (Left_Type, Right_Type   : Entity_Id;
      Left_Value, Right_Value : GL_Value) return GL_Value is

   begin
      --  Use the type of right side unless its complexity is more
      --  than that of the size of the type on the left side.

      if Get_Type_Size_Complexity (Right_Type) >
        Get_Type_Size_Complexity (Left_Type)
      then
         return Get_Type_Size (Left_Type, Left_Value);
      else
         return Get_Type_Size (Right_Type, Right_Value);
      end if;

   end Compute_Size;

   ------------------------------
   -- Get_Type_Size_Complexity --
   ------------------------------

   function Get_Type_Size_Complexity (TE : Entity_Id) return Natural is
   begin

      if Is_Record_Type (TE) then
         return Get_Record_Size_Complexity (TE);
      elsif Is_Array_Type (TE) then
         return Get_Array_Size_Complexity (TE);

      else
         --  All other types are constant size

         return 0;

      end if;
   end Get_Type_Size_Complexity;

end GNATLLVM.Types;
