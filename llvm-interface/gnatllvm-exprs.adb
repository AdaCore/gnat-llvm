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

with Ada.Unchecked_Deallocation;

with Errout;     use Errout;
with Eval_Fat;   use Eval_Fat;
with Exp_Code;   use Exp_Code;
with Nlists;     use Nlists;
with Sem_Aggr;   use Sem_Aggr;
with Snames;     use Snames;
with Stand;      use Stand;
with Stringt;    use Stringt;
with Uintp.LLVM; use Uintp.LLVM;
with Urealp;     use Urealp;

with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Blocks;       use GNATLLVM.Blocks;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Conditionals; use GNATLLVM.Conditionals;
with GNATLLVM.GLType;       use GNATLLVM.GLType;
with GNATLLVM.Records;      use GNATLLVM.Records;
with GNATLLVM.Subprograms;  use GNATLLVM.Subprograms;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Utils;        use GNATLLVM.Utils;

package body GNATLLVM.Exprs is

   ----------------
   -- Emit_Undef --
   ----------------

   function Emit_Undef (TE : Entity_Id) return GL_Value is
     ((if Is_Loadable_Type (TE) then Get_Undef (TE) else Get_Undef_Ref (TE)));

   ------------------
   -- Emit_Literal --
   ------------------

   function Emit_Literal (N : Node_Id) return GL_Value is
      TE : constant Entity_Id := Full_Etype (N);
      V  : GL_Value;

   begin
      case Nkind (N) is
         when N_Character_Literal =>

            --  If a Entity is present, it means that this was one of the
            --  literals in a user-defined character type.

            return Const_Int (TE, (if Present (Entity (N))
                                   then Enumeration_Rep (Entity (N))
                                   else Char_Literal_Value (N)));

         when N_Integer_Literal =>
            return Const_Int (TE, Intval (N));

         when N_Real_Literal =>
            if Is_Fixed_Point_Type (TE) then
               return Const_Int (TE, Corresponding_Integer_Value (N));
            else
               declare
                  Val : Ureal := Realval (N);

               begin
                  --  Handle zero separately to save time, but be sure we
                  --  get the proper sign for zero.

                  if UR_Is_Zero (Val) then
                     V := Const_Real (TE, 0.0);
                     return (if UR_Is_Negative (Val) then F_Neg (V) else V);
                  end if;

                  --  First convert the value to a machine number if it isn't
                  --  already. That will force the base to 2 for non-zero
                  --  values and simplify the rest of the logic.

                  if not Is_Machine_Number (N) then
                     Val := Machine (Full_Base_Type (TE), Val, Round_Even, N);
                  end if;

                  pragma Assert (Rbase (Val) = 2);

                  --  Next get the bits for the numerator from its Uint
                  --  value and the value of the denominator (which we know
                  --  must fit into an integer) and call LLVM routines to
                  --  convert it to the desired FP value and then negate it
                  --  if needed.

                  declare
                     Words : constant Word_Array :=
                       UI_To_Words (Numerator (Val));

                  begin
                     V := Get_Float_From_Words_And_Exp
                       (TE, -UI_To_Int (Denominator (Val)), Words);
                     return (if UR_Is_Negative (Val) then F_Neg (V) else V);
                  end;
               end;
            end if;

         when N_String_Literal =>
            declare
               Str_Id  : constant String_Id := Strval (N);
               Length  : constant Nat       := String_Length (Str_Id);
               Elmt_TE : constant Entity_Id := Full_Component_Type (TE);

            begin
               --  If this is a normal string, where the size of a character
               --  is a byte, use Const_String to create the string.

               if Get_Type_Size_In_Bits (Type_Of (Elmt_TE)) = 8 then
                  declare
                     type String_Access is access String;
                     procedure Free is new Ada.Unchecked_Deallocation
                       (String, String_Access);

                     Str : String_Access := new String (1 .. Integer (Length));

                  begin
                     for J in Str'Range loop
                        Str (J) :=
                          Get_Character (Get_String_Char (Str_Id, Nat (J)));
                     end loop;

                     V := Const_String (Str.all, TE);
                     Free (Str);
                     return V;
                  end;

               else
                  --  Otherwise, we have to create an array for each character
                  --  this might be large, so don't try to allocate it in
                  --  the stack.

                  declare
                     Elements : Access_GL_Value_Array :=
                       new GL_Value_Array (1 .. Length);

                  begin
                     for J in Elements'Range loop
                        Elements (J) :=
                          Const_Int (Elmt_TE,
                                     ULL (Get_String_Char (Str_Id, Nat (J))));
                     end loop;

                     V := Const_Array (Elements.all, TE);
                     Free (Elements);
                     return V;
                  end;
               end if;
            end;

         when others =>
            Error_Msg_N ("unhandled literal node", N);
            return Get_Undef (TE);

      end case;
   end Emit_Literal;

   ---------------------------
   -- Emit_Binary_Operation --
   ---------------------------

   function Emit_Binary_Operation (N : Node_Id) return GL_Value is
      type Opf is access function
        (LHS, RHS : GL_Value; Name : String := "") return GL_Value;

      LHS_Node   : constant Node_Id   := Left_Opnd (N);
      RHS_Node   : constant Node_Id   := Right_Opnd (N);
      Left_Type  : constant Entity_Id := Full_Etype (LHS_Node);
      Right_Type : constant Entity_Id := Full_Etype (RHS_Node);
      LHS_BT     : constant Entity_Id := Full_Base_Type (Left_Type);
      RHS_BT     : constant Entity_Id := Full_Base_Type (Right_Type);
      LVal       : constant GL_Value  := Emit_Convert_Value (LHS_Node, LHS_BT);
      RVal       : constant GL_Value  := Emit_Convert_Value (RHS_Node, RHS_BT);
      FP         : constant Boolean   := Is_Floating_Point_Type (LHS_BT);
      Ovfl_Check : constant Boolean   := Do_Overflow_Check (N);
      Unsign     : constant Boolean   := Is_Unsigned_Type (LHS_BT);
      Subp       : Opf                := null;
      Result     : GL_Value;
      Ovfl_Name  : String (1 .. 4);

   begin
      case Nkind (N) is
         when N_Op_Add =>
            if Ovfl_Check then
               Ovfl_Name := (if Unsign then "uadd" else "sadd");
            else
               Subp := (if FP then F_Add'Access else Add'Access);
            end if;

         when N_Op_Subtract =>
            if Ovfl_Check then
               Ovfl_Name := (if Unsign then "usub" else "ssub");
            else
               Subp := (if FP then F_Sub'Access else Sub'Access);
            end if;

         when N_Op_Multiply =>
            if Ovfl_Check then
               Ovfl_Name := (if Unsign then "umul" else "smul");
            else
               Subp := (if FP then F_Mul'Access else Mul'Access);
            end if;

         when N_Op_Divide =>
            Subp := (if FP then F_Div'Access
                     elsif Unsign then U_Div'Access else S_Div'Access);

         when N_Op_Rem =>
            Subp := (if Unsign then U_Rem'Access else S_Rem'Access);

         when N_Op_And =>
            Subp := Build_And'Access;

         when N_Op_Or =>
            Subp := Build_Or'Access;

         when N_Op_Xor =>
            Subp := Build_Xor'Access;

         when N_Op_Mod =>
            Subp := (if Unsign then U_Rem'Access else S_Rem'Access);

         when others =>
            null;

      end case;

       --  We either do a normal operation if Subp is not null or an
       --  overflow test.

      if Subp /= null then
         Result := Subp (LVal, RVal);
      else
         pragma Assert (Do_Overflow_Check (N));

         declare
            Func      : constant GL_Value  := Build_Intrinsic
              (Overflow, "llvm." & Ovfl_Name & ".with.overflow.i", LHS_BT);
            Fn_Ret    : constant GL_Value  :=
              Call (Func, LHS_BT, (1 => LVal, 2 => RVal));
            Overflow  : constant GL_Value  :=
              Extract_Value (Standard_Boolean, Fn_Ret, 1, "overflow");
            Label_Ent : constant Entity_Id :=
              Get_Exception_Goto_Entry (N_Raise_Constraint_Error);
            BB_Next   : Basic_Block_T;

         begin
            if Present (Label_Ent) then
               BB_Next := Create_Basic_Block;
               Build_Cond_Br
                 (Overflow, Get_Label_BB (Label_Ent), BB_Next);
               Position_Builder_At_End (BB_Next);
            else
               Emit_Raise_Call_If (Overflow, N);
            end if;

            Result := Extract_Value (LHS_BT, Fn_Ret, 0);
         end;
      end if;

      --  If this is a signed mod operation, we have to adjust the result,
      --  since what we did is a rem operation.  If the result is zero or
      --  the result and the RHS have the same sign, the result is correct.
      --  Otherwise, we have to add the RHS to the result.  Two values have
      --  the same sign iff their xor is non-negative, which is the best
      --  code for the general case, but having a variable as the second
      --  operand of mod is quite rare, so it's best to do slightly less
      --  efficient code for the general case that will get constant-folded
      --  in the constant case.

      if not Unsign and Nkind (N) = N_Op_Mod then
         declare
            Add_Back      : constant GL_Value := Add (Result, RVal);
            RHS_Neg       : constant GL_Value :=
              I_Cmp (Int_SLT, RVal, Const_Null (RVal), "RHS-neg");
            Result_Nonpos : constant GL_Value :=
              I_Cmp (Int_SLE, Result, Const_Null (Result), "result-nonpos");
            Result_Nonneg : constant GL_Value :=
              I_Cmp (Int_SGE, Result, Const_Null (Result), "result-nonneg");
            Signs_Same    : constant GL_Value :=
              Build_Select (RHS_Neg, Result_Nonpos, Result_Nonneg, "signsame");

         begin
            Result := Build_Select (Signs_Same, Result, Add_Back);
         end;

      --  If this is an integer division operation with Round_Result set,
      --  we have to do that rounding.  There are two different cases, one
      --  for signed and one for unsigned.

      elsif Nkind (N) = N_Op_Divide and then Rounded_Result (N)
        and then Unsign and then not FP
      then
         declare

            --  We compute the remainder.  If the remainder is greater then
            --  half of the RHS (e.g., > (RHS + 1) / 2), we add one to the
            --  result.

            One         : constant GL_Value := Const_Int (RVal, Uint_1);
            Remainder   : constant GL_Value := U_Rem (LVal, RVal);
            Half_RHS    : constant GL_Value := L_Shr (Sub (RVal, One), One);
            Plus_One    : constant GL_Value := Add (Result, One);
            Need_Adjust : constant GL_Value :=
              I_Cmp (Int_UGT, Remainder, Half_RHS);

         begin
            Result := Build_Select (Need_Adjust, Plus_One, Result);
         end;

      elsif Nkind (N) = N_Op_Divide and then Rounded_Result (N)
        and then not Unsign and then not FP
      then
         declare

            --  We compute the quotient.  Then it gets more complicated.
            --  As in the mod case, we optimize for the case when RHS is a
            --  constant.  If twice the absolute value of the remainder is
            --  greater than RHS, we have to either add or subtract one
            --  from the result, depending on whether the remainder is the
            --  same sign as the RHS not.  Again, we optimize for the case
            --  where the RHS is a constant.

            One          : constant GL_Value  := Const_Int (RVal, Uint_1);
            Remainder    : constant GL_Value  := S_Rem (LVal, RVal);
            Rem_Neg      : constant GL_Value  :=
              I_Cmp (Int_SLT, Remainder, Const_Null (Remainder));
            Rem_Nonneg   : constant GL_Value  := Build_Not (Rem_Neg);
            Abs_Rem      : constant GL_Value  :=
              Build_Select (Rem_Neg, Neg (Remainder), Remainder);
            RHS_Neg      : constant GL_Value  :=
              I_Cmp (Int_SLT, RVal, Const_Null (RVal));
            Abs_RHS      : constant GL_Value  :=
              Build_Select (RHS_Neg, Neg (RVal), RVal);
            Need_Adjust  : constant GL_Value  :=
              I_Cmp (Int_UGE, Shl (Abs_Rem, One), Abs_RHS);
            Signs_Same    : constant GL_Value :=
              Build_Select (RHS_Neg, Rem_Neg, Rem_Nonneg, "signsame");
            Plus_One     : constant GL_Value  := Add (Result, One);
            Minus_One    : constant GL_Value  := Sub (Result, One);
            Which_Adjust : constant GL_Value  :=
              Build_Select (Signs_Same, Plus_One, Minus_One);

         begin
            Result := Build_Select (Need_Adjust, Which_Adjust, Result);
         end;
      end if;

      return Result;

   end Emit_Binary_Operation;

   --------------------------
   -- Emit_Unary_Operation --
   --------------------------

   function Emit_Unary_Operation (N : Node_Id) return GL_Value is
      Result : constant GL_Value  := Emit_Expression (Right_Opnd (N));
      TE     : constant Entity_Id := Full_Etype (Result);
      BT     : constant Entity_Id := Full_Base_Type (TE);

   begin
      case Nkind (N) is

         when N_Op_Not =>
            return (if   Is_Boolean_Type (Result)
                    then Build_Xor (Result, Const_Int (Result, ULL (1)))
                    else Build_Not (Result));

         when N_Op_Abs =>

            --  Emit: X >= 0 ? X : -X;

            declare
               Zero      : constant GL_Value := Const_Null (Result);
               Compare   : constant GL_Value :=
                 Emit_Elementary_Comparison (N_Op_Ge, Result, Zero);
               Neg_Expr  : constant GL_Value :=
                 (if   Is_Floating_Point_Type (Result)
                  then F_Neg (Result) else Neg (Result));

            begin
               if Is_Unsigned_Type (Result) then
                  return Result;
               else
                  return Build_Select (Compare, Result, Neg_Expr, "abs");
               end if;
            end;

         when N_Op_Plus =>
            return Result;

         when N_Op_Minus =>
            declare
               V : constant GL_Value  := Convert (Result, BT);

            begin
               if Is_Floating_Point_Type (BT) then
                  return F_Neg (V);
               elsif Do_Overflow_Check (N)
                 and then not Is_Unsigned_Type (BT)
               then
                  declare
                     Func      : constant GL_Value := Build_Intrinsic
                       (Overflow, "llvm.ssub.with.overflow.i", BT);
                     Fn_Ret    : constant GL_Value :=
                       Call (Func, TE, (1 => Const_Null (BT), 2 => V));
                     Overflow  : constant GL_Value :=
                       Extract_Value (Standard_Boolean, Fn_Ret, 1, "overflow");
                     Label_Ent : constant Entity_Id :=
                       Get_Exception_Goto_Entry (N_Raise_Constraint_Error);
                     BB_Next   : Basic_Block_T;

                  begin
                     if Present (Label_Ent) then
                        BB_Next := Create_Basic_Block;
                        Build_Cond_Br
                          (Overflow, Get_Label_BB (Label_Ent), BB_Next);
                        Position_Builder_At_End (BB_Next);
                     else
                        Emit_Raise_Call_If (Overflow, N);
                     end if;

                     return Extract_Value (BT, Fn_Ret, 0);
                  end;
               else
                  return Neg (V);
               end if;
            end;

         when others =>
            pragma Assert (False);
            return Emit_Undef (TE);
      end case;

   end Emit_Unary_Operation;

   -------------------------
   -- Emit_Overflow_Check --
   -------------------------

   procedure Emit_Overflow_Check (V : GL_Value; N : Node_Id) is
      In_TE      : constant Entity_Id := Full_Etype (V);
      Out_TE     : constant Entity_Id := Full_Etype (N);
      In_BT      : constant Entity_Id := Full_Base_Type (In_TE);
      Out_BT     : constant Entity_Id := Full_Base_Type (Out_TE);
      In_FP      : constant Boolean   := Is_Floating_Point_Type (In_TE);
      Out_FP     : constant Boolean   := Is_Floating_Point_Type (Out_TE);
      In_LB      : constant Node_Id   := Type_Low_Bound  (In_BT);
      In_UB      : constant Node_Id   := Type_High_Bound (In_BT);
      Out_LB     : constant Node_Id   := Type_Low_Bound  (Out_BT);
      Out_UB     : constant Node_Id   := Type_High_Bound (Out_BT);
      Label_Ent  : constant Entity_Id :=
        Get_Exception_Goto_Entry (N_Raise_Constraint_Error);
      Compare_LB : GL_Value           := No_GL_Value;
      Compare_UB : GL_Value           := No_GL_Value;
      BB_Raise   : Basic_Block_T;
      BB_Next    : Basic_Block_T;

   begin
      --  If both types are integers, we determine the need for each check
      --  individually by seeing if the output bounds are tighter than the
      --  input bounds.  But this isn't worth doing for FP since the
      --  chances of having a difference is very low.  Since an FP NaN or
      --  Inf always compares false, do the comparison so false is a
      --  failure.

      if In_FP or else Out_FP
        or else Get_Uint_Value (Out_LB) > Get_Uint_Value (In_LB)
      then
         Compare_LB := Emit_Elementary_Comparison
           (N_Op_Ge, V, Emit_Convert_Value (Out_LB, In_TE));
      end if;

      if In_FP or else Out_FP
        or else Get_Uint_Value (Out_UB) < Get_Uint_Value (In_UB)
      then
         Compare_UB := Emit_Elementary_Comparison
           (N_Op_Le, V, Emit_Convert_Value (Out_UB, In_TE));
      end if;

      --  If neither comparison is needed, we're done

      if No (Compare_LB) and then No (Compare_UB) then
         return;
      end if;

      --  Otherwise, make the labels and branch to them depending on the
      --  results of the tests.  If we're doing both comparisons, we'll have
      --  put both before the first test, but the optimizer can clean that up.

      BB_Raise :=
        (if   Present (Label_Ent) then Get_Label_BB (Label_Ent)
         else Create_Basic_Block);
      BB_Next := Create_Basic_Block;

      if Present (Compare_UB) then
         Build_Cond_Br (Compare_UB, BB_Next, BB_Raise);
         if Present (Compare_LB) then
            Position_Builder_At_End (BB_Next);
            BB_Next := Create_Basic_Block;
         end if;
      end if;

      if Present (Compare_LB) then
         Build_Cond_Br (Compare_LB, BB_Next, BB_Raise);
      end if;

      --  If we were branching to a label for the exception, we're done.
      --  Otherwise, generate a call to the raise statement.

      if No (Label_Ent) then
         Position_Builder_At_End (BB_Raise);
         Emit_Raise_Call (N, CE_Overflow_Check_Failed);
         Build_Br (BB_Next);
      end if;

      Position_Builder_At_End (BB_Next);
   end Emit_Overflow_Check;

   ----------------
   -- Emit_Shift --
   ----------------

   function Emit_Shift
     (Operation          : Node_Kind;
      LHS_Node, RHS_Node : Node_Id) return GL_Value
   is
      To_Left, Rotate, Arithmetic : Boolean := False;

      LHS       : constant GL_Value := Emit_Expression (LHS_Node);
      RHS       : constant GL_Value := Emit_Expression (RHS_Node);
      N         : constant GL_Value := Convert (RHS, LHS);
      LHS_Size  : constant GL_Value := Get_Type_Size_In_Bits (LHS);
      LHS_Bits  : constant GL_Value := Convert (LHS_Size, LHS);
      Result    : GL_Value          := LHS;
      Saturated : GL_Value;

   begin
      --  Extract properties for the operation we are asked to generate code
      --  for.  We defaulted to a right shift above.

      case Operation is
         when N_Op_Shift_Left =>
            To_Left := True;

         when N_Op_Shift_Right_Arithmetic =>
            Arithmetic := True;

         when N_Op_Rotate_Left =>
            To_Left := True;
            Rotate := True;

         when N_Op_Rotate_Right =>
            Rotate := True;

         when others =>
            null;
      end case;

      if Rotate then

         declare
            --  There's no "rotate" instruction in LLVM, so we have to use
            --  two shift instructions, just like in C. If we're rotating
            --  to the left:
            --
            --     Result := (Operand << Bits) | (Operand >> (Size - Bits));
            --               -----------------   --------------------------
            --                    Upper                   Lower
            --
            --  If we're rotating to the right, we switch the direction of
            --  the two shifts.
            --
            --  LLVM instructions return an undefined value for rotations
            --  with too many bits, so we must handle "multiple turns".

            Reduced_N   : constant GL_Value := U_Rem (N, LHS_Bits);
            Lower_Shift : constant GL_Value := Sub (LHS_Bits, Reduced_N);
            Reduced_Low : constant GL_Value := U_Rem (Lower_Shift, LHS_Bits);
            Upper       : constant GL_Value :=
              (if   To_Left
               then Shl   (LHS, Reduced_N,   "rotate-upper")
               else L_Shr (LHS, Reduced_N,   "rotate-upper"));
            Lower       : constant GL_Value :=
              (if   To_Left
               then L_Shr (LHS, Reduced_Low, "rotate-lower")
               else Shl   (LHS, Reduced_Low, "rotate-lower"));

         begin
            return Build_Or (Upper, Lower, "rotate-result");
         end;

      else
         --  First, compute the value using the underlying LLVM instruction

         Result :=
           (if    To_Left
            then  Shl (LHS, N)
            elsif Arithmetic then A_Shr (LHS, N) else L_Shr (LHS, N));

         --  If this is a packed array implementation type, we know the
         --  shift won't overflow, so we can just use the result.

         if Is_Packed_Array_Impl_Type (LHS) then
            return Result;
         end if;

         --  Otherwise, if the number of bits shifted is larger or equal to
         --  the number of bits in LHS, the underlying LLVM instruction
         --  returns an undefined value, so build what we want ourselves
         --  (we call this a "saturated value").

         Saturated :=
           (if Arithmetic

            --  If we're performing an arithmetic shift, the saturated
            --  value is 0 if LHS is positive and -1 otherwise (in this
            --  context, LHS is always interpreted as a signed integer).

            then Build_Select
              (C_If   => I_Cmp
                 (Int_SLT, LHS, Const_Null (LHS), "is-lhs-negative"),
               C_Then => Const_Ones (LHS),
               C_Else => Const_Null (LHS),
               Name   => "saturated")

            else Const_Null (LHS));

         --  Now, we must decide at runtime if it is safe to rely on the
         --  underlying LLVM instruction. If so, use it, otherwise return
         --  the saturated value.

         return Build_Select
           (C_If   => I_Cmp (Int_UGE, N, LHS_Bits, "is-saturated"),
            C_Then => Saturated,
            C_Else => Result,
            Name   => "sr-result");
      end if;
   end Emit_Shift;

   ------------------------------
   -- Emit_Attribute_Reference --
   ------------------------------

   function Emit_Attribute_Reference (N : Node_Id) return GL_Value
   is
      Attr : constant Attribute_Id := Get_Attribute_Id (Attribute_Name (N));
      TE   : constant Entity_Id    := Full_Etype (N);
      P_TE : Entity_Id             := Full_Etype (Prefix (N));
      Ret  : Uint;
      V    : GL_Value;

   begin
      --  First see if this is something we can compute from annotations
      --  in the tree.

      Ret := Get_Attribute_From_Annotation (N);
      if Ret /= No_Uint then
         return Const_Int (TE, Ret);
      end if;

      case Attr is
         when Attribute_Access | Attribute_Unchecked_Access
            | Attribute_Unrestricted_Access =>

            --  We store values as pointers, so getting an access to an
            --  expression is the same as getting an LValue and has the
            --  same constraints.  But we do have to be sure that it's of
            --  the right type.

            return Convert_To_Access (Emit_LValue (Prefix (N)), TE);

         when Attribute_Address | Attribute_Code_Address =>

            --  We need a single-word pointer, then convert it to the
            --  desired integral type.

            return Ptr_To_Int (Get (Emit_LValue (Prefix (N)),
                                    Reference_For_Integer),
                               TE, "attr-address");

         when Attribute_Pool_Address =>

            --  Evaluate this object.  We normally want to look at the
            --  address of the object itself (e.g., as a Reference), but if
            --  it's an access type, what we want is the value.  So convert
            --  it to a reference.

            V := Emit (Prefix (N), Prefer_LHS => True);
            if Is_Access_Type (P_TE) then
               V := From_Access (Get (V, Data));
            end if;

            --  If it's an unconstrained array, we want the location of the
            --  bounds, which is the first thing allocated.

            V := Get (V, (if   Is_Unconstrained_Array (V)
                          then Reference_To_Bounds_And_Data else Reference));
            return Ptr_To_Int (V, TE, "pool-address");

         when Attribute_Deref =>
            declare
               Expr : constant Node_Id := First (Expressions (N));
               pragma Assert (Is_Descendant_Of_Address (Full_Etype (Expr)));

            begin
               return Int_To_Ref (Emit_Expression (Expr), TE, "attr-deref");
            end;

         when Attribute_First  | Attribute_Last
            | Attribute_Length | Attribute_Range_Length =>

            declare
               BT          : constant Entity_Id := Full_Base_Type (P_TE);
               Dim         : constant Nat       :=
                 (if   Present (Expressions (N))
                  then UI_To_Int (Intval (First (Expressions (N)))) - 1
                  else 0);
               Array_Descr : GL_Value;
               Low, High   : GL_Value;

            begin
               if Is_Scalar_Type (P_TE) then
                  Low  := Emit_Convert_Value (Type_Low_Bound  (P_TE), BT);
                  High := Emit_Convert_Value (Type_High_Bound (P_TE), BT);

                  if Attr = Attribute_First then
                     V := Low;
                  elsif Attr = Attribute_Last then
                     V := High;
                  elsif Attr = Attribute_Range_Length then
                     V := Bounds_To_Length (Low, High, TE);
                  else
                     Error_Msg_N ("unsupported attribute: `" &
                                    Attribute_Id'Image (Attr) & "`", N);
                     V := Get_Undef (TE);
                  end if;

               elsif Is_Array_Type (P_TE) then

                  --  If what we're taking the prefix of is a type, we can't
                  --  evaluate it as an expression.

                  if Is_Entity_Name (Prefix (N))
                    and then Is_Type (Entity (Prefix (N)))
                  then
                     Array_Descr := No_GL_Value;
                  else
                     Array_Descr := Emit_LValue (Prefix (N));
                     P_TE        := Related_Type (Array_Descr);
                  end if;

                  if Attr = Attribute_Length then
                     V := Get_Array_Length (P_TE, Dim, Array_Descr);
                  else
                     V := Get_Array_Bound
                       (P_TE, Dim, Attr = Attribute_First, Array_Descr,
                        For_Orig => Is_Bit_Packed_Array_Impl_Type (P_TE));
                  end if;
               else
                  Error_Msg_N ("unsupported attribute: `" &
                                 Attribute_Id'Image (Attr) & "`", N);
                  V := Get_Undef (TE);
               end if;

               return Convert (V, TE);
            end;

         when Attribute_Position | Attribute_Bit_Position =>

            --  We don't pack, so the bit position is always a multiple
            --  of the byte size.

            if Nkind (Prefix (N)) = N_Identifier then
               V := Emit_Field_Position (Entity (Prefix (N)), No_GL_Value);
            else
               V := Emit_Field_Position (Entity (Selector_Name (Prefix (N))),
                                         Emit_LValue (Prefix (Prefix (N))));
            end if;

            if No (V) then
               V := Get_Undef (Size_Type);
            end if;
            if Attr = Attribute_Bit_Position then
               V := Mul (V, Byte_Size);
            end if;

            return Convert (V, TE);

         when Attribute_First_Bit | Attribute_Bit =>

            --  We don't support packing, so this is always zero

            return Const_Null (TE);

         when Attribute_Last_Bit =>

            --  We don't support packing, so this is always the size minus 1

            return Convert
              (Sub (Mul (Get_Type_Size (P_TE), Byte_Size),
                    Size_Const_Int (Uint_1)),
               TE);

         when Attribute_Max | Attribute_Min =>
            pragma Assert (List_Length (Expressions (N)) = 2);
            return Emit_Min_Max (Expressions (N), Attr = Attribute_Max);

         when Attribute_Pos | Attribute_Val =>
            return Emit_Conversion (First (Expressions (N)), TE, N);

         when Attribute_Succ | Attribute_Pred =>
            declare
               Exprs : constant List_Id  := Expressions (N);
               Base  : constant GL_Value := Emit_Expression (First (Exprs));
               One   : constant GL_Value := Const_Int (Base, Uint_1);

            begin
               pragma Assert (List_Length (Exprs) = 1);
               V := (if   Attr = Attribute_Succ
                     then Add (Base, One, "attr-succ")
                     else Sub (Base, One, "attr-pred"));

               --  If this is a modular type, we have to check for wrap
               --  and adjust if so.

               if Non_Binary_Modulus (TE) then
                  declare
                     C_0  : constant GL_Value := Const_Null (Base);
                     C_M1 : constant GL_Value :=
                       Const_Int (Base, Modulus (TE) - 1);

                  begin
                     if Attr = Attribute_Succ then
                        V := Build_Select (I_Cmp (Int_EQ, Base, C_M1), C_0, V);
                     else --  Attr = Attribute_Pred
                        V := Build_Select (I_Cmp (Int_EQ, Base, C_0), C_M1, V);
                     end if;
                  end;
               end if;

               return V;
            end;

         when Attribute_Machine | Attribute_Model =>

            --  ??? For now return the prefix itself. Would need to force a
            --  store in some cases.

            return Emit_Expression (First (Expressions (N)));

         when Attribute_Alignment =>
            return Const_Int (TE, Get_Type_Alignment (P_TE),
                              Sign_Extend => False);

         when Attribute_Size | Attribute_Object_Size
            | Attribute_Value_Size
            | Attribute_Max_Size_In_Storage_Elements =>

            declare
               Is_A_Type   : constant Boolean   :=
                 (Is_Entity_Name (Prefix (N))
                    and then Is_Type (Entity (Prefix (N))));
               Max_Size    : constant Boolean   :=
                 Is_A_Type and then not Is_Constrained (P_TE);

            begin
               V := (if Is_A_Type then No_GL_Value
                                  else Emit_LValue (Prefix (N)));
               V := Get_Type_Size (P_TE, V, Max_Size);
               if Attr = Attribute_Max_Size_In_Storage_Elements then
                  if Is_Unconstrained_Array (P_TE) then
                     V := Add (V, Get_Bound_Size (P_TE));
                  end if;

                  return Convert (V, TE);
               else
                  return Convert (Mul (V, Byte_Size), TE);
               end if;
            end;

         when Attribute_Component_Size =>
            return Convert
              (Mul (Get_Type_Size
                      (Full_Component_Type (P_TE), Max_Size => True),
                    Byte_Size),
               TE);

         when Attribute_Descriptor_Size =>
            pragma Assert (Is_Unconstrained_Array (P_TE));

            return Mul (Get_Bound_Size (P_TE), Byte_Size);

         when Attribute_Passed_By_Reference =>

            --  Return 1 if must pass by reference or if default to pass by ref

            return Const_Int
              (TE, (if   Get_Param_By_Ref_Mech (Default_GL_Type (P_TE))
                            = Default_By_Copy
                    then Uint_0 else Uint_1));

         when Attribute_Mechanism_Code =>
            return Const_Int
              (TE, Get_Mechanism_Code (Entity (Prefix (N)), Expressions (N)));

         when Attribute_Null_Parameter =>
            return Load (Const_Null_Ref (P_TE));

         when others =>
            Error_Msg_N ("unsupported attribute: `" &
                           Attribute_Id'Image (Attr) & "`", N);
            return Get_Undef (TE);
      end case;
   end Emit_Attribute_Reference;

   ---------------------
   -- Emit_Assignment --
   ---------------------

   procedure Emit_Assignment
     (LValue       : GL_Value;
      Expr         : Node_Id  := Empty;
      Value        : GL_Value := No_GL_Value;
      Forwards_OK  : Boolean  := True;
      Backwards_OK : Boolean  := True)
   is
      E         : constant Node_Id   := Strip_Complex_Conversions (Expr);
      Dest_Type : constant Entity_Id := Full_Designated_Type (LValue);
      Src_Type  : constant Entity_Id :=
        (if Present (Value) then Related_Type (Value) else Full_Etype (E));
      Dest      : GL_Value           := LValue;
      Src       : GL_Value           := Value;
      Dest_R    : GL_Relationship;
      Src_R     : GL_Relationship;

   begin
      --  The back-end supports exactly two types of array aggregates.
      --  One, handled in Emit_Array_Aggregate, is for a fixed-size
      --  aggregate of fixed-size components.  The other type is special
      --  cases of Others tested for in Aggr_Assignment_OK_For_Backend in
      --  Exp_Aggr.  We have to handle them here because we want to store
      --  directly into the LHS.  The front end guarantees that any Others
      --  aggregate will always be the RHS of an assignment, so we'll see
      --  it here.

      if Is_Array_Type (Dest_Type) and then Present (E)
        and then Nkind_In (E, N_Aggregate, N_Extension_Aggregate)
        and then Is_Others_Aggregate (E)
      then
         Maybe_Store_Bounds (Dest, No_GL_Value, Full_Etype (E), False);
         Emit_Others_Aggregate (Dest, E);
         return;
      end if;

      --  If we haven't yet computed our source expression, do it now.  If
      --  the evaluation used the location we specified, we're done.
      --  Otherwise, if we want a value, get it.

      if No (Src) then
         Src := Emit (E, LHS => Dest);
         if Src = Dest then
            return;
         elsif not Is_Data (Src) and then Is_Loadable_Type (Src_Type) then
            Src := Get (Src, Object);
         end if;
      end if;

      --  See what relationships we have for the source and destination to
      --  help make choices below.

      Dest_R := Relationship (Dest);
      Src_R  := Relationship (Src);

      --  If we're assigning to a type that's the nominal constrained
      --  subtype of an unconstrained array for an aliased object, see if
      --  we can get the value and bounds together and store them.  If we
      --  can, do so and we're done.  Otherwise, store the bounds.

      if Type_Needs_Bounds (Dest_Type) and then Src_R /= Bounds_And_Data then
         if Is_Data (Src) and then Is_Loadable_Type (Dest_Type) then
            Store (Get (Src, Bounds_And_Data),
                   Get (LValue, Reference_To_Bounds_And_Data));
            return;
         end if;

         Maybe_Store_Bounds (LValue, Src, Src_Type, False);
      end if;

      --  We now have three case: we're copying an object of an elementary
      --  type, we're copying an object that's not elementary, but can be
      --  copied with a Store instruction, or we're copying an object of
      --  variable size.

      if Is_Elementary_Type (Dest_Type) and then Src_R /= Bounds_And_Data then

         --  The easy case: convert the source to the destination type and
         --  store it.  However, we may have a packed array implementation
         --  type on the LHS and an array on the RHS.  Convert it to the LHS
         --  type if so.

         if Is_Packed_Array_Impl_Type (Dest_Type)
           and then not Is_Elementary_Type (Src_Type)
         then
            Src := Convert_Ref (Get (Src, Reference), Dest_Type);
         end if;

         Store (Convert (Get (Src, Data), Dest_Type), Get (Dest, Reference));

      elsif (Present (E) and then Is_Loadable_Type (Full_Etype (E)))
         or else (Present (Value) and then Is_Loadable_Type (Value))
      then
         --  Here, the source is of an LLVM value small enough to store,
         --  but the destination may or may not be a variable-sized type.
         --  Since we know the size and know the object to store, we can
         --  convert Dest to the type of the pointer to Src, which we know
         --  is fixed-size, and do the store.  If Dest is pointer to an
         --  array type, we need to get the actual array data.

         Src := Get (Src, (if Src_R = Bounds_And_Data then Src_R else Data));
         if Pointer_Type (Type_Of (Src),  0) /= Type_Of (Dest) then
            Dest := Ptr_To_Ref (Get (Dest, Reference), Full_Etype (Src));
         end if;

         Store (Src, Dest);

      else
         --  Otherwise, we have to do a variable-sized copy

         declare
            Func_Name : constant String   :=
              (if Forwards_OK and then Backwards_OK
               then "memcpy" else "memmove");
            Size      : GL_Value          := Compute_Size
              (Dest_Type, Related_Type (Src), Dest, Src);

         begin
            --  Get the proper relationship.  If we're copying both bounds
            --  and data, get the reference to that.  Otherwise, if this is
            --  a fat pointer, get the reference to the data.  Otherwise,
            --  any reference will do.

            Dest_R := (if    Src_R = Bounds_And_Data then Ref (Src_R)
                       elsif Dest_R = Fat_Pointer then Reference
                       else Any_Reference);
            Src_R  := (if    Src_R = Bounds_And_Data then Ref (Src_R)
                       elsif Src_R = Fat_Pointer then Reference
                       else Any_Reference);

            --  If we're copying both data and bounds, allow for the bounds
            --  in the size computation. If we have a fat pointer, make it
            --  a reference to the data.

            if Src_R = Reference_To_Bounds_And_Data then
               pragma Assert (Dest_R = Src_R);
               Size := Add (Size, Get_Bound_Size (Related_Type (Src)));
            end if;

            Call_With_Align_2
              (Build_Intrinsic
                 (Memcpy, "llvm." & Func_Name & ".p0i8.p0i8.i", Size_Type),
               (1 => Pointer_Cast (Get (Dest, Dest_R), Standard_A_Char),
                2 => Pointer_Cast (Get (Src,  Src_R),  Standard_A_Char),
                3 => Size,
                4 => Const_False), -- Is_Volatile
               Get_Type_Alignment (Dest_Type),
               Get_Type_Alignment (Related_Type (Src)));
         end;
      end if;
   end Emit_Assignment;

   -------------------------
   -- Emit_Code_Statement --
   ------------------------

   procedure Emit_Code_Statement (N : Node_Id) is
      Template_Strval   : constant String_Id := Strval (Asm_Template (N));
      Num_Inputs        : Nat                := 0;
      Constraint_Length : Nat                := 0;
      Output_Constraint : Node_Id            := Empty;
      Output_Val        : GL_Value           := No_GL_Value;
      Output_Variable   : Node_Id;
      Input             : Node_Id;
      Clobber           : System.Address;

   begin
      --  LLVM only allows one output, so just get the information on
      --  it, if any, and give an error if there's a second one.

      Setup_Asm_Outputs (N);
      Output_Variable := Asm_Output_Variable;

      if Present (Output_Variable) then
         Output_Constraint := Asm_Output_Constraint;
         Constraint_Length := String_Length (Strval (Output_Constraint));
         Output_Val        := Emit_LValue (Output_Variable);
         Next_Asm_Output;

         if Present (Asm_Output_Variable) then
            Error_Msg_N ("'L'L'V'M only allows one output", N);
         end if;
      end if;

      --  For inputs, just count the number of them and the total
      --  constraint length so we can allocate what we need later.

      Setup_Asm_Inputs (N);
      Input := Asm_Input_Value;

      while Present (Input) loop
         Num_Inputs        := Num_Inputs + 1;
         Constraint_Length := Constraint_Length +
           String_Length (Strval (Asm_Input_Constraint));
         Next_Asm_Input;
         Input := Asm_Input_Value;
      end loop;

      --  Likewise for clobbers, but we only need the length of the
      --  constraints here.  Node that Clobber_Get_Next isn't very friendly
      --  for an Ada caller, so we'll use fact that it's set Name_Buffer
      --  and Name_Len;

      Clobber_Setup (N);
      Clobber := Clobber_Get_Next;

      while not System."=" (Clobber, System.Null_Address) loop
         Constraint_Length := Constraint_Length + Nat (Name_Len) + 4;
         Clobber := Clobber_Get_Next;
      end loop;

      declare
         Args           : GL_Value_Array (1 .. Num_Inputs);
         Constraint_Pos : Integer          := 0;
         Input_Pos      : Nat              := 0;
         Need_Comma     : Boolean          := False;
         Constraint_Len : constant Integer :=
           Integer (Num_Inputs + Constraint_Length + 3);
         Template_Len   : constant Integer :=
           Integer (String_Length (Template_Strval));
         Constraints    : String (1 .. Constraint_Len);
         Template       : String (1 .. Template_Len);
         Asm            : GL_Value;

         procedure Add_Char (C : Character);
         procedure Add_Constraint (N : Node_Id)
           with Pre => Nkind (N) = N_String_Literal;

         --------------
         -- Add_Char --
         --------------

         procedure Add_Char (C : Character) is
         begin
            Constraint_Pos := Constraint_Pos + 1;
            Constraints (Constraint_Pos) := C;
            Need_Comma := C /= ',';
         end Add_Char;

         --------------------
         -- Add_Constraint --
         --------------------

         procedure Add_Constraint (N : Node_Id) is
         begin
            if Need_Comma then
               Add_Char (',');
            end if;

            for J in 1 .. String_Length (Strval (N)) loop
               Add_Char (Get_Character (Get_String_Char (Strval (N), J)));
            end loop;
         end Add_Constraint;

      begin
         --  Output constraints come first

         if Present (Output_Variable) then
            Add_Constraint (Output_Constraint);
         end if;

         --  Collect inputs and add their constraints

         Setup_Asm_Inputs (N);
         Input := Asm_Input_Value;
         while Present (Input) loop
            Input_Pos := Input_Pos + 1;
            Args (Input_Pos) := Get (Emit_Expression (Input), Object);
            Add_Constraint (Asm_Input_Constraint);
            Next_Asm_Input;
            Input := Asm_Input_Value;
         end loop;

         --  Add clobber constraints

         Clobber_Setup (N);
         Clobber := Clobber_Get_Next;
         while not System."=" (Clobber, System.Null_Address) loop
            if Need_Comma then
               Add_Char (',');
            end if;

            Add_Char ('~');
            Add_Char ('{');
            for J in 1 .. Name_Len loop
               Add_Char (Name_Buffer (J));
            end loop;

            Add_Char ('}');
            Clobber := Clobber_Get_Next;
         end loop;

         --  Finally, build the template

         for J in 1 .. Integer (String_Length (Template_Strval)) loop
            Template (J) :=
              Get_Character (Get_String_Char (Template_Strval, Int (J)));
         end loop;

         --  Create the inline asm

         Asm := Inline_Asm (Args, Output_Variable, Template,
                            Constraints (1 .. Constraint_Pos),
                            Is_Asm_Volatile (N), False);

         --  If we have an output, generate the call with an output and store
         --  the result.  Otherwise, just do the call.

         if Present (Output_Variable) then
            Store (Call (Asm, Full_Etype (Output_Variable), Args), Output_Val);
         else
            Call (Asm, Args);
         end if;
      end;
   end Emit_Code_Statement;

end GNATLLVM.Exprs;
