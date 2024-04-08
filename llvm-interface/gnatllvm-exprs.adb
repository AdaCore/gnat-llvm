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
with Eval_Fat;    use Eval_Fat;
with Exp_Code;    use Exp_Code;
with Exp_Util;    use Exp_Util;
with Nlists;      use Nlists;
with Opt;         use Opt;
with Sem_Aggr;    use Sem_Aggr;
with Sem_Util;    use Sem_Util;
with Snames;      use Snames;
with Stringt;     use Stringt;
with Uintp.LLVM;  use Uintp.LLVM;
with Urealp;      use Urealp;

with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Aliasing;     use GNATLLVM.Aliasing;
with GNATLLVM.Blocks;       use GNATLLVM.Blocks;
with GNATLLVM.Builtins;     use GNATLLVM.Builtins;
with GNATLLVM.Codegen;      use GNATLLVM.Codegen;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Conditionals; use GNATLLVM.Conditionals;
with GNATLLVM.Conversions;  use GNATLLVM.Conversions;
with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.GLType;       use GNATLLVM.GLType;
with GNATLLVM.Records;      use GNATLLVM.Records;
with GNATLLVM.Subprograms;  use GNATLLVM.Subprograms;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Utils;        use GNATLLVM.Utils;

with CCG; use CCG;

package body GNATLLVM.Exprs is

   procedure Emit_For_Address
     (N : N_Subexpr_Id; V : out GL_Value; Bits : out Uint);
   --  Helper for Emit_Attribute_Reference to recursively find the address
   --  of an object. Returns a GL_Value that's a reference that points into
   --  an object and a number of bits that must be added to that value.

   function Is_Safe_From_Entity (LHS : GL_Value; E : Entity_Id) return Boolean
     with Pre => Present (E);
   --  Similar to Is_Safe_From but applies to entities

   procedure Emit_Annotation (N : Node_Id)
     with Pre => Emit_C;
   --  Process node N, an annotation

   Annotate_Fn : GL_Value := No_GL_Value;
   --  Declaration for CCG builtin annotation function, if any

   ------------------
   -- Is_Safe_From --
   ------------------

   function Is_Safe_From (LHS : GL_Value; N : N_Subexpr_Id) return Boolean is
      Expr : Opt_N_Subexpr_Id;

   begin
      --  If LHS is pristine or N is static, we know this must be safe

      if Is_Pristine (LHS) or else Is_Static_Expression (N) then
         return True;
      end if;

      --  Otherwise, do Nkind-specific processing

      case Nkind (N) is
         when N_Binary_Op | N_And_Then | N_Or_Else =>
            return Is_Safe_From (LHS, Left_Opnd (N))
              and then Is_Safe_From (LHS, Right_Opnd (N));

         when N_Unary_Op | N_In =>
            return Is_Safe_From (LHS, Right_Opnd (N));

         when N_Character_Literal | N_Numeric_Or_String_Literal
            | N_Reference | N_Null | N_Raise_xxx_Error =>
            return True;

         when N_Unchecked_Type_Conversion | N_Type_Conversion
            | N_Qualified_Expression =>
            return Is_Safe_From (LHS, Expression (N));

         when N_Allocator =>
            return Nkind (Expression (N)) /= N_Qualified_Expression
              or else Is_Safe_From (LHS, Expression (N));

         when N_Entity_Name =>
            return Is_Safe_From_Entity (LHS, Entity (N));

         when N_Selected_Component =>
            return Is_Safe_From (LHS, Prefix (N));

         when N_Indexed_Component =>
            if not Is_Safe_From (LHS, Prefix (N)) then
               return False;
            end if;

            --  Not only must the prefix not conflict with RHS, but the
            --  bounds must not as well.

            Expr := First (Expressions (N));
            while Present (Expr) loop
               if not Is_Safe_From (LHS, Expr) then
                  return False;
               end if;

               Next (Expr);
            end loop;

            return True;

         when N_Slice =>
            return Is_Safe_From (LHS, Prefix (N))
              and then Is_Safe_From (LHS, Low_Bound  (Discrete_Range (N)))
              and then Is_Safe_From (LHS, High_Bound (Discrete_Range (N)));

         when N_Aggregate | N_Extension_Aggregate =>

            --  The way the components in the aggregate are presented by the
            --  front end differs between records and arrays.

            if Is_Record_Type (Full_Etype (N)) then
               declare
                  CA : Opt_N_Component_Association_Id :=
                    First (Component_Associations (N));

               begin
                  while Present (CA) loop
                     if Present (Expression (CA))
                       and then not Is_Safe_From (LHS, Expression (CA))
                     then
                        return False;
                     end if;

                     Next (CA);
                  end loop;
               end;
            else
               pragma Assert (Is_Array_Type (Full_Etype (N)));
               Expr := First (Expressions (N));
               while Present (Expr) loop
                  if not Is_Safe_From (LHS, Expr) then
                     return False;
                  end if;

                  Next (Expr);
               end loop;
            end if;

            return True;

         when N_If_Expression =>
            Expr := First (Expressions (N));
            return Is_Safe_From (LHS, Expr)
              and then Is_Safe_From (LHS, Next (Expr))
              and then Is_Safe_From (LHS, Next (Next (Expr)));

         when N_Attribute_Reference =>

            --  Most are safe, so we just concern ourselves with the ones
            --  that aren't.

            Expr := First (Expressions (N));
            case Get_Attribute_Id (Attribute_Name (N)) is
               when Attribute_Deref =>
                  return False;

               when Attribute_Min | Attribute_Max =>
                  return Is_Safe_From (LHS, Expr)
                    and then Is_Safe_From (LHS, Next (Expr));

               when Attribute_Pos | Attribute_Val | Attribute_Succ
                  | Attribute_Pred | Attribute_Model =>
                  return Is_Safe_From (LHS, Expr);

               when  others =>
                  return True;
            end case;

         when others =>
            return False;
      end case;
   end Is_Safe_From;

   -------------------------
   -- Is_Safe_From_Entity --
   -------------------------

   function Is_Safe_From_Entity
     (LHS : GL_Value; E : Entity_Id) return Boolean
   is
      V : constant GL_Value := Get_Value (E);

   begin
      --  If we have a value for V and it's either data or V and LHS
      --  represent two different variables, it's safe.

      if not Present (V) or else LHS = V then
         return False;
      elsif Is_Data (V) then
         return True;
      else
         return (Is_A_Alloca_Inst (LHS)
                   or else Is_A_Global_Variable (LHS))
           and then (Is_A_Alloca_Inst (V)
                       or else Is_A_Global_Variable (V));
      end if;
   end Is_Safe_From_Entity;

   --------------------------------------
   -- LHS_And_Component_For_Assignment --
   --------------------------------------

   procedure LHS_And_Component_For_Assignment
     (N             : N_Subexpr_Id;
      LHS           : out GL_Value;
      F             : out Opt_Record_Field_Kind_Id;
      Idxs          : out Access_GL_Value_Array;
      For_LHS       : Boolean := False;
      Only_Bitfield : Boolean := False) is
   begin
      --  Start by assuming there's no special processing, then
      --  see if there is. If we're just elaborating decls, there isn't

      F    := Empty;
      Idxs := null;
      LHS  := No_GL_Value;

      if Decls_Only then
         null;

      elsif Nkind (N) = N_Selected_Component then
         declare
            Fld  : constant Record_Field_Kind_Id := Entity (Selector_Name (N));

         begin
            --  If we want field processing for all fields or if this is a
            --  bitfield, set the field and LHS.

            if not Only_Bitfield or else Is_Bitfield_By_Rep (Fld) then
               LHS := Emit_LValue (Prefix (N), For_LHS => For_LHS);
               F   := Field_To_Use (LHS, Fld);
            end if;
         end;

      elsif Nkind (N) = N_Indexed_Component and then not Only_Bitfield then
         LHS  := Emit_LValue (Prefix (N), For_LHS => For_LHS);
         Idxs := new GL_Value_Array'(Get_Indices (Expressions (N), LHS));
      end if;

      if No (LHS) then
         LHS := Emit_LValue (N, For_LHS => For_LHS);
      end if;

   end LHS_And_Component_For_Assignment;

   ----------------
   -- Emit_Undef --
   ----------------

   function Emit_Undef (GT : GL_Type) return GL_Value is
     ((if Is_Loadable_Type (GT) then Get_Undef (GT) else Get_Undef_Ref (GT)));

   ------------------
   -- Emit_Literal --
   ------------------

   function Emit_Literal (N : N_Subexpr_Id) return GL_Value is
      GT      : constant GL_Type := Full_GL_Type (N);
      Prim_GT : constant GL_Type := Primitive_GL_Type (GT);
      V       : GL_Value;

   begin
      case Nkind (N) is
         when N_Character_Literal =>

            --  If a Entity is present, it means that this was one of the
            --  literals in a user-defined character type.

            V := Const_Int (Prim_GT, (if Present (Entity (N))
                                      then Enumeration_Rep (Entity (N))
                                      else Char_Literal_Value (N)));

         when N_Integer_Literal =>

            --  On architectures with tagged pointers, we need to represent
            --  addresses as pointers to preserve tags; consequently,
            --  address literals also need to be pointers. The easiest way
            --  to get one from an integer is to derive it from the null
            --  pointer.

            if Tagged_Pointers and then Is_Address (GT) then
               V :=
                 Null_Derived_Ptr
                   (Const_Int (Size_GL_Type, Intval (N)), GT);
            else
               V := Const_Int (Prim_GT, Intval (N));
            end if;

         when N_Real_Literal =>
            if Is_Fixed_Point_Type (GT) then
               V := Const_Int (Prim_GT, Corresponding_Integer_Value (N));
            else
               declare
                  Val : Ureal := Realval (N);

               begin
                  --  Handle zero separately to save time, but be sure we
                  --  get the proper sign for zero.

                  if UR_Is_Zero (Val) then
                     V := Const_Real (Prim_GT, 0.0);
                     V := (if UR_Is_Negative (Val) then F_Neg (V) else V);

                  else
                     --  First convert the value to a machine number
                     --  if it isn't already. That will force the base
                     --  to 2 for non-zero values and simplify the
                     --  rest of the logic.

                     if not Is_Machine_Number (N) then
                        Val := Machine (Full_Base_Type (GT), Val, Round_Even,
                                        N);
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
                          (Prim_GT, -(+Denominator (Val)), Words);
                        V := (if UR_Is_Negative (Val) then F_Neg (V) else V);
                     end;
                  end if;
               end;
            end if;

         when N_String_Literal => String_Literal : declare

            Str_Id  : constant String_Id := Strval (N);
            Length  : constant Nat       := String_Length (Str_Id);
            Elmt_GT : constant GL_Type   := Full_Component_GL_Type (GT);

         begin
            --  If this is a normal string, where the size of a character
            --  is a byte, use Const_String to create the string. We can't
            --  use Name_Buffer here since it might overflow.

            if ULL'(Get_Type_Size (Type_Of (Elmt_GT))) = 8 then
               declare
                  Str : String_Access := new String (1 .. Integer (Length));

               begin
                  for J in Str'Range loop
                     Str (J) :=
                       Get_Character (Get_String_Char (Str_Id, Nat (J)));
                  end loop;

                  V := Const_String (Str.all, Prim_GT);
                  Free (Str);
               end;

            else
               --  Otherwise, we have to create an array for each character
               --  this might be large, so don't try to allocate it in the
               --  stack.

               declare
                  Elements : Access_GL_Value_Array :=
                    new GL_Value_Array (1 .. Length);

               begin
                  for J in Elements'Range loop
                     Elements (J) :=
                       Const_Int (Elmt_GT,
                                  ULL (Get_String_Char (Str_Id, Nat (J))));
                  end loop;

                  V := Const_Array (Elements.all, Prim_GT);
                  Free (Elements);
               end;
            end if;
         end String_Literal;

         when others =>
            pragma Assert (Decls_Only);
            V := Emit_Undef (Prim_GT);
      end case;

      return From_Primitive (V, Prim_GT);
   end Emit_Literal;

   ---------------------------
   -- Emit_Binary_Operation --
   ---------------------------

   function Emit_Binary_Operation (N : N_Binary_Op_Id) return GL_Value is
      type Opf is access function
        (LHS, RHS : GL_Value; Name : String := "") return GL_Value;

      LHS_Node   : constant N_Subexpr_Id := Left_Opnd (N);
      RHS_Node   : constant N_Subexpr_Id := Right_Opnd (N);
      LHS_GT     : constant GL_Type      := Full_GL_Type (LHS_Node);
      RHS_GT     : constant GL_Type      := Full_GL_Type (RHS_Node);
      LHS_BT     : constant GL_Type      := Base_GL_Type (LHS_GT);
      RHS_BT     : constant GL_Type      := Base_GL_Type (RHS_GT);
      LVal       :          GL_Value     :=
        Emit_Convert_Value (LHS_Node, LHS_BT);
      RVal       :          GL_Value     :=
        Emit_Convert_Value (RHS_Node, RHS_BT);
      Ptr        :          GL_Value     := No_GL_Value;
      FP         : constant Boolean      := Is_Floating_Point_Type (LHS_BT);
      Ovfl_Check : constant Boolean      := Do_Overflow_Check (N)
        and then not (Is_A_Constant_Int (LVal)
                      and then Is_A_Constant_Int (RVal));
      --  If both are constant, we don't need to do an explicit overflow
      --  check since we always check the results of constant operations
      --  for overflow.
      --  ??? Testing Emit_C shouldn't be needed but the frontend apparently
      --  doesn't clear the Do_Overflow_Check flag when handling overflow
      --  checking in the frontend.

      Unsign     : constant Boolean  := Is_Unsigned_Type (LHS_BT);
      Subp       : Opf               := null;
      Result     : GL_Value;
      Ovfl_Name  : String (1 .. 4);

   begin
      --  If we're just doing compiling to back-annotate things, don't try
      --  to compute this operation.

      if Decls_Only then
         return Emit_Undef (Full_GL_Type (N));
      end if;

      --  If we're doing arithmetic on tagged pointers, extract their
      --  addresses, perform the computation, and then reassemble the
      --  result pointer.

      if Tagged_Pointers then
         if Is_Address (RVal) then
            Ptr  := RVal;
            RVal := Get_Pointer_Address (RVal);
         end if;

         if Is_Address (LVal) then
            Ptr  := LVal;
            LVal := Get_Pointer_Address (LVal);
         end if;
      end if;

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
            Func      : constant GL_Value       :=
              Build_Intrinsic
               ("llvm." & Ovfl_Name & ".with.overflow", LHS_BT,
                (1 => Type_Of (LHS_BT)));
            Fn_Ret    : constant GL_Value       :=
              Call_Relationship (Func, (1 => LVal, 2 => RVal),
                                 Boolean_And_Data);
            Overflow  : constant GL_Value       := Get (Fn_Ret, Boolean_Data);
            Label_Ent : constant Opt_E_Label_Id :=
              Get_Exception_Goto_Entry (N_Raise_Constraint_Error);
            BB_Next   : Basic_Block_T;

         begin
            if Present (Label_Ent) then
               BB_Next := Create_Basic_Block;
               Build_Cond_Br (Overflow, Get_Label_BB (Label_Ent), BB_Next);
               Position_Builder_At_End (BB_Next);
            else
               Emit_Raise_Call_If (Overflow, N);
            end if;

            Result := Get (Fn_Ret, Data);
         end;
      end if;

      --  If this is a signed mod operation, we have to adjust the result,
      --  since what we did is a rem operation. If the result is zero or
      --  the result and the RHS have the same sign, the result is correct.
      --  Otherwise, we have to add the RHS to the result. Two values have
      --  the same sign iff their xor is non-negative, which is the best
      --  code for the general case, but having a variable as the second
      --  operand of mod is quite rare, so it's best to do slightly less
      --  efficient code for the general case that will get constant-folded
      --  in the constant case.

      if not Unsign and Nkind (N) = N_Op_Mod then
         declare
            Add_Back      : constant GL_Value := Result + RVal;
            RHS_Neg       : constant GL_Value :=
              I_Cmp (Int_SLT, RVal, Const_Null (RVal), "RHS.neg");
            Result_Nonpos : constant GL_Value :=
              I_Cmp (Int_SLE, Result, Const_Null (Result), "result.nonpos");
            Result_Nonneg : constant GL_Value :=
              I_Cmp (Int_SGE, Result, Const_Null (Result), "result.nonneg");
            Signs_Same    : constant GL_Value :=
              Build_Select (RHS_Neg, Result_Nonpos, Result_Nonneg,
                            "sign.same");

         begin
            Result := Build_Select (Signs_Same, Result, Add_Back);
         end;

      --  If this is an integer division operation with Round_Result set,
      --  we have to do that rounding. There are two different cases, one
      --  for signed and one for unsigned.

      elsif Nkind (N) = N_Op_Divide and then Rounded_Result (N)
        and then Unsign and then not FP
      then
         declare

            --  We compute the remainder. If the remainder is greater then
            --  half of the RHS (e.g., > (RHS + 1) / 2), we add one to the
            --  result.

            One         : constant GL_Value := Const_Int (RVal, Uint_1);
            Remainder   : constant GL_Value := U_Rem (LVal, RVal);
            Half_RHS    : constant GL_Value := L_Shr (RVal - One, One);
            Plus_One    : constant GL_Value := Result + One;
            Need_Adjust : constant GL_Value :=
              I_Cmp (Int_UGT, Remainder, Half_RHS);

         begin
            Result := Build_Select (Need_Adjust, Plus_One, Result);
         end;

      elsif Nkind (N) = N_Op_Divide and then Rounded_Result (N)
        and then not Unsign and then not FP
      then
         declare

            --  We compute the quotient. Then it gets more complicated.
            --  As in the mod case, we optimize for the case when RHS is a
            --  constant. If twice the absolute value of the remainder is
            --  greater than RHS, we have to either add or subtract one
            --  from the result, depending on whether the remainder is the
            --  same sign as the RHS not. Again, we optimize for the case
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
              Build_Select (RHS_Neg, Rem_Neg, Rem_Nonneg, "sign.same");
            Plus_One     : constant GL_Value  := Result + One;
            Minus_One    : constant GL_Value  := Result - One;
            Which_Adjust : constant GL_Value  :=
              Build_Select (Signs_Same, Plus_One, Minus_One);

         begin
            Result := Build_Select (Need_Adjust, Which_Adjust, Result);
         end;
      end if;

      --  If this was an operation on tagged pointers, assemble the result
      --  pointer using tags from (one of) the arguments.

      if Present (Ptr) then
         Result := Set_Pointer_Address (Ptr, Result);
      end if;

      return Result;

   end Emit_Binary_Operation;

   --------------------------
   -- Emit_Unary_Operation --
   --------------------------

   function Emit_Unary_Operation (N : N_Unary_Op_Id) return GL_Value is
      Op     : constant GL_Value := To_Primitive (Emit (Right_Opnd (N)));
      GT     : constant GL_Type  := Related_Type (Op);
      BT     : constant GL_Type  := Base_GL_Type (GT);
      Result : GL_Value;

   begin
      --  If we're just doing compiling to back-annotate things, don't try
      --  to compute this operation.

      if Decls_Only then
         return Emit_Undef (Full_GL_Type (N));
      end if;

      case Nkind (N) is

         when N_Op_Not =>

            --  If this is Boolean_Data, we can compute the result that way.
            --  Otherwise, force to Data.

            if Relationship (Op) = Boolean_Data then
               return Build_Xor (Op, Const_True);
            else
               Result := Get (Op, Data);
               return (if   Is_Boolean_Type (Result)
                       then Build_Xor (Result, Const_Int (Result, ULL (1)))
                       else Build_Not (Result));
            end if;

         when N_Op_Abs =>

            Result := Get (Op, Data);

            --  Emit: X >= 0 ? X : -X;

            declare
               Zero      : constant GL_Value := Const_Null (Result);
               Compare   : constant GL_Value :=
                 Build_Elementary_Comparison (N_Op_Ge, Result, Zero);
               Neg_Expr  : constant GL_Value :=
                 (if   Is_Floating_Point_Type (Result) then F_Neg (Result)
                  else Neg (Result));

            begin
               if Is_Unsigned_Type (Result) then
                  return Result;
               else
                  return Build_Select (Compare, Result, Neg_Expr, "ABS");
               end if;
            end;

         when N_Op_Plus =>
            return Get (Op, Data);

         when N_Op_Minus => Minus : declare

            V : constant GL_Value  := Convert (Get (Op, Data), BT);

         begin
            if Is_Floating_Point_Type (BT) then
               return F_Neg (V);

            --  ??? Testing Emit_C shouldn't be needed but the frontend
            --  apparently doesn't clear the Do_Overflow_Check flag when
            --  handling overflow checking in the frontend.

            elsif Do_Overflow_Check (N) and then not Emit_C
              and then not Is_Unsigned_Type (BT)
            then
               declare
                  Func      : constant GL_Value      :=
                    Build_Intrinsic ("llvm.ssub.with.overflow", BT,
                                     (1 => Type_Of (BT)));
                  Fn_Ret    : constant GL_Value       :=
                    Call_Relationship (Func, (1 => Const_Null (BT), 2 => V),
                                       Boolean_And_Data);
                  Overflow  : constant GL_Value       :=
                    Get (Fn_Ret, Boolean_Data);
                  Label_Ent : constant Opt_E_Label_Id :=
                    Get_Exception_Goto_Entry (N_Raise_Constraint_Error);
                  BB_Next   : Basic_Block_T;

               begin
                  if Present (Label_Ent) then
                     BB_Next := Create_Basic_Block;
                     Build_Cond_Br (Overflow, Get_Label_BB (Label_Ent),
                                    BB_Next);
                     Position_Builder_At_End (BB_Next);
                  else
                     Emit_Raise_Call_If (Overflow, N);
                  end if;

                  return Get (Fn_Ret, Data);
               end;
            else
               return Neg (V);
            end if;
         end Minus;

         when others =>
            pragma Assert (Standard.False);
            return Emit_Undef (GT);
      end case;

   end Emit_Unary_Operation;

   -------------------------
   -- Emit_Overflow_Check --
   -------------------------

   procedure Emit_Overflow_Check (V : GL_Value; N : N_Type_Conversion_Id) is
      In_GT      : constant GL_Type        := Related_Type (V);
      Out_GT     : constant GL_Type        := Full_GL_Type (N);
      In_BT      : constant GL_Type        := Base_GL_Type (In_GT);
      Out_BT     : constant GL_Type        := Base_GL_Type (Out_GT);
      In_FP      : constant Boolean        := Is_Floating_Point_Type (In_GT);
      Out_FP     : constant Boolean        := Is_Floating_Point_Type (Out_GT);
      In_LB      : constant N_Subexpr_Id   := Type_Low_Bound  (In_BT);
      In_HB      : constant N_Subexpr_Id   := Type_High_Bound (In_BT);
      Out_LB     : constant N_Subexpr_Id   := Type_Low_Bound  (Out_BT);
      Out_HB     : constant N_Subexpr_Id   := Type_High_Bound (Out_BT);
      Label_Ent  : constant Opt_E_Label_Id :=
        Get_Exception_Goto_Entry (N_Raise_Constraint_Error);
      Compare_LB : GL_Value                := No_GL_Value;
      Compare_HB : GL_Value                := No_GL_Value;
      BB_Raise   : Basic_Block_T;
      BB_Next    : Basic_Block_T;

   begin
      --  If both types are integers, we determine the need for each check
      --  individually by seeing if the output bounds are tighter than the
      --  input bounds. But this isn't worth doing for FP since the
      --  chances of having a difference is very low. Since an FP NaN or
      --  Inf always compares false, do the comparison so false is a
      --  failure.

      if In_FP or else Out_FP
        or else Get_Uint_Value (Out_LB) > Get_Uint_Value (In_LB)
      then
         Compare_LB := Build_Elementary_Comparison
           (N_Op_Ge, V, Emit_Convert_Value (Out_LB, In_GT));
      end if;

      if In_FP or else Out_FP
        or else Get_Uint_Value (Out_HB) < Get_Uint_Value (In_HB)
      then
         Compare_HB := Build_Elementary_Comparison
           (N_Op_Le, V, Emit_Convert_Value (Out_HB, In_GT));
      end if;

      --  If neither comparison is needed, we're done

      if No (Compare_LB) and then No (Compare_HB) then
         return;

      --  Test for a known result. If either comparison is always false,
      --  we know this test will fail. So generate the raise.

      elsif Compare_LB = Const_False or else Compare_HB = Const_False then
         if Present (Label_Ent) then
            Build_Br (Get_Label_BB (Label_Ent));
         else
            Emit_Raise_Call (N, CE_Overflow_Check_Failed);
         end if;

      --  If all the Present tests are True, there's no overflow and we're
      --  done.

      elsif Compare_LB in No_GL_Value | Const_True
        and then Compare_HB in No_GL_Value | Const_True
      then
         return;
      end if;

      --  Otherwise, make the labels and branch to them depending on the
      --  results of the tests. If we're doing both comparisons, we'll have
      --  put both before the first test, but the optimizer can clean that up.

      BB_Raise :=
        (if   Present (Label_Ent) then Get_Label_BB (Label_Ent)
         else Create_Basic_Block);
      BB_Next := Create_Basic_Block;

      if Present (Compare_HB) then
         Build_Cond_Br (Compare_HB, BB_Next, BB_Raise);

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
      LHS_Node, RHS_Node : N_Subexpr_Id) return GL_Value
   is
      To_Left, Rotate, Arithmetic : Boolean := False;

      LHS       : constant GL_Value := Emit_Expression (LHS_Node);
      RHS       : constant GL_Value := Emit_Expression (RHS_Node);
      N         : constant GL_Value := Convert (RHS, LHS);
      LHS_Size  : constant GL_Value := Get_Type_Size (LHS);
      LHS_Bits  : constant GL_Value := Convert (LHS_Size, LHS);
      Result    : GL_Value          := LHS;
      Saturated : GL_Value;

   begin
      --  If we're just doing compiling to back-annotate things, don't try
      --  to compute this operation.

      if Decls_Only then
         return Emit_Undef (Primitive_GL_Type (N));
      end if;

      --  Extract properties for the operation we are asked to generate code
      --  for. We defaulted to a right shift above.

      case Operation is
         when N_Op_Shift_Left =>
            To_Left := True;

         when N_Op_Shift_Right_Arithmetic =>
            Arithmetic := True;

         when N_Op_Rotate_Left =>
            To_Left := True;
            Rotate  := True;

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
            Lower_Shift : constant GL_Value := LHS_Bits - Reduced_N;
            Reduced_Low : constant GL_Value := U_Rem (Lower_Shift, LHS_Bits);
            Upper       : constant GL_Value :=
              (if   To_Left then Shl   (LHS, Reduced_N,   "rotate.upper")
               else L_Shr (LHS, Reduced_N,   "rotate.upper"));
            Lower       : constant GL_Value :=
              (if   To_Left then L_Shr (LHS, Reduced_Low, "rotate.lower")
               else Shl   (LHS, Reduced_Low, "rotate.lower"));

         begin
            return Build_Or (Upper, Lower, "rotate.result");
         end;

      else
         --  First, compute the value using the underlying LLVM instruction

         Result :=
           (if   To_Left then  Shl (LHS, N) elsif Arithmetic
            then A_Shr (LHS, N) else L_Shr (LHS, N));

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
                 (Int_SLT, LHS, Const_Null (LHS), "is.lhs.negative"),
               C_Then => Const_Ones (LHS),
               C_Else => Const_Null (LHS),
               Name   => "SATURATED")

            else Const_Null (LHS));

         --  Now, we must decide at runtime if it is safe to rely on the
         --  underlying LLVM instruction. If so, use it, otherwise return
         --  the saturated value.

         return Build_Select
           (C_If   => I_Cmp (Int_UGE, N, LHS_Bits, "is.saturated"),
            C_Then => Saturated,
            C_Else => Result,
            Name   => "sr.result");
      end if;
   end Emit_Shift;

   ----------------------
   -- Emit_For_Address --
   ----------------------

   procedure Emit_For_Address
     (N : N_Subexpr_Id; V : out GL_Value; Bits : out Uint)
   is
      GT : constant GL_Type := Full_GL_Type (N);

   begin
      case Nkind (N) is
         when N_Indexed_Component =>
            Emit_For_Address (Prefix (N), V, Bits);
            V := Get_Indexed_LValue (Get_Indices (Expressions (N), V), V);

         when N_Slice =>
            Emit_For_Address (Prefix (N), V, Bits);
            V := Get_Slice_LValue (GT, V);

         when N_Selected_Component => Selected_Component : declare

            In_F  : constant Record_Field_Kind_Id :=
              Entity (Selector_Name (N));
            R_TE  : constant Record_Kind_Id       := Full_Scope (In_F);
            Rec_T : constant Type_T               := Type_Of (R_TE);
            F     : constant Record_Field_Kind_Id :=
              Find_Matching_Field (R_TE, In_F);
            pragma Unreferenced (Rec_T);

         begin
            --  Compute an LValue that points either to the field or the
            --  bitfield field that contains the field.

            Emit_For_Address (Prefix (N), V, Bits);
            V := Record_Field_Offset (V, F);

            --  If it's a bitfield, record the offset in bits and force the
            --  type to be the desired type so outer component references
            --  will work properly.

            if Is_Bitfield (F) then
               pragma Assert (Relationship (V) = Reference_To_Unknown);
               V := Convert_Ref (V, GT);
               Bits := Bits + Field_Bit_Offset (F);
            end if;
         end Selected_Component;

         when others =>
            --  Just get this as an LValue and initialize the bit offset.

            V    := Emit_LValue (N);
            Bits := Uint_0;
      end case;
   end Emit_For_Address;

   -------------------------------
   -- Emit_Assignment_Statement --
   -------------------------------

   procedure Emit_Assignment_Statement (N : N_Assignment_Statement_Id) is
      LHS  : GL_Value;
      Idxs : Access_GL_Value_Array;
      F    : Opt_Record_Field_Kind_Id;

   begin
      --  Get the LHS to evaluate and see if we need to do a field or
      --  array operation.

      LHS_And_Component_For_Assignment (Name (N), LHS, F, Idxs,
                                        For_LHS => True);

      --  If this is a reference, set atomic or volatile as neeed

      if Present (F) or else Idxs /= null then
         Mark_Volatile (LHS,
                        Atomic_Sync_Required (Name (N))
                        or else Is_Volatile_Reference (Name (N)));
         Mark_Atomic   (LHS, Atomic_Sync_Required (Name (N)));
      end if;

      --  Now do the operation

      if Present (F) then

         --  We have a special case when LHS is a reference, F is not a
         --  bitfield, and we have no VFA. In the case, we may be able to
         --  do this assignment in place, so pass the LHS when we evaluate N.

         if Is_Reference (LHS) and then not Is_Bitfield (F)
           and then not Is_VFA_Ref (Name (N))
         then
            declare
               Our_LHS : constant GL_Value := Record_Field_Offset (LHS, F);

            begin
               Emit_Assignment (Our_LHS, Expression (N));
            end;
         else
            Build_Field_Store (LHS, F,
                               Emit_Expression (Expression (N)),
                               VFA => Is_VFA_Ref (Name (N)));
         end if;

      elsif Idxs /= null then
         Build_Indexed_Store (LHS, Idxs.all,
                              Emit_Expression (Expression (N)),
                              VFA => Is_VFA_Ref (Name (N)));
         Free (Idxs);
      else
         Emit_Assignment (LHS,
                          Expr         => Expression (N),
                          Forwards_OK  => Forwards_OK (N),
                          Backwards_OK => Backwards_OK (N),
                          VFA          => Has_Full_Access (Name (N)));
      end if;

      --  Deal with any writebacks needed if we had a bitfield in an
      --  LHS context above.

      Perform_Writebacks;
   end Emit_Assignment_Statement;

   -----------------
   -- Emit_Pragma --
   -----------------

   procedure Emit_Pragma (N : N_Pragma_Id) is
      PAAs : constant List_Id := Pragma_Argument_Associations (N);

   begin
      case Get_Pragma_Id (N) is

         when Pragma_Reviewable =>
            if not Emit_Full_Debug_Info then
               Error_Msg_N ("??must specify -g", N);
            end if;

         when Pragma_Optimize =>

            case Chars (Expression (First (PAAs))) is

               when Name_Off =>
                  if Code_Opt_Level /= 0 then
                     Error_Msg_N ("??must specify -O0", N);
                  end if;

               when Name_Space =>
                  if Size_Opt_Level = 0 then
                     Error_Msg_N ("??must specify -Os or -Oz", N);
                  end if;

               when Name_Time =>
                  if Code_Opt_Level = 0 then
                     Error_Msg_N ("??insufficient -O value", N);
                  end if;

               when others =>
                  pragma Assert (Standard.False);
            end case;

         --  For Compile_Time_Error or warning, be sure that any types of
         --  expressions in the first argument are elaborated.

         when Pragma_Compile_Time_Error | Pragma_Compile_Time_Warning =>
            declare
               function Elaborate_Type (N : Node_Id) return Traverse_Result;
               --  If N has a type, ensure that we've elaborated it

               procedure Scan is new Traverse_Proc (Elaborate_Type);
               --  Search an expression for unelaborated types

               --------------------
               -- Elaborate_Type --
               --------------------

               function Elaborate_Type (N : Node_Id) return Traverse_Result is
               begin
                  if Nkind (N) in N_Has_Etype and then Present (Etype (N)) then
                     Discard (Type_Of (Full_Etype (N)));
                  end if;

                  return OK;
               end Elaborate_Type;

            begin
               Scan (Expression (First (PAAs)));
            end;

         when Pragma_Inspection_Point
           | Pragma_Loop_Optimize
           | Pragma_Warning_As_Error
           | Pragma_Warnings =>
            --  ??? These are the ones that Gigi supports and we may want
            --  to support as well at some point.

            null;

         --  For Annotate, only do something if we're emitting C and we
         --  have three operands.

         when Pragma_Annotate | Pragma_GNAT_Annotate =>

            if Emit_C and then List_Length (PAAs) = 3 then
               Emit_Annotation (N);
            end if;

         --  For pragma Comment, set up an annotation for the comment
         --  if we're generating C and aren't outputting source code.

         when Pragma_Comment =>
            if Emit_C and then not Dump_Source_Text
              and then Is_Non_Empty_List (PAAs)
              and then Nkind (Expression (First (PAAs))) = N_String_Literal
            then
               Emit_Annotation (N);
            end if;

         when others => null;
      end case;
   end Emit_Pragma;

   ---------------------
   -- Emit_Annotation --
   ---------------------

   procedure Emit_Annotation (N : Node_Id) is
      Ann_Idx : Nat;

   begin
      if Library_Level then
         C_Add_To_Source_Order (N);
         return;
      end if;

      --  Create the annotation and do nothing if there isn't anything
      --  to annotate here.

      Ann_Idx := C_Create_Annotation (N);
      if Ann_Idx = 0 then
         return;

      --  If the annotation builtin isn't defined yet, define it.
      --  This piece in some sense is part of CCG, but is put here because
      --  we have infrastructure here to write calls using GLValues that's
      --  more straightforward than what we have in CCG.

      elsif No (Annotate_Fn) then
         Annotate_Fn :=
           Add_Global_Function ("llvm.ccg.annotate",
                                Fn_Ty ((1 => Type_Of (Integer_GL_Type)),
                                       Void_Type),
                                Void_GL_Type);
      end if;

      --  Now call it

      Call (Annotate_Fn,
            (1 => Const_Int (Integer_GL_Type, ULL (Ann_Idx), True)));

   end Emit_Annotation;

   ------------------------------
   -- Emit_Attribute_Reference --
   ------------------------------

   function Emit_Attribute_Reference
     (N : N_Attribute_Reference_Id) return GL_Value
   is
      Attr   : constant Attribute_Id := Get_Attribute_Id (Attribute_Name (N));
      Pref   : constant N_Subexpr_Id := Prefix (N);
      GT     : constant GL_Type      := Full_GL_Type (N);
      V      : GL_Value              := No_GL_Value;
      P_GT   : GL_Type               := Full_GL_Type (Pref);
      Bits   : Uint;
      Ret_UI : Uint;
      Result : GL_Value;

   begin
      --  First see if this is something we can compute from annotations
      --  in the tree.

      Ret_UI := Get_Attribute_From_Annotation (N);

      if Present (Ret_UI) then
         return Const_Int (GT, Ret_UI);
      end if;

      case Attr is
         when Attribute_Access | Attribute_Unchecked_Access
            | Attribute_Unrestricted_Access =>

            --  We store values as pointers, so getting an access to an
            --  expression is the same as getting an LValue and has the
            --  same constraints. But we do have to be sure that it's of
            --  the right type.

            if Decls_Only and then Is_Access_Protected_Subprogram_Type (GT)
            then
               return Get_Undef (GT);
            else
               Result :=  Convert_To_Access (Emit_LValue (Pref), GT);

               --  If we're taking the address of a subprogram, flag that
               --  we are (and that we may need a nest parameter added if
               --  generating C). Do this after the above call so we know
               --  that if Pref has an entity, it will have been evaluated.

               if Nkind (Pref) in N_Has_Entity
                 and then Present (Entity (Pref))
                 and then Ekind (Entity (Pref)) in Subprogram_Kind
                 and then not Has_Foreign_Convention (Entity (Pref))
                 and then Present (Get_Value (Entity (Pref)))
               then
                  C_Address_Taken (+Get_Value (Entity (Pref)));
               end if;

               return Result;
            end if;

         when Attribute_Address | Attribute_Code_Address =>

            --  If just elaborating decls, don't try to go inside this

            if Decls_Only then
               return Get_Undef (GT);
            end if;

            --  Otherwise, get an LValue and byte offset for this expression

            Emit_For_Address (Pref, V, Bits);

            --  We need a single-word pointer, then convert it to the
            --  desired integral type.

            V := Ptr_To_Int (Get (V, Reference_For_Integer), GT,
                             "attr.address");

            --  Now add in any bit offset. If the offset is zero don't try
            --  to add it because the code that we emit in this case is
            --  unsuitable for constant initializers.

            return
              (if   Bits = 0
               then V
               else Address_Add (V, Const_Int (Size_GL_Type, Bits / BPU)));

         when Attribute_Pool_Address =>

            --  Evaluate this object. We normally want to look at the
            --  address of the object itself (e.g., as a Reference), but if
            --  it's an access type, what we want is the value. So convert
            --  it to a reference.

            V := Emit (Pref, Prefer_LHS => True);

            if Is_Access_Type (P_GT) then
               V := From_Access (Get (V, Data));
            end if;

            --  If it's an unconstrained array, we want the location of the
            --  bounds, which is the first thing allocated.

            V := Get (V, (if   Is_Unconstrained_Array (V)
                          then Reference_To_Bounds_And_Data else Reference));
            return Ptr_To_Int (V, GT, "pool.address");

         when Attribute_Deref => Deref : declare

            Expr : constant N_Subexpr_Id := First (Expressions (N));

         begin
            pragma Assert (Is_Descendant_Of_Address (Full_Etype (Expr)));
            return Int_To_Ref (Emit_Expression (Expr), GT, "attr.deref");
         end Deref;

         when Attribute_First  | Attribute_Last | Attribute_Length
            | Attribute_Range_Length
         =>
         Range_Attr : declare

            BT          : constant GL_Type := Base_GL_Type (P_GT);
            Dim         : constant Nat     :=
              (if   Present (Expressions (N))
              then +Intval (First (Expressions (N))) - 1 else 0);
            Array_Descr : GL_Value;
            Low, High   : GL_Value;

         begin
            if Is_Scalar_Type (P_GT) then
               Low  := Emit_Convert_Value (Type_Low_Bound  (P_GT), BT);
               High := Emit_Convert_Value (Type_High_Bound (P_GT), BT);

               if Attr = Attribute_First then
                  V := Low;
               elsif Attr = Attribute_Last then
                  V := High;
               elsif Attr = Attribute_Range_Length then
                     V := Bounds_To_Length (Low, High, GT);
               else
                  pragma Assert (Decls_Only);
                  V := Emit_Undef (GT);
               end if;

            elsif Is_Array_Type (P_GT) then

               --  If what we're taking the prefix of is a type, we can't
               --  evaluate it as an expression.

               if Is_Entity_Name (Pref) and then Is_Type (Entity (Pref)) then
                  Array_Descr := No_GL_Value;
               else
                  Array_Descr := Emit_LValue (Pref);
                  P_GT        := Related_Type (Array_Descr);
               end if;

               if Attr = Attribute_Length then
                  V := Get_Array_Length (Full_Etype (P_GT), Dim, Array_Descr);
               else
                  V := Get_Array_Bound
                    (P_GT, Dim, Attr = Attribute_First, Array_Descr,
                     For_Orig => Is_Bit_Packed_Array_Impl_Type (P_GT));
               end if;
            else
               pragma Assert (Decls_Only);
               V := Emit_Undef (GT);
            end if;

            return Convert (V, GT);
         end Range_Attr;

         when Attribute_Position | Attribute_Bit_Position
            | Attribute_First_Bit | Attribute_Bit | Attribute_Last_Bit
         =>
         Position_Attr : declare

            F            : Record_Field_Kind_Id;
            Val          : GL_Value;
            Position     : GL_Value;

         begin
            --  Get the relevant field and expression for this operation,
            --  if any.

            if Nkind (Pref) = N_Identifier
              and then Ekind (Entity (Pref)) in E_Discriminant | E_Component
            then
               F   := Entity (Pref);
               Val := No_GL_Value;
            elsif Nkind (Pref) = N_Selected_Component then
               F   := Entity (Selector_Name (Pref));
               Val := Emit_LValue (Prefix (Pref));
            else
               pragma Assert (Attr = Attribute_Bit);
               return Const_Null (GT);
            end if;

            --  Now compute the offset and bit position

            Position := Emit_Field_Position (F, Val);
            if No (Position) then
               return Get_Undef (GT);
            else
               Position := Position + Size_Const_Int (Field_Bit_Offset (F));
            end if;

            case Attr is
               when Attribute_Position =>
                  V := Position / BPU;

               when Attribute_Bit_Position =>
                  V := Position;

               when Attribute_First_Bit | Attribute_Bit =>
                  V := S_Rem (Position, Size_Const_Int (+BPU));

               when Attribute_Last_Bit =>
                  V := Position + Size_Const_Int (Esize (F) - 1);

               when others =>
                  pragma Assert (Standard.False);
            end case;

            return Convert (V, GT);
         end Position_Attr;

         when Attribute_Max | Attribute_Min =>
            pragma Assert (List_Length (Expressions (N)) = 2);
            return Emit_Min_Max (Expressions (N), Attr = Attribute_Max);

         when Attribute_Pos | Attribute_Val =>
            return Emit_Conversion (First (Expressions (N)), GT, N);

         when Attribute_Succ | Attribute_Pred => Succ_Pred : declare

            Exprs : constant List_Id      := Expressions (N);
            Expr  : constant N_Subexpr_Id := First (Exprs);
            Base  : constant GL_Value     := Emit_Expression (Expr);
            One   : constant GL_Value     := Const_Int (Base, Uint_1);

         begin
            pragma Assert (List_Length (Exprs) = 1);
            V := (if   Attr = Attribute_Succ
                   then Add (Base, One, "attr.succ")
                     else Sub (Base, One, "attr.pred"));

            --  If this is a modular type, we have to check for wrap and
            --  adjust if so.

            if Non_Binary_Modulus (GT) then
               declare
                  C_0  : constant GL_Value := Const_Null (Base);
                  C_M1 : constant GL_Value :=
                    Const_Int (Base, Modulus (GT) - 1);

               begin
                  if Attr = Attribute_Succ then
                     V := Build_Select (I_Cmp (Int_EQ, Base, C_M1), C_0, V);
                  else --  Attr = Attribute_Pred
                     V := Build_Select (I_Cmp (Int_EQ, Base, C_0), C_M1, V);
                  end if;
               end;
            end if;

            return V;
         end Succ_Pred;

         when Attribute_Machine | Attribute_Model =>

            --  ??? For now return the prefix itself. Would need to force a
            --  store in some cases.

            return Emit_Expression (First (Expressions (N)));

         when Attribute_Alignment =>
            return Const_Int (GT, To_Bytes (Get_Type_Alignment (P_GT)),
                              Sign_Extend => False);

         when Attribute_Size | Attribute_Object_Size | Attribute_Value_Size
            | Attribute_Max_Size_In_Storage_Elements
         =>
         Size : declare

            Is_A_Type   : constant Boolean :=
              Is_Entity_Name (Pref) and then Is_Type (Entity (Pref));
            Max_Size    : constant Boolean :=
              Is_A_Type and then not Is_Constrained (P_GT);
            No_Padding  : constant Boolean :=
              Is_A_Type
              and then Attr in Attribute_Size | Attribute_Value_Size;

         begin
            --  If this is a value we want to use that value to help find
            --  the size of the type and also to get the actual
            --  GL_Type. This is only useful for aggregate types or
            --  N_Selected_Component and could cause us to needlessly make
            --  a Reference in other cases.

            if not Is_A_Type
              and then (Is_Aggregate_Type (Full_Etype (Pref))
                        or else Nkind (Pref) = N_Selected_Component)
            then
               V    := Emit_LValue (Pref);
               P_GT := Related_Type (V);
            end if;

            V := Get_Type_Size (P_GT, V,
                                Max_Size   => Max_Size,
                                No_Padding => No_Padding);
            if Attr = Attribute_Max_Size_In_Storage_Elements then
               if Is_Unconstrained_Array (P_GT) then
                  V := V + Get_Bound_Size (P_GT);
               end if;

               return Convert (To_Bytes (V), GT);
            else
               return Convert (V, GT);
            end if;
         end Size;

         when Attribute_Component_Size =>
            return Convert
              (Get_Type_Size (Full_Component_GL_Type (P_GT), Max_Size => True),
               GT);

         when Attribute_Descriptor_Size =>
            pragma Assert (Is_Unconstrained_Array (P_GT));

            return Get_Bound_Size (P_GT);

         when Attribute_Passed_By_Reference =>

            --  Return 1 if must pass by reference or if default to pass by ref

            return Const_Int
              (GT, (if   Get_Param_By_Ref_Mech (P_GT) = Default_By_Copy
                    then Uint_0 else Uint_1));

         when Attribute_Mechanism_Code =>
            return Const_Int
              (GT, Get_Mechanism_Code (Entity (Pref), Expressions (N)));

         when Attribute_Null_Parameter =>
            return Load (Const_Null_Ref (P_GT));

         when others =>
            pragma Assert (Decls_Only);
            return Emit_Undef (GT);
      end case;
   end Emit_Attribute_Reference;

   ---------------------
   -- Emit_Assignment --
   ---------------------

   procedure Emit_Assignment
     (LValue       : GL_Value;
      Expr         : Opt_N_Subexpr_Id  := Empty;
      Value        : GL_Value          := No_GL_Value;
      Forwards_OK  : Boolean           := True;
      Backwards_OK : Boolean           := True;
      VFA          : Boolean           := False)
   is
      E       : constant Opt_N_Subexpr_Id := Strip_Complex_Conversions (Expr);
      Dest_GT : constant GL_Type          := Related_Type (LValue);
      Src_GT  : GL_Type                   :=
        (if Present (Value) then Related_Type (Value) else No_GL_Type);
      Dest    : GL_Value                  := LValue;
      Src     : GL_Value                  := Value;
      Dest_R  : GL_Relationship;
      Src_R   : GL_Relationship;

   begin
      --  The back-end supports exactly two types of array aggregates.
      --  One, handled in Emit_Array_Aggregate, is for a fixed-size
      --  aggregate of fixed-size components. The other type is the special
      --  case of a single entry that's tested for in
      --  Aggr_Assignment_OK_For_Backend in Exp_Aggr. We have to handle
      --  them here because we want to store directly into the LHS. The
      --  front end guarantees that any single-entry aggregate will always
      --  be the RHS of an assignment, so we'll see it here.

      if Is_Array_Type (Dest_GT) and then Present (E)
        and then Nkind (E) in N_Aggregate | N_Extension_Aggregate
        and then Is_Single_Aggregate (E)
      then
         Emit_Single_Aggregate (Dest, E);
         return;
      end if;

      --  If we haven't yet computed our source expression, do it now. If
      --  the evaluation used the location we specified, we're done.
      --  Otherwise, if we want a value, get it.

      if No (Src) then
         Src := Emit (E, LHS => (if VFA then No_GL_Value else Dest));
         Src_GT := Related_Type (Src);

         if Value_T'(+Src) = +Dest then
            Maybe_Store_Bounds (Dest, Src, Src_GT, False);
            return;
         elsif not Is_Data (Src) and then Is_Loadable_Type (Src_GT)
           and then not Has_SM_Copy_From (Src)
           and then not Has_SM_Copy_To (Dest)
         then
            Src := Get (Src, Object);
         end if;

      end if;

      --  Once we've elaborated everything, we don't need to do anything
      --  more if all we're to do is to elaborate and back-annotate.

      if Decls_Only then
         return;
      end if;

      --  See what relationships we have for the source and destination to
      --  help make choices below.

      Dest_R := Relationship (Dest);
      Src_R  := Relationship (Src);

      --  If we're assigning to a type that's the nominal constrained
      --  subtype of an unconstrained array for an aliased object, see if
      --  we can get the value and bounds together and store them. If we
      --  can, do so and we're done. Otherwise, store the bounds.

      if Type_Needs_Bounds (Dest_GT) and then Src_R /= Bounds_And_Data then
         if Is_Data (Src) and then Is_Loadable_Type (Dest_GT) then
            Store (Get (Src, Bounds_And_Data),
                   Get (Convert_Pointer (LValue, Src_GT),
                        Reference_To_Bounds_And_Data));
            return;
         end if;

         Maybe_Store_Bounds (LValue, Src, Src_GT, False);
      end if;

      --  We now have three case: we're copying an object of an elementary
      --  type, we're copying an object that's not elementary, but can be
      --  copied with a Store instruction, or we're copying an object of
      --  variable size.
      --
      --  First handle the easy case: convert the source to the destination
      --  type and store it. However, we may have a packed array
      --  implementation type on the LHS and an array on the RHS. Convert
      --  it to the LHS type if so.

      if Is_Elementary_Type (Dest_GT) and then Src_R /= Bounds_And_Data then
         if Is_Packed_Array_Impl_Type (Dest_GT)
           and then not Is_Elementary_Type (Src_GT)
         then
            Src := Convert_Ref (Get (Src, Reference), Dest_GT);
         end if;

         --  If we have to convert or Dest doesn't have a copy-from
         --  procedure, get Src as data.

         if Related_Type (Src) /= Dest_GT or else not Has_SM_Copy_From (Dest)
         then
            Src := Convert (Get (Src, Data), Dest_GT);
         end if;

         --  And finally do the store

         Store (Src, Get (Dest, Reference));

      --  If we're setting Dest to zeros, use memset

      elsif Is_A_Constant_Aggregate_Zero (Src)
        and then not Has_SM_Copy_To (Dest)
      then
         declare
            Size : constant GL_Value :=
              To_Bytes (Compute_Size (Dest_GT, Related_Type (Src), Dest, Src,
                                      For_Assignment => True));

         begin
            if Size /= 0 then
               Build_MemSet (Pointer_Cast (Get (Dest, Reference),
                                           A_Char_GL_Type),
                             Const_Null (SSI_GL_Type), Size,
                             To_Bytes (Get_Type_Alignment (Dest_GT)),
                             Is_Volatile (Dest),
                             TBAA =>
                               Compute_TBAA_Access (Dest, No_GL_Value, Size));
            end if;
         end;

      --  Now see if the source is of an LLVM value small enough to store, but
      --  the destination may or may not be a variable-sized type. Since
      --  we know the size and know the object to store, we can convert
      --  Dest to the type of the pointer to Src, which we know is
      --  fixed-size, and do the store. If Dest is a pointer to an array
      --  type, we need to point to the actual array data. But if Dest
      --  isn't of its default type, the pointer pun below may cause us to
      --  copy the wrong amount of data, so don't do this optimization
      --  in that case.

      elsif (No (E) or else (Is_Loadable_Type (Full_GL_Type (E))
                             and then Full_GL_Type (E) = Related_Type (Dest)))
        and then (No (Value) or else Is_Loadable_Type (Value))
        and then not Is_Class_Wide_Equivalent_Type (Dest_GT)
      then
         Src := Get (Src, (if Src_R = Bounds_And_Data then Src_R else Data));

         if Relationship (Dest) = Fat_Pointer
           or else Type_Of (Src) /= Element_Type_Of (Dest)
         then
            Dest := Ptr_To_Relationship (Get (Dest, Reference), Src,
                                         Ref (Relationship (Src)));
         end if;

         Store (Src, Dest);

      --  Otherwise, we have to do a block copy, which may be variable-sized

      else
         declare
            Need_Volatile    : constant Boolean :=
              Is_Volatile (Src) or else Is_Volatile (Dest);
            Size             : GL_Value         :=
              Compute_Size (Dest_GT, Related_Type (Src), Dest, Src,
                            For_Assignment => True);
            Mem_Src, Mem_Dst : GL_Value;

         begin
            --  Get the proper relationship. If we're copying both bounds
            --  and data, get the reference to that. Otherwise, if this is
            --  a fat pointer, get the reference to the data. Otherwise,
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
               Size := Size + Get_Bound_Size (Related_Type (Src));
            end if;

            --  Compute the size in bytes and any pointer casts
            --  outside of the call to create the memory operation to
            --  ensure a consistent ordering.

            Size    := To_Bytes (Size);
            Mem_Src := Get (Src, Src_R);
            Mem_Dst := Get (Dest, Dest_R);

            --  We may have to use a copy from/to procedure (or both) if
            --  either side has a non-default Storage Model.

            if Has_SM_Copy_From (Mem_Src) and then not Has_SM_Copy_To (Mem_Dst)
            then
               Call_SM_Copy_From (Mem_Dst, Mem_Src, Size);
            elsif Has_SM_Copy_To (Mem_Dst)
              and then not Has_SM_Copy_From (Mem_Src)
            then
               Call_SM_Copy_To (Mem_Dst, Mem_Src, Size);
            elsif Has_SM_Copy_From (Mem_Src) and then Has_SM_Copy_To (Mem_Dst)
            then
               declare
                  Temp : constant GL_Value :=
                    Array_Alloca
                      (SSI_GL_Type, Size, Empty,
                       To_Bytes (Nat'Max (Get_Type_Alignment (Dest_GT),
                                          Get_Type_Alignment (Src_GT))));

               begin
                  Call_SM_Copy_From (Temp, Mem_Src, Size);
                  Call_SM_Copy_To   (Mem_Dst, Temp, Size);
               end;

            --  If emitting C and this is of zero size, do nothing

            elsif Emit_C and then Size = Size_Const_Null then
               null;

            --  Otherwise use memcpy if we know there's no overlap

            elsif (Forwards_OK and then Backwards_OK)
              or else (Present (Expr) and then Is_Safe_From (Dest, Expr))
            then
               Mem_Src := Pointer_Cast (Mem_Src, A_Char_GL_Type);
               Mem_Dst := Pointer_Cast (Mem_Dst, A_Char_GL_Type);
               Build_MemCpy (Mem_Dst, To_Bytes (Get_Type_Alignment (Dest_GT)),
                             Mem_Src, To_Bytes (Get_Type_Alignment (Src_GT)),
                             Size, Need_Volatile,
                             TBAA        =>
                               Compute_TBAA_Access (Dest, Src, Size),
                             TBAA_Struct =>
                               Compute_TBAA_Struct (Dest, Src, Size));

            --  Or else use memmove

            else
               Build_MemMove (Mem_Dst, To_Bytes (Get_Type_Alignment (Dest_GT)),
                              Mem_Src, To_Bytes (Get_Type_Alignment (Src_GT)),
                              Size, Need_Volatile,
                              TBAA => Compute_TBAA_Access (Dest, Src, Size));
            end if;
         end;
      end if;
   end Emit_Assignment;

   -------------------------
   -- Emit_Code_Statement --
   ------------------------

   procedure Emit_Code_Statement (N : N_Code_Statement_Id) is
      Template_Strval   : constant String_Id := Strval (Asm_Template (N));
      Num_Inputs        : Nat                := 0;
      Constraint_Length : Nat                := 0;
      Output_Constraint : Node_Id            := Empty;
      Output_Val        : GL_Value           := No_GL_Value;
      Output_Variable   : Node_Id;
      Input             : Node_Id;
      Clobber           : System.Address;
      Fn_T              : Type_T;

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
      --  constraints here. Node that Clobber_Get_Next isn't very friendly
      --  for an Ada caller, so we'll use fact that it's set Name_Buffer
      --  and Name_Len;

      Clobber_Setup (N);
      Clobber := Clobber_Get_Next;

      while not System."=" (Clobber, System.Null_Address) loop
         Constraint_Length := Constraint_Length + Nat (Name_Len) + 4;
         Clobber           := Clobber_Get_Next;
      end loop;

      declare
         Args             : GL_Value_Array (1 .. Num_Inputs);
         Constraint_Pos   : Integer          := 0;
         Input_Pos        : Nat              := 0;
         Need_Comma       : Boolean          := False;
         Constraint_Len   : constant Integer :=
           Integer (Num_Inputs + Constraint_Length + 3);
         In_Template_Len  : constant Integer :=
           Integer (String_Length (Template_Strval));
         Out_Template_Len : constant Integer :=
           In_Template_Len + (Integer (Num_Inputs) * 3);
         Constraints      : String (1 .. Constraint_Len);
         Template         : String (1 .. Out_Template_Len);
         Template_Char    : Character;
         Arg_Modifier     : String (1 .. In_Template_Len);
         Arg_Modifier_Pos : Integer;
         Asm              : GL_Value;
         In_Template_Pos  : Integer := 0;
         Out_Template_Pos : Integer := 0;

         procedure Add_Char (C : Character);
         procedure Add_Constraint (N : N_String_Literal_Id);
         procedure Add_Template_Char (C : Character);
         procedure Next_Template_Char;

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

         procedure Add_Constraint (N : N_String_Literal_Id) is
         begin
            if Need_Comma then
               Add_Char (',');
            end if;

            for J in 1 .. String_Length (Strval (N)) loop
               Add_Char (Get_Character (Get_String_Char (Strval (N), J)));
            end loop;
         end Add_Constraint;

         -----------------------
         -- Add_Template_Char --
         -----------------------

         procedure Add_Template_Char (C : Character) is
         begin
            Out_Template_Pos := Out_Template_Pos + 1;
            Template (Out_Template_Pos) := C;
         end Add_Template_Char;

         ------------------------
         -- Next_Template_Char --
         ------------------------

         procedure Next_Template_Char is
         begin
            In_Template_Pos := In_Template_Pos + 1;
            Template_Char :=
              Get_Character
                (Get_String_Char
                   (Template_Strval, Int (In_Template_Pos)));
         end Next_Template_Char;

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

         --  Finally, build the template. LLVM bitcode uses a syntax for
         --  the template that is different from GNU as. For now, we only
         --  translate numeric input/output references with optional
         --  argument modifiers (e.g., "%1" or "%w1").

         Template_Parser : while In_Template_Pos < In_Template_Len loop
            Next_Template_Char;

            if Template_Char = '%' then

               --  Parse the argument reference. It can be a simple
               --  reference like "%1", or a reference with argument
               --  modifiers like "%w1". The LLVM equivalents are "$1" and
               --  "${1:w}", respectively.

               Add_Template_Char ('$');
               exit Template_Parser when
                 In_Template_Pos = In_Template_Len;
               Next_Template_Char;

               if Template_Char in '0' .. '9' then
                  Add_Template_Char (Template_Char);
               else
                  Arg_Modifier_Pos := 0;

                  while Template_Char not in '0' .. '9' loop
                     Arg_Modifier_Pos := Arg_Modifier_Pos + 1;
                     Arg_Modifier (Arg_Modifier_Pos) := Template_Char;

                     exit Template_Parser when
                       In_Template_Pos = In_Template_Len;
                     Next_Template_Char;
                  end loop;

                  Add_Template_Char ('{');
                  Add_Template_Char (Template_Char);
                  Add_Template_Char (':');

                  for J in 1 .. Arg_Modifier_Pos loop
                     Add_Template_Char (Arg_Modifier (J));
                  end loop;

                  Add_Template_Char ('}');
               end if;
            else
               Add_Template_Char (Template_Char);
            end if;
         end loop Template_Parser;

         --  Create the inline asm

         Asm :=
           Inline_Asm
             (Args, Output_Variable, Template (1 .. Out_Template_Pos),
              Constraints (1 .. Constraint_Pos), Fn_T,
              Is_Asm_Volatile (N), False);

         --  If we have an output, generate the call with an output and
         --  store the result, casting the output pointer to the right type
         --  to handle LLVM's refusal of some output types. Otherwise, just
         --  do the call.

         if Present (Output_Variable) then
            Store
              (Call (Asm, Fn_T, Args),
               Ptr_To_Ref (Output_Val, Related_Type (Asm)));
         else
            Call (Asm, Fn_T, Args);
         end if;
      end;
   end Emit_Code_Statement;

end GNATLLVM.Exprs;
