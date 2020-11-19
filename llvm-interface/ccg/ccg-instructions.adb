------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020, AdaCore                          --
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

with Interfaces.C;            use Interfaces.C;

--  This clause is only needed with old versions of GNAT
pragma Warnings (Off);
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
pragma Warnings (On);

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with CCG.Aggregates;  use CCG.Aggregates;
with CCG.Blocks;      use CCG.Blocks;
with CCG.Output;      use CCG.Output;
with CCG.Subprograms; use CCG.Subprograms;
with CCG.Utils;       use CCG.Utils;

package body CCG.Instructions is

   function Get_Extra_Bits (J : ULL) return ULL;
   function Get_Extra_Bits (T : Type_T) return ULL is
     (Get_Extra_Bits (Get_Scalar_Bit_Size (T)))
     with Pre => Get_Type_Kind (T) = Integer_Type_Kind, Unreferenced;
   --  Return the number of bits needed to go from J or the bit width of T to
   --  the next power-of-two size in bits that's at least a byte wide.

   function Is_Comparison (V : Value_T) return Boolean
     with Pre => Present (V);
   --  Return True if V is known to be the result of a comparison or a
   --  logical operation on comparisons.

   function Binary_Instruction (V, Op1, Op2 : Value_T) return Str
     with Pre  => Acts_As_Instruction (V) and then Present (Op1)
                  and then Present (Op2),
          Post => Present (Binary_Instruction'Result);
   --  Return the value corresponding to a binary instruction

   function Cast_Instruction (V, Op : Value_T) return Str
     with Pre  => Acts_As_Instruction (V) and then Present (Op),
          Post => Present (Cast_Instruction'Result);
   --  Return the value corresponding to a cast instruction

   function Cmp_Instruction (V, Op1, Op2 : Value_T) return Str
     with Pre  => Get_Opcode (V) in Op_I_Cmp | Op_F_Cmp
                  and then Present (Op1) and then Present (Op2),
          Post => Present (Cmp_Instruction'Result);
   --  Return the value corresponding to a comparison instruction

   function Maybe_Unsigned
     (V : Value_T; Op_Unsigned : Boolean := True) return Str
   is
     ((if Op_Unsigned then V + Is_Unsigned else V + Is_Signed))
     with Pre => Present (V), Post => Present (Maybe_Unsigned'Result);
     --  If Op_Unsigned is True, V must be treated as unsigned. Otherwise
     --  it must be treated as signed.

   --------------------
   -- Get_Extra_Bits --
   --------------------

   function Get_Extra_Bits (J : ULL) return ULL is
      type M is mod 2**16;
      Pow2_M_1 : M := M (J) - 1;

   begin
      --  We do this with bit-twiddling that turns on all bits that are
      --  one within Width - 1, use at least a value of 7 so that we go
      --  to at least a byte, and then add the one back.

      Pow2_M_1 := Pow2_M_1 or Pow2_M_1 / 2;
      Pow2_M_1 := Pow2_M_1 or Pow2_M_1 / 4;
      Pow2_M_1 := M'Max (Pow2_M_1 or Pow2_M_1 / 16, 7);

      return ULL (Pow2_M_1 - M (J) + 1);
   end Get_Extra_Bits;

   -------------------
   -- Is_Comparison --
   -------------------

   function Is_Comparison (V : Value_T) return Boolean is
   begin
      --  If this isn't a bit type, we know it isn't a comparison. If it's
      --  a simple constant, we know that it is.  If this isn't an
      --  instruction, we don't know that it's a comparison.

      if Type_Of (V) /= Bit_T then
         return False;
      elsif Is_Simple_Constant (V) then
         return True;
      elsif not Is_A_Instruction (V) then
         return False;
      end if;

      --  Otherwise our test is opcode-specific

      case Get_Opcode (V) is
         when Op_I_Cmp | Op_F_Cmp =>
            return True;

         when Op_And | Op_Or =>
            return Is_Comparison (Get_Operand (V, Nat (0)))
              and then Is_Comparison (Get_Operand (V, Nat (1)));

         when Op_Xor =>
            return Is_Comparison (Get_Operand (V, Nat (0)))
              and then Is_A_Constant_Int (Get_Operand (V, Nat (1)))
              and then Equals_Int (Get_Operand (V, Nat (1)), 1);

         when Op_PHI =>
            return (for all J in Nat range 0 .. Get_Num_Operands (V) - 1 =>
                     Is_Comparison (Get_Operand (V, J)));

         when Op_Select =>
            return Is_Comparison (Get_Operand (V, Nat (1)))
              and then Is_Comparison (Get_Operand (V, Nat (2)));

         when others =>
            return False;
      end case;
   end Is_Comparison;

   ------------------------
   -- Binary_Instruction --
   ------------------------

   function Binary_Instruction (V, Op1, Op2 : Value_T) return Str is
      Opc : constant Opcode_T := Get_Opcode (V);
      T   : constant Type_T   := Type_Of (V);

   begin
      case Opc is
         when Op_Add =>
            return TP ("#1 + #2", Op1, Op2) + Add;

         when Op_Sub =>
            return TP ("#1 - #2", Op1, Op2) + Add;

         when Op_Mul =>
            return TP ("#1 * #2", Op1, Op2) + Mult;

         when Op_S_Div | Op_U_Div =>
            return Maybe_Unsigned (Op1, Opc = Op_U_Div) & " / " &
              Maybe_Unsigned (Op2, Opc = Op_U_Div) + Mult;

         when Op_S_Rem | Op_U_Rem =>
            return Maybe_Unsigned (Op1, Opc = Op_U_Rem) & " % " &
              Maybe_Unsigned (Op2, Opc = Op_U_Rem) + Mult;

         when Op_Shl =>
            return TP ("#1 << #2", Op1, Op2) + Shift;

         when Op_L_Shr | Op_A_Shr =>
            return Maybe_Unsigned (Op1, Opc = Op_L_Shr) & " >> " & Op2 + Shift;

         when Op_F_Add =>
            return TP ("#1 + #2", Op1, Op2) + Add;

         when Op_F_Sub =>
            return TP ("#1 - #2", Op1, Op2) + Add;

         when Op_F_Mul =>
            return TP ("#1 * #2", Op1, Op2) + Mult;

         when Op_F_Div =>
            return TP ("#1 / #2", Op1, Op2) + Mult;

         when Op_And =>
            if T = Bit_T then
               return TP ("#1 && #2", Op1, Op2) + Logical_AND;
            else
               return TP ("#1 & #2", Op1, Op2) + Bit;
            end if;

         when Op_Or =>
            if T = Bit_T then
               return TP ("#1 || #2", Op1, Op2) + Logical_OR;
            else
               return TP ("#1 | #2", Op1, Op2) + Bit;
            end if;

         when Op_Xor =>
            if T = Bit_T and then Is_A_Constant_Int (Op2)
              and then Equals_Int (Op2, 1)
            then
               return TP ("! #1", Op1) + Unary;
            else
               return TP ("#1 ^ #2", Op1, Op2) + Bit;
            end if;

         when others =>
            return raise Program_Error;
      end case;
   end Binary_Instruction;

   ----------------------
   -- Cast_Instruction --
   ----------------------

   function Cast_Instruction (V, Op : Value_T) return Str is
      Opc    : constant Opcode_T := Get_Opcode (V);
      Src_T  : constant Type_T   := Type_Of (Op);
      Dest_T : constant Type_T   := Type_Of (V);
      Our_Op : constant Str      :=
        Maybe_Unsigned (Op, Opc in Op_UI_To_FP | Op_Z_Ext);

   begin
      --  If we're doing a bitcast and the input and output types aren't
      --  both pointers, we need to do this by pointer-punning.

      if Opc = Op_Bit_Cast
        and then (Get_Type_Kind (Src_T) /= Pointer_Type_Kind
                    or else Get_Type_Kind (Dest_T) /= Pointer_Type_Kind)
      then
         --  If or operand is an expression, we probably can't validly take
         --  its address, so be sure that we make an actual variable that
         --  we can take the address of.

         if Present (Get_C_Value (Op)) then
            declare
               C_Val : constant Str := Get_C_Value (Op);

            begin
               --  We have to undo what was done to show that we don't need
               --  a variable for Op. Specifically, we have to clear its
               --  value, declare it, and copy the value to it.

               Set_C_Value (Op, No_Str);
               Maybe_Decl  (Op);
               Write_Copy  (Op, C_Val, Type_Of (Op));
            end;
         end if;

         return TP ("*((#T) #A1)", Op, T => Pointer_Type (Dest_T, 0)) + Unary;

      --  If we're zero-extending a value that's known to be a comparison
      --  result to an i8, we do nothing since we know that the value is
      --  already either a zero or one.

      elsif Opc = Op_Z_Ext and then Is_Comparison (Op)
        and then Dest_T = Byte_T
      then
         return +Op;

      --  For integral types, we have the possibility that we're starting
      --  with a size that's smaller than a storage unit or not a power of
      --  two bits. For those, we have to do the sign- or zero-extension
      --  to the next-higher power of two with two shifts (for zero-extension,
      --  this can be done with an AND of a constant, but all compilers wil
      --  convert the shifts to that and it's simpler to use shifts). Since
      --  C will promote a shift of a width narrower than int to int, we
      --  need to adjust the shift amount to compensate. Note that an
      --  arithmetic shift of a negative number is implementation-defined,
      --  but all known implementations sign extend.

      elsif Get_Type_Kind (Src_T) = Integer_Type_Kind
        and then Get_Type_Kind (Dest_T) = Integer_Type_Kind
      then
         declare
            Dest_Size     : constant ULL    := Get_Scalar_Bit_Size (Dest_T);
            Src_Size      : constant ULL    := Get_Scalar_Bit_Size (Src_T);
            Src_Extras    : constant ULL    := Get_Extra_Bits (Src_Size);
            Dest_Extras   : constant ULL    := Get_Extra_Bits (Dest_Size);
            Src_Int_Size  : constant ULL    := Src_Size  + Src_Extras;
            Dest_Int_Size : constant ULL    := Dest_Size + Dest_Extras;
            Compute_Size  : constant ULL    :=
              (if   Src_Extras /= 0 and then Src_Int_Size < ULL (Int_Size)
               then ULL (Int_Size) else Src_Int_Size);
            Compute_T     : constant Type_T := Int_Ty (Compute_Size);
            Cnt           : constant Nat    := Nat (Compute_Size - Src_Size);
            Result        : Str             := Our_Op;

         begin
            --  If we have extra bits to bring this to normal integer type,
            --  do the shifts to sign/zero extend.

            if Src_Extras /= 0 then

               --  If we're doing a zext and integer promotion will occur
               --  for the shift, it'll be to int, not unsigned int, because
               --  of the way the integer promotion rules work. So handle that
               --  case specially.

               if Opc = Op_Z_Ext and then Compute_Size /= Src_Int_Size then
                  Result := "(unsigned " & Compute_T & ") " & Op + Unary;
               end if;

               Result := "(" & Result & " << " & Cnt & ") >> " & Cnt + Shift;
            end if;

            --  If we have to change sizes beyond (possibly) doing the shifts,
            --  emit that cast.

            if Dest_Int_Size /= Compute_Size then
               Result := ("(" & Dest_T & ") " & (Result + Unary)) + Unary;
            end if;

            --  The operation is usually a shift and/or a cast, but in the
            --  case of a trunc from, say, i16 to i12, we have nothing to do.

            return Result;
         end;

      --  Otherwise, just do a cast

      else
         return ("(" & Dest_T & ") " & Our_Op) + Unary;
      end if;

   end Cast_Instruction;

   ---------------------
   -- Cmp_Instruction --
   ---------------------

   function Cmp_Instruction (V, Op1, Op2 : Value_T) return Str is

   begin
      if Get_Opcode (V) = Op_I_Cmp then
         declare
            type I_Info is record
               Is_Unsigned : Boolean;
               Length      : Integer;
               Op          : String (1 .. 2);
            end record;
            type I_Info_Array is array (Int_Predicate_T range <>) of I_Info;
            Pred        : constant Int_Predicate_T := Get_I_Cmp_Predicate (V);
            Int_Info    : constant I_Info_Array :=
              (Int_EQ  => (False, 2, "=="),
               Int_NE  => (False, 2, "!="),
               Int_UGT => (True,  1, "> "),
               Int_UGE => (True,  2, ">="),
               Int_ULT => (True,  1, "< "),
               Int_ULE => (True,  2, "<="),
               Int_SGT => (False, 1, "> "),
               Int_SGE => (False, 2, ">="),
               Int_SLT => (False, 1, "< "),
               Int_SLE => (False, 2, "<="));
            Info        : constant I_Info := Int_Info (Pred);
            Maybe_Uns   : constant Boolean :=
              Might_Be_Unsigned (Op1) or else Might_Be_Unsigned (Op2);
            Do_Unsigned : constant Boolean :=
              (if   Pred in Int_EQ | Int_NE then Maybe_Uns
               else Info.Is_Unsigned);
            LHS         : constant Str    := Maybe_Unsigned (Op1, Do_Unsigned);
            RHS         : constant Str    := Maybe_Unsigned (Op2, Do_Unsigned);

         begin
            return (LHS & " " & Info.Op (1 .. Info.Length) & " " & RHS) +
              Relation;
         end;

      elsif Get_Opcode (V) = Op_F_Cmp then

         case Get_F_Cmp_Predicate (V) is
            when Real_OEQ | Real_UEQ =>
               return TP ("#1 == #2", Op1, Op2) + Relation;
            when Real_OGT | Real_UGT =>
               return TP ("#1 > #2", Op1, Op2) + Relation;
            when Real_OGE | Real_UGE =>
               return TP ("#1 >= #2", Op1, Op2) + Relation;
            when Real_OLT | Real_ULT =>
               return TP ("#1 < #2", Op1, Op2) + Relation;
            when Real_OLE | Real_ULE =>
               return TP ("#1 <= #2", Op1, Op2) + Relation;
            when Real_ONE | Real_UNE =>
               return TP ("#1 != #2", Op1, Op2) + Relation;
            when others =>
               null;
         end case;
      end if;

      return +("<unsupported comparison: " & Get_Opcode_Name (V) & ">");

   end Cmp_Instruction;

   ----------------
   -- Write_Copy --
   ----------------

   procedure Write_Copy (LHS : Value_T; RHS : Str; T : Type_T) is
   begin
      Write_Copy (+LHS, RHS, T);
   end Write_Copy;

   ----------------
   -- Write_Copy --
   ----------------

   procedure Write_Copy (LHS : Str; RHS : Value_T; T : Type_T) is
   begin
      Write_Copy (LHS, +RHS, T);
   end Write_Copy;

   ----------------
   -- Write_Copy --
   ----------------

   procedure Write_Copy (LHS, RHS : Value_T; T : Type_T) is
   begin
      Write_Copy (+LHS, +RHS, T);
   end Write_Copy;

   ----------------
   -- Write_Copy --
   ----------------

   procedure Write_Copy (LHS, RHS : Str; T : Type_T) is
   begin
      --  If this isn't an array type, write a normal assignment. Otherwise,
      --  use memmove.
      --  ??? We can usually use memcpy, but it's not clear what test to
      --  do here at the moment.

      if Get_Type_Kind (T) /= Array_Type_Kind then
         Output_Stmt (LHS & " = " & RHS + Assign);
      else
         Output_Stmt ("memmove ((void *) " & (Addr_Of (LHS) + Comma) &
                        ", (void *) " & (Addr_Of (RHS) + Comma) &
                        ", sizeof (" & T & "))");
      end if;
   end Write_Copy;

   ----------------
   -- Assignment --
   ----------------

   procedure Assignment (LHS : Value_T; RHS : Str) is
   begin
      --  If LHS is a LHS, has more than one use in the IR, if we've
      --  already emitted a decl for it (e.g., it was defined in a block we
      --  haven't processed yet), or if it's a function call or volatile load,
      --  generate an assignment statement into LHS. Otherwise, mark LHS as
      --  having value RHS. If LHS is a constant expression or of array
      --  types, never generate an assignment statement, the former because
      --  we may be at top level and the latter because C doesn't allow
      --  assignments of objects of aggregate type.

      if (Get_Is_LHS (LHS) or else Num_Uses (LHS) > 1
            or else Get_Is_Decl_Output (LHS) or else Is_A_Call_Inst (LHS)
            or else (Is_A_Load_Inst (LHS) and then Get_Volatile (LHS)))
        and then not Is_A_Constant_Expr (LHS)
        and then Get_Type_Kind (Type_Of (LHS)) /= Array_Type_Kind
      then
         Maybe_Decl (LHS);
         Write_Copy (LHS, RHS, Type_Of (LHS));
      else
         Set_C_Value (LHS, RHS);
      end if;
   end Assignment;

   ------------------
   --  Instruction --
   ------------------

   procedure Instruction (V : Value_T; Ops : Value_Array) is
      Op1 : constant Value_T  :=
        (if Ops'Length >= 1 then Ops (Ops'First) else No_Value_T);
      Op2 : constant Value_T  :=
        (if Ops'Length >= 2 then Ops (Ops'First + 1) else No_Value_T);
      Op3 : constant Value_T  :=
        (if Ops'Length >= 3 then Ops (Ops'First + 2) else No_Value_T);
      Opc : constant Opcode_T := Get_Opcode (V);

   begin
      --  See if we need to write a declaration for an operand

      for Op of Ops loop
         Maybe_Decl (Op);
      end loop;

      --  Handle the instruction according to its opcode

      case Opc is
         when Op_Ret =>

            --  If our function is marked no-return, we don't do anything.
            --  Otherwise, handle the cases with and without a value to
            --  return.

            if Does_Not_Return (Current_Func) then
               null;
            elsif Present (Op1) then
               Output_Stmt ("return " & Op1 + Assign);
            else
               Output_Stmt ("return");
            end if;

         when Op_Call =>
            Call_Instruction (V, Ops);

         when Op_Alloca =>
            if Is_Entry_Block (V) and then Is_A_Constant_Int (Op1)
              and then Equals_Int (Op1, 1)
            then
               Set_Is_LHS (V);
               Maybe_Decl (V);
            else
               declare
                  Size : constant Str :=
                    TP ("sizeof (#T) * #1", Op1, T => Get_Allocated_Type (V)) +
                    Mult;
                  Call : constant Str := "alloca (" & Size & ")" + Component;

               begin
                  Assignment (V, "(" & Type_Of (V) & ") " & Call + Unary);
               end;
            end if;

         when Op_Load =>

            --  ??? Need to deal with both unaligned load and unaligned store

            Assignment (V, Deref (Op1));

         when Op_Store =>
            Write_Copy (Deref (Op2), Op1, Type_Of (Op1));

         when Op_I_Cmp | Op_F_Cmp =>
            Assignment (V, Cmp_Instruction (V, Op1, Op2));

         when Op_Select =>
            Assignment (V, TP ("#1 ? #2 : #3", Op1, Op2, Op3) + Conditional);

         when Op_Br =>
            if Ops'Length = 1 then
               Output_Branch (V, Op1);
            else
               Output_Stmt (TP ("if (#1)", Op1) + Assign, Semicolon => False);
               Output_Branch (V, Op3, Need_Block => True);
               Output_Stmt ("else", Semicolon => False);
               Output_Branch (V, Op2, Need_Block => True);
            end if;

         when Op_PHI =>

            --  PHI is processed on a branch to this block, so we need
            --  do nothing with it here.

            null;

         when Op_Add | Op_Sub | Op_Mul | Op_S_Div | Op_U_Div | Op_S_Rem
            | Op_U_Rem | Op_Shl | Op_L_Shr | Op_A_Shr | Op_F_Add | Op_F_Sub
            | Op_F_Mul | Op_F_Div | Op_And | Op_Or | Op_Xor =>
            Assignment (V, Binary_Instruction (V, Op1, Op2));

         when Op_F_Neg =>
            Assignment (V, TP (" -#1", Op1) + Unary);

         when Op_Trunc | Op_SI_To_FP | Op_FP_Trunc | Op_FP_Ext | Op_S_Ext
            | Op_UI_To_FP | Op_FP_To_SI | Op_FP_To_UI | Op_Z_Ext | Op_Bit_Cast
            | Op_Ptr_To_Int | Op_Int_To_Ptr =>
            Assignment (V, Cast_Instruction (V, Op1));

         when Op_Extract_Value =>
            Assignment (V, Extract_Value_Instruction (V, Op1));

         when Op_Insert_Value =>
            Insert_Value_Instruction (V, Op1, Op2);

         when Op_Get_Element_Ptr =>
            GEP_Instruction (V, Ops);

         when Op_Switch =>
            Switch_Instruction (V, Ops);

         when Op_Unreachable =>
            null;

         when others =>
            Output_Stmt
              ("<unsupported instruction: " & Get_Opcode_Name (Opc) & ">");
      end case;
   end Instruction;

   -------------------------
   -- Process_Instruction --
   -------------------------

   procedure Process_Instruction (V : Value_T) is
      N_Ops : constant Nat := Get_Num_Operands (V);
      Ops   : Value_Array (1 .. N_Ops);

   begin
      for J in Ops'Range loop
         Ops (J) := Get_Operand (V, J - 1);
      end loop;

      Instruction (V, Ops);
   end Process_Instruction;

end CCG.Instructions;
