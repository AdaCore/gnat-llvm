------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2022, AdaCore                     --
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

with Table;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with CCG.Aggregates;  use CCG.Aggregates;
with CCG.Environment; use CCG.Environment;
with CCG.Flow;        use CCG.Flow;
with CCG.Output;      use CCG.Output;
with CCG.Subprograms; use CCG.Subprograms;
with CCG.Target;      use CCG.Target;
with CCG.Utils;       use CCG.Utils;

package body CCG.Instructions is

   function Get_Extra_Bits (J : Nat)    return Nat;
   function Get_Extra_Bits (T : Type_T) return Nat is
     (Get_Extra_Bits (Get_Scalar_Bit_Size (T)))
     with Pre => Get_Type_Kind (T) = Integer_Type_Kind, Unreferenced;
   --  Return the number of bits needed to go from J or the bit width of T to
   --  the next power-of-two size in bits that's at least a byte wide.

   function Is_Comparison (V : Value_T) return Boolean
     with Pre => Present (V);
   --  Return True if V is known to be the result of a comparison or a
   --  logical operation on comparisons.

   procedure Alloca_Instruction (V, Op : Value_T)
     with Pre  => Is_A_Alloca_Inst (V) and then Present (Op);
   --  Return the value corresponding to a cast instruction

   procedure Load_Instruction (V, Op : Value_T)
     with Pre  => Is_A_Load_Inst (V) and then Present (Op);
   --  Process a load instruction

   procedure Store_Instruction (V, Op1, Op2 : Value_T)
     with Pre  => Is_A_Store_Inst (V) and then Present (Op1)
                  and then Present (Op2);
   --  Process a store instruction

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

   procedure Force_To_Variable (V : Value_T)
     with Pre  => Present (V), Post => No (Get_C_Value (V));
   --  If V has an expression for it, declare V as a variable and copy the
   --  expression into it.

   --  We need to record those values where we've made them equivalent to
   --  a C value but haven't written them yet because if we encounter a
   --  store or procedure call, we need to write them out since a variable
   --  may be changed by that store or procedure call. Here we store each
   --  such value when we make the assignment, but don't delete it when
   --  we've written it; we assume that the caller will check if it's been
   --  written.

   package Pending_Values is new Table.Table
     (Table_Component_Type => Value_T,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 50,
      Table_Name           => "Pending_Values");

   --------------------
   -- Get_Extra_Bits --
   --------------------

   function Get_Extra_Bits (J : Nat) return Nat is
      type M is mod 2**16;
      Pow2_M_1 : M := M (J) - 1;

   begin
      --  We do this with bit-twiddling that turns on all bits that are
      --  one within Width - 1, use at least a value of BPU-1 so that we go
      --  to at least a byte, and then add the one back.

      Pow2_M_1 := Pow2_M_1 or Pow2_M_1 / 2;
      Pow2_M_1 := Pow2_M_1 or Pow2_M_1 / 4;
      Pow2_M_1 := M'Max (Pow2_M_1 or Pow2_M_1 / 16, M (BPU) - 1);

      return Nat (Pow2_M_1 - M (J) + 1);
   end Get_Extra_Bits;

   ---------------------
   -- Process_Operand --
   ---------------------

   function Process_Operand
     (V : Value_T; POO : Process_Operand_Option) return Str
   is
      T      : constant Type_T := Type_Of (V);
      Size   : constant Nat    :=
        (if   Get_Type_Kind (T) = Integer_Type_Kind
         then Get_Scalar_Bit_Size (T) else BPU);
      Extras : constant Nat    := Get_Extra_Bits (Size);
      Result : Str             :=
        (case POO is when X            => +V,
                     when POO_Signed   => V + Need_Signed,
                     when POO_Unsigned => V + Need_Unsigned);

   begin
      --  If all we have to do is deal with signedness, we're done

      if Extras = 0 then
         return Result;
      end if;

      --  Otherwise, we have to do a pair of shifts. Because of the C's
      --  integer promotion rules, we have to cast back to our type after
      --  each shift. We use the unsigned or signed version of the type of
      --  V depending on which we want or which we think it is if we don't
      --  care.

      declare
         Use_Signed : constant Boolean :=
           (case POO is when X            => Might_Be_Unsigned (V),
                        when POO_Signed   => True,
                        when POO_Unsigned => False);
         Cast       : constant Str     :=
           (if Use_Signed then "(" else "(unsigned ") & T & ") ";

      begin
         Result := Cast & "(" & (Result + Shift) & " << " & Extras & ")";
         return Cast & "(" & Result & " >> " & Extras & ")";
      end;
   end Process_Operand;

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
            return Is_Comparison (Get_Operand0 (V))
              and then Is_Comparison (Get_Operand1 (V));

         when Op_Xor =>
            return Is_Comparison (Get_Operand0 (V))
              and then Is_A_Constant_Int (Get_Operand1 (V))
              and then Equals_Int (Get_Operand1 (V), 1);

         when Op_Select =>
            return Is_Comparison (Get_Operand1 (V))
              and then Is_Comparison (Get_Operand2 (V));

         when others =>
            return False;
      end case;
   end Is_Comparison;

   ----------------------------
   -- Process_Pending_Values --
   ----------------------------

   procedure Process_Pending_Values is
   begin
      --  We have a list of pending values, which represent LLVM
      --  instructions that are being stored as C expressions and not
      --  copied into declared variables. We want to do the stores for any
      --  "final" values. The only values that have been saved to this list
      --  are those that are used exactly once. That usage is either by a
      --  later entry in the list or by an instruction we haven't
      --  encountered yet. In the former case, we want to use it in the
      --  elaboration of that later list entry.
      --
      --  We work from the end of the list towards the front since we don't
      --  need to produce variables for expressions only used later.  But
      --  because the only entries in the list are used exactly once, we
      --  know that we don't see the reference to the value as a variable
      --  in elaborating any other list entry. So we know that the values
      --  written out here are independent and thus the fact that we're
      --  writing them out "backwards" is fine.

      for J in reverse 1 .. Pending_Values.Last loop
         declare
            V : constant Value_T := Pending_Values.Table (J);

         begin
            if not Get_Is_Used (V) then
               Force_To_Variable (V);
            end if;
         end;
      end loop;

      Pending_Values.Set_Last (0);
   end Process_Pending_Values;

   --------------------------
   -- Clear_Pending_Values --
   --------------------------

   procedure Clear_Pending_Values is
   begin
      Pending_Values.Set_Last (0);
   end Clear_Pending_Values;

   ------------------------
   -- Alloca_Instruction --
   ------------------------

   procedure Alloca_Instruction (V, Op : Value_T) is
   begin
      --  If this is in the entry block and we're allocating one of an
      --  object, this is a simple variable.

      if Is_Entry_Block (V) and then Is_A_Constant_Int (Op)
        and then Equals_Int (Op, 1)
      then
         Set_Is_LHS (V);
         Maybe_Decl (V);

      --  Otherwise, it's of variable size and we have to call alloca and
      --  set V to our result.

      else
         declare
            Size : constant Str :=
              "sizeof (" & Get_Allocated_Type (V) & ") * " & (Op + Mult);
            Call : constant Str := "alloca (" & Size & ")" + Component;

         begin
            Assignment (V, "(" & Type_Of (V) & ") " & Call + Unary);
         end;
      end if;
   end Alloca_Instruction;

   ----------------------
   -- Load_Instruction --
   ----------------------

   procedure Load_Instruction (V, Op : Value_T) is
   begin
      --  ??? Need to deal with both unaligned load and unaligned store

      --  If V is unsigned but Op1 isn't (meaning that it's not a variable
      --  that's marked unsigned, so it may be an array or record
      --  reference), add a cast to the unsigned form.

      if Get_Is_Unsigned (V) and then not Get_Is_Unsigned (Op) then
         Assignment (V, "(unsigned " & Type_Of (V) & ") " & Deref (Op));
      else
         Assignment (V, Deref (Op));
      end if;
   end Load_Instruction;

   -----------------------
   -- Store_Instruction --
   -----------------------

   procedure Store_Instruction (V, Op1, Op2 : Value_T) is
      LHS : constant Str := Deref (Op2);
      RHS : constant Str := +Op1;

   begin
      Process_Pending_Values;
      Write_Copy (LHS, RHS, Type_Of (Op1), V => V);
   end Store_Instruction;

   ------------------------
   -- Binary_Instruction --
   ------------------------

   function Binary_Instruction (V, Op1, Op2 : Value_T) return Str is
      Opc : constant Opcode_T := Get_Opcode (V);
      T   : constant Type_T   := Type_Of (V);
      POO : constant Process_Operand_Option :=
        (case Opc is when Op_U_Div | Op_U_Rem | Op_L_Shr => POO_Unsigned,
                     when Op_S_Div | Op_S_Rem | Op_A_Shr => POO_Signed,
                     when others => X);
   begin
      case Opc is
         when Op_Add =>
            --  ??? J + -6 should be J - 6 and likewise for Op_Sub
            return TP ("#1 + #2", Op1, Op2) + Add;

         when Op_Sub =>
            return TP ("#1 - #2", Op1, Op2) + Add;

         when Op_Mul =>
            return TP ("#1 * #2", Op1, Op2) + Mult;

         when Op_S_Div | Op_U_Div =>
            return Process_Operand (Op1, POO) & " / " &
              Process_Operand (Op2, POO) + Mult;

         when Op_S_Rem | Op_U_Rem =>
            return Process_Operand (Op1, POO) & " % " &
              Process_Operand (Op2, POO) + Mult;

         when Op_Shl =>
            return TP ("#1 << #2", Op1, Op2) + Shift;

         when Op_L_Shr | Op_A_Shr =>
            return Process_Operand (Op1, POO) & " >> " & Op2 + Shift;

         when Op_F_Add =>
            return TP ("#1 + #2", Op1, Op2) + Add;

         when Op_F_Sub =>
            return TP ("#1 - #2", Op1, Op2) + Add;

         when Op_F_Mul =>
            return TP ("#1 * #2", Op1, Op2) + Mult;

         when Op_F_Div =>
            return TP ("#1 / #2", Op1, Op2) + Mult;

         when Op_And =>

            --  If this is a bit operation and neither has side-effects, use
            --  && because this is clearer and more efficient in C.

            if T = Bit_T and then not Has_Side_Effects (Op1)
              and then not Has_Side_Effects (Op2)
            then
               return TP ("#1 && #2", Op1, Op2) + Logical_AND;
            else
               return TP ("#1 & #2", Op1, Op2) + Bit;
            end if;

         when Op_Or =>
            if T = Bit_T and then not Has_Side_Effects (Op1)
              and then not Has_Side_Effects (Op2)
            then
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
        Process_Operand
        (Op, (case Opc is when Op_UI_To_FP | Op_Z_Ext => POO_Unsigned,
                          when Op_SI_To_FP | Op_S_Ext => POO_Signed,
                          when others                 => X));

   begin
      --  If we're doing a bitcast and the input and output types aren't
      --  both pointers, we need to do this by pointer-punning.

      if Opc = Op_Bit_Cast
        and then (not Is_Pointer_Type (Src_T)
                    or else not Is_Pointer_Type (Dest_T))
      then
         --  If our operand is an expression, we probably can't validly take
         --  its address, so be sure that we make an actual variable that
         --  we can take the address of.

         Force_To_Variable (Op);
         return TP ("*((#T2 *) #A1)", Op, V) + Unary;

      --  If we're zero-extending a value that's known to be a comparison
      --  result to an i8, we do nothing since we know that the value is
      --  already either a zero or one.

      elsif Opc = Op_Z_Ext and then Is_Comparison (Op)
        and then Dest_T = Byte_T
      then
         return +Op;

      --  Otherwise, just do a cast

      else
         return ("(" & (V + Write_Type) & ") " & Our_Op) + Unary;
      end if;

   end Cast_Instruction;

   ---------------------
   -- Cmp_Instruction --
   ---------------------

   function Cmp_Instruction (V, Op1, Op2 : Value_T) return Str is
      Result : Str;

   begin
      --  This is either an integer or an FP comparison

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
            POO         : constant Process_Operand_Option :=
              (if Do_Unsigned then POO_Unsigned else POO_Signed);
            LHS         : constant Str    := Process_Operand (Op1, POO);
            RHS         : constant Str    := Process_Operand (Op2, POO);

         begin
            return (LHS & " " & Info.Op (1 .. Info.Length) & " " & RHS) +
              Relation;
         end;

      --  If not integer comparison, it must be FP

      else
         case Get_F_Cmp_Predicate (V) is
            when Real_Predicate_True =>
               return +"1";
            when Real_Predicate_False =>
               return +"0";
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

            when Real_ORD =>

               --  This tests that neither input is a Nan, which means that
               --  both inputs are equal to themselves in C. We check if
               --  Op2 is a constant since it often is.

               Result := TP ("#1 == #1", Op1) + Relation;
               if not Is_A_Constant (Op2) then
                  Result :=
                    (Result & " && " & (TP ("#1 == #1", Op2) + Relation))
                    + Logical_AND;
               end if;

               return Result;

            when Real_UNO =>

               --  This is the opposite of ORD

               Result := TP ("#1 != #1", Op1) + Relation;
               if not Is_A_Constant (Op2) then
                  Result :=
                    (Result & " || " & (TP ("#1 != #1", Op2) + Relation))
                    + Logical_OR;
               end if;

               return Result;
         end case;
      end if;
   end Cmp_Instruction;

   ----------------
   -- Write_Copy --
   ----------------

   procedure Write_Copy (LHS : Value_T; RHS : Str; T : Type_T) is
   begin
      Write_Copy (+LHS, RHS, T, V => LHS);
   end Write_Copy;

   ----------------
   -- Write_Copy --
   ----------------

   procedure Write_Copy (LHS : Str; RHS : Value_T; T : Type_T) is
   begin
      Write_Copy (LHS, +RHS, T, V => RHS);
   end Write_Copy;

   ----------------
   -- Write_Copy --
   ----------------

   procedure Write_Copy (LHS, RHS : Value_T; T : Type_T) is
   begin
      Write_Copy (+LHS, +RHS, T, V => LHS);
   end Write_Copy;

   ----------------
   -- Write_Copy --
   ----------------

   procedure Write_Copy (LHS, RHS : Str; T : Type_T; V : Value_T := No_Value_T)
   is
   begin
      --  If this isn't an array type, write a normal assignment. Otherwise,
      --  use memmove.
      --  ??? We can usually use memcpy, but it's not clear what test to
      --  do here at the moment.

      if Get_Type_Kind (T) /= Array_Type_Kind then
         Add_Line (LHS & " = " & RHS + Assign, V);
      else
         --  If T is a zero-sized array, it means that we're not to move
         --  anything, but we make a one-element array for zero-length
         --  arrays, so taking sizeof the type is wrong.

         if Get_Array_Length (T) /= Nat (0) then
            Add_Line ("memmove ((void *) " & (Addr_Of (LHS) + Comma) &
                        ", (void *) " & (Addr_Of (RHS) + Comma) &
                        ", sizeof (" & T & "))", V);
         end if;
      end if;
   end Write_Copy;

   -----------------------
   -- Force_To_Variable --
   -----------------------

   procedure Force_To_Variable (V : Value_T) is
      C_Val : Str := Get_C_Value (V);

   begin
      if Present (C_Val) then

         --  We have to undo what was done to show that we don't need a
         --  variable for Op. Specifically, we have to clear its value,
         --  declare it, and copy the value to it.

         Set_C_Value (V, No_Str);

         --  If V is a LHS, it means that we're presenting the value as if it
         --  was the address. So take the address and clear the flag.

         if Get_Is_LHS (V) then
            C_Val := Addr_Of (C_Val);
            Set_Is_LHS (V, False);
         end if;

         --  Declare the variable and write the copy into it

         Maybe_Decl  (V);
         Write_Copy  (V, C_Val, Type_Of (V));
      end if;
   end Force_To_Variable;

   ----------------
   -- Assignment --
   ----------------

   procedure Assignment
     (LHS : Value_T; RHS : Str; Is_Opencode_Builtin : Boolean := False)
   is
   begin
      --  If LHS has no uses, see if it has side-effects. If so, force it
      --  out so that we evaluate the side-effects. If not, do nothing.

      if Num_Uses (LHS) = 0 then
         if Has_Side_Effects (LHS) then
            Maybe_Decl (LHS);
            Write_Copy (LHS, RHS, Type_Of (LHS));
         else
            return;
         end if;

      --  If LHS is a LHS, has more than one use in the IR, if we've
      --  already emitted a decl for it (e.g., it was defined in a block we
      --  haven't processed yet), if it's a source-level variable, or if
      --  it's a function call that returns an aggregate type (because we
      --  may try to take the address of it), generate an assignment
      --  statement into LHS. Otherwise, mark LHS as having value RHS. If
      --  LHS is a constant expression or of array types, never generate an
      --  assignment statement, the former because we may be at top level
      --  and the latter because C doesn't allow assignments of objects of
      --  aggregate type.
      --
      --  ??? We'd like to avoid the copy if we have a load of a variable
      --  that's used more than once, but that causes problems if it gets
      --  deferred until after a store into that variable. The handling of
      --  pending values can't help us here because it assumes  that all
      --  deferred values are used only once. It may be worth trying to do
      --  something better here because it generates a lot of extra variables
      --  in optimized code.

      elsif (Get_Is_LHS (LHS) or else Num_Uses (LHS) > 1
            or else Get_Is_Variable (LHS) or else Get_Is_Decl_Output (LHS)
            or else (Is_A_Call_Inst (LHS) and then Is_Aggregate_Type (LHS)))
        and then not Is_A_Constant_Expr (LHS)
        and then Get_Type_Kind (Type_Of (LHS)) /= Array_Type_Kind
      then
         Maybe_Decl (LHS);
         Write_Copy (LHS, RHS, Type_Of (LHS));
      else
         --  Make a note of the value of V. If V is an instruction, that
         --  has a potential side effect-such as call or load, make a
         --  note of this pending assignment in case we get a store or
         --  call.

         Set_C_Value (LHS, RHS);
         if Is_A_Instruction (LHS)
           and then (Num_Uses (LHS) > 0
                     or else (Is_A_Call_Inst (LHS) and not Is_Opencode_Builtin)
                     or else Is_A_Load_Inst (LHS))
         then
            Pending_Values.Append (LHS);
         end if;
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
      --  First, make sure we've declared all operands

      for Op of Ops loop
         Maybe_Decl (Op);
      end loop;

      --  Handle the instruction according to its opcode

      case Opc is
         when Op_Call =>
            Call_Instruction (V, Ops);

         when Op_Alloca =>
            Alloca_Instruction (V, Op1);

         when Op_Load =>
            Load_Instruction (V, Op1);

         when Op_Store =>
            Store_Instruction (V, Op1, Op2);

         when Op_I_Cmp | Op_F_Cmp =>
            Assignment (V, Cmp_Instruction (V, Op1, Op2));

         when Op_Select =>
            Assignment (V, TP ("#1 ? #2 : #3", Op1, Op2, Op3) + Conditional);

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

         when Op_Unreachable =>
            null;

         when others =>
            Error_Msg ("unsupported instruction: " & Get_Opcode_Name (Opc));
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
