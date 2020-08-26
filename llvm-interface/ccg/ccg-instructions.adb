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

with Interfaces.C; use Interfaces.C;

with LLVM.Core; use LLVM.Core;

with CCG.Aggregates;  use CCG.Aggregates;
with CCG.Output;      use CCG.Output;
with CCG.Subprograms; use CCG.Subprograms;
with CCG.Tables;      use CCG.Tables;
with CCG.Utils;       use CCG.Utils;

package body CCG.Instructions is

   procedure Assignment (LHS : Value_T; RHS : Str)
     with Pre => Present (LHS) and then Present (RHS);
   --  Take action to assign LHS the value RHS

   function Binary_Instruction (V, Op1, Op2 : Value_T) return Str
     with Pre  => Is_A_Instruction (V) and then Present (Op1)
                  and then Present (Op2),
          Post => Present (Binary_Instruction'Result);
   --  Return the value corresponding to a binary instruction

   function Cast_Instruction (V, Op : Value_T) return Str
     with Pre  => Is_A_Instruction (V) and then Present (Op),
          Post => Present (Cast_Instruction'Result);
   --  Return the value corresponding to a cast instruction

   function Cmp_Instruction (V, Op1, Op2 : Value_T) return Str
     with Pre  => Get_Instruction_Opcode (V) in Op_I_Cmp | Op_F_Cmp
                  and then Present (Op1) and then Present (Op2),
          Post => Present (Cmp_Instruction'Result);
   --  Return the value corresponding to a comparison instruction

   procedure Call_Instruction (V : Value_T; Ops : Value_Array)
     with Pre => Present (V);
   --  Process a call instruction

   procedure Maybe_Decl (V : Value_T)
     with Pre => Present (V), Post => Get_Is_Decl_Output (V);
   --  See if we need to write a declaration for V and write one if so

   function Maybe_Unsigned
     (V : Value_T; Is_Unsigned : Boolean := True) return Str
   is
     ((if   Is_Unsigned then TP ("(#T) #1", V, T => Type_Of (V),
                                 Is_Unsigned => True)
       else +V))
     with Pre => Present (V), Post => Present (Maybe_Unsigned'Result);
   --  Return V if it's not unsigned and return a cast to unsigned if it is.

   ----------------
   -- Maybe_Decl --
   ----------------

   procedure Maybe_Decl (V : Value_T) is
   begin
      if not Get_Is_Decl_Output (V) then
         Write_Decl (V);
      end if;
   end Maybe_Decl;

   ------------------------
   -- Binary_Instruction --
   ------------------------

   function Binary_Instruction (V, Op1, Op2 : Value_T) return Str is
      Opc : constant Opcode_T := Get_Instruction_Opcode (V);

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
            return TP ("#1 & #2", Op1, Op2) + Logic;

         when Op_Or =>
            return TP ("#1 | #2", Op1, Op2) + Logic;

         when Op_Xor =>
            return TP ("#1 ^ #2", Op1, Op2) + Logic;

         when others =>
            pragma Assert (False);
            return No_Str;

      end case;
   end Binary_Instruction;

   ----------------------
   -- Cast_Instruction --
   ----------------------

   function Cast_Instruction (V, Op : Value_T) return Str is
      Opc    : constant Opcode_T := Get_Instruction_Opcode (V);
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
         return TP ("*((#) &#1", Op, T => Pointer_Type (Dest_T, 0)) + Unary;
      else
         return ("(" & Dest_T & ") " & Our_Op) + Unary;
      end if;

   end Cast_Instruction;

   ---------------------
   -- Cmp_Instruction --
   ---------------------

   function Cmp_Instruction (V, Op1, Op2 : Value_T) return Str is

   begin
      if Get_Instruction_Opcode (V) = Op_I_Cmp then
         declare
            type I_Info is record
               Is_Unsigned : Boolean;
               Length      : Integer;
               Op          : String (1 .. 2);
            end record;
            type I_Info_Array is array (Int_Predicate_T range <>) of I_Info;
            Int_Info : constant I_Info_Array :=
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
            Info     : constant I_Info := Int_Info (Get_I_Cmp_Predicate (V));
            LHS      : constant Str    :=
              Maybe_Unsigned (Op1, Info.Is_Unsigned);
            RHS      : constant Str    :=
              Maybe_Unsigned (Op2, Info.Is_Unsigned);

         begin
            return (LHS & " " & Info.Op (1 .. Info.Length) & " " & RHS) +
              Relation;
         end;

      elsif Get_Instruction_Opcode (V) = Op_F_Cmp then

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

      return +"<unsupported comparison>";

   end Cmp_Instruction;

   ----------------------
   -- Call_Instruction --
   ----------------------

   procedure Call_Instruction (V : Value_T; Ops : Value_Array) is
      Func  : constant Value_T := Ops (Ops'Last);
      Call  : Str              := Func & " (";
      First : Boolean          := True;

   begin
      --  Generate the argument list for the call

      for Op of Ops (Ops'First .. Ops'Last - 1) loop
         if First then
            Call  := Call & Op;
            First := False;
         else
            Call := Call & ", " & Op;
         end if;
      end loop;

      --  Add the final close paren. If this is a procedure call,
      --  output it. Otherwise, set this as the value of V.

      Call := (Call & ")") + Assign;
      if Get_Type_Kind (Type_Of (V)) = Void_Type_Kind then
         Output_Stmt (Call);
      else
         Assignment (V, Call);
      end if;
   end Call_Instruction;

   ----------------
   -- Assignment --
   ----------------

   procedure Assignment (LHS : Value_T; RHS : Str) is
   begin
      --  If LHS is an entry alloca or has more than one use in the IR,
      --  generate an assignment statement into LHS. Otherwise, mark LHS
      --  as having value RHS.

      if Get_Is_Variable (LHS) or else Num_Uses (LHS) > 1 then
         Output_Stmt (LHS & " = " & RHS);
      else
         Set_C_Value (LHS, RHS);
      end if;
   end Assignment;

   ------------------
   --  Instruction --
   ------------------

   procedure Instruction (V : Value_T; Ops : Value_Array) is
      Op1 : constant Value_T  :=
        (if Ops'Length >= 1 then Ops (1) else No_Value_T);
      Op2 : constant Value_T  :=
        (if Ops'Length >= 2 then Ops (2) else No_Value_T);
      Op3 : constant Value_T  :=
        (if Ops'Length >= 3 then Ops (3) else No_Value_T);
      Opc : constant Opcode_T := Get_Instruction_Opcode (V);
   begin
      --  See if we need to write a declaration for an operand

      for Op of Ops loop
         Maybe_Decl (Op);
      end loop;

      --  Handle the instruction according to its opcode

      case Opc is

         when Op_Ret =>
            if Present (Op1) then
               Output_Stmt ("return " + Assign & Op1);
            else
               Output_Stmt ("return");
            end if;

         when Op_Call =>
            Call_Instruction (V, Ops);

         when Op_Alloca =>
            if Get_Is_Entry (Get_Instruction_Parent (V)) then
               Set_Is_Variable (V);
               Write_Decl (V);
            else
               Output_Stmt ("<unsupported instruction>");
            end if;

         when Op_Load =>
            Assignment
              (V, TP ((if Get_Is_Variable (Op1) then "#D1" else "*#1"), Op1) +
                  Unary);

         when Op_Store =>
            Output_Stmt (TP ((if   Get_Is_Variable (Op2) then "#D2 = #1"
                              else "*#2 = #1"),
                                Op1, Op2) +
                         Assign);

         when Op_I_Cmp | Op_F_Cmp =>
            Assignment (V, Cmp_Instruction (V, Op1, Op2));

         when Op_Br =>
            if Ops'Length = 1 then
               Output_Stmt (TP ("goto #B1", Op1));
            else
               Output_Stmt (TP ("if (#1) goto #B3; else goto #B2",
                               Op1, Op2, Op3));
            end if;

         when Op_Add | Op_Sub | Op_Mul | Op_S_Div | Op_U_Div | Op_S_Rem
            | Op_U_Rem | Op_Shl | Op_L_Shr | Op_A_Shr | Op_F_Add | Op_F_Sub
            | Op_F_Mul | Op_F_Div | Op_And | Op_Or | Op_Xor =>
            Assignment (V, Binary_Instruction (V, Op1, Op2));

         when Op_F_Neg =>
            Assignment (V, TP (" -#1", Op1) + Unary);

         when Op_Trunc | Op_SI_To_FP | Op_FP_Trunc | Op_FP_Ext | Op_S_Ext
            | Op_UI_To_FP | Op_Z_Ext | Op_Bit_Cast =>
            Assignment (V, Cast_Instruction (V, Op1));

         when Op_Extract_Value =>
            Assignment (V, Extract_Value_Instruction (V, Op1));

         when Op_Insert_Value =>
            Insert_Value_Instruction (V, Op1, Op2);

         when others =>
            Output_Stmt ("<unsupported instruction>");

      end case;
   end Instruction;

end CCG.Instructions;
