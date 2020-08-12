------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2013-2020, AdaCore                     --
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

with CCG.Tables;      use CCG.Tables;
with CCG.Subprograms; use CCG.Subprograms;

package body CCG.Instructions is

   function Num_Uses (V : Value_T) return Nat
     with Pre => Present (V), Post => Num_Uses'Result >= 1;
   --  Returns the number of uses of V

   procedure Assignment (LHS : Value_T; RHS : Str)
     with Pre => Present (LHS) and then Present (RHS);
   --  Take action to assign LHS the value RHS

   function Cmp_Instruction (V, Op1, Op2 : Value_T) return Str
     with Pre  => Get_Instruction_Opcode (V) in Op_I_Cmp | Op_F_Cmp
                  and then Present (Op1) and then Present (Op2),
          Post => Present (Cmp_Instruction'Result);
   --  Return the value corresponding to a comparison instruction

   --------------
   -- Num_Uses --
   --------------

   function Num_Uses (V : Value_T) return Nat is
      V_Use : Use_T := Get_First_Use (V);

   begin
      return J : Nat := 0 do
         while Present (V_Use) loop
            J := J + 1;
            V_Use := Get_Next_Use (V_Use);
         end loop;
      end return;
   end Num_Uses;

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
            LHS      : Str := +Op1;
            RHS      : Str := +Op2;
            Info     : constant I_Info := Int_Info (Get_I_Cmp_Predicate (V));

         begin
            if Info.Is_Unsigned then
               LHS := " (unsigned) " & LHS;
               RHS := " (unsigned) " & RHS;
            end if;

            return LHS & " " & Info.Op (1 .. Info.Length) & " " & RHS;
         end;

      elsif Get_Instruction_Opcode (V) = Op_F_Cmp then

         case Get_F_Cmp_Predicate (V) is
            when Real_OEQ | Real_UEQ =>
               return Op1 & " == " & Op2;
            when Real_OGT | Real_UGT =>
               return Op1 & " > " & Op2;
            when Real_OGE | Real_UGE =>
               return Op1 & " >= " & Op2;
            when Real_OLT | Real_ULT =>
               return Op1 & " < " & Op2;
            when Real_OLE | Real_ULE =>
               return Op1 & " > " & Op2;
            when Real_ONE | Real_UNE =>
               return Op1 & " != " & Op2;
            when others =>
               null;
         end case;
      end if;

      return +"<unsupported comparison>";

   end Cmp_Instruction;

   ----------------
   -- Assignment --
   ----------------

   procedure Assignment (LHS : Value_T; RHS : Str) is
   begin
      --  If LHS is an entry alloca or has more than one use in the IR,
      --  generate an assignment statement into LHS. Otherwise, mark LHS
      --  as having value RHS.

      if Get_Is_Entry_Alloca (LHS) or else Num_Uses (LHS) > 1 then
         Output_Stmt (LHS & " = " & RHS);
      else
         Set_C_Value (LHS, RHS);
      end if;
   end Assignment;

   -------------------------
   --  Output_Instruction --
   -------------------------

   procedure Output_Instruction (V : Value_T; Ops : Value_Array) is
      Op1 : constant Value_T :=
        (if Ops'Length >= 1 then Ops (1) else No_Value_T);
      Op2 : constant Value_T :=
        (if Ops'Length >= 2 then Ops (2) else No_Value_T);
      Op3 : constant Value_T :=
        (if Ops'Length >= 3 then Ops (3) else No_Value_T);
   begin
      case Get_Instruction_Opcode (V) is

         when Op_Ret =>
            if Present (Op1) then
               Output_Stmt ("return " & Op1);
            else
               Output_Stmt ("return");
            end if;

         when Op_Alloca =>
            if Get_Is_Entry (Get_Instruction_Parent (V)) then
               Set_Is_Entry_Alloca (V);
            else
               Output_Stmt ("<unsupported instruction>");
            end if;

         when Op_Load =>
            if Get_Is_Entry_Alloca (Op1) then
               Assignment (V, To_Data (Op1));
            else
               Output_Stmt (V & " = *" & Op1);
            end if;

         when Op_Store =>
            if Get_Is_Entry_Alloca (Op2) then
               Output_Stmt (To_Data (Op2) & " = " & Op1);
            else
               Output_Stmt ("*" & Op2 & " = *" & Op1);
            end if;

         when Op_I_Cmp | Op_F_Cmp =>
            Assignment (V, Cmp_Instruction (V, Op1, Op2));

         when Op_Br =>
            if Ops'Length = 1 then
               Output_Stmt ("goto " & Value_As_Basic_Block (Op1));
            else
               Output_Stmt ("if (" & Op1 & " goto " &
                              Value_As_Basic_Block (Op3) &
                              "; else goto " & Value_As_Basic_Block (Op2));
            end if;

         when Op_Add =>
            Assignment (V, Op1 & " + " & Op2);

         when Op_Sub =>
            Assignment (V, Op1 & " - " & Op2);

         when Op_Mul =>
            Assignment (V, Op1 & " * " & Op2);

         when Op_F_Add =>
            Assignment (V, Op1 & " + " & Op2);

         when Op_F_Sub =>
            Assignment (V, Op1 & " - " & Op2);

         when Op_F_Mul =>
            Assignment (V, Op1 & " * " & Op2);

         when Op_F_Div =>
            Assignment (V, Op1 & " / " & Op2);

         when Op_And =>
            Assignment (V, Op1 & " & " & Op2);

         when Op_Or =>
            Assignment (V, Op1 & " | " & Op2);

         when Op_F_Neg =>
            Assignment (V, " -" & Op1);

         when others =>
            Output_Stmt ("<unsupported instruction>");

      end case;
   end Output_Instruction;

end CCG.Instructions;
