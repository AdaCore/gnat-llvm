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
               Assignment (V, To_Str_As_Data (Op1));
            else
               Output_Stmt (V & " = *" & Op1);
            end if;

         when Op_Store =>
            if Get_Is_Entry_Alloca (Op2) then
               Output_Stmt (To_Str_As_Data (Op2) & " = " & Op1);
            else
               Output_Stmt ("*" & Op2 & " = *" & Op1);
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
