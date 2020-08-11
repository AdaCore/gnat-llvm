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
               Output_Stmt (V & " = " & To_Str_As_Data (Op1));
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
            Output_Stmt (V & " = " & Op1 & " + " & Op2);

         when Op_Sub =>
            Output_Stmt (V & " = " & Op1 & " - " & Op2);

         when Op_Mul =>
            Output_Stmt (V & " = " & Op1 & " * " & Op2);

         when Op_F_Add =>
            Output_Stmt (V & " = " & Op1 & " + " & Op2);

         when Op_F_Sub =>
            Output_Stmt (V & " = " & Op1 & " - " & Op2);

         when Op_F_Mul =>
            Output_Stmt (V & " = " & Op1 & " * " & Op2);

         when Op_F_Div =>
            Output_Stmt (V & " = " & Op1 & " / " & Op2);

         when Op_And =>
            Output_Stmt (V & " = " & Op1 & " & " & Op2);

         when Op_Or =>
            Output_Stmt (V & " = " & Op1 & " | " & Op2);

         when Op_F_Neg =>
            Output_Stmt (V & " = -" & Op1);

         when others =>
            Output_Stmt ("<unsupported instruction>");

      end case;
   end Output_Instruction;

end CCG.Instructions;
