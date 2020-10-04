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

with LLVM.Core;  use LLVM.Core;

with CCG.Instructions; use CCG.Instructions;
with CCG.Subprograms;  use CCG.Subprograms;
with CCG.Tables;       use CCG.Tables;

package body CCG.Blocks is

   ---------------
   -- Output_BB --
   ---------------

   procedure Output_BB (V : Value_T) is
   begin
      Output_BB (Value_As_Basic_Block (V));
   end Output_BB;

   ---------------
   -- Output_BB --
   ---------------

   procedure Output_BB (BB : Basic_Block_T) is
      V          : Value_T          := Get_First_Instruction (BB);
      Terminator : constant Value_T := Get_Basic_Block_Terminator (BB);

   begin
      --  If we already processed this basic block, mark that we did

      if Get_Was_Output (BB) then
         return;
      end if;

      --  Otherwise, if this isn't the entry block, output a label for it

      if not Get_Is_Entry (BB) then
         Output_Stmt (BB & ":", Semicolon => False);
      end if;

      --  Mark that we're outputing this block and process each
      --  instruction it.

      Set_Was_Output (BB);
      while Present (V) loop
         Process_Instruction (V);
         V := Get_Next_Instruction (V);
      end loop;

      --  Now process any block referenced by the terminator

      case Get_Instruction_Opcode (Terminator) is
         when Op_Ret | Op_Unreachable =>
            null;

         when Op_Br =>
            if Get_Num_Operands (Terminator) = Nat (1) then
               Output_BB (Get_Operand (Terminator, Nat (0)));
            else
               Output_BB (Get_Operand (Terminator, Nat (2)));
               Output_BB (Get_Operand (Terminator, Nat (1)));
            end if;

         when others =>
            Output_Stmt
              (+("<unsupported terminator: " &
                   Get_Opcode_Name (Terminator) & ">"));

      end case;

   end Output_BB;

   -------------------
   -- Output_Branch --
   -------------------

   procedure Output_Branch (From, To : Value_T) is
   begin
      Output_Branch (From, Value_As_Basic_Block (To));
   end Output_Branch;

   -------------------
   -- Output_Branch --
   -------------------

   procedure Output_Branch (From : Value_T; To : Basic_Block_T) is
      pragma Unreferenced (From);
   begin
      Output_Stmt ("goto " & To);
   end Output_Branch;

end CCG.Blocks;
