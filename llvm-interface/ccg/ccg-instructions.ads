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

with CCG.Helper; use CCG.Helper;
with CCG.Tables; use CCG.Tables;

package CCG.Instructions is

   procedure Assignment (LHS : Value_T; RHS : Str)
     with Pre => Present (LHS) and then Present (RHS);
   --  Take action to assign LHS the value RHS

   procedure Instruction (V : Value_T; Ops : Value_Array)
     with Pre => Acts_As_Instruction (V);
   --  Output the instruction V with operands Ops

   procedure Process_Instruction (V : Value_T)
     with Pre => Acts_As_Instruction (V);
   --  Process instruction V

   procedure Write_Copy (LHS, RHS : Str; T : Type_T)
     with Pre => Present (LHS) and then Present (RHS) and then Present (T);
   procedure Write_Copy (LHS : Str; RHS : Value_T; T : Type_T)
     with Pre => Present (LHS) and then Present (RHS) and then Present (T);
   procedure Write_Copy (LHS, RHS : Value_T; T : Type_T)
     with Pre => Present (LHS) and then Present (RHS) and then Present (T);
   procedure Write_Copy (LHS : Value_T; RHS : Str; T : Type_T)
     with Pre => Present (LHS) and then Present (RHS) and then Present (T);
   --  Write a statement to copy RHS, of type T, to LHS

   procedure Clear_Pending_Values;
   --  Called when switching to a new basic block to the pending unwritten
   --  value table (thre shouldn't be any active entries in it unless
   --  there are unused values in the block).

   procedure Process_Pending_Values;
   --  Walk the set of pending values in reverse order and generate
   --  assignments for any that haven't been written yet.

end CCG.Instructions;
