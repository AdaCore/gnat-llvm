------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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

with CCG.Environment; use CCG.Environment;
with CCG.Helper;      use CCG.Helper;
with CCG.Strs;        use CCG.Strs;

package CCG.Instructions is

   procedure Force_To_Variable (V : Value_T)
     with Pre  => Present (V), Post => No (Get_C_Value (V));
   --  If V has an expression for it, declare V as a variable and copy the
   --  expression into it.

   procedure Assignment
     (LHS : Value_T; RHS : Str; Is_Opencode_Builtin : Boolean := False)
     with Pre => Present (LHS) and then Present (RHS);
   --  Take action to assign LHS the value RHS. If Is_Builtin is True,
   --  this is a call instruction that we've rewritten as code, so
   --  no call is involved.

   procedure Instruction (V : Value_T; Ops : Value_Array)
     with Pre => Acts_As_Instruction (V);
   --  Output the instruction V with operands Ops

   procedure Process_Instruction (V : Value_T)
     with Pre => Acts_As_Instruction (V);
   --  Process instruction V

   type Process_Operand_Option is (POO_Signed, POO_Unsigned, X);
   --  An operand to Process_Operand that says whether we care which
   --  signedless the operand is and, if so, which one.

   function Process_Operand
     (V : Value_T; POO : Process_Operand_Option; P : Precedence) return Str
     with Pre => Present (V), Post => Present (Process_Operand'Result);
   --  Called when we care about any high bits in a possible partial-word
   --  operand and possibly about signedness. We return the way to
   --  reference V. If nothing is special, this is just +V + P.

   procedure Output_Copy
     (LHS, RHS : Str; T : Type_T; V : Value_T := No_Value_T)
     with Pre => Present (LHS) and then Present (RHS) and then Present (T);
   procedure Output_Copy (LHS : Str; RHS : Value_T; T : Type_T)
     with Pre => Present (LHS) and then Present (RHS) and then Present (T);
   procedure Output_Copy (LHS, RHS : Value_T; T : Type_T)
     with Pre => Present (LHS) and then Present (RHS) and then Present (T);
   procedure Output_Copy (LHS : Value_T; RHS : Str; T : Type_T)
     with Pre => Present (LHS) and then Present (RHS) and then Present (T);
   --  Write a statement to copy RHS, of type T, to LHS. If V is Present,
   --  it represents something that may give line/file information.

   procedure Process_Pending_Values (Calls_Only : Boolean := False);
   --  Walk the set of pending values in reverse order and generate
   --  assignments for any that haven't been written yet. Is Call_Only,
   --  we only want to process pending calls (this is used when seeing a
   --  load).

   procedure Clear_Pending_Values with Inline;
   --  Clear any pending values that remain in the table. We do this after
   --  we've processed all of them and at the end of a subprogram.  In the
   --  latter case, they're dead, but we don't want them to be output as
   --  part of another subprogram.

   function Create_Annotation (N : N_Pragma_Id) return Nat;
   --  Return the value to eventually pass to Output_Annotation to perform
   --  the operation designated by the pragma N if there is one to perform.
   --  Otherwise, return 0.

   procedure Output_Annotation (J : Nat; V : Value_T; Is_Global : Boolean);
   --  Output the annotation we recorded as J (the return of the previous
   --  function) in instruction V. If Is_Global, this is at file level.

end CCG.Instructions;
