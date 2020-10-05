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

with CCG.Instructions; use CCG.Instructions;
with CCG.Output;       use CCG.Output;
with CCG.Subprograms;  use CCG.Subprograms;
with CCG.Tables;       use CCG.Tables;
with CCG.Utils;        use CCG.Utils;

package body CCG.Blocks is

   function Is_Stub_Block (BB : Basic_Block_T) return Boolean
     with Pre => Present (BB);
   --  Return True iff BB is a block containing just an unconditional branch
   --  possibly proceeded by one or more phi instructions. We'll never branch
   --  to such a block.

   -------------------
   -- Is_Stub_Block --
   -------------------

   function Is_Stub_Block (BB : Basic_Block_T) return Boolean is
      Inst : Value_T := Get_First_Instruction (BB);
   begin
      while Present (Inst)
        and then (Get_Opcode (Inst) = Op_PHI
                    or else (Get_Opcode (Inst) = Op_Br
                               and then Nat'(Get_Num_Operands (Inst)) = 1))
      loop
         Inst := Get_Next_Instruction (Inst);
      end loop;

      return No (Inst);
   end Is_Stub_Block;

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
      --  If we already processed this basic block, do nothing

      if Get_Was_Output (BB) then
         return;

      --  If this is a stub block and not the entry block, we don't
      --  need to process it, but do need to process what it jumps to.

      elsif Is_Stub_Block (BB) and then not Is_Entry_Block (BB) then
         Output_BB (Get_Operand (Terminator, Nat (0)));
         return;

      --  Otherwise, if this isn't the entry block, output a label for it

      elsif not Is_Entry_Block (BB) then
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

         when Op_Switch =>

            --  We have pairs of operands. The first pair is the value to
            --  test and the default destination followed by pairs of values
            --  and destinations. All odd numbered operands are destinations.

            for J in Nat range 0 .. Get_Num_Operands (Terminator) / 2 - 1 loop
               Output_BB (Get_Operand (Terminator, J * 2 + 1));
            end loop;

         when others =>
            Output_Stmt
              (+("<unsupported terminator: " &
                   Get_Opcode_Name (Terminator) & ">"));

      end case;

   end Output_BB;

   -------------------
   -- Output_Branch --
   -------------------

   procedure Output_Branch
     (From       : Value_T;
      To         : Value_T;
      Need_Block : Boolean := False;
      Had_Phi    : Boolean := False) is
   begin
      Output_Branch (From, Value_As_Basic_Block (To), Need_Block, Had_Phi);
   end Output_Branch;

   -------------------
   -- Output_Branch --
   -------------------

   procedure Output_Branch
     (From       : Value_T;
      To         : Basic_Block_T;
      Need_Block : Boolean := False;
      Had_Phi    : Boolean := False)
   is
      Our_BB       : constant Basic_Block_T := Get_Instruction_Parent (From);
      Our_Had_Phi  : Boolean                := Had_Phi;
      Target_I     : Value_T                := Get_First_Instruction (To);

   begin
      --  Scan the start of the target block looking for Phi instructions

      while Present (Target_I) and then Get_Opcode (Target_I) = Op_PHI loop
         declare
            Phi_Val : Value_T := No_Value_T;

         begin
            --  If we find a phi, we must ensure we have a declaration of its
            --  type and then copy the appropriate data into it.

            Maybe_Decl (Target_I);
            for J in 0 .. Count_Incoming (Target_I) loop
               if Get_Incoming_Block (Target_I, J) = Our_BB then
                  Phi_Val := Get_Operand (Target_I, J);
               end if;
            end loop;

            if not Our_Had_Phi and then Need_Block then
               Output_Stmt ("{", Semicolon => False);
            end if;

            Write_Copy (+Target_I, +Phi_Val, Type_Of (Phi_Val));
            Our_Had_Phi := True;
            Target_I    := Get_Next_Instruction (Target_I);
         end;
      end loop;

      --  If we've hit an unconditional branch instruction, branch there

      if Present (Target_I) and then Get_Opcode (Target_I) = Op_Br
        and then Nat'(Get_Num_Operands (Target_I)) = 1
      then
         Output_Branch (Target_I, Get_Operand (Target_I, Nat (0)), Need_Block,
                        Our_Had_Phi);
      else
         --  Otherwise, write the goto and, if we had a phi, close the
         --  block we opened.

         Output_Stmt ("goto " & To);
         if Our_Had_Phi and then Need_Block then
            Output_Stmt ("}", Semicolon => False);
         end if;
      end if;
   end Output_Branch;

   ------------------------
   -- Switch_Instruction --
   ------------------------

   procedure Switch_Instruction (V : Value_T; Ops : Value_Array) is
      Val     : constant Value_T := Ops (Ops'First);
      Default : constant Basic_Block_T :=
        Value_As_Basic_Block (Ops (Ops'First + 1));

   begin
      --  Write out the initial part of the switch, which is the switch
      --  statement and the default option.

      Output_Stmt (TP ("switch (#1) {", Val) + Assign, Semicolon => False);
      Output_Stmt ("default:", Semicolon => False);
      Output_Branch (V, Default);

      --  Now handle each case. They start after the first two operands and
      --  alternate between value and branch target.

      for J in 1 .. Nat ((Ops'Length / 2) - 1) loop
         Output_Stmt (TP ("case #1:", Ops (Ops'First + J * 2)),
                      Semicolon => False);
         Output_Branch (V, Ops (Ops'First + J * 2 + 1));
      end loop;

      Output_Stmt ("}", Semicolon => False);
   end Switch_Instruction;

end CCG.Blocks;
