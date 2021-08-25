------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with Output; use Output;
with Table;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with CCG.Environment;  use CCG.Environment;
with CCG.Instructions; use CCG.Instructions;
with CCG.Output;       use CCG.Output;
with CCG.Subprograms;  use CCG.Subprograms;
with CCG.Target;       use CCG.Target;
with CCG.Utils;        use CCG.Utils;

package body CCG.Blocks is

   --  Tables for global and local decls and statements

   package Global_Decls is new Table.Table
     (Table_Component_Type => Out_Line,
      Table_Index_Type     => Global_Decl_Idx,
      Table_Low_Bound      => Global_Decl_Idx_Start,
      Table_Initial        => 500,
      Table_Increment      => 100,
      Table_Name           => "Global_Decls");

   package Local_Decls is new Table.Table
     (Table_Component_Type => Out_Line,
      Table_Index_Type     => Local_Decl_Idx,
      Table_Low_Bound      => Local_Decl_Idx_Low_Bound,
      Table_Initial        => 500,
      Table_Increment      => 100,
      Table_Name           => "Local_Decls");

   package Stmts is new Table.Table
     (Table_Component_Type => Out_Line,
      Table_Index_Type     => Stmt_Idx,
      Table_Low_Bound      => Stmt_Idx_Low_Bound,
      Table_Initial        => 1000,
      Table_Increment      => 1000,
      Table_Name           => "Stmts");

   Current_BB : Basic_Block_T := No_BB_T;
   --  The basic block for which we're outputting statements

   function Is_Return_Phi (V : Value_T) return Boolean
     with Pre => Is_APHI_Node (V);
   --  Return True if V is a Phi instruction that's only used in a Return
   --  instruction.

   function Effective_Dest (V : Value_T) return Basic_Block_T
     with Pre => Is_A_Basic_Block (V);
   --  Returns the location to which we'll be generating a branch, if any,
   --  when we see a branch to V, a basic block. Normally, this is that
   --  block, but if the block consists just of Phi's and an unconditional
   --  branch, we follow that branch. If the first Phi's only use is a
   --  return instruction, we don't go anywhere.

   -------------------
   -- Is_Return_Phi --
   -------------------

   function Is_Return_Phi (V : Value_T) return Boolean is
      Single_User : constant Value_T := Get_Single_User (V);

   begin
      --  The optimizer sometimes creates a Phi just to merge returns.
      --  When generating C, we want to undo that and prefer to generate
      --  the return. So check for a Phi that's just used once and for a
      --  return. For simplicity, do this only if it's the first Phi (which
      --  it should be) and don't do this for array types, since they can't
      --  be directly returned in C.

      return Present (Single_User) and then Get_Opcode (Single_User) = Op_Ret
        and then Get_Type_Kind (V) /= Array_Type_Kind;
   end Is_Return_Phi;

   --------------------
   -- Effective_Dest --
   --------------------

   function Effective_Dest (V : Value_T) return Basic_Block_T is
      BB   : constant Basic_Block_T := Value_As_Basic_Block (V);
      Inst : Value_T                := Get_First_Instruction (BB);

   begin
      --  If this is an empty block (shouldn't happen), we return that as
      --  the destination to avoid propagating errors.

      if No (Inst) then
         return BB;

      --  Next handle the return case

      elsif Is_APHI_Node (Inst) and then Is_Return_Phi (Inst) then
         return No_BB_T;
      end if;

      --  Now skip any Phi's or debug value builtins. If what's left is an
      --  unconditional branch, return its target; otherwise, return our
      --  target.

      Inst := Get_First_Non_Phi_Or_Dbg (BB);
      return (if   Is_Unc_Br (Inst) then Effective_Dest (Get_Operand0 (Inst))
              else BB);

   end Effective_Dest;

   -----------------
   -- Output_Decl --
   ----------------

   procedure Output_Decl
     (S             : Str;
      Semicolon     : Boolean := True;
      Is_Global     : Boolean := False;
      No_Indent     : Boolean := False;
      Indent_Before : Integer := 0;
      Indent_After  : Integer := 0;
      V             : Value_T := No_Value_T)
   is
      OL : constant Out_Line :=
        (Line_Text      => (if Semicolon then S & ";" else S),
         No_Indent      => No_Indent,
         Indent_Before  => Indent_Before,
         Indent_After   => Indent_After,
         V              => V,
         BB             => No_BB_T,
         Need_Brace     => False);
   begin
      if Is_Global then
         Global_Decls.Append (OL);
      else
         Local_Decls.Append (OL);
         Add_Decl_Line (Local_Decls.Last);
      end if;
   end Output_Decl;

   -----------------
   -- Output_Decl --
   ----------------

   procedure Output_Decl
     (S             : String;
      Semicolon     : Boolean := True;
      Is_Global     : Boolean := False;
      No_Indent     : Boolean := False;
      Indent_Before : Integer := 0;
      Indent_After  : Integer := 0;
      V             : Value_T := No_Value_T)
   is
   begin
      Output_Decl (+S, Semicolon, Is_Global, No_Indent, Indent_Before,
                   Indent_After, V);
   end Output_Decl;

   -----------------
   -- Output_Stmt --
   ----------------

   procedure Output_Stmt
     (S             : Str;
      Semicolon     : Boolean       := True;
      No_Indent     : Boolean       := False;
      Indent_Before : Integer       := 0;
      Indent_After  : Integer       := 0;
      V             : Value_T       := No_Value_T;
      BB            : Basic_Block_T := No_BB_T;
      Need_Brace    : Boolean       := False)
   is
   begin
      --  If we've been given an instruction corresponding to this
      --  statement and it has side-effects, first flush any pending
      --  assignments.

      if Present (V) and then Has_Side_Effects (V) then
         Process_Pending_Values;
      end if;

      --  Add the statement to the appropriate block

      Stmts.Append ((Line_Text      => (if Semicolon then S & ";" else S),
                     No_Indent      => No_Indent,
                     Indent_Before  => Indent_Before,
                     Indent_After   => Indent_After,
                     V              => V,
                     BB             => BB,
                     Need_Brace     => Need_Brace));
      Set_Last_Stmt (Current_BB, Stmts.Last);
      if No (Get_First_Stmt (Current_BB)) then
         Set_First_Stmt (Current_BB, Stmts.Last);
      end if;
   end Output_Stmt;

   -----------------
   -- Output_Stmt --
   ----------------

   procedure Output_Stmt
     (S             : String;
      Semicolon     : Boolean       := True;
      No_Indent     : Boolean       := False;
      Indent_Before : Integer       := 0;
      Indent_After  : Integer       := 0;
      V             : Value_T       := No_Value_T;
      BB            : Basic_Block_T := No_BB_T;
      Need_Brace    : Boolean       := False)
   is
   begin
      Output_Stmt (+S, Semicolon, No_Indent, Indent_Before, Indent_After, V,
                   BB, Need_Brace);
   end Output_Stmt;

   --------------------------
   -- Get_Global_Decl_Line --
   --------------------------

   function Get_Global_Decl_Line (Idx : Global_Decl_Idx) return Out_Line is
     (Global_Decls.Table (Idx));

   -------------------------
   -- Get_Local_Decl_Line --
   -------------------------

   function Get_Local_Decl_Line (Idx : Local_Decl_Idx) return Out_Line is
     (Local_Decls.Table (Idx));

   -------------------
   -- Get_Stmt_Line --
   -------------------

   function Get_Stmt_Line (Idx : Stmt_Idx) return Out_Line is
     (Stmts.Table (Idx));

   --------------------------
   -- Get_Last_Global_Decl --
   --------------------------

   function Get_Last_Global_Decl return Global_Decl_Idx is
     (Global_Decls.Last);

   ---------------
   -- Output_BB --
   ---------------

   procedure Output_BB (BB : Basic_Block_T) is
      V          : Value_T          :=
        (if Present (BB) then Get_First_Instruction (BB) else No_Value_T);
      Terminator : constant Value_T :=
        (if Present (BB) then Get_Basic_Block_Terminator (BB) else No_Value_T);

   begin
      --  Set which block we're dealing with

      Current_BB := BB;

      --  If this isn't really a basic block or we already processed it, do
      --  nothing.

      if No (BB) or else Get_Was_Output (BB) then
         return;
      end if;

      --  Mark that we're outputing this block and process each
      --  instruction it.

      Set_Was_Output (BB);
      while Present (V) loop
         Process_Instruction (V);
         V := Get_Next_Instruction (V);
      end loop;

      --  Now process any block referenced by the terminator

      for J in Nat range 0 .. Get_Num_Successors (Terminator) - 1 loop
         Output_BB (Effective_Dest (Basic_Block_As_Value
                                      (Get_Successor (Terminator, J))));
      end loop;

      Current_BB := No_BB_T;
   end Output_BB;

   -------------------
   -- Output_Branch --
   -------------------

   procedure Output_Branch
     (From       : Value_T;
      To         : Value_T;
      Orig_From  : Value_T := No_Value_T;
      Need_Brace : Boolean := False;
      Had_Phi    : Boolean := False) is
   begin
      Output_Branch (From, Value_As_Basic_Block (To), Orig_From, Need_Brace,
                     Had_Phi);
   end Output_Branch;

   -------------------
   -- Output_Branch --
   -------------------

   procedure Output_Branch
     (From       : Value_T;
      To         : Basic_Block_T;
      Orig_From  : Value_T := No_Value_T;
      Need_Brace : Boolean := False;
      Had_Phi    : Boolean := False)
   is
      Our_From    : constant Value_T       :=
        (if Present (Orig_From) then Orig_From else From);
      From_BB     : constant Basic_Block_T := Get_Instruction_Parent (From);
      Our_Had_Phi : Boolean                := Had_Phi;
      Target_I    : Value_T                := Get_First_Instruction (To);

   begin
      --  Scan the start of the target block looking for Phi instructions

      while Present (Target_I) and then Is_APHI_Node (Target_I) loop
         declare
            Used_In_Return : constant Boolean :=
              not Our_Had_Phi and then Is_Return_Phi (Target_I);
            Phi_Val        : Value_T := No_Value_T;

         begin
            --  If we find a phi, we must ensure we've declared its type
            --  and then copy the appropriate data into a temporary
            --  associated with it.  We need to use the temporary because
            --  if we have something like
            --
            --     %foo = phi i32 [ 7, %0 ], [ 0, %entry ]
            --     %1 = phi i32 [ %foo, %0 ], [ 3, %entry ]
            --
            --  we're supposed to set %1 to the value of %foo before the
            --  assignment to it. Declare the temporary here too.

            if not Used_In_Return
              and then not Get_Is_Temp_Decl_Output (Target_I)
            then
               Maybe_Write_Typedef (Type_Of (Target_I));
               Output_Decl (TP ("#T1 #P1", Target_I));
               Set_Is_Temp_Decl_Output (Target_I);
            end if;

            for J in 0 .. Count_Incoming (Target_I) - 1 loop
               if Get_Incoming_Block (Target_I, J) = From_BB then
                  Phi_Val := Get_Operand (Target_I, J);
               end if;
            end loop;

            --  If this Phi is only used in a return, emit the return and
            --  we're done.

            if Used_In_Return then
               Output_Stmt ("return " & Phi_Val + Assign,
                            Indent_Before =>
                              (if Need_Brace then C_Indent else 0),
                            Indent_After  =>
                              (if Need_Brace then -C_Indent else 0),
                             V            => Target_I);
               return;

            --  Otherwise, if we need to write a brace and indent, do so

            elsif not Our_Had_Phi and then Need_Brace then
               Output_Stmt ("{",
                            Semicolon     => False,
                            Indent_After  => C_Indent,
                            Indent_Before => C_Indent);
            end if;

            Maybe_Decl (Phi_Val);
            Write_Copy (Target_I + Phi_Temp, Phi_Val, Type_Of (Phi_Val));
            Our_Had_Phi := True;
            Target_I    := Get_Next_Instruction (Target_I);
         end;
      end loop;

      --  Skip any debug intrinsics

      while Is_A_Dbg_Info_Intrinsic (Target_I) loop
         Target_I := Get_Next_Instruction (Target_I);
      end loop;

      --  If the instruction at the target is an unconditional branch, go to
      --  it instead.

      if Present (Target_I) and then Is_Unc_Br (Target_I) then
         --  Since we're not going to actually execute this block, we need
         --  to copy back the temporaries, if any, we made above. Do this
         --  by executing any Phi nodes at the start.

         Target_I := Get_First_Instruction (To);
         while Present (Target_I) and then Is_APHI_Node (Target_I) loop
            Process_Instruction (Target_I);
            Target_I := Get_Next_Instruction (Target_I);
         end loop;

         --  Now branch to where this block would branch

         Output_Branch (Target_I, Get_Operand0 (Target_I),
                        Orig_From  => Our_From,
                        Need_Brace => Need_Brace and then not Our_Had_Phi);
      else
         Output_Stmt ("goto " & To,
                      V          => Our_From,
                      BB         => To,
                      Need_Brace => Need_Brace and then not Our_Had_Phi);
      end if;

      --  Now, if we had a Phi, close the block we opened

      if Our_Had_Phi and then Need_Brace then
         Output_Stmt ("}",
                      Semicolon     => False,
                      Indent_After  => -C_Indent,
                      Indent_Before => -C_Indent);
      end if;
   end Output_Branch;

   ------------------------
   -- Branch_Instruction --
   ------------------------

   procedure Branch_Instruction (V : Value_T; Ops : Value_Array) is
      Op1    : constant Value_T := Ops (Ops'First);
      Result : Str;
   begin
      --  See if this is an unconditional or conditional branch. Treat a
      --  conditional branch both of whom go to the same location as an
      --  unconditional branch.  We need to process all pending values
      --  before taking the branch, but want to do that after elaborating
      --  the condition to avoid needing to force elaboration of the
      --  condition.
      --  ??? We'd also prefer not to force elaboration of values needed in
      --  the phi computation, but that's hard and may not be possible.

      if Is_Conditional (V) and then Ops (Ops'First + 1) /= Ops (Ops'First + 2)
      then
         Result := TP ("if (#1)", Op1) + Assign;
         Output_Stmt (Result, Semicolon => False, V => V);
         Output_Branch (V, Ops (Ops'First + 2), Need_Brace => True);
         Output_Stmt ("else", Semicolon => False, V => V);
         Output_Branch (V, Ops (Ops'First + 1), Need_Brace => True);
      elsif Is_Conditional (V) then
         Output_Branch (V, Ops (Ops'First + 1));
      else
         Output_Branch (V, Op1);
      end if;

   end Branch_Instruction;

   ------------------------
   -- Switch_Instruction --
   ------------------------

   procedure Switch_Instruction (V : Value_T; Ops : Value_Array) is
      Val       : constant Value_T                := Ops (Ops'First);
      Default   : constant Value_T                := Ops (Ops'First + 1);
      POO       : constant Process_Operand_Option :=
        (if Get_Is_Unsigned (Val) then POO_Unsigned else POO_Signed);
      Last_Case : constant Nat := Ops'Length / 2 - 1;
      Result    : Str                             :=
        Process_Operand (Val, POO);

   begin
      --  If Val is narrower than int, we must force it to its size

      if Get_Scalar_Bit_Size (Val) < Int_Size then
         Result := TP ("(#T1) ", Val) & (Result + Unary);
      end if;

      --  Write the switch statement itself

      Output_Stmt ("switch (" & Result +  Assign & ")",
                   Semicolon => False,
                   V         => V);
      Output_Stmt ("{",
                   Semicolon     => False,
                   Indent_After  => C_Indent,
                   Indent_Before => C_Indent);

      --  Now handle each case. They start after the first two operands and
      --  alternate between value and branch target.

      for J in 1 .. Last_Case loop
         declare
            Value     : constant Value_T := Ops (Ops'First + J * 2);
            Dest      : constant Value_T := Ops (Ops'First + J * 2 + 1);
            Next_Dest : constant Value_T :=
              (if J = Last_Case then Default else Ops (Ops'First + J * 2 + 3));

         begin
            Output_Stmt ("case " & Process_Operand (Value, POO) & ":",
                         Semicolon     => False,
                         Indent_After  => C_Indent,
                         Indent_Before => -C_Indent);

            --  If this isn't branching to the same label as the next case
            --  (or default if this is the last case), output the branch.
            --  Note that we can't use Effective_Dest here because that's
            --  used to determine which basic block is branched to and
            --  ignores Phi nodes.
            --  ??? We may want to sort to ensure this happens if there are
            --  any duplicates.

            if Dest /= Next_Dest then
               Output_Branch (V, Dest);
               Output_Stmt ("", Semicolon => False);
            end if;
         end;
      end loop;

      --  Finally, write the default and end the statement

      Output_Stmt ("default:",
                   Semicolon     => False,
                   Indent_Before => -C_Indent,
                   Indent_After  => C_Indent);
      Output_Branch (V, Default);
      Output_Stmt ("}",
                   Semicolon     => False,
                   Indent_After  => -C_Indent,
                   Indent_Before => -C_Indent);

   end Switch_Instruction;

   --------------
   -- Write_BB --
   --------------

   procedure Write_BB (BB : Basic_Block_T; Omit_Label : Boolean := False) is
   begin
      --  If we didn't output anything for this BB or we've already written
      --  it, do nothing.

      if not Get_Was_Output (BB) or else Get_Was_Written (BB) then
         return;

      --  Unless this is the entry block or we've been asked to omit the
      --  label, write the label for the block, preceeded by a blank line.

      elsif not Omit_Label and then not Is_Entry_Block (BB) then
         Write_Eol;
         Write_Line (BB & ":",
                     No_Indent => True,
                     V => Get_First_Instruction (BB));
      end if;

      --  Now mark as written and write each statement that we output for
      --  this block.

      Set_Was_Written (BB);
      for Idx in Get_First_Stmt (BB) .. Get_Last_Stmt (BB) loop
         Write_Line (Idx);
      end loop;

   end Write_BB;

begin
   --  Ensure we have an empty entry in the tables that support empty
   --  entries.

   Local_Decls.Increment_Last;
   Stmts.Increment_Last;

end CCG.Blocks;
