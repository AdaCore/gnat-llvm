------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with Errout; use Errout;
with Stand;  use Stand;
with Table;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Subprograms; use GNATLLVM.Subprograms;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package body GNATLLVM.Blocks is

   --  This data structure records the information about each block that
   --  we're in and we construct a table to act as a block stack.

   type Block_Info is record
      Stack_Save  : GL_Value;
      --  Value of the stack pointer at entry to the block

      At_End_Proc : GL_Value;
      --  Function to be called at normal or abnormal exit of the block

      EH_List     : List_Id;
      --  List of exception handlers.  ?? We may not want to keep it this way

      In_Stmts    : Boolean;
      --  True if this block was entered from the statement section of its
      --  parent block.
   end record;

   package Block_Stack is new Table.Table
     (Table_Component_Type => Block_Info,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 15,
      Table_Increment      => 5,
      Table_Name           => "Block_Stack");
   --  Stack of blocks that we're in.

   In_Stmt_Part : Boolean := False;
   --  True when we're in the statements part of a block (as opposed to
   --  the declarative part.

   --  These tables implement local exception handling, where a
   --  language-defined check within a block jumps directly to a label
   --  associated with the actions for that exception.

   package Constraint_Error_Stack is new Table.Table
     (Table_Component_Type => Entity_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 1,
      Table_Name           => "Constraint_Error_Stack");
   --  Stack of labels for constraint error

   package Storage_Error_Stack is new Table.Table
     (Table_Component_Type => Entity_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 1,
      Table_Name           => "Storage_Error_Stack");
   --  Stack of labels for storage error

   package Program_Error_Stack is new Table.Table
     (Table_Component_Type => Entity_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 1,
      Table_Name           => "Program_Error_Stack");
   --  Stack of labels for program error

   type Exit_Point is record
      Label_Entity : Entity_Id;
      Exit_BB      : Basic_Block_T;
   end record;

   Exit_Point_Low_Bound : constant := 1;

   package Exit_Point_Table is new Table.Table
     (Table_Component_Type => Exit_Point,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => Exit_Point_Low_Bound,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Exit_Point_Table");
   --  Table of scoped loop exit points. Last inserted exit point correspond
   --  to the innermost loop.

   ----------------
   -- Push_Block --
   ----------------

   procedure Push_Block is
      Stack_Save : constant GL_Value :=
        (if Block_Stack.Last < 1 then No_GL_Value
         else Call (Get_Stack_Save_Fn, Standard_A_Char, (1 .. 0 => <>)));
   begin
      Block_Stack.Append ((Stack_Save => Stack_Save,
                           At_End_Proc => No_GL_Value,
                           EH_List => No_List,
                           In_Stmts => In_Stmt_Part));

      In_Stmt_Part := False;

   end Push_Block;

   -----------------------------
   --  Start_Block_Statements --
   -----------------------------

   procedure Start_Block_Statements
     (At_End_Proc : Entity_Id; EH_List : List_Id) is

   begin
      Block_Stack.Table (Block_Stack.Last).EH_List := EH_List;

      if Present (At_End_Proc) then
         Block_Stack.Table (Block_Stack.Last).At_End_Proc :=
           Emit_LValue (At_End_Proc);
      end if;

      In_Stmt_Part := True;

   end Start_Block_Statements;

   ---------------
   -- Pop_Block --
   ---------------

   procedure Pop_Block is
      At_End     : constant GL_Value :=
        Block_Stack.Table (Block_Stack.Last).At_End_Proc;
      Stack_Save : constant GL_Value :=
        Block_Stack.Table (Block_Stack.Last).Stack_Save;

   begin
      if not Are_In_Dead_Code then

         --  First call the "at end" handler before any variables get
         --  deallocated.

         if Present (At_End) then
            Call (At_End, (1 .. 0 => <>));
         end if;

         --  Then deallocate variables

         if Present (Stack_Save) then
            Call (Get_Stack_Restore_Fn, (1 => Stack_Save));
         end if;
      end if;

      In_Stmt_Part := Block_Stack.Table (Block_Stack.Last).In_Stmts;
      Block_Stack.Decrement_Last;
   end Pop_Block;

   --------------------------------------
   -- Process_Push_Pop_xxx_Error_Label --
   --------------------------------------

   procedure Process_Push_Pop_xxx_Error_Label (N : Node_Id) is begin
      case Nkind (N) is
         when N_Push_Constraint_Error_Label =>
            Constraint_Error_Stack.Append (Exception_Label (N));

         when N_Push_Storage_Error_Label =>
            Storage_Error_Stack.Append (Exception_Label (N));

         when N_Push_Program_Error_Label =>
            Program_Error_Stack.Append (Exception_Label (N));

         when N_Pop_Constraint_Error_Label =>
            Constraint_Error_Stack.Decrement_Last;

         when N_Pop_Storage_Error_Label =>
            Storage_Error_Stack.Decrement_Last;

         when N_Pop_Program_Error_Label =>
            Program_Error_Stack.Decrement_Last;

         when others =>
            pragma Assert (False);
      end case;
   end Process_Push_Pop_xxx_Error_Label;

   ------------------------------
   -- Get_Exception_Goto_Entry --
   ------------------------------

   function Get_Exception_Goto_Entry (Kind : Node_Kind) return Entity_Id is
   begin
      if Kind = N_Raise_Constraint_Error
        and then Constraint_Error_Stack.Last /= 0
        and then Present (Constraint_Error_Stack.Table
                            (Constraint_Error_Stack.Last))
      then
         return Constraint_Error_Stack.Table (Constraint_Error_Stack.Last);

      elsif Kind = N_Raise_Program_Error
        and then Program_Error_Stack.Last /= 0
        and then Present (Program_Error_Stack.Table
                            (Program_Error_Stack.Last))
      then
         return Program_Error_Stack.Table (Program_Error_Stack.Last);

      elsif Kind = N_Raise_Storage_Error
        and then Storage_Error_Stack.Last /= 0
        and then Present (Storage_Error_Stack.Table
                            (Storage_Error_Stack.Last))
      then
         return Storage_Error_Stack.Table (Storage_Error_Stack.Last);
      else
         return Empty;
      end if;
   end Get_Exception_Goto_Entry;

   ------------------
   -- Get_Label_BB --
   ------------------

   function Get_Label_BB (E : Entity_Id) return Basic_Block_T is
      BB : Basic_Block_T := Get_Basic_Block (E);

   begin
      if No (BB) then
         BB := Create_Basic_Block (Get_Name (E));
         Set_Basic_Block (E, BB);
      end if;

      return BB;
   end Get_Label_BB;

   ---------------------------
   -- Enter_Block_With_Node --
   ---------------------------

   function Enter_Block_With_Node (Node : Node_Id) return Basic_Block_T
   is
      E         : constant Entity_Id     :=
        (if Present (Node) and then Present (Identifier (Node))
         then Entity (Identifier (Node)) else Empty);
      This_BB   : constant Basic_Block_T := Get_Insert_Block;
      Last_Inst : constant Value_T       := Get_Last_Instruction (This_BB);
      Entry_BB  : constant Basic_Block_T :=
        Get_Entry_Basic_Block (LLVM_Value (Current_Func));
      BB        : constant Basic_Block_T :=
          (if Present (E) and then Has_BB (E) then Get_Basic_Block (E)
           elsif No (Last_Inst) and then This_BB /= Entry_BB
           then This_BB else Create_Basic_Block);
      --  If we have an identifier and it has a basic block already set,
      --  that's the one that we have to use.  If we've just started a
      --  basic block with no instructions in it, that basic block will do,
      --  unless it's the entry BB since we're going to branch to it.
      --  Otherwise, get a new one.

   begin
      --  Now, unless this is our basic block, jump to it and position there

      if BB /= This_BB then
         Build_Br (BB);
         Position_Builder_At_End (BB);
      end if;

      --  If we have an entity to point to the block, make that linkage.

      if Present (E) then
         Set_Basic_Block (E, BB);
      end if;

      return BB;
   end Enter_Block_With_Node;

   ---------------
   -- Push_Loop --
   ---------------

   procedure Push_Loop (LE : Entity_Id; Exit_Point : Basic_Block_T) is
   begin
      Exit_Point_Table.Append ((LE, Exit_Point));
   end Push_Loop;

   --------------
   -- Pop_Loop --
   --------------

   procedure Pop_Loop is
   begin
      Exit_Point_Table.Decrement_Last;
   end Pop_Loop;

   --------------------
   -- Get_Exit_Point --
   --------------------

   function Get_Exit_Point (N : Node_Id) return Basic_Block_T is
   begin
      --  If no exit label was specified, use the last one

      if No (N) then
         return Exit_Point_Table.Table (Exit_Point_Table.Last).Exit_BB;
      end if;

      --  Otherwise search for a match

      for I in Exit_Point_Low_Bound .. Exit_Point_Table.Last loop
         if Exit_Point_Table.Table (I).Label_Entity = Entity (N) then
            return Exit_Point_Table.Table (I).Exit_BB;
         end if;
      end loop;

      --  If the loop label isn't registered, then we just met an exit
      --  statement with no corresponding loop: should not happen.

      Error_Msg_N ("unknown loop identifier", N);
      raise Program_Error;
   end Get_Exit_Point;

end GNATLLVM.Blocks;
