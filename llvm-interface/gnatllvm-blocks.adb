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

with Errout;   use Errout;
with Exp_Unst; use Exp_Unst;
with Nlists;   use Nlists;
with Stand;    use Stand;
with Table;    use Table;
with Uintp;    use Uintp;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Subprograms; use GNATLLVM.Subprograms;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package body GNATLLVM.Blocks is

   --  This data structure records the information about each block that
   --  we're in and we construct a table to act as a block stack.

   type Block_Info is record
      In_Stmts           : Boolean;
      --  True if we are in the statement section of the current block

      Stack_Save         : GL_Value;
      --  Value of the stack pointer at entry to the block

      At_End_Proc        : GL_Value;
      --  Procedure to be called at normal or abnormal exit of the block

      At_End_Static_Link : GL_Value;
      --  The activation record to pass to the At_End_Proc

      Landing_Pad        : Basic_Block_T;
      --  Basic block containing the landing pad for this block, if any.

      EH_List            : List_Id;
      --  List of exception handlers.  ??? We may not want to keep it this way

   end record;

   package Block_Stack is new Table.Table
     (Table_Component_Type => Block_Info,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 15,
      Table_Increment      => 5,
      Table_Name           => "Block_Stack");
   --  Stack of blocks that we're in.

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

   procedure Emit_One_Fixup (Blk : Integer; Do_At_End, Do_Stack : Boolean);
   --  Do one fixup when exiting Blk, saying whether to run "at end handler
   --  and whether to restore the stack pointer.

   procedure Make_Landing_Pad (Lpad : Basic_Block_T; EH_List : List_Id)
     with Pre => Present (Lpad) and then Present (EH_List);
   --  Generate a landingpad instruction from the data in EH_List

   procedure Initialize_Predefines;
   --  Initialize the predefined functions and variables below

   Predefines_Set   : Boolean  := False;
   --  True when all of the below have been initialized

   Personality_Fn   : GL_Value;
   --  The definition of the personality function

   EH_Slot_Id_Fn    : GL_Value;
   --  The LLVM builtin that converts an exception into a slot number

   Begin_Handler_Fn : GL_Value;
   End_Handler_Fn   : GL_Value;
   --  Begin and end functions for handlers

   Others_Value     : GL_Value;
   All_Others_Value : GL_Value;
   --  "Exception" address for "others" and special "all others"

   ----------------
   -- Push_Block --
   ----------------

   procedure Push_Block is
      Stack_Save : constant GL_Value :=
        (if Block_Stack.Last < 1 then No_GL_Value
         else Call (Get_Stack_Save_Fn, Standard_A_Char, (1 .. 0 => <>)));
   begin
      Block_Stack.Append ((Stack_Save         => Stack_Save,
                           At_End_Proc        => No_GL_Value,
                           At_End_Static_Link => No_GL_Value,
                           Landing_Pad        => No_BB_T,
                           EH_List            => No_List,
                           In_Stmts           => False));

   end Push_Block;

   -----------------------------
   --  Start_Block_Statements --
   -----------------------------

   procedure Start_Block_Statements
     (At_End_Proc : Entity_Id; EH_List : List_Id) is

   begin
      pragma Assert (not Block_Stack.Table (Block_Stack.Last).In_Stmts);

      Block_Stack.Table (Block_Stack.Last).EH_List  := EH_List;
      Block_Stack.Table (Block_Stack.Last).In_Stmts := True;

      if Present (At_End_Proc) then

         --  Save both the end proc and the value of the static link.
         --  Since we'll be generating the call directly, we have to convert
         --  the static link to the proper pointer type for the activation
         --  record.  There may not be a static link, however, if there re
         --  no uplevel references.

         Block_Stack.Table (Block_Stack.Last).At_End_Proc :=
           Emit_LValue (At_End_Proc);
         if Subps_Index (Entity (At_End_Proc)) /= Uint_0
           and then Present (Subps.Table (Subp_Index
                                            (Entity (At_End_Proc))).ARECnF)
         then
            Block_Stack.Table (Block_Stack.Last).At_End_Static_Link :=
              Pointer_Cast (Get_Static_Link (At_End_Proc),
                            Full_Etype (Extra_Formals (Entity (At_End_Proc))));
         end if;
      end if;

   end Start_Block_Statements;

   ---------------------
   -- Get_Landing_Pad --
   ---------------------

   function Get_Landing_Pad return Basic_Block_T is
   begin
      --  If we're in the Statements part of a block that has nexceptions,
      --  see if we've made a block for the landing-pad.  If not, make one.

      for J in reverse 1 .. Block_Stack.Last loop
         if Present (Block_Stack.Table (J).EH_List)
           and then Block_Stack.Table (J).In_Stmts
         then
            if No (Block_Stack.Table (J).Landing_Pad) then
               Block_Stack.Table (J).Landing_Pad :=
                 Create_Basic_Block ("Lpad");
            end if;

            return Block_Stack.Table (J).Landing_Pad;
         end if;
      end loop;

      return No_BB_T;
   end Get_Landing_Pad;

   --------------------
   -- Emit_One_Fixup --
   --------------------

   procedure Emit_One_Fixup (Blk : Integer; Do_At_End, Do_Stack : Boolean) is
      Block_Inf : constant Block_Info := Block_Stack.Table (Blk);

   begin
      --  First call the "at end" handler before any variables get
      --  deallocated.

      if Do_At_End and then Present (Block_Inf.At_End_Proc) then
         if Present (Block_Inf.At_End_Static_Link) then
            Call (Block_Inf.At_End_Proc, (1 => Block_Inf.At_End_Static_Link));
         else
            Call (Block_Inf.At_End_Proc, (1 .. 0 => <>));
         end if;
      end if;

      --  Then deallocate variables

      if Do_Stack and then Present (Block_Inf.Stack_Save) then
         Call (Get_Stack_Restore_Fn, (1 => Block_Inf.Stack_Save));
      end if;
   end Emit_One_Fixup;

   ---------------------------
   -- Initialize_Predefines --
   ---------------------------

   procedure Initialize_Predefines is
   begin
      if Predefines_Set then
         return;
      end if;

      Personality_Fn  :=
        Add_Function ("__gnat_personality_v0",
                      Fn_Ty ((1 .. 0 => <>), Int_Ty (32), True),
                      Standard_Void_Type);
      Set_Does_Not_Throw (Personality_Fn);

      Begin_Handler_Fn :=
        Add_Function ("__gnat_begin_handler",
                      Fn_Ty ((1 => Void_Ptr_Type), Void_Type),
                      Standard_Void_Type);
      Set_Does_Not_Throw (Begin_Handler_Fn);

      End_Handler_Fn   :=
        Add_Function ("__gnat_end_handler",
                      Fn_Ty ((1 => Void_Ptr_Type), Void_Type),
                      Standard_Void_Type);
      Set_Does_Not_Throw (End_Handler_Fn);

      EH_Slot_Id_Fn    :=
        Add_Function ("llvm.eh.typeid.for",
                      Fn_Ty ((1 => Void_Ptr_Type), Int_Ty (32)), Int_32_Type);
      Set_Does_Not_Throw (EH_Slot_Id_Fn);

      Others_Value     := Add_Global (Standard_Short_Short_Integer,
                                      "__gnat_others_value");
      All_Others_Value := Add_Global (Standard_Short_Short_Integer,
                                      "__gnat_all_others_value");

      Predefines_Set   := True;
   end Initialize_Predefines;

   ----------------------
   -- Make_Landing_Pad --
   ----------------------

   procedure Make_Landing_Pad (Lpad : Basic_Block_T; EH_List : List_Id) is
      LP_Type           : constant Type_T        :=
        Build_Struct_Type ((1 => Void_Ptr_Type, 2 => Int_Ty (32)));
      Next_BB           : constant Basic_Block_T := Create_Basic_Block;
      BB                : Basic_Block_T;
      Handler, Choice   : Node_Id;
      LP_Inst           : GL_Value;
      Selector, Exc_Ptr : GL_Value;
      Exc               : GL_Value;

      type One_Clause is record
         BB    : Basic_Block_T;
         --  Basic block containing the actions for this exception

         Exc   : GL_Value;
         --  The address of the exception caught by this handler

         Stmts : List_Id;
         --  The statements in the handler
      end record;

      package Clauses is new Table.Table
        (Table_Component_Type => One_Clause,
         Table_Index_Type     => Nat,
         Table_Low_Bound      => 1,
         Table_Initial        => 15,
         Table_Increment      => 5,
         Table_Name           => "Clauses");

   begin
      Initialize_Predefines;

      --  Emit the landing pad instruction, indicate that it's a
      --  cleanup and add the clauses to the instruction and our table.

      Position_Builder_At_End (Lpad);
      LP_Inst := Landing_Pad (LP_Type, Personality_Fn);
      Handler := First_Non_Pragma (EH_List);
      while Present (Handler) loop
         BB     := Create_Basic_Block;
         Choice := First (Exception_Choices (Handler));
         while Present (Choice) loop
            if Nkind (Choice) = N_Others_Choice then
               Exc := (if All_Others (Choice) then All_Others_Value
                       else Others_Value);
            else
               Exc := Emit_LValue (Choice);
            end if;

            Add_Clause (LP_Inst, Exc);
            Clauses.Append ((BB => BB,
                             Exc => Convert_To_Access (Exc, Standard_A_Char),
                             Stmts => Statements (Handler)));
            Next (Choice);
         end loop;

         Next_Non_Pragma (Handler);
      end loop;

      --  Extract the selector and the exception pointer

      Exc_Ptr := Extract_Value (Standard_A_Char, LP_Inst, 0);
      Selector := Extract_Value (Standard_Integer, LP_Inst, 1);

      --  Generate code for the handlers, taking into account that we
      --  have duplicate BB's in the table.

      for J in 1 .. Clauses.Last loop
         if No (Get_Last_Instruction (Clauses.Table (J).BB)) then
            Position_Builder_At_End (Clauses.Table (J).BB);
            Call (Begin_Handler_Fn, (1 => Exc_Ptr));
            Emit (Clauses.Table (J). Stmts);

            --  If the above code branched out or returned, don't call the
            --  end handler code.  ???  TBD to make a block and make that
            --  the fixup.

            if not Are_In_Dead_Code then
               Call (End_Handler_Fn, (1 => Exc_Ptr));
               Build_Br (Next_BB);
            end if;
         end if;
      end loop;

      --  Now generate the code to branch to each exception handler

      BB := Lpad;
      for J in 1 .. Clauses.Last loop
         Position_Builder_At_End (BB);
         BB := Create_Basic_Block;
         Build_Cond_Br (I_Cmp (Int_EQ, Selector,
                               Call (EH_Slot_Id_Fn, Int_32_Type,
                                     (1 => Clauses.Table (J).Exc))),
                        Clauses.Table (J).BB, BB);
      end loop;

      Position_Builder_At_End (BB);
      Build_Resume (LP_Inst);
      Position_Builder_At_End (Next_BB);

   end Make_Landing_Pad;

   ---------------
   -- Pop_Block --
   ---------------

   procedure Pop_Block is
      Lpad     : constant Basic_Block_T :=
        Block_Stack.Table (Block_Stack.Last).Landing_Pad;
      Was_Dead : constant Boolean       := Are_In_Dead_Code;
      Next_BB  : constant Basic_Block_T :=
        (if Present (Lpad) and then not Was_Dead
         then Create_Basic_Block else No_BB_T);

   begin
      --  If we're not in dead code, we have to fixup the block and the branch
      --  around any landingpad.  But that code is not protected by any
      --  exception handlers in the block.

      if not Are_In_Dead_Code then
         Block_Stack.Table (Block_Stack.Last).In_Stmts := False;
         Emit_One_Fixup (Block_Stack.Last,
                         Do_At_End => True, Do_Stack => True);
         if Present (Next_BB) then
            Build_Br (Next_BB);
         end if;
      end if;

      --  Now output the landing pad and handlers

      if Present (Lpad) then
         Make_Landing_Pad (Lpad, Block_Stack.Table (Block_Stack.Last).EH_List);
         if Was_Dead then
            Build_Unreachable;
         end if;
         if Present (Next_BB) then
            Build_Br (Next_BB);
            Position_Builder_At_End (Next_BB);
         end if;
      end if;

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
