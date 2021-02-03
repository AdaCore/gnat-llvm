------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2021, AdaCore                     --
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

with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Subprograms; use GNATLLVM.Subprograms;

package GNATLLVM.Blocks is

   --  We define a "block" here as an area of code that needs some sort of
   --  "protection" in that certain things are to be done when the block is
   --  exited, either normally, abnormally, or both.
   --
   --  We handle three kinds of "things" here:
   --
   --  (1) The stack pointer needs to be saved and restored to
   --  deallocate any variables created in the block.  This is done
   --  both on normal and abnormal exit and operates from the start of
   --  the block to the end of the block.  Every block other than the
   --  block corresponding to the enter subprogram has this action.
   --
   --  (2) If there if an "at end" handler, it needs to be executed on
   --  any normal or abnormal exit, but this does not include the
   --  declarative region of the block.
   --
   --  (3) If there are exception handles, they are executed if an
   --  exception occurs, but this also does not include the declarative
   --  region of the block.

   procedure Push_Block
     with Pre => not Library_Level;
   --  Push a block onto the block stack and mark its start

   procedure Save_Stack_Pointer;
   --  Generate a stack save at the start of the current block

   procedure Add_Lifetime_Entry (Ptr, Size : GL_Value)
     with Pre => Present (Ptr) and then Present (Size);
   --  Add an entry for a variable lifetime that ends at the end of this block

   procedure Add_Invariant_Entry (V : GL_Value; Size : GL_Value := No_GL_Value)
     with Pre => Present (V);
   --  Add an entry for a constant whose invariant point starts at the
   --  start of the code for this block.

   procedure Start_Block_Statements
     (At_End_Proc : Entity_Id := Empty; EH_List : List_Id := No_List)
     with Pre => not Library_Level;
   --  Indicate that this is the start of a region of the block to be
   --  protected by the exception handlers and an At_End_Proc and provide
   --  those, if present.

   function Get_Landing_Pad return Basic_Block_T;
   --  Get the basic block for the landingpad in the current block, if any

   procedure Pop_Block
     with Pre => not Library_Level;
   --  End the current block, generating code for any handlers, and
   --  pop the block stack.

   procedure Process_Push_Pop_xxx_Error_Label (N : Node_Id)
     with Pre => Nkind (N) in N_Push_Constraint_Error_Label ..
                              N_Pop_Storage_Error_Label;
   --  Process the above nodes by pushing and popping entries in our tables

   function Get_Exception_Goto_Entry (Kind : Node_Kind) return Entity_Id
     with Pre => Kind in N_Raise_xxx_Error;
   --  Get the last entry in the exception goto stack for Kind, if any

   function Get_Label_BB
     (E : Entity_Id; For_Address : Boolean := False) return Basic_Block_T
     with Pre  => Ekind (E) = E_Label,
          Post => Present (Get_Label_BB'Result);
   --  Lazily get the basic block associated with label E, creating it
   --  if we don't have it already.  If For_Address is True, we're getting
   --  this label to take its address, so we ignore any fixups.

   function Enter_Block_With_Node (Node : Node_Id) return Basic_Block_T
     with Post => Present (Enter_Block_With_Node'Result);
   --  We need a basic block at the present location to branch to.
   --  This will normally be a new basic block, but may be the current
   --  basic block it if's empty and not the entry block.  If Node is
   --  Present and already points to a basic block, we have to use
   --  that one.  If Present, but it doesn't point to a basic block,
   --  set it to the one we made.

   procedure Push_Loop (LE : Entity_Id; Exit_Point : Basic_Block_T)
     with Pre => Present (Exit_Point);
   procedure Pop_Loop;

   function Get_Exit_Point (N : Node_Id) return Basic_Block_T
     with Post => Present (Get_Exit_Point'Result);
   --  If N is specied, find the exit point corresponding to its entity.
   --  Otherwise, find the most recent (most inner) exit point.

   procedure Emit_Raise_Call_If
     (V    : GL_Value;
      N    : Node_Id;
      Kind : RT_Exception_Code := CE_Overflow_Check_Failed)
     with Pre => Present (V) and then Present (N);
   --  Raise an exception (default is overflow) if V evaluates to True

   procedure Emit_Fixups_For_Return;
   --  We're going to do a return.  Emit any needed fixups from the block
   --  we're in all the way out of the subprogram.

   procedure Emit_Raise_Call
     (N : Node_Id; Kind : RT_Exception_Code; Column : Boolean := False)
     with Pre => Present (N);
   --  Generate a call to a raise subprogram.  If Column is true, this is
   --  call to and "extended" raise subprogram that accept column
   --  information.

   procedure Emit_Raise (N : Node_Id)
     with Pre => Nkind (N) in N_Raise_xxx_Error;
   --  Process an N_Raise_xxx_Error node

   procedure Emit_Reraise;
   --  Emit code for an N_Raise

   procedure Reset_Block_Tables;
   --  We don't cut back some of the tables in this module when we can
   --  because they aren't used much, but we can certinly empty them
   --  when a subprogram is completed.

   procedure Initialize;
   --  Initialize all global names

end GNATLLVM.Blocks;
