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

with Stand;    use Stand;
with Table;

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
   end record;

   package Block_Stack is new Table.Table
     (Table_Component_Type => Block_Info,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 15,
      Table_Increment      => 5,
      Table_Name           => "Block_Stack");
   --  Stack of blocks that we're in.

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
                           EH_List => No_List));
   end Push_Block;

   ----------------------
   --  Start_EH_Region --
   ----------------------

   procedure Start_EH_Region (EH_List : List_Id) is
   begin
      Block_Stack.Table (Block_Stack.Last).EH_List := EH_List;
   end Start_EH_Region;

   -------------------------
   -- Start_At_End_Region --
   -------------------------

   procedure Start_At_End_Region (At_End_Proc : Entity_Id) is
   begin
      Block_Stack.Table (Block_Stack.Last).At_End_Proc :=
        Emit_LValue (At_End_Proc);
   end Start_At_End_Region;

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

      Block_Stack.Decrement_Last;
   end Pop_Block;

end GNATLLVM.Blocks;
