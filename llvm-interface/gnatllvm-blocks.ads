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

with Atree;  use Atree;
with Nlists; use Nlists;
with Types;  use Types;

with GNATLLVM.Environment; use GNATLLVM.Environment;

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

   procedure Start_EH_Region (EH_List : List_Id)
     with Pre => not Library_Level and then Present (EH_List);
   --  Indicate that this is the start of a region of the block to be
   --  protected by the exception handlers.

   procedure Start_At_End_Region (At_End_Proc : Entity_Id)
     with Pre => not Library_Level and then Present (At_End_Proc);
   --  Indicate that this is the start of a region of the block to be
   --  protected by the "at end" procedure.

   procedure Pop_Block
     with Pre => not Library_Level;

end GNATLLVM.Blocks;
