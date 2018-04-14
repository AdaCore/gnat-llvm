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

with Types; use Types;

with LLVM.Types;      use LLVM.Types;

with GNATLLVM.Environment; use GNATLLVM.Environment;

package GNATLLVM.DebugInfo is

   type DI_File_Cache is array (Source_File_Index range <>) of Metadata_T;
   DI_Cache : access DI_File_Cache := null;

   function Get_Debug_File_Node
     (Bld  : DI_Builder_T;
      File : Source_File_Index) return Metadata_T;
   --  Produce and return a DIFile entry for the specified source file index

   procedure Set_Debug_Pos_At_Node (Env : Environ; N : Node_Id);
   --  Set builder position for debugging to the Sloc of N.

end GNATLLVM.DebugInfo;
