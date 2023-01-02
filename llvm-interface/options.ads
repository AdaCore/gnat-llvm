------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

--  We provide a unit separate from GNATLLVM to avoid dragging dependencies
--  on LLVM libraries in GNAT tools.

with Ada.Command_Line; use Ada.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with System.OS_Lib; use System.OS_Lib;

package Options is

   Executable : constant String  := Base_Name (Command_Name, ".exe");
   First      : constant Integer := Executable'First;

   CCG : constant Boolean :=
     Getenv ("CCG").all /= ""
     or else (Executable'Length > 2
                and then Executable (First .. First + 1) = "c-");
   --  True if CCG mode should be enabled

end Options;
