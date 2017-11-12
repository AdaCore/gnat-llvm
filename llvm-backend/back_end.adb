------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
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

with LLVM_Drive;
with Adabkend;
with Elists;
with Stringt;
with Namet;

package body Back_End is

   package GNAT2LLVM is new Adabkend
     (Product_Name       => "GNAT for LLVM",
      Copyright_Years    => "2013-2013",
      Driver             => LLVM_Drive.GNAT_To_LLVM,
      Is_Back_End_Switch => LLVM_Drive.Is_Back_End_Switch);

   procedure Scan_Compiler_Arguments renames GNAT2LLVM.Scan_Compiler_Arguments;

   -------------------
   -- Call_Back_End --
   -------------------

   procedure Call_Back_End (Mode : Back_End_Mode_Type) is
      pragma Unreferenced (Mode); -- Mode not referenced

   begin
      --  Since the back end is called with all tables locked,
      --  first unlock any tables that we need to change.

      Stringt.Unlock;
      Namet.Unlock;
      Elists.Unlock;

      GNAT2LLVM.Call_Back_End;

      --  Make sure to lock any unlocked tables again before returning

      Elists.Lock;
      Namet.Lock;
      Stringt.Lock;
   end Call_Back_End;

   -------------------------------
   -- Gen_Or_Update_Object_File --
   -------------------------------

   procedure Gen_Or_Update_Object_File is
   begin
      null;
   end Gen_Or_Update_Object_File;

end Back_End;
