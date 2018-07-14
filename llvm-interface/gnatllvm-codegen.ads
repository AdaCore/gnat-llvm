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

with System;           use System;
with System.OS_Lib;    use System.OS_Lib;
with Interfaces;
with Interfaces.C;     use Interfaces.C;

with Table;

package GNATLLVM.Codegen is

   Filename      : String_Access := new String'("");
   --  Filename to compile.

   CPU           :  String_Access := new String'("generic");
   --  Name of the specific CPU for this compilation.

   Code_Model    : Code_Model_T := Code_Model_Default;
   Reloc_Mode    : Reloc_Mode_T := Reloc_Default;
   --  Code generation options

   Target_Triple : String_Access :=
     new String'(Get_Default_Target_Triple);
   --  Name of the target for this compilation

   package Switch_Table is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Interfaces.C.int,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 1,
      Table_Name           => "Switch_Table");

   procedure Scan_Command_Line;
   --  Scan operands relevant to code generation

   procedure Initialize_LLVM_Target;
   --  Initialize all the data structures specific to the LLVM target code
   --  generation.

   procedure LLVM_Generate_Code (GNAT_Root : Node_Id);
   --  Generate LLVM code from what we've compiled with a node for error
   --  messages.

   function Is_Back_End_Switch (Switch : String) return Boolean;
   --  Return True if Switch is a switch known to the back end

end GNATLLVM.Codegen;
