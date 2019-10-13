------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

with GNATLLVM;         use GNATLLVM;
with GNATLLVM.Codegen; use GNATLLVM.Codegen;
with GNATLLVM.Compile; use GNATLLVM.Compile;

with Adabkend;
with Errout; use Errout;
with Lib;    use Lib;
with Opt;    use Opt;
with Types;  use Types;

package body Back_End is

   package GNAT2LLVM is new Adabkend
     (Product_Name       => "GNAT for LLVM",
      Copyright_Years    => "2013-2019",
      Driver             => GNAT_To_LLVM,
      Is_Back_End_Switch => Is_Back_End_Switch);

   procedure Scan_Compiler_Arguments renames GNAT2LLVM.Scan_Compiler_Arguments;

   -------------------
   -- Call_Back_End --
   -------------------

   procedure Call_Back_End (Mode : Back_End_Mode_Type) is
   begin
      --  Deal with case of generating SCIL, we should not be here unless
      --  debugging CodePeer mode in GNAT.

      if Generate_SCIL then
         Error_Msg_N ("'S'C'I'L generation not available", Cunit (Main_Unit));

         if CodePeer_Mode
           or else (Mode /= Generate_Object
                     and then not Back_Annotate_Rep_Info)
         then
            return;
         end if;
      end if;

      --  We should be here in GNATprove mode only when debugging GNAT. Do not
      --  call the back-end in that case, as it is not prepared to handle the
      --  special form of the tree obtained in GNATprove mode.

      if GNATprove_Mode then
         return;
      end if;

      --  Call the back end itself if it has work to do

      if Mode = Generate_Object or else Back_Annotate_Rep_Info then
         if Mode = Declarations_Only then
            Decls_Only      := True;
            Code_Generation := None;
         end if;

         GNAT2LLVM.Call_Back_End;
      end if;
   end Call_Back_End;

   -------------------------------
   -- Gen_Or_Update_Object_File --
   -------------------------------

   procedure Gen_Or_Update_Object_File is
   begin
      null;
   end Gen_Or_Update_Object_File;

begin
   --  Set the switches in Opt that we depend on

   Expand_Nonbinary_Modular_Ops    := True;
   Unnest_Subprogram_Mode          := True;
end Back_End;
