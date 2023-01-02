------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2008-2023, AdaCore                     --
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

with Ada.Directories; use Ada.Directories;
with GNAT.OS_Lib;     use GNAT.OS_Lib;
with Namet;           use Namet;
with Osint;           use Osint;
with Osint.C;         use Osint.C;
with Output;          use Output;

with Adabkend;
with Gnatvsn; use Gnatvsn;
with Errout;  use Errout;
with Lib;     use Lib;
with Opt;     use Opt;
with Options; use Options;
with Types;   use Types;

package body Back_End is

   package GNAT2LLVM is new Adabkend
     (Product_Name       => "GNAT for " & (if CCG then "CCG" else "LLVM"),
      Copyright_Years    => "2013-" & Current_Year,
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
      Obj_File_Name : constant String :=
        (if Output_File_Name_Present then Get_Output_Object_File_Name
         else Base_Name
                (Get_Name_String (Name_Id (Unit_File_Name (Main_Unit))))
                & Get_Target_Object_Suffix.all);
      Success       : Boolean;

   begin
      --  If we're to generate code, create an empty .o file is there isn't
      --  one already. Then set the time of that file to be the same as
      --  that of the .ali file.

      if Code_Generation = Write_Object then
         Close (Create_New_File (Obj_File_Name, Binary));
         Osint.C.Set_File_Name (ALI_Suffix.all);
         GNAT.OS_Lib.Copy_Time_Stamps
           (Name_Buffer (1 .. Name_Len), Obj_File_Name, Success);
      end if;

      --  If we're using JSON error messages, the GCC backend will write an
      --  empty JSON array and a newline. This doesn't relate to object files,
      --  but this is the only place that's called in the backend late enough.

      if Opt.JSON_Output then
         Set_Standard_Error;
         Write_Line ("[]");
         Set_Standard_Output;
      end if;

   end Gen_Or_Update_Object_File;

begin
   --  Set the switches in Opt that we depend on

   Back_End_Return_Slot         := True;
   Expand_Nonbinary_Modular_Ops := True;
   Unnest_Subprogram_Mode       := True;
   CCG_Mode                     := CCG;
end Back_End;
