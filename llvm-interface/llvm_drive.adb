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

with Ada.Directories;

with Interfaces.C; use Interfaces.C;

with System;         use System;
with System.Strings; use System.Strings;

with LLVM.Analysis;   use LLVM.Analysis;
with LLVM.Target;     use LLVM.Target;
with LLVM.Bit_Writer; use LLVM.Bit_Writer;
with LLVM.Core;       use LLVM.Core;

with Atree;    use Atree;
with Errout;   use Errout;
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint.C;  use Osint.C;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Switch;   use Switch;

with Get_Targ; use Get_Targ;

with GNATLLVM;             use GNATLLVM;
with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.DebugInfo;   use GNATLLVM.DebugInfo;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Utils;       use GNATLLVM.Utils;
with GNATLLVM.Variables;   use GNATLLVM.Variables;
with GNATLLVM.Wrapper;     use GNATLLVM.Wrapper;

package body LLVM_Drive is

   Output_Assembly : Boolean := False;
   --  True if -S was specified

   Emit_LLVM       : Boolean := False;
   --  True if -emit-llvm was specified

   type Code_Generation_Kind is
     (Dump_IR, Write_IR, Write_BC, Write_Assembly, Write_Object);

   Code_Generation : Code_Generation_Kind := Write_Object;
   --  Type of code generation we're doing

   Target : String_Access := new String'("");

   function Output_File_Name (Extension : String) return String;
   --  Return the name of the output file, using the given Extension

   ------------------
   -- GNAT_To_LLVM --
   ------------------

   procedure GNAT_To_LLVM (GNAT_Root : Node_Id) is
      Result : Integer;

   begin
      pragma Assert (Nkind (GNAT_Root) = N_Compilation_Unit);

      --  Finalize our compilation mode now that all switches are parsed

      if Emit_LLVM then
         Code_Generation := (if Output_Assembly then Write_IR else Write_BC);
      elsif Output_Assembly then
         Code_Generation := Write_Assembly;
      end if;

      --  Initialize the translation environment

      LLVM_Context := Get_Global_Context;
      IR_Builder   := Create_Builder_In_Context (LLVM_Context);
      MD_Builder   := Create_MDBuilder_In_Context (LLVM_Context);
      LLVM_Module  := Module_Create_With_Name_In_Context
        (Get_Name (Defining_Entity (Unit (GNAT_Root))), LLVM_Context);

      Result := LLVM_Init_Module
        (LLVM_Module,
         Get_Name_String (Name_Id (Unit_File_Name (Main_Unit))),
         Target.all);

      if Result /= 0 then
         if Target.all = "" then
            Error_Msg_N ("error initializing LLVM module", GNAT_Root);
         else
            Error_Msg_N
              ("error initializing LLVM module for target " & Target.all,
               GNAT_Root);
         end if;

         return;
      end if;

      TBAA_Root          := Create_TBAA_Root (MD_Builder);
      Module_Data_Layout := Get_Module_Data_Layout (LLVM_Module);
      LLVM_Info_Map := new LLVM_Info_Array'
        (First_Node_Id .. Last_Node_Id => Empty_LLVM_Info_Id);

      LLVM_Info_Table.Increment_Last;
      --  Ensure the first LLVM_Info entry isn't Empty_LLVM_Info_Id

      Void_Ptr_Type  := Create_Type (Standard_A_Char);

      --  Find the integer type corresponding to the size of a pointer
      --  and use that for our Size Type.

      if Get_Pointer_Size = Get_Long_Long_Size then
         Size_Type := Standard_Long_Long_Integer;
      elsif Get_Pointer_Size = Get_Long_Size then
         Size_Type := Standard_Long_Integer;
      else
         Size_Type := Standard_Integer;
      end if;

      LLVM_Size_Type := Create_Type (Size_Type);

      --  Likewise for the 32-bit integer type

      if Get_Long_Long_Size = 32 then
         Int_32_Type := Standard_Long_Long_Integer;
      elsif Get_Long_Size = 32 then
         Int_32_Type := Standard_Long_Integer;
      else
         Int_32_Type := Standard_Integer;
      end if;

      --  Actually translate

      Detect_Duplicate_Global_Names;
      Initialize_Debugging;
      Emit (GNAT_Root);

      --  Output the translation

      Finalize_Debugging;
      if Verify_Module (LLVM_Module, Print_Message_Action, Null_Address) then
         Error_Msg_N ("the backend generated bad LLVM code", GNAT_Root);
         if Code_Generation = Dump_IR then
            Dump_Module (LLVM_Module);
         end if;

      else
         case Code_Generation is
            when Dump_IR =>
               Dump_Module (LLVM_Module);
            when Write_BC =>
               declare
                  S : constant String := Output_File_Name (".bc");
               begin
                  if Write_Bitcode_To_File (LLVM_Module, S) /= 0
                  then
                     Error_Msg_N ("could not write `" & S & "`", GNAT_Root);
                  end if;
               end;

            when Write_IR =>
               declare
                  subtype Err_Msg_Type is String (1 .. 1000);
                  S              : constant String := Output_File_Name (".ll");
                  Ptr_Err_Msg    : access Err_Msg_Type;
                  Err_Msg_Length : Integer := Err_Msg_Type'Length;

               begin
                  if Print_Module_To_File (LLVM_Module, S, Ptr_Err_Msg'Address)
                  then
                     for I in Err_Msg_Type'Range loop
                        if Ptr_Err_Msg.all (I) = ASCII.NUL then
                           Err_Msg_Length := I - 1;
                           exit;
                        end if;
                     end loop;

                     Error_Msg_N
                       ("could not write `" & S & "`: " &
                          Ptr_Err_Msg.all (1 .. Err_Msg_Length), GNAT_Root);
                  end if;
               end;

            when Write_Assembly =>
               declare
                  S : constant String := Output_File_Name (".s");
               begin
                  if LLVM_Write_Module (LLVM_Module, False, S) /= 0 then
                     Error_Msg_N ("could not write `" & S & "`", GNAT_Root);
                  end if;
               end;

            when Write_Object =>
               declare
                  S : constant String := Output_File_Name (".o");
               begin
                  if LLVM_Write_Module (LLVM_Module, True, S) /= 0 then
                     Error_Msg_N ("could not write `" & S & "`", GNAT_Root);
                  end if;
               end;
         end case;
      end if;

      --  Release the environment

      Dispose_Builder (IR_Builder);
      Dispose_Module (LLVM_Module);
   end GNAT_To_LLVM;

   ------------------------
   -- Is_Back_End_Switch --
   ------------------------

   function Is_Back_End_Switch (Switch : String) return Boolean is
      First : constant Positive := Switch'First + 1;
      Last  : constant Natural  := Switch_Last (Switch);

   begin
      if Switch = "--dump-ir" then
         Code_Generation := Dump_IR;
         return True;
      elsif Switch = "--dump-bc" or else Switch = "--write-bc" then
         Code_Generation := Write_BC;
         return True;
      elsif Switch'Length > 9
        and then Switch (Switch'First .. Switch'First + 8) = "--target="
      then
         Target := new String'(Switch (First + 8 .. Last));
         return True;
      elsif Switch = "-emit-llvm" then
         Emit_LLVM := True;
         return True;
      elsif Switch = "-S" then
         Output_Assembly := True;
         return True;
      elsif Switch = "-g" then
         Emit_Debug_Info := True;
      end if;

      --  For now we allow the -g/-O/-f/-m/-W/-w and -pipe switches, even
      --  though they will have no effect.
      --  This permits compatibility with existing scripts.
      --  ??? Should take into account -g and -O

      return
        Is_Switch (Switch)
          and then (Switch (First) in 'f' | 'g' | 'm' | 'O' | 'W' | 'w'
                    or else Switch (First .. Last) = "pipe");
   end Is_Back_End_Switch;

   ----------------------
   -- Output_File_Name --
   ----------------------

   function Output_File_Name (Extension : String) return String is
   begin
      if not Output_File_Name_Present then
         return
           Ada.Directories.Base_Name
             (Get_Name_String (Name_Id (Unit_File_Name (Main_Unit))))
           & Extension;

      --  The Output file name was specified in the -o argument

      else
         --  Locate the last dot to remove the extension of native platforms
         --  (for example, file.o).

         declare
            S : constant String := Get_Output_Object_File_Name;
         begin
            for J in reverse S'Range loop
               if S (J) = '.' then
                  return S (S'First .. J - 1) & Extension;
               end if;
            end loop;

            return S & Extension;
         end;
      end if;
   end Output_File_Name;

end LLVM_Drive;
