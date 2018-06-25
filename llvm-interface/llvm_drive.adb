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

with System;         use System;

with LLVM.Analysis;       use LLVM.Analysis;
with LLVM.Bit_Writer;     use LLVM.Bit_Writer;
with LLVM.Core;           use LLVM.Core;
with LLVM.Target_Machine; use LLVM.Target_Machine;

with Atree;    use Atree;
with Errout;   use Errout;
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint.C;  use Osint.C;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Stringt;
with Switch;   use Switch;

with Get_Targ; use Get_Targ;

with GNATLLVM;             use GNATLLVM;
with GNATLLVM.Blocks;
with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.DebugInfo;   use GNATLLVM.DebugInfo;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.Subprograms;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Variables;   use GNATLLVM.Variables;

package body LLVM_Drive is

   function Output_File_Name (Extension : String) return String;
   --  Return the name of the output file, using the given Extension

   ------------------
   -- GNAT_To_LLVM --
   ------------------

   procedure GNAT_To_LLVM (GNAT_Root : Node_Id) is
      Err_Msg : aliased Ptr_Err_Msg_Type;
   begin
      pragma Assert (Nkind (GNAT_Root) = N_Compilation_Unit);

      --  We can't use a qualified expression here because that will cause
      --  a temporary to be placed in our stack and if the array is very
      --  large, it will blow our stack.

      LLVM_Info_Map := new LLVM_Info_Array (First_Node_Id .. Last_Node_Id);
      for J in LLVM_Info_Map'Range loop
         LLVM_Info_Map (J) := Empty_LLVM_Info_Id;
      end loop;

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

      --  Initialize modules and handle duplicate globals

      Stringt.Unlock;
      GNATLLVM.Blocks.Initialize;
      GNATLLVM.DebugInfo.Initialize;
      GNATLLVM.Subprograms.Initialize;
      Detect_Duplicate_Global_Names;
      Stringt.Lock;

      --  Actually translate

      Emit (GNAT_Root);

      --  Output the translation

      Finalize_Debugging;
      if Verify_Module (Module, Print_Message_Action, Null_Address) then
         Error_Msg_N ("the backend generated bad LLVM code", GNAT_Root);
         if Code_Generation = Dump_IR then
            Dump_Module (Module);
         end if;

      else
         case Code_Generation is
            when Dump_IR =>
               Dump_Module (Module);
            when Write_BC =>
               declare
                  S : constant String := Output_File_Name (".bc");
               begin
                  if Integer (Write_Bitcode_To_File (Module, S)) /= 0 then
                     Error_Msg_N ("could not write `" & S & "`", GNAT_Root);
                  end if;
               end;

            when Write_IR =>
               declare
                  S : constant String := Output_File_Name (".ll");

               begin
                  if Print_Module_To_File (Module, S, Err_Msg'Address) then
                     Error_Msg_N
                       ("could not write `" & S & "`: " &
                        Get_LLVM_Error_Msg (Err_Msg),
                        GNAT_Root);
                  end if;
               end;

            when Write_Assembly =>
               declare
                  S : constant String := Output_File_Name (".s");
               begin
                  if Target_Machine_Emit_To_File
                    (Target_Machine, Module, S, Assembly_File, Err_Msg'Address)
                  then
                     Error_Msg_N
                       ("could not write `" & S & "`: " &
                        Get_LLVM_Error_Msg (Err_Msg), GNAT_Root);
                  end if;
               end;

            when Write_Object =>
               declare
                  S : constant String := Output_File_Name (".o");
               begin
                  if Target_Machine_Emit_To_File
                    (Target_Machine, Module, S, Object_File, Err_Msg'Address)
                  then
                     Error_Msg_N
                       ("could not write `" & S & "`: " &
                        Get_LLVM_Error_Msg (Err_Msg), GNAT_Root);
                  end if;
               end;
         end case;
      end if;

      --  Release the environment

      Dispose_Debugging;
      Dispose_Builder (IR_Builder);
      Dispose_Module  (Module);
   end GNAT_To_LLVM;

   ------------------------
   -- Is_Back_End_Switch --
   ------------------------

   function Is_Back_End_Switch (Switch : String) return Boolean is
      First : constant Positive := Switch'First + 1;
      Last  : constant Natural  := Switch_Last (Switch);

   begin
      if not Is_Switch (Switch) then
         return False;
      elsif Switch = "--dump-ir" then
         return True;
      elsif Switch = "--dump-bc" or else Switch = "--write-bc" then
         return True;
      elsif Switch = "-emit-llvm" then
         return True;
      elsif Switch = "-S" then
         return True;
      elsif Switch = "-g" then
         return True;
      elsif Switch = "-fstack-check" then
         return True;
      elsif Last > First + 7
        and then Switch (First .. First + 7) = "-target="
      then
         return True;
      elsif Last > First + 4
        and then Switch (First .. First + 4) = "llvm-"
      then
         return True;
      end if;

      --  For now we allow the -f/-m/-W/-w and -pipe switches, even
      --  though they will have no effect.
      --  This permits compatibility with existing scripts.

      return Switch (First) in 'f' | 'm' | 'W' | 'w'
        or else Switch (First .. Last) = "pipe";
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
