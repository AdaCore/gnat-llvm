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

with System;       use System;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with LLVM.Analysis;   use LLVM.Analysis;
with LLVM.Target;     use LLVM.Target;
with LLVM.Types;      use LLVM.Types;
with LLVM.Bit_Writer; use LLVM.Bit_Writer;
with LLVM.Core;       use LLVM.Core;

with Atree;    use Atree;
with Errout;   use Errout;
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint.C;  use Osint.C;
with Sem;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Stand;    use Stand;
with Switch;   use Switch;

with Get_Targ; use Get_Targ;

with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Utils;        use GNATLLVM.Utils;

package body LLVM_Drive is

   Output_Assembly : Boolean := False;
   --  True if -S was specified

   Emit_LLVM       : Boolean := False;
   --  True if -emit-llvm was specified

   type Code_Generation_Kind is
     (Dump_IR, Write_IR, Write_BC, Write_Assembly, Write_Object);

   Code_Generation : Code_Generation_Kind := Write_Object;
   --  Type of code generation we're doing

   function Output_File_Name (Extension : String) return String;
   --  Return the name of the output file, using the given Extension

   ------------------
   -- GNAT_To_LLVM --
   ------------------

   procedure GNAT_To_LLVM (GNAT_Root : Node_Id) is
      Env : constant Environ :=
        new Environ_Record'(Max_Nodes => Last_Node_Id,
                            Ctx => Get_Global_Context,
                            Func => No_GL_Value,
                            others => <>);
      Result : Integer;

      procedure Emit_Lib_Item (N : Node_Id);
      procedure Emit_Lib_Item (N : Node_Id) is
      begin
         Emit_Library_Item (Env, N);
      end Emit_Lib_Item;
      --  Wrapper to encapsulate Env in call

      procedure Walk_All_Units is
        new Sem.Walk_Library_Items (Action => Emit_Lib_Item);

   begin
      pragma Assert (Nkind (GNAT_Root) = N_Compilation_Unit);

      --  Finalize our compilation mode now that all switches are parsed

      if Emit_LLVM then
         Code_Generation := (if Output_Assembly then Write_IR else Write_BC);
      elsif Output_Assembly then
         Code_Generation := Write_Assembly;
      end if;

      --  Initialize the translation environment

      Env.Bld := Create_Builder_In_Context (Env.Ctx);
      Env.MDBld := Create_MDBuilder_In_Context (Env.Ctx);
      Env.TBAA_Root := Create_TBAA_Root (Env.MDBld);
      Env.Mdl := Module_Create_With_Name_In_Context
        (Get_Name (Defining_Entity (Unit (GNAT_Root))),
         Env.Ctx);
      Result := LLVM_Init_Module
        (Env.Mdl,
         Get_Name_String (Name_Id (Unit_File_Name (Main_Unit))));
      pragma Assert (Result = 0);
      Env.Module_Data_Layout := Get_Module_Data_Layout (Env.Mdl);
      Env.LLVM_Info := (others => Empty_LLVM_Info_Id);

      if Emit_Debug_Info then
         Env.DIBld := Create_Debug_Builder (Env.Mdl);
         Env.Debug_Compile_Unit :=
           Create_Debug_Compile_Unit
           (Env.DIBld, Get_Debug_File_Node (Env.DIBld, Main_Source_File));
      end if;

      LLVM_Info_Table.Increment_Last;
      --  Ensure the first LLVM_Info entry isn't Empty_LLVM_Info_Id

      declare
         Void_Ptr_Type : constant Type_T := Pointer_Type (Int_Ty (8), 0);
         Size_Type     : constant Type_T :=
           Int_Ty (Integer (Get_Pointer_Size));
         C_Int_Type    : constant Type_T := Int_Ty (Integer (Get_Int_Size));
         Memcopy_Type  : constant Type_T := Fn_Ty
           ((Void_Ptr_Type, Void_Ptr_Type, Size_Type, Int_Ty (32), Int_Ty (1)),
            Void_Type_In_Context (Env.Ctx));
         Memset_Type   : constant Type_T := Fn_Ty
           ((Void_Ptr_Type, Int_Ty (8), Size_Type, Int_Ty (32), Int_Ty (1)),
            Void_Type_In_Context (Env.Ctx));

      begin
         Env.LLVM_Size_Type := Size_Type;

         --  Find the integer type corresponding to the size of a pointer
         --  and use that for our Size Type.

         if Get_Pointer_Size = Get_Long_Long_Size then
            Env.Size_Type := Standard_Long_Long_Integer;
         elsif Get_Pointer_Size = Get_Long_Size then
            Env.Size_Type := Standard_Long_Integer;
         else
            Env.Size_Type := Standard_Integer;
         end if;

         pragma Assert (Create_Type (Env, Env.Size_Type) = Env.LLVM_Size_Type);

         --  Likewise for the 32-bit integer type

         if Get_Long_Long_Size = 32 then
            Env.Int_32_Type := Standard_Long_Long_Integer;
         elsif Get_Long_Size = 32 then
            Env.Int_32_Type := Standard_Long_Integer;
         else
            Env.Int_32_Type := Standard_Integer;
         end if;

         --  Add malloc function to the env

         Env.Default_Alloc_Fn := Add_Function
           (Env.Mdl, "malloc",
            Fn_Ty ((1 => Size_Type), Void_Ptr_Type));

         --  Likewise for memcmp/memcpy/memmove/memset

         Env.Memory_Cmp_Fn := Add_Function
           (Env.Mdl, "memcmp",
            Fn_Ty ((Void_Ptr_Type, Void_Ptr_Type, Size_Type), C_Int_Type));

         if Get_Targ.Get_Pointer_Size = 32 then
            Env.Memory_Copy_Fn := Add_Function
              (Env.Mdl, "llvm.memcpy.p0i8.p0i8.i32", Memcopy_Type);
            Env.Memory_Move_Fn := Add_Function
              (Env.Mdl, "llvm.memmove.p0i8.p0i8.i32", Memcopy_Type);
            Env.Memory_Set_Fn := Add_Function
              (Env.Mdl, "llvm.memset.p0i8.i32", Memset_Type);
         else
            pragma Assert (Get_Targ.Get_Pointer_Size = 64);

            Env.Memory_Copy_Fn := Add_Function
              (Env.Mdl, "llvm.memcpy.p0i8.p0i8.i64", Memcopy_Type);
            Env.Memory_Move_Fn := Add_Function
              (Env.Mdl, "llvm.memmove.p0i8.p0i8.i64", Memcopy_Type);
            Env.Memory_Set_Fn := Add_Function
              (Env.Mdl, "llvm.memset.p0i8.i64", Memset_Type);
         end if;

         --  Likewise for stacksave/stackrestore

         Env.Stack_Save_Fn := Add_Function
           (Env.Mdl, "llvm.stacksave",
            Fn_Ty ((1 .. 0 => <>), Void_Ptr_Type));
         Env.Stack_Restore_Fn := Add_Function
           (Env.Mdl,
            "llvm.stackrestore",
            Fn_Ty ((1 => Void_Ptr_Type), Void_Type_In_Context (Env.Ctx)));

         --  Likewise for __gnat_last_chance_handler

         Env.LCH_Fn := Add_Function
           (Env.Mdl,
            "__gnat_last_chance_handler",
            Fn_Ty ((Void_Ptr_Type, C_Int_Type),
                   Void_Type_In_Context (Env.Ctx)));
      end;

      --  Actually translate

      Walk_All_Units;

      --  Output the translation

      if Emit_Debug_Info then
         Finalize_Debug_Info (Env.DIBld);
      end if;

      if Verify_Module (Env.Mdl, Print_Message_Action, Null_Address) then
         Error_Msg_N ("the backend generated bad `LLVM` code", GNAT_Root);

      else
         case Code_Generation is
            when Dump_IR =>
               Dump_Module (Env.Mdl);
            when Write_BC =>
               declare
                  S : constant String := Output_File_Name (".bc");
               begin
                  if Write_Bitcode_To_File (Env.Mdl, S) /= 0
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
                  if Print_Module_To_File (Env.Mdl, S, Ptr_Err_Msg'Address)
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
                  if LLVM_Write_Module (Env.Mdl, False, S) /= 0 then
                     Error_Msg_N ("could not write `" & S & "`", GNAT_Root);
                  end if;
               end;

            when Write_Object =>
               declare
                  S : constant String := Output_File_Name (".o");
               begin
                  if LLVM_Write_Module (Env.Mdl, True, S) /= 0 then
                     Error_Msg_N ("could not write `" & S & "`", GNAT_Root);
                  end if;
               end;
         end case;
      end if;

      --  Release the environment

      Dispose_Builder (Env.Bld);
      Dispose_Module (Env.Mdl);
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
