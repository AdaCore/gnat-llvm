------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2017, AdaCore                     --
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

with LLVM.Analysis; use LLVM.Analysis;
with LLVM.Types; use LLVM.Types;
with LLVM.Bit_Writer;
with LLVM.Core;     use LLVM.Core;

with Atree;    use Atree;
with Errout;   use Errout;
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint.C;  use Osint.C;
with Sem;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;

with Get_Targ;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.Nested_Subps; use GNATLLVM.Nested_Subps;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Utils;        use GNATLLVM.Utils;

package body LLVM_Drive is

   Dump_IR : Boolean := False;

   function Output_File_Name return String;
   --  Return the name of the output LLVM bitcode file

   ------------------
   -- GNAT_To_LLVM --
   ------------------

   procedure GNAT_To_LLVM (GNAT_Root : Node_Id) is
      Env : constant Environ :=
        new Environ_Record'(Ctx => Get_Global_Context, others => <>);

      procedure Emit_Library_Item (U : Node_Id);
      --  Generate code for the given library item

      -----------------------
      -- Emit_Library_Item --
      -----------------------

      procedure Emit_Library_Item (U : Node_Id) is
      begin
         --  Ignore Standard and ASCII packages

         if Sloc (U) <= Standard_Location then
            return;
         end if;

         --  Current_Unit := Get_Cunit_Unit_Number (Parent (U));
         --  Current_Source_File := Source_Index (Current_Unit);

         if In_Extended_Main_Code_Unit (U) then
            Set_In_Main_Unit (Env);
            Emit (Env, U);
         else
            --  Should we instead skip these units completely, and generate
            --  referenced items on the fly???

            Set_In_Main_Unit (Env, False);
            Env.Begin_Declarations;
            Emit (Env, U);
            Env.End_Declarations;
         end if;
      end Emit_Library_Item;

      procedure Walk_All_Units is
        new Sem.Walk_Library_Items (Action => Emit_Library_Item);

   begin
      pragma Assert (Nkind (GNAT_Root) = N_Compilation_Unit);

      --  Initialize the translation environment

      Env.Bld := Create_Builder_In_Context (Env.Ctx);
      Env.Mdl := Module_Create_With_Name_In_Context
        (Get_Name (Defining_Entity (Unit (GNAT_Root))),
         Env.Ctx);

      Compute_Static_Link_Descriptors (GNAT_Root, Env.S_Links);

      declare
         Void_Ptr_Type : constant Type_T := Pointer_Type (Int_Ty (8), 0);
         Size_Type     : constant Type_T := Int_Ty (64);
         C_Int_Type    : constant Type_T :=
           Int_Ty (Integer (Get_Targ.Get_Int_Size));

      begin
         --  Add malloc function to the env

         Env.Default_Alloc_Fn := Add_Function
           (Env.Mdl, "malloc",
            Fn_Ty ((1 => Size_Type), Void_Ptr_Type));

         --  Likewise for memcmp

         Env.Memory_Cmp_Fn := Add_Function
           (Env.Mdl, "memcmp",
            Fn_Ty ((Void_Ptr_Type, Void_Ptr_Type, Size_Type), C_Int_Type));
      end;

      Env.Push_Scope;
      Register_Builtin_Types (Env);

      --  Actually translate

      Walk_All_Units;

      --  Output the translation

      if Verify_Module (Env.Mdl, Print_Message_Action, Null_Address) /= 0
      then
         --  TODO??? Display the crash message, or something like this
         Error_Msg ("The backend generated bad LLVM code.", No_Location);

      else
         if Dump_IR then
            Dump_Module (Env.Mdl);
         else
            if LLVM.Bit_Writer.Write_Bitcode_To_File
              (Env.Mdl, Output_File_Name) /= 0
            then
               Error_Msg ("Could not write " & Output_File_Name, No_Location);
            end if;
         end if;
      end if;

      --  Release the environment

      Dispose_Builder (Env.Bld);
      Dispose_Module (Env.Mdl);
   end GNAT_To_LLVM;

   ------------------------
   -- Is_Back_End_Switch --
   ------------------------

   function Is_Back_End_Switch (Switch : String) return Boolean is
   begin
      if Switch = "--dump-ir" then
         Dump_IR := True;
         return True;
      end if;

      return False;
   end Is_Back_End_Switch;

   ----------------------
   -- Output_File_Name --
   ----------------------

   function Output_File_Name return String is
      Extension : constant String := ".bc";

   begin
      if not Output_File_Name_Present then
         return
           Ada.Directories.Base_Name
             (Get_Name_String (Name_Id (Unit_File_Name (Main_Unit))))
           & Extension;

      --  The Output file name was specified in the -o argument

      else
         --  Locate the last dot to remove the extension of native platforms
         --  (for example, file.o)

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
