with Ada.Directories;
with Interfaces.C; use Interfaces.C;
with System;       use System;

with LLVM.Analysis; use LLVM.Analysis;
with LLVM.Bit_Writer;
with LLVM.Core;     use LLVM.Core;

with Atree;    use Atree;
with Errout;   use Errout;
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint.C;  use Osint.C;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;

with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

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
   begin
      pragma Assert (Nkind (GNAT_Root) = N_Compilation_Unit);

      --  Initialize the translation environment

      Env.Bld := (Bld => Create_Builder_In_Context (Env.Ctx));
      Env.Mdl := Module_Create_With_Name_In_Context
        (Get_Name (Defining_Entity (Unit (GNAT_Root))),
         Env.Ctx);

      --  Add malloc function to the env

      Env.Default_Alloc_Fn := Add_Function
        (Env.Mdl, "malloc",
         Fn_Ty ((1 => Int_Ty (64)), Pointer_Type (Int_Ty (8), 0)));

      Env.Push_Scope;
      Register_Builtin_Types (Env);

      --  Actually translate

      Emit (Env, Unit (GNAT_Root));

      --  Output the translation

      if Verify_Module (Env.Mdl, Print_Message_Action, Null_Address) then
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

      Env.Bld.Dispose_Builder;
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
