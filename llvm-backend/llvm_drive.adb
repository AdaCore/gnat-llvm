with LLVM.Core; use LLVM.Core;

with Sinfo;    use Sinfo;
with Atree;    use Atree;
with Namet;    use Namet;
with Sem_Util; use Sem_Util;

with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.Environment; use GNATLLVM.Environment;

package body LLVM_Drive is

   procedure GNAT_To_LLVM (GNAT_Root : Node_Id) is
      Env : Environ :=
        (Ctx => Get_Global_Context, others => <>);
   begin
      pragma Assert (Nkind (GNAT_Root) = N_Compilation_Unit);

      --  Initialize the translation environment

      Env.Bld := Create_Builder_In_Context (Env.Ctx);
      Env.Mdl := Module_Create_With_Name_In_Context
        (Get_Name_String (Chars (Defining_Entity (Unit (GNAT_Root)))),
         Env.Ctx);

      --  Actually translate

      Compile (Env, GNAT_Root);

      --  Release the environment

      Dispose_Builder (Env.Bld);
      Dump_Module (Env.Mdl);
      Dispose_Module (Env.Mdl);
   end GNAT_To_LLVM;

   function Is_Back_End_Switch (Switch : String) return Boolean is
      pragma Unreferenced (Switch);
   begin
      return False;
   end Is_Back_End_Switch;

end LLVM_Drive;
