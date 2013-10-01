with LLVM.Core; use LLVM.Core;
with GNATLLVM.Compile; use GNATLLVM.Compile;
with Utils;
with Sinfo; use Sinfo;
with Atree; use Atree;
with Namet; use Namet;
with Sem_Util; use Sem_Util;

package body LLVM_Drive is

   procedure GNAT_To_LLVM (GNAT_Root : Node_Id) is
      Context : Utils.Context :=
        (Ctx => Get_Global_Context, others => <>);
   begin
      pragma Assert (Nkind (GNAT_Root) = N_Compilation_Unit);
      Context.Bld := Create_Builder_In_Context (Context.Ctx);
      Context.Mdl := Module_Create_With_Name_In_Context
        (Get_Name_String (Chars (Defining_Entity (Unit (GNAT_Root)))),
         Context.Ctx);
      Compile (Context, GNAT_Root);

      Dispose_Builder (Context.Bld);
      Dump_Module (Context.Mdl);
      Dispose_Module (Context.Mdl);
   end GNAT_To_LLVM;

   function Is_Back_End_Switch (Switch : String) return Boolean is
      pragma Unreferenced (Switch);
   begin
      return False;
   end Is_Back_End_Switch;

end LLVM_Drive;
