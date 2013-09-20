with LLVM.Core; use LLVM.Core;

package body LLVM_Drive is

   procedure GNAT_To_LLVM (GNAT_Root : Node_Id) is
      pragma Unreferenced (GNAT_Root);

      Ctx     : constant Context_T := Get_Global_Context;
      Builder : constant Builder_T := Create_Builder_In_Context (Ctx);
      Module  : constant Module_T  :=
         Module_Create_With_Name_In_Context ("My module", Ctx);
   begin
      Dump_Module (Module);
      Dispose_Builder (Builder);
      Dispose_Module (Module);
   end GNAT_To_LLVM;

   function Is_Back_End_Switch (Switch : String) return Boolean is
      pragma Unreferenced (Switch);
   begin
      return False;
   end Is_Back_End_Switch;

end LLVM_Drive;
