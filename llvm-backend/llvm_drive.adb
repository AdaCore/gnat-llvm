with Ada.Text_IO; use Ada.Text_IO;

package body LLVM_Drive is

   procedure GNAT_To_LLVM (GNAT_Root : Node_Id) is
      pragma Unreferenced (GNAT_Root);
   begin
      Put_Line ("Hello, world!");
   end GNAT_To_LLVM;

   function Is_Back_End_Switch (Switch : String) return Boolean is
      pragma Unreferenced (Switch);
   begin
      return False;
   end Is_Back_End_Switch;

end LLVM_Drive;
