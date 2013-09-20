with Types; use Types;

package LLVM_Drive is

   procedure GNAT_To_LLVM (GNAT_Root : Node_Id);

   function Is_Back_End_Switch (Switch : String) return Boolean;

end LLVM_Drive;
