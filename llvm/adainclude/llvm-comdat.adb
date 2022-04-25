pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Comdat is

   function Get_Or_Insert_Comdat
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Comdat_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetOrInsertComdat";
   function Get_Or_Insert_Comdat
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Comdat_T
   is
      Return_Value : LLVM.Types.Comdat_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_Or_Insert_Comdat (M, Name_String);
      return Return_Value;
   end Get_Or_Insert_Comdat;

end LLVM.Comdat;
