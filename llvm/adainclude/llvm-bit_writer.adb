pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Bit_Writer is

   function Write_Bitcode_To_File
     (M    : LLVM.Types.Module_T;
      Path : Interfaces.C.Strings.chars_ptr)
      return int
   with Import => True,
        Convention => C,
        External_Name => "LLVMWriteBitcodeToFile";
   function Write_Bitcode_To_File
     (M    : LLVM.Types.Module_T;
      Path : String)
      return int
   is
      Return_Value : int;
      Path_Array   : aliased char_array := To_C (Path);
      Path_String  : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      Return_Value := Write_Bitcode_To_File (M, Path_String);
      return Return_Value;
   end Write_Bitcode_To_File;

end LLVM.Bit_Writer;
