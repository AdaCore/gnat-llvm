pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Bit_Writer is

   function Write_Bitcode_To_File
     (M    : LLVM.Types.Module_T;
      Path : String)
      return int
   is
      Path_Array  : aliased char_array := To_C (Path);
      Path_String : constant chars_ptr := To_Chars_Ptr (Path_Array'Unchecked_Access);
   begin
      return Write_Bitcode_To_File_C (M, Path_String);
   end Write_Bitcode_To_File;

end LLVM.Bit_Writer;
