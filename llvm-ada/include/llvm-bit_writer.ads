pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with Interfaces.C.Strings;

package LLVM.Bit_Writer is

   function Write_Bitcode_To_File
     (M    : LLVM.Types.Module_T;
      Path : String)
      return int;
   function Write_Bitcode_To_File_C
     (M    : LLVM.Types.Module_T;
      Path : Interfaces.C.Strings.chars_ptr)
      return int;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/BitWriter.h:38
   pragma Import (C, Write_Bitcode_To_File_C, "LLVMWriteBitcodeToFile");

   function Write_Bitcode_To_FD
     (M : LLVM.Types.Module_T;
      FD : int;
      Should_Close : int;
      Unbuffered : int) return int;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/BitWriter.h:41
   pragma Import (C, Write_Bitcode_To_FD, "LLVMWriteBitcodeToFD");

   function Write_Bitcode_To_File_Handle (M : LLVM.Types.Module_T; Handle : int) return int;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/BitWriter.h:46
   pragma Import (C, Write_Bitcode_To_File_Handle, "LLVMWriteBitcodeToFileHandle");

   function Write_Bitcode_To_Memory_Buffer (M : LLVM.Types.Module_T) return LLVM.Types.Memory_Buffer_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/BitWriter.h:49
   pragma Import (C, Write_Bitcode_To_Memory_Buffer, "LLVMWriteBitcodeToMemoryBuffer");

end LLVM.Bit_Writer;

