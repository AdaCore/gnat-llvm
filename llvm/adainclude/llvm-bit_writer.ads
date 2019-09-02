pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with Interfaces.C.Strings;

package LLVM.Bit_Writer is

  --===-- llvm-c/BitWriter.h - BitWriter Library C Interface ------*- C++ -*-===*|*                                                                            *|
  --|
  --|*                     The LLVM Compiler Infrastructure                       *|
  --|*                                                                            *|
  --|* This file is distributed under the University of Illinois Open Source      *|
  --|* License. See LICENSE.TXT for details.                                      *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to libLLVMBitWriter.a, which          *|
  --|* implements output of the LLVM bitcode format.                              *|
  --|*                                                                            *|
  --|* Many exotic languages can interoperate with C code but have a harder time  *|
  --|* with C++ due to name mangling. So in addition to C, this interface enables *|
  --|* tools written in such languages.                                           *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCBitWriter Bit Writer
  -- * @ingroup LLVMC
  -- *
  -- * @{
  --  

  --===-- Operations on modules ---------------------------------------------=== 
  --* Writes a module to the specified path. Returns 0 on success.  
   function Write_Bitcode_To_File
     (M    : LLVM.Types.Module_T;
      Path : String)
      return int;
   function Write_Bitcode_To_File_C
     (M    : LLVM.Types.Module_T;
      Path : Interfaces.C.Strings.chars_ptr)
      return int;  -- llvm-8.0.0.src/include/llvm-c/BitWriter.h:38
   pragma Import (C, Write_Bitcode_To_File_C, "LLVMWriteBitcodeToFile");

  --* Writes a module to an open file descriptor. Returns 0 on success.  
   function Write_Bitcode_To_FD
     (M : LLVM.Types.Module_T;
      FD : int;
      Should_Close : int;
      Unbuffered : int) return int;  -- llvm-8.0.0.src/include/llvm-c/BitWriter.h:41
   pragma Import (C, Write_Bitcode_To_FD, "LLVMWriteBitcodeToFD");

  --* Deprecated for LLVMWriteBitcodeToFD. Writes a module to an open file
  --    descriptor. Returns 0 on success. Closes the Handle.  

   function Write_Bitcode_To_File_Handle (M : LLVM.Types.Module_T; Handle : int) return int;  -- llvm-8.0.0.src/include/llvm-c/BitWriter.h:46
   pragma Import (C, Write_Bitcode_To_File_Handle, "LLVMWriteBitcodeToFileHandle");

  --* Writes a module to a new memory buffer and returns it.  
   function Write_Bitcode_To_Memory_Buffer (M : LLVM.Types.Module_T) return LLVM.Types.Memory_Buffer_T;  -- llvm-8.0.0.src/include/llvm-c/BitWriter.h:49
   pragma Import (C, Write_Bitcode_To_Memory_Buffer, "LLVMWriteBitcodeToMemoryBuffer");

  --*
  -- * @}
  --  

end LLVM.Bit_Writer;

