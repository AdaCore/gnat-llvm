pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;

package LLVM.Linker is

  --===-- llvm-c/Linker.h - Module Linker C Interface -------------*- C++ -*-===*|*                                                                            *|
  --|
  --|*                     The LLVM Compiler Infrastructure                       *|
  --|*                                                                            *|
  --|* This file is distributed under the University of Illinois Open Source      *|
  --|* License. See LICENSE.TXT for details.                                      *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This file defines the C interface to the module/file/archive linker.       *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  -- This enum is provided for backwards-compatibility only. It has no effect.  
  -- This is the default behavior.  
  -- This option has been deprecated and
  --                                          should not be used.  

   type Linker_Mode_T is 
     (Linker_Destroy_Source,
      Linkerpreservesource_Removed);
   pragma Convention (C, Linker_Mode_T);  -- llvm-8.0.0.src/include/llvm-c/Linker.h:28

  -- Links the source module into the destination module. The source module is
  -- * destroyed.
  -- * The return value is true if an error occurred, false otherwise.
  -- * Use the diagnostic handler to get any diagnostic message.
  -- 

   function Link_Modules2
     (Dest : LLVM.Types.Module_T;
      Src  : LLVM.Types.Module_T)
      return Boolean;
   function Link_Modules2_C
     (Dest : LLVM.Types.Module_T;
      Src  : LLVM.Types.Module_T)
      return LLVM.Types.Bool_T;  -- llvm-8.0.0.src/include/llvm-c/Linker.h:35
   pragma Import (C, Link_Modules2_C, "LLVMLinkModules2");

end LLVM.Linker;

