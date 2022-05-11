pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;

package LLVM.Linker is

  --===-- llvm-c/Linker.h - Module Linker C Interface -------------*- C++ -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This file defines the C interface to the module/file/archive linker.       *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCCoreLinker Linker
  -- * @ingroup LLVMCCore
  -- *
  -- * @{
  --  

  -- This enum is provided for backwards-compatibility only. It has no effect.  
  -- This is the default behavior.  
  -- This option has been deprecated and
  --                                          should not be used.  

   type Linker_Mode_T is 
     (Linker_Destroy_Source,
      Linker_Preserve_Source_Removed)
   with Convention => C;  -- install/include/llvm-c/Linker.h:34

  -- Links the source module into the destination module. The source module is
  -- * destroyed.
  -- * The return value is true if an error occurred, false otherwise.
  -- * Use the diagnostic handler to get any diagnostic message.
  -- 

function Link_Modules_2
     (Dest : LLVM.Types.Module_T;
      Src  : LLVM.Types.Module_T)
      return Boolean;

  --*
  -- * @}
  --  

end LLVM.Linker;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
