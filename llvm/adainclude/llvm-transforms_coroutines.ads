pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with LLVM.Transforms_Pass_Manager_Builder;

package LLVM.Transforms_Coroutines is

  --===-- Coroutines.h - Coroutines Library C Interface -----------*- C++ -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to libLLVMCoroutines.a, which         *|
  --|* implements various scalar transformations of the LLVM IR.                  *|
  --|*                                                                            *|
  --|* Many exotic languages can interoperate with C code but have a harder time  *|
  --|* with C++ due to name mangling. So in addition to C, this interface enables *|
  --|* tools written in such languages.                                           *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCTransformsCoroutines Coroutine transformations
  -- * @ingroup LLVMCTransforms
  -- *
  -- * @{
  --  

  --* See llvm::createCoroEarlyLegacyPass function.  
   procedure Add_Coro_Early_Pass (PM : LLVM.Types.Pass_Manager_T)  -- llvm-12.0.0.src/include/llvm-c/Transforms/Coroutines.h:36
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddCoroEarlyPass";

  --* See llvm::createCoroSplitLegacyPass function.  
   procedure Add_Coro_Split_Pass (PM : LLVM.Types.Pass_Manager_T)  -- llvm-12.0.0.src/include/llvm-c/Transforms/Coroutines.h:39
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddCoroSplitPass";

  --* See llvm::createCoroElideLegacyPass function.  
   procedure Add_Coro_Elide_Pass (PM : LLVM.Types.Pass_Manager_T)  -- llvm-12.0.0.src/include/llvm-c/Transforms/Coroutines.h:42
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddCoroElidePass";

  --* See llvm::createCoroCleanupLegacyPass function.  
   procedure Add_Coro_Cleanup_Pass (PM : LLVM.Types.Pass_Manager_T)  -- llvm-12.0.0.src/include/llvm-c/Transforms/Coroutines.h:45
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddCoroCleanupPass";

  --* See llvm::addCoroutinePassesToExtensionPoints.  
   procedure Pass_Manager_Builder_Add_Coroutine_Passes_To_Extension_Points (PMB : LLVM.Transforms_Pass_Manager_Builder.Pass_Manager_Builder_T)  -- llvm-12.0.0.src/include/llvm-c/Transforms/Coroutines.h:48
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPassManagerBuilderAddCoroutinePassesToExtensionPoints";

  --*
  -- * @}
  --  

end LLVM.Transforms_Coroutines;

