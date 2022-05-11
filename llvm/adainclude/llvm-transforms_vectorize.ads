pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;

package LLVM.Transforms_Vectorize is

  --===---------------------------Vectorize.h --------------------- -*- C -*-===*|*===----------- Vectorization Transformation Library C Interface ---------===*|
  --|
  --|*                                                                            *|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to libLLVMVectorize.a, which          *|
  --|* implements various vectorization transformations of the LLVM IR.           *|
  --|*                                                                            *|
  --|* Many exotic languages can interoperate with C code but have a harder time  *|
  --|* with C++ due to name mangling. So in addition to C, this interface enables *|
  --|* tools written in such languages.                                           *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCTransformsVectorize Vectorization transformations
  -- * @ingroup LLVMCTransforms
  -- *
  -- * @{
  --  

  --* See llvm::createLoopVectorizePass function.  
   procedure Add_Loop_Vectorize_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Vectorize.h:36
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddLoopVectorizePass";

  --* See llvm::createSLPVectorizerPass function.  
   procedure Add_SLP_Vectorize_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Vectorize.h:39
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddSLPVectorizePass";

  --*
  -- * @}
  --  

end LLVM.Transforms_Vectorize;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
