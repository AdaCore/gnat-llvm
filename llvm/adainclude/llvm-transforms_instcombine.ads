pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;

package LLVM.Transforms_Instcombine is

  --===-- Scalar.h - Scalar Transformation Library C Interface ----*- C++ -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to libLLVMInstCombine.a, which        *|
  --|* combines instructions to form fewer, simple IR instructions.               *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCTransformsInstCombine Instruction Combining transformations
  -- * @ingroup LLVMCTransforms
  -- *
  -- * @{
  --  

  --* See llvm::createInstructionCombiningPass function.  
   procedure Add_Instruction_Combining_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/InstCombine.h:31
   pragma Import (C, Add_Instruction_Combining_Pass, "LLVMAddInstructionCombiningPass");

  --*
  -- * @}
  --  

end LLVM.Transforms_Instcombine;

