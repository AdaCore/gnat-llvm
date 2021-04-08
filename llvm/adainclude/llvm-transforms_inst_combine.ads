pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;

package LLVM.Transforms_Inst_Combine is

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
   procedure Add_Instruction_Combining_Pass (PM : LLVM.Types.Pass_Manager_T)  -- llvm-11.0.1.src/include/llvm-c/Transforms/InstCombine.h:31
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddInstructionCombiningPass";

  --*
  -- * @}
  --  

end LLVM.Transforms_Inst_Combine;

