pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;

package LLVM.Transforms_Aggressive_Inst_Combine is

  --===-- AggressiveInstCombine.h ---------------------------------*- C++ -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to libLLVMAggressiveInstCombine.a,    *|
  --|* which combines instructions to form fewer, simple IR instructions.         *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCTransformsAggressiveInstCombine Aggressive Instruction Combining transformations
  -- * @ingroup LLVMCTransforms
  -- *
  -- * @{
  --  

  --* See llvm::createAggressiveInstCombinerPass function.  
   procedure Add_Aggressive_Inst_Combiner_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/AggressiveInstCombine.h:31
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddAggressiveInstCombinerPass";

  --*
  -- * @}
  --  

end LLVM.Transforms_Aggressive_Inst_Combine;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
