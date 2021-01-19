pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with LLVM.Types;

package LLVM.Transforms_Passmanagerbuilder is

  --===-- llvm-c/Transform/PassManagerBuilder.h - PMB C Interface ---*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to the PassManagerBuilder class.      *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

   --  skipped empty struct LLVMOpaquePassManagerBuilder

   type Pass_Manager_Builder_T is new System.Address;  -- llvm-11.0.1.src/include/llvm-c/Transforms/PassManagerBuilder.h:20

  --*
  -- * @defgroup LLVMCTransformsPassManagerBuilder Pass manager builder
  -- * @ingroup LLVMCTransforms
  -- *
  -- * @{
  --  

  --* See llvm::PassManagerBuilder.  
   function Pass_Manager_Builder_Create return Pass_Manager_Builder_T;  -- llvm-11.0.1.src/include/llvm-c/Transforms/PassManagerBuilder.h:32
   pragma Import (C, Pass_Manager_Builder_Create, "LLVMPassManagerBuilderCreate");

   procedure Pass_Manager_Builder_Dispose (PMB : Pass_Manager_Builder_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/PassManagerBuilder.h:33
   pragma Import (C, Pass_Manager_Builder_Dispose, "LLVMPassManagerBuilderDispose");

  --* See llvm::PassManagerBuilder::OptLevel.  
   procedure Pass_Manager_Builder_Set_Opt_Level (PMB : Pass_Manager_Builder_T; Opt_Level : unsigned);  -- llvm-11.0.1.src/include/llvm-c/Transforms/PassManagerBuilder.h:37
   pragma Import (C, Pass_Manager_Builder_Set_Opt_Level, "LLVMPassManagerBuilderSetOptLevel");

  --* See llvm::PassManagerBuilder::SizeLevel.  
   procedure Pass_Manager_Builder_Set_Size_Level (PMB : Pass_Manager_Builder_T; Size_Level : unsigned);  -- llvm-11.0.1.src/include/llvm-c/Transforms/PassManagerBuilder.h:42
   pragma Import (C, Pass_Manager_Builder_Set_Size_Level, "LLVMPassManagerBuilderSetSizeLevel");

  --* See llvm::PassManagerBuilder::DisableUnitAtATime.  
   procedure Pass_Manager_Set_Disable_Unit_At_A_Time
     (PMB   : Pass_Manager_Builder_T;
      Value : Boolean);
   procedure Pass_Manager_Builder_Set_Disable_Unit_At_A_Time_C
     (PMB   : Pass_Manager_Builder_T;
      Value : LLVM.Types.Bool_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/PassManagerBuilder.h:47
   pragma Import (C, Pass_Manager_Builder_Set_Disable_Unit_At_A_Time_C, "LLVMPassManagerBuilderSetDisableUnitAtATime");

  --* See llvm::PassManagerBuilder::DisableUnrollLoops.  
   procedure Pass_Manager_Set_Disable_Unroll_Loops
     (PMB   : Pass_Manager_Builder_T;
      Value : Boolean);
   procedure Pass_Manager_Builder_Set_Disable_Unroll_Loops_C
     (PMB   : Pass_Manager_Builder_T;
      Value : LLVM.Types.Bool_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/PassManagerBuilder.h:52
   pragma Import (C, Pass_Manager_Builder_Set_Disable_Unroll_Loops_C, "LLVMPassManagerBuilderSetDisableUnrollLoops");

  --* See llvm::PassManagerBuilder::DisableSimplifyLibCalls  
   procedure Pass_Manager_Set_Disable_Simplify_Lib_Calls
     (PMB   : Pass_Manager_Builder_T;
      Value : Boolean);
   procedure Pass_Manager_Builder_Set_Disable_Simplify_Lib_Calls_C
     (PMB   : Pass_Manager_Builder_T;
      Value : LLVM.Types.Bool_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/PassManagerBuilder.h:57
   pragma Import (C, Pass_Manager_Builder_Set_Disable_Simplify_Lib_Calls_C, "LLVMPassManagerBuilderSetDisableSimplifyLibCalls");

  --* See llvm::PassManagerBuilder::Inliner.  
   procedure Pass_Manager_Builder_Use_Inliner_With_Threshold (PMB : Pass_Manager_Builder_T; Threshold : unsigned);  -- llvm-11.0.1.src/include/llvm-c/Transforms/PassManagerBuilder.h:62
   pragma Import (C, Pass_Manager_Builder_Use_Inliner_With_Threshold, "LLVMPassManagerBuilderUseInlinerWithThreshold");

  --* See llvm::PassManagerBuilder::populateFunctionPassManager.  
   procedure Pass_Manager_Builder_Populate_Function_Pass_Manager (PMB : Pass_Manager_Builder_T; PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/PassManagerBuilder.h:67
   pragma Import (C, Pass_Manager_Builder_Populate_Function_Pass_Manager, "LLVMPassManagerBuilderPopulateFunctionPassManager");

  --* See llvm::PassManagerBuilder::populateModulePassManager.  
   procedure Pass_Manager_Builder_Populate_Module_Pass_Manager (PMB : Pass_Manager_Builder_T; PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/PassManagerBuilder.h:72
   pragma Import (C, Pass_Manager_Builder_Populate_Module_Pass_Manager, "LLVMPassManagerBuilderPopulateModulePassManager");

  --* See llvm::PassManagerBuilder::populateLTOPassManager.  
procedure Pass_Manager_Populate_LTO_Pass_Manager
     (PMB         : Pass_Manager_Builder_T;
      PM          : LLVM.Types.Pass_Manager_T;
      Internalize : Boolean;
      Run_Inliner : Boolean);
   procedure Pass_Manager_Builder_Populate_LTO_Pass_Manager_C
     (PMB         : Pass_Manager_Builder_T;
      PM          : LLVM.Types.Pass_Manager_T;
      Internalize : LLVM.Types.Bool_T;
      Run_Inliner : LLVM.Types.Bool_T);
   pragma Import (C, Pass_Manager_Builder_Populate_LTO_Pass_Manager_C, "LLVMPassManagerBuilderPopulateLTOPassManager");

  --*
  -- * @}
  --  

end LLVM.Transforms_Passmanagerbuilder;

