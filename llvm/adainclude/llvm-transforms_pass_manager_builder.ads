pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;

package LLVM.Transforms_Pass_Manager_Builder is

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

   type Opaque_Pass_Manager_Builder_Impl_T is null record;   -- incomplete struct

   type Pass_Manager_Builder_T is access all Opaque_Pass_Manager_Builder_Impl_T;  -- install/include/llvm-c/Transforms/PassManagerBuilder.h:20

  --*
  -- * @defgroup LLVMCTransformsPassManagerBuilder Pass manager builder
  -- * @ingroup LLVMCTransforms
  -- *
  -- * @{
  --  

  --* See llvm::PassManagerBuilder.  
   function Pass_Manager_Builder_Create return Pass_Manager_Builder_T  -- install/include/llvm-c/Transforms/PassManagerBuilder.h:32
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPassManagerBuilderCreate";

   procedure Pass_Manager_Builder_Dispose (PMB : Pass_Manager_Builder_T)  -- install/include/llvm-c/Transforms/PassManagerBuilder.h:33
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPassManagerBuilderDispose";

  --* See llvm::PassManagerBuilder::OptLevel.  
   procedure Pass_Manager_Builder_Set_Opt_Level (PMB : Pass_Manager_Builder_T; Opt_Level : unsigned)  -- install/include/llvm-c/Transforms/PassManagerBuilder.h:37
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPassManagerBuilderSetOptLevel";

  --* See llvm::PassManagerBuilder::SizeLevel.  
   procedure Pass_Manager_Builder_Set_Size_Level (PMB : Pass_Manager_Builder_T; Size_Level : unsigned)  -- install/include/llvm-c/Transforms/PassManagerBuilder.h:42
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPassManagerBuilderSetSizeLevel";

  --* See llvm::PassManagerBuilder::DisableUnitAtATime.  
procedure Pass_Manager_Set_Disable_Unit_At_A_Time
     (PMB   : Pass_Manager_Builder_T;
      Value : Boolean);

  --* See llvm::PassManagerBuilder::DisableUnrollLoops.  
procedure Pass_Manager_Set_Disable_Unroll_Loops
     (PMB   : Pass_Manager_Builder_T;
      Value : Boolean);

  --* See llvm::PassManagerBuilder::DisableSimplifyLibCalls  
procedure Pass_Manager_Set_Disable_Simplify_Lib_Calls
     (PMB   : Pass_Manager_Builder_T;
      Value : Boolean);

  --* See llvm::PassManagerBuilder::Inliner.  
   procedure Pass_Manager_Builder_Use_Inliner_With_Threshold (PMB : Pass_Manager_Builder_T; Threshold : unsigned)  -- install/include/llvm-c/Transforms/PassManagerBuilder.h:62
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPassManagerBuilderUseInlinerWithThreshold";

  --* See llvm::PassManagerBuilder::populateFunctionPassManager.  
   procedure Pass_Manager_Builder_Populate_Function_Pass_Manager (PMB : Pass_Manager_Builder_T; PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/PassManagerBuilder.h:67
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPassManagerBuilderPopulateFunctionPassManager";

  --* See llvm::PassManagerBuilder::populateModulePassManager.  
   procedure Pass_Manager_Builder_Populate_Module_Pass_Manager (PMB : Pass_Manager_Builder_T; PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/PassManagerBuilder.h:72
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPassManagerBuilderPopulateModulePassManager";

  --*
  -- * @}
  --  

end LLVM.Transforms_Pass_Manager_Builder;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
