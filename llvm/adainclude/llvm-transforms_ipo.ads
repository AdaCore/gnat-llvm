pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with System;

package LLVM.Transforms_IPO is

  --===-- IPO.h - Interprocedural Transformations C Interface -----*- C++ -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to libLLVMIPO.a, which implements     *|
  --|* various interprocedural transformations of the LLVM IR.                    *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCTransformsIPO Interprocedural transformations
  -- * @ingroup LLVMCTransforms
  -- *
  -- * @{
  --  

  --* See llvm::createConstantMergePass function.  
   procedure Add_Constant_Merge_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/IPO.h:31
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddConstantMergePass";

  --* See llvm::createMergeFunctionsPass function.  
   procedure Add_Merge_Functions_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/IPO.h:34
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddMergeFunctionsPass";

  --* See llvm::createCalledValuePropagationPass function.  
   procedure Add_Called_Value_Propagation_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/IPO.h:37
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddCalledValuePropagationPass";

  --* See llvm::createDeadArgEliminationPass function.  
   procedure Add_Dead_Arg_Elimination_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/IPO.h:40
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddDeadArgEliminationPass";

  --* See llvm::createFunctionAttrsPass function.  
   procedure Add_Function_Attrs_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/IPO.h:43
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddFunctionAttrsPass";

  --* See llvm::createFunctionInliningPass function.  
   procedure Add_Function_Inlining_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/IPO.h:46
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddFunctionInliningPass";

  --* See llvm::createAlwaysInlinerPass function.  
   procedure Add_Always_Inliner_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/IPO.h:49
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddAlwaysInlinerPass";

  --* See llvm::createGlobalDCEPass function.  
   procedure Add_Global_DCE_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/IPO.h:52
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddGlobalDCEPass";

  --* See llvm::createGlobalOptimizerPass function.  
   procedure Add_Global_Optimizer_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/IPO.h:55
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddGlobalOptimizerPass";

  --* See llvm::createIPSCCPPass function.  
   procedure Add_IPSCCP_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/IPO.h:58
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddIPSCCPPass";

  --* See llvm::createInternalizePass function.  
   procedure Add_Internalize_Pass (Arg_1 : LLVM.Types.Pass_Manager_T; All_But_Main : unsigned)  -- install/include/llvm-c/Transforms/IPO.h:61
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddInternalizePass";

  --*
  -- * Create and add the internalize pass to the given pass manager with the
  -- * provided preservation callback.
  -- *
  -- * The context parameter is forwarded to the callback on each invocation.
  -- * As such, it is the responsibility of the caller to extend its lifetime
  -- * until execution of this pass has finished.
  -- *
  -- * @see llvm::createInternalizePass function.
  --  

   procedure Add_Internalize_Pass_With_Must_Preserve_Predicate
     (PM : LLVM.Types.Pass_Manager_T;
      Context : System.Address;
      Must_Preserve : access function (Arg_1 : LLVM.Types.Value_T; Arg_2 : System.Address) return LLVM.Types.Bool_T)  -- install/include/llvm-c/Transforms/IPO.h:73
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddInternalizePassWithMustPreservePredicate";

  --* See llvm::createStripDeadPrototypesPass function.  
   procedure Add_Strip_Dead_Prototypes_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/IPO.h:79
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddStripDeadPrototypesPass";

  --* See llvm::createStripSymbolsPass function.  
   procedure Add_Strip_Symbols_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/IPO.h:82
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddStripSymbolsPass";

  --*
  -- * @}
  --  

end LLVM.Transforms_IPO;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
