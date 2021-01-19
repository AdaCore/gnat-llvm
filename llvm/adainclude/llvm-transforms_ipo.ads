pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with System;

package LLVM.Transforms_Ipo is

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

  --* See llvm::createArgumentPromotionPass function.  
   procedure Add_Argument_Promotion_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:31
   pragma Import (C, Add_Argument_Promotion_Pass, "LLVMAddArgumentPromotionPass");

  --* See llvm::createConstantMergePass function.  
   procedure Add_Constant_Merge_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:34
   pragma Import (C, Add_Constant_Merge_Pass, "LLVMAddConstantMergePass");

  --* See llvm::createMergeFunctionsPass function.  
   procedure Add_Merge_Functions_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:37
   pragma Import (C, Add_Merge_Functions_Pass, "LLVMAddMergeFunctionsPass");

  --* See llvm::createCalledValuePropagationPass function.  
   procedure Add_Called_Value_Propagation_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:40
   pragma Import (C, Add_Called_Value_Propagation_Pass, "LLVMAddCalledValuePropagationPass");

  --* See llvm::createDeadArgEliminationPass function.  
   procedure Add_Dead_Arg_Elimination_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:43
   pragma Import (C, Add_Dead_Arg_Elimination_Pass, "LLVMAddDeadArgEliminationPass");

  --* See llvm::createFunctionAttrsPass function.  
   procedure Add_Function_Attrs_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:46
   pragma Import (C, Add_Function_Attrs_Pass, "LLVMAddFunctionAttrsPass");

  --* See llvm::createFunctionInliningPass function.  
   procedure Add_Function_Inlining_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:49
   pragma Import (C, Add_Function_Inlining_Pass, "LLVMAddFunctionInliningPass");

  --* See llvm::createAlwaysInlinerPass function.  
   procedure Add_Always_Inliner_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:52
   pragma Import (C, Add_Always_Inliner_Pass, "LLVMAddAlwaysInlinerPass");

  --* See llvm::createGlobalDCEPass function.  
   procedure Add_Global_DCE_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:55
   pragma Import (C, Add_Global_DCE_Pass, "LLVMAddGlobalDCEPass");

  --* See llvm::createGlobalOptimizerPass function.  
   procedure Add_Global_Optimizer_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:58
   pragma Import (C, Add_Global_Optimizer_Pass, "LLVMAddGlobalOptimizerPass");

  --* See llvm::createIPConstantPropagationPass function.  
   procedure Add_IP_Constant_Propagation_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:61
   pragma Import (C, Add_IP_Constant_Propagation_Pass, "LLVMAddIPConstantPropagationPass");

  --* See llvm::createPruneEHPass function.  
   procedure Add_Prune_EH_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:64
   pragma Import (C, Add_Prune_EH_Pass, "LLVMAddPruneEHPass");

  --* See llvm::createIPSCCPPass function.  
   procedure Add_IPSCCP_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:67
   pragma Import (C, Add_IPSCCP_Pass, "LLVMAddIPSCCPPass");

  --* See llvm::createInternalizePass function.  
   procedure Add_Internalize_Pass (arg1 : LLVM.Types.Pass_Manager_T; All_But_Main : unsigned);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:70
   pragma Import (C, Add_Internalize_Pass, "LLVMAddInternalizePass");

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
      Must_Preserve : access function  (arg1 : LLVM.Types.Value_T; arg2 : System.Address) return LLVM.Types.Bool_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:82
   pragma Import (C, Add_Internalize_Pass_With_Must_Preserve_Predicate, "LLVMAddInternalizePassWithMustPreservePredicate");

  --* See llvm::createStripDeadPrototypesPass function.  
   procedure Add_Strip_Dead_Prototypes_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:88
   pragma Import (C, Add_Strip_Dead_Prototypes_Pass, "LLVMAddStripDeadPrototypesPass");

  --* See llvm::createStripSymbolsPass function.  
   procedure Add_Strip_Symbols_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/IPO.h:91
   pragma Import (C, Add_Strip_Symbols_Pass, "LLVMAddStripSymbolsPass");

  --*
  -- * @}
  --  

end LLVM.Transforms_Ipo;

