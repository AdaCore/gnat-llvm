pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;

package LLVM.Transforms_Scalar is

  --===-- Scalar.h - Scalar Transformation Library C Interface ----*- C++ -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to libLLVMScalarOpts.a, which         *|
  --|* implements various scalar transformations of the LLVM IR.                  *|
  --|*                                                                            *|
  --|* Many exotic languages can interoperate with C code but have a harder time  *|
  --|* with C++ due to name mangling. So in addition to C, this interface enables *|
  --|* tools written in such languages.                                           *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCTransformsScalar Scalar transformations
  -- * @ingroup LLVMCTransforms
  -- *
  -- * @{
  --  

  --* See llvm::createAggressiveDCEPass function.  
   procedure Add_Aggressive_DCE_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:35
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddAggressiveDCEPass";

  --* See llvm::createDeadCodeEliminationPass function.  
   procedure Add_DCE_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:38
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddDCEPass";

  --* See llvm::createBitTrackingDCEPass function.  
   procedure Add_Bit_Tracking_DCE_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:41
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddBitTrackingDCEPass";

  --* See llvm::createAlignmentFromAssumptionsPass function.  
   procedure Add_Alignment_From_Assumptions_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:44
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddAlignmentFromAssumptionsPass";

  --* See llvm::createCFGSimplificationPass function.  
   procedure Add_CFG_Simplification_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:47
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddCFGSimplificationPass";

  --* See llvm::createDeadStoreEliminationPass function.  
   procedure Add_Dead_Store_Elimination_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:50
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddDeadStoreEliminationPass";

  --* See llvm::createScalarizerPass function.  
   procedure Add_Scalarizer_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:53
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddScalarizerPass";

  --* See llvm::createMergedLoadStoreMotionPass function.  
   procedure Add_Merged_Load_Store_Motion_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddMergedLoadStoreMotionPass";

  --* See llvm::createGVNPass function.  
   procedure Add_GVN_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:59
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddGVNPass";

  --* See llvm::createGVNPass function.  
   procedure Add_New_GVN_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:62
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddNewGVNPass";

  --* See llvm::createIndVarSimplifyPass function.  
   procedure Add_Ind_Var_Simplify_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:65
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddIndVarSimplifyPass";

  --* See llvm::createInstructionCombiningPass function.  
   procedure Add_Instruction_Combining_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:68
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddInstructionCombiningPass";

  --* See llvm::createInstSimplifyLegacyPass function.  
   procedure Add_Instruction_Simplify_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:71
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddInstructionSimplifyPass";

  --* See llvm::createJumpThreadingPass function.  
   procedure Add_Jump_Threading_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:74
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddJumpThreadingPass";

  --* See llvm::createLICMPass function.  
   procedure Add_LICM_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:77
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddLICMPass";

  --* See llvm::createLoopDeletionPass function.  
   procedure Add_Loop_Deletion_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:80
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddLoopDeletionPass";

  --* See llvm::createLoopIdiomPass function  
   procedure Add_Loop_Idiom_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:83
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddLoopIdiomPass";

  --* See llvm::createLoopRotatePass function.  
   procedure Add_Loop_Rotate_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:86
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddLoopRotatePass";

  --* See llvm::createLoopRerollPass function.  
   procedure Add_Loop_Reroll_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:89
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddLoopRerollPass";

  --* See llvm::createLoopUnrollPass function.  
   procedure Add_Loop_Unroll_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:92
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddLoopUnrollPass";

  --* See llvm::createLoopUnrollAndJamPass function.  
   procedure Add_Loop_Unroll_And_Jam_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:95
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddLoopUnrollAndJamPass";

  --* See llvm::createLowerAtomicPass function.  
   procedure Add_Lower_Atomic_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:98
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddLowerAtomicPass";

  --* See llvm::createMemCpyOptPass function.  
   procedure Add_Mem_Cpy_Opt_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:101
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddMemCpyOptPass";

  --* See llvm::createPartiallyInlineLibCallsPass function.  
   procedure Add_Partially_Inline_Lib_Calls_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:104
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddPartiallyInlineLibCallsPass";

  --* See llvm::createReassociatePass function.  
   procedure Add_Reassociate_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:107
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddReassociatePass";

  --* See llvm::createSCCPPass function.  
   procedure Add_SCCP_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:110
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddSCCPPass";

  --* See llvm::createSROAPass function.  
   procedure Add_Scalar_Repl_Aggregates_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:113
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddScalarReplAggregatesPass";

  --* See llvm::createSROAPass function.  
   procedure Add_Scalar_Repl_Aggregates_Pass_SSA (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:116
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddScalarReplAggregatesPassSSA";

  --* See llvm::createSROAPass function.  
   procedure Add_Scalar_Repl_Aggregates_Pass_With_Threshold (PM : LLVM.Types.Pass_Manager_T; Threshold : int)  -- install/include/llvm-c/Transforms/Scalar.h:119
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddScalarReplAggregatesPassWithThreshold";

  --* See llvm::createSimplifyLibCallsPass function.  
   procedure Add_Simplify_Lib_Calls_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:123
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddSimplifyLibCallsPass";

  --* See llvm::createTailCallEliminationPass function.  
   procedure Add_Tail_Call_Elimination_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:126
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddTailCallEliminationPass";

  --* See llvm::demotePromoteMemoryToRegisterPass function.  
   procedure Add_Demote_Memory_To_Register_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:129
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddDemoteMemoryToRegisterPass";

  --* See llvm::createVerifierPass function.  
   procedure Add_Verifier_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:132
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddVerifierPass";

  --* See llvm::createCorrelatedValuePropagationPass function  
   procedure Add_Correlated_Value_Propagation_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:135
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddCorrelatedValuePropagationPass";

  --* See llvm::createEarlyCSEPass function  
   procedure Add_Early_CSE_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:138
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddEarlyCSEPass";

  --* See llvm::createEarlyCSEPass function  
   procedure Add_Early_CSE_Mem_SSA_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:141
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddEarlyCSEMemSSAPass";

  --* See llvm::createLowerExpectIntrinsicPass function  
   procedure Add_Lower_Expect_Intrinsic_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:144
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddLowerExpectIntrinsicPass";

  --* See llvm::createLowerConstantIntrinsicsPass function  
   procedure Add_Lower_Constant_Intrinsics_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:147
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddLowerConstantIntrinsicsPass";

  --* See llvm::createTypeBasedAliasAnalysisPass function  
   procedure Add_Type_Based_Alias_Analysis_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:150
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddTypeBasedAliasAnalysisPass";

  --* See llvm::createScopedNoAliasAAPass function  
   procedure Add_Scoped_No_Alias_AA_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:153
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddScopedNoAliasAAPass";

  --* See llvm::createBasicAliasAnalysisPass function  
   procedure Add_Basic_Alias_Analysis_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:156
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddBasicAliasAnalysisPass";

  --* See llvm::createUnifyFunctionExitNodesPass function  
   procedure Add_Unify_Function_Exit_Nodes_Pass (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Transforms/Scalar.h:159
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddUnifyFunctionExitNodesPass";

  --*
  -- * @}
  --  

end LLVM.Transforms_Scalar;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
