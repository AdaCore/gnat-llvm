pragma Style_Checks (Off);

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
   procedure Add_Aggressive_DCE_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:35
   pragma Import (C, Add_Aggressive_DCE_Pass, "LLVMAddAggressiveDCEPass");

  --* See llvm::createDeadCodeEliminationPass function.  
   procedure Add_DCE_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:38
   pragma Import (C, Add_DCE_Pass, "LLVMAddDCEPass");

  --* See llvm::createBitTrackingDCEPass function.  
   procedure Add_Bit_Tracking_DCE_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:41
   pragma Import (C, Add_Bit_Tracking_DCE_Pass, "LLVMAddBitTrackingDCEPass");

  --* See llvm::createAlignmentFromAssumptionsPass function.  
   procedure Add_Alignment_From_Assumptions_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:44
   pragma Import (C, Add_Alignment_From_Assumptions_Pass, "LLVMAddAlignmentFromAssumptionsPass");

  --* See llvm::createCFGSimplificationPass function.  
   procedure Add_CFG_Simplification_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:47
   pragma Import (C, Add_CFG_Simplification_Pass, "LLVMAddCFGSimplificationPass");

  --* See llvm::createDeadStoreEliminationPass function.  
   procedure Add_Dead_Store_Elimination_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:50
   pragma Import (C, Add_Dead_Store_Elimination_Pass, "LLVMAddDeadStoreEliminationPass");

  --* See llvm::createScalarizerPass function.  
   procedure Add_Scalarizer_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:53
   pragma Import (C, Add_Scalarizer_Pass, "LLVMAddScalarizerPass");

  --* See llvm::createMergedLoadStoreMotionPass function.  
   procedure Add_Merged_Load_Store_Motion_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:56
   pragma Import (C, Add_Merged_Load_Store_Motion_Pass, "LLVMAddMergedLoadStoreMotionPass");

  --* See llvm::createGVNPass function.  
   procedure Add_GVN_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:59
   pragma Import (C, Add_GVN_Pass, "LLVMAddGVNPass");

  --* See llvm::createGVNPass function.  
   procedure Add_New_GVN_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:62
   pragma Import (C, Add_New_GVN_Pass, "LLVMAddNewGVNPass");

  --* See llvm::createIndVarSimplifyPass function.  
   procedure Add_Ind_Var_Simplify_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:65
   pragma Import (C, Add_Ind_Var_Simplify_Pass, "LLVMAddIndVarSimplifyPass");

  --* See llvm::createInstructionCombiningPass function.  
   procedure Add_Instruction_Combining_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:68
   pragma Import (C, Add_Instruction_Combining_Pass, "LLVMAddInstructionCombiningPass");

  --* See llvm::createJumpThreadingPass function.  
   procedure Add_Jump_Threading_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:71
   pragma Import (C, Add_Jump_Threading_Pass, "LLVMAddJumpThreadingPass");

  --* See llvm::createLICMPass function.  
   procedure Add_LICM_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:74
   pragma Import (C, Add_LICM_Pass, "LLVMAddLICMPass");

  --* See llvm::createLoopDeletionPass function.  
   procedure Add_Loop_Deletion_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:77
   pragma Import (C, Add_Loop_Deletion_Pass, "LLVMAddLoopDeletionPass");

  --* See llvm::createLoopIdiomPass function  
   procedure Add_Loop_Idiom_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:80
   pragma Import (C, Add_Loop_Idiom_Pass, "LLVMAddLoopIdiomPass");

  --* See llvm::createLoopRotatePass function.  
   procedure Add_Loop_Rotate_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:83
   pragma Import (C, Add_Loop_Rotate_Pass, "LLVMAddLoopRotatePass");

  --* See llvm::createLoopRerollPass function.  
   procedure Add_Loop_Reroll_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:86
   pragma Import (C, Add_Loop_Reroll_Pass, "LLVMAddLoopRerollPass");

  --* See llvm::createLoopUnrollPass function.  
   procedure Add_Loop_Unroll_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:89
   pragma Import (C, Add_Loop_Unroll_Pass, "LLVMAddLoopUnrollPass");

  --* See llvm::createLoopUnrollAndJamPass function.  
   procedure Add_Loop_Unroll_And_Jam_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:92
   pragma Import (C, Add_Loop_Unroll_And_Jam_Pass, "LLVMAddLoopUnrollAndJamPass");

  --* See llvm::createLoopUnswitchPass function.  
   procedure Add_Loop_Unswitch_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:95
   pragma Import (C, Add_Loop_Unswitch_Pass, "LLVMAddLoopUnswitchPass");

  --* See llvm::createLowerAtomicPass function.  
   procedure Add_Lower_Atomic_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:98
   pragma Import (C, Add_Lower_Atomic_Pass, "LLVMAddLowerAtomicPass");

  --* See llvm::createMemCpyOptPass function.  
   procedure Add_Mem_Cpy_Opt_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:101
   pragma Import (C, Add_Mem_Cpy_Opt_Pass, "LLVMAddMemCpyOptPass");

  --* See llvm::createPartiallyInlineLibCallsPass function.  
   procedure Add_Partially_Inline_Lib_Calls_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:104
   pragma Import (C, Add_Partially_Inline_Lib_Calls_Pass, "LLVMAddPartiallyInlineLibCallsPass");

  --* See llvm::createReassociatePass function.  
   procedure Add_Reassociate_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:107
   pragma Import (C, Add_Reassociate_Pass, "LLVMAddReassociatePass");

  --* See llvm::createSCCPPass function.  
   procedure Add_SCCP_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:110
   pragma Import (C, Add_SCCP_Pass, "LLVMAddSCCPPass");

  --* See llvm::createSROAPass function.  
   procedure Add_Scalar_Repl_Aggregates_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:113
   pragma Import (C, Add_Scalar_Repl_Aggregates_Pass, "LLVMAddScalarReplAggregatesPass");

  --* See llvm::createSROAPass function.  
   procedure Add_Scalar_Repl_Aggregates_Pass_SSA (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:116
   pragma Import (C, Add_Scalar_Repl_Aggregates_Pass_SSA, "LLVMAddScalarReplAggregatesPassSSA");

  --* See llvm::createSROAPass function.  
   procedure Add_Scalar_Repl_Aggregates_Pass_With_Threshold (PM : LLVM.Types.Pass_Manager_T; Threshold : int);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:119
   pragma Import (C, Add_Scalar_Repl_Aggregates_Pass_With_Threshold, "LLVMAddScalarReplAggregatesPassWithThreshold");

  --* See llvm::createSimplifyLibCallsPass function.  
   procedure Add_Simplify_Lib_Calls_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:123
   pragma Import (C, Add_Simplify_Lib_Calls_Pass, "LLVMAddSimplifyLibCallsPass");

  --* See llvm::createTailCallEliminationPass function.  
   procedure Add_Tail_Call_Elimination_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:126
   pragma Import (C, Add_Tail_Call_Elimination_Pass, "LLVMAddTailCallEliminationPass");

  --* See llvm::createConstantPropagationPass function.  
   procedure Add_Constant_Propagation_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:129
   pragma Import (C, Add_Constant_Propagation_Pass, "LLVMAddConstantPropagationPass");

  --* See llvm::demotePromoteMemoryToRegisterPass function.  
   procedure Add_Demote_Memory_To_Register_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:132
   pragma Import (C, Add_Demote_Memory_To_Register_Pass, "LLVMAddDemoteMemoryToRegisterPass");

  --* See llvm::createVerifierPass function.  
   procedure Add_Verifier_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:135
   pragma Import (C, Add_Verifier_Pass, "LLVMAddVerifierPass");

  --* See llvm::createCorrelatedValuePropagationPass function  
   procedure Add_Correlated_Value_Propagation_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:138
   pragma Import (C, Add_Correlated_Value_Propagation_Pass, "LLVMAddCorrelatedValuePropagationPass");

  --* See llvm::createEarlyCSEPass function  
   procedure Add_Early_CSE_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:141
   pragma Import (C, Add_Early_CSE_Pass, "LLVMAddEarlyCSEPass");

  --* See llvm::createEarlyCSEPass function  
   procedure Add_Early_CSE_Mem_SSA_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:144
   pragma Import (C, Add_Early_CSE_Mem_SSA_Pass, "LLVMAddEarlyCSEMemSSAPass");

  --* See llvm::createLowerExpectIntrinsicPass function  
   procedure Add_Lower_Expect_Intrinsic_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:147
   pragma Import (C, Add_Lower_Expect_Intrinsic_Pass, "LLVMAddLowerExpectIntrinsicPass");

  --* See llvm::createLowerConstantIntrinsicsPass function  
   procedure Add_Lower_Constant_Intrinsics_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:150
   pragma Import (C, Add_Lower_Constant_Intrinsics_Pass, "LLVMAddLowerConstantIntrinsicsPass");

  --* See llvm::createTypeBasedAliasAnalysisPass function  
   procedure Add_Type_Based_Alias_Analysis_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:153
   pragma Import (C, Add_Type_Based_Alias_Analysis_Pass, "LLVMAddTypeBasedAliasAnalysisPass");

  --* See llvm::createScopedNoAliasAAPass function  
   procedure Add_Scoped_No_Alias_AA_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:156
   pragma Import (C, Add_Scoped_No_Alias_AA_Pass, "LLVMAddScopedNoAliasAAPass");

  --* See llvm::createBasicAliasAnalysisPass function  
   procedure Add_Basic_Alias_Analysis_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:159
   pragma Import (C, Add_Basic_Alias_Analysis_Pass, "LLVMAddBasicAliasAnalysisPass");

  --* See llvm::createUnifyFunctionExitNodesPass function  
   procedure Add_Unify_Function_Exit_Nodes_Pass (PM : LLVM.Types.Pass_Manager_T);  -- llvm-11.0.1.src/include/llvm-c/Transforms/Scalar.h:162
   pragma Import (C, Add_Unify_Function_Exit_Nodes_Pass, "LLVMAddUnifyFunctionExitNodesPass");

  --*
  -- * @}
  --  

end LLVM.Transforms_Scalar;

