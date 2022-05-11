pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with Interfaces.C.Strings;
with LLVM.Target_Machine;
with LLVM.Error;

package LLVM.Transforms_Pass_Builder is

  --===-- llvm-c/Transform/PassBuilder.h - PassBuilder for LLVM C ---*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header contains the LLVM-C interface into the new pass manager        *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCCoreNewPM New Pass Manager
  -- * @ingroup LLVMCCore
  -- *
  -- * @{
  --  

  --*
  -- * A set of options passed which are attached to the Pass Manager upon run.
  -- *
  -- * This corresponds to an llvm::LLVMPassBuilderOptions instance
  -- *
  -- * The details for how the different properties of this structure are used can
  -- * be found in the source for LLVMRunPasses
  --  

   type Opaque_Pass_Builder_Options_Impl_T is null record;   -- incomplete struct

   type Pass_Builder_Options_T is access all Opaque_Pass_Builder_Options_Impl_T;  -- install/include/llvm-c/Transforms/PassBuilder.h:38

  --*
  -- * Construct and run a set of passes over a module
  -- *
  -- * This function takes a string with the passes that should be used. The format
  -- * of this string is the same as opt's -passes argument for the new pass
  -- * manager. Individual passes may be specified, separated by commas. Full
  -- * pipelines may also be invoked using `default<O3>` and friends. See opt for
  -- * full reference of the Passes format.
  --  

function Run_Passes
     (M       : LLVM.Types.Module_T;
      Passes  : String;
      TM      : LLVM.Target_Machine.Target_Machine_T;
      Options : Pass_Builder_Options_T)
      return LLVM.Error.Error_T;

  --*
  -- * Create a new set of options for a PassBuilder
  -- *
  -- * Ownership of the returned instance is given to the client, and they are
  -- * responsible for it. The client should call LLVMDisposePassBuilderOptions
  -- * to free the pass builder options.
  --  

   function Create_Pass_Builder_Options return Pass_Builder_Options_T  -- install/include/llvm-c/Transforms/PassBuilder.h:60
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreatePassBuilderOptions";

  --*
  -- * Toggle adding the VerifierPass for the PassBuilder, ensuring all functions
  -- * inside the module is valid.
  --  

procedure Pass_Options_Set_Verify_Each
     (Options     : Pass_Builder_Options_T;
      Verify_Each : Boolean);

  --*
  -- * Toggle debug logging when running the PassBuilder
  --  

procedure Pass_Options_Set_Debug_Logging
     (Options       : Pass_Builder_Options_T;
      Debug_Logging : Boolean);

procedure Pass_Options_Set_Loop_Interleaving
     (Options           : Pass_Builder_Options_T;
      Loop_Interleaving : Boolean);

procedure Pass_Options_Set_Loop_Vectorization
     (Options            : Pass_Builder_Options_T;
      Loop_Vectorization : Boolean);

procedure Pass_Options_Set_SLP_Vectorization
     (Options           : Pass_Builder_Options_T;
      SLP_Vectorization : Boolean);

procedure Pass_Options_Set_Loop_Unrolling
     (Options        : Pass_Builder_Options_T;
      Loop_Unrolling : Boolean);

procedure Pass_Options_Set_Forget_All_SCEV_In_Loop_Unroll
     (Options                        : Pass_Builder_Options_T;
      Forget_All_SCEV_In_Loop_Unroll : Boolean);

   procedure Pass_Builder_Options_Set_Licm_Mssa_Opt_Cap (Options : Pass_Builder_Options_T; Licm_Mssa_Opt_Cap : unsigned)  -- install/include/llvm-c/Transforms/PassBuilder.h:90
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPassBuilderOptionsSetLicmMssaOptCap";

   procedure Pass_Builder_Options_Set_Licm_Mssa_No_Acc_For_Promotion_Cap (Options : Pass_Builder_Options_T; Licm_Mssa_No_Acc_For_Promotion_Cap : unsigned)  -- install/include/llvm-c/Transforms/PassBuilder.h:93
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPassBuilderOptionsSetLicmMssaNoAccForPromotionCap";

procedure Pass_Options_Set_Call_Graph_Profile
     (Options            : Pass_Builder_Options_T;
      Call_Graph_Profile : Boolean);

procedure Pass_Options_Set_Merge_Functions
     (Options         : Pass_Builder_Options_T;
      Merge_Functions : Boolean);

  --*
  -- * Dispose of a heap-allocated PassBuilderOptions instance
  --  

   procedure Dispose_Pass_Builder_Options (Options : Pass_Builder_Options_T)  -- install/include/llvm-c/Transforms/PassBuilder.h:105
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposePassBuilderOptions";

  --*
  -- * @}
  --  

end LLVM.Transforms_Pass_Builder;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
