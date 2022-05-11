pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with System;

package LLVM.Analysis is

  --===-- llvm-c/Analysis.h - Analysis Library C Interface --------*- C++ -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to libLLVMAnalysis.a, which           *|
  --|* implements various analyses of the LLVM IR.                                *|
  --|*                                                                            *|
  --|* Many exotic languages can interoperate with C code but have a harder time  *|
  --|* with C++ due to name mangling. So in addition to C, this interface enables *|
  --|* tools written in such languages.                                           *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCAnalysis Analysis
  -- * @ingroup LLVMC
  -- *
  -- * @{
  --  

  -- verifier will print to stderr and abort()  
  -- verifier will print to stderr and return 1  
  -- verifier will just return 1  
   type Verifier_Failure_Action_T is 
     (Abort_Process_Action,
      Print_Message_Action,
      Return_Status_Action)
   with Convention => C;  -- install/include/llvm-c/Analysis.h:38

  -- Verifies that a module is valid, taking the specified action if not.
  --   Optionally returns a human-readable description of any invalid constructs.
  --   OutMessage must be disposed with LLVMDisposeMessage.  

function Verify_Module
     (M           : LLVM.Types.Module_T;
      Action      : Verifier_Failure_Action_T;
      Out_Message : System.Address)
      return Boolean;

  -- Verifies that a single function is valid, taking the specified action. Useful
  --   for debugging.  

function Verify_Function
     (Fn     : LLVM.Types.Value_T;
      Action : Verifier_Failure_Action_T)
      return Boolean;

  -- Open up a ghostview window that displays the CFG of the current function.
  --   Useful for debugging.  

   procedure View_Function_CFG (Fn : LLVM.Types.Value_T)  -- install/include/llvm-c/Analysis.h:53
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMViewFunctionCFG";

   procedure View_Function_CFG_Only (Fn : LLVM.Types.Value_T)  -- install/include/llvm-c/Analysis.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMViewFunctionCFGOnly";

  --*
  -- * @}
  --  

end LLVM.Analysis;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
