pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with System;

package LLVM.Analysis is

  --===-- llvm-c/Analysis.h - Analysis Library C Interface --------*- C++ -*-===*|*                                                                            *|
  --|
  --|*                     The LLVM Compiler Infrastructure                       *|
  --|*                                                                            *|
  --|* This file is distributed under the University of Illinois Open Source      *|
  --|* License. See LICENSE.TXT for details.                                      *|
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
      Return_Status_Action);
   pragma Convention (C, Verifier_Failure_Action_T);  -- llvm-8.0.1.src/include/llvm-c/Analysis.h:39

  -- Verifies that a module is valid, taking the specified action if not.
  --   Optionally returns a human-readable description of any invalid constructs.
  --   OutMessage must be disposed with LLVMDisposeMessage.  

function Verify_Module
     (M           : LLVM.Types.Module_T;
      Action      : Verifier_Failure_Action_T;
      Out_Message : System.Address)
      return Boolean;
   function Verify_Module_C
     (M           : LLVM.Types.Module_T;
      Action      : Verifier_Failure_Action_T;
      Out_Message : System.Address)
      return LLVM.Types.Bool_T;
   pragma Import (C, Verify_Module_C, "LLVMVerifyModule");

  -- Verifies that a single function is valid, taking the specified action. Useful
  --   for debugging.  

   function Verify_Function
     (Fn     : LLVM.Types.Value_T;
      Action : Verifier_Failure_Action_T)
      return Boolean;
   function Verify_Function_C
     (Fn     : LLVM.Types.Value_T;
      Action : Verifier_Failure_Action_T)
      return LLVM.Types.Bool_T;  -- llvm-8.0.1.src/include/llvm-c/Analysis.h:50
   pragma Import (C, Verify_Function_C, "LLVMVerifyFunction");

  -- Open up a ghostview window that displays the CFG of the current function.
  --   Useful for debugging.  

   procedure View_Function_CFG (Fn : LLVM.Types.Value_T);  -- llvm-8.0.1.src/include/llvm-c/Analysis.h:54
   pragma Import (C, View_Function_CFG, "LLVMViewFunctionCFG");

   procedure View_Function_CFG_Only (Fn : LLVM.Types.Value_T);  -- llvm-8.0.1.src/include/llvm-c/Analysis.h:55
   pragma Import (C, View_Function_CFG_Only, "LLVMViewFunctionCFGOnly");

  --*
  -- * @}
  --  

end LLVM.Analysis;

