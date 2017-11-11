pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with System;

package LLVM.Analysis is

   type Verifier_Failure_Action_T is 
     (Abort_Process_Action,
      Print_Message_Action,
      Return_Status_Action);
   pragma Convention (C, Verifier_Failure_Action_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Analysis.h:39

   function Verify_Module
     (M : LLVM.Types.Module_T;
      Action : Verifier_Failure_Action_T;
      Out_Message : System.Address) return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Analysis.h:45
   pragma Import (C, Verify_Module, "LLVMVerifyModule");

   function Verify_Function (Fn : LLVM.Types.Value_T; Action : Verifier_Failure_Action_T) return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Analysis.h:50
   pragma Import (C, Verify_Function, "LLVMVerifyFunction");

   procedure View_Function_CFG (Fn : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Analysis.h:54
   pragma Import (C, View_Function_CFG, "LLVMViewFunctionCFG");

   procedure View_Function_CFG_Only (Fn : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Analysis.h:55
   pragma Import (C, View_Function_CFG_Only, "LLVMViewFunctionCFGOnly");

end LLVM.Analysis;

