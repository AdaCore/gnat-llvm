pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with Interfaces.C.Strings;

package LLVM.Error_Handling is

   type Fatal_Error_Handler_T is access procedure  (arg1 : Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, Fatal_Error_Handler_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/ErrorHandling.h:21

   procedure Install_Fatal_Error_Handler (Handler : Fatal_Error_Handler_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/ErrorHandling.h:30
   pragma Import (C, Install_Fatal_Error_Handler, "LLVMInstallFatalErrorHandler");

   procedure Reset_Fatal_Error_Handler;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/ErrorHandling.h:36
   pragma Import (C, Reset_Fatal_Error_Handler, "LLVMResetFatalErrorHandler");

   procedure Enable_Pretty_Stack_Trace;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/ErrorHandling.h:43
   pragma Import (C, Enable_Pretty_Stack_Trace, "LLVMEnablePrettyStackTrace");

end LLVM.Error_Handling;

