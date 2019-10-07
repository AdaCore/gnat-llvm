pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with Interfaces.C.Strings;

package LLVM.Error_Handling is

  --===-- llvm-c/ErrorHandling.h - Error Handling C Interface -------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This file defines the C interface to LLVM's error handling mechanism.      *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

   type Fatal_Error_Handler_T is access procedure  (arg1 : Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, Fatal_Error_Handler_T);  -- llvm-9.0.0.src/include/llvm-c/ErrorHandling.h:21

  --*
  -- * Install a fatal error handler. By default, if LLVM detects a fatal error, it
  -- * will call exit(1). This may not be appropriate in many contexts. For example,
  -- * doing exit(1) will bypass many crash reporting/tracing system tools. This
  -- * function allows you to install a callback that will be invoked prior to the
  -- * call to exit(1).
  --  

   procedure Install_Fatal_Error_Handler (Handler : Fatal_Error_Handler_T);  -- llvm-9.0.0.src/include/llvm-c/ErrorHandling.h:30
   pragma Import (C, Install_Fatal_Error_Handler, "LLVMInstallFatalErrorHandler");

  --*
  -- * Reset the fatal error handler. This resets LLVM's fatal error handling
  -- * behavior to the default.
  --  

   procedure Reset_Fatal_Error_Handler;  -- llvm-9.0.0.src/include/llvm-c/ErrorHandling.h:36
   pragma Import (C, Reset_Fatal_Error_Handler, "LLVMResetFatalErrorHandler");

  --*
  -- * Enable LLVM's built-in stack trace code. This intercepts the OS's crash
  -- * signals and prints which component of LLVM you were in at the time if the
  -- * crash.
  --  

   procedure Enable_Pretty_Stack_Trace;  -- llvm-9.0.0.src/include/llvm-c/ErrorHandling.h:43
   pragma Import (C, Enable_Pretty_Stack_Trace, "LLVMEnablePrettyStackTrace");

end LLVM.Error_Handling;

