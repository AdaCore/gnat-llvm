pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

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

  --*
  -- * @addtogroup LLVMCError
  -- *
  -- * @{
  --  

   type Fatal_Error_Handler_T is access procedure (Arg_1 : Interfaces.C.Strings.chars_ptr)
   with Convention => C;  -- install/include/llvm-c/ErrorHandling.h:27

  --*
  -- * Install a fatal error handler. By default, if LLVM detects a fatal error, it
  -- * will call exit(1). This may not be appropriate in many contexts. For example,
  -- * doing exit(1) will bypass many crash reporting/tracing system tools. This
  -- * function allows you to install a callback that will be invoked prior to the
  -- * call to exit(1).
  --  

   procedure Install_Fatal_Error_Handler (Handler : Fatal_Error_Handler_T)  -- install/include/llvm-c/ErrorHandling.h:36
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInstallFatalErrorHandler";

  --*
  -- * Reset the fatal error handler. This resets LLVM's fatal error handling
  -- * behavior to the default.
  --  

   procedure Reset_Fatal_Error_Handler  -- install/include/llvm-c/ErrorHandling.h:42
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMResetFatalErrorHandler";

  --*
  -- * Enable LLVM's built-in stack trace code. This intercepts the OS's crash
  -- * signals and prints which component of LLVM you were in at the time if the
  -- * crash.
  --  

   procedure Enable_Pretty_Stack_Trace  -- install/include/llvm-c/ErrorHandling.h:49
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMEnablePrettyStackTrace";

  --*
  -- * @}
  --  

end LLVM.Error_Handling;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
