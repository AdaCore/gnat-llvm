pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);

package Clang.Fatal_Error_Handler is

  --===-- clang-c/FatalErrorHandler.h - Fatal Error Handling --------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * Installs error handler that prints error message to stderr and calls abort().
  -- * Replaces currently installed error handler (if any).
  --  

   procedure Install_Aborting_Llvm_Fatal_Error_Handler  -- install/include/clang-c/FatalErrorHandler.h:21
   with Import => True, 
        Convention => C, 
        External_Name => "clang_install_aborting_llvm_fatal_error_handler";

  --*
  -- * Removes currently installed error handler (if any).
  -- * If no error handler is intalled, the default strategy is to print error
  -- * message to stderr and call exit(1).
  --  

   procedure Uninstall_Llvm_Fatal_Error_Handler  -- install/include/clang-c/FatalErrorHandler.h:28
   with Import => True, 
        Convention => C, 
        External_Name => "clang_uninstall_llvm_fatal_error_handler";

end Clang.Fatal_Error_Handler;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
