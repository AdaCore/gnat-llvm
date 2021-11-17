pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);

package Clang.Extern_C is

   --  unsupported macro: LLVM_CLANG_C_EXTERN_C_BEGIN LLVM_CLANG_C_STRICT_PROTOTYPES_BEGIN
   --  unsupported macro: LLVM_CLANG_C_EXTERN_C_END LLVM_CLANG_C_STRICT_PROTOTYPES_END
  --===- clang-c/ExternC.h - Wrapper for 'extern "C"' ---------------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This file defines an 'extern "C"' wrapper.                                 *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

end Clang.Extern_C;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
