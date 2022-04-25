pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);

package LLVM.Deprecated is

   --  arg-macro: procedure LLVM_ATTRIBUTE_C_DEPRECATED (decl, message)
   --    decl __attribute__((deprecated))
  --===-- llvm-c/Deprecated.h - Deprecation macro -------------------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares LLVM_ATTRIBUTE_C_DEPRECATED() macro, which can be     *|
  --|* used to deprecate functions in the C interface.                            *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  -- This is a variant of LLVM_ATTRIBUTE_DEPRECATED() that is compatible with
  -- C compilers.
end LLVM.Deprecated;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
