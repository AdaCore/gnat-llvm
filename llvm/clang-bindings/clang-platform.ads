pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);

package Clang.Platform is

   --  unsupported macro: CINDEX_LINKAGE __attribute__((visibility("default")))
   --  unsupported macro: CINDEX_DEPRECATED __attribute__((deprecated))
  --===-- clang-c/Platform.h - C Index platform decls   -------------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header provides platform specific macros (dllimport, deprecated, ...) *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  -- Windows DLL import/export.  
end Clang.Platform;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
