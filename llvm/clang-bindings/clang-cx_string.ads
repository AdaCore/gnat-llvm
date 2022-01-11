pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with Interfaces.C.Strings;

package Clang.CX_String is

  --===-- clang-c/CXString.h - C Index strings  --------------------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header provides the interface to C Index strings.                     *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * \defgroup CINDEX_STRING String manipulation routines
  -- * \ingroup CINDEX
  -- *
  -- * @{
  --  

  --*
  -- * A character string.
  -- *
  -- * The \c CXString type is used to return strings from the interface when
  -- * the ownership of that string might differ from one call to the next.
  -- * Use \c clang_getCString() to retrieve the string data and, once finished
  -- * with the string data, call \c clang_disposeString() to free the string.
  --  

   type String_T is record
      data : System.Address;  -- /usr/local/include/clang-c/CXString.h:38
      private_flags : aliased unsigned;  -- /usr/local/include/clang-c/CXString.h:39
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/clang-c/CXString.h:40

   type String_Set_T is record
      Strings : access String_T;  -- /usr/local/include/clang-c/CXString.h:43
      Count : aliased unsigned;  -- /usr/local/include/clang-c/CXString.h:44
   end record
   with Convention => C_Pass_By_Copy;  -- /usr/local/include/clang-c/CXString.h:45

  --*
  -- * Retrieve the character data associated with the given string.
  --  

function Get_C_String
     (Str : String_T)
      return String;
   function Get_C_String_C
     (Str : String_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "clang_getCString";

  --*
  -- * Free the given string.
  --  

   procedure Dispose_String (Str : String_T)  -- /usr/local/include/clang-c/CXString.h:55
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeString";

  --*
  -- * Free the given string set.
  --  

   procedure Dispose_String_Set (Set : access String_Set_T)  -- /usr/local/include/clang-c/CXString.h:60
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeStringSet";

  --*
  -- * @}
  --  

end Clang.CX_String;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");