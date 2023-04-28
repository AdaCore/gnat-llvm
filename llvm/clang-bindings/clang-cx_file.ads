pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with Clang.CX_String;
with time_h;
with Interfaces.C.Extensions;

package Clang.CX_File is

  --===-- clang-c/CXFile.h - C Index File ---------------------------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header provides the interface to C Index files.                       *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * \defgroup CINDEX_FILES File manipulation routines
  -- *
  -- * @{
  --  

  --*
  -- * A particular source file that is part of a translation unit.
  --  

   type File_T is new System.Address;  -- install/include/clang-c/CXFile.h:34

  --*
  -- * Retrieve the complete file and path name of the given file.
  --  

function Get_File_Name
     (S_File : File_T)
      return String;

  --*
  -- * Retrieve the last modification time of the given file.
  --  

   function Get_File_Time (S_File : File_T) return time_h.time_t  -- install/include/clang-c/CXFile.h:44
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getFileTime";

  --*
  -- * Uniquely identifies a CXFile, that refers to the same underlying file,
  -- * across an indexing session.
  --  

   type anon_array1106 is array (0 .. 2) of aliased Extensions.unsigned_long_long;
   type File_Unique_ID_T is record
      data : aliased anon_array1106;  -- install/include/clang-c/CXFile.h:51
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/CXFile.h:52

  --*
  -- * Retrieve the unique ID for the given \c file.
  -- *
  -- * \param file the file to get the ID for.
  -- * \param outID stores the returned CXFileUniqueID.
  -- * \returns If there was a failure getting the unique ID, returns non-zero,
  -- * otherwise returns 0.
  --  

   function Get_File_Unique_ID (File : File_T; Out_ID : access File_Unique_ID_T) return int  -- install/include/clang-c/CXFile.h:62
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getFileUniqueID";

  --*
  -- * Returns non-zero if the \c file1 and \c file2 point to the same file,
  -- * or they are both NULL.
  --  

function File_Is_Equal
     (File_1 : File_T;
      File_2 : File_T)
      return Boolean;

  --*
  -- * Returns the real path name of \c file.
  -- *
  -- * An empty string may be returned. Use \c clang_getFileName() in that case.
  --  

function File_Try_Get_Real_Path_Name
     (File : File_T)
      return String;

  --*
  -- * @}
  --  

end Clang.CX_File;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
