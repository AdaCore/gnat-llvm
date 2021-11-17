pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with Clang.Index;
with Interfaces.C.Strings;

package Clang.Rewrite is

  --===-- clang-c/Rewrite.h - C CXRewriter   --------------------------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------=== 

   type Rewriter_T is new System.Address;  -- llvm-13.0.0.src/tools/clang/include/clang-c/Rewrite.h:20

  --*
  -- * Create CXRewriter.
  --  

   function CX_Rewriter_Create (TU : Clang.Index.Translation_Unit_T) return Rewriter_T  -- llvm-13.0.0.src/tools/clang/include/clang-c/Rewrite.h:25
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CXRewriter_create";

  --*
  -- * Insert the specified string at the specified location in the original buffer.
  --  

procedure CX_Rewriter_Insert_Text_Before
     (Rew    : Rewriter_T;
      Loc    : Clang.Index.Source_Location_T;
      Insert : String);
   procedure CX_Rewriter_Insert_Text_Before_C
     (Rew    : Rewriter_T;
      Loc    : Clang.Index.Source_Location_T;
      Insert : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "clang_CXRewriter_insertTextBefore";

  --*
  -- * Replace the specified range of characters in the input with the specified
  -- * replacement.
  --  

procedure CX_Rewriter_Replace_Text
     (Rew            : Rewriter_T;
      To_Be_Replaced : Clang.Index.Source_Range_T;
      Replacement    : String);
   procedure CX_Rewriter_Replace_Text_C
     (Rew            : Rewriter_T;
      To_Be_Replaced : Clang.Index.Source_Range_T;
      Replacement    : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "clang_CXRewriter_replaceText";

  --*
  -- * Remove the specified range.
  --  

   procedure CX_Rewriter_Remove_Text (Rew : Rewriter_T; To_Be_Removed : Clang.Index.Source_Range_T)  -- llvm-13.0.0.src/tools/clang/include/clang-c/Rewrite.h:43
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CXRewriter_removeText";

  --*
  -- * Save all changed files to disk.
  -- * Returns 1 if any files were not saved successfully, returns 0 otherwise.
  --  

   function CX_Rewriter_Overwrite_Changed_Files (Rew : Rewriter_T) return int  -- llvm-13.0.0.src/tools/clang/include/clang-c/Rewrite.h:49
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CXRewriter_overwriteChangedFiles";

  --*
  -- * Write out rewritten version of the main file to stdout.
  --  

   procedure CX_Rewriter_Write_Main_File_To_Std_Out (Rew : Rewriter_T)  -- llvm-13.0.0.src/tools/clang/include/clang-c/Rewrite.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CXRewriter_writeMainFileToStdOut";

  --*
  -- * Free the given CXRewriter.
  --  

   procedure CX_Rewriter_Dispose (Rew : Rewriter_T)  -- llvm-13.0.0.src/tools/clang/include/clang-c/Rewrite.h:59
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CXRewriter_dispose";

end Clang.Rewrite;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
