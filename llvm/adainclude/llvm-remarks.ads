pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with Interfaces.C.Strings;
with stdint_h;
with System;
with LLVM.Types;

package LLVM.Remarks is

   REMARKS_API_VERSION : constant := 1;  --  install/include/llvm-c/Remarks.h:36

  --===-- llvm-c/Remarks.h - Remarks Public C Interface -------------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header provides a public interface to a remark diagnostics library.   *|
  --|* LLVM provides an implementation of this interface.                         *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCREMARKS Remarks
  -- * @ingroup LLVMC
  -- *
  -- * @{
  --  

  -- 0 -> 1: Bitstream remarks support.
  --*
  -- * The type of the emitted remark.
  --  

   type Remark_Type_T is 
     (Remark_Type_Unknown,
      Remark_Type_Passed,
      Remark_Type_Missed,
      Remark_Type_Analysis,
      Remark_Type_Analysis_FP_Commute,
      Remark_Type_Analysis_Aliasing,
      Remark_Type_Failure)
   with Convention => C;  -- install/include/llvm-c/Remarks.h:41

  --*
  -- * String containing a buffer and a length. The buffer is not guaranteed to be
  -- * zero-terminated.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   type Remark_Opaque_String_Impl_T is null record;   -- incomplete struct

   type Remark_String_T is access all Remark_Opaque_String_Impl_T;  -- install/include/llvm-c/Remarks.h:57

  --*
  -- * Returns the buffer holding the string.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

function Remark_String_Get_Data
     (Str : Remark_String_T)
      return String;

  --*
  -- * Returns the size of the string.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_String_Get_Len (Str : Remark_String_T) return stdint_h.uint32_t  -- install/include/llvm-c/Remarks.h:71
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkStringGetLen";

  --*
  -- * DebugLoc containing File, Line and Column.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   type Remark_Opaque_Debug_Loc_Impl_T is null record;   -- incomplete struct

   type Remark_Debug_Loc_T is access all Remark_Opaque_Debug_Loc_Impl_T;  -- install/include/llvm-c/Remarks.h:78

  --*
  -- * Return the path to the source file for a debug location.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Debug_Loc_Get_Source_File_Path (DL : Remark_Debug_Loc_T) return Remark_String_T  -- install/include/llvm-c/Remarks.h:86
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkDebugLocGetSourceFilePath";

  --*
  -- * Return the line in the source file for a debug location.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Debug_Loc_Get_Source_Line (DL : Remark_Debug_Loc_T) return stdint_h.uint32_t  -- install/include/llvm-c/Remarks.h:93
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkDebugLocGetSourceLine";

  --*
  -- * Return the column in the source file for a debug location.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Debug_Loc_Get_Source_Column (DL : Remark_Debug_Loc_T) return stdint_h.uint32_t  -- install/include/llvm-c/Remarks.h:100
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkDebugLocGetSourceColumn";

  --*
  -- * Element of the "Args" list. The key might give more information about what
  -- * the semantics of the value are, e.g. "Callee" will tell you that the value
  -- * is a symbol that names a function.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   type Remark_Opaque_Arg_Impl_T is null record;   -- incomplete struct

   type Remark_Arg_T is access all Remark_Opaque_Arg_Impl_T;  -- install/include/llvm-c/Remarks.h:109

  --*
  -- * Returns the key of an argument. The key defines what the value is, and the
  -- * same key can appear multiple times in the list of arguments.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Arg_Get_Key (Arg : Remark_Arg_T) return Remark_String_T  -- install/include/llvm-c/Remarks.h:117
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkArgGetKey";

  --*
  -- * Returns the value of an argument. This is a string that can contain newlines.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Arg_Get_Value (Arg : Remark_Arg_T) return Remark_String_T  -- install/include/llvm-c/Remarks.h:124
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkArgGetValue";

  --*
  -- * Returns the debug location that is attached to the value of this argument.
  -- *
  -- * If there is no debug location, the return value will be `NULL`.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Arg_Get_Debug_Loc (Arg : Remark_Arg_T) return Remark_Debug_Loc_T  -- install/include/llvm-c/Remarks.h:133
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkArgGetDebugLoc";

  --*
  -- * A remark emitted by the compiler.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   type Remark_Opaque_Entry_Impl_T is null record;   -- incomplete struct

   type Remark_Entry_T is access all Remark_Opaque_Entry_Impl_T;  -- install/include/llvm-c/Remarks.h:140

  --*
  -- * Free the resources used by the remark entry.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   procedure Remark_Entry_Dispose (Remark : Remark_Entry_T)  -- install/include/llvm-c/Remarks.h:147
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkEntryDispose";

  --*
  -- * The type of the remark. For example, it can allow users to only keep the
  -- * missed optimizations from the compiler.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Entry_Get_Type (Remark : Remark_Entry_T) return Remark_Type_T  -- install/include/llvm-c/Remarks.h:155
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkEntryGetType";

  --*
  -- * Get the name of the pass that emitted this remark.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Entry_Get_Pass_Name (Remark : Remark_Entry_T) return Remark_String_T  -- install/include/llvm-c/Remarks.h:163
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkEntryGetPassName";

  --*
  -- * Get an identifier of the remark.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Entry_Get_Remark_Name (Remark : Remark_Entry_T) return Remark_String_T  -- install/include/llvm-c/Remarks.h:171
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkEntryGetRemarkName";

  --*
  -- * Get the name of the function being processed when the remark was emitted.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Entry_Get_Function_Name (Remark : Remark_Entry_T) return Remark_String_T  -- install/include/llvm-c/Remarks.h:179
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkEntryGetFunctionName";

  --*
  -- * Returns the debug location that is attached to this remark.
  -- *
  -- * If there is no debug location, the return value will be `NULL`.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Entry_Get_Debug_Loc (Remark : Remark_Entry_T) return Remark_Debug_Loc_T  -- install/include/llvm-c/Remarks.h:189
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkEntryGetDebugLoc";

  --*
  -- * Return the hotness of the remark.
  -- *
  -- * A hotness of `0` means this value is not set.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Entry_Get_Hotness (Remark : Remark_Entry_T) return stdint_h.uint64_t  -- install/include/llvm-c/Remarks.h:198
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkEntryGetHotness";

  --*
  -- * The number of arguments the remark holds.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Entry_Get_Num_Args (Remark : Remark_Entry_T) return stdint_h.uint32_t  -- install/include/llvm-c/Remarks.h:205
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkEntryGetNumArgs";

  --*
  -- * Get a new iterator to iterate over a remark's argument.
  -- *
  -- * If there are no arguments in \p Remark, the return value will be `NULL`.
  -- *
  -- * The lifetime of the returned value is bound to the lifetime of \p Remark.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Entry_Get_First_Arg (Remark : Remark_Entry_T) return Remark_Arg_T  -- install/include/llvm-c/Remarks.h:216
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkEntryGetFirstArg";

  --*
  -- * Get the next argument in \p Remark from the position of \p It.
  -- *
  -- * Returns `NULL` if there are no more arguments available.
  -- *
  -- * The lifetime of the returned value is bound to the lifetime of \p Remark.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Entry_Get_Next_Arg (It : Remark_Arg_T; Remark : Remark_Entry_T) return Remark_Arg_T  -- install/include/llvm-c/Remarks.h:227
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkEntryGetNextArg";

   type Remark_Opaque_Parser_Impl_T is null record;   -- incomplete struct

   type Remark_Parser_T is access all Remark_Opaque_Parser_Impl_T;  -- install/include/llvm-c/Remarks.h:230

  --*
  -- * Creates a remark parser that can be used to parse the buffer located in \p
  -- * Buf of size \p Size bytes.
  -- *
  -- * \p Buf cannot be `NULL`.
  -- *
  -- * This function should be paired with LLVMRemarkParserDispose() to avoid
  -- * leaking resources.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Parser_Create_YAML (Buf : System.Address; Size : stdint_h.uint64_t) return Remark_Parser_T  -- install/include/llvm-c/Remarks.h:243
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkParserCreateYAML";

  --*
  -- * Creates a remark parser that can be used to parse the buffer located in \p
  -- * Buf of size \p Size bytes.
  -- *
  -- * \p Buf cannot be `NULL`.
  -- *
  -- * This function should be paired with LLVMRemarkParserDispose() to avoid
  -- * leaking resources.
  -- *
  -- * \since REMARKS_API_VERSION=1
  --  

   function Remark_Parser_Create_Bitstream (Buf : System.Address; Size : stdint_h.uint64_t) return Remark_Parser_T  -- install/include/llvm-c/Remarks.h:257
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkParserCreateBitstream";

  --*
  -- * Returns the next remark in the file.
  -- *
  -- * The value pointed to by the return value needs to be disposed using a call to
  -- * LLVMRemarkEntryDispose().
  -- *
  -- * All the entries in the returned value that are of LLVMRemarkStringRef type
  -- * will become invalidated once a call to LLVMRemarkParserDispose is made.
  -- *
  -- * If the parser reaches the end of the buffer, the return value will be `NULL`.
  -- *
  -- * In the case of an error, the return value will be `NULL`, and:
  -- *
  -- * 1) LLVMRemarkParserHasError() will return `1`.
  -- *
  -- * 2) LLVMRemarkParserGetErrorMessage() will return a descriptive error
  -- *    message.
  -- *
  -- * An error may occur if:
  -- *
  -- * 1) An argument is invalid.
  -- *
  -- * 2) There is a parsing error. This can occur on things like malformed YAML.
  -- *
  -- * 3) There is a Remark semantic error. This can occur on well-formed files with
  -- *    missing or extra fields.
  -- *
  -- * Here is a quick example of the usage:
  -- *
  -- * ```
  -- * LLVMRemarkParserRef Parser = LLVMRemarkParserCreateYAML(Buf, Size);
  -- * LLVMRemarkEntryRef Remark = NULL;
  -- * while ((Remark = LLVMRemarkParserGetNext(Parser))) {
  -- *    // use Remark
  -- *    LLVMRemarkEntryDispose(Remark); // Release memory.
  -- * }
  -- * bool HasError = LLVMRemarkParserHasError(Parser);
  -- * LLVMRemarkParserDispose(Parser);
  -- * ```
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Parser_Get_Next (Parser : Remark_Parser_T) return Remark_Entry_T  -- install/include/llvm-c/Remarks.h:302
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkParserGetNext";

  --*
  -- * Returns `1` if the parser encountered an error while parsing the buffer.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

function Remark_Parser_Has_Error
     (Parser : Remark_Parser_T)
      return Boolean;

  --*
  -- * Returns a null-terminated string containing an error message.
  -- *
  -- * In case of no error, the result is `NULL`.
  -- *
  -- * The memory of the string is bound to the lifetime of \p Parser. If
  -- * LLVMRemarkParserDispose() is called, the memory of the string will be
  -- * released.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

function Remark_Parser_Get_Error_Message
     (Parser : Remark_Parser_T)
      return String;

  --*
  -- * Releases all the resources used by \p Parser.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   procedure Remark_Parser_Dispose (Parser : Remark_Parser_T)  -- install/include/llvm-c/Remarks.h:329
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkParserDispose";

  --*
  -- * Returns the version of the remarks library.
  -- *
  -- * \since REMARKS_API_VERSION=0
  --  

   function Remark_Version return stdint_h.uint32_t  -- install/include/llvm-c/Remarks.h:336
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemarkVersion";

  --*
  -- * @} // endgoup LLVMCREMARKS
  --  

end LLVM.Remarks;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
