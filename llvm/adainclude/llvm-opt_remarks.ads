pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with Interfaces.C.Strings;
with stdint_h;
with System;
with LLVM.Types;

package LLVM.Opt_Remarks is

   OPT_REMARKS_API_VERSION : constant := 0;  --  llvm-8.0.0.src/include/llvm-c/OptRemarks.h:34

  --===-- llvm-c/OptRemarks.h - OptRemarks Public C Interface -------*- C -*-===*|*                                                                            *|
  --|
  --|*                     The LLVM Compiler Infrastructure                       *|
  --|*                                                                            *|
  --|* This file is distributed under the University of Illinois Open Source      *|
  --|* License. See LICENSE.TXT for details.                                      *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header provides a public interface to an opt-remark library.          *|
  --|* LLVM provides an implementation of this interface.                         *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCOPTREMARKS OptRemarks
  -- * @ingroup LLVMC
  -- *
  -- * @{
  --  

  --*
  -- * String containing a buffer and a length. The buffer is not guaranteed to be
  -- * zero-terminated.
  -- *
  -- * \since OPT_REMARKS_API_VERSION=0
  --  

   type Opt_Remark_String_T is record
      Str : Interfaces.C.Strings.chars_ptr;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:43
      Len : aliased stdint_h.uint32_t;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:44
   end record;
   pragma Convention (C_Pass_By_Copy, Opt_Remark_String_T);  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:45

   --  skipped anonymous struct anon_36

  --*
  -- * DebugLoc containing File, Line and Column.
  -- *
  -- * \since OPT_REMARKS_API_VERSION=0
  --  

  -- File:
   type Opt_Remark_Debug_Loc_T is record
      SourceFile : aliased Opt_Remark_String_T;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:54
      SourceLineNumber : aliased stdint_h.uint32_t;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:56
      SourceColumnNumber : aliased stdint_h.uint32_t;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:58
   end record;
   pragma Convention (C_Pass_By_Copy, Opt_Remark_Debug_Loc_T);  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:59

   --  skipped anonymous struct anon_37

  -- Line:
  -- Column:
  --*
  -- * Element of the "Args" list. The key might give more information about what
  -- * are the semantics of the value, e.g. "Callee" will tell you that the value
  -- * is a symbol that names a function.
  -- *
  -- * \since OPT_REMARKS_API_VERSION=0
  --  

  -- e.g. "Callee"
   type Opt_Remark_Arg_T is record
      Key : aliased Opt_Remark_String_T;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:70
      Value : aliased Opt_Remark_String_T;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:72
      DebugLoc : aliased Opt_Remark_Debug_Loc_T;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:75
   end record;
   pragma Convention (C_Pass_By_Copy, Opt_Remark_Arg_T);  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:76

   --  skipped anonymous struct anon_38

  -- e.g. "malloc"
  -- "DebugLoc": Optional
  --*
  -- * One remark entry.
  -- *
  -- * \since OPT_REMARKS_API_VERSION=0
  --  

  -- e.g. !Missed, !Passed
   type Opt_Remark_Entry_T is record
      RemarkType : aliased Opt_Remark_String_T;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:85
      PassName : aliased Opt_Remark_String_T;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:87
      RemarkName : aliased Opt_Remark_String_T;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:89
      FunctionName : aliased Opt_Remark_String_T;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:91
      DebugLoc : aliased Opt_Remark_Debug_Loc_T;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:94
      Hotness : aliased stdint_h.uint32_t;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:96
      NumArgs : aliased stdint_h.uint32_t;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:98
      Args : access Opt_Remark_Arg_T;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:99
   end record;
   pragma Convention (C_Pass_By_Copy, Opt_Remark_Entry_T);  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:100

   --  skipped anonymous struct anon_39

  -- "Pass": Required
  -- "Name": Required
  -- "Function": Required
  -- "DebugLoc": Optional
  -- "Hotness": Optional
  -- "Args": Optional. It is an array of `num_args` elements.
   --  skipped empty struct LLVMOptRemarkOpaqueParser

   type Opt_Remark_Parser_T is new System.Address;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:102

  --*
  -- * Creates a remark parser that can be used to read and parse the buffer located
  -- * in \p Buf of size \p Size.
  -- *
  -- * \p Buf cannot be NULL.
  -- *
  -- * This function should be paired with LLVMOptRemarkParserDispose() to avoid
  -- * leaking resources.
  -- *
  -- * \since OPT_REMARKS_API_VERSION=0
  --  

   function Opt_Remark_Parser_Create (Buf : System.Address; Size : stdint_h.uint64_t) return Opt_Remark_Parser_T;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:115
   pragma Import (C, Opt_Remark_Parser_Create, "LLVMOptRemarkParserCreate");

  --*
  -- * Returns the next remark in the file.
  -- *
  -- * The value pointed to by the return value is invalidated by the next call to
  -- * LLVMOptRemarkParserGetNext().
  -- *
  -- * If the parser reaches the end of the buffer, the return value will be NULL.
  -- *
  -- * In the case of an error, the return value will be NULL, and:
  -- *
  -- * 1) LLVMOptRemarkParserHasError() will return `1`.
  -- *
  -- * 2) LLVMOptRemarkParserGetErrorMessage() will return a descriptive error
  -- *    message.
  -- *
  -- * An error may occur if:
  -- *
  -- * 1) An argument is invalid.
  -- *
  -- * 2) There is a YAML parsing error. This type of error aborts parsing
  -- *    immediately and returns `1`. It can occur on malformed YAML.
  -- *
  -- * 3) Remark parsing error. If this type of error occurs, the parser won't call
  -- *    the handler and will continue to the next one. It can occur on malformed
  -- *    remarks, like missing or extra fields in the file.
  -- *
  -- * Here is a quick example of the usage:
  -- *
  -- * ```
  -- *  LLVMOptRemarkParserRef Parser = LLVMOptRemarkParserCreate(Buf, Size);
  -- *  LLVMOptRemarkEntry *Remark = NULL;
  -- *  while ((Remark == LLVMOptRemarkParserGetNext(Parser))) {
  -- *    // use Remark
  -- *  }
  -- *  bool HasError = LLVMOptRemarkParserHasError(Parser);
  -- *  LLVMOptRemarkParserDispose(Parser);
  -- * ```
  -- *
  -- * \since OPT_REMARKS_API_VERSION=0
  --  

   function Opt_Remark_Parser_Get_Next (Parser : Opt_Remark_Parser_T) return access Opt_Remark_Entry_T;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:159
   pragma Import (C, Opt_Remark_Parser_Get_Next, "LLVMOptRemarkParserGetNext");

  --*
  -- * Returns `1` if the parser encountered an error while parsing the buffer.
  -- *
  -- * \since OPT_REMARKS_API_VERSION=0
  --  

   function Opt_Remark_Parser_Has_Error
     (Parser : Opt_Remark_Parser_T)
      return Boolean;
   function Opt_Remark_Parser_Has_Error_C
     (Parser : Opt_Remark_Parser_T)
      return LLVM.Types.Bool_T;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:166
   pragma Import (C, Opt_Remark_Parser_Has_Error_C, "LLVMOptRemarkParserHasError");

  --*
  -- * Returns a null-terminated string containing an error message.
  -- *
  -- * In case of no error, the result is `NULL`.
  -- *
  -- * The memory of the string is bound to the lifetime of \p Parser. If
  -- * LLVMOptRemarkParserDispose() is called, the memory of the string will be
  -- * released.
  -- *
  -- * \since OPT_REMARKS_API_VERSION=0
  --  

   function Opt_Remark_Parser_Get_Error_Message
     (Parser : Opt_Remark_Parser_T)
      return String;
   function Opt_Remark_Parser_Get_Error_Message_C
     (Parser : Opt_Remark_Parser_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:180
   pragma Import (C, Opt_Remark_Parser_Get_Error_Message_C, "LLVMOptRemarkParserGetErrorMessage");

  --*
  -- * Releases all the resources used by \p Parser.
  -- *
  -- * \since OPT_REMARKS_API_VERSION=0
  --  

   procedure Opt_Remark_Parser_Dispose (Parser : Opt_Remark_Parser_T);  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:187
   pragma Import (C, Opt_Remark_Parser_Dispose, "LLVMOptRemarkParserDispose");

  --*
  -- * Returns the version of the opt-remarks dylib.
  -- *
  -- * \since OPT_REMARKS_API_VERSION=0
  --  

   function Opt_Remark_Version return stdint_h.uint32_t;  -- llvm-8.0.0.src/include/llvm-c/OptRemarks.h:194
   pragma Import (C, Opt_Remark_Version, "LLVMOptRemarkVersion");

  --*
  -- * @} // endgoup LLVMCOPTREMARKS
  --  

end LLVM.Opt_Remarks;

