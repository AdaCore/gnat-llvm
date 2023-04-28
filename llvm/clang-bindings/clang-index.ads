pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with Interfaces.C.Strings;
with Clang.CX_File;
with stddef_h;
with Clang.CX_Source_Location;
with Clang.CX_Diagnostic;
with Clang.CX_String;
with Clang.CX_Error_Code;
with Interfaces.C.Extensions;

package Clang.Index is

   CINDEX_VERSION_MAJOR : constant := 0;  --  install/include/clang-c/Index.h:36
   CINDEX_VERSION_MINOR : constant := 63;  --  install/include/clang-c/Index.h:37
   --  arg-macro: function CINDEX_VERSION_ENCODE (major, minor)
   --    return ((major)*10000) + ((minor)*1);
   --  unsupported macro: CINDEX_VERSION CINDEX_VERSION_ENCODE(CINDEX_VERSION_MAJOR, CINDEX_VERSION_MINOR)
   --  unsupported macro: CINDEX_VERSION_STRINGIZE_(major,minor) #major "." #minor
   --  arg-macro: procedure CINDEX_VERSION_STRINGIZE (major, minor)
   --    CINDEX_VERSION_STRINGIZE_(major, minor)
   --  unsupported macro: CINDEX_VERSION_STRING CINDEX_VERSION_STRINGIZE(CINDEX_VERSION_MAJOR, CINDEX_VERSION_MINOR)

  --===-- clang-c/Index.h - Indexing Public C Interface -------------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header provides a public interface to a Clang library for extracting  *|
  --|* high-level symbol information from source files without exposing the full  *|
  --|* Clang C++ API.                                                             *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * The version constants for the libclang API.
  -- * CINDEX_VERSION_MINOR should increase when there are API additions.
  -- * CINDEX_VERSION_MAJOR is intended for "major" source/ABI breaking changes.
  -- *
  -- * The policy about the libclang API was always to keep it source and ABI
  -- * compatible, thus CINDEX_VERSION_MAJOR is expected to remain stable.
  --  

  --* \defgroup CINDEX libclang: C Interface to Clang
  -- *
  -- * The C Interface to Clang provides a relatively small API that exposes
  -- * facilities for parsing source code into an abstract syntax tree (AST),
  -- * loading already-parsed ASTs, traversing the AST, associating
  -- * physical source locations with elements within the AST, and other
  -- * facilities that support Clang-based development tools.
  -- *
  -- * This C interface to Clang will never provide all of the information
  -- * representation stored in Clang's C++ AST, nor should it: the intent is to
  -- * maintain an API that is relatively stable from one release to the next,
  -- * providing only the basic functionality needed to support development tools.
  -- *
  -- * To avoid namespace pollution, data types are prefixed with "CX" and
  -- * functions are prefixed with "clang_".
  -- *
  -- * @{
  --  

  --*
  -- * An "index" that consists of a set of translation units that would
  -- * typically be linked together into an executable or library.
  --  

   type Index_T is new System.Address;  -- install/include/clang-c/Index.h:76

  --*
  -- * An opaque type representing target information for a given translation
  -- * unit.
  --  

   type Target_Info_Impl_T is null record;   -- incomplete struct

   type Target_Info_T is access all Target_Info_Impl_T;  -- install/include/clang-c/Index.h:82

  --*
  -- * A single translation unit, which resides in an index.
  --  

   type Translation_Unit_Impl_T is null record;   -- incomplete struct

   type Translation_Unit_T is access all Translation_Unit_Impl_T;  -- install/include/clang-c/Index.h:87

  --*
  -- * Opaque pointer representing client data that will be passed through
  -- * to various callbacks and visitors.
  --  

   type Client_Data_T is new System.Address;  -- install/include/clang-c/Index.h:93

  --*
  -- * Provides the contents of a file that has not yet been saved to disk.
  -- *
  -- * Each CXUnsavedFile instance provides the name of a file on the
  -- * system along with the current contents of that file that have not
  -- * yet been saved to disk.
  --  

  --*
  --   * The file whose contents have not yet been saved.
  --   *
  --   * This file must already exist in the file system.
  --    

   type Unsaved_File_T is record
      Filename : Interfaces.C.Strings.chars_ptr;  -- install/include/clang-c/Index.h:108
      Contents : Interfaces.C.Strings.chars_ptr;  -- install/include/clang-c/Index.h:113
      Length : aliased unsigned_long;  -- install/include/clang-c/Index.h:118
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:102

  --*
  --   * A buffer containing the unsaved contents of this file.
  --    

  --*
  --   * The length of the unsaved contents of this buffer.
  --    

  --*
  -- * Describes the availability of a particular entity, which indicates
  -- * whether the use of this entity will result in a warning or error due to
  -- * it being deprecated or unavailable.
  --  

   type Availability_Kind_T is 
     (Availability_Available,
      Availability_Deprecated,
      Availability_Not_Available,
      Availability_Not_Accessible)
   with Convention => C;  -- install/include/clang-c/Index.h:126

  --*
  --   * The entity is available.
  --    

  --*
  --   * The entity is available, but has been deprecated (and its use is
  --   * not recommended).
  --    

  --*
  --   * The entity is not available; any use of it will be an error.
  --    

  --*
  --   * The entity is available, but not accessible; any use of it will be
  --   * an error.
  --    

  --*
  -- * Describes a version number of the form major.minor.subminor.
  --  

  --*
  --   * The major version number, e.g., the '10' in '10.7.3'. A negative
  --   * value indicates that there is no version number at all.
  --    

   type Version_T is record
      Major : aliased int;  -- install/include/clang-c/Index.h:155
      Minor : aliased int;  -- install/include/clang-c/Index.h:161
      Subminor : aliased int;  -- install/include/clang-c/Index.h:167
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:150

  --*
  --   * The minor version number, e.g., the '7' in '10.7.3'. This value
  --   * will be negative if no minor version number was provided, e.g., for
  --   * version '10'.
  --    

  --*
  --   * The subminor version number, e.g., the '3' in '10.7.3'. This value
  --   * will be negative if no minor or subminor version number was provided,
  --   * e.g., in version '10' or '10.7'.
  --    

  --*
  -- * Describes the exception specification of a cursor.
  -- *
  -- * A negative value indicates that the cursor is not a function declaration.
  --  

   type Cursor_Exception_Specification_Kind_T is 
     (Cursor_Exception_Specification_Kind_None,
      Cursor_Exception_Specification_Kind_Dynamic_None,
      Cursor_Exception_Specification_Kind_Dynamic,
      Cursor_Exception_Specification_Kind_MS_Any,
      Cursor_Exception_Specification_Kind_Basic_Noexcept,
      Cursor_Exception_Specification_Kind_Computed_Noexcept,
      Cursor_Exception_Specification_Kind_Unevaluated,
      Cursor_Exception_Specification_Kind_Uninstantiated,
      Cursor_Exception_Specification_Kind_Unparsed,
      Cursor_Exception_Specification_Kind_No_Throw)
   with Convention => C;  -- install/include/clang-c/Index.h:175

  --*
  --   * The cursor has no exception specification.
  --    

  --*
  --   * The cursor has exception specification throw()
  --    

  --*
  --   * The cursor has exception specification throw(T1, T2)
  --    

  --*
  --   * The cursor has exception specification throw(...).
  --    

  --*
  --   * The cursor has exception specification basic noexcept.
  --    

  --*
  --   * The cursor has exception specification computed noexcept.
  --    

  --*
  --   * The exception specification has not yet been evaluated.
  --    

  --*
  --   * The exception specification has not yet been instantiated.
  --    

  --*
  --   * The exception specification has not been parsed yet.
  --    

  --*
  --   * The cursor has a __declspec(nothrow) exception specification.
  --    

  --*
  -- * Provides a shared context for creating translation units.
  -- *
  -- * It provides two options:
  -- *
  -- * - excludeDeclarationsFromPCH: When non-zero, allows enumeration of "local"
  -- * declarations (when loading any new translation units). A "local" declaration
  -- * is one that belongs in the translation unit itself and not in a precompiled
  -- * header that was used by the translation unit. If zero, all declarations
  -- * will be enumerated.
  -- *
  -- * Here is an example:
  -- *
  -- * \code
  -- *   // excludeDeclsFromPCH = 1, displayDiagnostics=1
  -- *   Idx = clang_createIndex(1, 1);
  -- *
  -- *   // IndexTest.pch was produced with the following command:
  -- *   // "clang -x c IndexTest.h -emit-ast -o IndexTest.pch"
  -- *   TU = clang_createTranslationUnit(Idx, "IndexTest.pch");
  -- *
  -- *   // This will load all the symbols from 'IndexTest.pch'
  -- *   clang_visitChildren(clang_getTranslationUnitCursor(TU),
  -- *                       TranslationUnitVisitor, 0);
  -- *   clang_disposeTranslationUnit(TU);
  -- *
  -- *   // This will load all the symbols from 'IndexTest.c', excluding symbols
  -- *   // from 'IndexTest.pch'.
  -- *   char *args[] = { "-Xclang", "-include-pch=IndexTest.pch" };
  -- *   TU = clang_createTranslationUnitFromSourceFile(Idx, "IndexTest.c", 2, args,
  -- *                                                  0, 0);
  -- *   clang_visitChildren(clang_getTranslationUnitCursor(TU),
  -- *                       TranslationUnitVisitor, 0);
  -- *   clang_disposeTranslationUnit(TU);
  -- * \endcode
  -- *
  -- * This process of creating the 'pch', loading it separately, and using it (via
  -- * -include-pch) allows 'excludeDeclsFromPCH' to remove redundant callbacks
  -- * (which gives the indexer the same performance benefit as the compiler).
  --  

   function Create_Index (Exclude_Declarations_From_PCH : int; Display_Diagnostics : int) return Index_T  -- install/include/clang-c/Index.h:267
   with Import => True, 
        Convention => C, 
        External_Name => "clang_createIndex";

  --*
  -- * Destroy the given index.
  -- *
  -- * The index must not be destroyed until all of the translation units created
  -- * within that index have been destroyed.
  --  

   procedure Dispose_Index (Index : Index_T)  -- install/include/clang-c/Index.h:276
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeIndex";

  --*
  --   * Used to indicate that no special CXIndex options are needed.
  --    

  --*
  --   * Used to indicate that threads that libclang creates for indexing
  --   * purposes should use background priority.
  --   *
  --   * Affects #clang_indexSourceFile, #clang_indexTranslationUnit,
  --   * #clang_parseTranslationUnit, #clang_saveTranslationUnit.
  --    

  --*
  --   * Used to indicate that threads that libclang creates for editing
  --   * purposes should use background priority.
  --   *
  --   * Affects #clang_reparseTranslationUnit, #clang_codeCompleteAt,
  --   * #clang_annotateTokens
  --    

  --*
  --   * Used to indicate that all threads that libclang creates should use
  --   * background priority.
  --    

   type Global_Opt_Flags_T is 
     (Global_Opt_None,
      Global_Opt_Thread_Background_Priority_For_Indexing,
      Global_Opt_Thread_Background_Priority_For_Editing,
      Global_Opt_Thread_Background_Priority_For_All)
   with Convention => C;  -- install/include/clang-c/Index.h:310

  --*
  -- * Sets general options associated with a CXIndex.
  -- *
  -- * For example:
  -- * \code
  -- * CXIndex idx = ...;
  -- * clang_CXIndex_setGlobalOptions(idx,
  -- *     clang_CXIndex_getGlobalOptions(idx) |
  -- *     CXGlobalOpt_ThreadBackgroundPriorityForIndexing);
  -- * \endcode
  -- *
  -- * \param options A bitmask of options, a bitwise OR of CXGlobalOpt_XXX flags.
  --  

   procedure CX_Index_Set_Global_Options (Arg_1 : Index_T; Options : unsigned)  -- install/include/clang-c/Index.h:325
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CXIndex_setGlobalOptions";

  --*
  -- * Gets the general options associated with a CXIndex.
  -- *
  -- * \returns A bitmask of options, a bitwise OR of CXGlobalOpt_XXX flags that
  -- * are associated with the given CXIndex object.
  --  

   function CX_Index_Get_Global_Options (Arg_1 : Index_T) return unsigned  -- install/include/clang-c/Index.h:333
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CXIndex_getGlobalOptions";

  --*
  -- * Sets the invocation emission path option in a CXIndex.
  -- *
  -- * The invocation emission path specifies a path which will contain log
  -- * files for certain libclang invocations. A null value (default) implies that
  -- * libclang invocations are not logged..
  --  

procedure CX_Index_Set_Invocation_Emission_Path_Option
     (Arg_1 : Index_T;
      Path  : String);

  --*
  -- * Determine whether the given header is guarded against
  -- * multiple inclusions, either with the conventional
  -- * \#ifndef/\#define/\#endif macro guards or with \#pragma once.
  --  

function Is_File_Multiple_Include_Guarded
     (Tu   : Translation_Unit_T;
      File : Clang.CX_File.File_T)
      return Boolean;

  --*
  -- * Retrieve a file handle within the given translation unit.
  -- *
  -- * \param tu the translation unit
  -- *
  -- * \param file_name the name of the file.
  -- *
  -- * \returns the file handle for the named file in the translation unit \p tu,
  -- * or a NULL file handle if the file was not a part of this translation unit.
  --  

function Get_File
     (Tu        : Translation_Unit_T;
      File_Name : String)
      return Clang.CX_File.File_T;

  --*
  -- * Retrieve the buffer associated with the given file.
  -- *
  -- * \param tu the translation unit
  -- *
  -- * \param file the file for which to retrieve the buffer.
  -- *
  -- * \param size [out] if non-NULL, will be set to the size of the buffer.
  -- *
  -- * \returns a pointer to the buffer in memory that holds the contents of
  -- * \p file, or a NULL pointer when the file is not loaded.
  --  

function Get_File_Contents
     (Tu   : Translation_Unit_T;
      File : Clang.CX_File.File_T;
      Size : access stddef_h.size_t)
      return String;

  --*
  -- * Retrieves the source location associated with a given file/line/column
  -- * in a particular translation unit.
  --  

   function Get_Location
     (Tu : Translation_Unit_T;
      File : Clang.CX_File.File_T;
      Line : unsigned;
      Column : unsigned) return Clang.CX_Source_Location.Source_Location_T  -- install/include/clang-c/Index.h:385
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getLocation";

  --*
  -- * Retrieves the source location associated with a given character offset
  -- * in a particular translation unit.
  --  

   function Get_Location_For_Offset
     (Tu : Translation_Unit_T;
      File : Clang.CX_File.File_T;
      Offset : unsigned) return Clang.CX_Source_Location.Source_Location_T  -- install/include/clang-c/Index.h:392
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getLocationForOffset";

  --*
  -- * Retrieve all ranges that were skipped by the preprocessor.
  -- *
  -- * The preprocessor will skip lines when they are surrounded by an
  -- * if/ifdef/ifndef directive whose condition does not evaluate to true.
  --  

   function Get_Skipped_Ranges (Tu : Translation_Unit_T; File : Clang.CX_File.File_T) return access Clang.CX_Source_Location.Source_Range_List_T  -- install/include/clang-c/Index.h:402
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getSkippedRanges";

  --*
  -- * Retrieve all ranges from all files that were skipped by the
  -- * preprocessor.
  -- *
  -- * The preprocessor will skip lines when they are surrounded by an
  -- * if/ifdef/ifndef directive whose condition does not evaluate to true.
  --  

   function Get_All_Skipped_Ranges (Tu : Translation_Unit_T) return access Clang.CX_Source_Location.Source_Range_List_T  -- install/include/clang-c/Index.h:413
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getAllSkippedRanges";

  --*
  -- * Determine the number of diagnostics produced for the given
  -- * translation unit.
  --  

   function Get_Num_Diagnostics (Unit : Translation_Unit_T) return unsigned  -- install/include/clang-c/Index.h:419
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getNumDiagnostics";

  --*
  -- * Retrieve a diagnostic associated with the given translation unit.
  -- *
  -- * \param Unit the translation unit to query.
  -- * \param Index the zero-based diagnostic number to retrieve.
  -- *
  -- * \returns the requested diagnostic. This diagnostic must be freed
  -- * via a call to \c clang_disposeDiagnostic().
  --  

   function Get_Diagnostic (Unit : Translation_Unit_T; Index : unsigned) return Clang.CX_Diagnostic.Diagnostic_T  -- install/include/clang-c/Index.h:430
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getDiagnostic";

  --*
  -- * Retrieve the complete set of diagnostics associated with a
  -- *        translation unit.
  -- *
  -- * \param Unit the translation unit to query.
  --  

   function Get_Diagnostic_Set_From_TU (Unit : Translation_Unit_T) return Clang.CX_Diagnostic.Diagnostic_Set_T  -- install/include/clang-c/Index.h:440
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getDiagnosticSetFromTU";

  --*
  -- * \defgroup CINDEX_TRANSLATION_UNIT Translation unit manipulation
  -- *
  -- * The routines in this group provide the ability to create and destroy
  -- * translation units from files, either by parsing the contents of the files or
  -- * by reading in a serialized representation of a translation unit.
  -- *
  -- * @{
  --  

  --*
  -- * Get the original translation unit source file name.
  --  

function Get_Translation_Unit_Spelling
     (CT_Unit : Translation_Unit_T)
      return String;

  --*
  -- * Return the CXTranslationUnit for a given source file and the provided
  -- * command line arguments one would pass to the compiler.
  -- *
  -- * Note: The 'source_filename' argument is optional.  If the caller provides a
  -- * NULL pointer, the name of the source file is expected to reside in the
  -- * specified command line arguments.
  -- *
  -- * Note: When encountered in 'clang_command_line_args', the following options
  -- * are ignored:
  -- *
  -- *   '-c'
  -- *   '-emit-ast'
  -- *   '-fsyntax-only'
  -- *   '-o \<output file>'  (both '-o' and '\<output file>' are ignored)
  -- *
  -- * \param CIdx The index object with which the translation unit will be
  -- * associated.
  -- *
  -- * \param source_filename The name of the source file to load, or NULL if the
  -- * source file is included in \p clang_command_line_args.
  -- *
  -- * \param num_clang_command_line_args The number of command-line arguments in
  -- * \p clang_command_line_args.
  -- *
  -- * \param clang_command_line_args The command-line arguments that would be
  -- * passed to the \c clang executable if it were being invoked out-of-process.
  -- * These command-line options will be parsed and will affect how the translation
  -- * unit is parsed. Note that the following options are ignored: '-c',
  -- * '-emit-ast', '-fsyntax-only' (which is the default), and '-o \<output file>'.
  -- *
  -- * \param num_unsaved_files the number of unsaved file entries in \p
  -- * unsaved_files.
  -- *
  -- * \param unsaved_files the files that have not yet been saved to disk
  -- * but may be required for code completion, including the contents of
  -- * those files.  The contents and name of these files (as specified by
  -- * CXUnsavedFile) are copied when necessary, so the client only needs to
  -- * guarantee their validity until the call to this function returns.
  --  

function Create_Translation_Unit_From_Source_File
     (C_Idx                       : Index_T;
      Source_Filename             : String;
      Num_Clang_Command_Line_Args : int;
      Command_Line_Args           : System.Address;
      Num_Unsaved_Files           : unsigned;
      Unsaved_Files               : access Unsaved_File_T)
      return Translation_Unit_T;

  --*
  -- * Same as \c clang_createTranslationUnit2, but returns
  -- * the \c CXTranslationUnit instead of an error code.  In case of an error this
  -- * routine returns a \c NULL \c CXTranslationUnit, without further detailed
  -- * error codes.
  --  

function Create_Translation_Unit
     (C_Idx        : Index_T;
      Ast_Filename : String)
      return Translation_Unit_T;

  --*
  -- * Create a translation unit from an AST file (\c -emit-ast).
  -- *
  -- * \param[out] out_TU A non-NULL pointer to store the created
  -- * \c CXTranslationUnit.
  -- *
  -- * \returns Zero on success, otherwise returns an error code.
  --  

function Create_Translation_Unit_2
     (C_Idx        : Index_T;
      Ast_Filename : String;
      Out_TU       : System.Address)
      return Clang.CX_Error_Code.Error_Code_T;

  --*
  -- * Flags that control the creation of translation units.
  -- *
  -- * The enumerators in this enumeration type are meant to be bitwise
  -- * ORed together to specify which options should be used when
  -- * constructing the translation unit.
  --  

   subtype Translation_Unit_Flags_T is unsigned;
   Translation_Unit_None : constant Translation_Unit_Flags_T := 0;
   Translation_Unit_Detailed_Preprocessing_Record : constant Translation_Unit_Flags_T := 1;
   Translation_Unit_Incomplete : constant Translation_Unit_Flags_T := 2;
   Translation_Unit_Precompiled_Preamble : constant Translation_Unit_Flags_T := 4;
   Translation_Unit_Cache_Completion_Results : constant Translation_Unit_Flags_T := 8;
   Translation_Unit_For_Serialization : constant Translation_Unit_Flags_T := 16;
   Translation_Unit_CXX_Chained_PCH : constant Translation_Unit_Flags_T := 32;
   Translation_Unit_Skip_Function_Bodies : constant Translation_Unit_Flags_T := 64;
   Translation_Unit_Include_Brief_Comments_In_Code_Completion : constant Translation_Unit_Flags_T := 128;
   Translation_Unit_Create_Preamble_On_First_Parse : constant Translation_Unit_Flags_T := 256;
   Translation_Unit_Keep_Going : constant Translation_Unit_Flags_T := 512;
   Translation_Unit_Single_File_Parse : constant Translation_Unit_Flags_T := 1024;
   Translation_Unit_Limit_Skip_Function_Bodies_To_Preamble : constant Translation_Unit_Flags_T := 2048;
   Translation_Unit_Include_Attributed_Types : constant Translation_Unit_Flags_T := 4096;
   Translation_Unit_Visit_Implicit_Attributes : constant Translation_Unit_Flags_T := 8192;
   Translation_Unit_Ignore_Non_Errors_From_Included_Files : constant Translation_Unit_Flags_T := 16384;
   Translation_Unit_Retain_Excluded_Conditional_Blocks : constant Translation_Unit_Flags_T := 32768;  -- install/include/clang-c/Index.h:531

  --*
  --   * Used to indicate that no special translation-unit options are
  --   * needed.
  --    

  --*
  --   * Used to indicate that the parser should construct a "detailed"
  --   * preprocessing record, including all macro definitions and instantiations.
  --   *
  --   * Constructing a detailed preprocessing record requires more memory
  --   * and time to parse, since the information contained in the record
  --   * is usually not retained. However, it can be useful for
  --   * applications that require more detailed information about the
  --   * behavior of the preprocessor.
  --    

  --*
  --   * Used to indicate that the translation unit is incomplete.
  --   *
  --   * When a translation unit is considered "incomplete", semantic
  --   * analysis that is typically performed at the end of the
  --   * translation unit will be suppressed. For example, this suppresses
  --   * the completion of tentative declarations in C and of
  --   * instantiation of implicitly-instantiation function templates in
  --   * C++. This option is typically used when parsing a header with the
  --   * intent of producing a precompiled header.
  --    

  --*
  --   * Used to indicate that the translation unit should be built with an
  --   * implicit precompiled header for the preamble.
  --   *
  --   * An implicit precompiled header is used as an optimization when a
  --   * particular translation unit is likely to be reparsed many times
  --   * when the sources aren't changing that often. In this case, an
  --   * implicit precompiled header will be built containing all of the
  --   * initial includes at the top of the main file (what we refer to as
  --   * the "preamble" of the file). In subsequent parses, if the
  --   * preamble or the files in it have not changed, \c
  --   * clang_reparseTranslationUnit() will re-use the implicit
  --   * precompiled header to improve parsing performance.
  --    

  --*
  --   * Used to indicate that the translation unit should cache some
  --   * code-completion results with each reparse of the source file.
  --   *
  --   * Caching of code-completion results is a performance optimization that
  --   * introduces some overhead to reparsing but improves the performance of
  --   * code-completion operations.
  --    

  --*
  --   * Used to indicate that the translation unit will be serialized with
  --   * \c clang_saveTranslationUnit.
  --   *
  --   * This option is typically used when parsing a header with the intent of
  --   * producing a precompiled header.
  --    

  --*
  --   * DEPRECATED: Enabled chained precompiled preambles in C++.
  --   *
  --   * Note: this is a *temporary* option that is available only while
  --   * we are testing C++ precompiled preamble support. It is deprecated.
  --    

  --*
  --   * Used to indicate that function/method bodies should be skipped while
  --   * parsing.
  --   *
  --   * This option can be used to search for declarations/definitions while
  --   * ignoring the usages.
  --    

  --*
  --   * Used to indicate that brief documentation comments should be
  --   * included into the set of code completions returned from this translation
  --   * unit.
  --    

  --*
  --   * Used to indicate that the precompiled preamble should be created on
  --   * the first parse. Otherwise it will be created on the first reparse. This
  --   * trades runtime on the first parse (serializing the preamble takes time) for
  --   * reduced runtime on the second parse (can now reuse the preamble).
  --    

  --*
  --   * Do not stop processing when fatal errors are encountered.
  --   *
  --   * When fatal errors are encountered while parsing a translation unit,
  --   * semantic analysis is typically stopped early when compiling code. A common
  --   * source for fatal errors are unresolvable include files. For the
  --   * purposes of an IDE, this is undesirable behavior and as much information
  --   * as possible should be reported. Use this flag to enable this behavior.
  --    

  --*
  --   * Sets the preprocessor in a mode for parsing a single file only.
  --    

  --*
  --   * Used in combination with CXTranslationUnit_SkipFunctionBodies to
  --   * constrain the skipping of function bodies to the preamble.
  --   *
  --   * The function bodies of the main file are not skipped.
  --    

  --*
  --   * Used to indicate that attributed types should be included in CXType.
  --    

  --*
  --   * Used to indicate that implicit attributes should be visited.
  --    

  --*
  --   * Used to indicate that non-errors from included files should be ignored.
  --   *
  --   * If set, clang_getDiagnosticSetFromTU() will not report e.g. warnings from
  --   * included files anymore. This speeds up clang_getDiagnosticSetFromTU() for
  --   * the case where these warnings are not of interest, as for an IDE for
  --   * example, which typically shows only the diagnostics in the main file.
  --    

  --*
  --   * Tells the preprocessor not to skip excluded conditional blocks.
  --    

  --*
  -- * Returns the set of flags that is suitable for parsing a translation
  -- * unit that is being edited.
  -- *
  -- * The set of flags returned provide options for \c clang_parseTranslationUnit()
  -- * to indicate that the translation unit is likely to be reparsed many times,
  -- * either explicitly (via \c clang_reparseTranslationUnit()) or implicitly
  -- * (e.g., by code completion (\c clang_codeCompletionAt())). The returned flag
  -- * set contains an unspecified set of optimizations (e.g., the precompiled
  -- * preamble) geared toward improving the performance of these routines. The
  -- * set of optimizations enabled may change from one version to the next.
  --  

   function Default_Editing_Translation_Unit_Options return unsigned  -- install/include/clang-c/Index.h:692
   with Import => True, 
        Convention => C, 
        External_Name => "clang_defaultEditingTranslationUnitOptions";

  --*
  -- * Same as \c clang_parseTranslationUnit2, but returns
  -- * the \c CXTranslationUnit instead of an error code.  In case of an error this
  -- * routine returns a \c NULL \c CXTranslationUnit, without further detailed
  -- * error codes.
  --  

function Parse_Translation_Unit
     (C_Idx                 : Index_T;
      Source_Filename       : String;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Options               : unsigned)
      return Translation_Unit_T;

  --*
  -- * Parse the given source file and the translation unit corresponding
  -- * to that file.
  -- *
  -- * This routine is the main entry point for the Clang C API, providing the
  -- * ability to parse a source file into a translation unit that can then be
  -- * queried by other functions in the API. This routine accepts a set of
  -- * command-line arguments so that the compilation can be configured in the same
  -- * way that the compiler is configured on the command line.
  -- *
  -- * \param CIdx The index object with which the translation unit will be
  -- * associated.
  -- *
  -- * \param source_filename The name of the source file to load, or NULL if the
  -- * source file is included in \c command_line_args.
  -- *
  -- * \param command_line_args The command-line arguments that would be
  -- * passed to the \c clang executable if it were being invoked out-of-process.
  -- * These command-line options will be parsed and will affect how the translation
  -- * unit is parsed. Note that the following options are ignored: '-c',
  -- * '-emit-ast', '-fsyntax-only' (which is the default), and '-o \<output file>'.
  -- *
  -- * \param num_command_line_args The number of command-line arguments in
  -- * \c command_line_args.
  -- *
  -- * \param unsaved_files the files that have not yet been saved to disk
  -- * but may be required for parsing, including the contents of
  -- * those files.  The contents and name of these files (as specified by
  -- * CXUnsavedFile) are copied when necessary, so the client only needs to
  -- * guarantee their validity until the call to this function returns.
  -- *
  -- * \param num_unsaved_files the number of unsaved file entries in \p
  -- * unsaved_files.
  -- *
  -- * \param options A bitmask of options that affects how the translation unit
  -- * is managed but not its compilation. This should be a bitwise OR of the
  -- * CXTranslationUnit_XXX flags.
  -- *
  -- * \param[out] out_TU A non-NULL pointer to store the created
  -- * \c CXTranslationUnit, describing the parsed code and containing any
  -- * diagnostics produced by the compiler.
  -- *
  -- * \returns Zero on success, otherwise returns an error code.
  --  

function Parse_Translation_Unit_2
     (C_Idx                 : Index_T;
      Source_Filename       : String;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Options               : unsigned;
      Out_TU                : System.Address)
      return Clang.CX_Error_Code.Error_Code_T;

  --*
  -- * Same as clang_parseTranslationUnit2 but requires a full command line
  -- * for \c command_line_args including argv[0]. This is useful if the standard
  -- * library paths are relative to the binary.
  --  

function Parse_Translation_Unit_2_Full_Argv
     (C_Idx                 : Index_T;
      Source_Filename       : String;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Options               : unsigned;
      Out_TU                : System.Address)
      return Clang.CX_Error_Code.Error_Code_T;

  --*
  -- * Flags that control how translation units are saved.
  -- *
  -- * The enumerators in this enumeration type are meant to be bitwise
  -- * ORed together to specify which options should be used when
  -- * saving the translation unit.
  --  

   type Save_Translation_Unit_Flags_T is 
     (Save_Translation_Unit_None)
   with Convention => C;  -- install/include/clang-c/Index.h:774

  --*
  --   * Used to indicate that no special saving options are needed.
  --    

  --*
  -- * Returns the set of flags that is suitable for saving a translation
  -- * unit.
  -- *
  -- * The set of flags returned provide options for
  -- * \c clang_saveTranslationUnit() by default. The returned flag
  -- * set contains an unspecified set of options that save translation units with
  -- * the most commonly-requested data.
  --  

   function Default_Save_Options (TU : Translation_Unit_T) return unsigned  -- install/include/clang-c/Index.h:790
   with Import => True, 
        Convention => C, 
        External_Name => "clang_defaultSaveOptions";

  --*
  -- * Describes the kind of error that occurred (if any) in a call to
  -- * \c clang_saveTranslationUnit().
  --  

   type Save_Error_T is 
     (Save_Error_None,
      Save_Error_Unknown,
      Save_Error_Translation_Errors,
      Save_Error_Invalid_TU)
   with Convention => C;  -- install/include/clang-c/Index.h:796

  --*
  --   * Indicates that no error occurred while saving a translation unit.
  --    

  --*
  --   * Indicates that an unknown error occurred while attempting to save
  --   * the file.
  --   *
  --   * This error typically indicates that file I/O failed when attempting to
  --   * write the file.
  --    

  --*
  --   * Indicates that errors during translation prevented this attempt
  --   * to save the translation unit.
  --   *
  --   * Errors that prevent the translation unit from being saved can be
  --   * extracted using \c clang_getNumDiagnostics() and \c clang_getDiagnostic().
  --    

  --*
  --   * Indicates that the translation unit to be saved was somehow
  --   * invalid (e.g., NULL).
  --    

  --*
  -- * Saves a translation unit into a serialized representation of
  -- * that translation unit on disk.
  -- *
  -- * Any translation unit that was parsed without error can be saved
  -- * into a file. The translation unit can then be deserialized into a
  -- * new \c CXTranslationUnit with \c clang_createTranslationUnit() or,
  -- * if it is an incomplete translation unit that corresponds to a
  -- * header, used as a precompiled header when parsing other translation
  -- * units.
  -- *
  -- * \param TU The translation unit to save.
  -- *
  -- * \param FileName The file to which the translation unit will be saved.
  -- *
  -- * \param options A bitmask of options that affects how the translation unit
  -- * is saved. This should be a bitwise OR of the
  -- * CXSaveTranslationUnit_XXX flags.
  -- *
  -- * \returns A value that will match one of the enumerators of the CXSaveError
  -- * enumeration. Zero (CXSaveError_None) indicates that the translation unit was
  -- * saved successfully, while a non-zero value indicates that a problem occurred.
  --  

function Save_Translation_Unit
     (TU        : Translation_Unit_T;
      File_Name : String;
      Options   : unsigned)
      return int;

  --*
  -- * Suspend a translation unit in order to free memory associated with it.
  -- *
  -- * A suspended translation unit uses significantly less memory but on the other
  -- * side does not support any other calls than \c clang_reparseTranslationUnit
  -- * to resume it or \c clang_disposeTranslationUnit to dispose it completely.
  --  

   function Suspend_Translation_Unit (Arg_1 : Translation_Unit_T) return unsigned  -- install/include/clang-c/Index.h:861
   with Import => True, 
        Convention => C, 
        External_Name => "clang_suspendTranslationUnit";

  --*
  -- * Destroy the specified CXTranslationUnit object.
  --  

   procedure Dispose_Translation_Unit (Arg_1 : Translation_Unit_T)  -- install/include/clang-c/Index.h:866
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeTranslationUnit";

  --*
  -- * Flags that control the reparsing of translation units.
  -- *
  -- * The enumerators in this enumeration type are meant to be bitwise
  -- * ORed together to specify which options should be used when
  -- * reparsing the translation unit.
  --  

   type Reparse_Flags_T is 
     (Reparse_None)
   with Convention => C;  -- install/include/clang-c/Index.h:875

  --*
  --   * Used to indicate that no special reparsing options are needed.
  --    

  --*
  -- * Returns the set of flags that is suitable for reparsing a translation
  -- * unit.
  -- *
  -- * The set of flags returned provide options for
  -- * \c clang_reparseTranslationUnit() by default. The returned flag
  -- * set contains an unspecified set of optimizations geared toward common uses
  -- * of reparsing. The set of optimizations enabled may change from one version
  -- * to the next.
  --  

   function Default_Reparse_Options (TU : Translation_Unit_T) return unsigned  -- install/include/clang-c/Index.h:892
   with Import => True, 
        Convention => C, 
        External_Name => "clang_defaultReparseOptions";

  --*
  -- * Reparse the source files that produced this translation unit.
  -- *
  -- * This routine can be used to re-parse the source files that originally
  -- * created the given translation unit, for example because those source files
  -- * have changed (either on disk or as passed via \p unsaved_files). The
  -- * source code will be reparsed with the same command-line options as it
  -- * was originally parsed.
  -- *
  -- * Reparsing a translation unit invalidates all cursors and source locations
  -- * that refer into that translation unit. This makes reparsing a translation
  -- * unit semantically equivalent to destroying the translation unit and then
  -- * creating a new translation unit with the same command-line arguments.
  -- * However, it may be more efficient to reparse a translation
  -- * unit using this routine.
  -- *
  -- * \param TU The translation unit whose contents will be re-parsed. The
  -- * translation unit must originally have been built with
  -- * \c clang_createTranslationUnitFromSourceFile().
  -- *
  -- * \param num_unsaved_files The number of unsaved file entries in \p
  -- * unsaved_files.
  -- *
  -- * \param unsaved_files The files that have not yet been saved to disk
  -- * but may be required for parsing, including the contents of
  -- * those files.  The contents and name of these files (as specified by
  -- * CXUnsavedFile) are copied when necessary, so the client only needs to
  -- * guarantee their validity until the call to this function returns.
  -- *
  -- * \param options A bitset of options composed of the flags in CXReparse_Flags.
  -- * The function \c clang_defaultReparseOptions() produces a default set of
  -- * options recommended for most uses, based on the translation unit.
  -- *
  -- * \returns 0 if the sources could be reparsed.  A non-zero error code will be
  -- * returned if reparsing was impossible, such that the translation unit is
  -- * invalid. In such cases, the only valid call for \c TU is
  -- * \c clang_disposeTranslationUnit(TU).  The error codes returned by this
  -- * routine are described by the \c CXErrorCode enum.
  --  

   function Reparse_Translation_Unit
     (TU : Translation_Unit_T;
      Num_Unsaved_Files : unsigned;
      Unsaved_Files : access Unsaved_File_T;
      Options : unsigned) return int  -- install/include/clang-c/Index.h:934
   with Import => True, 
        Convention => C, 
        External_Name => "clang_reparseTranslationUnit";

  --*
  -- * Categorizes how memory is being used by a translation unit.
  --  

   subtype TU_Resource_Usage_Kind_T is unsigned;
   TU_Resource_Usage_AST : constant TU_Resource_Usage_Kind_T := 1;
   TU_Resource_Usage_Identifiers : constant TU_Resource_Usage_Kind_T := 2;
   TU_Resource_Usage_Selectors : constant TU_Resource_Usage_Kind_T := 3;
   TU_Resource_Usage_Global_Completion_Results : constant TU_Resource_Usage_Kind_T := 4;
   TU_Resource_Usage_Source_Manager_Content_Cache : constant TU_Resource_Usage_Kind_T := 5;
   TU_Resource_Usage_AST_Side_Tables : constant TU_Resource_Usage_Kind_T := 6;
   TU_Resource_Usage_Source_Manager_Membuffer_Malloc : constant TU_Resource_Usage_Kind_T := 7;
   TU_Resource_Usage_Source_Manager_Membuffer_M_Map : constant TU_Resource_Usage_Kind_T := 8;
   TU_Resource_Usage_External_AST_Source_Membuffer_Malloc : constant TU_Resource_Usage_Kind_T := 9;
   TU_Resource_Usage_External_AST_Source_Membuffer_M_Map : constant TU_Resource_Usage_Kind_T := 10;
   TU_Resource_Usage_Preprocessor : constant TU_Resource_Usage_Kind_T := 11;
   TU_Resource_Usage_Preprocessing_Record : constant TU_Resource_Usage_Kind_T := 12;
   TU_Resource_Usage_Source_Manager_Data_Structures : constant TU_Resource_Usage_Kind_T := 13;
   TU_Resource_Usage_Preprocessor_Header_Search : constant TU_Resource_Usage_Kind_T := 14;
   TU_Resource_Usage_MEMORY_IN_BYTES_BEGIN : constant TU_Resource_Usage_Kind_T := 1;
   TU_Resource_Usage_MEMORY_IN_BYTES_END : constant TU_Resource_Usage_Kind_T := 14;
   TU_Resource_Usage_First : constant TU_Resource_Usage_Kind_T := 1;
   TU_Resource_Usage_Last : constant TU_Resource_Usage_Kind_T := 14;  -- install/include/clang-c/Index.h:941

  --*
  -- * Returns the human-readable null-terminated C string that represents
  -- *  the name of the memory category.  This string should never be freed.
  --  

function Get_TU_Resource_Usage_Name
     (Kind : TU_Resource_Usage_Kind_T)
      return String;

  -- The memory usage category.  
   type TU_Resource_Usage_Entry_T is record
      kind : aliased TU_Resource_Usage_Kind_T;  -- install/include/clang-c/Index.h:973
      amount : aliased unsigned_long;  -- install/include/clang-c/Index.h:976
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:971

  -- Amount of resources used.
  --      The units will depend on the resource kind.  

  --*
  -- * The memory usage of a CXTranslationUnit, broken into categories.
  --  

  -- Private data member, used for queries.  
   type TU_Resource_Usage_T is record
      data : System.Address;  -- install/include/clang-c/Index.h:984
      numEntries : aliased unsigned;  -- install/include/clang-c/Index.h:987
      entries : access TU_Resource_Usage_Entry_T;  -- install/include/clang-c/Index.h:991
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:982

  -- The number of entries in the 'entries' array.  
  -- An array of key-value pairs, representing the breakdown of memory
  --            usage.  

  --*
  -- * Return the memory usage of a translation unit.  This object
  -- *  should be released with clang_disposeCXTUResourceUsage().
  --  

   function Get_CXTU_Resource_Usage (TU : Translation_Unit_T) return TU_Resource_Usage_T  -- install/include/clang-c/Index.h:1000
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCXTUResourceUsage";

   procedure Dispose_CXTU_Resource_Usage (Usage : TU_Resource_Usage_T)  -- install/include/clang-c/Index.h:1002
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeCXTUResourceUsage";

  --*
  -- * Get target information for this translation unit.
  -- *
  -- * The CXTargetInfo object cannot outlive the CXTranslationUnit object.
  --  

   function Get_Translation_Unit_Target_Info (CT_Unit : Translation_Unit_T) return Target_Info_T  -- install/include/clang-c/Index.h:1010
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getTranslationUnitTargetInfo";

  --*
  -- * Destroy the CXTargetInfo object.
  --  

   procedure Target_Info_Dispose (Info : Target_Info_T)  -- install/include/clang-c/Index.h:1015
   with Import => True, 
        Convention => C, 
        External_Name => "clang_TargetInfo_dispose";

  --*
  -- * Get the normalized target triple as a string.
  -- *
  -- * Returns the empty string in case of any error.
  --  

function Target_Info_Get_Triple
     (Info : Target_Info_T)
      return String;

  --*
  -- * Get the pointer width of the target in bits.
  -- *
  -- * Returns -1 in case of error.
  --  

   function Target_Info_Get_Pointer_Width (Info : Target_Info_T) return int  -- install/include/clang-c/Index.h:1029
   with Import => True, 
        Convention => C, 
        External_Name => "clang_TargetInfo_getPointerWidth";

  --*
  -- * @}
  --  

  --*
  -- * Describes the kind of entity that a cursor refers to.
  --  

   subtype Cursor_Kind_T is unsigned;
   Cursor_Unexposed_Decl : constant Cursor_Kind_T := 1;
   Cursor_Struct_Decl : constant Cursor_Kind_T := 2;
   Cursor_Union_Decl : constant Cursor_Kind_T := 3;
   Cursor_Class_Decl : constant Cursor_Kind_T := 4;
   Cursor_Enum_Decl : constant Cursor_Kind_T := 5;
   Cursor_Field_Decl : constant Cursor_Kind_T := 6;
   Cursor_Enum_Constant_Decl : constant Cursor_Kind_T := 7;
   Cursor_Function_Decl : constant Cursor_Kind_T := 8;
   Cursor_Var_Decl : constant Cursor_Kind_T := 9;
   Cursor_Parm_Decl : constant Cursor_Kind_T := 10;
   Cursor_Obj_C_Interface_Decl : constant Cursor_Kind_T := 11;
   Cursor_Obj_C_Category_Decl : constant Cursor_Kind_T := 12;
   Cursor_Obj_C_Protocol_Decl : constant Cursor_Kind_T := 13;
   Cursor_Obj_C_Property_Decl : constant Cursor_Kind_T := 14;
   Cursor_Obj_C_Ivar_Decl : constant Cursor_Kind_T := 15;
   Cursor_Obj_C_Instance_Method_Decl : constant Cursor_Kind_T := 16;
   Cursor_Obj_C_Class_Method_Decl : constant Cursor_Kind_T := 17;
   Cursor_Obj_C_Implementation_Decl : constant Cursor_Kind_T := 18;
   Cursor_Obj_C_Category_Impl_Decl : constant Cursor_Kind_T := 19;
   Cursor_Typedef_Decl : constant Cursor_Kind_T := 20;
   Cursor_CXX_Method : constant Cursor_Kind_T := 21;
   Cursor_Namespace : constant Cursor_Kind_T := 22;
   Cursor_Linkage_Spec : constant Cursor_Kind_T := 23;
   Cursor_Constructor : constant Cursor_Kind_T := 24;
   Cursor_Destructor : constant Cursor_Kind_T := 25;
   Cursor_Conversion_Function : constant Cursor_Kind_T := 26;
   Cursor_Template_Type_Parameter : constant Cursor_Kind_T := 27;
   Cursor_Non_Type_Template_Parameter : constant Cursor_Kind_T := 28;
   Cursor_Template_Template_Parameter : constant Cursor_Kind_T := 29;
   Cursor_Function_Template : constant Cursor_Kind_T := 30;
   Cursor_Class_Template : constant Cursor_Kind_T := 31;
   Cursor_Class_Template_Partial_Specialization : constant Cursor_Kind_T := 32;
   Cursor_Namespace_Alias : constant Cursor_Kind_T := 33;
   Cursor_Using_Directive : constant Cursor_Kind_T := 34;
   Cursor_Using_Declaration : constant Cursor_Kind_T := 35;
   Cursor_Type_Alias_Decl : constant Cursor_Kind_T := 36;
   Cursor_Obj_C_Synthesize_Decl : constant Cursor_Kind_T := 37;
   Cursor_Obj_C_Dynamic_Decl : constant Cursor_Kind_T := 38;
   Cursor_CXX_Access_Specifier : constant Cursor_Kind_T := 39;
   Cursor_First_Decl : constant Cursor_Kind_T := 1;
   Cursor_Last_Decl : constant Cursor_Kind_T := 39;
   Cursor_First_Ref : constant Cursor_Kind_T := 40;
   Cursor_Obj_C_Super_Class_Ref : constant Cursor_Kind_T := 40;
   Cursor_Obj_C_Protocol_Ref : constant Cursor_Kind_T := 41;
   Cursor_Obj_C_Class_Ref : constant Cursor_Kind_T := 42;
   Cursor_Type_Ref : constant Cursor_Kind_T := 43;
   Cursor_CXX_Base_Specifier : constant Cursor_Kind_T := 44;
   Cursor_Template_Ref : constant Cursor_Kind_T := 45;
   Cursor_Namespace_Ref : constant Cursor_Kind_T := 46;
   Cursor_Member_Ref : constant Cursor_Kind_T := 47;
   Cursor_Label_Ref : constant Cursor_Kind_T := 48;
   Cursor_Overloaded_Decl_Ref : constant Cursor_Kind_T := 49;
   Cursor_Variable_Ref : constant Cursor_Kind_T := 50;
   Cursor_Last_Ref : constant Cursor_Kind_T := 50;
   Cursor_First_Invalid : constant Cursor_Kind_T := 70;
   Cursor_Invalid_File : constant Cursor_Kind_T := 70;
   Cursor_No_Decl_Found : constant Cursor_Kind_T := 71;
   Cursor_Not_Implemented : constant Cursor_Kind_T := 72;
   Cursor_Invalid_Code : constant Cursor_Kind_T := 73;
   Cursor_Last_Invalid : constant Cursor_Kind_T := 73;
   Cursor_First_Expr : constant Cursor_Kind_T := 100;
   Cursor_Unexposed_Expr : constant Cursor_Kind_T := 100;
   Cursor_Decl_Ref_Expr : constant Cursor_Kind_T := 101;
   Cursor_Member_Ref_Expr : constant Cursor_Kind_T := 102;
   Cursor_Call_Expr : constant Cursor_Kind_T := 103;
   Cursor_Obj_C_Message_Expr : constant Cursor_Kind_T := 104;
   Cursor_Block_Expr : constant Cursor_Kind_T := 105;
   Cursor_Integer_Literal : constant Cursor_Kind_T := 106;
   Cursor_Floating_Literal : constant Cursor_Kind_T := 107;
   Cursor_Imaginary_Literal : constant Cursor_Kind_T := 108;
   Cursor_String_Literal : constant Cursor_Kind_T := 109;
   Cursor_Character_Literal : constant Cursor_Kind_T := 110;
   Cursor_Paren_Expr : constant Cursor_Kind_T := 111;
   Cursor_Unary_Operator : constant Cursor_Kind_T := 112;
   Cursor_Array_Subscript_Expr : constant Cursor_Kind_T := 113;
   Cursor_Binary_Operator : constant Cursor_Kind_T := 114;
   Cursor_Compound_Assign_Operator : constant Cursor_Kind_T := 115;
   Cursor_Conditional_Operator : constant Cursor_Kind_T := 116;
   Cursor_C_Style_Cast_Expr : constant Cursor_Kind_T := 117;
   Cursor_Compound_Literal_Expr : constant Cursor_Kind_T := 118;
   Cursor_Init_List_Expr : constant Cursor_Kind_T := 119;
   Cursor_Addr_Label_Expr : constant Cursor_Kind_T := 120;
   Cursor_Stmt_Expr : constant Cursor_Kind_T := 121;
   Cursor_Generic_Selection_Expr : constant Cursor_Kind_T := 122;
   Cursor_GNU_Null_Expr : constant Cursor_Kind_T := 123;
   Cursor_CXX_Static_Cast_Expr : constant Cursor_Kind_T := 124;
   Cursor_CXX_Dynamic_Cast_Expr : constant Cursor_Kind_T := 125;
   Cursor_CXX_Reinterpret_Cast_Expr : constant Cursor_Kind_T := 126;
   Cursor_CXX_Const_Cast_Expr : constant Cursor_Kind_T := 127;
   Cursor_CXX_Functional_Cast_Expr : constant Cursor_Kind_T := 128;
   Cursor_CXX_Typeid_Expr : constant Cursor_Kind_T := 129;
   Cursor_CXX_Bool_Literal_Expr : constant Cursor_Kind_T := 130;
   Cursor_CXX_Null_Ptr_Literal_Expr : constant Cursor_Kind_T := 131;
   Cursor_CXX_This_Expr : constant Cursor_Kind_T := 132;
   Cursor_CXX_Throw_Expr : constant Cursor_Kind_T := 133;
   Cursor_CXX_New_Expr : constant Cursor_Kind_T := 134;
   Cursor_CXX_Delete_Expr : constant Cursor_Kind_T := 135;
   Cursor_Unary_Expr : constant Cursor_Kind_T := 136;
   Cursor_Obj_C_String_Literal : constant Cursor_Kind_T := 137;
   Cursor_Obj_C_Encode_Expr : constant Cursor_Kind_T := 138;
   Cursor_Obj_C_Selector_Expr : constant Cursor_Kind_T := 139;
   Cursor_Obj_C_Protocol_Expr : constant Cursor_Kind_T := 140;
   Cursor_Obj_C_Bridged_Cast_Expr : constant Cursor_Kind_T := 141;
   Cursor_Pack_Expansion_Expr : constant Cursor_Kind_T := 142;
   Cursor_Size_Of_Pack_Expr : constant Cursor_Kind_T := 143;
   Cursor_Lambda_Expr : constant Cursor_Kind_T := 144;
   Cursor_Obj_C_Bool_Literal_Expr : constant Cursor_Kind_T := 145;
   Cursor_Obj_C_Self_Expr : constant Cursor_Kind_T := 146;
   Cursor_OMP_Array_Section_Expr : constant Cursor_Kind_T := 147;
   Cursor_Obj_C_Availability_Check_Expr : constant Cursor_Kind_T := 148;
   Cursor_Fixed_Point_Literal : constant Cursor_Kind_T := 149;
   Cursor_OMP_Array_Shaping_Expr : constant Cursor_Kind_T := 150;
   Cursor_OMP_Iterator_Expr : constant Cursor_Kind_T := 151;
   Cursor_CXX_Addrspace_Cast_Expr : constant Cursor_Kind_T := 152;
   Cursor_Concept_Specialization_Expr : constant Cursor_Kind_T := 153;
   Cursor_Requires_Expr : constant Cursor_Kind_T := 154;
   Cursor_CXX_Paren_List_Init_Expr : constant Cursor_Kind_T := 155;
   Cursor_Last_Expr : constant Cursor_Kind_T := 155;
   Cursor_First_Stmt : constant Cursor_Kind_T := 200;
   Cursor_Unexposed_Stmt : constant Cursor_Kind_T := 200;
   Cursor_Label_Stmt : constant Cursor_Kind_T := 201;
   Cursor_Compound_Stmt : constant Cursor_Kind_T := 202;
   Cursor_Case_Stmt : constant Cursor_Kind_T := 203;
   Cursor_Default_Stmt : constant Cursor_Kind_T := 204;
   Cursor_If_Stmt : constant Cursor_Kind_T := 205;
   Cursor_Switch_Stmt : constant Cursor_Kind_T := 206;
   Cursor_While_Stmt : constant Cursor_Kind_T := 207;
   Cursor_Do_Stmt : constant Cursor_Kind_T := 208;
   Cursor_For_Stmt : constant Cursor_Kind_T := 209;
   Cursor_Goto_Stmt : constant Cursor_Kind_T := 210;
   Cursor_Indirect_Goto_Stmt : constant Cursor_Kind_T := 211;
   Cursor_Continue_Stmt : constant Cursor_Kind_T := 212;
   Cursor_Break_Stmt : constant Cursor_Kind_T := 213;
   Cursor_Return_Stmt : constant Cursor_Kind_T := 214;
   Cursor_GCC_Asm_Stmt : constant Cursor_Kind_T := 215;
   Cursor_Asm_Stmt : constant Cursor_Kind_T := 215;
   Cursor_Obj_C_At_Try_Stmt : constant Cursor_Kind_T := 216;
   Cursor_Obj_C_At_Catch_Stmt : constant Cursor_Kind_T := 217;
   Cursor_Obj_C_At_Finally_Stmt : constant Cursor_Kind_T := 218;
   Cursor_Obj_C_At_Throw_Stmt : constant Cursor_Kind_T := 219;
   Cursor_Obj_C_At_Synchronized_Stmt : constant Cursor_Kind_T := 220;
   Cursor_Obj_C_Autorelease_Pool_Stmt : constant Cursor_Kind_T := 221;
   Cursor_Obj_C_For_Collection_Stmt : constant Cursor_Kind_T := 222;
   Cursor_CXX_Catch_Stmt : constant Cursor_Kind_T := 223;
   Cursor_CXX_Try_Stmt : constant Cursor_Kind_T := 224;
   Cursor_CXX_For_Range_Stmt : constant Cursor_Kind_T := 225;
   Cursor_SEH_Try_Stmt : constant Cursor_Kind_T := 226;
   Cursor_SEH_Except_Stmt : constant Cursor_Kind_T := 227;
   Cursor_SEH_Finally_Stmt : constant Cursor_Kind_T := 228;
   Cursor_MS_Asm_Stmt : constant Cursor_Kind_T := 229;
   Cursor_Null_Stmt : constant Cursor_Kind_T := 230;
   Cursor_Decl_Stmt : constant Cursor_Kind_T := 231;
   Cursor_OMP_Parallel_Directive : constant Cursor_Kind_T := 232;
   Cursor_OMP_Simd_Directive : constant Cursor_Kind_T := 233;
   Cursor_OMP_For_Directive : constant Cursor_Kind_T := 234;
   Cursor_OMP_Sections_Directive : constant Cursor_Kind_T := 235;
   Cursor_OMP_Section_Directive : constant Cursor_Kind_T := 236;
   Cursor_OMP_Single_Directive : constant Cursor_Kind_T := 237;
   Cursor_OMP_Parallel_For_Directive : constant Cursor_Kind_T := 238;
   Cursor_OMP_Parallel_Sections_Directive : constant Cursor_Kind_T := 239;
   Cursor_OMP_Task_Directive : constant Cursor_Kind_T := 240;
   Cursor_OMP_Master_Directive : constant Cursor_Kind_T := 241;
   Cursor_OMP_Critical_Directive : constant Cursor_Kind_T := 242;
   Cursor_OMP_Taskyield_Directive : constant Cursor_Kind_T := 243;
   Cursor_OMP_Barrier_Directive : constant Cursor_Kind_T := 244;
   Cursor_OMP_Taskwait_Directive : constant Cursor_Kind_T := 245;
   Cursor_OMP_Flush_Directive : constant Cursor_Kind_T := 246;
   Cursor_SEH_Leave_Stmt : constant Cursor_Kind_T := 247;
   Cursor_OMP_Ordered_Directive : constant Cursor_Kind_T := 248;
   Cursor_OMP_Atomic_Directive : constant Cursor_Kind_T := 249;
   Cursor_OMP_For_Simd_Directive : constant Cursor_Kind_T := 250;
   Cursor_OMP_Parallel_For_Simd_Directive : constant Cursor_Kind_T := 251;
   Cursor_OMP_Target_Directive : constant Cursor_Kind_T := 252;
   Cursor_OMP_Teams_Directive : constant Cursor_Kind_T := 253;
   Cursor_OMP_Taskgroup_Directive : constant Cursor_Kind_T := 254;
   Cursor_OMP_Cancellation_Point_Directive : constant Cursor_Kind_T := 255;
   Cursor_OMP_Cancel_Directive : constant Cursor_Kind_T := 256;
   Cursor_OMP_Target_Data_Directive : constant Cursor_Kind_T := 257;
   Cursor_OMP_Task_Loop_Directive : constant Cursor_Kind_T := 258;
   Cursor_OMP_Task_Loop_Simd_Directive : constant Cursor_Kind_T := 259;
   Cursor_OMP_Distribute_Directive : constant Cursor_Kind_T := 260;
   Cursor_OMP_Target_Enter_Data_Directive : constant Cursor_Kind_T := 261;
   Cursor_OMP_Target_Exit_Data_Directive : constant Cursor_Kind_T := 262;
   Cursor_OMP_Target_Parallel_Directive : constant Cursor_Kind_T := 263;
   Cursor_OMP_Target_Parallel_For_Directive : constant Cursor_Kind_T := 264;
   Cursor_OMP_Target_Update_Directive : constant Cursor_Kind_T := 265;
   Cursor_OMP_Distribute_Parallel_For_Directive : constant Cursor_Kind_T := 266;
   Cursor_OMP_Distribute_Parallel_For_Simd_Directive : constant Cursor_Kind_T := 267;
   Cursor_OMP_Distribute_Simd_Directive : constant Cursor_Kind_T := 268;
   Cursor_OMP_Target_Parallel_For_Simd_Directive : constant Cursor_Kind_T := 269;
   Cursor_OMP_Target_Simd_Directive : constant Cursor_Kind_T := 270;
   Cursor_OMP_Teams_Distribute_Directive : constant Cursor_Kind_T := 271;
   Cursor_OMP_Teams_Distribute_Simd_Directive : constant Cursor_Kind_T := 272;
   Cursor_OMP_Teams_Distribute_Parallel_For_Simd_Directive : constant Cursor_Kind_T := 273;
   Cursor_OMP_Teams_Distribute_Parallel_For_Directive : constant Cursor_Kind_T := 274;
   Cursor_OMP_Target_Teams_Directive : constant Cursor_Kind_T := 275;
   Cursor_OMP_Target_Teams_Distribute_Directive : constant Cursor_Kind_T := 276;
   Cursor_OMP_Target_Teams_Distribute_Parallel_For_Directive : constant Cursor_Kind_T := 277;
   Cursor_OMP_Target_Teams_Distribute_Parallel_For_Simd_Directive : constant Cursor_Kind_T := 278;
   Cursor_OMP_Target_Teams_Distribute_Simd_Directive : constant Cursor_Kind_T := 279;
   Cursor_Builtin_Bit_Cast_Expr : constant Cursor_Kind_T := 280;
   Cursor_OMP_Master_Task_Loop_Directive : constant Cursor_Kind_T := 281;
   Cursor_OMP_Parallel_Master_Task_Loop_Directive : constant Cursor_Kind_T := 282;
   Cursor_OMP_Master_Task_Loop_Simd_Directive : constant Cursor_Kind_T := 283;
   Cursor_OMP_Parallel_Master_Task_Loop_Simd_Directive : constant Cursor_Kind_T := 284;
   Cursor_OMP_Parallel_Master_Directive : constant Cursor_Kind_T := 285;
   Cursor_OMP_Depobj_Directive : constant Cursor_Kind_T := 286;
   Cursor_OMP_Scan_Directive : constant Cursor_Kind_T := 287;
   Cursor_OMP_Tile_Directive : constant Cursor_Kind_T := 288;
   Cursor_OMP_Canonical_Loop : constant Cursor_Kind_T := 289;
   Cursor_OMP_Interop_Directive : constant Cursor_Kind_T := 290;
   Cursor_OMP_Dispatch_Directive : constant Cursor_Kind_T := 291;
   Cursor_OMP_Masked_Directive : constant Cursor_Kind_T := 292;
   Cursor_OMP_Unroll_Directive : constant Cursor_Kind_T := 293;
   Cursor_OMP_Meta_Directive : constant Cursor_Kind_T := 294;
   Cursor_OMP_Generic_Loop_Directive : constant Cursor_Kind_T := 295;
   Cursor_OMP_Teams_Generic_Loop_Directive : constant Cursor_Kind_T := 296;
   Cursor_OMP_Target_Teams_Generic_Loop_Directive : constant Cursor_Kind_T := 297;
   Cursor_OMP_Parallel_Generic_Loop_Directive : constant Cursor_Kind_T := 298;
   Cursor_OMP_Target_Parallel_Generic_Loop_Directive : constant Cursor_Kind_T := 299;
   Cursor_OMP_Parallel_Masked_Directive : constant Cursor_Kind_T := 300;
   Cursor_OMP_Masked_Task_Loop_Directive : constant Cursor_Kind_T := 301;
   Cursor_OMP_Masked_Task_Loop_Simd_Directive : constant Cursor_Kind_T := 302;
   Cursor_OMP_Parallel_Masked_Task_Loop_Directive : constant Cursor_Kind_T := 303;
   Cursor_OMP_Parallel_Masked_Task_Loop_Simd_Directive : constant Cursor_Kind_T := 304;
   Cursor_OMP_Error_Directive : constant Cursor_Kind_T := 305;
   Cursor_Last_Stmt : constant Cursor_Kind_T := 305;
   Cursor_Translation_Unit : constant Cursor_Kind_T := 350;
   Cursor_First_Attr : constant Cursor_Kind_T := 400;
   Cursor_Unexposed_Attr : constant Cursor_Kind_T := 400;
   Cursor_IB_Action_Attr : constant Cursor_Kind_T := 401;
   Cursor_IB_Outlet_Attr : constant Cursor_Kind_T := 402;
   Cursor_IB_Outlet_Collection_Attr : constant Cursor_Kind_T := 403;
   Cursor_CXX_Final_Attr : constant Cursor_Kind_T := 404;
   Cursor_CXX_Override_Attr : constant Cursor_Kind_T := 405;
   Cursor_Annotate_Attr : constant Cursor_Kind_T := 406;
   Cursor_Asm_Label_Attr : constant Cursor_Kind_T := 407;
   Cursor_Packed_Attr : constant Cursor_Kind_T := 408;
   Cursor_Pure_Attr : constant Cursor_Kind_T := 409;
   Cursor_Const_Attr : constant Cursor_Kind_T := 410;
   Cursor_No_Duplicate_Attr : constant Cursor_Kind_T := 411;
   Cursor_CUDA_Constant_Attr : constant Cursor_Kind_T := 412;
   Cursor_CUDA_Device_Attr : constant Cursor_Kind_T := 413;
   Cursor_CUDA_Global_Attr : constant Cursor_Kind_T := 414;
   Cursor_CUDA_Host_Attr : constant Cursor_Kind_T := 415;
   Cursor_CUDA_Shared_Attr : constant Cursor_Kind_T := 416;
   Cursor_Visibility_Attr : constant Cursor_Kind_T := 417;
   Cursor_DLL_Export : constant Cursor_Kind_T := 418;
   Cursor_DLL_Import : constant Cursor_Kind_T := 419;
   Cursor_NS_Returns_Retained : constant Cursor_Kind_T := 420;
   Cursor_NS_Returns_Not_Retained : constant Cursor_Kind_T := 421;
   Cursor_NS_Returns_Autoreleased : constant Cursor_Kind_T := 422;
   Cursor_NS_Consumes_Self : constant Cursor_Kind_T := 423;
   Cursor_NS_Consumed : constant Cursor_Kind_T := 424;
   Cursor_Obj_C_Exception : constant Cursor_Kind_T := 425;
   Cursor_Obj_CNS_Object : constant Cursor_Kind_T := 426;
   Cursor_Obj_C_Independent_Class : constant Cursor_Kind_T := 427;
   Cursor_Obj_C_Precise_Lifetime : constant Cursor_Kind_T := 428;
   Cursor_Obj_C_Returns_Inner_Pointer : constant Cursor_Kind_T := 429;
   Cursor_Obj_C_Requires_Super : constant Cursor_Kind_T := 430;
   Cursor_Obj_C_Root_Class : constant Cursor_Kind_T := 431;
   Cursor_Obj_C_Subclassing_Restricted : constant Cursor_Kind_T := 432;
   Cursor_Obj_C_Explicit_Protocol_Impl : constant Cursor_Kind_T := 433;
   Cursor_Obj_C_Designated_Initializer : constant Cursor_Kind_T := 434;
   Cursor_Obj_C_Runtime_Visible : constant Cursor_Kind_T := 435;
   Cursor_Obj_C_Boxable : constant Cursor_Kind_T := 436;
   Cursor_Flag_Enum : constant Cursor_Kind_T := 437;
   Cursor_Convergent_Attr : constant Cursor_Kind_T := 438;
   Cursor_Warn_Unused_Attr : constant Cursor_Kind_T := 439;
   Cursor_Warn_Unused_Result_Attr : constant Cursor_Kind_T := 440;
   Cursor_Aligned_Attr : constant Cursor_Kind_T := 441;
   Cursor_Last_Attr : constant Cursor_Kind_T := 441;
   Cursor_Preprocessing_Directive : constant Cursor_Kind_T := 500;
   Cursor_Macro_Definition : constant Cursor_Kind_T := 501;
   Cursor_Macro_Expansion : constant Cursor_Kind_T := 502;
   Cursor_Macro_Instantiation : constant Cursor_Kind_T := 502;
   Cursor_Inclusion_Directive : constant Cursor_Kind_T := 503;
   Cursor_First_Preprocessing : constant Cursor_Kind_T := 500;
   Cursor_Last_Preprocessing : constant Cursor_Kind_T := 503;
   Cursor_Module_Import_Decl : constant Cursor_Kind_T := 600;
   Cursor_Type_Alias_Template_Decl : constant Cursor_Kind_T := 601;
   Cursor_Static_Assert : constant Cursor_Kind_T := 602;
   Cursor_Friend_Decl : constant Cursor_Kind_T := 603;
   Cursor_Concept_Decl : constant Cursor_Kind_T := 604;
   Cursor_First_Extra_Decl : constant Cursor_Kind_T := 600;
   Cursor_Last_Extra_Decl : constant Cursor_Kind_T := 604;
   Cursor_Overload_Candidate : constant Cursor_Kind_T := 700;  -- install/include/clang-c/Index.h:1038

  -- Declarations  
  --*
  --   * A declaration whose specific kind is not exposed via this
  --   * interface.
  --   *
  --   * Unexposed declarations have the same operations as any other kind
  --   * of declaration; one can extract their location information,
  --   * spelling, find their definitions, etc. However, the specific kind
  --   * of the declaration is not reported.
  --    

  --* A C or C++ struct.  
  --* A C or C++ union.  
  --* A C++ class.  
  --* An enumeration.  
  --*
  --   * A field (in C) or non-static data member (in C++) in a
  --   * struct, union, or C++ class.
  --    

  --* An enumerator constant.  
  --* A function.  
  --* A variable.  
  --* A function or method parameter.  
  --* An Objective-C \@interface.  
  --* An Objective-C \@interface for a category.  
  --* An Objective-C \@protocol declaration.  
  --* An Objective-C \@property declaration.  
  --* An Objective-C instance variable.  
  --* An Objective-C instance method.  
  --* An Objective-C class method.  
  --* An Objective-C \@implementation.  
  --* An Objective-C \@implementation for a category.  
  --* A typedef.  
  --* A C++ class method.  
  --* A C++ namespace.  
  --* A linkage specification, e.g. 'extern "C"'.  
  --* A C++ constructor.  
  --* A C++ destructor.  
  --* A C++ conversion function.  
  --* A C++ template type parameter.  
  --* A C++ non-type template parameter.  
  --* A C++ template template parameter.  
  --* A C++ function template.  
  --* A C++ class template.  
  --* A C++ class template partial specialization.  
  --* A C++ namespace alias declaration.  
  --* A C++ using directive.  
  --* A C++ using declaration.  
  --* A C++ alias declaration  
  --* An Objective-C \@synthesize definition.  
  --* An Objective-C \@dynamic definition.  
  --* An access specifier.  
  -- References  
  -- Decl references  
  --*
  --   * A reference to a type declaration.
  --   *
  --   * A type reference occurs anywhere where a type is named but not
  --   * declared. For example, given:
  --   *
  --   * \code
  --   * typedef unsigned size_type;
  --   * size_type size;
  --   * \endcode
  --   *
  --   * The typedef is a declaration of size_type (CXCursor_TypedefDecl),
  --   * while the type of the variable "size" is referenced. The cursor
  --   * referenced by the type of size is the typedef for size_type.
  --    

  --*
  --   * A reference to a class template, function template, template
  --   * template parameter, or class template partial specialization.
  --    

  --*
  --   * A reference to a namespace or namespace alias.
  --    

  --*
  --   * A reference to a member of a struct, union, or class that occurs in
  --   * some non-expression context, e.g., a designated initializer.
  --    

  --*
  --   * A reference to a labeled statement.
  --   *
  --   * This cursor kind is used to describe the jump to "start_over" in the
  --   * goto statement in the following example:
  --   *
  --   * \code
  --   *   start_over:
  --   *     ++counter;
  --   *
  --   *     goto start_over;
  --   * \endcode
  --   *
  --   * A label reference cursor refers to a label statement.
  --    

  --*
  --   * A reference to a set of overloaded functions or function templates
  --   * that has not yet been resolved to a specific function or function template.
  --   *
  --   * An overloaded declaration reference cursor occurs in C++ templates where
  --   * a dependent name refers to a function. For example:
  --   *
  --   * \code
  --   * template<typename T> void swap(T&, T&);
  --   *
  --   * struct X { ... };
  --   * void swap(X&, X&);
  --   *
  --   * template<typename T>
  --   * void reverse(T* first, T* last) {
  --   *   while (first < last - 1) {
  --   *     swap(*first, *--last);
  --   *     ++first;
  --   *   }
  --   * }
  --   *
  --   * struct Y { };
  --   * void swap(Y&, Y&);
  --   * \endcode
  --   *
  --   * Here, the identifier "swap" is associated with an overloaded declaration
  --   * reference. In the template definition, "swap" refers to either of the two
  --   * "swap" functions declared above, so both results will be available. At
  --   * instantiation time, "swap" may also refer to other functions found via
  --   * argument-dependent lookup (e.g., the "swap" function at the end of the
  --   * example).
  --   *
  --   * The functions \c clang_getNumOverloadedDecls() and
  --   * \c clang_getOverloadedDecl() can be used to retrieve the definitions
  --   * referenced by this cursor.
  --    

  --*
  --   * A reference to a variable that occurs in some non-expression
  --   * context, e.g., a C++ lambda capture list.
  --    

  -- Error conditions  
  -- Expressions  
  --*
  --   * An expression whose specific kind is not exposed via this
  --   * interface.
  --   *
  --   * Unexposed expressions have the same operations as any other kind
  --   * of expression; one can extract their location information,
  --   * spelling, children, etc. However, the specific kind of the
  --   * expression is not reported.
  --    

  --*
  --   * An expression that refers to some value declaration, such
  --   * as a function, variable, or enumerator.
  --    

  --*
  --   * An expression that refers to a member of a struct, union,
  --   * class, Objective-C class, etc.
  --    

  --* An expression that calls a function.  
  --* An expression that sends a message to an Objective-C
  --   object or class.  

  --* An expression that represents a block literal.  
  --* An integer literal.
  --    

  --* A floating point number literal.
  --    

  --* An imaginary number literal.
  --    

  --* A string literal.
  --    

  --* A character literal.
  --    

  --* A parenthesized expression, e.g. "(1)".
  --   *
  --   * This AST node is only formed if full location information is requested.
  --    

  --* This represents the unary-expression's (except sizeof and
  --   * alignof).
  --    

  --* [C99 6.5.2.1] Array Subscripting.
  --    

  --* A builtin binary operation expression such as "x + y" or
  --   * "x <= y".
  --    

  --* Compound assignment such as "+=".
  --    

  --* The ?: ternary operator.
  --    

  --* An explicit cast in C (C99 6.5.4) or a C-style cast in C++
  --   * (C++ [expr.cast]), which uses the syntax (Type)expr.
  --   *
  --   * For example: (int)f.
  --    

  --* [C99 6.5.2.5]
  --    

  --* Describes an C or C++ initializer list.
  --    

  --* The GNU address of label extension, representing &&label.
  --    

  --* This is the GNU Statement Expression extension: ({int X=4; X;})
  --    

  --* Represents a C11 generic selection.
  --    

  --* Implements the GNU __null extension, which is a name for a null
  --   * pointer constant that has integral type (e.g., int or long) and is the same
  --   * size and alignment as a pointer.
  --   *
  --   * The __null extension is typically only used by system headers, which define
  --   * NULL as __null in C++ rather than using 0 (which is an integer that may not
  --   * match the size of a pointer).
  --    

  --* C++'s static_cast<> expression.
  --    

  --* C++'s dynamic_cast<> expression.
  --    

  --* C++'s reinterpret_cast<> expression.
  --    

  --* C++'s const_cast<> expression.
  --    

  --* Represents an explicit C++ type conversion that uses "functional"
  --   * notion (C++ [expr.type.conv]).
  --   *
  --   * Example:
  --   * \code
  --   *   x = int(0.5);
  --   * \endcode
  --    

  --* A C++ typeid expression (C++ [expr.typeid]).
  --    

  --* [C++ 2.13.5] C++ Boolean Literal.
  --    

  --* [C++0x 2.14.7] C++ Pointer Literal.
  --    

  --* Represents the "this" expression in C++
  --    

  --* [C++ 15] C++ Throw Expression.
  --   *
  --   * This handles 'throw' and 'throw' assignment-expression. When
  --   * assignment-expression isn't present, Op will be null.
  --    

  --* A new expression for memory allocation and constructor calls, e.g:
  --   * "new CXXNewExpr(foo)".
  --    

  --* A delete expression for memory deallocation and destructor calls,
  --   * e.g. "delete[] pArray".
  --    

  --* A unary expression. (noexcept, sizeof, or other traits)
  --    

  --* An Objective-C string literal i.e. @"foo".
  --    

  --* An Objective-C \@encode expression.
  --    

  --* An Objective-C \@selector expression.
  --    

  --* An Objective-C \@protocol expression.
  --    

  --* An Objective-C "bridged" cast expression, which casts between
  --   * Objective-C pointers and C pointers, transferring ownership in the process.
  --   *
  --   * \code
  --   *   NSString *str = (__bridge_transfer NSString *)CFCreateString();
  --   * \endcode
  --    

  --* Represents a C++0x pack expansion that produces a sequence of
  --   * expressions.
  --   *
  --   * A pack expansion expression contains a pattern (which itself is an
  --   * expression) followed by an ellipsis. For example:
  --   *
  --   * \code
  --   * template<typename F, typename ...Types>
  --   * void forward(F f, Types &&...args) {
  --   *  f(static_cast<Types&&>(args)...);
  --   * }
  --   * \endcode
  --    

  --* Represents an expression that computes the length of a parameter
  --   * pack.
  --   *
  --   * \code
  --   * template<typename ...Types>
  --   * struct count {
  --   *   static const unsigned value = sizeof...(Types);
  --   * };
  --   * \endcode
  --    

  -- Represents a C++ lambda expression that produces a local function
  --   * object.
  --   *
  --   * \code
  --   * void abssort(float *x, unsigned N) {
  --   *   std::sort(x, x + N,
  --   *             [](float a, float b) {
  --   *               return std::abs(a) < std::abs(b);
  --   *             });
  --   * }
  --   * \endcode
  --    

  --* Objective-c Boolean Literal.
  --    

  --* Represents the "self" expression in an Objective-C method.
  --    

  --* OpenMP 5.0 [2.1.5, Array Section].
  --    

  --* Represents an @available(...) check.
  --    

  --*
  --   * Fixed point literal
  --    

  --* OpenMP 5.0 [2.1.4, Array Shaping].
  --    

  --*
  --   * OpenMP 5.0 [2.1.6 Iterators]
  --    

  --* OpenCL's addrspace_cast<> expression.
  --    

  --*
  --   * Expression that references a C++20 concept.
  --    

  --*
  --   * Expression that references a C++20 concept.
  --    

  --*
  --   * Expression that references a C++20 parenthesized list aggregate
  --   * initializer.
  --    

  -- Statements  
  --*
  --   * A statement whose specific kind is not exposed via this
  --   * interface.
  --   *
  --   * Unexposed statements have the same operations as any other kind of
  --   * statement; one can extract their location information, spelling,
  --   * children, etc. However, the specific kind of the statement is not
  --   * reported.
  --    

  --* A labelled statement in a function.
  --   *
  --   * This cursor kind is used to describe the "start_over:" label statement in
  --   * the following example:
  --   *
  --   * \code
  --   *   start_over:
  --   *     ++counter;
  --   * \endcode
  --   *
  --    

  --* A group of statements like { stmt stmt }.
  --   *
  --   * This cursor kind is used to describe compound statements, e.g. function
  --   * bodies.
  --    

  --* A case statement.
  --    

  --* A default statement.
  --    

  --* An if statement
  --    

  --* A switch statement.
  --    

  --* A while statement.
  --    

  --* A do statement.
  --    

  --* A for statement.
  --    

  --* A goto statement.
  --    

  --* An indirect goto statement.
  --    

  --* A continue statement.
  --    

  --* A break statement.
  --    

  --* A return statement.
  --    

  --* A GCC inline assembly statement extension.
  --    

  --* Objective-C's overall \@try-\@catch-\@finally statement.
  --    

  --* Objective-C's \@catch statement.
  --    

  --* Objective-C's \@finally statement.
  --    

  --* Objective-C's \@throw statement.
  --    

  --* Objective-C's \@synchronized statement.
  --    

  --* Objective-C's autorelease pool statement.
  --    

  --* Objective-C's collection statement.
  --    

  --* C++'s catch statement.
  --    

  --* C++'s try statement.
  --    

  --* C++'s for (* : *) statement.
  --    

  --* Windows Structured Exception Handling's try statement.
  --    

  --* Windows Structured Exception Handling's except statement.
  --    

  --* Windows Structured Exception Handling's finally statement.
  --    

  --* A MS inline assembly statement extension.
  --    

  --* The null statement ";": C99 6.8.3p3.
  --   *
  --   * This cursor kind is used to describe the null statement.
  --    

  --* Adaptor class for mixing declarations with statements and
  --   * expressions.
  --    

  --* OpenMP parallel directive.
  --    

  --* OpenMP SIMD directive.
  --    

  --* OpenMP for directive.
  --    

  --* OpenMP sections directive.
  --    

  --* OpenMP section directive.
  --    

  --* OpenMP single directive.
  --    

  --* OpenMP parallel for directive.
  --    

  --* OpenMP parallel sections directive.
  --    

  --* OpenMP task directive.
  --    

  --* OpenMP master directive.
  --    

  --* OpenMP critical directive.
  --    

  --* OpenMP taskyield directive.
  --    

  --* OpenMP barrier directive.
  --    

  --* OpenMP taskwait directive.
  --    

  --* OpenMP flush directive.
  --    

  --* Windows Structured Exception Handling's leave statement.
  --    

  --* OpenMP ordered directive.
  --    

  --* OpenMP atomic directive.
  --    

  --* OpenMP for SIMD directive.
  --    

  --* OpenMP parallel for SIMD directive.
  --    

  --* OpenMP target directive.
  --    

  --* OpenMP teams directive.
  --    

  --* OpenMP taskgroup directive.
  --    

  --* OpenMP cancellation point directive.
  --    

  --* OpenMP cancel directive.
  --    

  --* OpenMP target data directive.
  --    

  --* OpenMP taskloop directive.
  --    

  --* OpenMP taskloop simd directive.
  --    

  --* OpenMP distribute directive.
  --    

  --* OpenMP target enter data directive.
  --    

  --* OpenMP target exit data directive.
  --    

  --* OpenMP target parallel directive.
  --    

  --* OpenMP target parallel for directive.
  --    

  --* OpenMP target update directive.
  --    

  --* OpenMP distribute parallel for directive.
  --    

  --* OpenMP distribute parallel for simd directive.
  --    

  --* OpenMP distribute simd directive.
  --    

  --* OpenMP target parallel for simd directive.
  --    

  --* OpenMP target simd directive.
  --    

  --* OpenMP teams distribute directive.
  --    

  --* OpenMP teams distribute simd directive.
  --    

  --* OpenMP teams distribute parallel for simd directive.
  --    

  --* OpenMP teams distribute parallel for directive.
  --    

  --* OpenMP target teams directive.
  --    

  --* OpenMP target teams distribute directive.
  --    

  --* OpenMP target teams distribute parallel for directive.
  --    

  --* OpenMP target teams distribute parallel for simd directive.
  --    

  --* OpenMP target teams distribute simd directive.
  --    

  --* C++2a std::bit_cast expression.
  --    

  --* OpenMP master taskloop directive.
  --    

  --* OpenMP parallel master taskloop directive.
  --    

  --* OpenMP master taskloop simd directive.
  --    

  --* OpenMP parallel master taskloop simd directive.
  --    

  --* OpenMP parallel master directive.
  --    

  --* OpenMP depobj directive.
  --    

  --* OpenMP scan directive.
  --    

  --* OpenMP tile directive.
  --    

  --* OpenMP canonical loop.
  --    

  --* OpenMP interop directive.
  --    

  --* OpenMP dispatch directive.
  --    

  --* OpenMP masked directive.
  --    

  --* OpenMP unroll directive.
  --    

  --* OpenMP metadirective directive.
  --    

  --* OpenMP loop directive.
  --    

  --* OpenMP teams loop directive.
  --    

  --* OpenMP target teams loop directive.
  --    

  --* OpenMP parallel loop directive.
  --    

  --* OpenMP target parallel loop directive.
  --    

  --* OpenMP parallel masked directive.
  --    

  --* OpenMP masked taskloop directive.
  --    

  --* OpenMP masked taskloop simd directive.
  --    

  --* OpenMP parallel masked taskloop directive.
  --    

  --* OpenMP parallel masked taskloop simd directive.
  --    

  --* OpenMP error directive.
  --    

  --*
  --   * Cursor that represents the translation unit itself.
  --   *
  --   * The translation unit cursor exists primarily to act as the root
  --   * cursor for traversing the contents of a translation unit.
  --    

  -- Attributes  
  --*
  --   * An attribute whose specific kind is not exposed via this
  --   * interface.
  --    

  -- Preprocessing  
  -- Extra Declarations  
  --*
  --   * A module import declaration.
  --    

  --*
  --   * A static_assert or _Static_assert node
  --    

  --*
  --   * a friend declaration.
  --    

  --*
  --   * a concept declaration.
  --    

  --*
  --   * A code completion overload candidate.
  --    

  --*
  -- * A cursor representing some element in the abstract syntax tree for
  -- * a translation unit.
  -- *
  -- * The cursor abstraction unifies the different kinds of entities in a
  -- * program--declaration, statements, expressions, references to declarations,
  -- * etc.--under a single "cursor" abstraction with a common set of operations.
  -- * Common operation for a cursor include: getting the physical location in
  -- * a source file where the cursor points, getting the name associated with a
  -- * cursor, and retrieving cursors for any child nodes of a particular cursor.
  -- *
  -- * Cursors can be produced in two specific ways.
  -- * clang_getTranslationUnitCursor() produces a cursor for a translation unit,
  -- * from which one can use clang_visitChildren() to explore the rest of the
  -- * translation unit. clang_getCursor() maps from a physical source location
  -- * to the entity that resides at that location, allowing one to map from the
  -- * source code into the AST.
  --  

   type anon_array1272 is array (0 .. 2) of System.Address;
   type Cursor_T is record
      kind : aliased Cursor_Kind_T;  -- install/include/clang-c/Index.h:2108
      xdata : aliased int;  -- install/include/clang-c/Index.h:2109
      data : anon_array1272;  -- install/include/clang-c/Index.h:2110
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:2111

  --*
  -- * \defgroup CINDEX_CURSOR_MANIP Cursor manipulations
  -- *
  -- * @{
  --  

  --*
  -- * Retrieve the NULL cursor, which represents no entity.
  --  

   function Get_Null_Cursor return Cursor_T  -- install/include/clang-c/Index.h:2122
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getNullCursor";

  --*
  -- * Retrieve the cursor that represents the given translation unit.
  -- *
  -- * The translation unit cursor can be used to start traversing the
  -- * various declarations within the given translation unit.
  --  

   function Get_Translation_Unit_Cursor (Arg_1 : Translation_Unit_T) return Cursor_T  -- install/include/clang-c/Index.h:2130
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getTranslationUnitCursor";

  --*
  -- * Determine whether two cursors are equivalent.
  --  

   function Equal_Cursors (Arg_1 : Cursor_T; Arg_2 : Cursor_T) return unsigned  -- install/include/clang-c/Index.h:2135
   with Import => True, 
        Convention => C, 
        External_Name => "clang_equalCursors";

  --*
  -- * Returns non-zero if \p cursor is null.
  --  

function Cursor_Is_Null
     (Cursor : Cursor_T)
      return Boolean;

  --*
  -- * Compute a hash value for the given cursor.
  --  

   function Hash_Cursor (Arg_1 : Cursor_T) return unsigned  -- install/include/clang-c/Index.h:2145
   with Import => True, 
        Convention => C, 
        External_Name => "clang_hashCursor";

  --*
  -- * Retrieve the kind of the given cursor.
  --  

   function Get_Cursor_Kind (Arg_1 : Cursor_T) return Cursor_Kind_T  -- install/include/clang-c/Index.h:2150
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorKind";

  --*
  -- * Determine whether the given cursor kind represents a declaration.
  --  

function Is_Declaration
     (Arg_1 : Cursor_Kind_T)
      return Boolean;

  --*
  -- * Determine whether the given declaration is invalid.
  -- *
  -- * A declaration is invalid if it could not be parsed successfully.
  -- *
  -- * \returns non-zero if the cursor represents a declaration and it is
  -- * invalid, otherwise NULL.
  --  

function Is_Invalid_Declaration
     (Arg_1 : Cursor_T)
      return Boolean;

  --*
  -- * Determine whether the given cursor kind represents a simple
  -- * reference.
  -- *
  -- * Note that other kinds of cursors (such as expressions) can also refer to
  -- * other cursors. Use clang_getCursorReferenced() to determine whether a
  -- * particular cursor refers to another entity.
  --  

function Is_Reference
     (Arg_1 : Cursor_Kind_T)
      return Boolean;

  --*
  -- * Determine whether the given cursor kind represents an expression.
  --  

function Is_Expression
     (Arg_1 : Cursor_Kind_T)
      return Boolean;

  --*
  -- * Determine whether the given cursor kind represents a statement.
  --  

function Is_Statement
     (Arg_1 : Cursor_Kind_T)
      return Boolean;

  --*
  -- * Determine whether the given cursor kind represents an attribute.
  --  

function Is_Attribute
     (Arg_1 : Cursor_Kind_T)
      return Boolean;

  --*
  -- * Determine whether the given cursor has any attributes.
  --  

   function Cursor_Has_Attrs (C : Cursor_T) return unsigned  -- install/include/clang-c/Index.h:2195
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_hasAttrs";

  --*
  -- * Determine whether the given cursor kind represents an invalid
  -- * cursor.
  --  

function Is_Invalid
     (Arg_1 : Cursor_Kind_T)
      return Boolean;

  --*
  -- * Determine whether the given cursor kind represents a translation
  -- * unit.
  --  

function Is_Translation_Unit
     (Arg_1 : Cursor_Kind_T)
      return Boolean;

  --**
  -- * Determine whether the given cursor represents a preprocessing
  -- * element, such as a preprocessor directive or macro instantiation.
  --  

function Is_Preprocessing
     (Arg_1 : Cursor_Kind_T)
      return Boolean;

  --**
  -- * Determine whether the given cursor represents a currently
  -- *  unexposed piece of the AST (e.g., CXCursor_UnexposedStmt).
  --  

function Is_Unexposed
     (Arg_1 : Cursor_Kind_T)
      return Boolean;

  --*
  -- * Describe the linkage of the entity referred to by a cursor.
  --  

   type Linkage_Kind_T is 
     (Linkage_Invalid,
      Linkage_No_Linkage,
      Linkage_Internal,
      Linkage_Unique_External,
      Linkage_External)
   with Convention => C;  -- install/include/clang-c/Index.h:2224

  --* This value indicates that no linkage information is available
  --   * for a provided CXCursor.  

  --*
  --   * This is the linkage for variables, parameters, and so on that
  --   *  have automatic storage.  This covers normal (non-extern) local variables.
  --    

  --* This is the linkage for static variables and static functions.  
  --* This is the linkage for entities with external linkage that live
  --   * in C++ anonymous namespaces. 

  --* This is the linkage for entities with true, external linkage.  
  --*
  -- * Determine the linkage of the entity referred to by a given cursor.
  --  

   function Get_Cursor_Linkage (Cursor : Cursor_T) return Linkage_Kind_T  -- install/include/clang-c/Index.h:2245
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorLinkage";

   type Visibility_Kind_T is 
     (Visibility_Invalid,
      Visibility_Hidden,
      Visibility_Protected,
      Visibility_Default)
   with Convention => C;  -- install/include/clang-c/Index.h:2247

  --* This value indicates that no visibility information is available
  --   * for a provided CXCursor.  

  --* Symbol not seen by the linker.  
  --* Symbol seen by the linker but resolves to a symbol inside this object.  
  --* Symbol seen by the linker and acts like a normal symbol.  
  --*
  -- * Describe the visibility of the entity referred to by a cursor.
  -- *
  -- * This returns the default visibility if not explicitly specified by
  -- * a visibility attribute. The default visibility may be changed by
  -- * commandline arguments.
  -- *
  -- * \param cursor The cursor to query.
  -- *
  -- * \returns The visibility of the cursor.
  --  

   function Get_Cursor_Visibility (Cursor : Cursor_T) return Visibility_Kind_T  -- install/include/clang-c/Index.h:2271
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorVisibility";

  --*
  -- * Determine the availability of the entity that this cursor refers to,
  -- * taking the current target platform into account.
  -- *
  -- * \param cursor The cursor to query.
  -- *
  -- * \returns The availability of the cursor.
  --  

   function Get_Cursor_Availability (Cursor : Cursor_T) return Availability_Kind_T  -- install/include/clang-c/Index.h:2282
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorAvailability";

  --*
  -- * Describes the availability of a given entity on a particular platform, e.g.,
  -- * a particular class might only be available on Mac OS 10.7 or newer.
  --  

  --*
  --   * A string that describes the platform for which this structure
  --   * provides availability information.
  --   *
  --   * Possible values are "ios" or "macos".
  --    

   type Platform_Availability_T is record
      Platform : aliased Clang.CX_String.String_T;  -- install/include/clang-c/Index.h:2295
      Introduced : aliased Version_T;  -- install/include/clang-c/Index.h:2299
      Deprecated : aliased Version_T;  -- install/include/clang-c/Index.h:2304
      Obsoleted : aliased Version_T;  -- install/include/clang-c/Index.h:2309
      Unavailable : aliased int;  -- install/include/clang-c/Index.h:2313
      Message : aliased Clang.CX_String.String_T;  -- install/include/clang-c/Index.h:2318
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:2288

  --*
  --   * The version number in which this entity was introduced.
  --    

  --*
  --   * The version number in which this entity was deprecated (but is
  --   * still available).
  --    

  --*
  --   * The version number in which this entity was obsoleted, and therefore
  --   * is no longer available.
  --    

  --*
  --   * Whether the entity is unconditionally unavailable on this platform.
  --    

  --*
  --   * An optional message to provide to a user of this API, e.g., to
  --   * suggest replacement APIs.
  --    

  --*
  -- * Determine the availability of the entity that this cursor refers to
  -- * on any platforms for which availability information is known.
  -- *
  -- * \param cursor The cursor to query.
  -- *
  -- * \param always_deprecated If non-NULL, will be set to indicate whether the
  -- * entity is deprecated on all platforms.
  -- *
  -- * \param deprecated_message If non-NULL, will be set to the message text
  -- * provided along with the unconditional deprecation of this entity. The client
  -- * is responsible for deallocating this string.
  -- *
  -- * \param always_unavailable If non-NULL, will be set to indicate whether the
  -- * entity is unavailable on all platforms.
  -- *
  -- * \param unavailable_message If non-NULL, will be set to the message text
  -- * provided along with the unconditional unavailability of this entity. The
  -- * client is responsible for deallocating this string.
  -- *
  -- * \param availability If non-NULL, an array of CXPlatformAvailability instances
  -- * that will be populated with platform availability information, up to either
  -- * the number of platforms for which availability information is available (as
  -- * returned by this function) or \c availability_size, whichever is smaller.
  -- *
  -- * \param availability_size The number of elements available in the
  -- * \c availability array.
  -- *
  -- * \returns The number of platforms (N) for which availability information is
  -- * available (which is unrelated to \c availability_size).
  -- *
  -- * Note that the client is responsible for calling
  -- * \c clang_disposeCXPlatformAvailability to free each of the
  -- * platform-availability structures returned. There are
  -- * \c min(N, availability_size) such structures.
  --  

   function Get_Cursor_Platform_Availability
     (Cursor : Cursor_T;
      Always_Deprecated : access int;
      Deprecated_Message : access Clang.CX_String.String_T;
      Always_Unavailable : access int;
      Unavailable_Message : access Clang.CX_String.String_T;
      Availability : access Platform_Availability_T;
      Availability_Size : int) return int  -- install/include/clang-c/Index.h:2357
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorPlatformAvailability";

  --*
  -- * Free the memory associated with a \c CXPlatformAvailability structure.
  --  

   procedure Dispose_CX_Platform_Availability (Availability : access Platform_Availability_T)  -- install/include/clang-c/Index.h:2366
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeCXPlatformAvailability";

  --*
  -- * If cursor refers to a variable declaration and it has initializer returns
  -- * cursor referring to the initializer otherwise return null cursor.
  --  

   function Cursor_Get_Var_Decl_Initializer (Cursor : Cursor_T) return Cursor_T  -- install/include/clang-c/Index.h:2372
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getVarDeclInitializer";

  --*
  -- * If cursor refers to a variable declaration that has global storage returns 1.
  -- * If cursor refers to a variable declaration that doesn't have global storage
  -- * returns 0. Otherwise returns -1.
  --  

   function Cursor_Has_Var_Decl_Global_Storage (Cursor : Cursor_T) return int  -- install/include/clang-c/Index.h:2379
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_hasVarDeclGlobalStorage";

  --*
  -- * If cursor refers to a variable declaration that has external storage
  -- * returns 1. If cursor refers to a variable declaration that doesn't have
  -- * external storage returns 0. Otherwise returns -1.
  --  

   function Cursor_Has_Var_Decl_External_Storage (Cursor : Cursor_T) return int  -- install/include/clang-c/Index.h:2386
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_hasVarDeclExternalStorage";

  --*
  -- * Describe the "language" of the entity referred to by a cursor.
  --  

   type Language_Kind_T is 
     (Language_Invalid,
      Language_C,
      Language_Obj_C,
      Language_C_Plus_Plus)
   with Convention => C;  -- install/include/clang-c/Index.h:2391

  --*
  -- * Determine the "language" of the entity referred to by a given cursor.
  --  

   function Get_Cursor_Language (Cursor : Cursor_T) return Language_Kind_T  -- install/include/clang-c/Index.h:2401
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorLanguage";

  --*
  -- * Describe the "thread-local storage (TLS) kind" of the declaration
  -- * referred to by a cursor.
  --  

   type TLS_Kind_T is 
     (TLS_None,
      TLS_Dynamic,
      TLS_Static)
   with Convention => C;  -- install/include/clang-c/Index.h:2407

  --*
  -- * Determine the "thread-local storage (TLS) kind" of the declaration
  -- * referred to by a cursor.
  --  

   function Get_Cursor_TLS_Kind (Cursor : Cursor_T) return TLS_Kind_T  -- install/include/clang-c/Index.h:2413
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorTLSKind";

  --*
  -- * Returns the translation unit that a cursor originated from.
  --  

   function Cursor_Get_Translation_Unit (Arg_1 : Cursor_T) return Translation_Unit_T  -- install/include/clang-c/Index.h:2418
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getTranslationUnit";

  --*
  -- * A fast container representing a set of CXCursors.
  --  

   type Cursor_Set_Impl_T is null record;   -- incomplete struct

   type Cursor_Set_T is access all Cursor_Set_Impl_T;  -- install/include/clang-c/Index.h:2423

  --*
  -- * Creates an empty CXCursorSet.
  --  

   function Create_CX_Cursor_Set return Cursor_Set_T  -- install/include/clang-c/Index.h:2428
   with Import => True, 
        Convention => C, 
        External_Name => "clang_createCXCursorSet";

  --*
  -- * Disposes a CXCursorSet and releases its associated memory.
  --  

   procedure Dispose_CX_Cursor_Set (Cset : Cursor_Set_T)  -- install/include/clang-c/Index.h:2433
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeCXCursorSet";

  --*
  -- * Queries a CXCursorSet to see if it contains a specific CXCursor.
  -- *
  -- * \returns non-zero if the set contains the specified cursor.
  --  

   function CX_Cursor_Set_Contains (Cset : Cursor_Set_T; Cursor : Cursor_T) return unsigned  -- install/include/clang-c/Index.h:2440
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CXCursorSet_contains";

  --*
  -- * Inserts a CXCursor into a CXCursorSet.
  -- *
  -- * \returns zero if the CXCursor was already in the set, and non-zero otherwise.
  --  

   function CX_Cursor_Set_Insert (Cset : Cursor_Set_T; Cursor : Cursor_T) return unsigned  -- install/include/clang-c/Index.h:2448
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CXCursorSet_insert";

  --*
  -- * Determine the semantic parent of the given cursor.
  -- *
  -- * The semantic parent of a cursor is the cursor that semantically contains
  -- * the given \p cursor. For many declarations, the lexical and semantic parents
  -- * are equivalent (the lexical parent is returned by
  -- * \c clang_getCursorLexicalParent()). They diverge when declarations or
  -- * definitions are provided out-of-line. For example:
  -- *
  -- * \code
  -- * class C {
  -- *  void f();
  -- * };
  -- *
  -- * void C::f() { }
  -- * \endcode
  -- *
  -- * In the out-of-line definition of \c C::f, the semantic parent is
  -- * the class \c C, of which this function is a member. The lexical parent is
  -- * the place where the declaration actually occurs in the source code; in this
  -- * case, the definition occurs in the translation unit. In general, the
  -- * lexical parent for a given entity can change without affecting the semantics
  -- * of the program, and the lexical parent of different declarations of the
  -- * same entity may be different. Changing the semantic parent of a declaration,
  -- * on the other hand, can have a major impact on semantics, and redeclarations
  -- * of a particular entity should all have the same semantic context.
  -- *
  -- * In the example above, both declarations of \c C::f have \c C as their
  -- * semantic context, while the lexical context of the first \c C::f is \c C
  -- * and the lexical context of the second \c C::f is the translation unit.
  -- *
  -- * For global declarations, the semantic parent is the translation unit.
  --  

   function Get_Cursor_Semantic_Parent (Cursor : Cursor_T) return Cursor_T  -- install/include/clang-c/Index.h:2484
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorSemanticParent";

  --*
  -- * Determine the lexical parent of the given cursor.
  -- *
  -- * The lexical parent of a cursor is the cursor in which the given \p cursor
  -- * was actually written. For many declarations, the lexical and semantic parents
  -- * are equivalent (the semantic parent is returned by
  -- * \c clang_getCursorSemanticParent()). They diverge when declarations or
  -- * definitions are provided out-of-line. For example:
  -- *
  -- * \code
  -- * class C {
  -- *  void f();
  -- * };
  -- *
  -- * void C::f() { }
  -- * \endcode
  -- *
  -- * In the out-of-line definition of \c C::f, the semantic parent is
  -- * the class \c C, of which this function is a member. The lexical parent is
  -- * the place where the declaration actually occurs in the source code; in this
  -- * case, the definition occurs in the translation unit. In general, the
  -- * lexical parent for a given entity can change without affecting the semantics
  -- * of the program, and the lexical parent of different declarations of the
  -- * same entity may be different. Changing the semantic parent of a declaration,
  -- * on the other hand, can have a major impact on semantics, and redeclarations
  -- * of a particular entity should all have the same semantic context.
  -- *
  -- * In the example above, both declarations of \c C::f have \c C as their
  -- * semantic context, while the lexical context of the first \c C::f is \c C
  -- * and the lexical context of the second \c C::f is the translation unit.
  -- *
  -- * For declarations written in the global scope, the lexical parent is
  -- * the translation unit.
  --  

   function Get_Cursor_Lexical_Parent (Cursor : Cursor_T) return Cursor_T  -- install/include/clang-c/Index.h:2520
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorLexicalParent";

  --*
  -- * Determine the set of methods that are overridden by the given
  -- * method.
  -- *
  -- * In both Objective-C and C++, a method (aka virtual member function,
  -- * in C++) can override a virtual method in a base class. For
  -- * Objective-C, a method is said to override any method in the class's
  -- * base class, its protocols, or its categories' protocols, that has the same
  -- * selector and is of the same kind (class or instance).
  -- * If no such method exists, the search continues to the class's superclass,
  -- * its protocols, and its categories, and so on. A method from an Objective-C
  -- * implementation is considered to override the same methods as its
  -- * corresponding method in the interface.
  -- *
  -- * For C++, a virtual member function overrides any virtual member
  -- * function with the same signature that occurs in its base
  -- * classes. With multiple inheritance, a virtual member function can
  -- * override several virtual member functions coming from different
  -- * base classes.
  -- *
  -- * In all cases, this function determines the immediate overridden
  -- * method, rather than all of the overridden methods. For example, if
  -- * a method is originally declared in a class A, then overridden in B
  -- * (which in inherits from A) and also in C (which inherited from B),
  -- * then the only overridden method returned from this function when
  -- * invoked on C's method will be B's method. The client may then
  -- * invoke this function again, given the previously-found overridden
  -- * methods, to map out the complete method-override set.
  -- *
  -- * \param cursor A cursor representing an Objective-C or C++
  -- * method. This routine will compute the set of methods that this
  -- * method overrides.
  -- *
  -- * \param overridden A pointer whose pointee will be replaced with a
  -- * pointer to an array of cursors, representing the set of overridden
  -- * methods. If there are no overridden methods, the pointee will be
  -- * set to NULL. The pointee must be freed via a call to
  -- * \c clang_disposeOverriddenCursors().
  -- *
  -- * \param num_overridden A pointer to the number of overridden
  -- * functions, will be set to the number of overridden functions in the
  -- * array pointed to by \p overridden.
  --  

   procedure Get_Overridden_Cursors
     (Cursor : Cursor_T;
      Overridden : System.Address;
      Num_Overridden : access unsigned)  -- install/include/clang-c/Index.h:2565
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getOverriddenCursors";

  --*
  -- * Free the set of overridden cursors returned by \c
  -- * clang_getOverriddenCursors().
  --  

   procedure Dispose_Overridden_Cursors (Overridden : access Cursor_T)  -- install/include/clang-c/Index.h:2573
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeOverriddenCursors";

  --*
  -- * Retrieve the file that is included by the given inclusion directive
  -- * cursor.
  --  

   function Get_Included_File (Cursor : Cursor_T) return Clang.CX_File.File_T  -- install/include/clang-c/Index.h:2579
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getIncludedFile";

  --*
  -- * @}
  --  

  --*
  -- * \defgroup CINDEX_CURSOR_SOURCE Mapping between cursors and source code
  -- *
  -- * Cursors represent a location within the Abstract Syntax Tree (AST). These
  -- * routines help map between cursors and the physical locations where the
  -- * described entities occur in the source code. The mapping is provided in
  -- * both directions, so one can map from source code to the AST and back.
  -- *
  -- * @{
  --  

  --*
  -- * Map a source location to the cursor that describes the entity at that
  -- * location in the source code.
  -- *
  -- * clang_getCursor() maps an arbitrary source location within a translation
  -- * unit down to the most specific cursor that describes the entity at that
  -- * location. For example, given an expression \c x + y, invoking
  -- * clang_getCursor() with a source location pointing to "x" will return the
  -- * cursor for "x"; similarly for "y". If the cursor points anywhere between
  -- * "x" or "y" (e.g., on the + or the whitespace around it), clang_getCursor()
  -- * will return a cursor referring to the "+" expression.
  -- *
  -- * \returns a cursor representing the entity at the given source location, or
  -- * a NULL cursor if no such entity can be found.
  --  

   function Get_Cursor (Arg_1 : Translation_Unit_T; Arg_2 : Clang.CX_Source_Location.Source_Location_T) return Cursor_T  -- install/include/clang-c/Index.h:2611
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursor";

  --*
  -- * Retrieve the physical location of the source constructor referenced
  -- * by the given cursor.
  -- *
  -- * The location of a declaration is typically the location of the name of that
  -- * declaration, where the name of that declaration would occur if it is
  -- * unnamed, or some keyword that introduces that particular declaration.
  -- * The location of a reference is where that reference occurs within the
  -- * source code.
  --  

   function Get_Cursor_Location (Arg_1 : Cursor_T) return Clang.CX_Source_Location.Source_Location_T  -- install/include/clang-c/Index.h:2623
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorLocation";

  --*
  -- * Retrieve the physical extent of the source construct referenced by
  -- * the given cursor.
  -- *
  -- * The extent of a cursor starts with the file/line/column pointing at the
  -- * first character within the source construct that the cursor refers to and
  -- * ends with the last character within that source construct. For a
  -- * declaration, the extent covers the declaration itself. For a reference,
  -- * the extent covers the location of the reference (e.g., where the referenced
  -- * entity was actually used).
  --  

   function Get_Cursor_Extent (Arg_1 : Cursor_T) return Clang.CX_Source_Location.Source_Range_T  -- install/include/clang-c/Index.h:2636
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorExtent";

  --*
  -- * @}
  --  

  --*
  -- * \defgroup CINDEX_TYPES Type information for CXCursors
  -- *
  -- * @{
  --  

  --*
  -- * Describes the kind of type
  --  

   subtype Type_Kind_T is unsigned;
   Type_Invalid : constant Type_Kind_T := 0;
   Type_Unexposed : constant Type_Kind_T := 1;
   Type_Void : constant Type_Kind_T := 2;
   Type_Bool : constant Type_Kind_T := 3;
   Type_Char_U : constant Type_Kind_T := 4;
   Type_U_Char : constant Type_Kind_T := 5;
   Type_Char_16 : constant Type_Kind_T := 6;
   Type_Char_32 : constant Type_Kind_T := 7;
   Type_U_Short : constant Type_Kind_T := 8;
   Type_U_Int : constant Type_Kind_T := 9;
   Type_U_Long : constant Type_Kind_T := 10;
   Type_U_Long_Long : constant Type_Kind_T := 11;
   Type_U_Int_128 : constant Type_Kind_T := 12;
   Type_Char_S : constant Type_Kind_T := 13;
   Type_S_Char : constant Type_Kind_T := 14;
   Type_W_Char : constant Type_Kind_T := 15;
   Type_Short : constant Type_Kind_T := 16;
   Type_Int : constant Type_Kind_T := 17;
   Type_Long : constant Type_Kind_T := 18;
   Type_Long_Long : constant Type_Kind_T := 19;
   Type_Int_128 : constant Type_Kind_T := 20;
   Type_Float : constant Type_Kind_T := 21;
   Type_Double : constant Type_Kind_T := 22;
   Type_Long_Double : constant Type_Kind_T := 23;
   Type_Null_Ptr : constant Type_Kind_T := 24;
   Type_Overload : constant Type_Kind_T := 25;
   Type_Dependent : constant Type_Kind_T := 26;
   Type_Obj_C_Id : constant Type_Kind_T := 27;
   Type_Obj_C_Class : constant Type_Kind_T := 28;
   Type_Obj_C_Sel : constant Type_Kind_T := 29;
   Type_Float_128 : constant Type_Kind_T := 30;
   Type_Half : constant Type_Kind_T := 31;
   Type_Float_16 : constant Type_Kind_T := 32;
   Type_Short_Accum : constant Type_Kind_T := 33;
   Type_Accum : constant Type_Kind_T := 34;
   Type_Long_Accum : constant Type_Kind_T := 35;
   Type_U_Short_Accum : constant Type_Kind_T := 36;
   Type_U_Accum : constant Type_Kind_T := 37;
   Type_U_Long_Accum : constant Type_Kind_T := 38;
   Type_B_Float_16 : constant Type_Kind_T := 39;
   Type_Ibm_128 : constant Type_Kind_T := 40;
   Type_First_Builtin : constant Type_Kind_T := 2;
   Type_Last_Builtin : constant Type_Kind_T := 40;
   Type_Complex : constant Type_Kind_T := 100;
   Type_Pointer : constant Type_Kind_T := 101;
   Type_Block_Pointer : constant Type_Kind_T := 102;
   Type_L_Value_Reference : constant Type_Kind_T := 103;
   Type_R_Value_Reference : constant Type_Kind_T := 104;
   Type_Record : constant Type_Kind_T := 105;
   Type_Enum : constant Type_Kind_T := 106;
   Type_Typedef : constant Type_Kind_T := 107;
   Type_Obj_C_Interface : constant Type_Kind_T := 108;
   Type_Obj_C_Object_Pointer : constant Type_Kind_T := 109;
   Type_Function_No_Proto : constant Type_Kind_T := 110;
   Type_Function_Proto : constant Type_Kind_T := 111;
   Type_Constant_Array : constant Type_Kind_T := 112;
   Type_Vector : constant Type_Kind_T := 113;
   Type_Incomplete_Array : constant Type_Kind_T := 114;
   Type_Variable_Array : constant Type_Kind_T := 115;
   Type_Dependent_Sized_Array : constant Type_Kind_T := 116;
   Type_Member_Pointer : constant Type_Kind_T := 117;
   Type_Auto : constant Type_Kind_T := 118;
   Type_Elaborated : constant Type_Kind_T := 119;
   Type_Pipe : constant Type_Kind_T := 120;
   Type_OCL_Image_1d_RO : constant Type_Kind_T := 121;
   Type_OCL_Image_1d_Array_RO : constant Type_Kind_T := 122;
   Type_OCL_Image_1d_Buffer_RO : constant Type_Kind_T := 123;
   Type_OCL_Image_2d_RO : constant Type_Kind_T := 124;
   Type_OCL_Image_2d_Array_RO : constant Type_Kind_T := 125;
   Type_OCL_Image_2d_Depth_RO : constant Type_Kind_T := 126;
   Type_OCL_Image_2d_Array_Depth_RO : constant Type_Kind_T := 127;
   Type_OCL_Image_2d_MSAARO : constant Type_Kind_T := 128;
   Type_OCL_Image_2d_Array_MSAARO : constant Type_Kind_T := 129;
   Type_OCL_Image_2d_MSAA_Depth_RO : constant Type_Kind_T := 130;
   Type_OCL_Image_2d_Array_MSAA_Depth_RO : constant Type_Kind_T := 131;
   Type_OCL_Image_3d_RO : constant Type_Kind_T := 132;
   Type_OCL_Image_1d_WO : constant Type_Kind_T := 133;
   Type_OCL_Image_1d_Array_WO : constant Type_Kind_T := 134;
   Type_OCL_Image_1d_Buffer_WO : constant Type_Kind_T := 135;
   Type_OCL_Image_2d_WO : constant Type_Kind_T := 136;
   Type_OCL_Image_2d_Array_WO : constant Type_Kind_T := 137;
   Type_OCL_Image_2d_Depth_WO : constant Type_Kind_T := 138;
   Type_OCL_Image_2d_Array_Depth_WO : constant Type_Kind_T := 139;
   Type_OCL_Image_2d_MSAAWO : constant Type_Kind_T := 140;
   Type_OCL_Image_2d_Array_MSAAWO : constant Type_Kind_T := 141;
   Type_OCL_Image_2d_MSAA_Depth_WO : constant Type_Kind_T := 142;
   Type_OCL_Image_2d_Array_MSAA_Depth_WO : constant Type_Kind_T := 143;
   Type_OCL_Image_3d_WO : constant Type_Kind_T := 144;
   Type_OCL_Image_1d_RW : constant Type_Kind_T := 145;
   Type_OCL_Image_1d_Array_RW : constant Type_Kind_T := 146;
   Type_OCL_Image_1d_Buffer_RW : constant Type_Kind_T := 147;
   Type_OCL_Image_2d_RW : constant Type_Kind_T := 148;
   Type_OCL_Image_2d_Array_RW : constant Type_Kind_T := 149;
   Type_OCL_Image_2d_Depth_RW : constant Type_Kind_T := 150;
   Type_OCL_Image_2d_Array_Depth_RW : constant Type_Kind_T := 151;
   Type_OCL_Image_2d_MSAARW : constant Type_Kind_T := 152;
   Type_OCL_Image_2d_Array_MSAARW : constant Type_Kind_T := 153;
   Type_OCL_Image_2d_MSAA_Depth_RW : constant Type_Kind_T := 154;
   Type_OCL_Image_2d_Array_MSAA_Depth_RW : constant Type_Kind_T := 155;
   Type_OCL_Image_3d_RW : constant Type_Kind_T := 156;
   Type_OCL_Sampler : constant Type_Kind_T := 157;
   Type_OCL_Event : constant Type_Kind_T := 158;
   Type_OCL_Queue : constant Type_Kind_T := 159;
   Type_OCL_Reserve_ID : constant Type_Kind_T := 160;
   Type_Obj_C_Object : constant Type_Kind_T := 161;
   Type_Obj_C_Type_Param : constant Type_Kind_T := 162;
   Type_Attributed : constant Type_Kind_T := 163;
   Type_OCL_Intel_Subgroup_AVC_Mce_Payload : constant Type_Kind_T := 164;
   Type_OCL_Intel_Subgroup_AVC_Ime_Payload : constant Type_Kind_T := 165;
   Type_OCL_Intel_Subgroup_AVC_Ref_Payload : constant Type_Kind_T := 166;
   Type_OCL_Intel_Subgroup_AVC_Sic_Payload : constant Type_Kind_T := 167;
   Type_OCL_Intel_Subgroup_AVC_Mce_Result : constant Type_Kind_T := 168;
   Type_OCL_Intel_Subgroup_AVC_Ime_Result : constant Type_Kind_T := 169;
   Type_OCL_Intel_Subgroup_AVC_Ref_Result : constant Type_Kind_T := 170;
   Type_OCL_Intel_Subgroup_AVC_Sic_Result : constant Type_Kind_T := 171;
   Type_OCL_Intel_Subgroup_AVC_Ime_Result_Single_Ref_Streamout : constant Type_Kind_T := 172;
   Type_OCL_Intel_Subgroup_AVC_Ime_Result_Dual_Ref_Streamout : constant Type_Kind_T := 173;
   Type_OCL_Intel_Subgroup_AVC_Ime_Single_Ref_Streamin : constant Type_Kind_T := 174;
   Type_OCL_Intel_Subgroup_AVC_Ime_Dual_Ref_Streamin : constant Type_Kind_T := 175;
   Type_Ext_Vector : constant Type_Kind_T := 176;
   Type_Atomic : constant Type_Kind_T := 177;
   Type_BTF_Tag_Attributed : constant Type_Kind_T := 178;  -- install/include/clang-c/Index.h:2651

  --*
  --   * Represents an invalid type (e.g., where no type is available).
  --    

  --*
  --   * A type whose specific kind is not exposed via this
  --   * interface.
  --    

  -- Builtin types  
  --*
  --   * Represents a type that was referred to using an elaborated type keyword.
  --   *
  --   * E.g., struct S, or via a qualified name, e.g., N::M::type, or both.
  --    

  -- OpenCL PipeType.  
  -- OpenCL builtin types.  
  --*
  -- * Describes the calling convention of a function type
  --  

   subtype Calling_Conv_T is unsigned;
   Calling_Conv_Default : constant Calling_Conv_T := 0;
   Calling_Conv_C : constant Calling_Conv_T := 1;
   Calling_Conv_X86_Std_Call : constant Calling_Conv_T := 2;
   Calling_Conv_X86_Fast_Call : constant Calling_Conv_T := 3;
   Calling_Conv_X86_This_Call : constant Calling_Conv_T := 4;
   Calling_Conv_X86_Pascal : constant Calling_Conv_T := 5;
   Calling_Conv_AAPCS : constant Calling_Conv_T := 6;
   Calling_Conv_AAPCS_VFP : constant Calling_Conv_T := 7;
   Calling_Conv_X86_Reg_Call : constant Calling_Conv_T := 8;
   Calling_Conv_Intel_Ocl_Bicc : constant Calling_Conv_T := 9;
   Calling_Conv_Win_64 : constant Calling_Conv_T := 10;
   Calling_Conv_X86_64_Win_64 : constant Calling_Conv_T := 10;
   Calling_Conv_X86_64_Sys_V : constant Calling_Conv_T := 11;
   Calling_Conv_X86_Vector_Call : constant Calling_Conv_T := 12;
   Calling_Conv_Swift : constant Calling_Conv_T := 13;
   Calling_Conv_Preserve_Most : constant Calling_Conv_T := 14;
   Calling_Conv_Preserve_All : constant Calling_Conv_T := 15;
   Calling_Conv_A_Arch_64_Vector_Call : constant Calling_Conv_T := 16;
   Calling_Conv_Swift_Async : constant Calling_Conv_T := 17;
   Calling_Conv_A_Arch_64SVEPCS : constant Calling_Conv_T := 18;
   Calling_Conv_Invalid : constant Calling_Conv_T := 100;
   Calling_Conv_Unexposed : constant Calling_Conv_T := 200;  -- install/include/clang-c/Index.h:2804

  -- Alias for compatibility with older versions of API.  
  --*
  -- * The type of an element in the abstract syntax tree.
  -- *
  --  

   type anon_array1331 is array (0 .. 1) of System.Address;
   type Type_T is record
      kind : aliased Type_Kind_T;  -- install/include/clang-c/Index.h:2836
      data : anon_array1331;  -- install/include/clang-c/Index.h:2837
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:2838

  --*
  -- * Retrieve the type of a CXCursor (if any).
  --  

   function Get_Cursor_Type (C : Cursor_T) return Type_T  -- install/include/clang-c/Index.h:2843
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorType";

  --*
  -- * Pretty-print the underlying type using the rules of the
  -- * language of the translation unit from which it came.
  -- *
  -- * If the type is invalid, an empty string is returned.
  --  

function Get_Type_Spelling
     (CT : Type_T)
      return String;

  --*
  -- * Retrieve the underlying type of a typedef declaration.
  -- *
  -- * If the cursor does not reference a typedef declaration, an invalid type is
  -- * returned.
  --  

   function Get_Typedef_Decl_Underlying_Type (C : Cursor_T) return Type_T  -- install/include/clang-c/Index.h:2859
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getTypedefDeclUnderlyingType";

  --*
  -- * Retrieve the integer type of an enum declaration.
  -- *
  -- * If the cursor does not reference an enum declaration, an invalid type is
  -- * returned.
  --  

   function Get_Enum_Decl_Integer_Type (C : Cursor_T) return Type_T  -- install/include/clang-c/Index.h:2867
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getEnumDeclIntegerType";

  --*
  -- * Retrieve the integer value of an enum constant declaration as a signed
  -- *  long long.
  -- *
  -- * If the cursor does not reference an enum constant declaration, LLONG_MIN is
  -- * returned. Since this is also potentially a valid constant value, the kind of
  -- * the cursor must be verified before calling this function.
  --  

   function Get_Enum_Constant_Decl_Value (C : Cursor_T) return Long_Long_Integer  -- install/include/clang-c/Index.h:2877
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getEnumConstantDeclValue";

  --*
  -- * Retrieve the integer value of an enum constant declaration as an unsigned
  -- *  long long.
  -- *
  -- * If the cursor does not reference an enum constant declaration, ULLONG_MAX is
  -- * returned. Since this is also potentially a valid constant value, the kind of
  -- * the cursor must be verified before calling this function.
  --  

   function Get_Enum_Constant_Decl_Unsigned_Value (C : Cursor_T) return Extensions.unsigned_long_long  -- install/include/clang-c/Index.h:2888
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getEnumConstantDeclUnsignedValue";

  --*
  -- * Retrieve the bit width of a bit field declaration as an integer.
  -- *
  -- * If a cursor that is not a bit field declaration is passed in, -1 is returned.
  --  

   function Get_Field_Decl_Bit_Width (C : Cursor_T) return int  -- install/include/clang-c/Index.h:2895
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getFieldDeclBitWidth";

  --*
  -- * Retrieve the number of non-variadic arguments associated with a given
  -- * cursor.
  -- *
  -- * The number of arguments can be determined for calls as well as for
  -- * declarations of functions or methods. For other cursors -1 is returned.
  --  

   function Cursor_Get_Num_Arguments (C : Cursor_T) return int  -- install/include/clang-c/Index.h:2904
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getNumArguments";

  --*
  -- * Retrieve the argument cursor of a function or method.
  -- *
  -- * The argument cursor can be determined for calls as well as for declarations
  -- * of functions or methods. For other cursors and for invalid indices, an
  -- * invalid cursor is returned.
  --  

   function Cursor_Get_Argument (C : Cursor_T; I : unsigned) return Cursor_T  -- install/include/clang-c/Index.h:2913
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getArgument";

  --*
  -- * Describes the kind of a template argument.
  -- *
  -- * See the definition of llvm::clang::TemplateArgument::ArgKind for full
  -- * element descriptions.
  --  

   type Template_Argument_Kind_T is 
     (Template_Argument_Kind_Null,
      Template_Argument_Kind_Type,
      Template_Argument_Kind_Declaration,
      Template_Argument_Kind_Null_Ptr,
      Template_Argument_Kind_Integral,
      Template_Argument_Kind_Template,
      Template_Argument_Kind_Template_Expansion,
      Template_Argument_Kind_Expression,
      Template_Argument_Kind_Pack,
      Template_Argument_Kind_Invalid)
   with Convention => C;  -- install/include/clang-c/Index.h:2921

  -- Indicates an error case, preventing the kind from being deduced.  
  --*
  -- * Returns the number of template args of a function, struct, or class decl
  -- * representing a template specialization.
  -- *
  -- * If the argument cursor cannot be converted into a template function
  -- * declaration, -1 is returned.
  -- *
  -- * For example, for the following declaration and specialization:
  -- *   template <typename T, int kInt, bool kBool>
  -- *   void foo() { ... }
  -- *
  -- *   template <>
  -- *   void foo<float, -7, true>();
  -- *
  -- * The value 3 would be returned from this call.
  --  

   function Cursor_Get_Num_Template_Arguments (C : Cursor_T) return int  -- install/include/clang-c/Index.h:2951
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getNumTemplateArguments";

  --*
  -- * Retrieve the kind of the I'th template argument of the CXCursor C.
  -- *
  -- * If the argument CXCursor does not represent a FunctionDecl, StructDecl, or
  -- * ClassTemplatePartialSpecialization, an invalid template argument kind is
  -- * returned.
  -- *
  -- * For example, for the following declaration and specialization:
  -- *   template <typename T, int kInt, bool kBool>
  -- *   void foo() { ... }
  -- *
  -- *   template <>
  -- *   void foo<float, -7, true>();
  -- *
  -- * For I = 0, 1, and 2, Type, Integral, and Integral will be returned,
  -- * respectively.
  --  

   function Cursor_Get_Template_Argument_Kind (C : Cursor_T; I : unsigned) return Template_Argument_Kind_T  -- install/include/clang-c/Index.h:2971
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getTemplateArgumentKind";

  --*
  -- * Retrieve a CXType representing the type of a TemplateArgument of a
  -- *  function decl representing a template specialization.
  -- *
  -- * If the argument CXCursor does not represent a FunctionDecl, StructDecl,
  -- * ClassDecl or ClassTemplatePartialSpecialization whose I'th template argument
  -- * has a kind of CXTemplateArgKind_Integral, an invalid type is returned.
  -- *
  -- * For example, for the following declaration and specialization:
  -- *   template <typename T, int kInt, bool kBool>
  -- *   void foo() { ... }
  -- *
  -- *   template <>
  -- *   void foo<float, -7, true>();
  -- *
  -- * If called with I = 0, "float", will be returned.
  -- * Invalid types will be returned for I == 1 or 2.
  --  

   function Cursor_Get_Template_Argument_Type (C : Cursor_T; I : unsigned) return Type_T  -- install/include/clang-c/Index.h:2991
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getTemplateArgumentType";

  --*
  -- * Retrieve the value of an Integral TemplateArgument (of a function
  -- *  decl representing a template specialization) as a signed long long.
  -- *
  -- * It is undefined to call this function on a CXCursor that does not represent a
  -- * FunctionDecl, StructDecl, ClassDecl or ClassTemplatePartialSpecialization
  -- * whose I'th template argument is not an integral value.
  -- *
  -- * For example, for the following declaration and specialization:
  -- *   template <typename T, int kInt, bool kBool>
  -- *   void foo() { ... }
  -- *
  -- *   template <>
  -- *   void foo<float, -7, true>();
  -- *
  -- * If called with I = 1 or 2, -7 or true will be returned, respectively.
  -- * For I == 0, this function's behavior is undefined.
  --  

   function Cursor_Get_Template_Argument_Value (C : Cursor_T; I : unsigned) return Long_Long_Integer  -- install/include/clang-c/Index.h:3012
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getTemplateArgumentValue";

  --*
  -- * Retrieve the value of an Integral TemplateArgument (of a function
  -- *  decl representing a template specialization) as an unsigned long long.
  -- *
  -- * It is undefined to call this function on a CXCursor that does not represent a
  -- * FunctionDecl, StructDecl, ClassDecl or ClassTemplatePartialSpecialization or
  -- * whose I'th template argument is not an integral value.
  -- *
  -- * For example, for the following declaration and specialization:
  -- *   template <typename T, int kInt, bool kBool>
  -- *   void foo() { ... }
  -- *
  -- *   template <>
  -- *   void foo<float, 2147483649, true>();
  -- *
  -- * If called with I = 1 or 2, 2147483649 or true will be returned, respectively.
  -- * For I == 0, this function's behavior is undefined.
  --  

   function Cursor_Get_Template_Argument_Unsigned_Value (C : Cursor_T; I : unsigned) return Extensions.unsigned_long_long  -- install/include/clang-c/Index.h:3034
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getTemplateArgumentUnsignedValue";

  --*
  -- * Determine whether two CXTypes represent the same type.
  -- *
  -- * \returns non-zero if the CXTypes represent the same type and
  -- *          zero otherwise.
  --  

   function Equal_Types (A : Type_T; B : Type_T) return unsigned  -- install/include/clang-c/Index.h:3042
   with Import => True, 
        Convention => C, 
        External_Name => "clang_equalTypes";

  --*
  -- * Return the canonical type for a CXType.
  -- *
  -- * Clang's type system explicitly models typedefs and all the ways
  -- * a specific type can be represented.  The canonical type is the underlying
  -- * type with all the "sugar" removed.  For example, if 'T' is a typedef
  -- * for 'int', the canonical type for 'T' would be 'int'.
  --  

   function Get_Canonical_Type (T : Type_T) return Type_T  -- install/include/clang-c/Index.h:3052
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCanonicalType";

  --*
  -- * Determine whether a CXType has the "const" qualifier set,
  -- * without looking through typedefs that may have added "const" at a
  -- * different level.
  --  

function Is_Const_Qualified_Type
     (T : Type_T)
      return Boolean;

  --*
  -- * Determine whether a  CXCursor that is a macro, is
  -- * function like.
  --  

function Cursor_Is_Macro_Function_Like
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine whether a  CXCursor that is a macro, is a
  -- * builtin one.
  --  

function Cursor_Is_Macro_Builtin
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine whether a  CXCursor that is a function declaration, is an
  -- * inline declaration.
  --  

function Cursor_Is_Function_Inlined
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine whether a CXType has the "volatile" qualifier set,
  -- * without looking through typedefs that may have added "volatile" at
  -- * a different level.
  --  

function Is_Volatile_Qualified_Type
     (T : Type_T)
      return Boolean;

  --*
  -- * Determine whether a CXType has the "restrict" qualifier set,
  -- * without looking through typedefs that may have added "restrict" at a
  -- * different level.
  --  

function Is_Restrict_Qualified_Type
     (T : Type_T)
      return Boolean;

  --*
  -- * Returns the address space of the given type.
  --  

   function Get_Address_Space (T : Type_T) return unsigned  -- install/include/clang-c/Index.h:3096
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getAddressSpace";

  --*
  -- * Returns the typedef name of the given type.
  --  

function Get_Typedef_Name
     (CT : Type_T)
      return String;

  --*
  -- * For pointer types, returns the type of the pointee.
  --  

   function Get_Pointee_Type (T : Type_T) return Type_T  -- install/include/clang-c/Index.h:3106
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getPointeeType";

  --*
  -- * Retrieve the unqualified variant of the given type, removing as
  -- * little sugar as possible.
  -- *
  -- * For example, given the following series of typedefs:
  -- *
  -- * \code
  -- * typedef int Integer;
  -- * typedef const Integer CInteger;
  -- * typedef CInteger DifferenceType;
  -- * \endcode
  -- *
  -- * Executing \c clang_getUnqualifiedType() on a \c CXType that
  -- * represents \c DifferenceType, will desugar to a type representing
  -- * \c Integer, that has no qualifiers.
  -- *
  -- * And, executing \c clang_getUnqualifiedType() on the type of the
  -- * first argument of the following function declaration:
  -- *
  -- * \code
  -- * void foo(const int);
  -- * \endcode
  -- *
  -- * Will return a type representing \c int, removing the \c const
  -- * qualifier.
  -- *
  -- * Sugar over array types is not desugared.
  -- *
  -- * A type can be checked for qualifiers with \c
  -- * clang_isConstQualifiedType(), \c clang_isVolatileQualifiedType()
  -- * and \c clang_isRestrictQualifiedType().
  -- *
  -- * A type that resulted from a call to \c clang_getUnqualifiedType
  -- * will return \c false for all of the above calls.
  --  

   function Get_Unqualified_Type (CT : Type_T) return Type_T  -- install/include/clang-c/Index.h:3143
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getUnqualifiedType";

  --*
  -- * For reference types (e.g., "const int&"), returns the type that the
  -- * reference refers to (e.g "const int").
  -- *
  -- * Otherwise, returns the type itself.
  -- *
  -- * A type that has kind \c CXType_LValueReference or
  -- * \c CXType_RValueReference is a reference type.
  --  

   function Get_Non_Reference_Type (CT : Type_T) return Type_T  -- install/include/clang-c/Index.h:3154
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getNonReferenceType";

  --*
  -- * Return the cursor for the declaration of the given type.
  --  

   function Get_Type_Declaration (T : Type_T) return Cursor_T  -- install/include/clang-c/Index.h:3159
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getTypeDeclaration";

  --*
  -- * Returns the Objective-C type encoding for the specified declaration.
  --  

function Get_Decl_Obj_C_Type_Encoding
     (C : Cursor_T)
      return String;

  --*
  -- * Returns the Objective-C type encoding for the specified CXType.
  --  

function Type_Get_Obj_C_Encoding
     (C_Type : Type_T)
      return String;

  --*
  -- * Retrieve the spelling of a given CXTypeKind.
  --  

function Get_Type_Kind_Spelling
     (K : Type_Kind_T)
      return String;

  --*
  -- * Retrieve the calling convention associated with a function type.
  -- *
  -- * If a non-function type is passed in, CXCallingConv_Invalid is returned.
  --  

   function Get_Function_Type_Calling_Conv (T : Type_T) return Calling_Conv_T  -- install/include/clang-c/Index.h:3181
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getFunctionTypeCallingConv";

  --*
  -- * Retrieve the return type associated with a function type.
  -- *
  -- * If a non-function type is passed in, an invalid type is returned.
  --  

   function Get_Result_Type (T : Type_T) return Type_T  -- install/include/clang-c/Index.h:3188
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getResultType";

  --*
  -- * Retrieve the exception specification type associated with a function type.
  -- * This is a value of type CXCursor_ExceptionSpecificationKind.
  -- *
  -- * If a non-function type is passed in, an error code of -1 is returned.
  --  

   function Get_Exception_Specification_Type (T : Type_T) return int  -- install/include/clang-c/Index.h:3196
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getExceptionSpecificationType";

  --*
  -- * Retrieve the number of non-variadic parameters associated with a
  -- * function type.
  -- *
  -- * If a non-function type is passed in, -1 is returned.
  --  

   function Get_Num_Arg_Types (T : Type_T) return int  -- install/include/clang-c/Index.h:3204
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getNumArgTypes";

  --*
  -- * Retrieve the type of a parameter of a function type.
  -- *
  -- * If a non-function type is passed in or the function does not have enough
  -- * parameters, an invalid type is returned.
  --  

   function Get_Arg_Type (T : Type_T; I : unsigned) return Type_T  -- install/include/clang-c/Index.h:3212
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getArgType";

  --*
  -- * Retrieves the base type of the ObjCObjectType.
  -- *
  -- * If the type is not an ObjC object, an invalid type is returned.
  --  

   function Type_Get_Obj_C_Object_Base_Type (T : Type_T) return Type_T  -- install/include/clang-c/Index.h:3219
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getObjCObjectBaseType";

  --*
  -- * Retrieve the number of protocol references associated with an ObjC object/id.
  -- *
  -- * If the type is not an ObjC object, 0 is returned.
  --  

   function Type_Get_Num_Obj_C_Protocol_Refs (T : Type_T) return unsigned  -- install/include/clang-c/Index.h:3226
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getNumObjCProtocolRefs";

  --*
  -- * Retrieve the decl for a protocol reference for an ObjC object/id.
  -- *
  -- * If the type is not an ObjC object or there are not enough protocol
  -- * references, an invalid cursor is returned.
  --  

   function Type_Get_Obj_C_Protocol_Decl (T : Type_T; I : unsigned) return Cursor_T  -- install/include/clang-c/Index.h:3234
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getObjCProtocolDecl";

  --*
  -- * Retrieve the number of type arguments associated with an ObjC object.
  -- *
  -- * If the type is not an ObjC object, 0 is returned.
  --  

   function Type_Get_Num_Obj_C_Type_Args (T : Type_T) return unsigned  -- install/include/clang-c/Index.h:3241
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getNumObjCTypeArgs";

  --*
  -- * Retrieve a type argument associated with an ObjC object.
  -- *
  -- * If the type is not an ObjC or the index is not valid,
  -- * an invalid type is returned.
  --  

   function Type_Get_Obj_C_Type_Arg (T : Type_T; I : unsigned) return Type_T  -- install/include/clang-c/Index.h:3249
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getObjCTypeArg";

  --*
  -- * Return 1 if the CXType is a variadic function type, and 0 otherwise.
  --  

function Is_Function_Type_Variadic
     (T : Type_T)
      return Boolean;

  --*
  -- * Retrieve the return type associated with a given cursor.
  -- *
  -- * This only returns a valid type if the cursor refers to a function or method.
  --  

   function Get_Cursor_Result_Type (C : Cursor_T) return Type_T  -- install/include/clang-c/Index.h:3261
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorResultType";

  --*
  -- * Retrieve the exception specification type associated with a given cursor.
  -- * This is a value of type CXCursor_ExceptionSpecificationKind.
  -- *
  -- * This only returns a valid result if the cursor refers to a function or
  -- * method.
  --  

   function Get_Cursor_Exception_Specification_Type (C : Cursor_T) return int  -- install/include/clang-c/Index.h:3270
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorExceptionSpecificationType";

  --*
  -- * Return 1 if the CXType is a POD (plain old data) type, and 0
  -- *  otherwise.
  --  

function Is_POD_Type
     (T : Type_T)
      return Boolean;

  --*
  -- * Return the element type of an array, complex, or vector type.
  -- *
  -- * If a type is passed in that is not an array, complex, or vector type,
  -- * an invalid type is returned.
  --  

   function Get_Element_Type (T : Type_T) return Type_T  -- install/include/clang-c/Index.h:3284
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getElementType";

  --*
  -- * Return the number of elements of an array or vector type.
  -- *
  -- * If a type is passed in that is not an array or vector type,
  -- * -1 is returned.
  --  

   function Get_Num_Elements (T : Type_T) return Long_Long_Integer  -- install/include/clang-c/Index.h:3292
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getNumElements";

  --*
  -- * Return the element type of an array type.
  -- *
  -- * If a non-array type is passed in, an invalid type is returned.
  --  

   function Get_Array_Element_Type (T : Type_T) return Type_T  -- install/include/clang-c/Index.h:3299
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getArrayElementType";

  --*
  -- * Return the array size of a constant array.
  -- *
  -- * If a non-array type is passed in, -1 is returned.
  --  

   function Get_Array_Size (T : Type_T) return Long_Long_Integer  -- install/include/clang-c/Index.h:3306
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getArraySize";

  --*
  -- * Retrieve the type named by the qualified-id.
  -- *
  -- * If a non-elaborated type is passed in, an invalid type is returned.
  --  

   function Type_Get_Named_Type (T : Type_T) return Type_T  -- install/include/clang-c/Index.h:3313
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getNamedType";

  --*
  -- * Determine if a typedef is 'transparent' tag.
  -- *
  -- * A typedef is considered 'transparent' if it shares a name and spelling
  -- * location with its underlying tag type, as is the case with the NS_ENUM macro.
  -- *
  -- * \returns non-zero if transparent and zero otherwise.
  --  

function Type_Is_Transparent_Tag_Typedef
     (T : Type_T)
      return Boolean;

   type Type_Nullability_Kind_T is 
     (Type_Nullability_Non_Null,
      Type_Nullability_Nullable,
      Type_Nullability_Unspecified,
      Type_Nullability_Invalid,
      Type_Nullability_Nullable_Result)
   with Convention => C;  -- install/include/clang-c/Index.h:3325

  --*
  --   * Values of this type can never be null.
  --    

  --*
  --   * Values of this type can be null.
  --    

  --*
  --   * Whether values of this type can be null is (explicitly)
  --   * unspecified. This captures a (fairly rare) case where we
  --   * can't conclude anything about the nullability of the type even
  --   * though it has been considered.
  --    

  --*
  --   * Nullability is not applicable to this type.
  --    

  --*
  --   * Generally behaves like Nullable, except when used in a block parameter that
  --   * was imported into a swift async method. There, swift will assume that the
  --   * parameter can get null even if no error occurred. _Nullable parameters are
  --   * assumed to only get null on error.
  --    

  --*
  -- * Retrieve the nullability kind of a pointer type.
  --  

   function Type_Get_Nullability (T : Type_T) return Type_Nullability_Kind_T  -- install/include/clang-c/Index.h:3358
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getNullability";

  --*
  -- * List the possible error codes for \c clang_Type_getSizeOf,
  -- *   \c clang_Type_getAlignOf, \c clang_Type_getOffsetOf and
  -- *   \c clang_Cursor_getOffsetOf.
  -- *
  -- * A value of this enumeration type can be returned if the target type is not
  -- * a valid argument to sizeof, alignof or offsetof.
  --  

   subtype Type_Layout_Error_T is int;
   Type_Layout_Error_Invalid : constant Type_Layout_Error_T := -1;
   Type_Layout_Error_Incomplete : constant Type_Layout_Error_T := -2;
   Type_Layout_Error_Dependent : constant Type_Layout_Error_T := -3;
   Type_Layout_Error_Not_Constant_Size : constant Type_Layout_Error_T := -4;
   Type_Layout_Error_Invalid_Field_Name : constant Type_Layout_Error_T := -5;
   Type_Layout_Error_Undeduced : constant Type_Layout_Error_T := -6;  -- install/include/clang-c/Index.h:3368

  --*
  --   * Type is of kind CXType_Invalid.
  --    

  --*
  --   * The type is an incomplete Type.
  --    

  --*
  --   * The type is a dependent Type.
  --    

  --*
  --   * The type is not a constant size type.
  --    

  --*
  --   * The Field name is not valid for this record.
  --    

  --*
  --   * The type is undeduced.
  --    

  --*
  -- * Return the alignment of a type in bytes as per C++[expr.alignof]
  -- *   standard.
  -- *
  -- * If the type declaration is invalid, CXTypeLayoutError_Invalid is returned.
  -- * If the type declaration is an incomplete type, CXTypeLayoutError_Incomplete
  -- *   is returned.
  -- * If the type declaration is a dependent type, CXTypeLayoutError_Dependent is
  -- *   returned.
  -- * If the type declaration is not a constant size type,
  -- *   CXTypeLayoutError_NotConstantSize is returned.
  --  

   function Type_Get_Align_Of (T : Type_T) return Long_Long_Integer  -- install/include/clang-c/Index.h:3407
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getAlignOf";

  --*
  -- * Return the class type of an member pointer type.
  -- *
  -- * If a non-member-pointer type is passed in, an invalid type is returned.
  --  

   function Type_Get_Class_Type (T : Type_T) return Type_T  -- install/include/clang-c/Index.h:3414
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getClassType";

  --*
  -- * Return the size of a type in bytes as per C++[expr.sizeof] standard.
  -- *
  -- * If the type declaration is invalid, CXTypeLayoutError_Invalid is returned.
  -- * If the type declaration is an incomplete type, CXTypeLayoutError_Incomplete
  -- *   is returned.
  -- * If the type declaration is a dependent type, CXTypeLayoutError_Dependent is
  -- *   returned.
  --  

   function Type_Get_Size_Of (T : Type_T) return Long_Long_Integer  -- install/include/clang-c/Index.h:3425
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getSizeOf";

  --*
  -- * Return the offset of a field named S in a record of type T in bits
  -- *   as it would be returned by __offsetof__ as per C++11[18.2p4]
  -- *
  -- * If the cursor is not a record field declaration, CXTypeLayoutError_Invalid
  -- *   is returned.
  -- * If the field's type declaration is an incomplete type,
  -- *   CXTypeLayoutError_Incomplete is returned.
  -- * If the field's type declaration is a dependent type,
  -- *   CXTypeLayoutError_Dependent is returned.
  -- * If the field's name S is not found,
  -- *   CXTypeLayoutError_InvalidFieldName is returned.
  --  

function Type_Get_Offset_Of
     (T : Type_T;
      S : String)
      return Long_Long_Integer;

  --*
  -- * Return the type that was modified by this attributed type.
  -- *
  -- * If the type is not an attributed type, an invalid type is returned.
  --  

   function Type_Get_Modified_Type (T : Type_T) return Type_T  -- install/include/clang-c/Index.h:3447
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getModifiedType";

  --*
  -- * Gets the type contained by this atomic type.
  -- *
  -- * If a non-atomic type is passed in, an invalid type is returned.
  --  

   function Type_Get_Value_Type (CT : Type_T) return Type_T  -- install/include/clang-c/Index.h:3454
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getValueType";

  --*
  -- * Return the offset of the field represented by the Cursor.
  -- *
  -- * If the cursor is not a field declaration, -1 is returned.
  -- * If the cursor semantic parent is not a record field declaration,
  -- *   CXTypeLayoutError_Invalid is returned.
  -- * If the field's type declaration is an incomplete type,
  -- *   CXTypeLayoutError_Incomplete is returned.
  -- * If the field's type declaration is a dependent type,
  -- *   CXTypeLayoutError_Dependent is returned.
  -- * If the field's name S is not found,
  -- *   CXTypeLayoutError_InvalidFieldName is returned.
  --  

   function Cursor_Get_Offset_Of_Field (C : Cursor_T) return Long_Long_Integer  -- install/include/clang-c/Index.h:3469
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getOffsetOfField";

  --*
  -- * Determine whether the given cursor represents an anonymous
  -- * tag or namespace
  --  

function Cursor_Is_Anonymous
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine whether the given cursor represents an anonymous record
  -- * declaration.
  --  

function Cursor_Is_Anonymous_Record_Decl
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine whether the given cursor represents an inline namespace
  -- * declaration.
  --  

function Cursor_Is_Inline_Namespace
     (C : Cursor_T)
      return Boolean;

   type Ref_Qualifier_Kind_T is 
     (Ref_Qualifier_None,
      Ref_Qualifier_L_Value,
      Ref_Qualifier_R_Value)
   with Convention => C;  -- install/include/clang-c/Index.h:3489

  --* No ref-qualifier was provided.  
  --* An lvalue ref-qualifier was provided (\c &).  
  --* An rvalue ref-qualifier was provided (\c &&).  
  --*
  -- * Returns the number of template arguments for given template
  -- * specialization, or -1 if type \c T is not a template specialization.
  --  

   function Type_Get_Num_Template_Arguments (T : Type_T) return int  -- install/include/clang-c/Index.h:3502
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getNumTemplateArguments";

  --*
  -- * Returns the type template argument of a template class specialization
  -- * at given index.
  -- *
  -- * This function only returns template type arguments and does not handle
  -- * template template arguments or variadic packs.
  --  

   function Type_Get_Template_Argument_As_Type (T : Type_T; I : unsigned) return Type_T  -- install/include/clang-c/Index.h:3511
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getTemplateArgumentAsType";

  --*
  -- * Retrieve the ref-qualifier kind of a function or method.
  -- *
  -- * The ref-qualifier is returned for C++ functions or methods. For other types
  -- * or non-C++ declarations, CXRefQualifier_None is returned.
  --  

   function Type_Get_CXX_Ref_Qualifier (T : Type_T) return Ref_Qualifier_Kind_T  -- install/include/clang-c/Index.h:3520
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_getCXXRefQualifier";

  --*
  -- * Returns non-zero if the cursor specifies a Record member that is a
  -- *   bitfield.
  --  

function Cursor_Is_Bit_Field
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Returns 1 if the base class specified by the cursor with kind
  -- *   CX_CXXBaseSpecifier is virtual.
  --  

function Is_Virtual_Base
     (Arg_1 : Cursor_T)
      return Boolean;

  --*
  -- * Represents the C++ access control level to a base class for a
  -- * cursor with kind CX_CXXBaseSpecifier.
  --  

   type CXX_Access_Specifier_T is 
     (CXX_Invalid_Access_Specifier,
      CXX_Public,
      CXX_Protected,
      CXX_Private)
   with Convention => C;  -- install/include/clang-c/Index.h:3538

  --*
  -- * Returns the access control level for the referenced object.
  -- *
  -- * If the cursor refers to a C++ declaration, its access control level within
  -- * its parent scope is returned. Otherwise, if the cursor refers to a base
  -- * specifier or access specifier, the specifier itself is returned.
  --  

   function Get_CXX_Access_Specifier (Arg_1 : Cursor_T) return CXX_Access_Specifier_T  -- install/include/clang-c/Index.h:3552
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCXXAccessSpecifier";

  --*
  -- * Represents the storage classes as declared in the source. CX_SC_Invalid
  -- * was added for the case that the passed cursor in not a declaration.
  --  

   type Storage_Class_T is 
     (SC_Invalid,
      SC_None,
      SC_Extern,
      SC_Static,
      SC_Private_Extern,
      SC_Open_CL_Work_Group_Local,
      SC_Auto,
      SC_Register)
   with Convention => C;  -- install/include/clang-c/Index.h:3558

  --*
  -- * Returns the storage class for a function or variable declaration.
  -- *
  -- * If the passed in Cursor is not a function or variable declaration,
  -- * CX_SC_Invalid is returned else the storage class.
  --  

   function Cursor_Get_Storage_Class (Arg_1 : Cursor_T) return Storage_Class_T  -- install/include/clang-c/Index.h:3575
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getStorageClass";

  --*
  -- * Determine the number of overloaded declarations referenced by a
  -- * \c CXCursor_OverloadedDeclRef cursor.
  -- *
  -- * \param cursor The cursor whose overloaded declarations are being queried.
  -- *
  -- * \returns The number of overloaded declarations referenced by \c cursor. If it
  -- * is not a \c CXCursor_OverloadedDeclRef cursor, returns 0.
  --  

   function Get_Num_Overloaded_Decls (Cursor : Cursor_T) return unsigned  -- install/include/clang-c/Index.h:3586
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getNumOverloadedDecls";

  --*
  -- * Retrieve a cursor for one of the overloaded declarations referenced
  -- * by a \c CXCursor_OverloadedDeclRef cursor.
  -- *
  -- * \param cursor The cursor whose overloaded declarations are being queried.
  -- *
  -- * \param index The zero-based index into the set of overloaded declarations in
  -- * the cursor.
  -- *
  -- * \returns A cursor representing the declaration referenced by the given
  -- * \c cursor at the specified \c index. If the cursor does not have an
  -- * associated set of overloaded declarations, or if the index is out of bounds,
  -- * returns \c clang_getNullCursor();
  --  

   function Get_Overloaded_Decl (Cursor : Cursor_T; Index : unsigned) return Cursor_T  -- install/include/clang-c/Index.h:3602
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getOverloadedDecl";

  --*
  -- * @}
  --  

  --*
  -- * \defgroup CINDEX_ATTRIBUTES Information for attributes
  -- *
  -- * @{
  --  

  --*
  -- * For cursors representing an iboutletcollection attribute,
  -- *  this function returns the collection element type.
  -- *
  --  

   function Get_IB_Outlet_Collection_Type (Arg_1 : Cursor_T) return Type_T  -- install/include/clang-c/Index.h:3620
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getIBOutletCollectionType";

  --*
  -- * @}
  --  

  --*
  -- * \defgroup CINDEX_CURSOR_TRAVERSAL Traversing the AST with cursors
  -- *
  -- * These routines provide the ability to traverse the abstract syntax tree
  -- * using cursors.
  -- *
  -- * @{
  --  

  --*
  -- * Describes how the traversal of the children of a particular
  -- * cursor should proceed after visiting a particular child cursor.
  -- *
  -- * A value of this enumeration type should be returned by each
  -- * \c CXCursorVisitor to indicate how clang_visitChildren() proceed.
  --  

   type Child_Visit_Result_T is 
     (Child_Visit_Break,
      Child_Visit_Continue,
      Child_Visit_Recurse)
   with Convention => C;  -- install/include/clang-c/Index.h:3642

  --*
  --   * Terminates the cursor traversal.
  --    

  --*
  --   * Continues the cursor traversal with the next sibling of
  --   * the cursor just visited, without visiting its children.
  --    

  --*
  --   * Recursively traverse the children of this cursor, using
  --   * the same visitor and client data.
  --    

  --*
  -- * Visitor invoked for each cursor found by a traversal.
  -- *
  -- * This visitor function will be invoked for each cursor found by
  -- * clang_visitCursorChildren(). Its first argument is the cursor being
  -- * visited, its second argument is the parent visitor for that cursor,
  -- * and its third argument is the client data provided to
  -- * clang_visitCursorChildren().
  -- *
  -- * The visitor should return one of the \c CXChildVisitResult values
  -- * to direct clang_visitCursorChildren().
  --  

   type Cursor_Visitor_T is access function
        (Arg_1 : Cursor_T;
         Arg_2 : Cursor_T;
         Arg_3 : Client_Data_T) return Child_Visit_Result_T
   with Convention => C;  -- install/include/clang-c/Index.h:3671

  --*
  -- * Visit the children of a particular cursor.
  -- *
  -- * This function visits all the direct children of the given cursor,
  -- * invoking the given \p visitor function with the cursors of each
  -- * visited child. The traversal may be recursive, if the visitor returns
  -- * \c CXChildVisit_Recurse. The traversal may also be ended prematurely, if
  -- * the visitor returns \c CXChildVisit_Break.
  -- *
  -- * \param parent the cursor whose child may be visited. All kinds of
  -- * cursors can be visited, including invalid cursors (which, by
  -- * definition, have no children).
  -- *
  -- * \param visitor the visitor function that will be invoked for each
  -- * child of \p parent.
  -- *
  -- * \param client_data pointer data supplied by the client, which will
  -- * be passed to the visitor each time it is invoked.
  -- *
  -- * \returns a non-zero value if the traversal was terminated
  -- * prematurely by the visitor returning \c CXChildVisit_Break.
  --  

   function Visit_Children
     (Parent : Cursor_T;
      Visitor : Cursor_Visitor_T;
      Client_Data : Client_Data_T) return unsigned  -- install/include/clang-c/Index.h:3697
   with Import => True, 
        Convention => C, 
        External_Name => "clang_visitChildren";

  --*
  -- * Visitor invoked for each cursor found by a traversal.
  -- *
  -- * This visitor block will be invoked for each cursor found by
  -- * clang_visitChildrenWithBlock(). Its first argument is the cursor being
  -- * visited, its second argument is the parent visitor for that cursor.
  -- *
  -- * The visitor should return one of the \c CXChildVisitResult values
  -- * to direct clang_visitChildrenWithBlock().
  --  

  --*
  -- * Visits the children of a cursor using the specified block.  Behaves
  -- * identically to clang_visitChildren() in all other respects.
  --  

  --*
  -- * @}
  --  

  --*
  -- * \defgroup CINDEX_CURSOR_XREF Cross-referencing in the AST
  -- *
  -- * These routines provide the ability to determine references within and
  -- * across translation units, by providing the names of the entities referenced
  -- * by cursors, follow reference cursors to the declarations they reference,
  -- * and associate declarations with their definitions.
  -- *
  -- * @{
  --  

  --*
  -- * Retrieve a Unified Symbol Resolution (USR) for the entity referenced
  -- * by the given cursor.
  -- *
  -- * A Unified Symbol Resolution (USR) is a string that identifies a particular
  -- * entity (function, class, variable, etc.) within a program. USRs can be
  -- * compared across translation units to determine, e.g., when references in
  -- * one translation refer to an entity defined in another translation unit.
  --  

function Get_Cursor_USR
     (Arg_1 : Cursor_T)
      return String;

  --*
  -- * Construct a USR for a specified Objective-C class.
  --  

function Construct_USR_Obj_C_Class
     (Class_Name : String)
      return String;

  --*
  -- * Construct a USR for a specified Objective-C category.
  --  

function Construct_USR_Obj_C_Category
     (Class_Name    : String;
      Category_Name : String)
      return String;

  --*
  -- * Construct a USR for a specified Objective-C protocol.
  --  

function Construct_USR_Obj_C_Protocol
     (Protocol_Name : String)
      return String;

  --*
  -- * Construct a USR for a specified Objective-C instance variable and
  -- *   the USR for its containing class.
  --  

function Construct_USR_Obj_C_Ivar
     (Name      : String;
      Class_USR : Clang.CX_String.String_T)
      return String;

  --*
  -- * Construct a USR for a specified Objective-C method and
  -- *   the USR for its containing class.
  --  

function Construct_USR_Obj_C_Method
     (Name               : String;
      Is_Instance_Method : unsigned;
      Class_USR          : Clang.CX_String.String_T)
      return String;

  --*
  -- * Construct a USR for a specified Objective-C property and the USR
  -- *  for its containing class.
  --  

function Construct_USR_Obj_C_Property
     (Property  : String;
      Class_USR : Clang.CX_String.String_T)
      return String;

  --*
  -- * Retrieve a name for the entity referenced by this cursor.
  --  

function Get_Cursor_Spelling
     (Arg_1 : Cursor_T)
      return String;

  --*
  -- * Retrieve a range for a piece that forms the cursors spelling name.
  -- * Most of the times there is only one range for the complete spelling but for
  -- * Objective-C methods and Objective-C message expressions, there are multiple
  -- * pieces for each selector identifier.
  -- *
  -- * \param pieceIndex the index of the spelling name piece. If this is greater
  -- * than the actual number of pieces, it will return a NULL (invalid) range.
  -- *
  -- * \param options Reserved.
  --  

   function Cursor_Get_Spelling_Name_Range
     (Arg_1 : Cursor_T;
      Piece_Index : unsigned;
      Options : unsigned) return Clang.CX_Source_Location.Source_Range_T  -- install/include/clang-c/Index.h:3805
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getSpellingNameRange";

  --*
  -- * Opaque pointer representing a policy that controls pretty printing
  -- * for \c clang_getCursorPrettyPrinted.
  --  

   type Printing_Policy_T is new System.Address;  -- install/include/clang-c/Index.h:3812

  --*
  -- * Properties for the printing policy.
  -- *
  -- * See \c clang::PrintingPolicy for more information.
  --  

   subtype Printing_Policy_Property_T is unsigned;
   Printing_Policy_Indentation : constant Printing_Policy_Property_T := 0;
   Printing_Policy_Suppress_Specifiers : constant Printing_Policy_Property_T := 1;
   Printing_Policy_Suppress_Tag_Keyword : constant Printing_Policy_Property_T := 2;
   Printing_Policy_Include_Tag_Definition : constant Printing_Policy_Property_T := 3;
   Printing_Policy_Suppress_Scope : constant Printing_Policy_Property_T := 4;
   Printing_Policy_Suppress_Unwritten_Scope : constant Printing_Policy_Property_T := 5;
   Printing_Policy_Suppress_Initializers : constant Printing_Policy_Property_T := 6;
   Printing_Policy_Constant_Array_Size_As_Written : constant Printing_Policy_Property_T := 7;
   Printing_Policy_Anonymous_Tag_Locations : constant Printing_Policy_Property_T := 8;
   Printing_Policy_Suppress_Strong_Lifetime : constant Printing_Policy_Property_T := 9;
   Printing_Policy_Suppress_Lifetime_Qualifiers : constant Printing_Policy_Property_T := 10;
   Printing_Policy_Suppress_Template_Args_In_CXX_Constructors : constant Printing_Policy_Property_T := 11;
   Printing_Policy_Bool : constant Printing_Policy_Property_T := 12;
   Printing_Policy_Restrict : constant Printing_Policy_Property_T := 13;
   Printing_Policy_Alignof : constant Printing_Policy_Property_T := 14;
   Printing_Policy_Underscore_Alignof : constant Printing_Policy_Property_T := 15;
   Printing_Policy_Use_Void_For_Zero_Params : constant Printing_Policy_Property_T := 16;
   Printing_Policy_Terse_Output : constant Printing_Policy_Property_T := 17;
   Printing_Policy_Polish_For_Declaration : constant Printing_Policy_Property_T := 18;
   Printing_Policy_Half : constant Printing_Policy_Property_T := 19;
   Printing_Policy_MSW_Char : constant Printing_Policy_Property_T := 20;
   Printing_Policy_Include_Newlines : constant Printing_Policy_Property_T := 21;
   Printing_Policy_MSVC_Formatting : constant Printing_Policy_Property_T := 22;
   Printing_Policy_Constants_As_Written : constant Printing_Policy_Property_T := 23;
   Printing_Policy_Suppress_Implicit_Base : constant Printing_Policy_Property_T := 24;
   Printing_Policy_Fully_Qualified_Name : constant Printing_Policy_Property_T := 25;
   Printing_Policy_Last_Property : constant Printing_Policy_Property_T := 25;  -- install/include/clang-c/Index.h:3819

  --*
  -- * Get a property value for the given printing policy.
  --  

   function Printing_Policy_Get_Property (Policy : Printing_Policy_T; Property : Printing_Policy_Property_T) return unsigned  -- install/include/clang-c/Index.h:3854
   with Import => True, 
        Convention => C, 
        External_Name => "clang_PrintingPolicy_getProperty";

  --*
  -- * Set a property value for the given printing policy.
  --  

   procedure Printing_Policy_Set_Property
     (Policy : Printing_Policy_T;
      Property : Printing_Policy_Property_T;
      Value : unsigned)  -- install/include/clang-c/Index.h:3861
   with Import => True, 
        Convention => C, 
        External_Name => "clang_PrintingPolicy_setProperty";

  --*
  -- * Retrieve the default policy for the cursor.
  -- *
  -- * The policy should be released after use with \c
  -- * clang_PrintingPolicy_dispose.
  --  

   function Get_Cursor_Printing_Policy (Arg_1 : Cursor_T) return Printing_Policy_T  -- install/include/clang-c/Index.h:3871
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorPrintingPolicy";

  --*
  -- * Release a printing policy.
  --  

   procedure Printing_Policy_Dispose (Policy : Printing_Policy_T)  -- install/include/clang-c/Index.h:3876
   with Import => True, 
        Convention => C, 
        External_Name => "clang_PrintingPolicy_dispose";

  --*
  -- * Pretty print declarations.
  -- *
  -- * \param Cursor The cursor representing a declaration.
  -- *
  -- * \param Policy The policy to control the entities being printed. If
  -- * NULL, a default policy is used.
  -- *
  -- * \returns The pretty printed declaration or the empty string for
  -- * other cursors.
  --  

function Get_Cursor_Pretty_Printed
     (Cursor : Cursor_T;
      Policy : Printing_Policy_T)
      return String;

  --*
  -- * Retrieve the display name for the entity referenced by this cursor.
  -- *
  -- * The display name contains extra information that helps identify the cursor,
  -- * such as the parameters of a function or template or the arguments of a
  -- * class template specialization.
  --  

function Get_Cursor_Display_Name
     (Arg_1 : Cursor_T)
      return String;

  --* For a cursor that is a reference, retrieve a cursor representing the
  -- * entity that it references.
  -- *
  -- * Reference cursors refer to other entities in the AST. For example, an
  -- * Objective-C superclass reference cursor refers to an Objective-C class.
  -- * This function produces the cursor for the Objective-C class from the
  -- * cursor for the superclass reference. If the input cursor is a declaration or
  -- * definition, it returns that declaration or definition unchanged.
  -- * Otherwise, returns the NULL cursor.
  --  

   function Get_Cursor_Referenced (Arg_1 : Cursor_T) return Cursor_T  -- install/include/clang-c/Index.h:3911
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorReferenced";

  --*
  -- *  For a cursor that is either a reference to or a declaration
  -- *  of some entity, retrieve a cursor that describes the definition of
  -- *  that entity.
  -- *
  -- *  Some entities can be declared multiple times within a translation
  -- *  unit, but only one of those declarations can also be a
  -- *  definition. For example, given:
  -- *
  -- *  \code
  -- *  int f(int, int);
  -- *  int g(int x, int y) { return f(x, y); }
  -- *  int f(int a, int b) { return a + b; }
  -- *  int f(int, int);
  -- *  \endcode
  -- *
  -- *  there are three declarations of the function "f", but only the
  -- *  second one is a definition. The clang_getCursorDefinition()
  -- *  function will take any cursor pointing to a declaration of "f"
  -- *  (the first or fourth lines of the example) or a cursor referenced
  -- *  that uses "f" (the call to "f' inside "g") and will return a
  -- *  declaration cursor pointing to the definition (the second "f"
  -- *  declaration).
  -- *
  -- *  If given a cursor for which there is no corresponding definition,
  -- *  e.g., because there is no definition of that entity within this
  -- *  translation unit, returns a NULL cursor.
  --  

   function Get_Cursor_Definition (Arg_1 : Cursor_T) return Cursor_T  -- install/include/clang-c/Index.h:3941
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorDefinition";

  --*
  -- * Determine whether the declaration pointed to by this cursor
  -- * is also a definition of that entity.
  --  

function Is_Cursor_Definition
     (Arg_1 : Cursor_T)
      return Boolean;

  --*
  -- * Retrieve the canonical cursor corresponding to the given cursor.
  -- *
  -- * In the C family of languages, many kinds of entities can be declared several
  -- * times within a single translation unit. For example, a structure type can
  -- * be forward-declared (possibly multiple times) and later defined:
  -- *
  -- * \code
  -- * struct X;
  -- * struct X;
  -- * struct X {
  -- *   int member;
  -- * };
  -- * \endcode
  -- *
  -- * The declarations and the definition of \c X are represented by three
  -- * different cursors, all of which are declarations of the same underlying
  -- * entity. One of these cursor is considered the "canonical" cursor, which
  -- * is effectively the representative for the underlying entity. One can
  -- * determine if two cursors are declarations of the same underlying entity by
  -- * comparing their canonical cursors.
  -- *
  -- * \returns The canonical cursor for the entity referred to by the given cursor.
  --  

   function Get_Canonical_Cursor (Arg_1 : Cursor_T) return Cursor_T  -- install/include/clang-c/Index.h:3973
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCanonicalCursor";

  --*
  -- * If the cursor points to a selector identifier in an Objective-C
  -- * method or message expression, this returns the selector index.
  -- *
  -- * After getting a cursor with #clang_getCursor, this can be called to
  -- * determine if the location points to a selector identifier.
  -- *
  -- * \returns The selector index if the cursor is an Objective-C method or message
  -- * expression and the cursor is pointing to a selector identifier, or -1
  -- * otherwise.
  --  

   function Cursor_Get_Obj_C_Selector_Index (Arg_1 : Cursor_T) return int  -- install/include/clang-c/Index.h:3986
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getObjCSelectorIndex";

  --*
  -- * Given a cursor pointing to a C++ method call or an Objective-C
  -- * message, returns non-zero if the method/message is "dynamic", meaning:
  -- *
  -- * For a C++ method: the call is virtual.
  -- * For an Objective-C message: the receiver is an object instance, not 'super'
  -- * or a specific class.
  -- *
  -- * If the method/message is "static" or the cursor does not point to a
  -- * method/message, it will return zero.
  --  

function Cursor_Is_Dynamic_Call
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Given a cursor pointing to an Objective-C message or property
  -- * reference, or C++ method call, returns the CXType of the receiver.
  --  

   function Cursor_Get_Receiver_Type (C : Cursor_T) return Type_T  -- install/include/clang-c/Index.h:4005
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getReceiverType";

  --*
  -- * Property attributes for a \c CXCursor_ObjCPropertyDecl.
  --  

   subtype Obj_C_Property_Attr_Kind_T is unsigned;
   Obj_C_Property_Attr_Noattr : constant Obj_C_Property_Attr_Kind_T := 0;
   Obj_C_Property_Attr_Readonly : constant Obj_C_Property_Attr_Kind_T := 1;
   Obj_C_Property_Attr_Getter : constant Obj_C_Property_Attr_Kind_T := 2;
   Obj_C_Property_Attr_Assign : constant Obj_C_Property_Attr_Kind_T := 4;
   Obj_C_Property_Attr_Readwrite : constant Obj_C_Property_Attr_Kind_T := 8;
   Obj_C_Property_Attr_Retain : constant Obj_C_Property_Attr_Kind_T := 16;
   Obj_C_Property_Attr_Copy : constant Obj_C_Property_Attr_Kind_T := 32;
   Obj_C_Property_Attr_Nonatomic : constant Obj_C_Property_Attr_Kind_T := 64;
   Obj_C_Property_Attr_Setter : constant Obj_C_Property_Attr_Kind_T := 128;
   Obj_C_Property_Attr_Atomic : constant Obj_C_Property_Attr_Kind_T := 256;
   Obj_C_Property_Attr_Weak : constant Obj_C_Property_Attr_Kind_T := 512;
   Obj_C_Property_Attr_Strong : constant Obj_C_Property_Attr_Kind_T := 1024;
   Obj_C_Property_Attr_Unsafe_Unretained : constant Obj_C_Property_Attr_Kind_T := 2048;
   Obj_C_Property_Attr_Class : constant Obj_C_Property_Attr_Kind_T := 4096;  -- install/include/clang-c/Index.h:4025

  --*
  -- * Given a cursor that represents a property declaration, return the
  -- * associated property attributes. The bits are formed from
  -- * \c CXObjCPropertyAttrKind.
  -- *
  -- * \param reserved Reserved for future use, pass 0.
  --  

   function Cursor_Get_Obj_C_Property_Attributes (C : Cursor_T; Reserved : unsigned) return unsigned  -- install/include/clang-c/Index.h:4035
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getObjCPropertyAttributes";

  --*
  -- * Given a cursor that represents a property declaration, return the
  -- * name of the method that implements the getter.
  --  

function Cursor_Get_Obj_C_Property_Getter_Name
     (C : Cursor_T)
      return String;

  --*
  -- * Given a cursor that represents a property declaration, return the
  -- * name of the method that implements the setter, if any.
  --  

function Cursor_Get_Obj_C_Property_Setter_Name
     (C : Cursor_T)
      return String;

  --*
  -- * 'Qualifiers' written next to the return and parameter types in
  -- * Objective-C method declarations.
  --  

   subtype Obj_C_Decl_Qualifier_Kind_T is unsigned;
   Obj_C_Decl_Qualifier_None : constant Obj_C_Decl_Qualifier_Kind_T := 0;
   Obj_C_Decl_Qualifier_In : constant Obj_C_Decl_Qualifier_Kind_T := 1;
   Obj_C_Decl_Qualifier_Inout : constant Obj_C_Decl_Qualifier_Kind_T := 2;
   Obj_C_Decl_Qualifier_Out : constant Obj_C_Decl_Qualifier_Kind_T := 4;
   Obj_C_Decl_Qualifier_Bycopy : constant Obj_C_Decl_Qualifier_Kind_T := 8;
   Obj_C_Decl_Qualifier_Byref : constant Obj_C_Decl_Qualifier_Kind_T := 16;
   Obj_C_Decl_Qualifier_Oneway : constant Obj_C_Decl_Qualifier_Kind_T := 32;  -- install/include/clang-c/Index.h:4061

  --*
  -- * Given a cursor that represents an Objective-C method or parameter
  -- * declaration, return the associated Objective-C qualifiers for the return
  -- * type or the parameter respectively. The bits are formed from
  -- * CXObjCDeclQualifierKind.
  --  

   function Cursor_Get_Obj_C_Decl_Qualifiers (C : Cursor_T) return unsigned  -- install/include/clang-c/Index.h:4069
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getObjCDeclQualifiers";

  --*
  -- * Given a cursor that represents an Objective-C method or property
  -- * declaration, return non-zero if the declaration was affected by "\@optional".
  -- * Returns zero if the cursor is not such a declaration or it is "\@required".
  --  

function Cursor_Is_Obj_C_Optional
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Returns non-zero if the given cursor is a variadic function or method.
  --  

function Cursor_Is_Variadic
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Returns non-zero if the given cursor points to a symbol marked with
  -- * external_source_symbol attribute.
  -- *
  -- * \param language If non-NULL, and the attribute is present, will be set to
  -- * the 'language' string from the attribute.
  -- *
  -- * \param definedIn If non-NULL, and the attribute is present, will be set to
  -- * the 'definedIn' string from the attribute.
  -- *
  -- * \param isGenerated If non-NULL, and the attribute is present, will be set to
  -- * non-zero if the 'generated_declaration' is set in the attribute.
  --  

function Cursor_Is_External_Symbol
     (C            : Cursor_T;
      Language     : access Clang.CX_String.String_T;
      Defined_In   : access Clang.CX_String.String_T;
      Is_Generated : access unsigned)
      return Boolean;

  --*
  -- * Given a cursor that represents a declaration, return the associated
  -- * comment's source range.  The range may include multiple consecutive comments
  -- * with whitespace in between.
  --  

   function Cursor_Get_Comment_Range (C : Cursor_T) return Clang.CX_Source_Location.Source_Range_T  -- install/include/clang-c/Index.h:4106
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getCommentRange";

  --*
  -- * Given a cursor that represents a declaration, return the associated
  -- * comment text, including comment markers.
  --  

function Cursor_Get_Raw_Comment_Text
     (C : Cursor_T)
      return String;

  --*
  -- * Given a cursor that represents a documentable entity (e.g.,
  -- * declaration), return the associated \paragraph; otherwise return the
  -- * first paragraph.
  --  

function Cursor_Get_Brief_Comment_Text
     (C : Cursor_T)
      return String;

  --*
  -- * @}
  --  

  --* \defgroup CINDEX_MANGLE Name Mangling API Functions
  -- *
  -- * @{
  --  

  --*
  -- * Retrieve the CXString representing the mangled name of the cursor.
  --  

function Cursor_Get_Mangling
     (Arg_1 : Cursor_T)
      return String;

  --*
  -- * Retrieve the CXStrings representing the mangled symbols of the C++
  -- * constructor or destructor at the cursor.
  --  

   function Cursor_Get_CXX_Manglings (Arg_1 : Cursor_T) return access Clang.CX_String.String_Set_T  -- install/include/clang-c/Index.h:4139
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getCXXManglings";

  --*
  -- * Retrieve the CXStrings representing the mangled symbols of the ObjC
  -- * class interface or implementation at the cursor.
  --  

   function Cursor_Get_Obj_C_Manglings (Arg_1 : Cursor_T) return access Clang.CX_String.String_Set_T  -- install/include/clang-c/Index.h:4145
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getObjCManglings";

  --*
  -- * @}
  --  

  --*
  -- * \defgroup CINDEX_MODULE Module introspection
  -- *
  -- * The functions in this group provide access to information about modules.
  -- *
  -- * @{
  --  

   type Module_T is new System.Address;  -- install/include/clang-c/Index.h:4159

  --*
  -- * Given a CXCursor_ModuleImportDecl cursor, return the associated module.
  --  

   function Cursor_Get_Module (C : Cursor_T) return Module_T  -- install/include/clang-c/Index.h:4164
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getModule";

  --*
  -- * Given a CXFile header file, return the module that contains it, if one
  -- * exists.
  --  

   function Get_Module_For_File (Arg_1 : Translation_Unit_T; Arg_2 : Clang.CX_File.File_T) return Module_T  -- install/include/clang-c/Index.h:4170
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getModuleForFile";

  --*
  -- * \param Module a module object.
  -- *
  -- * \returns the module file where the provided module object came from.
  --  

   function Module_Get_AST_File (Module : Module_T) return Clang.CX_File.File_T  -- install/include/clang-c/Index.h:4177
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Module_getASTFile";

  --*
  -- * \param Module a module object.
  -- *
  -- * \returns the parent of a sub-module or NULL if the given module is top-level,
  -- * e.g. for 'std.vector' it will return the 'std' module.
  --  

   function Module_Get_Parent (Module : Module_T) return Module_T  -- install/include/clang-c/Index.h:4185
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Module_getParent";

  --*
  -- * \param Module a module object.
  -- *
  -- * \returns the name of the module, e.g. for the 'std.vector' sub-module it
  -- * will return "vector".
  --  

function Module_Get_Name
     (Module : Module_T)
      return String;

  --*
  -- * \param Module a module object.
  -- *
  -- * \returns the full name of the module, e.g. "std.vector".
  --  

function Module_Get_Full_Name
     (Module : Module_T)
      return String;

  --*
  -- * \param Module a module object.
  -- *
  -- * \returns non-zero if the module is a system one.
  --  

function Module_Is_System
     (Module : Module_T)
      return Boolean;

  --*
  -- * \param Module a module object.
  -- *
  -- * \returns the number of top level headers associated with this module.
  --  

   function Module_Get_Num_Top_Level_Headers (Arg_1 : Translation_Unit_T; Module : Module_T) return unsigned  -- install/include/clang-c/Index.h:4214
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Module_getNumTopLevelHeaders";

  --*
  -- * \param Module a module object.
  -- *
  -- * \param Index top level header index (zero-based).
  -- *
  -- * \returns the specified top level header associated with the module.
  --  

   function Module_Get_Top_Level_Header
     (Arg_1 : Translation_Unit_T;
      Module : Module_T;
      Index : unsigned) return Clang.CX_File.File_T  -- install/include/clang-c/Index.h:4225
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Module_getTopLevelHeader";

  --*
  -- * @}
  --  

  --*
  -- * \defgroup CINDEX_CPP C++ AST introspection
  -- *
  -- * The routines in this group provide access information in the ASTs specific
  -- * to C++ language features.
  -- *
  -- * @{
  --  

  --*
  -- * Determine if a C++ constructor is a converting constructor.
  --  

function CXX_Constructor_Is_Converting_Constructor
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine if a C++ constructor is a copy constructor.
  --  

function CXX_Constructor_Is_Copy_Constructor
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine if a C++ constructor is the default constructor.
  --  

function CXX_Constructor_Is_Default_Constructor
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine if a C++ constructor is a move constructor.
  --  

function CXX_Constructor_Is_Move_Constructor
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine if a C++ field is declared 'mutable'.
  --  

function CXX_Field_Is_Mutable
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine if a C++ method is declared '= default'.
  --  

function CXX_Method_Is_Defaulted
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine if a C++ method is declared '= delete'.
  --  

function CXX_Method_Is_Deleted
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine if a C++ member function or member function template is
  -- * pure virtual.
  --  

function CXX_Method_Is_Pure_Virtual
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine if a C++ member function or member function template is
  -- * declared 'static'.
  --  

function CXX_Method_Is_Static
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine if a C++ member function or member function template is
  -- * explicitly declared 'virtual' or if it overrides a virtual method from
  -- * one of the base classes.
  --  

function CXX_Method_Is_Virtual
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine if a C++ member function is a copy-assignment operator,
  -- * returning 1 if such is the case and 0 otherwise.
  -- *
  -- * > A copy-assignment operator `X::operator=` is a non-static,
  -- * > non-template member function of _class_ `X` with exactly one
  -- * > parameter of type `X`, `X&`, `const X&`, `volatile X&` or `const
  -- * > volatile X&`.
  -- *
  -- * That is, for example, the `operator=` in:
  -- *
  -- *    class Foo {
  -- *        bool operator=(const volatile Foo&);
  -- *    };
  -- *
  -- * Is a copy-assignment operator, while the `operator=` in:
  -- *
  -- *    class Bar {
  -- *        bool operator=(const int&);
  -- *    };
  -- *
  -- * Is not.
  --  

function CXX_Method_Is_Copy_Assignment_Operator
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine if a C++ member function is a move-assignment operator,
  -- * returning 1 if such is the case and 0 otherwise.
  -- *
  -- * > A move-assignment operator `X::operator=` is a non-static,
  -- * > non-template member function of _class_ `X` with exactly one
  -- * > parameter of type `X&&`, `const X&&`, `volatile X&&` or `const
  -- * > volatile X&&`.
  -- *
  -- * That is, for example, the `operator=` in:
  -- *
  -- *    class Foo {
  -- *        bool operator=(const volatile Foo&&);
  -- *    };
  -- *
  -- * Is a move-assignment operator, while the `operator=` in:
  -- *
  -- *    class Bar {
  -- *        bool operator=(const int&&);
  -- *    };
  -- *
  -- * Is not.
  --  

function CXX_Method_Is_Move_Assignment_Operator
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine if a C++ record is abstract, i.e. whether a class or struct
  -- * has a pure virtual member function.
  --  

function CXX_Record_Is_Abstract
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine if an enum declaration refers to a scoped enum.
  --  

function Enum_Decl_Is_Scoped
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Determine if a C++ member function or member function template is
  -- * declared 'const'.
  --  

function CXX_Method_Is_Const
     (C : Cursor_T)
      return Boolean;

  --*
  -- * Given a cursor that represents a template, determine
  -- * the cursor kind of the specializations would be generated by instantiating
  -- * the template.
  -- *
  -- * This routine can be used to determine what flavor of function template,
  -- * class template, or class template partial specialization is stored in the
  -- * cursor. For example, it can describe whether a class template cursor is
  -- * declared with "struct", "class" or "union".
  -- *
  -- * \param C The cursor to query. This cursor should represent a template
  -- * declaration.
  -- *
  -- * \returns The cursor kind of the specializations that would be generated
  -- * by instantiating the template \p C. If \p C is not a template, returns
  -- * \c CXCursor_NoDeclFound.
  --  

   function Get_Template_Cursor_Kind (C : Cursor_T) return Cursor_Kind_T  -- install/include/clang-c/Index.h:4380
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getTemplateCursorKind";

  --*
  -- * Given a cursor that may represent a specialization or instantiation
  -- * of a template, retrieve the cursor that represents the template that it
  -- * specializes or from which it was instantiated.
  -- *
  -- * This routine determines the template involved both for explicit
  -- * specializations of templates and for implicit instantiations of the template,
  -- * both of which are referred to as "specializations". For a class template
  -- * specialization (e.g., \c std::vector<bool>), this routine will return
  -- * either the primary template (\c std::vector) or, if the specialization was
  -- * instantiated from a class template partial specialization, the class template
  -- * partial specialization. For a class template partial specialization and a
  -- * function template specialization (including instantiations), this
  -- * this routine will return the specialized template.
  -- *
  -- * For members of a class template (e.g., member functions, member classes, or
  -- * static data members), returns the specialized or instantiated member.
  -- * Although not strictly "templates" in the C++ language, members of class
  -- * templates have the same notions of specializations and instantiations that
  -- * templates do, so this routine treats them similarly.
  -- *
  -- * \param C A cursor that may be a specialization of a template or a member
  -- * of a template.
  -- *
  -- * \returns If the given cursor is a specialization or instantiation of a
  -- * template or a member thereof, the template or member that it specializes or
  -- * from which it was instantiated. Otherwise, returns a NULL cursor.
  --  

   function Get_Specialized_Cursor_Template (C : Cursor_T) return Cursor_T  -- install/include/clang-c/Index.h:4410
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getSpecializedCursorTemplate";

  --*
  -- * Given a cursor that references something else, return the source range
  -- * covering that reference.
  -- *
  -- * \param C A cursor pointing to a member reference, a declaration reference, or
  -- * an operator call.
  -- * \param NameFlags A bitset with three independent flags:
  -- * CXNameRange_WantQualifier, CXNameRange_WantTemplateArgs, and
  -- * CXNameRange_WantSinglePiece.
  -- * \param PieceIndex For contiguous names or when passing the flag
  -- * CXNameRange_WantSinglePiece, only one piece with index 0 is
  -- * available. When the CXNameRange_WantSinglePiece flag is not passed for a
  -- * non-contiguous names, this index can be used to retrieve the individual
  -- * pieces of the name. See also CXNameRange_WantSinglePiece.
  -- *
  -- * \returns The piece of the name pointed to by the given cursor. If there is no
  -- * name, or if the PieceIndex is out-of-range, a null-cursor will be returned.
  --  

   function Get_Cursor_Reference_Name_Range
     (C : Cursor_T;
      Name_Flags : unsigned;
      Piece_Index : unsigned) return Clang.CX_Source_Location.Source_Range_T  -- install/include/clang-c/Index.h:4430
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorReferenceNameRange";

   subtype Name_Ref_Flags_T is unsigned;
   Name_Range_Want_Qualifier : constant Name_Ref_Flags_T := 1;
   Name_Range_Want_Template_Args : constant Name_Ref_Flags_T := 2;
   Name_Range_Want_Single_Piece : constant Name_Ref_Flags_T := 4;  -- install/include/clang-c/Index.h:4433

  --*
  --   * Include the nested-name-specifier, e.g. Foo:: in x.Foo::y, in the
  --   * range.
  --    

  --*
  --   * Include the explicit template arguments, e.g. \<int> in x.f<int>,
  --   * in the range.
  --    

  --*
  --   * If the name is non-contiguous, return the full spanning range.
  --   *
  --   * Non-contiguous names occur in Objective-C when a selector with two or more
  --   * parameters is used, or in C++ when using an operator:
  --   * \code
  --   * [object doSomething:here withValue:there]; // Objective-C
  --   * return some_vector[1]; // C++
  --   * \endcode
  --    

  --*
  -- * @}
  --  

  --*
  -- * \defgroup CINDEX_LEX Token extraction and manipulation
  -- *
  -- * The routines in this group provide access to the tokens within a
  -- * translation unit, along with a semantic mapping of those tokens to
  -- * their corresponding cursors.
  -- *
  -- * @{
  --  

  --*
  -- * Describes a kind of token.
  --  

   type Token_Kind_T is 
     (Token_Punctuation,
      Token_Keyword,
      Token_Identifier,
      Token_Literal,
      Token_Comment)
   with Convention => C;  -- install/include/clang-c/Index.h:4476

  --*
  --   * A token that contains some kind of punctuation.
  --    

  --*
  --   * A language keyword.
  --    

  --*
  --   * An identifier (that is not a keyword).
  --    

  --*
  --   * A numeric, string, or character literal.
  --    

  --*
  --   * A comment.
  --    

  --*
  -- * Describes a single preprocessing token.
  --  

   type anon_array1420 is array (0 .. 3) of aliased unsigned;
   type Token_T is record
      int_data : aliased anon_array1420;  -- install/include/clang-c/Index.h:4507
      ptr_data : System.Address;  -- install/include/clang-c/Index.h:4508
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:4509

  --*
  -- * Get the raw lexical token starting with the given location.
  -- *
  -- * \param TU the translation unit whose text is being tokenized.
  -- *
  -- * \param Location the source location with which the token starts.
  -- *
  -- * \returns The token starting with the given location or NULL if no such token
  -- * exist. The returned pointer must be freed with clang_disposeTokens before the
  -- * translation unit is destroyed.
  --  

   function Get_Token (TU : Translation_Unit_T; Location : Clang.CX_Source_Location.Source_Location_T) return access Token_T  -- install/include/clang-c/Index.h:4522
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getToken";

  --*
  -- * Determine the kind of the given token.
  --  

   function Get_Token_Kind (Arg_1 : Token_T) return Token_Kind_T  -- install/include/clang-c/Index.h:4528
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getTokenKind";

  --*
  -- * Determine the spelling of the given token.
  -- *
  -- * The spelling of a token is the textual representation of that token, e.g.,
  -- * the text of an identifier or keyword.
  --  

function Get_Token_Spelling
     (Arg_1 : Translation_Unit_T;
      Arg_2 : Token_T)
      return String;

  --*
  -- * Retrieve the source location of the given token.
  --  

   function Get_Token_Location (Arg_1 : Translation_Unit_T; Arg_2 : Token_T) return Clang.CX_Source_Location.Source_Location_T  -- install/include/clang-c/Index.h:4541
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getTokenLocation";

  --*
  -- * Retrieve a source range that covers the given token.
  --  

   function Get_Token_Extent (Arg_1 : Translation_Unit_T; Arg_2 : Token_T) return Clang.CX_Source_Location.Source_Range_T  -- install/include/clang-c/Index.h:4547
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getTokenExtent";

  --*
  -- * Tokenize the source code described by the given range into raw
  -- * lexical tokens.
  -- *
  -- * \param TU the translation unit whose text is being tokenized.
  -- *
  -- * \param Range the source range in which text should be tokenized. All of the
  -- * tokens produced by tokenization will fall within this source range,
  -- *
  -- * \param Tokens this pointer will be set to point to the array of tokens
  -- * that occur within the given source range. The returned pointer must be
  -- * freed with clang_disposeTokens() before the translation unit is destroyed.
  -- *
  -- * \param NumTokens will be set to the number of tokens in the \c *Tokens
  -- * array.
  -- *
  --  

   procedure Tokenize
     (TU : Translation_Unit_T;
      C_Range : Clang.CX_Source_Location.Source_Range_T;
      Tokens : System.Address;
      Num_Tokens : access unsigned)  -- install/include/clang-c/Index.h:4566
   with Import => True, 
        Convention => C, 
        External_Name => "clang_tokenize";

  --*
  -- * Annotate the given set of tokens by providing cursors for each token
  -- * that can be mapped to a specific entity within the abstract syntax tree.
  -- *
  -- * This token-annotation routine is equivalent to invoking
  -- * clang_getCursor() for the source locations of each of the
  -- * tokens. The cursors provided are filtered, so that only those
  -- * cursors that have a direct correspondence to the token are
  -- * accepted. For example, given a function call \c f(x),
  -- * clang_getCursor() would provide the following cursors:
  -- *
  -- *   * when the cursor is over the 'f', a DeclRefExpr cursor referring to 'f'.
  -- *   * when the cursor is over the '(' or the ')', a CallExpr referring to 'f'.
  -- *   * when the cursor is over the 'x', a DeclRefExpr cursor referring to 'x'.
  -- *
  -- * Only the first and last of these cursors will occur within the
  -- * annotate, since the tokens "f" and "x' directly refer to a function
  -- * and a variable, respectively, but the parentheses are just a small
  -- * part of the full syntax of the function call expression, which is
  -- * not provided as an annotation.
  -- *
  -- * \param TU the translation unit that owns the given tokens.
  -- *
  -- * \param Tokens the set of tokens to annotate.
  -- *
  -- * \param NumTokens the number of tokens in \p Tokens.
  -- *
  -- * \param Cursors an array of \p NumTokens cursors, whose contents will be
  -- * replaced with the cursors corresponding to each token.
  --  

   procedure Annotate_Tokens
     (TU : Translation_Unit_T;
      Tokens : access Token_T;
      Num_Tokens : unsigned;
      Cursors : access Cursor_T)  -- install/include/clang-c/Index.h:4599
   with Import => True, 
        Convention => C, 
        External_Name => "clang_annotateTokens";

  --*
  -- * Free the given set of tokens.
  --  

   procedure Dispose_Tokens
     (TU : Translation_Unit_T;
      Tokens : access Token_T;
      Num_Tokens : unsigned)  -- install/include/clang-c/Index.h:4605
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeTokens";

  --*
  -- * @}
  --  

  --*
  -- * \defgroup CINDEX_DEBUG Debugging facilities
  -- *
  -- * These routines are used for testing and debugging, only, and should not
  -- * be relied upon.
  -- *
  -- * @{
  --  

  -- for debug/testing  
function Get_Cursor_Kind_Spelling
     (Kind : Cursor_Kind_T)
      return String;

   procedure Get_Definition_Spelling_And_Extent
     (Arg_1 : Cursor_T;
      Start_Buf : System.Address;
      End_Buf : System.Address;
      Start_Line : access unsigned;
      Start_Column : access unsigned;
      End_Line : access unsigned;
      End_Column : access unsigned)  -- install/include/clang-c/Index.h:4623
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getDefinitionSpellingAndExtent";

   procedure Enable_Stack_Traces  -- install/include/clang-c/Index.h:4626
   with Import => True, 
        Convention => C, 
        External_Name => "clang_enableStackTraces";

   procedure Execute_On_Thread
     (Fn : access procedure (Arg_1 : System.Address);
      User_Data : System.Address;
      Stack_Size : unsigned)  -- install/include/clang-c/Index.h:4627
   with Import => True, 
        Convention => C, 
        External_Name => "clang_executeOnThread";

  --*
  -- * @}
  --  

  --*
  -- * \defgroup CINDEX_CODE_COMPLET Code completion
  -- *
  -- * Code completion involves taking an (incomplete) source file, along with
  -- * knowledge of where the user is actively editing that file, and suggesting
  -- * syntactically- and semantically-valid constructs that the user might want to
  -- * use at that particular point in the source code. These data structures and
  -- * routines provide support for code completion.
  -- *
  -- * @{
  --  

  --*
  -- * A semantic string that describes a code-completion result.
  -- *
  -- * A semantic string that describes the formatting of a code-completion
  -- * result as a single "template" of text that should be inserted into the
  -- * source buffer when a particular code-completion result is selected.
  -- * Each semantic string is made up of some number of "chunks", each of which
  -- * contains some text along with a description of what that text means, e.g.,
  -- * the name of the entity being referenced, whether the text chunk is part of
  -- * the template, or whether it is a "placeholder" that the user should replace
  -- * with actual code,of a specific kind. See \c CXCompletionChunkKind for a
  -- * description of the different kinds of chunks.
  --  

   type Completion_String_T is new System.Address;  -- install/include/clang-c/Index.h:4659

  --*
  -- * A single result of code completion.
  --  

  --*
  --   * The kind of entity that this completion refers to.
  --   *
  --   * The cursor kind will be a macro, keyword, or a declaration (one of the
  --   * *Decl cursor kinds), describing the entity that the completion is
  --   * referring to.
  --   *
  --   * \todo In the future, we would like to provide a full cursor, to allow
  --   * the client to extract additional information from declaration.
  --    

   type Completion_Result_T is record
      CursorKind : aliased Cursor_Kind_T;  -- install/include/clang-c/Index.h:4675
      CompletionString : Completion_String_T;  -- install/include/clang-c/Index.h:4681
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:4682

  --*
  --   * The code-completion string that describes how to insert this
  --   * code-completion result into the editing buffer.
  --    

  --*
  -- * Describes a single piece of text within a code-completion string.
  -- *
  -- * Each "chunk" within a code-completion string (\c CXCompletionString) is
  -- * either a piece of text with a specific "kind" that describes how that text
  -- * should be interpreted by the client or is another completion string.
  --  

   type Completion_Chunk_Kind_T is 
     (Completion_Chunk_Optional,
      Completion_Chunk_Typed_Text,
      Completion_Chunk_Text,
      Completion_Chunk_Placeholder,
      Completion_Chunk_Informative,
      Completion_Chunk_Current_Parameter,
      Completion_Chunk_Left_Paren,
      Completion_Chunk_Right_Paren,
      Completion_Chunk_Left_Bracket,
      Completion_Chunk_Right_Bracket,
      Completion_Chunk_Left_Brace,
      Completion_Chunk_Right_Brace,
      Completion_Chunk_Left_Angle,
      Completion_Chunk_Right_Angle,
      Completion_Chunk_Comma,
      Completion_Chunk_Result_Type,
      Completion_Chunk_Colon,
      Completion_Chunk_Semi_Colon,
      Completion_Chunk_Equal,
      Completion_Chunk_Horizontal_Space,
      Completion_Chunk_Vertical_Space)
   with Convention => C;  -- install/include/clang-c/Index.h:4691

  --*
  --   * A code-completion string that describes "optional" text that
  --   * could be a part of the template (but is not required).
  --   *
  --   * The Optional chunk is the only kind of chunk that has a code-completion
  --   * string for its representation, which is accessible via
  --   * \c clang_getCompletionChunkCompletionString(). The code-completion string
  --   * describes an additional part of the template that is completely optional.
  --   * For example, optional chunks can be used to describe the placeholders for
  --   * arguments that match up with defaulted function parameters, e.g. given:
  --   *
  --   * \code
  --   * void f(int x, float y = 3.14, double z = 2.71828);
  --   * \endcode
  --   *
  --   * The code-completion string for this function would contain:
  --   *   - a TypedText chunk for "f".
  --   *   - a LeftParen chunk for "(".
  --   *   - a Placeholder chunk for "int x"
  --   *   - an Optional chunk containing the remaining defaulted arguments, e.g.,
  --   *       - a Comma chunk for ","
  --   *       - a Placeholder chunk for "float y"
  --   *       - an Optional chunk containing the last defaulted argument:
  --   *           - a Comma chunk for ","
  --   *           - a Placeholder chunk for "double z"
  --   *   - a RightParen chunk for ")"
  --   *
  --   * There are many ways to handle Optional chunks. Two simple approaches are:
  --   *   - Completely ignore optional chunks, in which case the template for the
  --   *     function "f" would only include the first parameter ("int x").
  --   *   - Fully expand all optional chunks, in which case the template for the
  --   *     function "f" would have all of the parameters.
  --    

  --*
  --   * Text that a user would be expected to type to get this
  --   * code-completion result.
  --   *
  --   * There will be exactly one "typed text" chunk in a semantic string, which
  --   * will typically provide the spelling of a keyword or the name of a
  --   * declaration that could be used at the current code point. Clients are
  --   * expected to filter the code-completion results based on the text in this
  --   * chunk.
  --    

  --*
  --   * Text that should be inserted as part of a code-completion result.
  --   *
  --   * A "text" chunk represents text that is part of the template to be
  --   * inserted into user code should this particular code-completion result
  --   * be selected.
  --    

  --*
  --   * Placeholder text that should be replaced by the user.
  --   *
  --   * A "placeholder" chunk marks a place where the user should insert text
  --   * into the code-completion template. For example, placeholders might mark
  --   * the function parameters for a function declaration, to indicate that the
  --   * user should provide arguments for each of those parameters. The actual
  --   * text in a placeholder is a suggestion for the text to display before
  --   * the user replaces the placeholder with real code.
  --    

  --*
  --   * Informative text that should be displayed but never inserted as
  --   * part of the template.
  --   *
  --   * An "informative" chunk contains annotations that can be displayed to
  --   * help the user decide whether a particular code-completion result is the
  --   * right option, but which is not part of the actual template to be inserted
  --   * by code completion.
  --    

  --*
  --   * Text that describes the current parameter when code-completion is
  --   * referring to function call, message send, or template specialization.
  --   *
  --   * A "current parameter" chunk occurs when code-completion is providing
  --   * information about a parameter corresponding to the argument at the
  --   * code-completion point. For example, given a function
  --   *
  --   * \code
  --   * int add(int x, int y);
  --   * \endcode
  --   *
  --   * and the source code \c add(, where the code-completion point is after the
  --   * "(", the code-completion string will contain a "current parameter" chunk
  --   * for "int x", indicating that the current argument will initialize that
  --   * parameter. After typing further, to \c add(17, (where the code-completion
  --   * point is after the ","), the code-completion string will contain a
  --   * "current parameter" chunk to "int y".
  --    

  --*
  --   * A left parenthesis ('('), used to initiate a function call or
  --   * signal the beginning of a function parameter list.
  --    

  --*
  --   * A right parenthesis (')'), used to finish a function call or
  --   * signal the end of a function parameter list.
  --    

  --*
  --   * A left bracket ('[').
  --    

  --*
  --   * A right bracket (']').
  --    

  --*
  --   * A left brace ('{').
  --    

  --*
  --   * A right brace ('}').
  --    

  --*
  --   * A left angle bracket ('<').
  --    

  --*
  --   * A right angle bracket ('>').
  --    

  --*
  --   * A comma separator (',').
  --    

  --*
  --   * Text that specifies the result type of a given result.
  --   *
  --   * This special kind of informative chunk is not meant to be inserted into
  --   * the text buffer. Rather, it is meant to illustrate the type that an
  --   * expression using the given completion string would have.
  --    

  --*
  --   * A colon (':').
  --    

  --*
  --   * A semicolon (';').
  --    

  --*
  --   * An '=' sign.
  --    

  --*
  --   * Horizontal space (' ').
  --    

  --*
  --   * Vertical space ('\\n'), after which it is generally a good idea to
  --   * perform indentation.
  --    

  --*
  -- * Determine the kind of a particular chunk within a completion string.
  -- *
  -- * \param completion_string the completion string to query.
  -- *
  -- * \param chunk_number the 0-based index of the chunk in the completion string.
  -- *
  -- * \returns the kind of the chunk at the index \c chunk_number.
  --  

   function Get_Completion_Chunk_Kind (Completion_String : Completion_String_T; Chunk_Number : unsigned) return Completion_Chunk_Kind_T  -- install/include/clang-c/Index.h:4865
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCompletionChunkKind";

  --*
  -- * Retrieve the text associated with a particular chunk within a
  -- * completion string.
  -- *
  -- * \param completion_string the completion string to query.
  -- *
  -- * \param chunk_number the 0-based index of the chunk in the completion string.
  -- *
  -- * \returns the text associated with the chunk at index \c chunk_number.
  --  

function Get_Completion_Chunk_Text
     (Completion_String : Completion_String_T;
      Chunk_Number      : unsigned)
      return String;

  --*
  -- * Retrieve the completion string associated with a particular chunk
  -- * within a completion string.
  -- *
  -- * \param completion_string the completion string to query.
  -- *
  -- * \param chunk_number the 0-based index of the chunk in the completion string.
  -- *
  -- * \returns the completion string associated with the chunk at index
  -- * \c chunk_number.
  --  

   function Get_Completion_Chunk_Completion_String (Completion_String : Completion_String_T; Chunk_Number : unsigned) return Completion_String_T  -- install/include/clang-c/Index.h:4892
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCompletionChunkCompletionString";

  --*
  -- * Retrieve the number of chunks in the given code-completion string.
  --  

   function Get_Num_Completion_Chunks (Completion_String : Completion_String_T) return unsigned  -- install/include/clang-c/Index.h:4899
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getNumCompletionChunks";

  --*
  -- * Determine the priority of this code completion.
  -- *
  -- * The priority of a code completion indicates how likely it is that this
  -- * particular completion is the completion that the user will select. The
  -- * priority is selected by various internal heuristics.
  -- *
  -- * \param completion_string The completion string to query.
  -- *
  -- * \returns The priority of this completion string. Smaller values indicate
  -- * higher-priority (more likely) completions.
  --  

   function Get_Completion_Priority (Completion_String : Completion_String_T) return unsigned  -- install/include/clang-c/Index.h:4914
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCompletionPriority";

  --*
  -- * Determine the availability of the entity that this code-completion
  -- * string refers to.
  -- *
  -- * \param completion_string The completion string to query.
  -- *
  -- * \returns The availability of the completion string.
  --  

   function Get_Completion_Availability (Completion_String : Completion_String_T) return Availability_Kind_T  -- install/include/clang-c/Index.h:4925
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCompletionAvailability";

  --*
  -- * Retrieve the number of annotations associated with the given
  -- * completion string.
  -- *
  -- * \param completion_string the completion string to query.
  -- *
  -- * \returns the number of annotations associated with the given completion
  -- * string.
  --  

   function Get_Completion_Num_Annotations (Completion_String : Completion_String_T) return unsigned  -- install/include/clang-c/Index.h:4937
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCompletionNumAnnotations";

  --*
  -- * Retrieve the annotation associated with the given completion string.
  -- *
  -- * \param completion_string the completion string to query.
  -- *
  -- * \param annotation_number the 0-based index of the annotation of the
  -- * completion string.
  -- *
  -- * \returns annotation string associated with the completion at index
  -- * \c annotation_number, or a NULL string if that annotation is not available.
  --  

function Get_Completion_Annotation
     (Completion_String : Completion_String_T;
      Annotation_Number : unsigned)
      return String;

  --*
  -- * Retrieve the parent context of the given completion string.
  -- *
  -- * The parent context of a completion string is the semantic parent of
  -- * the declaration (if any) that the code completion represents. For example,
  -- * a code completion for an Objective-C method would have the method's class
  -- * or protocol as its context.
  -- *
  -- * \param completion_string The code completion string whose parent is
  -- * being queried.
  -- *
  -- * \param kind DEPRECATED: always set to CXCursor_NotImplemented if non-NULL.
  -- *
  -- * \returns The name of the completion parent, e.g., "NSObject" if
  -- * the completion string represents a method in the NSObject class.
  --  

function Get_Completion_Parent
     (Completion_String : Completion_String_T;
      Kind              : access Cursor_Kind_T)
      return String;

  --*
  -- * Retrieve the brief documentation comment attached to the declaration
  -- * that corresponds to the given completion string.
  --  

function Get_Completion_Brief_Comment
     (Completion_String : Completion_String_T)
      return String;

  --*
  -- * Retrieve a completion string for an arbitrary declaration or macro
  -- * definition cursor.
  -- *
  -- * \param cursor The cursor to query.
  -- *
  -- * \returns A non-context-sensitive completion string for declaration and macro
  -- * definition cursors, or NULL for other kinds of cursors.
  --  

   function Get_Cursor_Completion_String (Cursor : Cursor_T) return Completion_String_T  -- install/include/clang-c/Index.h:4989
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCursorCompletionString";

  --*
  -- * Contains the results of code-completion.
  -- *
  -- * This data structure contains the results of code completion, as
  -- * produced by \c clang_codeCompleteAt(). Its contents must be freed by
  -- * \c clang_disposeCodeCompleteResults.
  --  

  --*
  --   * The code-completion results.
  --    

   type Code_Complete_Results_T is record
      Results : access Completion_Result_T;  -- install/include/clang-c/Index.h:5002
      NumResults : aliased unsigned;  -- install/include/clang-c/Index.h:5008
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5009

  --*
  --   * The number of code-completion results stored in the
  --   * \c Results array.
  --    

  --*
  -- * Retrieve the number of fix-its for the given completion index.
  -- *
  -- * Calling this makes sense only if CXCodeComplete_IncludeCompletionsWithFixIts
  -- * option was set.
  -- *
  -- * \param results The structure keeping all completion results
  -- *
  -- * \param completion_index The index of the completion
  -- *
  -- * \return The number of fix-its which must be applied before the completion at
  -- * completion_index can be applied
  --  

   function Get_Completion_Num_Fix_Its (Results : access Code_Complete_Results_T; Completion_Index : unsigned) return unsigned  -- install/include/clang-c/Index.h:5025
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getCompletionNumFixIts";

  --*
  -- * Fix-its that *must* be applied before inserting the text for the
  -- * corresponding completion.
  -- *
  -- * By default, clang_codeCompleteAt() only returns completions with empty
  -- * fix-its. Extra completions with non-empty fix-its should be explicitly
  -- * requested by setting CXCodeComplete_IncludeCompletionsWithFixIts.
  -- *
  -- * For the clients to be able to compute position of the cursor after applying
  -- * fix-its, the following conditions are guaranteed to hold for
  -- * replacement_range of the stored fix-its:
  -- *  - Ranges in the fix-its are guaranteed to never contain the completion
  -- *  point (or identifier under completion point, if any) inside them, except
  -- *  at the start or at the end of the range.
  -- *  - If a fix-it range starts or ends with completion point (or starts or
  -- *  ends after the identifier under completion point), it will contain at
  -- *  least one character. It allows to unambiguously recompute completion
  -- *  point after applying the fix-it.
  -- *
  -- * The intuition is that provided fix-its change code around the identifier we
  -- * complete, but are not allowed to touch the identifier itself or the
  -- * completion point. One example of completions with corrections are the ones
  -- * replacing '.' with '->' and vice versa:
  -- *
  -- * std::unique_ptr<std::vector<int>> vec_ptr;
  -- * In 'vec_ptr.^', one of the completions is 'push_back', it requires
  -- * replacing '.' with '->'.
  -- * In 'vec_ptr->^', one of the completions is 'release', it requires
  -- * replacing '->' with '.'.
  -- *
  -- * \param results The structure keeping all completion results
  -- *
  -- * \param completion_index The index of the completion
  -- *
  -- * \param fixit_index The index of the fix-it for the completion at
  -- * completion_index
  -- *
  -- * \param replacement_range The fix-it range that must be replaced before the
  -- * completion at completion_index can be applied
  -- *
  -- * \returns The fix-it string that must replace the code at replacement_range
  -- * before the completion at completion_index can be applied
  --  

function Get_Completion_Fix_It
     (Results           : access Code_Complete_Results_T;
      Completion_Index  : unsigned;
      Fixit_Index       : unsigned;
      Replacement_Range : access Clang.CX_Source_Location.Source_Range_T)
      return String;

  --*
  -- * Flags that can be passed to \c clang_codeCompleteAt() to
  -- * modify its behavior.
  -- *
  -- * The enumerators in this enumeration can be bitwise-OR'd together to
  -- * provide multiple options to \c clang_codeCompleteAt().
  --  

   subtype Code_Complete_Flags_T is unsigned;
   Code_Complete_Include_Macros : constant Code_Complete_Flags_T := 1;
   Code_Complete_Include_Code_Patterns : constant Code_Complete_Flags_T := 2;
   Code_Complete_Include_Brief_Comments : constant Code_Complete_Flags_T := 4;
   Code_Complete_Skip_Preamble : constant Code_Complete_Flags_T := 8;
   Code_Complete_Include_Completions_With_Fix_Its : constant Code_Complete_Flags_T := 16;  -- install/include/clang-c/Index.h:5082

  --*
  --   * Whether to include macros within the set of code
  --   * completions returned.
  --    

  --*
  --   * Whether to include code patterns for language constructs
  --   * within the set of code completions, e.g., for loops.
  --    

  --*
  --   * Whether to include brief documentation within the set of code
  --   * completions returned.
  --    

  --*
  --   * Whether to speed up completion by omitting top- or namespace-level entities
  --   * defined in the preamble. There's no guarantee any particular entity is
  --   * omitted. This may be useful if the headers are indexed externally.
  --    

  --*
  --   * Whether to include completions with small
  --   * fix-its, e.g. change '.' to '->' on member access, etc.
  --    

  --*
  -- * Bits that represent the context under which completion is occurring.
  -- *
  -- * The enumerators in this enumeration may be bitwise-OR'd together if multiple
  -- * contexts are occurring simultaneously.
  --  

   subtype Completion_Context_T is unsigned;
   Completion_Context_Unexposed : constant Completion_Context_T := 0;
   Completion_Context_Any_Type : constant Completion_Context_T := 1;
   Completion_Context_Any_Value : constant Completion_Context_T := 2;
   Completion_Context_Obj_C_Object_Value : constant Completion_Context_T := 4;
   Completion_Context_Obj_C_Selector_Value : constant Completion_Context_T := 8;
   Completion_Context_CXX_Class_Type_Value : constant Completion_Context_T := 16;
   Completion_Context_Dot_Member_Access : constant Completion_Context_T := 32;
   Completion_Context_Arrow_Member_Access : constant Completion_Context_T := 64;
   Completion_Context_Obj_C_Property_Access : constant Completion_Context_T := 128;
   Completion_Context_Enum_Tag : constant Completion_Context_T := 256;
   Completion_Context_Union_Tag : constant Completion_Context_T := 512;
   Completion_Context_Struct_Tag : constant Completion_Context_T := 1024;
   Completion_Context_Class_Tag : constant Completion_Context_T := 2048;
   Completion_Context_Namespace : constant Completion_Context_T := 4096;
   Completion_Context_Nested_Name_Specifier : constant Completion_Context_T := 8192;
   Completion_Context_Obj_C_Interface : constant Completion_Context_T := 16384;
   Completion_Context_Obj_C_Protocol : constant Completion_Context_T := 32768;
   Completion_Context_Obj_C_Category : constant Completion_Context_T := 65536;
   Completion_Context_Obj_C_Instance_Message : constant Completion_Context_T := 131072;
   Completion_Context_Obj_C_Class_Message : constant Completion_Context_T := 262144;
   Completion_Context_Obj_C_Selector_Name : constant Completion_Context_T := 524288;
   Completion_Context_Macro_Name : constant Completion_Context_T := 1048576;
   Completion_Context_Natural_Language : constant Completion_Context_T := 2097152;
   Completion_Context_Included_File : constant Completion_Context_T := 4194304;
   Completion_Context_Unknown : constant Completion_Context_T := 8388607;  -- install/include/clang-c/Index.h:5121

  --*
  --   * The context for completions is unexposed, as only Clang results
  --   * should be included. (This is equivalent to having no context bits set.)
  --    

  --*
  --   * Completions for any possible type should be included in the results.
  --    

  --*
  --   * Completions for any possible value (variables, function calls, etc.)
  --   * should be included in the results.
  --    

  --*
  --   * Completions for values that resolve to an Objective-C object should
  --   * be included in the results.
  --    

  --*
  --   * Completions for values that resolve to an Objective-C selector
  --   * should be included in the results.
  --    

  --*
  --   * Completions for values that resolve to a C++ class type should be
  --   * included in the results.
  --    

  --*
  --   * Completions for fields of the member being accessed using the dot
  --   * operator should be included in the results.
  --    

  --*
  --   * Completions for fields of the member being accessed using the arrow
  --   * operator should be included in the results.
  --    

  --*
  --   * Completions for properties of the Objective-C object being accessed
  --   * using the dot operator should be included in the results.
  --    

  --*
  --   * Completions for enum tags should be included in the results.
  --    

  --*
  --   * Completions for union tags should be included in the results.
  --    

  --*
  --   * Completions for struct tags should be included in the results.
  --    

  --*
  --   * Completions for C++ class names should be included in the results.
  --    

  --*
  --   * Completions for C++ namespaces and namespace aliases should be
  --   * included in the results.
  --    

  --*
  --   * Completions for C++ nested name specifiers should be included in
  --   * the results.
  --    

  --*
  --   * Completions for Objective-C interfaces (classes) should be included
  --   * in the results.
  --    

  --*
  --   * Completions for Objective-C protocols should be included in
  --   * the results.
  --    

  --*
  --   * Completions for Objective-C categories should be included in
  --   * the results.
  --    

  --*
  --   * Completions for Objective-C instance messages should be included
  --   * in the results.
  --    

  --*
  --   * Completions for Objective-C class messages should be included in
  --   * the results.
  --    

  --*
  --   * Completions for Objective-C selector names should be included in
  --   * the results.
  --    

  --*
  --   * Completions for preprocessor macro names should be included in
  --   * the results.
  --    

  --*
  --   * Natural language completions should be included in the results.
  --    

  --*
  --   * #include file completions should be included in the results.
  --    

  --*
  --   * The current context is unknown, so set all contexts.
  --    

  --*
  -- * Returns a default set of code-completion options that can be
  -- * passed to\c clang_codeCompleteAt().
  --  

   function Default_Code_Complete_Options return unsigned  -- install/include/clang-c/Index.h:5255
   with Import => True, 
        Convention => C, 
        External_Name => "clang_defaultCodeCompleteOptions";

  --*
  -- * Perform code completion at a given location in a translation unit.
  -- *
  -- * This function performs code completion at a particular file, line, and
  -- * column within source code, providing results that suggest potential
  -- * code snippets based on the context of the completion. The basic model
  -- * for code completion is that Clang will parse a complete source file,
  -- * performing syntax checking up to the location where code-completion has
  -- * been requested. At that point, a special code-completion token is passed
  -- * to the parser, which recognizes this token and determines, based on the
  -- * current location in the C/Objective-C/C++ grammar and the state of
  -- * semantic analysis, what completions to provide. These completions are
  -- * returned via a new \c CXCodeCompleteResults structure.
  -- *
  -- * Code completion itself is meant to be triggered by the client when the
  -- * user types punctuation characters or whitespace, at which point the
  -- * code-completion location will coincide with the cursor. For example, if \c p
  -- * is a pointer, code-completion might be triggered after the "-" and then
  -- * after the ">" in \c p->. When the code-completion location is after the ">",
  -- * the completion results will provide, e.g., the members of the struct that
  -- * "p" points to. The client is responsible for placing the cursor at the
  -- * beginning of the token currently being typed, then filtering the results
  -- * based on the contents of the token. For example, when code-completing for
  -- * the expression \c p->get, the client should provide the location just after
  -- * the ">" (e.g., pointing at the "g") to this code-completion hook. Then, the
  -- * client can filter the results based on the current token text ("get"), only
  -- * showing those results that start with "get". The intent of this interface
  -- * is to separate the relatively high-latency acquisition of code-completion
  -- * results from the filtering of results on a per-character basis, which must
  -- * have a lower latency.
  -- *
  -- * \param TU The translation unit in which code-completion should
  -- * occur. The source files for this translation unit need not be
  -- * completely up-to-date (and the contents of those source files may
  -- * be overridden via \p unsaved_files). Cursors referring into the
  -- * translation unit may be invalidated by this invocation.
  -- *
  -- * \param complete_filename The name of the source file where code
  -- * completion should be performed. This filename may be any file
  -- * included in the translation unit.
  -- *
  -- * \param complete_line The line at which code-completion should occur.
  -- *
  -- * \param complete_column The column at which code-completion should occur.
  -- * Note that the column should point just after the syntactic construct that
  -- * initiated code completion, and not in the middle of a lexical token.
  -- *
  -- * \param unsaved_files the Files that have not yet been saved to disk
  -- * but may be required for parsing or code completion, including the
  -- * contents of those files.  The contents and name of these files (as
  -- * specified by CXUnsavedFile) are copied when necessary, so the
  -- * client only needs to guarantee their validity until the call to
  -- * this function returns.
  -- *
  -- * \param num_unsaved_files The number of unsaved file entries in \p
  -- * unsaved_files.
  -- *
  -- * \param options Extra options that control the behavior of code
  -- * completion, expressed as a bitwise OR of the enumerators of the
  -- * CXCodeComplete_Flags enumeration. The
  -- * \c clang_defaultCodeCompleteOptions() function returns a default set
  -- * of code-completion options.
  -- *
  -- * \returns If successful, a new \c CXCodeCompleteResults structure
  -- * containing code-completion results, which should eventually be
  -- * freed with \c clang_disposeCodeCompleteResults(). If code
  -- * completion fails, returns NULL.
  --  

function Code_Complete_At
     (TU                : Translation_Unit_T;
      Complete_Filename : String;
      Complete_Line     : unsigned;
      Complete_Column   : unsigned;
      Unsaved_Files     : access Unsaved_File_T;
      Num_Unsaved_Files : unsigned;
      Options           : unsigned)
      return access Code_Complete_Results_T;

  --*
  -- * Sort the code-completion results in case-insensitive alphabetical
  -- * order.
  -- *
  -- * \param Results The set of results to sort.
  -- * \param NumResults The number of results in \p Results.
  --  

   procedure Sort_Code_Completion_Results (Results : access Completion_Result_T; Num_Results : unsigned)  -- install/include/clang-c/Index.h:5340
   with Import => True, 
        Convention => C, 
        External_Name => "clang_sortCodeCompletionResults";

  --*
  -- * Free the given set of code-completion results.
  --  

   procedure Dispose_Code_Complete_Results (Results : access Code_Complete_Results_T)  -- install/include/clang-c/Index.h:5347
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeCodeCompleteResults";

  --*
  -- * Determine the number of diagnostics produced prior to the
  -- * location where code completion was performed.
  --  

   function Code_Complete_Get_Num_Diagnostics (Results : access Code_Complete_Results_T) return unsigned  -- install/include/clang-c/Index.h:5354
   with Import => True, 
        Convention => C, 
        External_Name => "clang_codeCompleteGetNumDiagnostics";

  --*
  -- * Retrieve a diagnostic associated with the given code completion.
  -- *
  -- * \param Results the code completion results to query.
  -- * \param Index the zero-based diagnostic number to retrieve.
  -- *
  -- * \returns the requested diagnostic. This diagnostic must be freed
  -- * via a call to \c clang_disposeDiagnostic().
  --  

   function Code_Complete_Get_Diagnostic (Results : access Code_Complete_Results_T; Index : unsigned) return Clang.CX_Diagnostic.Diagnostic_T  -- install/include/clang-c/Index.h:5366
   with Import => True, 
        Convention => C, 
        External_Name => "clang_codeCompleteGetDiagnostic";

  --*
  -- * Determines what completions are appropriate for the context
  -- * the given code completion.
  -- *
  -- * \param Results the code completion results to query
  -- *
  -- * \returns the kinds of completions that are appropriate for use
  -- * along with the given code completion results.
  --  

   function Code_Complete_Get_Contexts (Results : access Code_Complete_Results_T) return Extensions.unsigned_long_long  -- install/include/clang-c/Index.h:5380
   with Import => True, 
        Convention => C, 
        External_Name => "clang_codeCompleteGetContexts";

  --*
  -- * Returns the cursor kind for the container for the current code
  -- * completion context. The container is only guaranteed to be set for
  -- * contexts where a container exists (i.e. member accesses or Objective-C
  -- * message sends); if there is not a container, this function will return
  -- * CXCursor_InvalidCode.
  -- *
  -- * \param Results the code completion results to query
  -- *
  -- * \param IsIncomplete on return, this value will be false if Clang has complete
  -- * information about the container. If Clang does not have complete
  -- * information, this value will be true.
  -- *
  -- * \returns the container kind, or CXCursor_InvalidCode if there is not a
  -- * container
  --  

   function Code_Complete_Get_Container_Kind (Results : access Code_Complete_Results_T; Is_Incomplete : access unsigned) return Cursor_Kind_T  -- install/include/clang-c/Index.h:5400
   with Import => True, 
        Convention => C, 
        External_Name => "clang_codeCompleteGetContainerKind";

  --*
  -- * Returns the USR for the container for the current code completion
  -- * context. If there is not a container for the current context, this
  -- * function will return the empty string.
  -- *
  -- * \param Results the code completion results to query
  -- *
  -- * \returns the USR for the container
  --  

function Code_Complete_Get_Container_USR
     (Results : access Code_Complete_Results_T)
      return String;

  --*
  -- * Returns the currently-entered selector for an Objective-C message
  -- * send, formatted like "initWithFoo:bar:". Only guaranteed to return a
  -- * non-empty string for CXCompletionContext_ObjCInstanceMessage and
  -- * CXCompletionContext_ObjCClassMessage.
  -- *
  -- * \param Results the code completion results to query
  -- *
  -- * \returns the selector (or partial selector) that has been entered thus far
  -- * for an Objective-C message send.
  --  

function Code_Complete_Get_Obj_C_Selector
     (Results : access Code_Complete_Results_T)
      return String;

  --*
  -- * @}
  --  

  --*
  -- * \defgroup CINDEX_MISC Miscellaneous utility functions
  -- *
  -- * @{
  --  

  --*
  -- * Return a version string, suitable for showing to a user, but not
  -- *        intended to be parsed (the format is not guaranteed to be stable).
  --  

function Get_Clang_Version
      return String;

  --*
  -- * Enable/disable crash recovery.
  -- *
  -- * \param isEnabled Flag to indicate if crash recovery is enabled.  A non-zero
  -- *        value enables crash recovery, while 0 disables it.
  --  

   procedure Toggle_Crash_Recovery (Is_Enabled : unsigned)  -- install/include/clang-c/Index.h:5451
   with Import => True, 
        Convention => C, 
        External_Name => "clang_toggleCrashRecovery";

  --*
  -- * Visitor invoked for each file in a translation unit
  -- *        (used with clang_getInclusions()).
  -- *
  -- * This visitor function will be invoked by clang_getInclusions() for each
  -- * file included (either at the top-level or by \#include directives) within
  -- * a translation unit.  The first argument is the file being included, and
  -- * the second and third arguments provide the inclusion stack.  The
  -- * array is sorted in order of immediate inclusion.  For example,
  -- * the first element refers to the location that included 'included_file'.
  --  

   type Inclusion_Visitor_T is access procedure
        (Arg_1 : Clang.CX_File.File_T;
         Arg_2 : access Clang.CX_Source_Location.Source_Location_T;
         Arg_3 : unsigned;
         Arg_4 : Client_Data_T)
   with Convention => C;  -- install/include/clang-c/Index.h:5464

  --*
  -- * Visit the set of preprocessor inclusions in a translation unit.
  -- *   The visitor function is called with the provided data for every included
  -- *   file.  This does not include headers included by the PCH file (unless one
  -- *   is inspecting the inclusions in the PCH file itself).
  --  

   procedure Get_Inclusions
     (Tu : Translation_Unit_T;
      Visitor : Inclusion_Visitor_T;
      Client_Data : Client_Data_T)  -- install/include/clang-c/Index.h:5475
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getInclusions";

   subtype Eval_Result_Kind_T is unsigned;
   Eval_Int : constant Eval_Result_Kind_T := 1;
   Eval_Float : constant Eval_Result_Kind_T := 2;
   Eval_Obj_C_Str_Literal : constant Eval_Result_Kind_T := 3;
   Eval_Str_Literal : constant Eval_Result_Kind_T := 4;
   Eval_CF_Str : constant Eval_Result_Kind_T := 5;
   Eval_Other : constant Eval_Result_Kind_T := 6;
   Eval_Un_Exposed : constant Eval_Result_Kind_T := 0;  -- install/include/clang-c/Index.h:5489

  --*
  -- * Evaluation result of a cursor
  --  

   type Eval_Result_T is new System.Address;  -- install/include/clang-c/Index.h:5494

  --*
  -- * If cursor is a statement declaration tries to evaluate the
  -- * statement and if its variable, tries to evaluate its initializer,
  -- * into its corresponding type.
  -- * If it's an expression, tries to evaluate the expression.
  --  

   function Cursor_Evaluate (C : Cursor_T) return Eval_Result_T  -- install/include/clang-c/Index.h:5502
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_Evaluate";

  --*
  -- * Returns the kind of the evaluated result.
  --  

   function Eval_Result_Get_Kind (E : Eval_Result_T) return Eval_Result_Kind_T  -- install/include/clang-c/Index.h:5507
   with Import => True, 
        Convention => C, 
        External_Name => "clang_EvalResult_getKind";

  --*
  -- * Returns the evaluation result as integer if the
  -- * kind is Int.
  --  

   function Eval_Result_Get_As_Int (E : Eval_Result_T) return int  -- install/include/clang-c/Index.h:5513
   with Import => True, 
        Convention => C, 
        External_Name => "clang_EvalResult_getAsInt";

  --*
  -- * Returns the evaluation result as a long long integer if the
  -- * kind is Int. This prevents overflows that may happen if the result is
  -- * returned with clang_EvalResult_getAsInt.
  --  

   function Eval_Result_Get_As_Long_Long (E : Eval_Result_T) return Long_Long_Integer  -- install/include/clang-c/Index.h:5520
   with Import => True, 
        Convention => C, 
        External_Name => "clang_EvalResult_getAsLongLong";

  --*
  -- * Returns a non-zero value if the kind is Int and the evaluation
  -- * result resulted in an unsigned integer.
  --  

function Eval_Result_Is_Unsigned_Int
     (E : Eval_Result_T)
      return Boolean;

  --*
  -- * Returns the evaluation result as an unsigned integer if
  -- * the kind is Int and clang_EvalResult_isUnsignedInt is non-zero.
  --  

   function Eval_Result_Get_As_Unsigned (E : Eval_Result_T) return Extensions.unsigned_long_long  -- install/include/clang-c/Index.h:5533
   with Import => True, 
        Convention => C, 
        External_Name => "clang_EvalResult_getAsUnsigned";

  --*
  -- * Returns the evaluation result as double if the
  -- * kind is double.
  --  

   function Eval_Result_Get_As_Double (E : Eval_Result_T) return double  -- install/include/clang-c/Index.h:5539
   with Import => True, 
        Convention => C, 
        External_Name => "clang_EvalResult_getAsDouble";

  --*
  -- * Returns the evaluation result as a constant string if the
  -- * kind is other than Int or float. User must not free this pointer,
  -- * instead call clang_EvalResult_dispose on the CXEvalResult returned
  -- * by clang_Cursor_Evaluate.
  --  

function Eval_Result_Get_As_Str
     (E : Eval_Result_T)
      return String;

  --*
  -- * Disposes the created Eval memory.
  --  

   procedure Eval_Result_Dispose (E : Eval_Result_T)  -- install/include/clang-c/Index.h:5552
   with Import => True, 
        Convention => C, 
        External_Name => "clang_EvalResult_dispose";

  --*
  -- * @}
  --  

  --* \defgroup CINDEX_REMAPPING Remapping functions
  -- *
  -- * @{
  --  

  --*
  -- * A remapping of original source files and their translated files.
  --  

   type Remapping_T is new System.Address;  -- install/include/clang-c/Index.h:5565

  --*
  -- * Retrieve a remapping.
  -- *
  -- * \param path the path that contains metadata about remappings.
  -- *
  -- * \returns the requested remapping. This remapping must be freed
  -- * via a call to \c clang_remap_dispose(). Can return NULL if an error occurred.
  --  

function Get_Remappings
     (Path : String)
      return Remapping_T;

  --*
  -- * Retrieve a remapping.
  -- *
  -- * \param filePaths pointer to an array of file paths containing remapping info.
  -- *
  -- * \param numFiles number of file paths.
  -- *
  -- * \returns the requested remapping. This remapping must be freed
  -- * via a call to \c clang_remap_dispose(). Can return NULL if an error occurred.
  --  

   function Get_Remappings_From_File_List (File_Paths : System.Address; Num_Files : unsigned) return Remapping_T  -- install/include/clang-c/Index.h:5588
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getRemappingsFromFileList";

  --*
  -- * Determine the number of remappings.
  --  

   function Remap_Get_Num_Files (Arg_1 : Remapping_T) return unsigned  -- install/include/clang-c/Index.h:5594
   with Import => True, 
        Convention => C, 
        External_Name => "clang_remap_getNumFiles";

  --*
  -- * Get the original and the associated filename from the remapping.
  -- *
  -- * \param original If non-NULL, will be set to the original filename.
  -- *
  -- * \param transformed If non-NULL, will be set to the filename that the original
  -- * is associated with.
  --  

   procedure Remap_Get_Filenames
     (Arg_1 : Remapping_T;
      Index : unsigned;
      Original : access Clang.CX_String.String_T;
      Transformed : access Clang.CX_String.String_T)  -- install/include/clang-c/Index.h:5604
   with Import => True, 
        Convention => C, 
        External_Name => "clang_remap_getFilenames";

  --*
  -- * Dispose the remapping.
  --  

   procedure Remap_Dispose (Arg_1 : Remapping_T)  -- install/include/clang-c/Index.h:5611
   with Import => True, 
        Convention => C, 
        External_Name => "clang_remap_dispose";

  --*
  -- * @}
  --  

  --* \defgroup CINDEX_HIGH Higher level API functions
  -- *
  -- * @{
  --  

   type Visitor_Result_T is 
     (Visit_Break,
      Visit_Continue)
   with Convention => C;  -- install/include/clang-c/Index.h:5622

   type Cursor_And_Range_Visitor_T is record
      context : System.Address;  -- install/include/clang-c/Index.h:5625
      visit : access function
           (Arg_1 : System.Address;
            Arg_2 : Cursor_T;
            Arg_3 : Clang.CX_Source_Location.Source_Range_T) return Visitor_Result_T;  -- install/include/clang-c/Index.h:5626
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5624

  --*
  --   * Function returned successfully.
  --    

  --*
  --   * One of the parameters was invalid for the function.
  --    

  --*
  --   * The function was terminated by a callback (e.g. it returned
  --   * CXVisit_Break)
  --    

   type Result_T is 
     (Result_Success,
      Result_Invalid,
      Result_Visit_Break)
   with Convention => C;  -- install/include/clang-c/Index.h:5644

  --*
  -- * Find references of a declaration in a specific file.
  -- *
  -- * \param cursor pointing to a declaration or a reference of one.
  -- *
  -- * \param file to search for references.
  -- *
  -- * \param visitor callback that will receive pairs of CXCursor/CXSourceRange for
  -- * each reference found.
  -- * The CXSourceRange will point inside the file; if the reference is inside
  -- * a macro (and not a macro argument) the CXSourceRange will be invalid.
  -- *
  -- * \returns one of the CXResult enumerators.
  --  

   function Find_References_In_File
     (Cursor : Cursor_T;
      File : Clang.CX_File.File_T;
      Visitor : Cursor_And_Range_Visitor_T) return Result_T  -- install/include/clang-c/Index.h:5660
   with Import => True, 
        Convention => C, 
        External_Name => "clang_findReferencesInFile";

  --*
  -- * Find #import/#include directives in a specific file.
  -- *
  -- * \param TU translation unit containing the file to query.
  -- *
  -- * \param file to search for #import/#include directives.
  -- *
  -- * \param visitor callback that will receive pairs of CXCursor/CXSourceRange for
  -- * each directive found.
  -- *
  -- * \returns one of the CXResult enumerators.
  --  

   function Find_Includes_In_File
     (TU : Translation_Unit_T;
      File : Clang.CX_File.File_T;
      Visitor : Cursor_And_Range_Visitor_T) return Result_T  -- install/include/clang-c/Index.h:5675
   with Import => True, 
        Convention => C, 
        External_Name => "clang_findIncludesInFile";

  --*
  -- * The client's data object that is associated with a CXFile.
  --  

   type Idx_Client_File_T is new System.Address;  -- install/include/clang-c/Index.h:5698

  --*
  -- * The client's data object that is associated with a semantic entity.
  --  

   type Idx_Client_Entity_T is new System.Address;  -- install/include/clang-c/Index.h:5703

  --*
  -- * The client's data object that is associated with a semantic container
  -- * of entities.
  --  

   type Idx_Client_Container_T is new System.Address;  -- install/include/clang-c/Index.h:5709

  --*
  -- * The client's data object that is associated with an AST file (PCH
  -- * or module).
  --  

   type Idx_Client_AST_File_T is new System.Address;  -- install/include/clang-c/Index.h:5715

  --*
  -- * Source location passed to index callbacks.
  --  

   type Idx_Loc_T is record
      ptr_data : anon_array1331;  -- install/include/clang-c/Index.h:5721
      int_data : aliased unsigned;  -- install/include/clang-c/Index.h:5722
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5723

  --*
  -- * Data for ppIncludedFile callback.
  --  

  --*
  --   * Location of '#' in the \#include/\#import directive.
  --    

   type Idx_Included_File_Info_T is record
      hashLoc : aliased Idx_Loc_T;  -- install/include/clang-c/Index.h:5732
      filename : Interfaces.C.Strings.chars_ptr;  -- install/include/clang-c/Index.h:5736
      file : Clang.CX_File.File_T;  -- install/include/clang-c/Index.h:5740
      isImport : aliased int;  -- install/include/clang-c/Index.h:5741
      isAngled : aliased int;  -- install/include/clang-c/Index.h:5742
      isModuleImport : aliased int;  -- install/include/clang-c/Index.h:5747
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5748

  --*
  --   * Filename as written in the \#include/\#import directive.
  --    

  --*
  --   * The actual file that the \#include/\#import directive resolved to.
  --    

  --*
  --   * Non-zero if the directive was automatically turned into a module
  --   * import.
  --    

  --*
  -- * Data for IndexerCallbacks#importedASTFile.
  --  

  --*
  --   * Top level AST file containing the imported PCH, module or submodule.
  --    

   type Idx_Imported_AST_File_Info_T is record
      file : Clang.CX_File.File_T;  -- install/include/clang-c/Index.h:5757
      module : Module_T;  -- install/include/clang-c/Index.h:5761
      loc : aliased Idx_Loc_T;  -- install/include/clang-c/Index.h:5765
      isImplicit : aliased int;  -- install/include/clang-c/Index.h:5770
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5772

  --*
  --   * The imported module or NULL if the AST file is a PCH.
  --    

  --*
  --   * Location where the file is imported. Applicable only for modules.
  --    

  --*
  --   * Non-zero if an inclusion directive was automatically turned into
  --   * a module import. Applicable only for modules.
  --    

   type Idx_Entity_Kind_T is 
     (Idx_Entity_Unexposed,
      Idx_Entity_Typedef,
      Idx_Entity_Function,
      Idx_Entity_Variable,
      Idx_Entity_Field,
      Idx_Entity_Enum_Constant,
      Idx_Entity_Obj_C_Class,
      Idx_Entity_Obj_C_Protocol,
      Idx_Entity_Obj_C_Category,
      Idx_Entity_Obj_C_Instance_Method,
      Idx_Entity_Obj_C_Class_Method,
      Idx_Entity_Obj_C_Property,
      Idx_Entity_Obj_C_Ivar,
      Idx_Entity_Enum,
      Idx_Entity_Struct,
      Idx_Entity_Union,
      Idx_Entity_CXX_Class,
      Idx_Entity_CXX_Namespace,
      Idx_Entity_CXX_Namespace_Alias,
      Idx_Entity_CXX_Static_Variable,
      Idx_Entity_CXX_Static_Method,
      Idx_Entity_CXX_Instance_Method,
      Idx_Entity_CXX_Constructor,
      Idx_Entity_CXX_Destructor,
      Idx_Entity_CXX_Conversion_Function,
      Idx_Entity_CXX_Type_Alias,
      Idx_Entity_CXX_Interface,
      Idx_Entity_CXX_Concept)
   with Convention => C;  -- install/include/clang-c/Index.h:5808

   type Idx_Entity_Language_T is 
     (Idx_Entity_Lang_None,
      Idx_Entity_Lang_C,
      Idx_Entity_Lang_Obj_C,
      Idx_Entity_Lang_CXX,
      Idx_Entity_Lang_Swift)
   with Convention => C;  -- install/include/clang-c/Index.h:5816

  --*
  -- * Extra C++ template information for an entity. This can apply to:
  -- * CXIdxEntity_Function
  -- * CXIdxEntity_CXXClass
  -- * CXIdxEntity_CXXStaticMethod
  -- * CXIdxEntity_CXXInstanceMethod
  -- * CXIdxEntity_CXXConstructor
  -- * CXIdxEntity_CXXConversionFunction
  -- * CXIdxEntity_CXXTypeAlias
  --  

   type Idx_Entity_CXX_Template_Kind_T is 
     (Idx_Entity_Non_Template,
      Idx_Entity_Template,
      Idx_Entity_Template_Partial_Specialization,
      Idx_Entity_Template_Specialization)
   with Convention => C;  -- install/include/clang-c/Index.h:5833

   type Idx_Attr_Kind_T is 
     (Idx_Attr_Unexposed,
      Idx_Attr_IB_Action,
      Idx_Attr_IB_Outlet,
      Idx_Attr_IB_Outlet_Collection)
   with Convention => C;  -- install/include/clang-c/Index.h:5840

   type Idx_Attr_Info_T is record
      kind : aliased Idx_Attr_Kind_T;  -- install/include/clang-c/Index.h:5843
      cursor : aliased Cursor_T;  -- install/include/clang-c/Index.h:5844
      loc : aliased Idx_Loc_T;  -- install/include/clang-c/Index.h:5845
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5846

   type Idx_Entity_Info_T is record
      kind : aliased Idx_Entity_Kind_T;  -- install/include/clang-c/Index.h:5849
      templateKind : aliased Idx_Entity_CXX_Template_Kind_T;  -- install/include/clang-c/Index.h:5850
      lang : aliased Idx_Entity_Language_T;  -- install/include/clang-c/Index.h:5851
      name : Interfaces.C.Strings.chars_ptr;  -- install/include/clang-c/Index.h:5852
      USR : Interfaces.C.Strings.chars_ptr;  -- install/include/clang-c/Index.h:5853
      cursor : aliased Cursor_T;  -- install/include/clang-c/Index.h:5854
      attributes : System.Address;  -- install/include/clang-c/Index.h:5855
      numAttributes : aliased unsigned;  -- install/include/clang-c/Index.h:5856
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5857

   type Idx_Container_Info_T is record
      cursor : aliased Cursor_T;  -- install/include/clang-c/Index.h:5860
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5861

   type Idx_IB_Outlet_Collection_Attr_Info_T is record
      attrInfo : access constant Idx_Attr_Info_T;  -- install/include/clang-c/Index.h:5864
      objcClass : access constant Idx_Entity_Info_T;  -- install/include/clang-c/Index.h:5865
      classCursor : aliased Cursor_T;  -- install/include/clang-c/Index.h:5866
      classLoc : aliased Idx_Loc_T;  -- install/include/clang-c/Index.h:5867
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5868

   subtype Idx_Decl_Info_Flags_T is unsigned;
   Idx_Decl_Flag_Skipped : constant Idx_Decl_Info_Flags_T := 1;  -- install/include/clang-c/Index.h:5870

   type Idx_Decl_Info_T is record
      entityInfo : access constant Idx_Entity_Info_T;  -- install/include/clang-c/Index.h:5873
      cursor : aliased Cursor_T;  -- install/include/clang-c/Index.h:5874
      loc : aliased Idx_Loc_T;  -- install/include/clang-c/Index.h:5875
      semanticContainer : access constant Idx_Container_Info_T;  -- install/include/clang-c/Index.h:5876
      lexicalContainer : access constant Idx_Container_Info_T;  -- install/include/clang-c/Index.h:5881
      isRedeclaration : aliased int;  -- install/include/clang-c/Index.h:5882
      isDefinition : aliased int;  -- install/include/clang-c/Index.h:5883
      isContainer : aliased int;  -- install/include/clang-c/Index.h:5884
      declAsContainer : access constant Idx_Container_Info_T;  -- install/include/clang-c/Index.h:5885
      isImplicit : aliased int;  -- install/include/clang-c/Index.h:5890
      attributes : System.Address;  -- install/include/clang-c/Index.h:5891
      numAttributes : aliased unsigned;  -- install/include/clang-c/Index.h:5892
      flags : aliased unsigned;  -- install/include/clang-c/Index.h:5894
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5896

  --*
  --   * Generally same as #semanticContainer but can be different in
  --   * cases like out-of-line C++ member functions.
  --    

  --*
  --   * Whether the declaration exists in code or was created implicitly
  --   * by the compiler, e.g. implicit Objective-C methods for properties.
  --    

   type Idx_Obj_C_Container_Kind_T is 
     (Idx_Obj_C_Container_Forward_Ref,
      Idx_Obj_C_Container_Interface,
      Idx_Obj_C_Container_Implementation)
   with Convention => C;  -- install/include/clang-c/Index.h:5902

   type Idx_Obj_C_Container_Decl_Info_T is record
      declInfo : access constant Idx_Decl_Info_T;  -- install/include/clang-c/Index.h:5905
      kind : aliased Idx_Obj_C_Container_Kind_T;  -- install/include/clang-c/Index.h:5906
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5907

   type Idx_Base_Class_Info_T is record
      base : access constant Idx_Entity_Info_T;  -- install/include/clang-c/Index.h:5910
      cursor : aliased Cursor_T;  -- install/include/clang-c/Index.h:5911
      loc : aliased Idx_Loc_T;  -- install/include/clang-c/Index.h:5912
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5913

   type Idx_Obj_C_Protocol_Ref_Info_T is record
      protocol : access constant Idx_Entity_Info_T;  -- install/include/clang-c/Index.h:5916
      cursor : aliased Cursor_T;  -- install/include/clang-c/Index.h:5917
      loc : aliased Idx_Loc_T;  -- install/include/clang-c/Index.h:5918
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5919

   type Idx_Obj_C_Protocol_Ref_List_Info_T is record
      protocols : System.Address;  -- install/include/clang-c/Index.h:5922
      numProtocols : aliased unsigned;  -- install/include/clang-c/Index.h:5923
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5924

   type Idx_Obj_C_Interface_Decl_Info_T is record
      containerInfo : access constant Idx_Obj_C_Container_Decl_Info_T;  -- install/include/clang-c/Index.h:5927
      superInfo : access constant Idx_Base_Class_Info_T;  -- install/include/clang-c/Index.h:5928
      protocols : access constant Idx_Obj_C_Protocol_Ref_List_Info_T;  -- install/include/clang-c/Index.h:5929
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5930

   type Idx_Obj_C_Category_Decl_Info_T is record
      containerInfo : access constant Idx_Obj_C_Container_Decl_Info_T;  -- install/include/clang-c/Index.h:5933
      objcClass : access constant Idx_Entity_Info_T;  -- install/include/clang-c/Index.h:5934
      classCursor : aliased Cursor_T;  -- install/include/clang-c/Index.h:5935
      classLoc : aliased Idx_Loc_T;  -- install/include/clang-c/Index.h:5936
      protocols : access constant Idx_Obj_C_Protocol_Ref_List_Info_T;  -- install/include/clang-c/Index.h:5937
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5938

   type Idx_Obj_C_Property_Decl_Info_T is record
      declInfo : access constant Idx_Decl_Info_T;  -- install/include/clang-c/Index.h:5941
      getter : access constant Idx_Entity_Info_T;  -- install/include/clang-c/Index.h:5942
      setter : access constant Idx_Entity_Info_T;  -- install/include/clang-c/Index.h:5943
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5944

   type Idx_CXX_Class_Decl_Info_T is record
      declInfo : access constant Idx_Decl_Info_T;  -- install/include/clang-c/Index.h:5947
      bases : System.Address;  -- install/include/clang-c/Index.h:5948
      numBases : aliased unsigned;  -- install/include/clang-c/Index.h:5949
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:5950

  --*
  -- * Data for IndexerCallbacks#indexEntityReference.
  -- *
  -- * This may be deprecated in a future version as this duplicates
  -- * the \c CXSymbolRole_Implicit bit in \c CXSymbolRole.
  --  

  --*
  --   * The entity is referenced directly in user's code.
  --    

  --*
  --   * An implicit reference, e.g. a reference of an Objective-C method
  --   * via the dot syntax.
  --    

   subtype Idx_Entity_Ref_Kind_T is unsigned;
   Idx_Entity_Ref_Direct : constant Idx_Entity_Ref_Kind_T := 1;
   Idx_Entity_Ref_Implicit : constant Idx_Entity_Ref_Kind_T := 2;  -- install/include/clang-c/Index.h:5968

  --*
  -- * Roles that are attributed to symbol occurrences.
  -- *
  -- * Internal: this currently mirrors low 9 bits of clang::index::SymbolRole with
  -- * higher bits zeroed. These high bits may be exposed in the future.
  --  

   subtype Symbol_Role_T is unsigned;
   Symbol_Role_None : constant Symbol_Role_T := 0;
   Symbol_Role_Declaration : constant Symbol_Role_T := 1;
   Symbol_Role_Definition : constant Symbol_Role_T := 2;
   Symbol_Role_Reference : constant Symbol_Role_T := 4;
   Symbol_Role_Read : constant Symbol_Role_T := 8;
   Symbol_Role_Write : constant Symbol_Role_T := 16;
   Symbol_Role_Call : constant Symbol_Role_T := 32;
   Symbol_Role_Dynamic : constant Symbol_Role_T := 64;
   Symbol_Role_Address_Of : constant Symbol_Role_T := 128;
   Symbol_Role_Implicit : constant Symbol_Role_T := 256;  -- install/include/clang-c/Index.h:5987

  --*
  -- * Data for IndexerCallbacks#indexEntityReference.
  --  

   type Idx_Entity_Ref_Info_T is record
      kind : aliased Idx_Entity_Ref_Kind_T;  -- install/include/clang-c/Index.h:5993
      cursor : aliased Cursor_T;  -- install/include/clang-c/Index.h:5997
      loc : aliased Idx_Loc_T;  -- install/include/clang-c/Index.h:5998
      referencedEntity : access constant Idx_Entity_Info_T;  -- install/include/clang-c/Index.h:6002
      parentEntity : access constant Idx_Entity_Info_T;  -- install/include/clang-c/Index.h:6014
      container : access constant Idx_Container_Info_T;  -- install/include/clang-c/Index.h:6018
      role : aliased Symbol_Role_T;  -- install/include/clang-c/Index.h:6022
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:6023

  --*
  --   * Reference cursor.
  --    

  --*
  --   * The entity that gets referenced.
  --    

  --*
  --   * Immediate "parent" of the reference. For example:
  --   *
  --   * \code
  --   * Foo *var;
  --   * \endcode
  --   *
  --   * The parent of reference of type 'Foo' is the variable 'var'.
  --   * For references inside statement bodies of functions/methods,
  --   * the parentEntity will be the function/method.
  --    

  --*
  --   * Lexical container context of the reference.
  --    

  --*
  --   * Sets of symbol roles of the reference.
  --    

  --*
  -- * A group of callbacks used by #clang_indexSourceFile and
  -- * #clang_indexTranslationUnit.
  --  

  --*
  --   * Called periodically to check whether indexing should be aborted.
  --   * Should return 0 to continue, and non-zero to abort.
  --    

   type IndexerCallbacks is record
      abortQuery : access function (Arg_1 : Client_Data_T; Arg_2 : System.Address) return int;  -- install/include/clang-c/Index.h:6034
      diagnostic : access procedure
           (Arg_1 : Client_Data_T;
            Arg_2 : Clang.CX_Diagnostic.Diagnostic_Set_T;
            Arg_3 : System.Address);  -- install/include/clang-c/Index.h:6039
      enteredMainFile : access function
           (Arg_1 : Client_Data_T;
            Arg_2 : Clang.CX_File.File_T;
            Arg_3 : System.Address) return Idx_Client_File_T;  -- install/include/clang-c/Index.h:6041
      ppIncludedFile : access function (Arg_1 : Client_Data_T; Arg_2 : access constant Idx_Included_File_Info_T) return Idx_Client_File_T;  -- install/include/clang-c/Index.h:6047
      importedASTFile : access function (Arg_1 : Client_Data_T; Arg_2 : access constant Idx_Imported_AST_File_Info_T) return Idx_Client_AST_File_T;  -- install/include/clang-c/Index.h:6058
      startedTranslationUnit : access function (Arg_1 : Client_Data_T; Arg_2 : System.Address) return Idx_Client_Container_T;  -- install/include/clang-c/Index.h:6064
      indexDeclaration : access procedure (Arg_1 : Client_Data_T; Arg_2 : access constant Idx_Decl_Info_T);  -- install/include/clang-c/Index.h:6067
      indexEntityReference : access procedure (Arg_1 : Client_Data_T; Arg_2 : access constant Idx_Entity_Ref_Info_T);  -- install/include/clang-c/Index.h:6072
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Index.h:6075

  --*
  --   * Called at the end of indexing; passes the complete diagnostic set.
  --    

  --*
  --   * Called when a file gets \#included/\#imported.
  --    

  --*
  --   * Called when a AST file (PCH or module) gets imported.
  --   *
  --   * AST files will not get indexed (there will not be callbacks to index all
  --   * the entities in an AST file). The recommended action is that, if the AST
  --   * file is not already indexed, to initiate a new indexing job specific to
  --   * the AST file.
  --    

  --*
  --   * Called at the beginning of indexing a translation unit.
  --    

  --*
  --   * Called to index a reference of an entity.
  --    

function Index_Is_Entity_Obj_C_Container_Kind
     (Arg_1 : Idx_Entity_Kind_T)
      return Boolean;

   function Index_Get_Obj_C_Container_Decl_Info (Arg_1 : access constant Idx_Decl_Info_T) return access constant Idx_Obj_C_Container_Decl_Info_T  -- install/include/clang-c/Index.h:6079
   with Import => True, 
        Convention => C, 
        External_Name => "clang_index_getObjCContainerDeclInfo";

   function Index_Get_Obj_C_Interface_Decl_Info (Arg_1 : access constant Idx_Decl_Info_T) return access constant Idx_Obj_C_Interface_Decl_Info_T  -- install/include/clang-c/Index.h:6082
   with Import => True, 
        Convention => C, 
        External_Name => "clang_index_getObjCInterfaceDeclInfo";

   function Index_Get_Obj_C_Category_Decl_Info (Arg_1 : access constant Idx_Decl_Info_T) return access constant Idx_Obj_C_Category_Decl_Info_T  -- install/include/clang-c/Index.h:6086
   with Import => True, 
        Convention => C, 
        External_Name => "clang_index_getObjCCategoryDeclInfo";

   function Index_Get_Obj_C_Protocol_Ref_List_Info (Arg_1 : access constant Idx_Decl_Info_T) return access constant Idx_Obj_C_Protocol_Ref_List_Info_T  -- install/include/clang-c/Index.h:6089
   with Import => True, 
        Convention => C, 
        External_Name => "clang_index_getObjCProtocolRefListInfo";

   function Index_Get_Obj_C_Property_Decl_Info (Arg_1 : access constant Idx_Decl_Info_T) return access constant Idx_Obj_C_Property_Decl_Info_T  -- install/include/clang-c/Index.h:6092
   with Import => True, 
        Convention => C, 
        External_Name => "clang_index_getObjCPropertyDeclInfo";

   function Index_Get_IB_Outlet_Collection_Attr_Info (Arg_1 : access constant Idx_Attr_Info_T) return access constant Idx_IB_Outlet_Collection_Attr_Info_T  -- install/include/clang-c/Index.h:6095
   with Import => True, 
        Convention => C, 
        External_Name => "clang_index_getIBOutletCollectionAttrInfo";

   function Index_Get_CXX_Class_Decl_Info (Arg_1 : access constant Idx_Decl_Info_T) return access constant Idx_CXX_Class_Decl_Info_T  -- install/include/clang-c/Index.h:6098
   with Import => True, 
        Convention => C, 
        External_Name => "clang_index_getCXXClassDeclInfo";

  --*
  -- * For retrieving a custom CXIdxClientContainer attached to a
  -- * container.
  --  

   function Index_Get_Client_Container (Arg_1 : access constant Idx_Container_Info_T) return Idx_Client_Container_T  -- install/include/clang-c/Index.h:6105
   with Import => True, 
        Convention => C, 
        External_Name => "clang_index_getClientContainer";

  --*
  -- * For setting a custom CXIdxClientContainer attached to a
  -- * container.
  --  

   procedure Index_Set_Client_Container (Arg_1 : access constant Idx_Container_Info_T; Arg_2 : Idx_Client_Container_T)  -- install/include/clang-c/Index.h:6111
   with Import => True, 
        Convention => C, 
        External_Name => "clang_index_setClientContainer";

  --*
  -- * For retrieving a custom CXIdxClientEntity attached to an entity.
  --  

   function Index_Get_Client_Entity (Arg_1 : access constant Idx_Entity_Info_T) return Idx_Client_Entity_T  -- install/include/clang-c/Index.h:6118
   with Import => True, 
        Convention => C, 
        External_Name => "clang_index_getClientEntity";

  --*
  -- * For setting a custom CXIdxClientEntity attached to an entity.
  --  

   procedure Index_Set_Client_Entity (Arg_1 : access constant Idx_Entity_Info_T; Arg_2 : Idx_Client_Entity_T)  -- install/include/clang-c/Index.h:6123
   with Import => True, 
        Convention => C, 
        External_Name => "clang_index_setClientEntity";

  --*
  -- * An indexing action/session, to be applied to one or multiple
  -- * translation units.
  --  

   type Index_Action_T is new System.Address;  -- install/include/clang-c/Index.h:6130

  --*
  -- * An indexing action/session, to be applied to one or multiple
  -- * translation units.
  -- *
  -- * \param CIdx The index object with which the index action will be associated.
  --  

   function Index_Action_Create (C_Idx : Index_T) return Index_Action_T  -- install/include/clang-c/Index.h:6138
   with Import => True, 
        Convention => C, 
        External_Name => "clang_IndexAction_create";

  --*
  -- * Destroy the given index action.
  -- *
  -- * The index action must not be destroyed until all of the translation units
  -- * created within that index action have been destroyed.
  --  

   procedure Index_Action_Dispose (Arg_1 : Index_Action_T)  -- install/include/clang-c/Index.h:6146
   with Import => True, 
        Convention => C, 
        External_Name => "clang_IndexAction_dispose";

  --*
  --   * Used to indicate that no special indexing options are needed.
  --    

  --*
  --   * Used to indicate that IndexerCallbacks#indexEntityReference should
  --   * be invoked for only one reference of an entity per source file that does
  --   * not also include a declaration/definition of the entity.
  --    

  --*
  --   * Function-local symbols should be indexed. If this is not set
  --   * function-local symbols will be ignored.
  --    

  --*
  --   * Implicit function/class template instantiations should be indexed.
  --   * If this is not set, implicit instantiations will be ignored.
  --    

  --*
  --   * Suppress all compiler warnings when parsing for indexing.
  --    

  --*
  --   * Skip a function/method body that was already parsed during an
  --   * indexing session associated with a \c CXIndexAction object.
  --   * Bodies in system headers are always skipped.
  --    

   subtype Index_Opt_Flags_T is unsigned;
   Index_Opt_None : constant Index_Opt_Flags_T := 0;
   Index_Opt_Suppress_Redundant_Refs : constant Index_Opt_Flags_T := 1;
   Index_Opt_Index_Function_Local_Symbols : constant Index_Opt_Flags_T := 2;
   Index_Opt_Index_Implicit_Template_Instantiations : constant Index_Opt_Flags_T := 4;
   Index_Opt_Suppress_Warnings : constant Index_Opt_Flags_T := 8;
   Index_Opt_Skip_Parsed_Bodies_In_Session : constant Index_Opt_Flags_T := 16;  -- install/include/clang-c/Index.h:6185

  --*
  -- * Index the given source file and the translation unit corresponding
  -- * to that file via callbacks implemented through #IndexerCallbacks.
  -- *
  -- * \param client_data pointer data supplied by the client, which will
  -- * be passed to the invoked callbacks.
  -- *
  -- * \param index_callbacks Pointer to indexing callbacks that the client
  -- * implements.
  -- *
  -- * \param index_callbacks_size Size of #IndexerCallbacks structure that gets
  -- * passed in index_callbacks.
  -- *
  -- * \param index_options A bitmask of options that affects how indexing is
  -- * performed. This should be a bitwise OR of the CXIndexOpt_XXX flags.
  -- *
  -- * \param[out] out_TU pointer to store a \c CXTranslationUnit that can be
  -- * reused after indexing is finished. Set to \c NULL if you do not require it.
  -- *
  -- * \returns 0 on success or if there were errors from which the compiler could
  -- * recover.  If there is a failure from which there is no recovery, returns
  -- * a non-zero \c CXErrorCode.
  -- *
  -- * The rest of the parameters are the same as #clang_parseTranslationUnit.
  --  

function Index_Source_File
     (Arg_1                 : Index_Action_T;
      Client_Data           : Client_Data_T;
      Index_Callbacks       : access IndexerCallbacks;
      Index_Callbacks_Size  : unsigned;
      Index_Options         : unsigned;
      Source_Filename       : String;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Out_TU                : System.Address;
      TU_Options            : unsigned)
      return int;

  --*
  -- * Same as clang_indexSourceFile but requires a full command line
  -- * for \c command_line_args including argv[0]. This is useful if the standard
  -- * library paths are relative to the binary.
  --  

function Index_Source_File_Full_Argv
     (Arg_1                 : Index_Action_T;
      Client_Data           : Client_Data_T;
      Index_Callbacks       : access IndexerCallbacks;
      Index_Callbacks_Size  : unsigned;
      Index_Options         : unsigned;
      Source_Filename       : String;
      Command_Line_Args     : System.Address;
      Num_Command_Line_Args : int;
      Unsaved_Files         : access Unsaved_File_T;
      Num_Unsaved_Files     : unsigned;
      Out_TU                : System.Address;
      TU_Options            : unsigned)
      return int;

  --*
  -- * Index the given translation unit via callbacks implemented through
  -- * #IndexerCallbacks.
  -- *
  -- * The order of callback invocations is not guaranteed to be the same as
  -- * when indexing a source file. The high level order will be:
  -- *
  -- *   -Preprocessor callbacks invocations
  -- *   -Declaration/reference callbacks invocations
  -- *   -Diagnostic callback invocations
  -- *
  -- * The parameters are the same as #clang_indexSourceFile.
  -- *
  -- * \returns If there is a failure from which there is no recovery, returns
  -- * non-zero, otherwise returns 0.
  --  

   function Index_Translation_Unit
     (Arg_1 : Index_Action_T;
      Client_Data : Client_Data_T;
      Index_Callbacks : access IndexerCallbacks;
      Index_Callbacks_Size : unsigned;
      Index_Options : unsigned;
      Arg_6 : Translation_Unit_T) return int  -- install/include/clang-c/Index.h:6247
   with Import => True, 
        Convention => C, 
        External_Name => "clang_indexTranslationUnit";

  --*
  -- * Retrieve the CXIdxFile, file, line, column, and offset represented by
  -- * the given CXIdxLoc.
  -- *
  -- * If the location refers into a macro expansion, retrieves the
  -- * location of the macro expansion and if it refers into a macro argument
  -- * retrieves the location of the argument.
  --  

   procedure Index_Loc_Get_File_Location
     (Loc : Idx_Loc_T;
      Index_File : System.Address;
      File : System.Address;
      Line : access unsigned;
      Column : access unsigned;
      Offset : access unsigned)  -- install/include/clang-c/Index.h:6259
   with Import => True, 
        Convention => C, 
        External_Name => "clang_indexLoc_getFileLocation";

  --*
  -- * Retrieve the CXSourceLocation represented by the given CXIdxLoc.
  --  

   function Index_Loc_Get_CX_Source_Location (Loc : Idx_Loc_T) return Clang.CX_Source_Location.Source_Location_T  -- install/include/clang-c/Index.h:6269
   with Import => True, 
        Convention => C, 
        External_Name => "clang_indexLoc_getCXSourceLocation";

  --*
  -- * Visitor invoked for each field found by a traversal.
  -- *
  -- * This visitor function will be invoked for each field found by
  -- * \c clang_Type_visitFields. Its first argument is the cursor being
  -- * visited, its second argument is the client data provided to
  -- * \c clang_Type_visitFields.
  -- *
  -- * The visitor should return one of the \c CXVisitorResult values
  -- * to direct \c clang_Type_visitFields.
  --  

   type Field_Visitor_T is access function (Arg_1 : Cursor_T; Arg_2 : Client_Data_T) return Visitor_Result_T
   with Convention => C;  -- install/include/clang-c/Index.h:6282

  --*
  -- * Visit the fields of a particular type.
  -- *
  -- * This function visits all the direct fields of the given cursor,
  -- * invoking the given \p visitor function with the cursors of each
  -- * visited field. The traversal may be ended prematurely, if
  -- * the visitor returns \c CXFieldVisit_Break.
  -- *
  -- * \param T the record type whose field may be visited.
  -- *
  -- * \param visitor the visitor function that will be invoked for each
  -- * field of \p T.
  -- *
  -- * \param client_data pointer data supplied by the client, which will
  -- * be passed to the visitor each time it is invoked.
  -- *
  -- * \returns a non-zero value if the traversal was terminated
  -- * prematurely by the visitor returning \c CXFieldVisit_Break.
  --  

   function Type_Visit_Fields
     (T : Type_T;
      Visitor : Field_Visitor_T;
      Client_Data : Client_Data_T) return unsigned  -- install/include/clang-c/Index.h:6304
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Type_visitFields";

  --*
  -- * @}
  --  

  --*
  -- * @}
  --  

end Clang.Index;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
