pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with Interfaces.C.Strings;
with Clang.CX_String;
with Clang.CX_Source_Location;

package Clang.CX_Diagnostic is

  --===-- clang-c/CXDiagnostic.h - C Index Diagnostics --------------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header provides the interface to C Index diagnostics.                 *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * \defgroup CINDEX_DIAG Diagnostic reporting
  -- *
  -- * @{
  --  

  --*
  -- * Describes the severity of a particular diagnostic.
  --  

   type Diagnostic_Severity_T is 
     (Diagnostic_Ignored,
      Diagnostic_Note,
      Diagnostic_Warning,
      Diagnostic_Error,
      Diagnostic_Fatal)
   with Convention => C;  -- install/include/clang-c/CXDiagnostic.h:33

  --*
  --   * A diagnostic that has been suppressed, e.g., by a command-line
  --   * option.
  --    

  --*
  --   * This diagnostic is a note that should be attached to the
  --   * previous (non-note) diagnostic.
  --    

  --*
  --   * This diagnostic indicates suspicious code that may not be
  --   * wrong.
  --    

  --*
  --   * This diagnostic indicates that the code is ill-formed.
  --    

  --*
  --   * This diagnostic indicates that the code is ill-formed such
  --   * that future parser recovery is unlikely to produce useful
  --   * results.
  --    

  --*
  -- * A single diagnostic, containing the diagnostic's severity,
  -- * location, text, source ranges, and fix-it hints.
  --  

   type Diagnostic_T is new System.Address;  -- install/include/clang-c/CXDiagnostic.h:69

  --*
  -- * A group of CXDiagnostics.
  --  

   type Diagnostic_Set_T is new System.Address;  -- install/include/clang-c/CXDiagnostic.h:74

  --*
  -- * Determine the number of diagnostics in a CXDiagnosticSet.
  --  

   function Get_Num_Diagnostics_In_Set (Diags : Diagnostic_Set_T) return unsigned  -- install/include/clang-c/CXDiagnostic.h:79
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getNumDiagnosticsInSet";

  --*
  -- * Retrieve a diagnostic associated with the given CXDiagnosticSet.
  -- *
  -- * \param Diags the CXDiagnosticSet to query.
  -- * \param Index the zero-based diagnostic number to retrieve.
  -- *
  -- * \returns the requested diagnostic. This diagnostic must be freed
  -- * via a call to \c clang_disposeDiagnostic().
  --  

   function Get_Diagnostic_In_Set (Diags : Diagnostic_Set_T; Index : unsigned) return Diagnostic_T  -- install/include/clang-c/CXDiagnostic.h:90
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getDiagnosticInSet";

  --*
  -- * Describes the kind of error that occurred (if any) in a call to
  -- * \c clang_loadDiagnostics.
  --  

   type Load_Diag_Error_T is 
     (Load_Diag_None,
      Load_Diag_Unknown,
      Load_Diag_Cannot_Load,
      Load_Diag_Invalid_File)
   with Convention => C;  -- install/include/clang-c/CXDiagnostic.h:97

  --*
  --   * Indicates that no error occurred.
  --    

  --*
  --   * Indicates that an unknown error occurred while attempting to
  --   * deserialize diagnostics.
  --    

  --*
  --   * Indicates that the file containing the serialized diagnostics
  --   * could not be opened.
  --    

  --*
  --   * Indicates that the serialized diagnostics file is invalid or
  --   * corrupt.
  --    

  --*
  -- * Deserialize a set of diagnostics from a Clang diagnostics bitcode
  -- * file.
  -- *
  -- * \param file The name of the file to deserialize.
  -- * \param error A pointer to a enum value recording if there was a problem
  -- *        deserializing the diagnostics.
  -- * \param errorString A pointer to a CXString for recording the error string
  -- *        if the file was not successfully loaded.
  -- *
  -- * \returns A loaded CXDiagnosticSet if successful, and NULL otherwise.  These
  -- * diagnostics should be released using clang_disposeDiagnosticSet().
  --  

function Load_Diagnostics
     (File         : String;
      Error        : access Load_Diag_Error_T;
      Error_String : access Clang.CX_String.String_T)
      return Diagnostic_Set_T;

  --*
  -- * Release a CXDiagnosticSet and all of its contained diagnostics.
  --  

   procedure Dispose_Diagnostic_Set (Diags : Diagnostic_Set_T)  -- install/include/clang-c/CXDiagnostic.h:141
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeDiagnosticSet";

  --*
  -- * Retrieve the child diagnostics of a CXDiagnostic.
  -- *
  -- * This CXDiagnosticSet does not need to be released by
  -- * clang_disposeDiagnosticSet.
  --  

   function Get_Child_Diagnostics (D : Diagnostic_T) return Diagnostic_Set_T  -- install/include/clang-c/CXDiagnostic.h:149
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getChildDiagnostics";

  --*
  -- * Destroy a diagnostic.
  --  

   procedure Dispose_Diagnostic (Diagnostic : Diagnostic_T)  -- install/include/clang-c/CXDiagnostic.h:154
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeDiagnostic";

  --*
  -- * Options to control the display of diagnostics.
  -- *
  -- * The values in this enum are meant to be combined to customize the
  -- * behavior of \c clang_formatDiagnostic().
  --  

   subtype Diagnostic_Display_Options_T is unsigned;
   Diagnostic_Display_Source_Location : constant Diagnostic_Display_Options_T := 1;
   Diagnostic_Display_Column : constant Diagnostic_Display_Options_T := 2;
   Diagnostic_Display_Source_Ranges : constant Diagnostic_Display_Options_T := 4;
   Diagnostic_Display_Option : constant Diagnostic_Display_Options_T := 8;
   Diagnostic_Display_Category_Id : constant Diagnostic_Display_Options_T := 16;
   Diagnostic_Display_Category_Name : constant Diagnostic_Display_Options_T := 32;  -- install/include/clang-c/CXDiagnostic.h:162

  --*
  --   * Display the source-location information where the
  --   * diagnostic was located.
  --   *
  --   * When set, diagnostics will be prefixed by the file, line, and
  --   * (optionally) column to which the diagnostic refers. For example,
  --   *
  --   * \code
  --   * test.c:28: warning: extra tokens at end of #endif directive
  --   * \endcode
  --   *
  --   * This option corresponds to the clang flag \c -fshow-source-location.
  --    

  --*
  --   * If displaying the source-location information of the
  --   * diagnostic, also include the column number.
  --   *
  --   * This option corresponds to the clang flag \c -fshow-column.
  --    

  --*
  --   * If displaying the source-location information of the
  --   * diagnostic, also include information about source ranges in a
  --   * machine-parsable format.
  --   *
  --   * This option corresponds to the clang flag
  --   * \c -fdiagnostics-print-source-range-info.
  --    

  --*
  --   * Display the option name associated with this diagnostic, if any.
  --   *
  --   * The option name displayed (e.g., -Wconversion) will be placed in brackets
  --   * after the diagnostic text. This option corresponds to the clang flag
  --   * \c -fdiagnostics-show-option.
  --    

  --*
  --   * Display the category number associated with this diagnostic, if any.
  --   *
  --   * The category number is displayed within brackets after the diagnostic text.
  --   * This option corresponds to the clang flag
  --   * \c -fdiagnostics-show-category=id.
  --    

  --*
  --   * Display the category name associated with this diagnostic, if any.
  --   *
  --   * The category name is displayed within brackets after the diagnostic text.
  --   * This option corresponds to the clang flag
  --   * \c -fdiagnostics-show-category=name.
  --    

  --*
  -- * Format the given diagnostic in a manner that is suitable for display.
  -- *
  -- * This routine will format the given diagnostic to a string, rendering
  -- * the diagnostic according to the various options given. The
  -- * \c clang_defaultDiagnosticDisplayOptions() function returns the set of
  -- * options that most closely mimics the behavior of the clang compiler.
  -- *
  -- * \param Diagnostic The diagnostic to print.
  -- *
  -- * \param Options A set of options that control the diagnostic display,
  -- * created by combining \c CXDiagnosticDisplayOptions values.
  -- *
  -- * \returns A new string containing for formatted diagnostic.
  --  

function Format_Diagnostic
     (Diagnostic : Diagnostic_T;
      Options    : unsigned)
      return String;

  --*
  -- * Retrieve the set of display options most similar to the
  -- * default behavior of the clang compiler.
  -- *
  -- * \returns A set of display options suitable for use with \c
  -- * clang_formatDiagnostic().
  --  

   function Default_Diagnostic_Display_Options return unsigned  -- install/include/clang-c/CXDiagnostic.h:249
   with Import => True, 
        Convention => C, 
        External_Name => "clang_defaultDiagnosticDisplayOptions";

  --*
  -- * Determine the severity of the given diagnostic.
  --  

   function Get_Diagnostic_Severity (Arg_1 : Diagnostic_T) return Diagnostic_Severity_T  -- install/include/clang-c/CXDiagnostic.h:255
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getDiagnosticSeverity";

  --*
  -- * Retrieve the source location of the given diagnostic.
  -- *
  -- * This location is where Clang would print the caret ('^') when
  -- * displaying the diagnostic on the command line.
  --  

   function Get_Diagnostic_Location (Arg_1 : Diagnostic_T) return Clang.CX_Source_Location.Source_Location_T  -- install/include/clang-c/CXDiagnostic.h:263
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getDiagnosticLocation";

  --*
  -- * Retrieve the text of the given diagnostic.
  --  

function Get_Diagnostic_Spelling
     (Arg_1 : Diagnostic_T)
      return String;

  --*
  -- * Retrieve the name of the command-line option that enabled this
  -- * diagnostic.
  -- *
  -- * \param Diag The diagnostic to be queried.
  -- *
  -- * \param Disable If non-NULL, will be set to the option that disables this
  -- * diagnostic (if any).
  -- *
  -- * \returns A string that contains the command-line option used to enable this
  -- * warning, such as "-Wconversion" or "-pedantic".
  --  

function Get_Diagnostic_Option
     (Diag    : Diagnostic_T;
      Disable : access Clang.CX_String.String_T)
      return String;

  --*
  -- * Retrieve the category number for this diagnostic.
  -- *
  -- * Diagnostics can be categorized into groups along with other, related
  -- * diagnostics (e.g., diagnostics under the same warning flag). This routine
  -- * retrieves the category number for the given diagnostic.
  -- *
  -- * \returns The number of the category that contains this diagnostic, or zero
  -- * if this diagnostic is uncategorized.
  --  

   function Get_Diagnostic_Category (Arg_1 : Diagnostic_T) return unsigned  -- install/include/clang-c/CXDiagnostic.h:295
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getDiagnosticCategory";

  --*
  -- * Retrieve the name of a particular diagnostic category.  This
  -- *  is now deprecated.  Use clang_getDiagnosticCategoryText()
  -- *  instead.
  -- *
  -- * \param Category A diagnostic category number, as returned by
  -- * \c clang_getDiagnosticCategory().
  -- *
  -- * \returns The name of the given diagnostic category.
  --  

function Get_Diagnostic_Category_Name
     (Category : unsigned)
      return String;

  --*
  -- * Retrieve the diagnostic category text for a given diagnostic.
  -- *
  -- * \returns The text of the given diagnostic category.
  --  

function Get_Diagnostic_Category_Text
     (Arg_1 : Diagnostic_T)
      return String;

  --*
  -- * Determine the number of source ranges associated with the given
  -- * diagnostic.
  --  

   function Get_Diagnostic_Num_Ranges (Arg_1 : Diagnostic_T) return unsigned  -- install/include/clang-c/CXDiagnostic.h:321
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getDiagnosticNumRanges";

  --*
  -- * Retrieve a source range associated with the diagnostic.
  -- *
  -- * A diagnostic's source ranges highlight important elements in the source
  -- * code. On the command line, Clang displays source ranges by
  -- * underlining them with '~' characters.
  -- *
  -- * \param Diagnostic the diagnostic whose range is being extracted.
  -- *
  -- * \param Range the zero-based index specifying which range to
  -- *
  -- * \returns the requested source range.
  --  

   function Get_Diagnostic_Range (Diagnostic : Diagnostic_T; C_Range : unsigned) return Clang.CX_Source_Location.Source_Range_T  -- install/include/clang-c/CXDiagnostic.h:336
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getDiagnosticRange";

  --*
  -- * Determine the number of fix-it hints associated with the
  -- * given diagnostic.
  --  

   function Get_Diagnostic_Num_Fix_Its (Diagnostic : Diagnostic_T) return unsigned  -- install/include/clang-c/CXDiagnostic.h:343
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getDiagnosticNumFixIts";

  --*
  -- * Retrieve the replacement information for a given fix-it.
  -- *
  -- * Fix-its are described in terms of a source range whose contents
  -- * should be replaced by a string. This approach generalizes over
  -- * three kinds of operations: removal of source code (the range covers
  -- * the code to be removed and the replacement string is empty),
  -- * replacement of source code (the range covers the code to be
  -- * replaced and the replacement string provides the new code), and
  -- * insertion (both the start and end of the range point at the
  -- * insertion location, and the replacement string provides the text to
  -- * insert).
  -- *
  -- * \param Diagnostic The diagnostic whose fix-its are being queried.
  -- *
  -- * \param FixIt The zero-based index of the fix-it.
  -- *
  -- * \param ReplacementRange The source range whose contents will be
  -- * replaced with the returned replacement string. Note that source
  -- * ranges are half-open ranges [a, b), so the source code should be
  -- * replaced from a and up to (but not including) b.
  -- *
  -- * \returns A string containing text that should be replace the source
  -- * code indicated by the \c ReplacementRange.
  --  

function Get_Diagnostic_Fix_It
     (Diagnostic        : Diagnostic_T;
      Fix_It            : unsigned;
      Replacement_Range : access Clang.CX_Source_Location.Source_Range_T)
      return String;

  --*
  -- * @}
  --  

end Clang.CX_Diagnostic;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
