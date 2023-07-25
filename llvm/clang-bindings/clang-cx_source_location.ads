pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
limited with Clang.CX_String;

package Clang.CX_Source_Location is

  --===-- clang-c/CXSourceLocation.h - C Index Source Location ------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header provides the interface to C Index source locations.            *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * \defgroup CINDEX_LOCATIONS Physical source locations
  -- *
  -- * Clang represents physical source locations in its abstract syntax tree in
  -- * great detail, with file, line, and column information for the majority of
  -- * the tokens parsed in the source code. These data types and functions are
  -- * used to represent source location information, either for a particular
  -- * point in the program or for a range of points in the program, and extract
  -- * specific location information from those data types.
  -- *
  -- * @{
  --  

  --*
  -- * Identifies a specific source location within a translation
  -- * unit.
  -- *
  -- * Use clang_getExpansionLocation() or clang_getSpellingLocation()
  -- * to map a source location to a particular file, line, and column.
  --  

   type anon_array1114 is array (0 .. 1) of System.Address;
   type Source_Location_T is record
      ptr_data : anon_array1114;  -- install/include/clang-c/CXSourceLocation.h:45
      int_data : aliased unsigned;  -- install/include/clang-c/CXSourceLocation.h:46
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/CXSourceLocation.h:47

  --*
  -- * Identifies a half-open character range in the source code.
  -- *
  -- * Use clang_getRangeStart() and clang_getRangeEnd() to retrieve the
  -- * starting and end locations from a source range, respectively.
  --  

   type Source_Range_T is record
      ptr_data : anon_array1114;  -- install/include/clang-c/CXSourceLocation.h:56
      begin_int_data : aliased unsigned;  -- install/include/clang-c/CXSourceLocation.h:57
      end_int_data : aliased unsigned;  -- install/include/clang-c/CXSourceLocation.h:58
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/CXSourceLocation.h:59

  --*
  -- * Retrieve a NULL (invalid) source location.
  --  

   function Get_Null_Location return Source_Location_T  -- install/include/clang-c/CXSourceLocation.h:64
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getNullLocation";

  --*
  -- * Determine whether two source locations, which must refer into
  -- * the same translation unit, refer to exactly the same point in the source
  -- * code.
  -- *
  -- * \returns non-zero if the source locations refer to the same location, zero
  -- * if they refer to different locations.
  --  

   function Equal_Locations (Loc_1 : Source_Location_T; Loc_2 : Source_Location_T) return unsigned  -- install/include/clang-c/CXSourceLocation.h:74
   with Import => True, 
        Convention => C, 
        External_Name => "clang_equalLocations";

  --*
  -- * Returns non-zero if the given source location is in a system header.
  --  

function Location_Is_In_System_Header
     (Location : Source_Location_T)
      return Boolean;

  --*
  -- * Returns non-zero if the given source location is in the main file of
  -- * the corresponding translation unit.
  --  

function Location_Is_From_Main_File
     (Location : Source_Location_T)
      return Boolean;

  --*
  -- * Retrieve a NULL (invalid) source range.
  --  

   function Get_Null_Range return Source_Range_T  -- install/include/clang-c/CXSourceLocation.h:91
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getNullRange";

  --*
  -- * Retrieve a source range given the beginning and ending source
  -- * locations.
  --  

   function Get_Range (C_Begin : Source_Location_T; C_End : Source_Location_T) return Source_Range_T  -- install/include/clang-c/CXSourceLocation.h:97
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getRange";

  --*
  -- * Determine whether two ranges are equivalent.
  -- *
  -- * \returns non-zero if the ranges are the same, zero if they differ.
  --  

   function Equal_Ranges (Range_1 : Source_Range_T; Range_2 : Source_Range_T) return unsigned  -- install/include/clang-c/CXSourceLocation.h:105
   with Import => True, 
        Convention => C, 
        External_Name => "clang_equalRanges";

  --*
  -- * Returns non-zero if \p range is null.
  --  

function Range_Is_Null
     (C_Range : Source_Range_T)
      return Boolean;

  --*
  -- * Retrieve the file, line, column, and offset represented by
  -- * the given source location.
  -- *
  -- * If the location refers into a macro expansion, retrieves the
  -- * location of the macro expansion.
  -- *
  -- * \param location the location within a source file that will be decomposed
  -- * into its parts.
  -- *
  -- * \param file [out] if non-NULL, will be set to the file to which the given
  -- * source location points.
  -- *
  -- * \param line [out] if non-NULL, will be set to the line to which the given
  -- * source location points.
  -- *
  -- * \param column [out] if non-NULL, will be set to the column to which the given
  -- * source location points.
  -- *
  -- * \param offset [out] if non-NULL, will be set to the offset into the
  -- * buffer to which the given source location points.
  --  

   procedure Get_Expansion_Location
     (Location : Source_Location_T;
      File : System.Address;
      Line : access unsigned;
      Column : access unsigned;
      Offset : access unsigned)  -- install/include/clang-c/CXSourceLocation.h:135
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getExpansionLocation";

  --*
  -- * Retrieve the file, line and column represented by the given source
  -- * location, as specified in a # line directive.
  -- *
  -- * Example: given the following source code in a file somefile.c
  -- *
  -- * \code
  -- * #123 "dummy.c" 1
  -- *
  -- * static int func(void)
  -- * {
  -- *     return 0;
  -- * }
  -- * \endcode
  -- *
  -- * the location information returned by this function would be
  -- *
  -- * File: dummy.c Line: 124 Column: 12
  -- *
  -- * whereas clang_getExpansionLocation would have returned
  -- *
  -- * File: somefile.c Line: 3 Column: 12
  -- *
  -- * \param location the location within a source file that will be decomposed
  -- * into its parts.
  -- *
  -- * \param filename [out] if non-NULL, will be set to the filename of the
  -- * source location. Note that filenames returned will be for "virtual" files,
  -- * which don't necessarily exist on the machine running clang - e.g. when
  -- * parsing preprocessed output obtained from a different environment. If
  -- * a non-NULL value is passed in, remember to dispose of the returned value
  -- * using \c clang_disposeString() once you've finished with it. For an invalid
  -- * source location, an empty string is returned.
  -- *
  -- * \param line [out] if non-NULL, will be set to the line number of the
  -- * source location. For an invalid source location, zero is returned.
  -- *
  -- * \param column [out] if non-NULL, will be set to the column number of the
  -- * source location. For an invalid source location, zero is returned.
  --  

   procedure Get_Presumed_Location
     (Location : Source_Location_T;
      Filename : access Clang.CX_String.String_T;
      Line : access unsigned;
      Column : access unsigned)  -- install/include/clang-c/CXSourceLocation.h:180
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getPresumedLocation";

  --*
  -- * Legacy API to retrieve the file, line, column, and offset represented
  -- * by the given source location.
  -- *
  -- * This interface has been replaced by the newer interface
  -- * #clang_getExpansionLocation(). See that interface's documentation for
  -- * details.
  --  

   procedure Get_Instantiation_Location
     (Location : Source_Location_T;
      File : System.Address;
      Line : access unsigned;
      Column : access unsigned;
      Offset : access unsigned)  -- install/include/clang-c/CXSourceLocation.h:192
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getInstantiationLocation";

  --*
  -- * Retrieve the file, line, column, and offset represented by
  -- * the given source location.
  -- *
  -- * If the location refers into a macro instantiation, return where the
  -- * location was originally spelled in the source file.
  -- *
  -- * \param location the location within a source file that will be decomposed
  -- * into its parts.
  -- *
  -- * \param file [out] if non-NULL, will be set to the file to which the given
  -- * source location points.
  -- *
  -- * \param line [out] if non-NULL, will be set to the line to which the given
  -- * source location points.
  -- *
  -- * \param column [out] if non-NULL, will be set to the column to which the given
  -- * source location points.
  -- *
  -- * \param offset [out] if non-NULL, will be set to the offset into the
  -- * buffer to which the given source location points.
  --  

   procedure Get_Spelling_Location
     (Location : Source_Location_T;
      File : System.Address;
      Line : access unsigned;
      Column : access unsigned;
      Offset : access unsigned)  -- install/include/clang-c/CXSourceLocation.h:219
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getSpellingLocation";

  --*
  -- * Retrieve the file, line, column, and offset represented by
  -- * the given source location.
  -- *
  -- * If the location refers into a macro expansion, return where the macro was
  -- * expanded or where the macro argument was written, if the location points at
  -- * a macro argument.
  -- *
  -- * \param location the location within a source file that will be decomposed
  -- * into its parts.
  -- *
  -- * \param file [out] if non-NULL, will be set to the file to which the given
  -- * source location points.
  -- *
  -- * \param line [out] if non-NULL, will be set to the line to which the given
  -- * source location points.
  -- *
  -- * \param column [out] if non-NULL, will be set to the column to which the given
  -- * source location points.
  -- *
  -- * \param offset [out] if non-NULL, will be set to the offset into the
  -- * buffer to which the given source location points.
  --  

   procedure Get_File_Location
     (Location : Source_Location_T;
      File : System.Address;
      Line : access unsigned;
      Column : access unsigned;
      Offset : access unsigned)  -- install/include/clang-c/CXSourceLocation.h:247
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getFileLocation";

  --*
  -- * Retrieve a source location representing the first character within a
  -- * source range.
  --  

   function Get_Range_Start (C_Range : Source_Range_T) return Source_Location_T  -- install/include/clang-c/CXSourceLocation.h:255
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getRangeStart";

  --*
  -- * Retrieve a source location representing the last character within a
  -- * source range.
  --  

   function Get_Range_End (C_Range : Source_Range_T) return Source_Location_T  -- install/include/clang-c/CXSourceLocation.h:261
   with Import => True, 
        Convention => C, 
        External_Name => "clang_getRangeEnd";

  --*
  -- * Identifies an array of ranges.
  --  

  --* The number of ranges in the \c ranges array.  
   type Source_Range_List_T is record
      count : aliased unsigned;  -- install/include/clang-c/CXSourceLocation.h:268
      ranges : access Source_Range_T;  -- install/include/clang-c/CXSourceLocation.h:272
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/CXSourceLocation.h:273

  --*
  --   * An array of \c CXSourceRanges.
  --    

  --*
  -- * Destroy the given \c CXSourceRangeList.
  --  

   procedure Dispose_Source_Range_List (Ranges : access Source_Range_List_T)  -- install/include/clang-c/CXSourceLocation.h:278
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeSourceRangeList";

  --*
  -- * @}
  --  

end Clang.CX_Source_Location;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
