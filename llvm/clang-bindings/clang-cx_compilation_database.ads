pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with Interfaces.C.Strings;
with Clang.CX_String;

package Clang.CX_Compilation_Database is

  --===-- clang-c/CXCompilationDatabase.h - Compilation database  ---*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header provides a public interface to use CompilationDatabase without *|
  --|* the full Clang C++ API.                                                    *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --* \defgroup COMPILATIONDB CompilationDatabase functions
  -- * \ingroup CINDEX
  -- *
  -- * @{
  --  

  --*
  -- * A compilation database holds all information used to compile files in a
  -- * project. For each file in the database, it can be queried for the working
  -- * directory or the command line used for the compiler invocation.
  -- *
  -- * Must be freed by \c clang_CompilationDatabase_dispose
  --  

   type Compilation_Database_T is new System.Address;  -- install/include/clang-c/CXCompilationDatabase.h:37

  --*
  -- * Contains the results of a search in the compilation database
  -- *
  -- * When searching for the compile command for a file, the compilation db can
  -- * return several commands, as the file may have been compiled with
  -- * different options in different places of the project. This choice of compile
  -- * commands is wrapped in this opaque data structure. It must be freed by
  -- * \c clang_CompileCommands_dispose.
  --  

   type Compile_Commands_T is new System.Address;  -- install/include/clang-c/CXCompilationDatabase.h:48

  --*
  -- * Represents the command line invocation to compile a specific file.
  --  

   type Compile_Command_T is new System.Address;  -- install/include/clang-c/CXCompilationDatabase.h:53

  --*
  -- * Error codes for Compilation Database
  --  

  --   * No error occurred
  --    

  --   * Database can not be loaded
  --    

   type Compilation_Database_Error_T is 
     (Compilation_Database_No_Error,
      Compilation_Database_Can_Not_Load_Database)
   with Convention => C;  -- install/include/clang-c/CXCompilationDatabase.h:69

  --*
  -- * Creates a compilation database from the database found in directory
  -- * buildDir. For example, CMake can output a compile_commands.json which can
  -- * be used to build the database.
  -- *
  -- * It must be freed by \c clang_CompilationDatabase_dispose.
  --  

function Compilation_Database_From_Directory
     (Build_Dir  : String;
      Error_Code : access Compilation_Database_Error_T)
      return Compilation_Database_T;

  --*
  -- * Free the given compilation database
  --  

   procedure Compilation_Database_Dispose (Arg_1 : Compilation_Database_T)  -- install/include/clang-c/CXCompilationDatabase.h:86
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompilationDatabase_dispose";

  --*
  -- * Find the compile commands used for a file. The compile commands
  -- * must be freed by \c clang_CompileCommands_dispose.
  --  

function Compilation_Database_Get_Compile_Commands
     (Arg_1              : Compilation_Database_T;
      Complete_File_Name : String)
      return Compile_Commands_T;

  --*
  -- * Get all the compile commands in the given compilation database.
  --  

   function Compilation_Database_Get_All_Compile_Commands (Arg_1 : Compilation_Database_T) return Compile_Commands_T  -- install/include/clang-c/CXCompilationDatabase.h:100
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompilationDatabase_getAllCompileCommands";

  --*
  -- * Free the given CompileCommands
  --  

   procedure Compile_Commands_Dispose (Arg_1 : Compile_Commands_T)  -- install/include/clang-c/CXCompilationDatabase.h:105
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommands_dispose";

  --*
  -- * Get the number of CompileCommand we have for a file
  --  

   function Compile_Commands_Get_Size (Arg_1 : Compile_Commands_T) return unsigned  -- install/include/clang-c/CXCompilationDatabase.h:111
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommands_getSize";

  --*
  -- * Get the I'th CompileCommand for a file
  -- *
  -- * Note : 0 <= i < clang_CompileCommands_getSize(CXCompileCommands)
  --  

   function Compile_Commands_Get_Command (Arg_1 : Compile_Commands_T; I : unsigned) return Compile_Command_T  -- install/include/clang-c/CXCompilationDatabase.h:119
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommands_getCommand";

  --*
  -- * Get the working directory where the CompileCommand was executed from
  --  

function Compile_Command_Get_Directory
     (Arg_1 : Compile_Command_T)
      return String;

  --*
  -- * Get the filename associated with the CompileCommand.
  --  

function Compile_Command_Get_Filename
     (Arg_1 : Compile_Command_T)
      return String;

  --*
  -- * Get the number of arguments in the compiler invocation.
  -- *
  --  

   function Compile_Command_Get_Num_Args (Arg_1 : Compile_Command_T) return unsigned  -- install/include/clang-c/CXCompilationDatabase.h:138
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommand_getNumArgs";

  --*
  -- * Get the I'th argument value in the compiler invocations
  -- *
  -- * Invariant :
  -- *  - argument 0 is the compiler executable
  --  

function Compile_Command_Get_Arg
     (Arg_1 : Compile_Command_T;
      I     : unsigned)
      return String;

  --*
  -- * Get the number of source mappings for the compiler invocation.
  --  

   function Compile_Command_Get_Num_Mapped_Sources (Arg_1 : Compile_Command_T) return unsigned  -- install/include/clang-c/CXCompilationDatabase.h:153
   with Import => True, 
        Convention => C, 
        External_Name => "clang_CompileCommand_getNumMappedSources";

  --*
  -- * Get the I'th mapped source path for the compiler invocation.
  --  

function Compile_Command_Get_Mapped_Source_Path
     (Arg_1 : Compile_Command_T;
      I     : unsigned)
      return String;

  --*
  -- * Get the I'th mapped source content for the compiler invocation.
  --  

function Compile_Command_Get_Mapped_Source_Content
     (Arg_1 : Compile_Command_T;
      I     : unsigned)
      return String;

  --*
  -- * @}
  --  

end Clang.CX_Compilation_Database;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
