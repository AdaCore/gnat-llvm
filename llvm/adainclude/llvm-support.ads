pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with Interfaces.C.Strings;
with LLVM.Types;
with System;

package LLVM.Support is

  --===-- llvm-c/Support.h - Support C Interface --------------------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This file defines the C interface to the LLVM support library.             *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @addtogroup LLVMCCore
  -- *
  -- * @{
  --  

  --*
  -- * This function permanently loads the dynamic library at the given path.
  -- * It is safe to call this function multiple times for the same library.
  -- *
  -- * @see sys::DynamicLibrary::LoadLibraryPermanently()
  --   

function Load_Library_Permanently
     (Filename : String)
      return Boolean;

  --*
  -- * This function parses the given arguments using the LLVM command line parser.
  -- * Note that the only stable thing about this function is its signature; you
  -- * cannot rely on any particular set of command line arguments being interpreted
  -- * the same way across LLVM versions.
  -- *
  -- * @see llvm::cl::ParseCommandLineOptions()
  --  

procedure Parse_Command_Line_Options
     (Argc     : int;
      Argv     : System.Address;
      Overview : String);

  --*
  -- * This function will search through all previously loaded dynamic
  -- * libraries for the symbol \p symbolName. If it is found, the address of
  -- * that symbol is returned. If not, null is returned.
  -- *
  -- * @see sys::DynamicLibrary::SearchForAddressOfSymbol()
  --  

function Search_For_Address_Of_Symbol
     (Symbol_Name : String)
      return System.Address;

  --*
  -- * This functions permanently adds the symbol \p symbolName with the
  -- * value \p symbolValue.  These symbols are searched before any
  -- * libraries.
  -- *
  -- * @see sys::DynamicLibrary::AddSymbol()
  --  

procedure Add_Symbol
     (Symbol_Name  : String;
      Symbol_Value : System.Address);

  --*
  -- * @}
  --  

end LLVM.Support;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
