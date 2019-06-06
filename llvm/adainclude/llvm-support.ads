pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with Interfaces.C.Strings;
with LLVM.Types;
with System;

package LLVM.Support is

  --===-- llvm-c/Support.h - Support C Interface --------------------*- C -*-===*|*                                                                            *|
  --|
  --|*                     The LLVM Compiler Infrastructure                       *|
  --|*                                                                            *|
  --|* This file is distributed under the University of Illinois Open Source      *|
  --|* License. See LICENSE.TXT for details.                                      *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This file defines the C interface to the LLVM support library.             *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * This function permanently loads the dynamic library at the given path.
  -- * It is safe to call this function multiple times for the same library.
  -- *
  -- * @see sys::DynamicLibrary::LoadLibraryPermanently()
  --   

   function Load_Library_Permanently
     (Filename : String)
      return Boolean;
   function Load_Library_Permanently_C
     (Filename : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Bool_T;  -- llvm-8.0.0.src/include/llvm-c/Support.h:30
   pragma Import (C, Load_Library_Permanently_C, "LLVMLoadLibraryPermanently");

  --*
  -- * This function parses the given arguments using the LLVM command line parser.
  -- * Note that the only stable thing about this function is its signature; you
  -- * cannot rely on any particular set of command line arguments being interpreted
  -- * the same way across LLVM versions.
  -- *
  -- * @see llvm::cl::ParseCommandLineOptions()
  --  

procedure Parse_Command_Line_Options
     (argc     : int;
      argv     : System.Address;
      Overview : String);
   procedure Parse_Command_Line_Options_C
     (argc     : int;
      argv     : System.Address;
      Overview : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Parse_Command_Line_Options_C, "LLVMParseCommandLineOptions");

  --*
  -- * This function will search through all previously loaded dynamic
  -- * libraries for the symbol \p symbolName. If it is found, the address of
  -- * that symbol is returned. If not, null is returned.
  -- *
  -- * @see sys::DynamicLibrary::SearchForAddressOfSymbol()
  --  

   function Search_For_Address_Of_Symbol
     (symbol_Name : String)
      return System.Address;
   function Search_For_Address_Of_Symbol_C
     (symbol_Name : Interfaces.C.Strings.chars_ptr)
      return System.Address;  -- llvm-8.0.0.src/include/llvm-c/Support.h:50
   pragma Import (C, Search_For_Address_Of_Symbol_C, "LLVMSearchForAddressOfSymbol");

  --*
  -- * This functions permanently adds the symbol \p symbolName with the
  -- * value \p symbolValue.  These symbols are searched before any
  -- * libraries.
  -- *
  -- * @see sys::DynamicLibrary::AddSymbol()
  --  

   procedure Add_Symbol
     (symbol_Name  : String;
      symbol_Value : System.Address);
   procedure Add_Symbol_C
     (symbol_Name  : Interfaces.C.Strings.chars_ptr;
      symbol_Value : System.Address);  -- llvm-8.0.0.src/include/llvm-c/Support.h:59
   pragma Import (C, Add_Symbol_C, "LLVMAddSymbol");

end LLVM.Support;

