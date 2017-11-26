pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with Interfaces.C.Strings;
with LLVM.Types;
with System;

package LLVM.Support is

   function Load_Library_Permanently
     (Filename : String)
      return Boolean;
   function Load_Library_Permanently_C
     (Filename : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Support.h:30
   pragma Import (C, Load_Library_Permanently_C, "LLVMLoadLibraryPermanently");

procedure Parse_Command_Line_Options
     (argc     : int;
      argv     : System.Address;
      Overview : String);
   procedure Parse_Command_Line_Options_C
     (argc     : int;
      argv     : System.Address;
      Overview : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Parse_Command_Line_Options_C, "LLVMParseCommandLineOptions");

   function Search_For_Address_Of_Symbol
     (symbol_Name : String)
      return System.Address;
   function Search_For_Address_Of_Symbol_C
     (symbol_Name : Interfaces.C.Strings.chars_ptr)
      return System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Support.h:50
   pragma Import (C, Search_For_Address_Of_Symbol_C, "LLVMSearchForAddressOfSymbol");

   procedure Add_Symbol
     (symbol_Name  : String;
      symbol_Value : System.Address);
   procedure Add_Symbol_C
     (symbol_Name  : Interfaces.C.Strings.chars_ptr;
      symbol_Value : System.Address);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Support.h:59
   pragma Import (C, Add_Symbol_C, "LLVMAddSymbol");

end LLVM.Support;

