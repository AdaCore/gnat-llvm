pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Support is

   function Load_Library_Permanently
     (Filename : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMLoadLibraryPermanently";
   function Load_Library_Permanently
     (Filename : String)
      return LLVM.Types.Bool_T
   is
      Return_Value    : LLVM.Types.Bool_T;
      Filename_Array  : aliased char_array := To_C (Filename);
      Filename_String : constant chars_ptr := To_Chars_Ptr (Filename_Array'Unchecked_Access);
   begin
      Return_Value := Load_Library_Permanently (Filename_String);
      return Return_Value;
   end Load_Library_Permanently;

   function Load_Library_Permanently
     (Filename : String)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Load_Library_Permanently (Filename);
      return Return_Value /= 0;
   end Load_Library_Permanently;

   procedure Parse_Command_Line_Options
     (Argc     : int;
      Argv     : System.Address;
      Overview : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "LLVMParseCommandLineOptions";
   procedure Parse_Command_Line_Options
     (Argc     : int;
      Argv     : System.Address;
      Overview : String)
   is
      Overview_Array  : aliased char_array := To_C (Overview);
      Overview_String : constant chars_ptr := To_Chars_Ptr (Overview_Array'Unchecked_Access);
   begin
      Parse_Command_Line_Options (Argc, Argv, Overview_String);
   end Parse_Command_Line_Options;

   function Search_For_Address_Of_Symbol
     (Symbol_Name : Interfaces.C.Strings.chars_ptr)
      return System.Address
   with Import => True,
        Convention => C,
        External_Name => "LLVMSearchForAddressOfSymbol";
   function Search_For_Address_Of_Symbol
     (Symbol_Name : String)
      return System.Address
   is
      Return_Value       : System.Address;
      Symbol_Name_Array  : aliased char_array := To_C (Symbol_Name);
      Symbol_Name_String : constant chars_ptr := To_Chars_Ptr (Symbol_Name_Array'Unchecked_Access);
   begin
      Return_Value := Search_For_Address_Of_Symbol (Symbol_Name_String);
      return Return_Value;
   end Search_For_Address_Of_Symbol;

   procedure Add_Symbol
     (Symbol_Name  : Interfaces.C.Strings.chars_ptr;
      Symbol_Value : System.Address)
   with Import => True,
        Convention => C,
        External_Name => "LLVMAddSymbol";
   procedure Add_Symbol
     (Symbol_Name  : String;
      Symbol_Value : System.Address)
   is
      Symbol_Name_Array  : aliased char_array := To_C (Symbol_Name);
      Symbol_Name_String : constant chars_ptr := To_Chars_Ptr (Symbol_Name_Array'Unchecked_Access);
   begin
      Add_Symbol (Symbol_Name_String, Symbol_Value);
   end Add_Symbol;

end LLVM.Support;
