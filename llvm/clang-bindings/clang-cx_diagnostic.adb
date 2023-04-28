pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body Clang.CX_Diagnostic is

   function Load_Diagnostics
     (File         : Interfaces.C.Strings.chars_ptr;
      Error        : access Load_Diag_Error_T;
      Error_String : access Clang.CX_String.String_T)
      return Diagnostic_Set_T
   with Import => True,
        Convention => C,
        External_Name => "clang_loadDiagnostics";
   function Load_Diagnostics
     (File         : String;
      Error        : access Load_Diag_Error_T;
      Error_String : access Clang.CX_String.String_T)
      return Diagnostic_Set_T
   is
      Return_Value : Diagnostic_Set_T;
      File_Array   : aliased char_array := To_C (File);
      File_String  : constant chars_ptr := To_Chars_Ptr (File_Array'Unchecked_Access);
   begin
      Return_Value := Load_Diagnostics (File_String, Error, Error_String);
      return Return_Value;
   end Load_Diagnostics;

   function Format_Diagnostic
     (Diagnostic : Diagnostic_T;
      Options    : unsigned)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_formatDiagnostic";
   function Format_Diagnostic
     (Diagnostic : Diagnostic_T;
      Options    : unsigned)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Format_Diagnostic (Diagnostic, Options);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Format_Diagnostic;

   function Get_Diagnostic_Spelling
     (Arg_1 : Diagnostic_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getDiagnosticSpelling";
   function Get_Diagnostic_Spelling
     (Arg_1 : Diagnostic_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Diagnostic_Spelling (Arg_1);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Diagnostic_Spelling;

   function Get_Diagnostic_Option
     (Diag    : Diagnostic_T;
      Disable : access Clang.CX_String.String_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getDiagnosticOption";
   function Get_Diagnostic_Option
     (Diag    : Diagnostic_T;
      Disable : access Clang.CX_String.String_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Diagnostic_Option (Diag, Disable);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Diagnostic_Option;

   function Get_Diagnostic_Category_Name
     (Category : unsigned)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getDiagnosticCategoryName";
   function Get_Diagnostic_Category_Name
     (Category : unsigned)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Diagnostic_Category_Name (Category);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Diagnostic_Category_Name;

   function Get_Diagnostic_Category_Text
     (Arg_1 : Diagnostic_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getDiagnosticCategoryText";
   function Get_Diagnostic_Category_Text
     (Arg_1 : Diagnostic_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Diagnostic_Category_Text (Arg_1);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Diagnostic_Category_Text;

   function Get_Diagnostic_Fix_It
     (Diagnostic        : Diagnostic_T;
      Fix_It            : unsigned;
      Replacement_Range : access Clang.CX_Source_Location.Source_Range_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getDiagnosticFixIt";
   function Get_Diagnostic_Fix_It
     (Diagnostic        : Diagnostic_T;
      Fix_It            : unsigned;
      Replacement_Range : access Clang.CX_Source_Location.Source_Range_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Diagnostic_Fix_It (Diagnostic, Fix_It, Replacement_Range);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Diagnostic_Fix_It;

end Clang.CX_Diagnostic;
