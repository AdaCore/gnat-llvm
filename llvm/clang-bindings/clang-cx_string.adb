pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body Clang.CX_String is

   function Get_C_String
     (Str : String_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "clang_getCString";
   function Get_C_String
     (Str : String_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_C_String (Str);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_C_String;

end Clang.CX_String;
