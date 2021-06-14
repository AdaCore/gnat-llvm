pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body Clang.CX_String is

   function Get_C_String
     (Str : String_T)
      return String
   is
   begin
      return Value (Get_C_String_C (Str));
   end Get_C_String;

end Clang.CX_String;
