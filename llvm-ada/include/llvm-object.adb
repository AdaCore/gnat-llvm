pragma Ada_2005;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Object is

   function Get_Section_Name
     (SI : Section_Iterator_T)
      return String
   is
   begin
      return Value (Get_Section_Name_C (SI));
   end Get_Section_Name;

   function Get_Section_Contents
     (SI : Section_Iterator_T)
      return String
   is
   begin
      return Value (Get_Section_Contents_C (SI));
   end Get_Section_Contents;

   function Get_Symbol_Name
     (SI : Symbol_Iterator_T)
      return String
   is
   begin
      return Value (Get_Symbol_Name_C (SI));
   end Get_Symbol_Name;

   function Get_Relocation_Type_Name
     (RI : Relocation_Iterator_T)
      return String
   is
   begin
      return Value (Get_Relocation_Type_Name_C (RI));
   end Get_Relocation_Type_Name;

   function Get_Relocation_Value_String
     (RI : Relocation_Iterator_T)
      return String
   is
   begin
      return Value (Get_Relocation_Value_String_C (RI));
   end Get_Relocation_Value_String;

end LLVM.Object;
