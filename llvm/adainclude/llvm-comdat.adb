pragma Ada_2005;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Comdat is

   function Get_Or_Insert_Comdat
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Comdat_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Get_Or_Insert_Comdat_C (M, Name_String);
   end Get_Or_Insert_Comdat;

end LLVM.Comdat;
