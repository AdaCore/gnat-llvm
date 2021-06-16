pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.LLJIT is

   function Orc_LLJIT_Get_Triple_String
     (J : Orc_LLJIT_T)
      return String
   is
   begin
      return Value (Orc_LLJIT_Get_Triple_String_C (J));
   end Orc_LLJIT_Get_Triple_String;

   function Orc_LLJIT_Mangle_And_Intern
     (J              : Orc_LLJIT_T;
      Unmangled_Name : String)
      return LLVM.Orc.Orc_Symbol_String_Pool_Entry_T
   is
      Unmangled_Name_Array  : aliased char_array := To_C (Unmangled_Name);
      Unmangled_Name_String : constant chars_ptr := To_Chars_Ptr (Unmangled_Name_Array'Unchecked_Access);
   begin
      return Orc_LLJIT_Mangle_And_Intern_C (J, Unmangled_Name_String);
   end Orc_LLJIT_Mangle_And_Intern;

   function Orc_LLJIT_Lookup
     (J      : Orc_LLJIT_T;
      Result : access LLVM.Orc.Orc_JIT_Target_Address_T;
      Name   : String)
      return LLVM.Error.Error_T
   is
      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Orc_LLJIT_Lookup_C (J, Result, Name_String);
   end Orc_LLJIT_Lookup;

end LLVM.LLJIT;
