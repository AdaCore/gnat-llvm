pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Blake_3 is

   function Blake_3_Version
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "llvm_blake3_version";
   function Blake_3_Version
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Blake_3_Version;
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Blake_3_Version;

   procedure Blake_3_Hasher_Init_Derive_Key
     (Self    : access Blake_3_Hasher_T;
      Context : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "llvm_blake3_hasher_init_derive_key";
   procedure Blake_3_Hasher_Init_Derive_Key
     (Self    : access Blake_3_Hasher_T;
      Context : String)
   is
      Context_Array  : aliased char_array := To_C (Context);
      Context_String : constant chars_ptr := To_Chars_Ptr (Context_Array'Unchecked_Access);
   begin
      Blake_3_Hasher_Init_Derive_Key (Self, Context_String);
   end Blake_3_Hasher_Init_Derive_Key;

end LLVM.Blake_3;
