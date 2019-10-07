pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Object is

   function Mach_O_Universal_Binary_Copy_Object_For_Arch
     (BR            : LLVM.Types.Binary_T;
      Arch          : String;
      Arch_Len      : stddef_h.size_t;
      Error_Message : System.Address)
      return LLVM.Types.Binary_T
   is
      Arch_Array  : aliased char_array := To_C (Arch);
      Arch_String : constant chars_ptr := To_Chars_Ptr (Arch_Array'Unchecked_Access);
   begin
      return Mach_O_Universal_Binary_Copy_Object_For_Arch_C (BR, Arch_String, Arch_Len, Error_Message);
   end Mach_O_Universal_Binary_Copy_Object_For_Arch;

   function Object_File_Is_Section_Iterator_At_End
     (BR : LLVM.Types.Binary_T;
      SI : Section_Iterator_T)
      return Boolean
   is
   begin
      return Object_File_Is_Section_Iterator_At_End_C (BR, SI) /= 0;
   end Object_File_Is_Section_Iterator_At_End;

   function Object_File_Is_Symbol_Iterator_At_End
     (BR : LLVM.Types.Binary_T;
      SI : Symbol_Iterator_T)
      return Boolean
   is
   begin
      return Object_File_Is_Symbol_Iterator_At_End_C (BR, SI) /= 0;
   end Object_File_Is_Symbol_Iterator_At_End;

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

   function Get_Section_Contains_Symbol
     (SI  : Section_Iterator_T;
      Sym : Symbol_Iterator_T)
      return Boolean
   is
   begin
      return Get_Section_Contains_Symbol_C (SI, Sym) /= 0;
   end Get_Section_Contains_Symbol;

   function Is_Relocation_Iterator_At_End
     (Section : Section_Iterator_T;
      RI      : Relocation_Iterator_T)
      return Boolean
   is
   begin
      return Is_Relocation_Iterator_At_End_C (Section, RI) /= 0;
   end Is_Relocation_Iterator_At_End;

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

   function Is_Section_Iterator_At_End
     (Object_File : Object_File_T;
      SI          : Section_Iterator_T)
      return Boolean
   is
   begin
      return Is_Section_Iterator_At_End_C (Object_File, SI) /= 0;
   end Is_Section_Iterator_At_End;

   function Is_Symbol_Iterator_At_End
     (Object_File : Object_File_T;
      SI          : Symbol_Iterator_T)
      return Boolean
   is
   begin
      return Is_Symbol_Iterator_At_End_C (Object_File, SI) /= 0;
   end Is_Symbol_Iterator_At_End;

end LLVM.Object;
