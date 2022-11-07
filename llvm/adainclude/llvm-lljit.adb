pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.LLJIT is

   function Orc_LLJIT_Get_Triple_String
     (J : Orc_LLJIT_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcLLJITGetTripleString";
   function Orc_LLJIT_Get_Triple_String
     (J : Orc_LLJIT_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Orc_LLJIT_Get_Triple_String (J);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Orc_LLJIT_Get_Triple_String;

   function Orc_LLJIT_Mangle_And_Intern
     (J              : Orc_LLJIT_T;
      Unmangled_Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Orc.Orc_Symbol_String_Pool_Entry_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcLLJITMangleAndIntern";
   function Orc_LLJIT_Mangle_And_Intern
     (J              : Orc_LLJIT_T;
      Unmangled_Name : String)
      return LLVM.Orc.Orc_Symbol_String_Pool_Entry_T
   is
      Return_Value          : LLVM.Orc.Orc_Symbol_String_Pool_Entry_T;
      Unmangled_Name_Array  : aliased char_array := To_C (Unmangled_Name);
      Unmangled_Name_String : constant chars_ptr := To_Chars_Ptr (Unmangled_Name_Array'Unchecked_Access);
   begin
      Return_Value := Orc_LLJIT_Mangle_And_Intern (J, Unmangled_Name_String);
      return Return_Value;
   end Orc_LLJIT_Mangle_And_Intern;

   function Orc_LLJIT_Lookup
     (J      : Orc_LLJIT_T;
      Result : access LLVM.Orc.Orc_Executor_Address_T;
      Name   : Interfaces.C.Strings.chars_ptr)
      return LLVM.Error.Error_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcLLJITLookup";
   function Orc_LLJIT_Lookup
     (J      : Orc_LLJIT_T;
      Result : access LLVM.Orc.Orc_Executor_Address_T;
      Name   : String)
      return LLVM.Error.Error_T
   is
      Return_Value : LLVM.Error.Error_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Orc_LLJIT_Lookup (J, Result, Name_String);
      return Return_Value;
   end Orc_LLJIT_Lookup;

   function Orc_LLJIT_Get_Data_Layout_Str
     (J : Orc_LLJIT_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcLLJITGetDataLayoutStr";
   function Orc_LLJIT_Get_Data_Layout_Str
     (J : Orc_LLJIT_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Orc_LLJIT_Get_Data_Layout_Str (J);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Orc_LLJIT_Get_Data_Layout_Str;

end LLVM.LLJIT;
