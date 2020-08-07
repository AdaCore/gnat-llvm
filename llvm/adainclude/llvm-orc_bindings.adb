pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Orc_Bindings is

   function Orc_Get_Error_Msg
     (JIT_Stack : Orc_JIT_Stack_T)
      return String
   is
   begin
      return Value (Orc_Get_Error_Msg_C (JIT_Stack));
   end Orc_Get_Error_Msg;

   function Orc_Create_Indirect_Stub
     (JIT_Stack : Orc_JIT_Stack_T;
      Stub_Name : String;
      Init_Addr : Orc_Target_Address_T)
      return LLVM.Error.Error_T
   is
      Stub_Name_Array  : aliased char_array := To_C (Stub_Name);
      Stub_Name_String : constant chars_ptr := To_Chars_Ptr (Stub_Name_Array'Unchecked_Access);
   begin
      return Orc_Create_Indirect_Stub_C (JIT_Stack, Stub_Name_String, Init_Addr);
   end Orc_Create_Indirect_Stub;

   function Orc_Set_Indirect_Stub_Pointer
     (JIT_Stack : Orc_JIT_Stack_T;
      Stub_Name : String;
      New_Addr  : Orc_Target_Address_T)
      return LLVM.Error.Error_T
   is
      Stub_Name_Array  : aliased char_array := To_C (Stub_Name);
      Stub_Name_String : constant chars_ptr := To_Chars_Ptr (Stub_Name_Array'Unchecked_Access);
   begin
      return Orc_Set_Indirect_Stub_Pointer_C (JIT_Stack, Stub_Name_String, New_Addr);
   end Orc_Set_Indirect_Stub_Pointer;

   function Orc_Get_Symbol_Address
     (JIT_Stack   : Orc_JIT_Stack_T;
      Ret_Addr    : access Orc_Target_Address_T;
      Symbol_Name : String)
      return LLVM.Error.Error_T
   is
      Symbol_Name_Array  : aliased char_array := To_C (Symbol_Name);
      Symbol_Name_String : constant chars_ptr := To_Chars_Ptr (Symbol_Name_Array'Unchecked_Access);
   begin
      return Orc_Get_Symbol_Address_C (JIT_Stack, Ret_Addr, Symbol_Name_String);
   end Orc_Get_Symbol_Address;

   function Orc_Get_Symbol_Address_In
     (JIT_Stack   : Orc_JIT_Stack_T;
      Ret_Addr    : access Orc_Target_Address_T;
      H           : Orc_Module_Handle_T;
      Symbol_Name : String)
      return LLVM.Error.Error_T
   is
      Symbol_Name_Array  : aliased char_array := To_C (Symbol_Name);
      Symbol_Name_String : constant chars_ptr := To_Chars_Ptr (Symbol_Name_Array'Unchecked_Access);
   begin
      return Orc_Get_Symbol_Address_In_C (JIT_Stack, Ret_Addr, H, Symbol_Name_String);
   end Orc_Get_Symbol_Address_In;

   procedure Orc_Get_Mangled_Symbol
     (JIT_Stack      : Orc_JIT_Stack_T;
      Mangled_Symbol : System.Address;
      Symbol         : String)
   is
      Symbol_Array  : aliased char_array := To_C (Symbol);
      Symbol_String : constant chars_ptr := To_Chars_Ptr (Symbol_Array'Unchecked_Access);
   begin
      Orc_Get_Mangled_Symbol_C (JIT_Stack, Mangled_Symbol, Symbol_String);
   end Orc_Get_Mangled_Symbol;

   procedure Orc_Dispose_Mangled_Symbol
     (Mangled_Symbol : String)
   is
      Mangled_Symbol_Array  : aliased char_array := To_C (Mangled_Symbol);
      Mangled_Symbol_String : constant chars_ptr := To_Chars_Ptr (Mangled_Symbol_Array'Unchecked_Access);
   begin
      Orc_Dispose_Mangled_Symbol_C (Mangled_Symbol_String);
   end Orc_Dispose_Mangled_Symbol;

end LLVM.Orc_Bindings;
