pragma Ada_2005;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Disassembler is

   function Create_Disasm
     (Triple_Name    : String;
      Dis_Info       : System.Address;
      Tag_Type       : int;
      Get_Op_Info    : LLVM.Disassembler_Types.Op_Info_Callback_T;
      Symbol_Look_Up : LLVM.Disassembler_Types.Symbol_Lookup_Callback_T)
      return LLVM.Disassembler_Types.Disasm_Context_T
   is
      Triple_Name_Array  : aliased char_array := To_C (Triple_Name);
      Triple_Name_String : constant chars_ptr := To_Chars_Ptr (Triple_Name_Array'Unchecked_Access);
   begin
      return Create_Disasm_C (Triple_Name_String, Dis_Info, Tag_Type, Get_Op_Info, Symbol_Look_Up);
   end Create_Disasm;

   function Create_Disasm_CPU
     (Triple         : String;
      CPU            : String;
      Dis_Info       : System.Address;
      Tag_Type       : int;
      Get_Op_Info    : LLVM.Disassembler_Types.Op_Info_Callback_T;
      Symbol_Look_Up : LLVM.Disassembler_Types.Symbol_Lookup_Callback_T)
      return LLVM.Disassembler_Types.Disasm_Context_T
   is
      Triple_Array  : aliased char_array := To_C (Triple);
      Triple_String : constant chars_ptr := To_Chars_Ptr (Triple_Array'Unchecked_Access);
      CPU_Array     : aliased char_array := To_C (CPU);
      CPU_String    : constant chars_ptr := To_Chars_Ptr (CPU_Array'Unchecked_Access);
   begin
      return Create_Disasm_CPU_C (Triple_String, CPU_String, Dis_Info, Tag_Type, Get_Op_Info, Symbol_Look_Up);
   end Create_Disasm_CPU;

   function Create_Disasm_CPU_Features
     (Triple         : String;
      CPU            : String;
      Features       : String;
      Dis_Info       : System.Address;
      Tag_Type       : int;
      Get_Op_Info    : LLVM.Disassembler_Types.Op_Info_Callback_T;
      Symbol_Look_Up : LLVM.Disassembler_Types.Symbol_Lookup_Callback_T)
      return LLVM.Disassembler_Types.Disasm_Context_T
   is
      Triple_Array    : aliased char_array := To_C (Triple);
      Triple_String   : constant chars_ptr := To_Chars_Ptr (Triple_Array'Unchecked_Access);
      CPU_Array       : aliased char_array := To_C (CPU);
      CPU_String      : constant chars_ptr := To_Chars_Ptr (CPU_Array'Unchecked_Access);
      Features_Array  : aliased char_array := To_C (Features);
      Features_String : constant chars_ptr := To_Chars_Ptr (Features_Array'Unchecked_Access);
   begin
      return Create_Disasm_CPU_Features_C (Triple_String, CPU_String, Features_String, Dis_Info, Tag_Type, Get_Op_Info, Symbol_Look_Up);
   end Create_Disasm_CPU_Features;

   function Disasm_Instruction
     (DC              : LLVM.Disassembler_Types.Disasm_Context_T;
      Bytes           : x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
      Bytes_Size      : x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;
      PC              : x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;
      Out_String      : String;
      Out_String_Size : stddef_h.size_t)
      return stddef_h.size_t
   is
      Out_String_Array  : aliased char_array := To_C (Out_String);
      Out_String_String : constant chars_ptr := To_Chars_Ptr (Out_String_Array'Unchecked_Access);
   begin
      return Disasm_Instruction_C (DC, Bytes, Bytes_Size, PC, Out_String_String, Out_String_Size);
   end Disasm_Instruction;

end LLVM.Disassembler;
