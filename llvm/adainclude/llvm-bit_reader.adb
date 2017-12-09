pragma Ada_2005;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Bit_Reader is

   function Parse_Bitcode
     (Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_Module  : System.Address;
      Out_Message : System.Address)
      return Boolean
   is
   begin
      return Parse_Bitcode_C (Mem_Buf, Out_Module, Out_Message) /= 0;
   end Parse_Bitcode;

   function Parse_Bitcode2
     (Mem_Buf    : LLVM.Types.Memory_Buffer_T;
      Out_Module : System.Address)
      return Boolean
   is
   begin
      return Parse_Bitcode2_C (Mem_Buf, Out_Module) /= 0;
   end Parse_Bitcode2;

   function Parse_Bitcode_In_Context
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_Module  : System.Address;
      Out_Message : System.Address)
      return Boolean
   is
   begin
      return Parse_Bitcode_In_Context_C (Context_Ref, Mem_Buf, Out_Module, Out_Message) /= 0;
   end Parse_Bitcode_In_Context;

   function Parse_Bitcode_In_Context2
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_Module  : System.Address)
      return Boolean
   is
   begin
      return Parse_Bitcode_In_Context2_C (Context_Ref, Mem_Buf, Out_Module) /= 0;
   end Parse_Bitcode_In_Context2;

   function Get_Bitcode_Module_In_Context
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address;
      Out_Message : System.Address)
      return Boolean
   is
   begin
      return Get_Bitcode_Module_In_Context_C (Context_Ref, Mem_Buf, Out_M, Out_Message) /= 0;
   end Get_Bitcode_Module_In_Context;

   function Get_Bitcode_Module_In_Context2
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address)
      return Boolean
   is
   begin
      return Get_Bitcode_Module_In_Context2_C (Context_Ref, Mem_Buf, Out_M) /= 0;
   end Get_Bitcode_Module_In_Context2;

   function Get_Bitcode_Module
     (Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address;
      Out_Message : System.Address)
      return Boolean
   is
   begin
      return Get_Bitcode_Module_C (Mem_Buf, Out_M, Out_Message) /= 0;
   end Get_Bitcode_Module;

   function Get_Bitcode_Module2
     (Mem_Buf : LLVM.Types.Memory_Buffer_T;
      Out_M   : System.Address)
      return Boolean
   is
   begin
      return Get_Bitcode_Module2_C (Mem_Buf, Out_M) /= 0;
   end Get_Bitcode_Module2;

end LLVM.Bit_Reader;
