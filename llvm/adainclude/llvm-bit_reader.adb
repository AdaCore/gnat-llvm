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
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMParseBitcode";
   function Parse_Bitcode
     (Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_Module  : System.Address;
      Out_Message : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Parse_Bitcode (Mem_Buf, Out_Module, Out_Message);
      return Return_Value /= 0;
   end Parse_Bitcode;

   function Parse_Bitcode_2
     (Mem_Buf    : LLVM.Types.Memory_Buffer_T;
      Out_Module : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMParseBitcode2";
   function Parse_Bitcode_2
     (Mem_Buf    : LLVM.Types.Memory_Buffer_T;
      Out_Module : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Parse_Bitcode_2 (Mem_Buf, Out_Module);
      return Return_Value /= 0;
   end Parse_Bitcode_2;

   function Parse_Bitcode_In_Context
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_Module  : System.Address;
      Out_Message : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMParseBitcodeInContext";
   function Parse_Bitcode_In_Context
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_Module  : System.Address;
      Out_Message : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Parse_Bitcode_In_Context (Context_Ref, Mem_Buf, Out_Module, Out_Message);
      return Return_Value /= 0;
   end Parse_Bitcode_In_Context;

   function Parse_Bitcode_In_Context_2
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_Module  : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMParseBitcodeInContext2";
   function Parse_Bitcode_In_Context_2
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_Module  : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Parse_Bitcode_In_Context_2 (Context_Ref, Mem_Buf, Out_Module);
      return Return_Value /= 0;
   end Parse_Bitcode_In_Context_2;

   function Get_Bitcode_Module_In_Context
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address;
      Out_Message : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetBitcodeModuleInContext";
   function Get_Bitcode_Module_In_Context
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address;
      Out_Message : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Get_Bitcode_Module_In_Context (Context_Ref, Mem_Buf, Out_M, Out_Message);
      return Return_Value /= 0;
   end Get_Bitcode_Module_In_Context;

   function Get_Bitcode_Module_In_Context_2
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetBitcodeModuleInContext2";
   function Get_Bitcode_Module_In_Context_2
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Get_Bitcode_Module_In_Context_2 (Context_Ref, Mem_Buf, Out_M);
      return Return_Value /= 0;
   end Get_Bitcode_Module_In_Context_2;

   function Get_Bitcode_Module
     (Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address;
      Out_Message : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetBitcodeModule";
   function Get_Bitcode_Module
     (Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address;
      Out_Message : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Get_Bitcode_Module (Mem_Buf, Out_M, Out_Message);
      return Return_Value /= 0;
   end Get_Bitcode_Module;

   function Get_Bitcode_Module_2
     (Mem_Buf : LLVM.Types.Memory_Buffer_T;
      Out_M   : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetBitcodeModule2";
   function Get_Bitcode_Module_2
     (Mem_Buf : LLVM.Types.Memory_Buffer_T;
      Out_M   : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Get_Bitcode_Module_2 (Mem_Buf, Out_M);
      return Return_Value /= 0;
   end Get_Bitcode_Module_2;

end LLVM.Bit_Reader;
