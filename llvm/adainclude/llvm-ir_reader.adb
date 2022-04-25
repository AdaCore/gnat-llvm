pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.IR_Reader is

   function Parse_IR_In_Context
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address;
      Out_Message : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMParseIRInContext";
   function Parse_IR_In_Context
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address;
      Out_Message : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Parse_IR_In_Context (Context_Ref, Mem_Buf, Out_M, Out_Message);
      return Return_Value /= 0;
   end Parse_IR_In_Context;

end LLVM.IR_Reader;
