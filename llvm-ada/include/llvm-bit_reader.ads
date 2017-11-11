pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with System;

package LLVM.Bit_Reader is

   function Parse_Bitcode
     (Mem_Buf : LLVM.Types.Memory_Buffer_T;
      Out_Module : System.Address;
      Out_Message : System.Address) return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/BitReader.h:40
   pragma Import (C, Parse_Bitcode, "LLVMParseBitcode");

   function Parse_Bitcode2 (Mem_Buf : LLVM.Types.Memory_Buffer_T; Out_Module : System.Address) return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/BitReader.h:45
   pragma Import (C, Parse_Bitcode2, "LLVMParseBitcode2");

   function Parse_Bitcode_In_Context
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf : LLVM.Types.Memory_Buffer_T;
      Out_Module : System.Address;
      Out_Message : System.Address) return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/BitReader.h:49
   pragma Import (C, Parse_Bitcode_In_Context, "LLVMParseBitcodeInContext");

   function Parse_Bitcode_In_Context2
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf : LLVM.Types.Memory_Buffer_T;
      Out_Module : System.Address) return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/BitReader.h:53
   pragma Import (C, Parse_Bitcode_In_Context2, "LLVMParseBitcodeInContext2");

   function Get_Bitcode_Module_In_Context
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf : LLVM.Types.Memory_Buffer_T;
      Out_M : System.Address;
      Out_Message : System.Address) return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/BitReader.h:61
   pragma Import (C, Get_Bitcode_Module_In_Context, "LLVMGetBitcodeModuleInContext");

   function Get_Bitcode_Module_In_Context2
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf : LLVM.Types.Memory_Buffer_T;
      Out_M : System.Address) return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/BitReader.h:67
   pragma Import (C, Get_Bitcode_Module_In_Context2, "LLVMGetBitcodeModuleInContext2");

   function Get_Bitcode_Module
     (Mem_Buf : LLVM.Types.Memory_Buffer_T;
      Out_M : System.Address;
      Out_Message : System.Address) return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/BitReader.h:72
   pragma Import (C, Get_Bitcode_Module, "LLVMGetBitcodeModule");

   function Get_Bitcode_Module2 (Mem_Buf : LLVM.Types.Memory_Buffer_T; Out_M : System.Address) return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/BitReader.h:75
   pragma Import (C, Get_Bitcode_Module2, "LLVMGetBitcodeModule2");

end LLVM.Bit_Reader;

