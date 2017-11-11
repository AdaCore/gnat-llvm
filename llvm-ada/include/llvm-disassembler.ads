pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with stdint_h;
with Interfaces.C.Strings;
with stddef_h;

package LLVM.Disassembler is

   LLVMDisassembler_VariantKind_None : constant := 0;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:94

   LLVMDisassembler_VariantKind_ARM_HI16 : constant := 1;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:99
   LLVMDisassembler_VariantKind_ARM_LO16 : constant := 2;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:100

   LLVMDisassembler_VariantKind_ARM64_PAGE : constant := 1;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:105
   LLVMDisassembler_VariantKind_ARM64_PAGEOFF : constant := 2;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:106
   LLVMDisassembler_VariantKind_ARM64_GOTPAGE : constant := 3;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:107
   LLVMDisassembler_VariantKind_ARM64_GOTPAGEOFF : constant := 4;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:108
   LLVMDisassembler_VariantKind_ARM64_TLVP : constant := 5;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:109
   LLVMDisassembler_VariantKind_ARM64_TLVOFF : constant := 6;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:110

   LLVMDisassembler_ReferenceType_InOut_None : constant := 0;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:133

   LLVMDisassembler_ReferenceType_In_Branch : constant := 1;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:136

   LLVMDisassembler_ReferenceType_In_PCrel_Load : constant := 2;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:138

   LLVMDisassembler_ReferenceType_In_ARM64_ADRP : constant := 16#100000001#;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:141

   LLVMDisassembler_ReferenceType_In_ARM64_ADDXri : constant := 16#100000002#;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:143

   LLVMDisassembler_ReferenceType_In_ARM64_LDRXui : constant := 16#100000003#;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:145

   LLVMDisassembler_ReferenceType_In_ARM64_LDRXl : constant := 16#100000004#;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:147

   LLVMDisassembler_ReferenceType_In_ARM64_ADR : constant := 16#100000005#;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:149

   LLVMDisassembler_ReferenceType_Out_SymbolStub : constant := 1;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:152

   LLVMDisassembler_ReferenceType_Out_LitPool_SymAddr : constant := 2;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:154

   LLVMDisassembler_ReferenceType_Out_LitPool_CstrAddr : constant := 3;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:156

   LLVMDisassembler_ReferenceType_Out_Objc_CFString_Ref : constant := 4;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:159

   LLVMDisassembler_ReferenceType_Out_Objc_Message : constant := 5;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:161

   LLVMDisassembler_ReferenceType_Out_Objc_Message_Ref : constant := 6;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:163

   LLVMDisassembler_ReferenceType_Out_Objc_Selector_Ref : constant := 7;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:165

   LLVMDisassembler_ReferenceType_Out_Objc_Class_Ref : constant := 8;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:167

   LLVMDisassembler_ReferenceType_DeMangled_Name : constant := 9;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:170

   LLVMDisassembler_Option_UseMarkup : constant := 1;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:221

   LLVMDisassembler_Option_PrintImmHex : constant := 2;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:223

   LLVMDisassembler_Option_AsmPrinterVariant : constant := 4;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:225

   LLVMDisassembler_Option_SetInstrComments : constant := 8;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:227

   LLVMDisassembler_Option_PrintLatency : constant := 16;  --  /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:229

   type Disasm_Context_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:35

   type Op_Info_Callback_T is access function 
        (arg1 : System.Address;
         arg2 : stdint_h.uint64_t;
         arg3 : stdint_h.uint64_t;
         arg4 : stdint_h.uint64_t;
         arg5 : int;
         arg6 : System.Address) return int;
   pragma Convention (C, Op_Info_Callback_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:54

   type Op_Info_Symbol1_T is record
      Present : aliased stdint_h.uint64_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:79
      Name : Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:80
      Value : aliased stdint_h.uint64_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:81
   end record;
   pragma Convention (C_Pass_By_Copy, Op_Info_Symbol1_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:78

   type Op_Info1_T is record
      AddSymbol : aliased Op_Info_Symbol1_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:85
      SubtractSymbol : aliased Op_Info_Symbol1_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:86
      Value : aliased stdint_h.uint64_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:87
      VariantKind : aliased stdint_h.uint64_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:88
   end record;
   pragma Convention (C_Pass_By_Copy, Op_Info1_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:84

   type Symbol_Lookup_Callback_T is access function 
        (arg1 : System.Address;
         arg2 : stdint_h.uint64_t;
         arg3 : access stdint_h.uint64_t;
         arg4 : stdint_h.uint64_t;
         arg5 : System.Address) return Interfaces.C.Strings.chars_ptr;
   pragma Convention (C, Symbol_Lookup_Callback_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:124

function Create_Disasm
     (Triple_Name    : String;
      Dis_Info       : System.Address;
      Tag_Type       : int;
      Get_Op_Info    : Op_Info_Callback_T;
      Symbol_Look_Up : Symbol_Lookup_Callback_T)
      return Disasm_Context_T;
   function Create_Disasm_C
     (Triple_Name    : Interfaces.C.Strings.chars_ptr;
      Dis_Info       : System.Address;
      Tag_Type       : int;
      Get_Op_Info    : Op_Info_Callback_T;
      Symbol_Look_Up : Symbol_Lookup_Callback_T)
      return Disasm_Context_T;
   pragma Import (C, Create_Disasm_C, "LLVMCreateDisasm");

function Create_Disasm_CPU
     (Triple         : String;
      CPU            : String;
      Dis_Info       : System.Address;
      Tag_Type       : int;
      Get_Op_Info    : Op_Info_Callback_T;
      Symbol_Look_Up : Symbol_Lookup_Callback_T)
      return Disasm_Context_T;
   function Create_Disasm_CPU_C
     (Triple         : Interfaces.C.Strings.chars_ptr;
      CPU            : Interfaces.C.Strings.chars_ptr;
      Dis_Info       : System.Address;
      Tag_Type       : int;
      Get_Op_Info    : Op_Info_Callback_T;
      Symbol_Look_Up : Symbol_Lookup_Callback_T)
      return Disasm_Context_T;
   pragma Import (C, Create_Disasm_CPU_C, "LLVMCreateDisasmCPU");

function Create_Disasm_CPU_Features
     (Triple         : String;
      CPU            : String;
      Features       : String;
      Dis_Info       : System.Address;
      Tag_Type       : int;
      Get_Op_Info    : Op_Info_Callback_T;
      Symbol_Look_Up : Symbol_Lookup_Callback_T)
      return Disasm_Context_T;
   function Create_Disasm_CPU_Features_C
     (Triple         : Interfaces.C.Strings.chars_ptr;
      CPU            : Interfaces.C.Strings.chars_ptr;
      Features       : Interfaces.C.Strings.chars_ptr;
      Dis_Info       : System.Address;
      Tag_Type       : int;
      Get_Op_Info    : Op_Info_Callback_T;
      Symbol_Look_Up : Symbol_Lookup_Callback_T)
      return Disasm_Context_T;
   pragma Import (C, Create_Disasm_CPU_Features_C, "LLVMCreateDisasmCPUFeatures");

   function Set_Disasm_Options (DC : Disasm_Context_T; Options : stdint_h.uint64_t) return int;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:218
   pragma Import (C, Set_Disasm_Options, "LLVMSetDisasmOptions");

   procedure Disasm_Dispose (DC : Disasm_Context_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Disassembler.h:234
   pragma Import (C, Disasm_Dispose, "LLVMDisasmDispose");

function Disasm_Instruction
     (DC              : Disasm_Context_T;
      Bytes           : stdint_h.uint8_t;
      Bytes_Size      : stdint_h.uint64_t;
      PC              : stdint_h.uint64_t;
      Out_String      : String;
      Out_String_Size : stddef_h.size_t)
      return stddef_h.size_t;
   function Disasm_Instruction_C
     (DC              : Disasm_Context_T;
      Bytes           : stdint_h.uint8_t;
      Bytes_Size      : stdint_h.uint64_t;
      PC              : stdint_h.uint64_t;
      Out_String      : Interfaces.C.Strings.chars_ptr;
      Out_String_Size : stddef_h.size_t)
      return stddef_h.size_t;
   pragma Import (C, Disasm_Instruction_C, "LLVMDisasmInstruction");

end LLVM.Disassembler;

