pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with stdint_h;
with Interfaces.C.Strings;
with stddef_h;

package LLVM.Disassembler is

   LLVMDisassembler_VariantKind_None : constant := 0;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:94

   LLVMDisassembler_VariantKind_ARM_HI16 : constant := 1;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:99
   LLVMDisassembler_VariantKind_ARM_LO16 : constant := 2;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:100

   LLVMDisassembler_VariantKind_ARM64_PAGE : constant := 1;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:105
   LLVMDisassembler_VariantKind_ARM64_PAGEOFF : constant := 2;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:106
   LLVMDisassembler_VariantKind_ARM64_GOTPAGE : constant := 3;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:107
   LLVMDisassembler_VariantKind_ARM64_GOTPAGEOFF : constant := 4;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:108
   LLVMDisassembler_VariantKind_ARM64_TLVP : constant := 5;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:109
   LLVMDisassembler_VariantKind_ARM64_TLVOFF : constant := 6;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:110

   LLVMDisassembler_ReferenceType_InOut_None : constant := 0;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:133

   LLVMDisassembler_ReferenceType_In_Branch : constant := 1;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:136

   LLVMDisassembler_ReferenceType_In_PCrel_Load : constant := 2;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:138

   LLVMDisassembler_ReferenceType_In_ARM64_ADRP : constant := 16#100000001#;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:141

   LLVMDisassembler_ReferenceType_In_ARM64_ADDXri : constant := 16#100000002#;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:143

   LLVMDisassembler_ReferenceType_In_ARM64_LDRXui : constant := 16#100000003#;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:145

   LLVMDisassembler_ReferenceType_In_ARM64_LDRXl : constant := 16#100000004#;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:147

   LLVMDisassembler_ReferenceType_In_ARM64_ADR : constant := 16#100000005#;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:149

   LLVMDisassembler_ReferenceType_Out_SymbolStub : constant := 1;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:152

   LLVMDisassembler_ReferenceType_Out_LitPool_SymAddr : constant := 2;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:154

   LLVMDisassembler_ReferenceType_Out_LitPool_CstrAddr : constant := 3;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:156

   LLVMDisassembler_ReferenceType_Out_Objc_CFString_Ref : constant := 4;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:159

   LLVMDisassembler_ReferenceType_Out_Objc_Message : constant := 5;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:161

   LLVMDisassembler_ReferenceType_Out_Objc_Message_Ref : constant := 6;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:163

   LLVMDisassembler_ReferenceType_Out_Objc_Selector_Ref : constant := 7;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:165

   LLVMDisassembler_ReferenceType_Out_Objc_Class_Ref : constant := 8;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:167

   LLVMDisassembler_ReferenceType_DeMangled_Name : constant := 9;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:170

   LLVMDisassembler_Option_UseMarkup : constant := 1;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:221

   LLVMDisassembler_Option_PrintImmHex : constant := 2;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:223

   LLVMDisassembler_Option_AsmPrinterVariant : constant := 4;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:225

   LLVMDisassembler_Option_SetInstrComments : constant := 8;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:227

   LLVMDisassembler_Option_PrintLatency : constant := 16;  --  llvm-5.0.0.src/include/llvm-c/Disassembler.h:229

  --===-- llvm-c/Disassembler.h - Disassembler Public C Interface ---*- C -*-===*|*                                                                            *|
  --|
  --|*                     The LLVM Compiler Infrastructure                       *|
  --|*                                                                            *|
  --|* This file is distributed under the University of Illinois Open Source      *|
  --|* License. See LICENSE.TXT for details.                                      *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header provides a public interface to a disassembler library.         *|
  --|* LLVM provides an implementation of this interface.                         *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCDisassembler Disassembler
  -- * @ingroup LLVMC
  -- *
  -- * @{
  --  

  --*
  -- * An opaque reference to a disassembler context.
  --  

   type Disasm_Context_T is new System.Address;  -- llvm-5.0.0.src/include/llvm-c/Disassembler.h:35

  --*
  -- * The type for the operand information call back function.  This is called to
  -- * get the symbolic information for an operand of an instruction.  Typically
  -- * this is from the relocation information, symbol table, etc.  That block of
  -- * information is saved when the disassembler context is created and passed to
  -- * the call back in the DisInfo parameter.  The instruction containing operand
  -- * is at the PC parameter.  For some instruction sets, there can be more than
  -- * one operand with symbolic information.  To determine the symbolic operand
  -- * information for each operand, the bytes for the specific operand in the
  -- * instruction are specified by the Offset parameter and its byte widith is the
  -- * size parameter.  For instructions sets with fixed widths and one symbolic
  -- * operand per instruction, the Offset parameter will be zero and Size parameter
  -- * will be the instruction width.  The information is returned in TagBuf and is
  -- * Triple specific with its specific information defined by the value of
  -- * TagType for that Triple.  If symbolic information is returned the function
  -- * returns 1, otherwise it returns 0.
  --  

   type Op_Info_Callback_T is access function 
        (arg1 : System.Address;
         arg2 : stdint_h.uint64_t;
         arg3 : stdint_h.uint64_t;
         arg4 : stdint_h.uint64_t;
         arg5 : int;
         arg6 : System.Address) return int;
   pragma Convention (C, Op_Info_Callback_T);  -- llvm-5.0.0.src/include/llvm-c/Disassembler.h:54

  --*
  -- * The initial support in LLVM MC for the most general form of a relocatable
  -- * expression is "AddSymbol - SubtractSymbol + Offset".  For some Darwin targets
  -- * this full form is encoded in the relocation information so that AddSymbol and
  -- * SubtractSymbol can be link edited independent of each other.  Many other
  -- * platforms only allow a relocatable expression of the form AddSymbol + Offset
  -- * to be encoded.
  -- *
  -- * The LLVMOpInfoCallback() for the TagType value of 1 uses the struct
  -- * LLVMOpInfo1.  The value of the relocatable expression for the operand,
  -- * including any PC adjustment, is passed in to the call back in the Value
  -- * field.  The symbolic information about the operand is returned using all
  -- * the fields of the structure with the Offset of the relocatable expression
  -- * returned in the Value field.  It is possible that some symbols in the
  -- * relocatable expression were assembly temporary symbols, for example
  -- * "Ldata - LpicBase + constant", and only the Values of the symbols without
  -- * symbol names are present in the relocation information.  The VariantKind
  -- * type is one of the Target specific #defines below and is used to print
  -- * operands like "_foo@GOT", ":lower16:_foo", etc.
  --  

  -- 1 if this symbol is present  
   type Op_Info_Symbol1_T is record
      Present : aliased stdint_h.uint64_t;  -- llvm-5.0.0.src/include/llvm-c/Disassembler.h:79
      Name : Interfaces.C.Strings.chars_ptr;  -- llvm-5.0.0.src/include/llvm-c/Disassembler.h:80
      Value : aliased stdint_h.uint64_t;  -- llvm-5.0.0.src/include/llvm-c/Disassembler.h:81
   end record;
   pragma Convention (C_Pass_By_Copy, Op_Info_Symbol1_T);  -- llvm-5.0.0.src/include/llvm-c/Disassembler.h:78

  -- symbol name if not NULL  
  -- symbol value if name is NULL  
   type Op_Info1_T is record
      AddSymbol : aliased Op_Info_Symbol1_T;  -- llvm-5.0.0.src/include/llvm-c/Disassembler.h:85
      SubtractSymbol : aliased Op_Info_Symbol1_T;  -- llvm-5.0.0.src/include/llvm-c/Disassembler.h:86
      Value : aliased stdint_h.uint64_t;  -- llvm-5.0.0.src/include/llvm-c/Disassembler.h:87
      VariantKind : aliased stdint_h.uint64_t;  -- llvm-5.0.0.src/include/llvm-c/Disassembler.h:88
   end record;
   pragma Convention (C_Pass_By_Copy, Op_Info1_T);  -- llvm-5.0.0.src/include/llvm-c/Disassembler.h:84

  --*
  -- * The operand VariantKinds for symbolic disassembly.
  --  

  --*
  -- * The ARM target VariantKinds.
  --  

  --*
  -- * The ARM64 target VariantKinds.
  --  

  --*
  -- * The type for the symbol lookup function.  This may be called by the
  -- * disassembler for things like adding a comment for a PC plus a constant
  -- * offset load instruction to use a symbol name instead of a load address value.
  -- * It is passed the block information is saved when the disassembler context is
  -- * created and the ReferenceValue to look up as a symbol.  If no symbol is found
  -- * for the ReferenceValue NULL is returned.  The ReferenceType of the
  -- * instruction is passed indirectly as is the PC of the instruction in
  -- * ReferencePC.  If the output reference can be determined its type is returned
  -- * indirectly in ReferenceType along with ReferenceName if any, or that is set
  -- * to NULL.
  --  

   type Symbol_Lookup_Callback_T is access function 
        (arg1 : System.Address;
         arg2 : stdint_h.uint64_t;
         arg3 : access stdint_h.uint64_t;
         arg4 : stdint_h.uint64_t;
         arg5 : System.Address) return Interfaces.C.Strings.chars_ptr;
   pragma Convention (C, Symbol_Lookup_Callback_T);  -- llvm-5.0.0.src/include/llvm-c/Disassembler.h:124

  --*
  -- * The reference types on input and output.
  --  

  -- No input reference type or no output reference type.  
  -- The input reference is from a branch instruction.  
  -- The input reference is from a PC relative load instruction.  
  -- The input reference is from an ARM64::ADRP instruction.  
  -- The input reference is from an ARM64::ADDXri instruction.  
  -- The input reference is from an ARM64::LDRXui instruction.  
  -- The input reference is from an ARM64::LDRXl instruction.  
  -- The input reference is from an ARM64::ADR instruction.  
  -- The output reference is to as symbol stub.  
  -- The output reference is to a symbol address in a literal pool.  
  -- The output reference is to a cstring address in a literal pool.  
  -- The output reference is to a Objective-C CoreFoundation string.  
  -- The output reference is to a Objective-C message.  
  -- The output reference is to a Objective-C message ref.  
  -- The output reference is to a Objective-C selector ref.  
  -- The output reference is to a Objective-C class ref.  
  -- The output reference is to a C++ symbol name.  
  --*
  -- * Create a disassembler for the TripleName.  Symbolic disassembly is supported
  -- * by passing a block of information in the DisInfo parameter and specifying the
  -- * TagType and callback functions as described above.  These can all be passed
  -- * as NULL.  If successful, this returns a disassembler context.  If not, it
  -- * returns NULL. This function is equivalent to calling
  -- * LLVMCreateDisasmCPUFeatures() with an empty CPU name and feature set.
  --  

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

  --*
  -- * Create a disassembler for the TripleName and a specific CPU.  Symbolic
  -- * disassembly is supported by passing a block of information in the DisInfo
  -- * parameter and specifying the TagType and callback functions as described
  -- * above.  These can all be passed * as NULL.  If successful, this returns a
  -- * disassembler context.  If not, it returns NULL. This function is equivalent
  -- * to calling LLVMCreateDisasmCPUFeatures() with an empty feature set.
  --  

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

  --*
  -- * Create a disassembler for the TripleName, a specific CPU and specific feature
  -- * string.  Symbolic disassembly is supported by passing a block of information
  -- * in the DisInfo parameter and specifying the TagType and callback functions as
  -- * described above.  These can all be passed * as NULL.  If successful, this
  -- * returns a disassembler context.  If not, it returns NULL.
  --  

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

  --*
  -- * Set the disassembler's options.  Returns 1 if it can set the Options and 0
  -- * otherwise.
  --  

   function Set_Disasm_Options (DC : Disasm_Context_T; Options : stdint_h.uint64_t) return int;  -- llvm-5.0.0.src/include/llvm-c/Disassembler.h:218
   pragma Import (C, Set_Disasm_Options, "LLVMSetDisasmOptions");

  -- The option to produce marked up assembly.  
  -- The option to print immediates as hex.  
  -- The option use the other assembler printer variant  
  -- The option to set comment on instructions  
  -- The option to print latency information alongside instructions  
  --*
  -- * Dispose of a disassembler context.
  --  

   procedure Disasm_Dispose (DC : Disasm_Context_T);  -- llvm-5.0.0.src/include/llvm-c/Disassembler.h:234
   pragma Import (C, Disasm_Dispose, "LLVMDisasmDispose");

  --*
  -- * Disassemble a single instruction using the disassembler context specified in
  -- * the parameter DC.  The bytes of the instruction are specified in the
  -- * parameter Bytes, and contains at least BytesSize number of bytes.  The
  -- * instruction is at the address specified by the PC parameter.  If a valid
  -- * instruction can be disassembled, its string is returned indirectly in
  -- * OutString whose size is specified in the parameter OutStringSize.  This
  -- * function returns the number of bytes in the instruction or zero if there was
  -- * no valid instruction.
  --  

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

  --*
  -- * @}
  --  

end LLVM.Disassembler;

