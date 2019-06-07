pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with stdint_h;
with Interfaces.C.Strings;

package LLVM.Disassembler_Types is

   LLVMDisassembler_VariantKind_None : constant := 0;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:82

   LLVMDisassembler_VariantKind_ARM_HI16 : constant := 1;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:87
   LLVMDisassembler_VariantKind_ARM_LO16 : constant := 2;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:88

   LLVMDisassembler_VariantKind_ARM64_PAGE : constant := 1;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:93
   LLVMDisassembler_VariantKind_ARM64_PAGEOFF : constant := 2;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:94
   LLVMDisassembler_VariantKind_ARM64_GOTPAGE : constant := 3;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:95
   LLVMDisassembler_VariantKind_ARM64_GOTPAGEOFF : constant := 4;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:96
   LLVMDisassembler_VariantKind_ARM64_TLVP : constant := 5;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:97
   LLVMDisassembler_VariantKind_ARM64_TLVOFF : constant := 6;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:98

   LLVMDisassembler_ReferenceType_InOut_None : constant := 0;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:121

   LLVMDisassembler_ReferenceType_In_Branch : constant := 1;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:124

   LLVMDisassembler_ReferenceType_In_PCrel_Load : constant := 2;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:126

   LLVMDisassembler_ReferenceType_In_ARM64_ADRP : constant := 16#100000001#;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:129

   LLVMDisassembler_ReferenceType_In_ARM64_ADDXri : constant := 16#100000002#;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:131

   LLVMDisassembler_ReferenceType_In_ARM64_LDRXui : constant := 16#100000003#;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:133

   LLVMDisassembler_ReferenceType_In_ARM64_LDRXl : constant := 16#100000004#;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:135

   LLVMDisassembler_ReferenceType_In_ARM64_ADR : constant := 16#100000005#;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:137

   LLVMDisassembler_ReferenceType_Out_SymbolStub : constant := 1;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:140

   LLVMDisassembler_ReferenceType_Out_LitPool_SymAddr : constant := 2;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:142

   LLVMDisassembler_ReferenceType_Out_LitPool_CstrAddr : constant := 3;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:144

   LLVMDisassembler_ReferenceType_Out_Objc_CFString_Ref : constant := 4;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:147

   LLVMDisassembler_ReferenceType_Out_Objc_Message : constant := 5;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:149

   LLVMDisassembler_ReferenceType_Out_Objc_Message_Ref : constant := 6;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:151

   LLVMDisassembler_ReferenceType_Out_Objc_Selector_Ref : constant := 7;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:153

   LLVMDisassembler_ReferenceType_Out_Objc_Class_Ref : constant := 8;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:155

   LLVMDisassembler_ReferenceType_DeMangled_Name : constant := 9;  --  llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:158

  --===-- llvm-c/DisassemblerTypedefs.h -----------------------------*- C -*-===*|*                                                                            *|
  --|
  --|*                     The LLVM Compiler Infrastructure                       *|
  --|*                                                                            *|
  --|* This file is distributed under the University of Illinois Open Source      *|
  --|* License. See LICENSE.TXT for details.                                      *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------=== 

  --*
  -- * An opaque reference to a disassembler context.
  --  

   type Disasm_Context_T is new System.Address;  -- llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:23

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
   pragma Convention (C, Op_Info_Callback_T);  -- llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:42

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
      Present : aliased stdint_h.uint64_t;  -- llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:67
      Name : Interfaces.C.Strings.chars_ptr;  -- llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:68
      Value : aliased stdint_h.uint64_t;  -- llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:69
   end record;
   pragma Convention (C_Pass_By_Copy, Op_Info_Symbol1_T);  -- llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:66

  -- symbol name if not NULL  
  -- symbol value if name is NULL  
   type Op_Info1_T is record
      AddSymbol : aliased Op_Info_Symbol1_T;  -- llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:73
      SubtractSymbol : aliased Op_Info_Symbol1_T;  -- llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:74
      Value : aliased stdint_h.uint64_t;  -- llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:75
      VariantKind : aliased stdint_h.uint64_t;  -- llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:76
   end record;
   pragma Convention (C_Pass_By_Copy, Op_Info1_T);  -- llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:72

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
   pragma Convention (C, Symbol_Lookup_Callback_T);  -- llvm-8.0.0.src/include/llvm-c/DisassemblerTypes.h:112

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
end LLVM.Disassembler_Types;

