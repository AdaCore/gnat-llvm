pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with Interfaces.C.Strings;
with System;
with stddef_h;
with stdint_h;
with Interfaces.C.Extensions;

package LLVM.Core is

   --  arg-macro: procedure LLVM_FOR_EACH_VALUE_SUBCLASS (macro)
   --    macro(Argument) macro(BasicBlock) macro(InlineAsm) macro(User) macro(Constant) macro(BlockAddress) macro(ConstantAggregateZero) macro(ConstantArray) macro(ConstantDataSequential) macro(ConstantDataArray) macro(ConstantDataVector) macro(ConstantExpr) macro(ConstantFP) macro(ConstantInt) macro(ConstantPointerNull) macro(ConstantStruct) macro(ConstantTokenNone) macro(ConstantVector) macro(GlobalValue) macro(GlobalAlias) macro(GlobalObject) macro(Function) macro(GlobalVariable) macro(GlobalIFunc) macro(UndefValue) macro(PoisonValue) macro(Instruction) macro(UnaryOperator) macro(BinaryOperator) macro(CallInst) macro(IntrinsicInst) macro(DbgInfoIntrinsic) macro(DbgVariableIntrinsic) macro(DbgDeclareInst) macro(DbgLabelInst) macro(MemIntrinsic) macro(MemCpyInst) macro(MemMoveInst) macro(MemSetInst) macro(CmpInst) macro(FCmpInst) macro(ICmpInst) macro(ExtractElementInst) macro(GetElementPtrInst) macro(InsertElementInst) macro(InsertValueInst) macro(LandingPadInst) macro(PHINode) macro(SelectInst) macro(ShuffleVectorInst) macro(StoreInst) macro(BranchInst) macro(IndirectBrInst) macro(InvokeInst) macro(ReturnInst) macro(SwitchInst) macro(UnreachableInst) macro(ResumeInst) macro(CleanupReturnInst) macro(CatchReturnInst) macro(CatchSwitchInst) macro(CallBrInst) macro(FuncletPadInst) macro(CatchPadInst) macro(CleanupPadInst) macro(UnaryInstruction) macro(AllocaInst) macro(CastInst) macro(AddrSpaceCastInst) macro(BitCastInst) macro(FPExtInst) macro(FPToSIInst) macro(FPToUIInst) macro(FPTruncInst) macro(IntToPtrInst) macro(PtrToIntInst) macro(SExtInst) macro(SIToFPInst) macro(TruncInst) macro(UIToFPInst) macro(ZExtInst) macro(ExtractValueInst) macro(LoadInst) macro(VAArgInst) macro(FreezeInst) macro(AtomicCmpXchgInst) macro(AtomicRMWInst) macro(FenceInst)
   --  unsupported macro: LLVM_DECLARE_VALUE_CAST(name) LLVMValueRef LLVMIsA ##name(LLVMValueRef Val);
  --===-- llvm-c/Core.h - Core Library C Interface ------------------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to libLLVMCore.a, which implements    *|
  --|* the LLVM intermediate representation.                                      *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMC LLVM-C: C interface to LLVM
  -- *
  -- * This module exposes parts of the LLVM library as a C API.
  -- *
  -- * @{
  --  

  --*
  -- * @defgroup LLVMCTransforms Transforms
  --  

  --*
  -- * @defgroup LLVMCCore Core
  -- *
  -- * This modules provide an interface to libLLVMCore, which implements
  -- * the LLVM intermediate representation as well as other related types
  -- * and utilities.
  -- *
  -- * Many exotic languages can interoperate with C code but have a harder time
  -- * with C++ due to name mangling. So in addition to C, this interface enables
  -- * tools written in such languages.
  -- *
  -- * @{
  --  

  --*
  -- * @defgroup LLVMCCoreTypes Types and Enumerations
  -- *
  -- * @{
  --  

  --/ External users depend on the following values being stable. It is not safe
  --/ to reorder them.
  -- Terminator Instructions  
  -- removed 6 due to API changes  
  -- Standard Unary Operators  
  -- Standard Binary Operators  
  -- Logical Operators  
  -- Memory Operators  
  -- Cast Operators  
  -- Other Operators  
  -- Atomic operators  
  -- Exception Handling Operators  
   subtype Opcode_T is unsigned;
   Op_Ret : constant Opcode_T := 1;
   Op_Br : constant Opcode_T := 2;
   Op_Switch : constant Opcode_T := 3;
   Op_Indirect_Br : constant Opcode_T := 4;
   Op_Invoke : constant Opcode_T := 5;
   Op_Unreachable : constant Opcode_T := 7;
   Op_Call_Br : constant Opcode_T := 67;
   Op_F_Neg : constant Opcode_T := 66;
   Op_Add : constant Opcode_T := 8;
   Op_F_Add : constant Opcode_T := 9;
   Op_Sub : constant Opcode_T := 10;
   Op_F_Sub : constant Opcode_T := 11;
   Op_Mul : constant Opcode_T := 12;
   Op_F_Mul : constant Opcode_T := 13;
   Op_U_Div : constant Opcode_T := 14;
   Op_S_Div : constant Opcode_T := 15;
   Op_F_Div : constant Opcode_T := 16;
   Op_U_Rem : constant Opcode_T := 17;
   Op_S_Rem : constant Opcode_T := 18;
   Op_F_Rem : constant Opcode_T := 19;
   Op_Shl : constant Opcode_T := 20;
   Op_L_Shr : constant Opcode_T := 21;
   Op_A_Shr : constant Opcode_T := 22;
   Op_And : constant Opcode_T := 23;
   Op_Or : constant Opcode_T := 24;
   Op_Xor : constant Opcode_T := 25;
   Op_Alloca : constant Opcode_T := 26;
   Op_Load : constant Opcode_T := 27;
   Op_Store : constant Opcode_T := 28;
   Op_Get_Element_Ptr : constant Opcode_T := 29;
   Op_Trunc : constant Opcode_T := 30;
   Op_Z_Ext : constant Opcode_T := 31;
   Op_S_Ext : constant Opcode_T := 32;
   Op_FP_To_UI : constant Opcode_T := 33;
   Op_FP_To_SI : constant Opcode_T := 34;
   Op_UI_To_FP : constant Opcode_T := 35;
   Op_SI_To_FP : constant Opcode_T := 36;
   Op_FP_Trunc : constant Opcode_T := 37;
   Op_FP_Ext : constant Opcode_T := 38;
   Op_Ptr_To_Int : constant Opcode_T := 39;
   Op_Int_To_Ptr : constant Opcode_T := 40;
   Op_Bit_Cast : constant Opcode_T := 41;
   Op_Addr_Space_Cast : constant Opcode_T := 60;
   Op_I_Cmp : constant Opcode_T := 42;
   Op_F_Cmp : constant Opcode_T := 43;
   Op_PHI : constant Opcode_T := 44;
   Op_Call : constant Opcode_T := 45;
   Op_Select : constant Opcode_T := 46;
   Op_User_Op_1 : constant Opcode_T := 47;
   Op_User_Op_2 : constant Opcode_T := 48;
   Op_VA_Arg : constant Opcode_T := 49;
   Op_Extract_Element : constant Opcode_T := 50;
   Op_Insert_Element : constant Opcode_T := 51;
   Op_Shuffle_Vector : constant Opcode_T := 52;
   Op_Extract_Value : constant Opcode_T := 53;
   Op_Insert_Value : constant Opcode_T := 54;
   Op_Freeze : constant Opcode_T := 68;
   Op_Fence : constant Opcode_T := 55;
   Op_Atomic_Cmp_Xchg : constant Opcode_T := 56;
   Op_Atomic_RMW : constant Opcode_T := 57;
   Op_Resume : constant Opcode_T := 58;
   Op_Landing_Pad : constant Opcode_T := 59;
   Op_Cleanup_Ret : constant Opcode_T := 61;
   Op_Catch_Ret : constant Opcode_T := 62;
   Op_Catch_Pad : constant Opcode_T := 63;
   Op_Cleanup_Pad : constant Opcode_T := 64;
   Op_Catch_Switch : constant Opcode_T := 65;  -- install/include/llvm-c/Core.h:146

  --*< type with no size  
  --*< 16 bit floating point type  
  --*< 32 bit floating point type  
  --*< 64 bit floating point type  
  --*< 80 bit floating point type (X87)  
  --*< 128 bit floating point type (112-bit mantissa) 
  --*< 128 bit floating point type (two 64-bits)  
  --*< Labels  
  --*< Arbitrary bit width integers  
  --*< Functions  
  --*< Structures  
  --*< Arrays  
  --*< Pointers  
  --*< Fixed width SIMD vector type  
  --*< Metadata  
  --*< X86 MMX  
  --*< Tokens  
  --*< Scalable SIMD vector type  
  --*< 16 bit brain floating point type  
  --*< X86 AMX  
  --*< Target extension type  
   type Type_Kind_T is 
     (Void_Type_Kind,
      Half_Type_Kind,
      Float_Type_Kind,
      Double_Type_Kind,
      X86_FP80_Type_Kind,
      FP128_Type_Kind,
      PPC_FP128_Type_Kind,
      Label_Type_Kind,
      Integer_Type_Kind,
      Function_Type_Kind,
      Struct_Type_Kind,
      Array_Type_Kind,
      Pointer_Type_Kind,
      Vector_Type_Kind,
      Metadata_Type_Kind,
      X86_MMX_Type_Kind,
      Token_Type_Kind,
      Scalable_Vector_Type_Kind,
      B_Float_Type_Kind,
      X86_AMX_Type_Kind,
      Target_Ext_Type_Kind)
   with Convention => C;  -- install/include/llvm-c/Core.h:170

  --*< Externally visible function  
  --*< Keep one copy of function when linking (inline) 
  --*< Same, but only replaced by something
  --                            equivalent.  

  --*< Obsolete  
  --*< Keep one copy of function when linking (weak)  
  --*< Same, but only replaced by something
  --                            equivalent.  

  --*< Special purpose, only applies to global arrays  
  --*< Rename collisions when linking (static
  --                               functions)  

  --*< Like Internal, but omit from symbol table  
  --*< Obsolete  
  --*< Obsolete  
  --*< ExternalWeak linkage description  
  --*< Obsolete  
  --*< Tentative definitions  
  --*< Like Private, but linker removes.  
  --*< Like LinkerPrivate, but is weak.  
   type Linkage_T is 
     (External_Linkage,
      Available_Externally_Linkage,
      Link_Once_Any_Linkage,
      Link_Once_ODR_Linkage,
      Link_Once_ODR_Auto_Hide_Linkage,
      Weak_Any_Linkage,
      Weak_ODR_Linkage,
      Appending_Linkage,
      Internal_Linkage,
      Private_Linkage,
      DLL_Import_Linkage,
      DLL_Export_Linkage,
      External_Weak_Linkage,
      Ghost_Linkage,
      Common_Linkage,
      Linker_Private_Linkage,
      Linker_Private_Weak_Linkage)
   with Convention => C;  -- install/include/llvm-c/Core.h:193

  --*< The GV is visible  
  --*< The GV is hidden  
  --*< The GV is protected  
   type Visibility_T is 
     (Default_Visibility,
      Hidden_Visibility,
      Protected_Visibility)
   with Convention => C;  -- install/include/llvm-c/Core.h:199

  --*< Address of the GV is significant.  
  --*< Address of the GV is locally insignificant.  
  --*< Address of the GV is globally insignificant.  
   type Unnamed_Addr_T is 
     (No_Unnamed_Addr,
      Local_Unnamed_Addr,
      Global_Unnamed_Addr)
   with Convention => C;  -- install/include/llvm-c/Core.h:205

  --*< Function to be imported from DLL.  
  --*< Function to be accessible from DLL.  
   type DLL_Storage_Class_T is 
     (Default_Storage_Class,
      DLL_Import_Storage_Class,
      DLL_Export_Storage_Class)
   with Convention => C;  -- install/include/llvm-c/Core.h:211

   subtype Call_Conv_T is unsigned;
   C_Call_Conv : constant Call_Conv_T := 0;
   Fast_Call_Conv : constant Call_Conv_T := 8;
   Cold_Call_Conv : constant Call_Conv_T := 9;
   GHC_Call_Conv : constant Call_Conv_T := 10;
   Hi_PE_Call_Conv : constant Call_Conv_T := 11;
   Web_Kit_JS_Call_Conv : constant Call_Conv_T := 12;
   Any_Reg_Call_Conv : constant Call_Conv_T := 13;
   Preserve_Most_Call_Conv : constant Call_Conv_T := 14;
   Preserve_All_Call_Conv : constant Call_Conv_T := 15;
   Swift_Call_Conv : constant Call_Conv_T := 16;
   CXXFASTTLS_Call_Conv : constant Call_Conv_T := 17;
   X86_Stdcall_Call_Conv : constant Call_Conv_T := 64;
   X86_Fastcall_Call_Conv : constant Call_Conv_T := 65;
   ARMAPCS_Call_Conv : constant Call_Conv_T := 66;
   ARMAAPCS_Call_Conv : constant Call_Conv_T := 67;
   ARMAAPCSVFP_Call_Conv : constant Call_Conv_T := 68;
   MSP430INTR_Call_Conv : constant Call_Conv_T := 69;
   X86_This_Call_Call_Conv : constant Call_Conv_T := 70;
   PTX_Kernel_Call_Conv : constant Call_Conv_T := 71;
   PTX_Device_Call_Conv : constant Call_Conv_T := 72;
   SPIRFUNC_Call_Conv : constant Call_Conv_T := 75;
   SPIRKERNEL_Call_Conv : constant Call_Conv_T := 76;
   Intel_OCLBI_Call_Conv : constant Call_Conv_T := 77;
   X8664_Sys_V_Call_Conv : constant Call_Conv_T := 78;
   Win_64_Call_Conv : constant Call_Conv_T := 79;
   X86_Vector_Call_Call_Conv : constant Call_Conv_T := 80;
   HHVM_Call_Conv : constant Call_Conv_T := 81;
   HHVMC_Call_Conv : constant Call_Conv_T := 82;
   X86INTR_Call_Conv : constant Call_Conv_T := 83;
   AVRINTR_Call_Conv : constant Call_Conv_T := 84;
   AVRSIGNAL_Call_Conv : constant Call_Conv_T := 85;
   AVRBUILTIN_Call_Conv : constant Call_Conv_T := 86;
   AMDGPUVS_Call_Conv : constant Call_Conv_T := 87;
   AMDGPUGS_Call_Conv : constant Call_Conv_T := 88;
   AMDGPUPS_Call_Conv : constant Call_Conv_T := 89;
   AMDGPUCS_Call_Conv : constant Call_Conv_T := 90;
   AMDGPUKERNEL_Call_Conv : constant Call_Conv_T := 91;
   X86_Reg_Call_Call_Conv : constant Call_Conv_T := 92;
   AMDGPUHS_Call_Conv : constant Call_Conv_T := 93;
   MSP430BUILTIN_Call_Conv : constant Call_Conv_T := 94;
   AMDGPULS_Call_Conv : constant Call_Conv_T := 95;
   AMDGPUES_Call_Conv : constant Call_Conv_T := 96;  -- install/include/llvm-c/Core.h:256

   type Value_Kind_T is 
     (Argument_Value_Kind,
      Basic_Block_Value_Kind,
      Memory_Use_Value_Kind,
      Memory_Def_Value_Kind,
      Memory_Phi_Value_Kind,
      Function_Value_Kind,
      Global_Alias_Value_Kind,
      Global_I_Func_Value_Kind,
      Global_Variable_Value_Kind,
      Block_Address_Value_Kind,
      Constant_Expr_Value_Kind,
      Constant_Array_Value_Kind,
      Constant_Struct_Value_Kind,
      Constant_Vector_Value_Kind,
      Undef_Value_Value_Kind,
      Constant_Aggregate_Zero_Value_Kind,
      Constant_Data_Array_Value_Kind,
      Constant_Data_Vector_Value_Kind,
      Constant_Int_Value_Kind,
      Constant_FP_Value_Kind,
      Constant_Pointer_Null_Value_Kind,
      Constant_Token_None_Value_Kind,
      Metadata_As_Value_Value_Kind,
      Inline_Asm_Value_Kind,
      Instruction_Value_Kind,
      Poison_Value_Value_Kind,
      Constant_Target_None_Value_Kind)
   with Convention => C;  -- install/include/llvm-c/Core.h:290

  --*< equal  
  --*< not equal  
  --*< unsigned greater than  
  --*< unsigned greater or equal  
  --*< unsigned less than  
  --*< unsigned less or equal  
  --*< signed greater than  
  --*< signed greater or equal  
  --*< signed less than  
  --*< signed less or equal  
   subtype Int_Predicate_T is unsigned;
   Int_EQ : constant Int_Predicate_T := 32;
   Int_NE : constant Int_Predicate_T := 33;
   Int_UGT : constant Int_Predicate_T := 34;
   Int_UGE : constant Int_Predicate_T := 35;
   Int_ULT : constant Int_Predicate_T := 36;
   Int_ULE : constant Int_Predicate_T := 37;
   Int_SGT : constant Int_Predicate_T := 38;
   Int_SGE : constant Int_Predicate_T := 39;
   Int_SLT : constant Int_Predicate_T := 40;
   Int_SLE : constant Int_Predicate_T := 41;  -- install/include/llvm-c/Core.h:303

  --*< Always false (always folded)  
  --*< True if ordered and equal  
  --*< True if ordered and greater than  
  --*< True if ordered and greater than or equal  
  --*< True if ordered and less than  
  --*< True if ordered and less than or equal  
  --*< True if ordered and operands are unequal  
  --*< True if ordered (no nans)  
  --*< True if unordered: isnan(X) | isnan(Y)  
  --*< True if unordered or equal  
  --*< True if unordered or greater than  
  --*< True if unordered, greater than, or equal  
  --*< True if unordered or less than  
  --*< True if unordered, less than, or equal  
  --*< True if unordered or not equal  
  --*< Always true (always folded)  
   type Real_Predicate_T is 
     (Real_Predicate_False,
      Real_OEQ,
      Real_OGT,
      Real_OGE,
      Real_OLT,
      Real_OLE,
      Real_ONE,
      Real_ORD,
      Real_UNO,
      Real_UEQ,
      Real_UGT,
      Real_UGE,
      Real_ULT,
      Real_ULE,
      Real_UNE,
      Real_Predicate_True)
   with Convention => C;  -- install/include/llvm-c/Core.h:322

  --*< A catch clause    
  --*< A filter clause   
   type Landing_Pad_Clause_Ty_T is 
     (Landing_Pad_Catch,
      Landing_Pad_Filter)
   with Convention => C;  -- install/include/llvm-c/Core.h:327

   type Thread_Local_Mode_T is 
     (Not_Thread_Local,
      General_Dynamic_TLS_Model,
      Local_Dynamic_TLS_Model,
      Initial_Exec_TLS_Model,
      Local_Exec_TLS_Model)
   with Convention => C;  -- install/include/llvm-c/Core.h:335

  --*< A load or store which is not atomic  
  --*< Lowest level of atomicity, guarantees
  --                                     somewhat sane results, lock free.  

  --*< guarantees that if you take all the
  --                                     operations affecting a specific address,
  --                                     a consistent ordering exists  

  --*< Acquire provides a barrier of the sort
  --                                   necessary to acquire a lock to access other
  --                                   memory with normal loads and stores.  

  --*< Release is similar to Acquire, but with
  --                                   a barrier of the sort necessary to release
  --                                   a lock.  

  --*< provides both an Acquire and a
  --                                          Release barrier (for fences and
  --                                          operations which both read and write
  --                                           memory).  

  --*< provides Acquire semantics
  --                                                 for loads and Release
  --                                                 semantics for stores.
  --                                                 Additionally, it guarantees
  --                                                 that a total ordering exists
  --                                                 between all
  --                                                 SequentiallyConsistent
  --                                                 operations.  

   subtype Atomic_Ordering_T is unsigned;
   Atomic_Ordering_Not_Atomic : constant Atomic_Ordering_T := 0;
   Atomic_Ordering_Unordered : constant Atomic_Ordering_T := 1;
   Atomic_Ordering_Monotonic : constant Atomic_Ordering_T := 2;
   Atomic_Ordering_Acquire : constant Atomic_Ordering_T := 4;
   Atomic_Ordering_Release : constant Atomic_Ordering_T := 5;
   Atomic_Ordering_Acquire_Release : constant Atomic_Ordering_T := 6;
   Atomic_Ordering_Sequentially_Consistent : constant Atomic_Ordering_T := 7;  -- install/include/llvm-c/Core.h:362

  --*< Set the new value and return the one old  
  --*< Add a value and return the old one  
  --*< Subtract a value and return the old one  
  --*< And a value and return the old one  
  --*< Not-And a value and return the old one  
  --*< OR a value and return the old one  
  --*< Xor a value and return the old one  
  --*< Sets the value if it's greater than the
  --                             original using a signed comparison and return
  --                             the old one  

  --*< Sets the value if it's Smaller than the
  --                             original using a signed comparison and return
  --                             the old one  

  --*< Sets the value if it's greater than the
  --                             original using an unsigned comparison and return
  --                             the old one  

  --*< Sets the value if it's greater than the
  --                              original using an unsigned comparison and return
  --                              the old one  

  --*< Add a floating point value and return the
  --                              old one  

  --*< Subtract a floating point value and return the
  --                            old one  

  --*< Sets the value if it's greater than the
  --                             original using an floating point comparison and
  --                             return the old one  

  --*< Sets the value if it's smaller than the
  --                             original using an floating point comparison and
  --                             return the old one  

   type Atomic_RMW_Bin_Op_T is 
     (Atomic_RMW_Bin_Op_Xchg,
      Atomic_RMW_Bin_Op_Add,
      Atomic_RMW_Bin_Op_Sub,
      Atomic_RMW_Bin_Op_And,
      Atomic_RMW_Bin_Op_Nand,
      Atomic_RMW_Bin_Op_Or,
      Atomic_RMW_Bin_Op_Xor,
      Atomic_RMW_Bin_Op_Max,
      Atomic_RMW_Bin_Op_Min,
      Atomic_RMW_Bin_Op_U_Max,
      Atomic_RMW_Bin_Op_U_Min,
      Atomic_RMW_Bin_Op_F_Add,
      Atomic_RMW_Bin_Op_F_Sub,
      Atomic_RMW_Bin_Op_F_Max,
      Atomic_RMW_Bin_Op_F_Min)
   with Convention => C;  -- install/include/llvm-c/Core.h:394

   type Diagnostic_Severity_T is 
     (DS_Error,
      DS_Warning,
      DS_Remark,
      DS_Note)
   with Convention => C;  -- install/include/llvm-c/Core.h:401

   type Inline_Asm_Dialect_T is 
     (Inline_Asm_Dialect_ATT,
      Inline_Asm_Dialect_Intel)
   with Convention => C;  -- install/include/llvm-c/Core.h:406

  --*
  --   * Emits an error if two values disagree, otherwise the resulting value is
  --   * that of the operands.
  --   *
  --   * @see Module::ModFlagBehavior::Error
  --    

  --*
  --   * Emits a warning if two values disagree. The result value will be the
  --   * operand for the flag from the first module being linked.
  --   *
  --   * @see Module::ModFlagBehavior::Warning
  --    

  --*
  --   * Adds a requirement that another module flag be present and have a
  --   * specified value after linking is performed. The value must be a metadata
  --   * pair, where the first element of the pair is the ID of the module flag
  --   * to be restricted, and the second element of the pair is the value the
  --   * module flag should be restricted to. This behavior can be used to
  --   * restrict the allowable results (via triggering of an error) of linking
  --   * IDs with the **Override** behavior.
  --   *
  --   * @see Module::ModFlagBehavior::Require
  --    

  --*
  --   * Uses the specified value, regardless of the behavior or value of the
  --   * other module. If both modules specify **Override**, but the values
  --   * differ, an error will be emitted.
  --   *
  --   * @see Module::ModFlagBehavior::Override
  --    

  --*
  --   * Appends the two values, which are required to be metadata nodes.
  --   *
  --   * @see Module::ModFlagBehavior::Append
  --    

  --*
  --   * Appends the two values, which are required to be metadata
  --   * nodes. However, duplicate entries in the second list are dropped
  --   * during the append operation.
  --   *
  --   * @see Module::ModFlagBehavior::AppendUnique
  --    

   type Module_Flag_Behavior_T is 
     (Module_Flag_Behavior_Error,
      Module_Flag_Behavior_Warning,
      Module_Flag_Behavior_Require,
      Module_Flag_Behavior_Override,
      Module_Flag_Behavior_Append,
      Module_Flag_Behavior_Append_Unique)
   with Convention => C;  -- install/include/llvm-c/Core.h:457

  --*
  -- * Attribute index are either LLVMAttributeReturnIndex,
  -- * LLVMAttributeFunctionIndex or a parameter number from 1 to N.
  --  

  -- ISO C restricts enumerator values to range of 'int'
  -- (4294967295 is too large)
  -- LLVMAttributeFunctionIndex = ~0U,
   subtype Attribute_Index_T is unsigned;  -- install/include/llvm-c/Core.h:471

  --*
  -- * @}
  --  

   procedure Initialize_Core (R : LLVM.Types.Pass_Registry_T)  -- install/include/llvm-c/Core.h:477
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeCore";

  --* Deallocate and destroy all ManagedStatic variables.
  --    @see llvm::llvm_shutdown
  --    @see ManagedStatic  

   procedure Shutdown  -- install/include/llvm-c/Core.h:482
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMShutdown";

  --===-- Version query -----------------------------------------------------=== 
  --*
  -- * Return the major, minor, and patch version of LLVM
  -- *
  -- * The version components are returned via the function's three output
  -- * parameters or skipped if a NULL pointer was supplied.
  --  

   procedure Get_Version
     (Major : access unsigned;
      Minor : access unsigned;
      Patch : access unsigned)  -- install/include/llvm-c/Core.h:492
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetVersion";

  --===-- Error handling ----------------------------------------------------=== 
function Create_Message
     (Message : String)
      return String;

procedure Dispose_Message
     (Message : String);

  --*
  -- * @defgroup LLVMCCoreContext Contexts
  -- *
  -- * Contexts are execution states for the core LLVM IR system.
  -- *
  -- * Most types are tied to a context instance. Multiple contexts can
  -- * exist simultaneously. A single context is not thread safe. However,
  -- * different contexts can execute on different threads simultaneously.
  -- *
  -- * @{
  --  

   type Diagnostic_Handler_T is access procedure (Arg_1 : LLVM.Types.Diagnostic_Info_T; Arg_2 : System.Address)
   with Convention => C;  -- install/include/llvm-c/Core.h:511

   type Yield_Callback_T is access procedure (Arg_1 : LLVM.Types.Context_T; Arg_2 : System.Address)
   with Convention => C;  -- install/include/llvm-c/Core.h:512

  --*
  -- * Create a new context.
  -- *
  -- * Every call to this function should be paired with a call to
  -- * LLVMContextDispose() or the context will leak memory.
  --  

   function Context_Create return LLVM.Types.Context_T  -- install/include/llvm-c/Core.h:520
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMContextCreate";

  --*
  -- * Obtain the global context instance.
  --  

   function Get_Global_Context return LLVM.Types.Context_T  -- install/include/llvm-c/Core.h:525
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetGlobalContext";

  --*
  -- * Set the diagnostic handler for this context.
  --  

   procedure Context_Set_Diagnostic_Handler
     (C : LLVM.Types.Context_T;
      Handler : Diagnostic_Handler_T;
      Diagnostic_Context : System.Address)  -- install/include/llvm-c/Core.h:530
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMContextSetDiagnosticHandler";

  --*
  -- * Get the diagnostic handler of this context.
  --  

   function Context_Get_Diagnostic_Handler (C : LLVM.Types.Context_T) return Diagnostic_Handler_T  -- install/include/llvm-c/Core.h:537
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMContextGetDiagnosticHandler";

  --*
  -- * Get the diagnostic context of this context.
  --  

   function Context_Get_Diagnostic_Context (C : LLVM.Types.Context_T) return System.Address  -- install/include/llvm-c/Core.h:542
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMContextGetDiagnosticContext";

  --*
  -- * Set the yield callback function for this context.
  -- *
  -- * @see LLVMContext::setYieldCallback()
  --  

   procedure Context_Set_Yield_Callback
     (C : LLVM.Types.Context_T;
      Callback : Yield_Callback_T;
      Opaque_Handle : System.Address)  -- install/include/llvm-c/Core.h:549
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMContextSetYieldCallback";

  --*
  -- * Retrieve whether the given context is set to discard all value names.
  -- *
  -- * @see LLVMContext::shouldDiscardValueNames()
  --  

function Context_Should_Discard_Value_Names
     (C : LLVM.Types.Context_T)
      return Boolean;

  --*
  -- * Set whether the given context discards all value names.
  -- *
  -- * If true, only the names of GlobalValue objects will be available in the IR.
  -- * This can be used to save memory and runtime, especially in release mode.
  -- *
  -- * @see LLVMContext::setDiscardValueNames()
  --  

procedure Context_Set_Discard_Value_Names
     (C       : LLVM.Types.Context_T;
      Discard : Boolean);

  --*
  -- * Set whether the given context is in opaque pointer mode.
  -- *
  -- * @see LLVMContext::setOpaquePointers()
  --  

procedure Context_Set_Opaque_Pointers
     (C               : LLVM.Types.Context_T;
      Opaque_Pointers : Boolean);

  --*
  -- * Destroy a context instance.
  -- *
  -- * This should be called for every call to LLVMContextCreate() or memory
  -- * will be leaked.
  --  

   procedure Context_Dispose (C : LLVM.Types.Context_T)  -- install/include/llvm-c/Core.h:582
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMContextDispose";

  --*
  -- * Return a string representation of the DiagnosticInfo. Use
  -- * LLVMDisposeMessage to free the string.
  -- *
  -- * @see DiagnosticInfo::print()
  --  

function Get_Diag_Info_Description
     (DI : LLVM.Types.Diagnostic_Info_T)
      return String;

  --*
  -- * Return an enum LLVMDiagnosticSeverity.
  -- *
  -- * @see DiagnosticInfo::getSeverity()
  --  

   function Get_Diag_Info_Severity (DI : LLVM.Types.Diagnostic_Info_T) return Diagnostic_Severity_T  -- install/include/llvm-c/Core.h:597
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetDiagInfoSeverity";

function Get_MD_Kind_ID_In_Context
     (C     : LLVM.Types.Context_T;
      Name  : String;
      S_Len : unsigned)
      return unsigned;

function Get_MD_Kind_ID
     (Name  : String;
      S_Len : unsigned)
      return unsigned;

  --*
  -- * Return an unique id given the name of a enum attribute,
  -- * or 0 if no attribute by that name exists.
  -- *
  -- * See http://llvm.org/docs/LangRef.html#parameter-attributes
  -- * and http://llvm.org/docs/LangRef.html#function-attributes
  -- * for the list of available attributes.
  -- *
  -- * NB: Attribute names and/or id are subject to change without
  -- * going through the C API deprecation cycle.
  --  

function Get_Enum_Attribute_Kind_For_Name
     (Name  : String;
      S_Len : stddef_h.size_t)
      return unsigned;

   function Get_Last_Enum_Attribute_Kind return unsigned  -- install/include/llvm-c/Core.h:615
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetLastEnumAttributeKind";

  --*
  -- * Create an enum attribute.
  --  

   function Create_Enum_Attribute
     (C : LLVM.Types.Context_T;
      Kind_ID : unsigned;
      Val : stdint_h.uint64_t) return LLVM.Types.Attribute_T  -- install/include/llvm-c/Core.h:620
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateEnumAttribute";

  --*
  -- * Get the unique id corresponding to the enum attribute
  -- * passed as argument.
  --  

   function Get_Enum_Attribute_Kind (A : LLVM.Types.Attribute_T) return unsigned  -- install/include/llvm-c/Core.h:627
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetEnumAttributeKind";

  --*
  -- * Get the enum attribute's value. 0 is returned if none exists.
  --  

   function Get_Enum_Attribute_Value (A : LLVM.Types.Attribute_T) return stdint_h.uint64_t  -- install/include/llvm-c/Core.h:632
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetEnumAttributeValue";

  --*
  -- * Create a type attribute
  --  

   function Create_Type_Attribute
     (C : LLVM.Types.Context_T;
      Kind_ID : unsigned;
      Type_Ref : LLVM.Types.Type_T) return LLVM.Types.Attribute_T  -- install/include/llvm-c/Core.h:637
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateTypeAttribute";

  --*
  -- * Get the type attribute's value.
  --  

   function Get_Type_Attribute_Value (A : LLVM.Types.Attribute_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:643
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetTypeAttributeValue";

  --*
  -- * Create a string attribute.
  --  

function Create_String_Attribute
     (C        : LLVM.Types.Context_T;
      K        : String;
      K_Length : unsigned;
      V        : String;
      V_Length : unsigned)
      return LLVM.Types.Attribute_T;

  --*
  -- * Get the string attribute's kind.
  --  

function Get_String_Attribute_Kind
     (A      : LLVM.Types.Attribute_T;
      Length : access unsigned)
      return String;

  --*
  -- * Get the string attribute's value.
  --  

function Get_String_Attribute_Value
     (A      : LLVM.Types.Attribute_T;
      Length : access unsigned)
      return String;

  --*
  -- * Check for the different types of attributes.
  --  

function Is_Enum_Attribute
     (A : LLVM.Types.Attribute_T)
      return Boolean;

function Is_String_Attribute
     (A : LLVM.Types.Attribute_T)
      return Boolean;

function Is_Type_Attribute
     (A : LLVM.Types.Attribute_T)
      return Boolean;

  --*
  -- * Obtain a Type from a context by its registered name.
  --  

function Get_Type_By_Name_2
     (C    : LLVM.Types.Context_T;
      Name : String)
      return LLVM.Types.Type_T;

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreModule Modules
  -- *
  -- * Modules represent the top-level structure in an LLVM program. An LLVM
  -- * module is effectively a translation unit or a collection of
  -- * translation units merged together.
  -- *
  -- * @{
  --  

  --*
  -- * Create a new, empty module in the global context.
  -- *
  -- * This is equivalent to calling LLVMModuleCreateWithNameInContext with
  -- * LLVMGetGlobalContext() as the context parameter.
  -- *
  -- * Every invocation should be paired with LLVMDisposeModule() or memory
  -- * will be leaked.
  --  

function Module_Create_With_Name
     (Module_ID : String)
      return LLVM.Types.Module_T;

  --*
  -- * Create a new, empty module in a specific context.
  -- *
  -- * Every invocation should be paired with LLVMDisposeModule() or memory
  -- * will be leaked.
  --  

function Module_Create_With_Name_In_Context
     (Module_ID : String;
      C         : LLVM.Types.Context_T)
      return LLVM.Types.Module_T;

  --*
  -- * Return an exact copy of the specified module.
  --  

   function Clone_Module (M : LLVM.Types.Module_T) return LLVM.Types.Module_T  -- install/include/llvm-c/Core.h:710
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCloneModule";

  --*
  -- * Destroy a module instance.
  -- *
  -- * This must be called for every created module or memory will be
  -- * leaked.
  --  

   procedure Dispose_Module (M : LLVM.Types.Module_T)  -- install/include/llvm-c/Core.h:718
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeModule";

  --*
  -- * Obtain the identifier of a module.
  -- *
  -- * @param M Module to obtain identifier of
  -- * @param Len Out parameter which holds the length of the returned string.
  -- * @return The identifier of M.
  -- * @see Module::getModuleIdentifier()
  --  

function Get_Module_Identifier
     (M   : LLVM.Types.Module_T;
      Len : access stddef_h.size_t)
      return String;

  --*
  -- * Set the identifier of a module to a string Ident with length Len.
  -- *
  -- * @param M The module to set identifier
  -- * @param Ident The string to set M's identifier to
  -- * @param Len Length of Ident
  -- * @see Module::setModuleIdentifier()
  --  

procedure Set_Module_Identifier
     (M     : LLVM.Types.Module_T;
      Ident : String;
      Len   : stddef_h.size_t);

  --*
  -- * Obtain the module's original source file name.
  -- *
  -- * @param M Module to obtain the name of
  -- * @param Len Out parameter which holds the length of the returned string
  -- * @return The original source file name of M
  -- * @see Module::getSourceFileName()
  --  

function Get_Source_File_Name
     (M   : LLVM.Types.Module_T;
      Len : access stddef_h.size_t)
      return String;

  --*
  -- * Set the original source file name of a module to a string Name with length
  -- * Len.
  -- *
  -- * @param M The module to set the source file name of
  -- * @param Name The string to set M's source file name to
  -- * @param Len Length of Name
  -- * @see Module::setSourceFileName()
  --  

procedure Set_Source_File_Name
     (M    : LLVM.Types.Module_T;
      Name : String;
      Len  : stddef_h.size_t);

  --*
  -- * Obtain the data layout for a module.
  -- *
  -- * @see Module::getDataLayoutStr()
  -- *
  -- * LLVMGetDataLayout is DEPRECATED, as the name is not only incorrect,
  -- * but match the name of another method on the module. Prefer the use
  -- * of LLVMGetDataLayoutStr, which is not ambiguous.
  --  

function Get_Data_Layout_Str
     (M : LLVM.Types.Module_T)
      return String;

function Get_Data_Layout
     (M : LLVM.Types.Module_T)
      return String;

  --*
  -- * Set the data layout for a module.
  -- *
  -- * @see Module::setDataLayout()
  --  

procedure Set_Data_Layout
     (M               : LLVM.Types.Module_T;
      Data_Layout_Str : String);

  --*
  -- * Obtain the target triple for a module.
  -- *
  -- * @see Module::getTargetTriple()
  --  

function Get_Target
     (M : LLVM.Types.Module_T)
      return String;

  --*
  -- * Set the target triple for a module.
  -- *
  -- * @see Module::setTargetTriple()
  --  

procedure Set_Target
     (M      : LLVM.Types.Module_T;
      Triple : String);

  --*
  -- * Returns the module flags as an array of flag-key-value triples.  The caller
  -- * is responsible for freeing this array by calling
  -- * \c LLVMDisposeModuleFlagsMetadata.
  -- *
  -- * @see Module::getModuleFlagsMetadata()
  --  

   function Copy_Module_Flags_Metadata (M : LLVM.Types.Module_T; Len : access stddef_h.size_t) return access LLVM.Types.Opaque_Module_Flag_Entry_Impl_T  -- install/include/llvm-c/Core.h:801
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCopyModuleFlagsMetadata";

  --*
  -- * Destroys module flags metadata entries.
  --  

   procedure Dispose_Module_Flags_Metadata (Entries : access LLVM.Types.Opaque_Module_Flag_Entry_Impl_T)  -- install/include/llvm-c/Core.h:806
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeModuleFlagsMetadata";

  --*
  -- * Returns the flag behavior for a module flag entry at a specific index.
  -- *
  -- * @see Module::ModuleFlagEntry::Behavior
  --  

   function Module_Flag_Entries_Get_Flag_Behavior (Entries : access LLVM.Types.Opaque_Module_Flag_Entry_Impl_T; Index : unsigned) return Module_Flag_Behavior_T  -- install/include/llvm-c/Core.h:814
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMModuleFlagEntriesGetFlagBehavior";

  --*
  -- * Returns the key for a module flag entry at a specific index.
  -- *
  -- * @see Module::ModuleFlagEntry::Key
  --  

function Module_Flag_Entries_Get_Key
     (Entries : access LLVM.Types.Opaque_Module_Flag_Entry_Impl_T;
      Index   : unsigned;
      Len     : access stddef_h.size_t)
      return String;

  --*
  -- * Returns the metadata for a module flag entry at a specific index.
  -- *
  -- * @see Module::ModuleFlagEntry::Val
  --  

   function Module_Flag_Entries_Get_Metadata (Entries : access LLVM.Types.Opaque_Module_Flag_Entry_Impl_T; Index : unsigned) return LLVM.Types.Metadata_T  -- install/include/llvm-c/Core.h:830
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMModuleFlagEntriesGetMetadata";

  --*
  -- * Add a module-level flag to the module-level flags metadata if it doesn't
  -- * already exist.
  -- *
  -- * @see Module::getModuleFlag()
  --  

function Get_Module_Flag
     (M       : LLVM.Types.Module_T;
      Key     : String;
      Key_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Add a module-level flag to the module-level flags metadata if it doesn't
  -- * already exist.
  -- *
  -- * @see Module::addModuleFlag()
  --  

procedure Add_Module_Flag
     (M        : LLVM.Types.Module_T;
      Behavior : Module_Flag_Behavior_T;
      Key      : String;
      Key_Len  : stddef_h.size_t;
      Val      : LLVM.Types.Metadata_T);

  --*
  -- * Dump a representation of a module to stderr.
  -- *
  -- * @see Module::dump()
  --  

   procedure Dump_Module (M : LLVM.Types.Module_T)  -- install/include/llvm-c/Core.h:857
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDumpModule";

  --*
  -- * Print a representation of a module to a file. The ErrorMessage needs to be
  -- * disposed with LLVMDisposeMessage. Returns 0 on success, 1 otherwise.
  -- *
  -- * @see Module::print()
  --  

function Print_Module_To_File
     (M             : LLVM.Types.Module_T;
      Filename      : String;
      Error_Message : System.Address)
      return Boolean;

  --*
  -- * Return a string representation of the module. Use
  -- * LLVMDisposeMessage to free the string.
  -- *
  -- * @see Module::print()
  --  

function Print_Module_To_String
     (M : LLVM.Types.Module_T)
      return String;

  --*
  -- * Get inline assembly for a module.
  -- *
  -- * @see Module::getModuleInlineAsm()
  --  

function Get_Module_Inline_Asm
     (M   : LLVM.Types.Module_T;
      Len : access stddef_h.size_t)
      return String;

  --*
  -- * Set inline assembly for a module.
  -- *
  -- * @see Module::setModuleInlineAsm()
  --  

procedure Set_Module_Inline_Asm_2
     (M   : LLVM.Types.Module_T;
      Asm : String;
      Len : stddef_h.size_t);

  --*
  -- * Append inline assembly to a module.
  -- *
  -- * @see Module::appendModuleInlineAsm()
  --  

procedure Append_Module_Inline_Asm
     (M   : LLVM.Types.Module_T;
      Asm : String;
      Len : stddef_h.size_t);

  --*
  -- * Create the specified uniqued inline asm string.
  -- *
  -- * @see InlineAsm::get()
  --  

function Get_Inline_Asm
     (Ty               : LLVM.Types.Type_T;
      Asm_String       : String;
      Asm_String_Size  : stddef_h.size_t;
      Constraints      : String;
      Constraints_Size : stddef_h.size_t;
      Has_Side_Effects : Boolean;
      Is_Align_Stack   : Boolean;
      Dialect          : Inline_Asm_Dialect_T;
      Can_Throw        : Boolean)
      return LLVM.Types.Value_T;

  --*
  -- * Obtain the context to which this module is associated.
  -- *
  -- * @see Module::getContext()
  --  

   function Get_Module_Context (M : LLVM.Types.Module_T) return LLVM.Types.Context_T  -- install/include/llvm-c/Core.h:913
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetModuleContext";

  --* Deprecated: Use LLVMGetTypeByName2 instead.  
function Get_Type_By_Name
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Type_T;

  --*
  -- * Obtain an iterator to the first NamedMDNode in a Module.
  -- *
  -- * @see llvm::Module::named_metadata_begin()
  --  

   function Get_First_Named_Metadata (M : LLVM.Types.Module_T) return LLVM.Types.Named_MD_Node_T  -- install/include/llvm-c/Core.h:923
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetFirstNamedMetadata";

  --*
  -- * Obtain an iterator to the last NamedMDNode in a Module.
  -- *
  -- * @see llvm::Module::named_metadata_end()
  --  

   function Get_Last_Named_Metadata (M : LLVM.Types.Module_T) return LLVM.Types.Named_MD_Node_T  -- install/include/llvm-c/Core.h:930
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetLastNamedMetadata";

  --*
  -- * Advance a NamedMDNode iterator to the next NamedMDNode.
  -- *
  -- * Returns NULL if the iterator was already at the end and there are no more
  -- * named metadata nodes.
  --  

   function Get_Next_Named_Metadata (Named_MD_Node : LLVM.Types.Named_MD_Node_T) return LLVM.Types.Named_MD_Node_T  -- install/include/llvm-c/Core.h:938
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNextNamedMetadata";

  --*
  -- * Decrement a NamedMDNode iterator to the previous NamedMDNode.
  -- *
  -- * Returns NULL if the iterator was already at the beginning and there are
  -- * no previous named metadata nodes.
  --  

   function Get_Previous_Named_Metadata (Named_MD_Node : LLVM.Types.Named_MD_Node_T) return LLVM.Types.Named_MD_Node_T  -- install/include/llvm-c/Core.h:946
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetPreviousNamedMetadata";

  --*
  -- * Retrieve a NamedMDNode with the given name, returning NULL if no such
  -- * node exists.
  -- *
  -- * @see llvm::Module::getNamedMetadata()
  --  

function Get_Named_Metadata
     (M        : LLVM.Types.Module_T;
      Name     : String;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Named_MD_Node_T;

  --*
  -- * Retrieve a NamedMDNode with the given name, creating a new node if no such
  -- * node exists.
  -- *
  -- * @see llvm::Module::getOrInsertNamedMetadata()
  --  

function Get_Or_Insert_Named_Metadata
     (M        : LLVM.Types.Module_T;
      Name     : String;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Named_MD_Node_T;

  --*
  -- * Retrieve the name of a NamedMDNode.
  -- *
  -- * @see llvm::NamedMDNode::getName()
  --  

function Get_Named_Metadata_Name
     (Named_MD : LLVM.Types.Named_MD_Node_T;
      Name_Len : access stddef_h.size_t)
      return String;

  --*
  -- * Obtain the number of operands for named metadata in a module.
  -- *
  -- * @see llvm::Module::getNamedMetadata()
  --  

function Get_Named_Metadata_Num_Operands
     (M    : LLVM.Types.Module_T;
      Name : String)
      return unsigned;

  --*
  -- * Obtain the named metadata operands for a module.
  -- *
  -- * The passed LLVMValueRef pointer should refer to an array of
  -- * LLVMValueRef at least LLVMGetNamedMetadataNumOperands long. This
  -- * array will be populated with the LLVMValueRef instances. Each
  -- * instance corresponds to a llvm::MDNode.
  -- *
  -- * @see llvm::Module::getNamedMetadata()
  -- * @see llvm::MDNode::getOperand()
  --  

procedure Get_Named_Metadata_Operands
     (M    : LLVM.Types.Module_T;
      Name : String;
      Dest : System.Address);

  --*
  -- * Add an operand to named metadata.
  -- *
  -- * @see llvm::Module::getNamedMetadata()
  -- * @see llvm::MDNode::addOperand()
  --  

procedure Add_Named_Metadata_Operand
     (M    : LLVM.Types.Module_T;
      Name : String;
      Val  : LLVM.Types.Value_T);

  --*
  -- * Return the directory of the debug location for this value, which must be
  -- * an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
  -- *
  -- * @see llvm::Instruction::getDebugLoc()
  -- * @see llvm::GlobalVariable::getDebugInfo()
  -- * @see llvm::Function::getSubprogram()
  --  

function Get_Debug_Loc_Directory
     (Val    : LLVM.Types.Value_T;
      Length : access unsigned)
      return String;

  --*
  -- * Return the filename of the debug location for this value, which must be
  -- * an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
  -- *
  -- * @see llvm::Instruction::getDebugLoc()
  -- * @see llvm::GlobalVariable::getDebugInfo()
  -- * @see llvm::Function::getSubprogram()
  --  

function Get_Debug_Loc_Filename
     (Val    : LLVM.Types.Value_T;
      Length : access unsigned)
      return String;

  --*
  -- * Return the line number of the debug location for this value, which must be
  -- * an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
  -- *
  -- * @see llvm::Instruction::getDebugLoc()
  -- * @see llvm::GlobalVariable::getDebugInfo()
  -- * @see llvm::Function::getSubprogram()
  --  

   function Get_Debug_Loc_Line (Val : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:1033
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetDebugLocLine";

  --*
  -- * Return the column number of the debug location for this value, which must be
  -- * an llvm::Instruction.
  -- *
  -- * @see llvm::Instruction::getDebugLoc()
  --  

   function Get_Debug_Loc_Column (Val : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:1041
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetDebugLocColumn";

  --*
  -- * Add a function to a module under a specified name.
  -- *
  -- * @see llvm::Function::Create()
  --  

function Add_Function
     (M           : LLVM.Types.Module_T;
      Name        : String;
      Function_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Value_T;

  --*
  -- * Obtain a Function value from a Module by its name.
  -- *
  -- * The returned value corresponds to a llvm::Function value.
  -- *
  -- * @see llvm::Module::getFunction()
  --  

function Get_Named_Function
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Value_T;

  --*
  -- * Obtain an iterator to the first Function in a Module.
  -- *
  -- * @see llvm::Module::begin()
  --  

   function Get_First_Function (M : LLVM.Types.Module_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1065
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetFirstFunction";

  --*
  -- * Obtain an iterator to the last Function in a Module.
  -- *
  -- * @see llvm::Module::end()
  --  

   function Get_Last_Function (M : LLVM.Types.Module_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1072
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetLastFunction";

  --*
  -- * Advance a Function iterator to the next Function.
  -- *
  -- * Returns NULL if the iterator was already at the end and there are no more
  -- * functions.
  --  

   function Get_Next_Function (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1080
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNextFunction";

  --*
  -- * Decrement a Function iterator to the previous Function.
  -- *
  -- * Returns NULL if the iterator was already at the beginning and there are
  -- * no previous functions.
  --  

   function Get_Previous_Function (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1088
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetPreviousFunction";

  --* Deprecated: Use LLVMSetModuleInlineAsm2 instead.  
procedure Set_Module_Inline_Asm
     (M   : LLVM.Types.Module_T;
      Asm : String);

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreType Types
  -- *
  -- * Types represent the type of a value.
  -- *
  -- * Types are associated with a context instance. The context internally
  -- * deduplicates types so there is only 1 instance of a specific type
  -- * alive at a time. In other words, a unique type is shared among all
  -- * consumers within a context.
  -- *
  -- * A Type in the C API corresponds to llvm::Type.
  -- *
  -- * Types have the following hierarchy:
  -- *
  -- *   types:
  -- *     integer type
  -- *     real type
  -- *     function type
  -- *     sequence types:
  -- *       array type
  -- *       pointer type
  -- *       vector type
  -- *     void type
  -- *     label type
  -- *     opaque type
  -- *
  -- * @{
  --  

  --*
  -- * Obtain the enumerated type of a Type instance.
  -- *
  -- * @see llvm::Type:getTypeID()
  --  

   function Get_Type_Kind (Ty : LLVM.Types.Type_T) return Type_Kind_T  -- install/include/llvm-c/Core.h:1131
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetTypeKind";

  --*
  -- * Whether the type has a known size.
  -- *
  -- * Things that don't have a size are abstract types, labels, and void.a
  -- *
  -- * @see llvm::Type::isSized()
  --  

function Type_Is_Sized
     (Ty : LLVM.Types.Type_T)
      return Boolean;

  --*
  -- * Obtain the context to which this type instance is associated.
  -- *
  -- * @see llvm::Type::getContext()
  --  

   function Get_Type_Context (Ty : LLVM.Types.Type_T) return LLVM.Types.Context_T  -- install/include/llvm-c/Core.h:1147
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetTypeContext";

  --*
  -- * Dump a representation of a type to stderr.
  -- *
  -- * @see llvm::Type::dump()
  --  

   procedure Dump_Type (Val : LLVM.Types.Type_T)  -- install/include/llvm-c/Core.h:1154
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDumpType";

  --*
  -- * Return a string representation of the type. Use
  -- * LLVMDisposeMessage to free the string.
  -- *
  -- * @see llvm::Type::print()
  --  

function Print_Type_To_String
     (Val : LLVM.Types.Type_T)
      return String;

  --*
  -- * @defgroup LLVMCCoreTypeInt Integer Types
  -- *
  -- * Functions in this section operate on integer types.
  -- *
  -- * @{
  --  

  --*
  -- * Obtain an integer type from a context with specified bit width.
  --  

   function Int_1_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1175
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInt1TypeInContext";

   function Int_8_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1176
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInt8TypeInContext";

   function Int_16_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1177
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInt16TypeInContext";

   function Int_32_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1178
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInt32TypeInContext";

   function Int_64_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1179
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInt64TypeInContext";

   function Int_128_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1180
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInt128TypeInContext";

   function Int_Type_In_Context (C : LLVM.Types.Context_T; Num_Bits : unsigned) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1181
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIntTypeInContext";

  --*
  -- * Obtain an integer type from the global context with a specified bit
  -- * width.
  --  

   function Int_1_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1187
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInt1Type";

   function Int_8_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1188
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInt8Type";

   function Int_16_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1189
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInt16Type";

   function Int_32_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1190
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInt32Type";

   function Int_64_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1191
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInt64Type";

   function Int_128_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1192
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInt128Type";

   function Int_Type (Num_Bits : unsigned) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1193
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIntType";

   function Get_Int_Type_Width (Integer_Ty : LLVM.Types.Type_T) return unsigned  -- install/include/llvm-c/Core.h:1194
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetIntTypeWidth";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreTypeFloat Floating Point Types
  -- *
  -- * @{
  --  

  --*
  -- * Obtain a 16-bit floating point type from a context.
  --  

   function Half_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1209
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMHalfTypeInContext";

  --*
  -- * Obtain a 16-bit brain floating point type from a context.
  --  

   function B_Float_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1214
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBFloatTypeInContext";

  --*
  -- * Obtain a 32-bit floating point type from a context.
  --  

   function Float_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1219
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMFloatTypeInContext";

  --*
  -- * Obtain a 64-bit floating point type from a context.
  --  

   function Double_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1224
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDoubleTypeInContext";

  --*
  -- * Obtain a 80-bit floating point type (X87) from a context.
  --  

   function X86FP80_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1229
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMX86FP80TypeInContext";

  --*
  -- * Obtain a 128-bit floating point type (112-bit mantissa) from a
  -- * context.
  --  

   function FP128_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1235
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMFP128TypeInContext";

  --*
  -- * Obtain a 128-bit floating point type (two 64-bits) from a context.
  --  

   function PPCFP128_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1240
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPPCFP128TypeInContext";

  --*
  -- * Obtain a floating point type from the global context.
  -- *
  -- * These map to the functions in this group of the same name.
  --  

   function Half_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1247
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMHalfType";

   function B_Float_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1248
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBFloatType";

   function Float_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1249
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMFloatType";

   function Double_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1250
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDoubleType";

   function X86FP80_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1251
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMX86FP80Type";

   function FP128_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1252
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMFP128Type";

   function PPCFP128_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1253
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPPCFP128Type";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreTypeFunction Function Types
  -- *
  -- * @{
  --  

  --*
  -- * Obtain a function type consisting of a specified signature.
  -- *
  -- * The function is defined as a tuple of a return Type, a list of
  -- * parameter types, and whether the function is variadic.
  --  

function Function_Type
     (Return_Type : LLVM.Types.Type_T;
      Param_Types : System.Address;
      Param_Count : unsigned;
      Is_Var_Arg  : Boolean)
      return LLVM.Types.Type_T;

  --*
  -- * Returns whether a function type is variadic.
  --  

function Is_Function_Var_Arg
     (Function_Ty : LLVM.Types.Type_T)
      return Boolean;

  --*
  -- * Obtain the Type this function Type returns.
  --  

   function Get_Return_Type (Function_Ty : LLVM.Types.Type_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1283
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetReturnType";

  --*
  -- * Obtain the number of parameters this function accepts.
  --  

   function Count_Param_Types (Function_Ty : LLVM.Types.Type_T) return unsigned  -- install/include/llvm-c/Core.h:1288
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCountParamTypes";

  --*
  -- * Obtain the types of a function's parameters.
  -- *
  -- * The Dest parameter should point to a pre-allocated array of
  -- * LLVMTypeRef at least LLVMCountParamTypes() large. On return, the
  -- * first LLVMCountParamTypes() entries in the array will be populated
  -- * with LLVMTypeRef instances.
  -- *
  -- * @param FunctionTy The function type to operate on.
  -- * @param Dest Memory address of an array to be filled with result.
  --  

   procedure Get_Param_Types (Function_Ty : LLVM.Types.Type_T; Dest : System.Address)  -- install/include/llvm-c/Core.h:1301
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetParamTypes";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreTypeStruct Structure Types
  -- *
  -- * These functions relate to LLVMTypeRef instances.
  -- *
  -- * @see llvm::StructType
  -- *
  -- * @{
  --  

  --*
  -- * Create a new structure type in a context.
  -- *
  -- * A structure is specified by a list of inner elements/types and
  -- * whether these can be packed together.
  -- *
  -- * @see llvm::StructType::create()
  --  

function Struct_Type_In_Context
     (C             : LLVM.Types.Context_T;
      Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : Boolean)
      return LLVM.Types.Type_T;

  --*
  -- * Create a new structure type in the global context.
  -- *
  -- * @see llvm::StructType::create()
  --  

function Struct_Type
     (Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : Boolean)
      return LLVM.Types.Type_T;

  --*
  -- * Create an empty structure in a context having a specified name.
  -- *
  -- * @see llvm::StructType::create()
  --  

function Struct_Create_Named
     (C    : LLVM.Types.Context_T;
      Name : String)
      return LLVM.Types.Type_T;

  --*
  -- * Obtain the name of a structure.
  -- *
  -- * @see llvm::StructType::getName()
  --  

function Get_Struct_Name
     (Ty : LLVM.Types.Type_T)
      return String;

  --*
  -- * Set the contents of a structure type.
  -- *
  -- * @see llvm::StructType::setBody()
  --  

procedure Struct_Set_Body
     (Struct_Ty     : LLVM.Types.Type_T;
      Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : Boolean);

  --*
  -- * Get the number of elements defined inside the structure.
  -- *
  -- * @see llvm::StructType::getNumElements()
  --  

   function Count_Struct_Element_Types (Struct_Ty : LLVM.Types.Type_T) return unsigned  -- install/include/llvm-c/Core.h:1363
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCountStructElementTypes";

  --*
  -- * Get the elements within a structure.
  -- *
  -- * The function is passed the address of a pre-allocated array of
  -- * LLVMTypeRef at least LLVMCountStructElementTypes() long. After
  -- * invocation, this array will be populated with the structure's
  -- * elements. The objects in the destination array will have a lifetime
  -- * of the structure type itself, which is the lifetime of the context it
  -- * is contained in.
  --  

   procedure Get_Struct_Element_Types (Struct_Ty : LLVM.Types.Type_T; Dest : System.Address)  -- install/include/llvm-c/Core.h:1375
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetStructElementTypes";

  --*
  -- * Get the type of the element at a given index in the structure.
  -- *
  -- * @see llvm::StructType::getTypeAtIndex()
  --  

   function Struct_Get_Type_At_Index (Struct_Ty : LLVM.Types.Type_T; I : unsigned) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1382
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMStructGetTypeAtIndex";

  --*
  -- * Determine whether a structure is packed.
  -- *
  -- * @see llvm::StructType::isPacked()
  --  

function Is_Packed_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return Boolean;

  --*
  -- * Determine whether a structure is opaque.
  -- *
  -- * @see llvm::StructType::isOpaque()
  --  

function Is_Opaque_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return Boolean;

  --*
  -- * Determine whether a structure is literal.
  -- *
  -- * @see llvm::StructType::isLiteral()
  --  

function Is_Literal_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return Boolean;

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreTypeSequential Sequential Types
  -- *
  -- * Sequential types represents "arrays" of types. This is a super class
  -- * for array, vector, and pointer types.
  -- *
  -- * @{
  --  

  --*
  -- * Obtain the element type of an array or vector type.
  -- *
  -- * This currently also works for pointer types, but this usage is deprecated.
  -- *
  -- * @see llvm::SequentialType::getElementType()
  --  

   function Get_Element_Type (Ty : LLVM.Types.Type_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1425
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetElementType";

  --*
  -- * Returns type's subtypes
  -- *
  -- * @see llvm::Type::subtypes()
  --  

   procedure Get_Subtypes (Tp : LLVM.Types.Type_T; Arr : System.Address)  -- install/include/llvm-c/Core.h:1432
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetSubtypes";

  --*
  -- *  Return the number of types in the derived type.
  -- *
  -- * @see llvm::Type::getNumContainedTypes()
  --  

   function Get_Num_Contained_Types (Tp : LLVM.Types.Type_T) return unsigned  -- install/include/llvm-c/Core.h:1439
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNumContainedTypes";

  --*
  -- * Create a fixed size array type that refers to a specific type.
  -- *
  -- * The created type will exist in the context that its element type
  -- * exists in.
  -- *
  -- * @see llvm::ArrayType::get()
  --  

   function Array_Type (Element_Type : LLVM.Types.Type_T; Element_Count : unsigned) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1449
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMArrayType";

  --*
  -- * Obtain the length of an array type.
  -- *
  -- * This only works on types that represent arrays.
  -- *
  -- * @see llvm::ArrayType::getNumElements()
  --  

   function Get_Array_Length (Array_Ty : LLVM.Types.Type_T) return unsigned  -- install/include/llvm-c/Core.h:1458
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetArrayLength";

  --*
  -- * Create a pointer type that points to a defined type.
  -- *
  -- * The created type will exist in the context that its pointee type
  -- * exists in.
  -- *
  -- * @see llvm::PointerType::get()
  --  

   function Pointer_Type (Element_Type : LLVM.Types.Type_T; Address_Space : unsigned) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1468
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPointerType";

  --*
  -- * Determine whether a pointer is opaque.
  -- *
  -- * True if this is an instance of an opaque PointerType.
  -- *
  -- * @see llvm::Type::isOpaquePointerTy()
  --  

function Pointer_Type_Is_Opaque
     (Ty : LLVM.Types.Type_T)
      return Boolean;

  --*
  -- * Create an opaque pointer type in a context.
  -- *
  -- * @see llvm::PointerType::get()
  --  

   function Pointer_Type_In_Context (C : LLVM.Types.Context_T; Address_Space : unsigned) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1484
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPointerTypeInContext";

  --*
  -- * Obtain the address space of a pointer type.
  -- *
  -- * This only works on types that represent pointers.
  -- *
  -- * @see llvm::PointerType::getAddressSpace()
  --  

   function Get_Pointer_Address_Space (Pointer_Ty : LLVM.Types.Type_T) return unsigned  -- install/include/llvm-c/Core.h:1493
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetPointerAddressSpace";

  --*
  -- * Create a vector type that contains a defined type and has a specific
  -- * number of elements.
  -- *
  -- * The created type will exist in the context thats its element type
  -- * exists in.
  -- *
  -- * @see llvm::VectorType::get()
  --  

   function Vector_Type (Element_Type : LLVM.Types.Type_T; Element_Count : unsigned) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1504
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMVectorType";

  --*
  -- * Create a vector type that contains a defined type and has a scalable
  -- * number of elements.
  -- *
  -- * The created type will exist in the context thats its element type
  -- * exists in.
  -- *
  -- * @see llvm::ScalableVectorType::get()
  --  

   function Scalable_Vector_Type (Element_Type : LLVM.Types.Type_T; Element_Count : unsigned) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1515
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMScalableVectorType";

  --*
  -- * Obtain the (possibly scalable) number of elements in a vector type.
  -- *
  -- * This only works on types that represent vectors (fixed or scalable).
  -- *
  -- * @see llvm::VectorType::getNumElements()
  --  

   function Get_Vector_Size (Vector_Ty : LLVM.Types.Type_T) return unsigned  -- install/include/llvm-c/Core.h:1525
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetVectorSize";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreTypeOther Other Types
  -- *
  -- * @{
  --  

  --*
  -- * Create a void type in a context.
  --  

   function Void_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1540
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMVoidTypeInContext";

  --*
  -- * Create a label type in a context.
  --  

   function Label_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1545
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMLabelTypeInContext";

  --*
  -- * Create a X86 MMX type in a context.
  --  

   function X86MMX_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1550
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMX86MMXTypeInContext";

  --*
  -- * Create a X86 AMX type in a context.
  --  

   function X86AMX_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1555
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMX86AMXTypeInContext";

  --*
  -- * Create a token type in a context.
  --  

   function Token_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1560
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMTokenTypeInContext";

  --*
  -- * Create a metadata type in a context.
  --  

   function Metadata_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1565
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMMetadataTypeInContext";

  --*
  -- * These are similar to the above functions except they operate on the
  -- * global context.
  --  

   function Void_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1571
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMVoidType";

   function Label_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1572
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMLabelType";

   function X86MMX_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1573
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMX86MMXType";

   function X86AMX_Type return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1574
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMX86AMXType";

  --*
  -- * Create a target extension type in LLVM context.
  --  

function Target_Ext_Type_In_Context
     (C                : LLVM.Types.Context_T;
      Name             : String;
      Type_Params      : System.Address;
      Type_Param_Count : unsigned;
      Int_Params       : access unsigned;
      Int_Param_Count  : unsigned)
      return LLVM.Types.Type_T;

  --*
  -- * @}
  --  

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValues Values
  -- *
  -- * The bulk of LLVM's object model consists of values, which comprise a very
  -- * rich type hierarchy.
  -- *
  -- * LLVMValueRef essentially represents llvm::Value. There is a rich
  -- * hierarchy of classes within this type. Depending on the instance
  -- * obtained, not all APIs are available.
  -- *
  -- * Callers can determine the type of an LLVMValueRef by calling the
  -- * LLVMIsA* family of functions (e.g. LLVMIsAArgument()). These
  -- * functions are defined by a macro, so it isn't obvious which are
  -- * available by looking at the Doxygen source code. Instead, look at the
  -- * source definition of LLVM_FOR_EACH_VALUE_SUBCLASS and note the list
  -- * of value names given. These value names also correspond to classes in
  -- * the llvm::Value hierarchy.
  -- *
  -- * @{
  --  

  --*
  -- * @defgroup LLVMCCoreValueGeneral General APIs
  -- *
  -- * Functions in this section work on all LLVMValueRef instances,
  -- * regardless of their sub-type. They correspond to functions available
  -- * on llvm::Value.
  -- *
  -- * @{
  --  

  --*
  -- * Obtain the type of a value.
  -- *
  -- * @see llvm::Value::getType()
  --  

   function Type_Of (Val : LLVM.Types.Value_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:1719
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMTypeOf";

  --*
  -- * Obtain the enumerated type of a Value instance.
  -- *
  -- * @see llvm::Value::getValueID()
  --  

   function Get_Value_Kind (Val : LLVM.Types.Value_T) return Value_Kind_T  -- install/include/llvm-c/Core.h:1726
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetValueKind";

  --*
  -- * Obtain the string name of a value.
  -- *
  -- * @see llvm::Value::getName()
  --  

function Get_Value_Name_2
     (Val    : LLVM.Types.Value_T;
      Length : access stddef_h.size_t)
      return String;

  --*
  -- * Set the string name of a value.
  -- *
  -- * @see llvm::Value::setName()
  --  

procedure Set_Value_Name_2
     (Val      : LLVM.Types.Value_T;
      Name     : String;
      Name_Len : stddef_h.size_t);

  --*
  -- * Dump a representation of a value to stderr.
  -- *
  -- * @see llvm::Value::dump()
  --  

   procedure Dump_Value (Val : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:1747
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDumpValue";

  --*
  -- * Return a string representation of the value. Use
  -- * LLVMDisposeMessage to free the string.
  -- *
  -- * @see llvm::Value::print()
  --  

function Print_Value_To_String
     (Val : LLVM.Types.Value_T)
      return String;

  --*
  -- * Replace all uses of a value with another one.
  -- *
  -- * @see llvm::Value::replaceAllUsesWith()
  --  

   procedure Replace_All_Uses_With (Old_Val : LLVM.Types.Value_T; New_Val : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:1762
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMReplaceAllUsesWith";

  --*
  -- * Determine whether the specified value instance is constant.
  --  

function Is_Constant
     (Val : LLVM.Types.Value_T)
      return Boolean;

  --*
  -- * Determine whether a value instance is undefined.
  --  

function Is_Undef
     (Val : LLVM.Types.Value_T)
      return Boolean;

  --*
  -- * Determine whether a value instance is poisonous.
  --  

function Is_Poison
     (Val : LLVM.Types.Value_T)
      return Boolean;

  --*
  -- * Convert value instances between types.
  -- *
  -- * Internally, an LLVMValueRef is "pinned" to a specific type. This
  -- * series of functions allows you to cast an instance to a specific
  -- * type.
  -- *
  -- * If the cast is not valid for the specified type, NULL is returned.
  -- *
  -- * @see llvm::dyn_cast_or_null<>
  --  

   function Is_A_Argument (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAArgument";

   function Is_A_Basic_Block (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsABasicBlock";

   function Is_A_Inline_Asm (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAInlineAsm";

   function Is_A_User (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAUser";

   function Is_A_Constant (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAConstant";

   function Is_A_Block_Address (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsABlockAddress";

   function Is_A_Constant_Aggregate_Zero (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAConstantAggregateZero";

   function Is_A_Constant_Array (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAConstantArray";

   function Is_A_Constant_Data_Sequential (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAConstantDataSequential";

   function Is_A_Constant_Data_Array (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAConstantDataArray";

   function Is_A_Constant_Data_Vector (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAConstantDataVector";

   function Is_A_Constant_Expr (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAConstantExpr";

   function Is_A_Constant_FP (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAConstantFP";

   function Is_A_Constant_Int (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAConstantInt";

   function Is_A_Constant_Pointer_Null (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAConstantPointerNull";

   function Is_A_Constant_Struct (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAConstantStruct";

   function Is_A_Constant_Token_None (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAConstantTokenNone";

   function Is_A_Constant_Vector (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAConstantVector";

   function Is_A_Global_Value (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAGlobalValue";

   function Is_A_Global_Alias (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAGlobalAlias";

   function Is_A_Global_Object (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAGlobalObject";

   function Is_A_Function (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAFunction";

   function Is_A_Global_Variable (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAGlobalVariable";

   function Is_A_Global_I_Func (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAGlobalIFunc";

   function Is_A_Undef_Value (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAUndefValue";

   function Is_A_Poison_Value (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAPoisonValue";

   function Is_A_Instruction (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAInstruction";

   function Is_A_Unary_Operator (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAUnaryOperator";

   function Is_A_Binary_Operator (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsABinaryOperator";

   function Is_A_Call_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsACallInst";

   function Is_A_Intrinsic_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAIntrinsicInst";

   function Is_A_Dbg_Info_Intrinsic (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsADbgInfoIntrinsic";

   function Is_A_Dbg_Variable_Intrinsic (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsADbgVariableIntrinsic";

   function Is_A_Dbg_Declare_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsADbgDeclareInst";

   function Is_A_Dbg_Label_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsADbgLabelInst";

   function Is_A_Mem_Intrinsic (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAMemIntrinsic";

   function Is_A_Mem_Cpy_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAMemCpyInst";

   function Is_A_Mem_Move_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAMemMoveInst";

   function Is_A_Mem_Set_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAMemSetInst";

   function Is_A_Cmp_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsACmpInst";

   function Is_AF_Cmp_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAFCmpInst";

   function Is_AI_Cmp_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAICmpInst";

   function Is_A_Extract_Element_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAExtractElementInst";

   function Is_A_Get_Element_Ptr_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAGetElementPtrInst";

   function Is_A_Insert_Element_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAInsertElementInst";

   function Is_A_Insert_Value_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAInsertValueInst";

   function Is_A_Landing_Pad_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsALandingPadInst";

   function Is_APHI_Node (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAPHINode";

   function Is_A_Select_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsASelectInst";

   function Is_A_Shuffle_Vector_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAShuffleVectorInst";

   function Is_A_Store_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAStoreInst";

   function Is_A_Branch_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsABranchInst";

   function Is_A_Indirect_Br_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAIndirectBrInst";

   function Is_A_Invoke_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAInvokeInst";

   function Is_A_Return_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAReturnInst";

   function Is_A_Switch_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsASwitchInst";

   function Is_A_Unreachable_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAUnreachableInst";

   function Is_A_Resume_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAResumeInst";

   function Is_A_Cleanup_Return_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsACleanupReturnInst";

   function Is_A_Catch_Return_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsACatchReturnInst";

   function Is_A_Catch_Switch_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsACatchSwitchInst";

   function Is_A_Call_Br_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsACallBrInst";

   function Is_A_Funclet_Pad_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAFuncletPadInst";

   function Is_A_Catch_Pad_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsACatchPadInst";

   function Is_A_Cleanup_Pad_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsACleanupPadInst";

   function Is_A_Unary_Instruction (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAUnaryInstruction";

   function Is_A_Alloca_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAAllocaInst";

   function Is_A_Cast_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsACastInst";

   function Is_A_Addr_Space_Cast_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAAddrSpaceCastInst";

   function Is_A_Bit_Cast_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsABitCastInst";

   function Is_AFP_Ext_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAFPExtInst";

   function Is_AFP_To_SI_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAFPToSIInst";

   function Is_AFP_To_UI_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAFPToUIInst";

   function Is_AFP_Trunc_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAFPTruncInst";

   function Is_A_Int_To_Ptr_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAIntToPtrInst";

   function Is_A_Ptr_To_Int_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAPtrToIntInst";

   function Is_AS_Ext_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsASExtInst";

   function Is_ASI_To_FP_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsASIToFPInst";

   function Is_A_Trunc_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsATruncInst";

   function Is_AUI_To_FP_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAUIToFPInst";

   function Is_AZ_Ext_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAZExtInst";

   function Is_A_Extract_Value_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAExtractValueInst";

   function Is_A_Load_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsALoadInst";

   function Is_AVA_Arg_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAVAArgInst";

   function Is_A_Freeze_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAFreezeInst";

   function Is_A_Atomic_Cmp_Xchg_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAAtomicCmpXchgInst";

   function Is_A_Atomic_RMW_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAAtomicRMWInst";

   function Is_A_Fence_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1792
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAFenceInst";

   function Is_AMD_Node (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1794
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAMDNode";

   function Is_AMD_String (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1795
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsAMDString";

  --* Deprecated: Use LLVMGetValueName2 instead.  
function Get_Value_Name
     (Val : LLVM.Types.Value_T)
      return String;

  --* Deprecated: Use LLVMSetValueName2 instead.  
procedure Set_Value_Name
     (Val  : LLVM.Types.Value_T;
      Name : String);

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueUses Usage
  -- *
  -- * This module defines functions that allow you to inspect the uses of a
  -- * LLVMValueRef.
  -- *
  -- * It is possible to obtain an LLVMUseRef for any LLVMValueRef instance.
  -- * Each LLVMUseRef (which corresponds to a llvm::Use instance) holds a
  -- * llvm::User and llvm::Value.
  -- *
  -- * @{
  --  

  --*
  -- * Obtain the first use of a value.
  -- *
  -- * Uses are obtained in an iterator fashion. First, call this function
  -- * to obtain a reference to the first use. Then, call LLVMGetNextUse()
  -- * on that instance and all subsequently obtained instances until
  -- * LLVMGetNextUse() returns NULL.
  -- *
  -- * @see llvm::Value::use_begin()
  --  

   function Get_First_Use (Val : LLVM.Types.Value_T) return LLVM.Types.Use_T  -- install/include/llvm-c/Core.h:1829
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetFirstUse";

  --*
  -- * Obtain the next use of a value.
  -- *
  -- * This effectively advances the iterator. It returns NULL if you are on
  -- * the final use and no more are available.
  --  

   function Get_Next_Use (U : LLVM.Types.Use_T) return LLVM.Types.Use_T  -- install/include/llvm-c/Core.h:1837
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNextUse";

  --*
  -- * Obtain the user value for a user.
  -- *
  -- * The returned value corresponds to a llvm::User type.
  -- *
  -- * @see llvm::Use::getUser()
  --  

   function Get_User (U : LLVM.Types.Use_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1846
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetUser";

  --*
  -- * Obtain the value this use corresponds to.
  -- *
  -- * @see llvm::Use::get().
  --  

   function Get_Used_Value (U : LLVM.Types.Use_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1853
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetUsedValue";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueUser User value
  -- *
  -- * Function in this group pertain to LLVMValueRef instances that descent
  -- * from llvm::User. This includes constants, instructions, and
  -- * operators.
  -- *
  -- * @{
  --  

  --*
  -- * Obtain an operand at a specific index in a llvm::User value.
  -- *
  -- * @see llvm::User::getOperand()
  --  

   function Get_Operand (Val : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1874
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetOperand";

  --*
  -- * Obtain the use of an operand at a specific index in a llvm::User value.
  -- *
  -- * @see llvm::User::getOperandUse()
  --  

   function Get_Operand_Use (Val : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Use_T  -- install/include/llvm-c/Core.h:1881
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetOperandUse";

  --*
  -- * Set an operand at a specific index in a llvm::User value.
  -- *
  -- * @see llvm::User::setOperand()
  --  

   procedure Set_Operand
     (User : LLVM.Types.Value_T;
      Index : unsigned;
      Val : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:1888
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetOperand";

  --*
  -- * Obtain the number of operands in a llvm::User value.
  -- *
  -- * @see llvm::User::getNumOperands()
  --  

   function Get_Num_Operands (Val : LLVM.Types.Value_T) return int  -- install/include/llvm-c/Core.h:1895
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNumOperands";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueConstant Constants
  -- *
  -- * This section contains APIs for interacting with LLVMValueRef that
  -- * correspond to llvm::Constant instances.
  -- *
  -- * These functions will work for any LLVMValueRef in the llvm::Constant
  -- * class hierarchy.
  -- *
  -- * @{
  --  

  --*
  -- * Obtain a constant value referring to the null instance of a type.
  -- *
  -- * @see llvm::Constant::getNullValue()
  --  

  -- all zeroes  
   function Const_Null (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1918
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstNull";

  --*
  -- * Obtain a constant value referring to the instance of a type
  -- * consisting of all ones.
  -- *
  -- * This is only valid for integer types.
  -- *
  -- * @see llvm::Constant::getAllOnesValue()
  --  

   function Const_All_Ones (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1928
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstAllOnes";

  --*
  -- * Obtain a constant value referring to an undefined value of a type.
  -- *
  -- * @see llvm::UndefValue::get()
  --  

   function Get_Undef (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1935
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetUndef";

  --*
  -- * Obtain a constant value referring to a poison value of a type.
  -- *
  -- * @see llvm::PoisonValue::get()
  --  

   function Get_Poison (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1942
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetPoison";

  --*
  -- * Determine whether a value instance is null.
  -- *
  -- * @see llvm::Constant::isNullValue()
  --  

function Is_Null
     (Val : LLVM.Types.Value_T)
      return Boolean;

  --*
  -- * Obtain a constant that is a constant pointer pointing to NULL for a
  -- * specified type.
  --  

   function Const_Pointer_Null (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1955
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstPointerNull";

  --*
  -- * @defgroup LLVMCCoreValueConstantScalar Scalar constants
  -- *
  -- * Functions in this group model LLVMValueRef instances that correspond
  -- * to constants referring to scalar types.
  -- *
  -- * For integer types, the LLVMTypeRef parameter should correspond to a
  -- * llvm::IntegerType instance and the returned LLVMValueRef will
  -- * correspond to a llvm::ConstantInt.
  -- *
  -- * For floating point types, the LLVMTypeRef returned corresponds to a
  -- * llvm::ConstantFP.
  -- *
  -- * @{
  --  

  --*
  -- * Obtain a constant value for an integer type.
  -- *
  -- * The returned value corresponds to a llvm::ConstantInt.
  -- *
  -- * @see llvm::ConstantInt::get()
  -- *
  -- * @param IntTy Integer type to obtain value of.
  -- * @param N The value the returned instance should refer to.
  -- * @param SignExtend Whether to sign extend the produced value.
  --  

function Const_Int
     (Int_Ty      : LLVM.Types.Type_T;
      N           : Extensions.unsigned_long_long;
      Sign_Extend : Boolean)
      return LLVM.Types.Value_T;

  --*
  -- * Obtain a constant value for an integer of arbitrary precision.
  -- *
  -- * @see llvm::ConstantInt::get()
  --  

   function Const_Int_Of_Arbitrary_Precision
     (Int_Ty : LLVM.Types.Type_T;
      Num_Words : unsigned;
      Words : access stdint_h.uint64_t) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:1992
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstIntOfArbitraryPrecision";

  --*
  -- * Obtain a constant value for an integer parsed from a string.
  -- *
  -- * A similar API, LLVMConstIntOfStringAndSize is also available. If the
  -- * string's length is available, it is preferred to call that function
  -- * instead.
  -- *
  -- * @see llvm::ConstantInt::get()
  --  

function Const_Int_Of_String
     (Int_Ty : LLVM.Types.Type_T;
      Text   : String;
      Radix  : stdint_h.uint8_t)
      return LLVM.Types.Value_T;

  --*
  -- * Obtain a constant value for an integer parsed from a string with
  -- * specified length.
  -- *
  -- * @see llvm::ConstantInt::get()
  --  

function Const_Int_Of_String_And_Size
     (Int_Ty : LLVM.Types.Type_T;
      Text   : String;
      S_Len  : unsigned;
      Radix  : stdint_h.uint8_t)
      return LLVM.Types.Value_T;

  --*
  -- * Obtain a constant value referring to a double floating point value.
  --  

   function Const_Real (Real_Ty : LLVM.Types.Type_T; N : double) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2020
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstReal";

  --*
  -- * Obtain a constant for a floating point value parsed from a string.
  -- *
  -- * A similar API, LLVMConstRealOfStringAndSize is also available. It
  -- * should be used if the input string's length is known.
  --  

function Const_Real_Of_String
     (Real_Ty : LLVM.Types.Type_T;
      Text    : String)
      return LLVM.Types.Value_T;

  --*
  -- * Obtain a constant for a floating point value parsed from a string.
  --  

function Const_Real_Of_String_And_Size
     (Real_Ty : LLVM.Types.Type_T;
      Text    : String;
      S_Len   : unsigned)
      return LLVM.Types.Value_T;

  --*
  -- * Obtain the zero extended value for an integer constant value.
  -- *
  -- * @see llvm::ConstantInt::getZExtValue()
  --  

   function Const_Int_Get_Z_Ext_Value (Constant_Val : LLVM.Types.Value_T) return Extensions.unsigned_long_long  -- install/include/llvm-c/Core.h:2041
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstIntGetZExtValue";

  --*
  -- * Obtain the sign extended value for an integer constant value.
  -- *
  -- * @see llvm::ConstantInt::getSExtValue()
  --  

   function Const_Int_Get_S_Ext_Value (Constant_Val : LLVM.Types.Value_T) return Long_Long_Integer  -- install/include/llvm-c/Core.h:2048
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstIntGetSExtValue";

  --*
  -- * Obtain the double value for an floating point constant value.
  -- * losesInfo indicates if some precision was lost in the conversion.
  -- *
  -- * @see llvm::ConstantFP::getDoubleValue
  --  

   function Const_Real_Get_Double (Constant_Val : LLVM.Types.Value_T; Loses_Info : access LLVM.Types.Bool_T) return double  -- install/include/llvm-c/Core.h:2056
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstRealGetDouble";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueConstantComposite Composite Constants
  -- *
  -- * Functions in this group operate on composite constants.
  -- *
  -- * @{
  --  

  --*
  -- * Create a ConstantDataSequential and initialize it with a string.
  -- *
  -- * @see llvm::ConstantDataArray::getString()
  --  

function Const_String_In_Context
     (C                   : LLVM.Types.Context_T;
      Str                 : String;
      Length              : unsigned;
      Dont_Null_Terminate : Boolean)
      return LLVM.Types.Value_T;

  --*
  -- * Create a ConstantDataSequential with string content in the global context.
  -- *
  -- * This is the same as LLVMConstStringInContext except it operates on the
  -- * global context.
  -- *
  -- * @see LLVMConstStringInContext()
  -- * @see llvm::ConstantDataArray::getString()
  --  

function Const_String
     (Str                 : String;
      Length              : unsigned;
      Dont_Null_Terminate : Boolean)
      return LLVM.Types.Value_T;

  --*
  -- * Returns true if the specified constant is an array of i8.
  -- *
  -- * @see ConstantDataSequential::getAsString()
  --  

function Is_Constant_String
     (C : LLVM.Types.Value_T)
      return Boolean;

  --*
  -- * Get the given constant data sequential as a string.
  -- *
  -- * @see ConstantDataSequential::getAsString()
  --  

function Get_As_String
     (C      : LLVM.Types.Value_T;
      Length : access stddef_h.size_t)
      return String;

  --*
  -- * Create an anonymous ConstantStruct with the specified values.
  -- *
  -- * @see llvm::ConstantStruct::getAnon()
  --  

function Const_Struct_In_Context
     (C             : LLVM.Types.Context_T;
      Constant_Vals : System.Address;
      Count         : unsigned;
      Packed        : Boolean)
      return LLVM.Types.Value_T;

  --*
  -- * Create a ConstantStruct in the global Context.
  -- *
  -- * This is the same as LLVMConstStructInContext except it operates on the
  -- * global Context.
  -- *
  -- * @see LLVMConstStructInContext()
  --  

function Const_Struct
     (Constant_Vals : System.Address;
      Count         : unsigned;
      Packed        : Boolean)
      return LLVM.Types.Value_T;

  --*
  -- * Create a ConstantArray from values.
  -- *
  -- * @see llvm::ConstantArray::get()
  --  

   function Const_Array
     (Element_Ty : LLVM.Types.Type_T;
      Constant_Vals : System.Address;
      Length : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2129
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstArray";

  --*
  -- * Create a non-anonymous ConstantStruct from values.
  -- *
  -- * @see llvm::ConstantStruct::get()
  --  

   function Const_Named_Struct
     (Struct_Ty : LLVM.Types.Type_T;
      Constant_Vals : System.Address;
      Count : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2137
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstNamedStruct";

  --*
  -- * Get element of a constant aggregate (struct, array or vector) at the
  -- * specified index. Returns null if the index is out of range, or it's not
  -- * possible to determine the element (e.g., because the constant is a
  -- * constant expression.)
  -- *
  -- * @see llvm::Constant::getAggregateElement()
  --  

   function Get_Aggregate_Element (C : LLVM.Types.Value_T; Idx : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2149
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetAggregateElement";

  --*
  -- * Get an element at specified index as a constant.
  -- *
  -- * @see ConstantDataSequential::getElementAsConstant()
  --  

   function Get_Element_As_Constant (C : LLVM.Types.Value_T; Idx : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2156
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetElementAsConstant";

  --*
  -- * Create a ConstantVector from values.
  -- *
  -- * @see llvm::ConstantVector::get()
  --  

   function Const_Vector (Scalar_Constant_Vals : System.Address; Size : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2165
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstVector";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueConstantExpressions Constant Expressions
  -- *
  -- * Functions in this group correspond to APIs on llvm::ConstantExpr.
  -- *
  -- * @see llvm::ConstantExpr.
  -- *
  -- * @{
  --  

   function Get_Const_Opcode (Constant_Val : LLVM.Types.Value_T) return Opcode_T  -- install/include/llvm-c/Core.h:2180
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetConstOpcode";

   function Align_Of (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2181
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAlignOf";

   function Size_Of (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2182
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSizeOf";

   function Const_Neg (Constant_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2183
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstNeg";

   function Const_NSW_Neg (Constant_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2184
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstNSWNeg";

   function Const_NUW_Neg (Constant_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2185
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstNUWNeg";

   function Const_Not (Constant_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2186
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstNot";

   function Const_Add (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2187
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstAdd";

   function Const_NSW_Add (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2188
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstNSWAdd";

   function Const_NUW_Add (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2189
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstNUWAdd";

   function Const_Sub (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2190
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstSub";

   function Const_NSW_Sub (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2191
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstNSWSub";

   function Const_NUW_Sub (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2192
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstNUWSub";

   function Const_Mul (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2193
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstMul";

   function Const_NSW_Mul (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2194
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstNSWMul";

   function Const_NUW_Mul (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2195
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstNUWMul";

   function Const_And (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2196
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstAnd";

   function Const_Or (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2197
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstOr";

   function Const_Xor (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2198
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstXor";

   function Const_I_Cmp
     (Predicate : Int_Predicate_T;
      LHS_Constant : LLVM.Types.Value_T;
      RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2199
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstICmp";

   function Const_F_Cmp
     (Predicate : Real_Predicate_T;
      LHS_Constant : LLVM.Types.Value_T;
      RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2201
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstFCmp";

   function Const_Shl (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2203
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstShl";

   function Const_L_Shr (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2204
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstLShr";

   function Const_A_Shr (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2205
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstAShr";

   function Const_GEP2
     (Ty : LLVM.Types.Type_T;
      Constant_Val : LLVM.Types.Value_T;
      Constant_Indices : System.Address;
      Num_Indices : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2206
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstGEP2";

   function Const_In_Bounds_GEP2
     (Ty : LLVM.Types.Type_T;
      Constant_Val : LLVM.Types.Value_T;
      Constant_Indices : System.Address;
      Num_Indices : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2208
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstInBoundsGEP2";

   function Const_Trunc (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2211
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstTrunc";

   function Const_S_Ext (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2212
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstSExt";

   function Const_Z_Ext (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2213
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstZExt";

   function Const_FP_Trunc (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2214
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstFPTrunc";

   function Const_FP_Ext (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2215
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstFPExt";

   function Const_UI_To_FP (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2216
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstUIToFP";

   function Const_SI_To_FP (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2217
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstSIToFP";

   function Const_FP_To_UI (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2218
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstFPToUI";

   function Const_FP_To_SI (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2219
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstFPToSI";

   function Const_Ptr_To_Int (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2220
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstPtrToInt";

   function Const_Int_To_Ptr (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2221
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstIntToPtr";

   function Const_Bit_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2222
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstBitCast";

   function Const_Addr_Space_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2223
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstAddrSpaceCast";

   function Const_Z_Ext_Or_Bit_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2224
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstZExtOrBitCast";

   function Const_S_Ext_Or_Bit_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2226
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstSExtOrBitCast";

   function Const_Trunc_Or_Bit_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2228
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstTruncOrBitCast";

   function Const_Pointer_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2230
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstPointerCast";

function Const_Int_Cast
     (Constant_Val : LLVM.Types.Value_T;
      To_Type      : LLVM.Types.Type_T;
      Is_Signed    : Boolean)
      return LLVM.Types.Value_T;

   function Const_FP_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2234
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstFPCast";

   function Const_Select
     (Constant_Condition : LLVM.Types.Value_T;
      Constant_If_True : LLVM.Types.Value_T;
      Constant_If_False : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2235
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstSelect";

   function Const_Extract_Element (Vector_Constant : LLVM.Types.Value_T; Index_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2238
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstExtractElement";

   function Const_Insert_Element
     (Vector_Constant : LLVM.Types.Value_T;
      Element_Value_Constant : LLVM.Types.Value_T;
      Index_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2240
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstInsertElement";

   function Const_Shuffle_Vector
     (Vector_A_Constant : LLVM.Types.Value_T;
      Vector_B_Constant : LLVM.Types.Value_T;
      Mask_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2243
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConstShuffleVector";

   function Block_Address (F : LLVM.Types.Value_T; BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2246
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBlockAddress";

  --* Deprecated: Use LLVMGetInlineAsm instead.  
function Const_Inline_Asm
     (Ty               : LLVM.Types.Type_T;
      Asm_String       : String;
      Constraints      : String;
      Has_Side_Effects : Boolean;
      Is_Align_Stack   : Boolean)
      return LLVM.Types.Value_T;

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueConstantGlobals Global Values
  -- *
  -- * This group contains functions that operate on global values. Functions in
  -- * this group relate to functions in the llvm::GlobalValue class tree.
  -- *
  -- * @see llvm::GlobalValue
  -- *
  -- * @{
  --  

   function Get_Global_Parent (Global : LLVM.Types.Value_T) return LLVM.Types.Module_T  -- install/include/llvm-c/Core.h:2268
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetGlobalParent";

function Is_Declaration
     (Global : LLVM.Types.Value_T)
      return Boolean;

   function Get_Linkage (Global : LLVM.Types.Value_T) return Linkage_T  -- install/include/llvm-c/Core.h:2270
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetLinkage";

   procedure Set_Linkage (Global : LLVM.Types.Value_T; Linkage : Linkage_T)  -- install/include/llvm-c/Core.h:2271
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetLinkage";

function Get_Section
     (Global : LLVM.Types.Value_T)
      return String;

procedure Set_Section
     (Global  : LLVM.Types.Value_T;
      Section : String);

   function Get_Visibility (Global : LLVM.Types.Value_T) return Visibility_T  -- install/include/llvm-c/Core.h:2274
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetVisibility";

   procedure Set_Visibility (Global : LLVM.Types.Value_T; Viz : Visibility_T)  -- install/include/llvm-c/Core.h:2275
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetVisibility";

   function Get_DLL_Storage_Class (Global : LLVM.Types.Value_T) return DLL_Storage_Class_T  -- install/include/llvm-c/Core.h:2276
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetDLLStorageClass";

   procedure Set_DLL_Storage_Class (Global : LLVM.Types.Value_T; Class : DLL_Storage_Class_T)  -- install/include/llvm-c/Core.h:2277
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetDLLStorageClass";

   function Get_Unnamed_Address (Global : LLVM.Types.Value_T) return Unnamed_Addr_T  -- install/include/llvm-c/Core.h:2278
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetUnnamedAddress";

   procedure Set_Unnamed_Address (Global : LLVM.Types.Value_T; Unnamed_Addr : Unnamed_Addr_T)  -- install/include/llvm-c/Core.h:2279
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetUnnamedAddress";

  --*
  -- * Returns the "value type" of a global value.  This differs from the formal
  -- * type of a global value which is always a pointer type.
  -- *
  -- * @see llvm::GlobalValue::getValueType()
  --  

   function Global_Get_Value_Type (Global : LLVM.Types.Value_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:2287
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGlobalGetValueType";

  --* Deprecated: Use LLVMGetUnnamedAddress instead.  
function Has_Unnamed_Addr
     (Global : LLVM.Types.Value_T)
      return Boolean;

  --* Deprecated: Use LLVMSetUnnamedAddress instead.  
procedure Set_Unnamed_Addr
     (Global           : LLVM.Types.Value_T;
      Has_Unnamed_Addr : Boolean);

  --*
  -- * @defgroup LLVMCCoreValueWithAlignment Values with alignment
  -- *
  -- * Functions in this group only apply to values with alignment, i.e.
  -- * global variables, load and store instructions.
  --  

  --*
  -- * Obtain the preferred alignment of the value.
  -- * @see llvm::AllocaInst::getAlignment()
  -- * @see llvm::LoadInst::getAlignment()
  -- * @see llvm::StoreInst::getAlignment()
  -- * @see llvm::AtomicRMWInst::setAlignment()
  -- * @see llvm::AtomicCmpXchgInst::setAlignment()
  -- * @see llvm::GlobalValue::getAlignment()
  --  

   function Get_Alignment (V : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:2310
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetAlignment";

  --*
  -- * Set the preferred alignment of the value.
  -- * @see llvm::AllocaInst::setAlignment()
  -- * @see llvm::LoadInst::setAlignment()
  -- * @see llvm::StoreInst::setAlignment()
  -- * @see llvm::AtomicRMWInst::setAlignment()
  -- * @see llvm::AtomicCmpXchgInst::setAlignment()
  -- * @see llvm::GlobalValue::setAlignment()
  --  

   procedure Set_Alignment (V : LLVM.Types.Value_T; Bytes : unsigned)  -- install/include/llvm-c/Core.h:2321
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetAlignment";

  --*
  -- * Sets a metadata attachment, erasing the existing metadata attachment if
  -- * it already exists for the given kind.
  -- *
  -- * @see llvm::GlobalObject::setMetadata()
  --  

   procedure Global_Set_Metadata
     (Global : LLVM.Types.Value_T;
      Kind : unsigned;
      MD : LLVM.Types.Metadata_T)  -- install/include/llvm-c/Core.h:2329
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGlobalSetMetadata";

  --*
  -- * Erases a metadata attachment of the given kind if it exists.
  -- *
  -- * @see llvm::GlobalObject::eraseMetadata()
  --  

   procedure Global_Erase_Metadata (Global : LLVM.Types.Value_T; Kind : unsigned)  -- install/include/llvm-c/Core.h:2337
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGlobalEraseMetadata";

  --*
  -- * Removes all metadata attachments from this value.
  -- *
  -- * @see llvm::GlobalObject::clearMetadata()
  --  

   procedure Global_Clear_Metadata (Global : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:2344
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGlobalClearMetadata";

  --*
  -- * Retrieves an array of metadata entries representing the metadata attached to
  -- * this value. The caller is responsible for freeing this array by calling
  -- * \c LLVMDisposeValueMetadataEntries.
  -- *
  -- * @see llvm::GlobalObject::getAllMetadata()
  --  

   function Global_Copy_All_Metadata (Value : LLVM.Types.Value_T; Num_Entries : access stddef_h.size_t) return access LLVM.Types.Opaque_Value_Metadata_Entry_Impl_T  -- install/include/llvm-c/Core.h:2353
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGlobalCopyAllMetadata";

  --*
  -- * Destroys value metadata entries.
  --  

   procedure Dispose_Value_Metadata_Entries (Entries : access LLVM.Types.Opaque_Value_Metadata_Entry_Impl_T)  -- install/include/llvm-c/Core.h:2359
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeValueMetadataEntries";

  --*
  -- * Returns the kind of a value metadata entry at a specific index.
  --  

   function Value_Metadata_Entries_Get_Kind (Entries : access LLVM.Types.Opaque_Value_Metadata_Entry_Impl_T; Index : unsigned) return unsigned  -- install/include/llvm-c/Core.h:2364
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMValueMetadataEntriesGetKind";

  --*
  -- * Returns the underlying metadata node of a value metadata entry at a
  -- * specific index.
  --  

   function Value_Metadata_Entries_Get_Metadata (Entries : access LLVM.Types.Opaque_Value_Metadata_Entry_Impl_T; Index : unsigned) return LLVM.Types.Metadata_T  -- install/include/llvm-c/Core.h:2372
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMValueMetadataEntriesGetMetadata";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCoreValueConstantGlobalVariable Global Variables
  -- *
  -- * This group contains functions that operate on global variable values.
  -- *
  -- * @see llvm::GlobalVariable
  -- *
  -- * @{
  --  

function Add_Global
     (M    : LLVM.Types.Module_T;
      Ty   : LLVM.Types.Type_T;
      Name : String)
      return LLVM.Types.Value_T;

function Add_Global_In_Address_Space
     (M             : LLVM.Types.Module_T;
      Ty            : LLVM.Types.Type_T;
      Name          : String;
      Address_Space : unsigned)
      return LLVM.Types.Value_T;

function Get_Named_Global
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Value_T;

   function Get_First_Global (M : LLVM.Types.Module_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2393
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetFirstGlobal";

   function Get_Last_Global (M : LLVM.Types.Module_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2394
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetLastGlobal";

   function Get_Next_Global (Global_Var : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2395
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNextGlobal";

   function Get_Previous_Global (Global_Var : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2396
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetPreviousGlobal";

   procedure Delete_Global (Global_Var : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:2397
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDeleteGlobal";

   function Get_Initializer (Global_Var : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2398
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetInitializer";

   procedure Set_Initializer (Global_Var : LLVM.Types.Value_T; Constant_Val : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:2399
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetInitializer";

function Is_Thread_Local
     (Global_Var : LLVM.Types.Value_T)
      return Boolean;

procedure Set_Thread_Local
     (Global_Var      : LLVM.Types.Value_T;
      Is_Thread_Local : Boolean);

function Is_Global_Constant
     (Global_Var : LLVM.Types.Value_T)
      return Boolean;

procedure Set_Global_Constant
     (Global_Var  : LLVM.Types.Value_T;
      Is_Constant : Boolean);

   function Get_Thread_Local_Mode (Global_Var : LLVM.Types.Value_T) return Thread_Local_Mode_T  -- install/include/llvm-c/Core.h:2404
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetThreadLocalMode";

   procedure Set_Thread_Local_Mode (Global_Var : LLVM.Types.Value_T; Mode : Thread_Local_Mode_T)  -- install/include/llvm-c/Core.h:2405
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetThreadLocalMode";

function Is_Externally_Initialized
     (Global_Var : LLVM.Types.Value_T)
      return Boolean;

procedure Set_Externally_Initialized
     (Global_Var  : LLVM.Types.Value_T;
      Is_Ext_Init : Boolean);

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCoreValueConstantGlobalAlias Global Aliases
  -- *
  -- * This group contains function that operate on global alias values.
  -- *
  -- * @see llvm::GlobalAlias
  -- *
  -- * @{
  --  

  --*
  -- * Add a GlobalAlias with the given value type, address space and aliasee.
  -- *
  -- * @see llvm::GlobalAlias::create()
  --  

function Add_Alias_2
     (M          : LLVM.Types.Module_T;
      Value_Ty   : LLVM.Types.Type_T;
      Addr_Space : unsigned;
      Aliasee    : LLVM.Types.Value_T;
      Name       : String)
      return LLVM.Types.Value_T;

  --*
  -- * Obtain a GlobalAlias value from a Module by its name.
  -- *
  -- * The returned value corresponds to a llvm::GlobalAlias value.
  -- *
  -- * @see llvm::Module::getNamedAlias()
  --  

function Get_Named_Global_Alias
     (M        : LLVM.Types.Module_T;
      Name     : String;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Value_T;

  --*
  -- * Obtain an iterator to the first GlobalAlias in a Module.
  -- *
  -- * @see llvm::Module::alias_begin()
  --  

   function Get_First_Global_Alias (M : LLVM.Types.Module_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2447
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetFirstGlobalAlias";

  --*
  -- * Obtain an iterator to the last GlobalAlias in a Module.
  -- *
  -- * @see llvm::Module::alias_end()
  --  

   function Get_Last_Global_Alias (M : LLVM.Types.Module_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2454
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetLastGlobalAlias";

  --*
  -- * Advance a GlobalAlias iterator to the next GlobalAlias.
  -- *
  -- * Returns NULL if the iterator was already at the end and there are no more
  -- * global aliases.
  --  

   function Get_Next_Global_Alias (GA : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2462
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNextGlobalAlias";

  --*
  -- * Decrement a GlobalAlias iterator to the previous GlobalAlias.
  -- *
  -- * Returns NULL if the iterator was already at the beginning and there are
  -- * no previous global aliases.
  --  

   function Get_Previous_Global_Alias (GA : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2470
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetPreviousGlobalAlias";

  --*
  -- * Retrieve the target value of an alias.
  --  

   function Alias_Get_Aliasee (Alias : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2475
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAliasGetAliasee";

  --*
  -- * Set the target value of an alias.
  --  

   procedure Alias_Set_Aliasee (Alias : LLVM.Types.Value_T; Aliasee : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:2480
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAliasSetAliasee";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueFunction Function values
  -- *
  -- * Functions in this group operate on LLVMValueRef instances that
  -- * correspond to llvm::Function instances.
  -- *
  -- * @see llvm::Function
  -- *
  -- * @{
  --  

  --*
  -- * Remove a function from its containing module and deletes it.
  -- *
  -- * @see llvm::Function::eraseFromParent()
  --  

   procedure Delete_Function (Fn : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:2502
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDeleteFunction";

  --*
  -- * Check whether the given function has a personality function.
  -- *
  -- * @see llvm::Function::hasPersonalityFn()
  --  

function Has_Personality_Fn
     (Fn : LLVM.Types.Value_T)
      return Boolean;

  --*
  -- * Obtain the personality function attached to the function.
  -- *
  -- * @see llvm::Function::getPersonalityFn()
  --  

   function Get_Personality_Fn (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2516
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetPersonalityFn";

  --*
  -- * Set the personality function attached to the function.
  -- *
  -- * @see llvm::Function::setPersonalityFn()
  --  

   procedure Set_Personality_Fn (Fn : LLVM.Types.Value_T; Personality_Fn : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:2523
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetPersonalityFn";

  --*
  -- * Obtain the intrinsic ID number which matches the given function name.
  -- *
  -- * @see llvm::Function::lookupIntrinsicID()
  --  

function Lookup_Intrinsic_ID
     (Name     : String;
      Name_Len : stddef_h.size_t)
      return unsigned;

  --*
  -- * Obtain the ID number from a function instance.
  -- *
  -- * @see llvm::Function::getIntrinsicID()
  --  

   function Get_Intrinsic_ID (Fn : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:2537
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetIntrinsicID";

  --*
  -- * Create or insert the declaration of an intrinsic.  For overloaded intrinsics,
  -- * parameter types must be provided to uniquely identify an overload.
  -- *
  -- * @see llvm::Intrinsic::getDeclaration()
  --  

   function Get_Intrinsic_Declaration
     (C_Mod : LLVM.Types.Module_T;
      ID : unsigned;
      Param_Types : System.Address;
      Param_Count : stddef_h.size_t) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2545
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetIntrinsicDeclaration";

  --*
  -- * Retrieves the type of an intrinsic.  For overloaded intrinsics, parameter
  -- * types must be provided to uniquely identify an overload.
  -- *
  -- * @see llvm::Intrinsic::getType()
  --  

   function Intrinsic_Get_Type
     (Ctx : LLVM.Types.Context_T;
      ID : unsigned;
      Param_Types : System.Address;
      Param_Count : stddef_h.size_t) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:2556
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIntrinsicGetType";

  --*
  -- * Retrieves the name of an intrinsic.
  -- *
  -- * @see llvm::Intrinsic::getName()
  --  

function Intrinsic_Get_Name
     (ID          : unsigned;
      Name_Length : access stddef_h.size_t)
      return String;

  --* Deprecated: Use LLVMIntrinsicCopyOverloadedName2 instead.  
function Intrinsic_Copy_Overloaded_Name
     (ID          : unsigned;
      Param_Types : System.Address;
      Param_Count : stddef_h.size_t;
      Name_Length : access stddef_h.size_t)
      return String;

  --*
  -- * Copies the name of an overloaded intrinsic identified by a given list of
  -- * parameter types.
  -- *
  -- * Unlike LLVMIntrinsicGetName, the caller is responsible for freeing the
  -- * returned string.
  -- *
  -- * This version also supports unnamed types.
  -- *
  -- * @see llvm::Intrinsic::getName()
  --  

function Intrinsic_Copy_Overloaded_Name_2
     (C_Mod       : LLVM.Types.Module_T;
      ID          : unsigned;
      Param_Types : System.Address;
      Param_Count : stddef_h.size_t;
      Name_Length : access stddef_h.size_t)
      return String;

  --*
  -- * Obtain if the intrinsic identified by the given ID is overloaded.
  -- *
  -- * @see llvm::Intrinsic::isOverloaded()
  --  

function Intrinsic_Is_Overloaded
     (ID : unsigned)
      return Boolean;

  --*
  -- * Obtain the calling function of a function.
  -- *
  -- * The returned value corresponds to the LLVMCallConv enumeration.
  -- *
  -- * @see llvm::Function::getCallingConv()
  --  

   function Get_Function_Call_Conv (Fn : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:2602
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetFunctionCallConv";

  --*
  -- * Set the calling convention of a function.
  -- *
  -- * @see llvm::Function::setCallingConv()
  -- *
  -- * @param Fn Function to operate on
  -- * @param CC LLVMCallConv to set calling convention to
  --  

   procedure Set_Function_Call_Conv (Fn : LLVM.Types.Value_T; CC : unsigned)  -- install/include/llvm-c/Core.h:2612
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetFunctionCallConv";

  --*
  -- * Obtain the name of the garbage collector to use during code
  -- * generation.
  -- *
  -- * @see llvm::Function::getGC()
  --  

function Get_GC
     (Fn : LLVM.Types.Value_T)
      return String;

  --*
  -- * Define the garbage collector to use during code generation.
  -- *
  -- * @see llvm::Function::setGC()
  --  

procedure Set_GC
     (Fn   : LLVM.Types.Value_T;
      Name : String);

  --*
  -- * Add an attribute to a function.
  -- *
  -- * @see llvm::Function::addAttribute()
  --  

   procedure Add_Attribute_At_Index
     (F : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      A : LLVM.Types.Attribute_T)  -- install/include/llvm-c/Core.h:2634
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddAttributeAtIndex";

   function Get_Attribute_Count_At_Index (F : LLVM.Types.Value_T; Idx : Attribute_Index_T) return unsigned  -- install/include/llvm-c/Core.h:2636
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetAttributeCountAtIndex";

   procedure Get_Attributes_At_Index
     (F : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Attrs : System.Address)  -- install/include/llvm-c/Core.h:2637
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetAttributesAtIndex";

   function Get_Enum_Attribute_At_Index
     (F : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Kind_ID : unsigned) return LLVM.Types.Attribute_T  -- install/include/llvm-c/Core.h:2639
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetEnumAttributeAtIndex";

function Get_String_Attribute_At_Index
     (F     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned)
      return LLVM.Types.Attribute_T;

   procedure Remove_Enum_Attribute_At_Index
     (F : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Kind_ID : unsigned)  -- install/include/llvm-c/Core.h:2645
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemoveEnumAttributeAtIndex";

procedure Remove_String_Attribute_At_Index
     (F     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned);

  --*
  -- * Add a target-dependent attribute to a function
  -- * @see llvm::AttrBuilder::addAttribute()
  --  

procedure Add_Target_Dependent_Function_Attr
     (Fn : LLVM.Types.Value_T;
      A  : String;
      V  : String);

  --*
  -- * @defgroup LLVMCCoreValueFunctionParameters Function Parameters
  -- *
  -- * Functions in this group relate to arguments/parameters on functions.
  -- *
  -- * Functions in this group expect LLVMValueRef instances that correspond
  -- * to llvm::Function instances.
  -- *
  -- * @{
  --  

  --*
  -- * Obtain the number of parameters in a function.
  -- *
  -- * @see llvm::Function::arg_size()
  --  

   function Count_Params (Fn : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:2673
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCountParams";

  --*
  -- * Obtain the parameters in a function.
  -- *
  -- * The takes a pointer to a pre-allocated array of LLVMValueRef that is
  -- * at least LLVMCountParams() long. This array will be filled with
  -- * LLVMValueRef instances which correspond to the parameters the
  -- * function receives. Each LLVMValueRef corresponds to a llvm::Argument
  -- * instance.
  -- *
  -- * @see llvm::Function::arg_begin()
  --  

   procedure Get_Params (Fn : LLVM.Types.Value_T; Params : System.Address)  -- install/include/llvm-c/Core.h:2686
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetParams";

  --*
  -- * Obtain the parameter at the specified index.
  -- *
  -- * Parameters are indexed from 0.
  -- *
  -- * @see llvm::Function::arg_begin()
  --  

   function Get_Param (Fn : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2695
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetParam";

  --*
  -- * Obtain the function to which this argument belongs.
  -- *
  -- * Unlike other functions in this group, this one takes an LLVMValueRef
  -- * that corresponds to a llvm::Attribute.
  -- *
  -- * The returned LLVMValueRef is the llvm::Function to which this
  -- * argument belongs.
  --  

   function Get_Param_Parent (Inst : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2706
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetParamParent";

  --*
  -- * Obtain the first parameter to a function.
  -- *
  -- * @see llvm::Function::arg_begin()
  --  

   function Get_First_Param (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2713
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetFirstParam";

  --*
  -- * Obtain the last parameter to a function.
  -- *
  -- * @see llvm::Function::arg_end()
  --  

   function Get_Last_Param (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2720
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetLastParam";

  --*
  -- * Obtain the next parameter to a function.
  -- *
  -- * This takes an LLVMValueRef obtained from LLVMGetFirstParam() (which is
  -- * actually a wrapped iterator) and obtains the next parameter from the
  -- * underlying iterator.
  --  

   function Get_Next_Param (Arg : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2729
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNextParam";

  --*
  -- * Obtain the previous parameter to a function.
  -- *
  -- * This is the opposite of LLVMGetNextParam().
  --  

   function Get_Previous_Param (Arg : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2736
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetPreviousParam";

  --*
  -- * Set the alignment for a function parameter.
  -- *
  -- * @see llvm::Argument::addAttr()
  -- * @see llvm::AttrBuilder::addAlignmentAttr()
  --  

   procedure Set_Param_Alignment (Arg : LLVM.Types.Value_T; Align : unsigned)  -- install/include/llvm-c/Core.h:2744
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetParamAlignment";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueGlobalIFunc IFuncs
  -- *
  -- * Functions in this group relate to indirect functions.
  -- *
  -- * Functions in this group expect LLVMValueRef instances that correspond
  -- * to llvm::GlobalIFunc instances.
  -- *
  -- * @{
  --  

  --*
  -- * Add a global indirect function to a module under a specified name.
  -- *
  -- * @see llvm::GlobalIFunc::create()
  --  

function Add_Global_I_Func
     (M          : LLVM.Types.Module_T;
      Name       : String;
      Name_Len   : stddef_h.size_t;
      Ty         : LLVM.Types.Type_T;
      Addr_Space : unsigned;
      Resolver   : LLVM.Types.Value_T)
      return LLVM.Types.Value_T;

  --*
  -- * Obtain a GlobalIFunc value from a Module by its name.
  -- *
  -- * The returned value corresponds to a llvm::GlobalIFunc value.
  -- *
  -- * @see llvm::Module::getNamedIFunc()
  --  

function Get_Named_Global_I_Func
     (M        : LLVM.Types.Module_T;
      Name     : String;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Value_T;

  --*
  -- * Obtain an iterator to the first GlobalIFunc in a Module.
  -- *
  -- * @see llvm::Module::ifunc_begin()
  --  

   function Get_First_Global_I_Func (M : LLVM.Types.Module_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2786
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetFirstGlobalIFunc";

  --*
  -- * Obtain an iterator to the last GlobalIFunc in a Module.
  -- *
  -- * @see llvm::Module::ifunc_end()
  --  

   function Get_Last_Global_I_Func (M : LLVM.Types.Module_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2793
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetLastGlobalIFunc";

  --*
  -- * Advance a GlobalIFunc iterator to the next GlobalIFunc.
  -- *
  -- * Returns NULL if the iterator was already at the end and there are no more
  -- * global aliases.
  --  

   function Get_Next_Global_I_Func (I_Func : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2801
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNextGlobalIFunc";

  --*
  -- * Decrement a GlobalIFunc iterator to the previous GlobalIFunc.
  -- *
  -- * Returns NULL if the iterator was already at the beginning and there are
  -- * no previous global aliases.
  --  

   function Get_Previous_Global_I_Func (I_Func : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2809
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetPreviousGlobalIFunc";

  --*
  -- * Retrieves the resolver function associated with this indirect function, or
  -- * NULL if it doesn't not exist.
  -- *
  -- * @see llvm::GlobalIFunc::getResolver()
  --  

   function Get_Global_I_Func_Resolver (I_Func : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2817
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetGlobalIFuncResolver";

  --*
  -- * Sets the resolver function associated with this indirect function.
  -- *
  -- * @see llvm::GlobalIFunc::setResolver()
  --  

   procedure Set_Global_I_Func_Resolver (I_Func : LLVM.Types.Value_T; Resolver : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:2824
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetGlobalIFuncResolver";

  --*
  -- * Remove a global indirect function from its parent module and delete it.
  -- *
  -- * @see llvm::GlobalIFunc::eraseFromParent()
  --  

   procedure Erase_Global_I_Func (I_Func : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:2831
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMEraseGlobalIFunc";

  --*
  -- * Remove a global indirect function from its parent module.
  -- *
  -- * This unlinks the global indirect function from its containing module but
  -- * keeps it alive.
  -- *
  -- * @see llvm::GlobalIFunc::removeFromParent()
  --  

   procedure Remove_Global_I_Func (I_Func : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:2841
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemoveGlobalIFunc";

  --*
  -- * @}
  --  

  --*
  -- * @}
  --  

  --*
  -- * @}
  --  

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueMetadata Metadata
  -- *
  -- * @{
  --  

  --*
  -- * Create an MDString value from a given string value.
  -- *
  -- * The MDString value does not take ownership of the given string, it remains
  -- * the responsibility of the caller to free it.
  -- *
  -- * @see llvm::MDString::get()
  --  

function MD_String_In_Context_2
     (C     : LLVM.Types.Context_T;
      Str   : String;
      S_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T;

  --*
  -- * Create an MDNode value with the given array of operands.
  -- *
  -- * @see llvm::MDNode::get()
  --  

   function MD_Node_In_Context_2
     (C : LLVM.Types.Context_T;
      M_Ds : System.Address;
      Count : stddef_h.size_t) return LLVM.Types.Metadata_T  -- install/include/llvm-c/Core.h:2881
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMMDNodeInContext2";

  --*
  -- * Obtain a Metadata as a Value.
  --  

   function Metadata_As_Value (C : LLVM.Types.Context_T; MD : LLVM.Types.Metadata_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2887
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMMetadataAsValue";

  --*
  -- * Obtain a Value as a Metadata.
  --  

   function Value_As_Metadata (Val : LLVM.Types.Value_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/Core.h:2892
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMValueAsMetadata";

  --*
  -- * Obtain the underlying string from a MDString value.
  -- *
  -- * @param V Instance to obtain string from.
  -- * @param Length Memory address which will hold length of returned string.
  -- * @return String data in MDString.
  --  

function Get_MD_String
     (V      : LLVM.Types.Value_T;
      Length : access unsigned)
      return String;

  --*
  -- * Obtain the number of operands from an MDNode value.
  -- *
  -- * @param V MDNode to get number of operands from.
  -- * @return Number of operands of the MDNode.
  --  

   function Get_MD_Node_Num_Operands (V : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:2909
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetMDNodeNumOperands";

  --*
  -- * Obtain the given MDNode's operands.
  -- *
  -- * The passed LLVMValueRef pointer should point to enough memory to hold all of
  -- * the operands of the given MDNode (see LLVMGetMDNodeNumOperands) as
  -- * LLVMValueRefs. This memory will be populated with the LLVMValueRefs of the
  -- * MDNode's operands.
  -- *
  -- * @param V MDNode to get the operands from.
  -- * @param Dest Destination array for operands.
  --  

   procedure Get_MD_Node_Operands (V : LLVM.Types.Value_T; Dest : System.Address)  -- install/include/llvm-c/Core.h:2922
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetMDNodeOperands";

  --* Deprecated: Use LLVMMDStringInContext2 instead.  
function MD_String_In_Context
     (C     : LLVM.Types.Context_T;
      Str   : String;
      S_Len : unsigned)
      return LLVM.Types.Value_T;

  --* Deprecated: Use LLVMMDStringInContext2 instead.  
function MD_String
     (Str   : String;
      S_Len : unsigned)
      return LLVM.Types.Value_T;

  --* Deprecated: Use LLVMMDNodeInContext2 instead.  
   function MD_Node_In_Context
     (C : LLVM.Types.Context_T;
      Vals : System.Address;
      Count : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2930
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMMDNodeInContext";

  --* Deprecated: Use LLVMMDNodeInContext2 instead.  
   function MD_Node (Vals : System.Address; Count : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2933
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMMDNode";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueBasicBlock Basic Block
  -- *
  -- * A basic block represents a single entry single exit section of code.
  -- * Basic blocks contain a list of instructions which form the body of
  -- * the block.
  -- *
  -- * Basic blocks belong to functions. They have the type of label.
  -- *
  -- * Basic blocks are themselves values. However, the C API models them as
  -- * LLVMBasicBlockRef.
  -- *
  -- * @see llvm::BasicBlock
  -- *
  -- * @{
  --  

  --*
  -- * Convert a basic block instance to a value type.
  --  

   function Basic_Block_As_Value (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2959
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBasicBlockAsValue";

  --*
  -- * Determine whether an LLVMValueRef is itself a basic block.
  --  

function Value_Is_Basic_Block
     (Val : LLVM.Types.Value_T)
      return Boolean;

  --*
  -- * Convert an LLVMValueRef to an LLVMBasicBlockRef instance.
  --  

   function Value_As_Basic_Block (Val : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T  -- install/include/llvm-c/Core.h:2969
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMValueAsBasicBlock";

  --*
  -- * Obtain the string name of a basic block.
  --  

function Get_Basic_Block_Name
     (BB : LLVM.Types.Basic_Block_T)
      return String;

  --*
  -- * Obtain the function to which a basic block belongs.
  -- *
  -- * @see llvm::BasicBlock::getParent()
  --  

   function Get_Basic_Block_Parent (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2981
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetBasicBlockParent";

  --*
  -- * Obtain the terminator instruction for a basic block.
  -- *
  -- * If the basic block does not have a terminator (it is not well-formed
  -- * if it doesn't), then NULL is returned.
  -- *
  -- * The returned LLVMValueRef corresponds to an llvm::Instruction.
  -- *
  -- * @see llvm::BasicBlock::getTerminator()
  --  

   function Get_Basic_Block_Terminator (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:2993
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetBasicBlockTerminator";

  --*
  -- * Obtain the number of basic blocks in a function.
  -- *
  -- * @param Fn Function value to operate on.
  --  

   function Count_Basic_Blocks (Fn : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:3000
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCountBasicBlocks";

  --*
  -- * Obtain all of the basic blocks in a function.
  -- *
  -- * This operates on a function value. The BasicBlocks parameter is a
  -- * pointer to a pre-allocated array of LLVMBasicBlockRef of at least
  -- * LLVMCountBasicBlocks() in length. This array is populated with
  -- * LLVMBasicBlockRef instances.
  --  

   procedure Get_Basic_Blocks (Fn : LLVM.Types.Value_T; Basic_Blocks : System.Address)  -- install/include/llvm-c/Core.h:3010
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetBasicBlocks";

  --*
  -- * Obtain the first basic block in a function.
  -- *
  -- * The returned basic block can be used as an iterator. You will likely
  -- * eventually call into LLVMGetNextBasicBlock() with it.
  -- *
  -- * @see llvm::Function::begin()
  --  

   function Get_First_Basic_Block (Fn : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T  -- install/include/llvm-c/Core.h:3020
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetFirstBasicBlock";

  --*
  -- * Obtain the last basic block in a function.
  -- *
  -- * @see llvm::Function::end()
  --  

   function Get_Last_Basic_Block (Fn : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T  -- install/include/llvm-c/Core.h:3027
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetLastBasicBlock";

  --*
  -- * Advance a basic block iterator.
  --  

   function Get_Next_Basic_Block (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Basic_Block_T  -- install/include/llvm-c/Core.h:3032
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNextBasicBlock";

  --*
  -- * Go backwards in a basic block iterator.
  --  

   function Get_Previous_Basic_Block (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Basic_Block_T  -- install/include/llvm-c/Core.h:3037
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetPreviousBasicBlock";

  --*
  -- * Obtain the basic block that corresponds to the entry point of a
  -- * function.
  -- *
  -- * @see llvm::Function::getEntryBlock()
  --  

   function Get_Entry_Basic_Block (Fn : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T  -- install/include/llvm-c/Core.h:3045
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetEntryBasicBlock";

  --*
  -- * Insert the given basic block after the insertion point of the given builder.
  -- *
  -- * The insertion point must be valid.
  -- *
  -- * @see llvm::Function::BasicBlockListType::insertAfter()
  --  

   procedure Insert_Existing_Basic_Block_After_Insert_Block (Builder : LLVM.Types.Builder_T; BB : LLVM.Types.Basic_Block_T)  -- install/include/llvm-c/Core.h:3054
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInsertExistingBasicBlockAfterInsertBlock";

  --*
  -- * Append the given basic block to the basic block list of the given function.
  -- *
  -- * @see llvm::Function::BasicBlockListType::push_back()
  --  

   procedure Append_Existing_Basic_Block (Fn : LLVM.Types.Value_T; BB : LLVM.Types.Basic_Block_T)  -- install/include/llvm-c/Core.h:3062
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAppendExistingBasicBlock";

  --*
  -- * Create a new basic block without inserting it into a function.
  -- *
  -- * @see llvm::BasicBlock::Create()
  --  

function Create_Basic_Block_In_Context
     (C    : LLVM.Types.Context_T;
      Name : String)
      return LLVM.Types.Basic_Block_T;

  --*
  -- * Append a basic block to the end of a function.
  -- *
  -- * @see llvm::BasicBlock::Create()
  --  

function Append_Basic_Block_In_Context
     (C    : LLVM.Types.Context_T;
      Fn   : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Basic_Block_T;

  --*
  -- * Append a basic block to the end of a function using the global
  -- * context.
  -- *
  -- * @see llvm::BasicBlock::Create()
  --  

function Append_Basic_Block
     (Fn   : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Basic_Block_T;

  --*
  -- * Insert a basic block in a function before another basic block.
  -- *
  -- * The function to add to is determined by the function of the
  -- * passed basic block.
  -- *
  -- * @see llvm::BasicBlock::Create()
  --  

function Insert_Basic_Block_In_Context
     (C    : LLVM.Types.Context_T;
      BB   : LLVM.Types.Basic_Block_T;
      Name : String)
      return LLVM.Types.Basic_Block_T;

  --*
  -- * Insert a basic block in a function using the global context.
  -- *
  -- * @see llvm::BasicBlock::Create()
  --  

function Insert_Basic_Block
     (Insert_Before_BB : LLVM.Types.Basic_Block_T;
      Name             : String)
      return LLVM.Types.Basic_Block_T;

  --*
  -- * Remove a basic block from a function and delete it.
  -- *
  -- * This deletes the basic block from its containing function and deletes
  -- * the basic block itself.
  -- *
  -- * @see llvm::BasicBlock::eraseFromParent()
  --  

   procedure Delete_Basic_Block (BB : LLVM.Types.Basic_Block_T)  -- install/include/llvm-c/Core.h:3118
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDeleteBasicBlock";

  --*
  -- * Remove a basic block from a function.
  -- *
  -- * This deletes the basic block from its containing function but keep
  -- * the basic block alive.
  -- *
  -- * @see llvm::BasicBlock::removeFromParent()
  --  

   procedure Remove_Basic_Block_From_Parent (BB : LLVM.Types.Basic_Block_T)  -- install/include/llvm-c/Core.h:3128
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemoveBasicBlockFromParent";

  --*
  -- * Move a basic block to before another one.
  -- *
  -- * @see llvm::BasicBlock::moveBefore()
  --  

   procedure Move_Basic_Block_Before (BB : LLVM.Types.Basic_Block_T; Move_Pos : LLVM.Types.Basic_Block_T)  -- install/include/llvm-c/Core.h:3135
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMMoveBasicBlockBefore";

  --*
  -- * Move a basic block to after another one.
  -- *
  -- * @see llvm::BasicBlock::moveAfter()
  --  

   procedure Move_Basic_Block_After (BB : LLVM.Types.Basic_Block_T; Move_Pos : LLVM.Types.Basic_Block_T)  -- install/include/llvm-c/Core.h:3142
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMMoveBasicBlockAfter";

  --*
  -- * Obtain the first instruction in a basic block.
  -- *
  -- * The returned LLVMValueRef corresponds to a llvm::Instruction
  -- * instance.
  --  

   function Get_First_Instruction (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3150
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetFirstInstruction";

  --*
  -- * Obtain the last instruction in a basic block.
  -- *
  -- * The returned LLVMValueRef corresponds to an LLVM:Instruction.
  --  

   function Get_Last_Instruction (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3157
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetLastInstruction";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueInstruction Instructions
  -- *
  -- * Functions in this group relate to the inspection and manipulation of
  -- * individual instructions.
  -- *
  -- * In the C++ API, an instruction is modeled by llvm::Instruction. This
  -- * class has a large number of descendents. llvm::Instruction is a
  -- * llvm::Value and in the C API, instructions are modeled by
  -- * LLVMValueRef.
  -- *
  -- * This group also contains sub-groups which operate on specific
  -- * llvm::Instruction types, e.g. llvm::CallInst.
  -- *
  -- * @{
  --  

  --*
  -- * Determine whether an instruction has any metadata attached.
  --  

   function Has_Metadata (Val : LLVM.Types.Value_T) return int  -- install/include/llvm-c/Core.h:3183
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMHasMetadata";

  --*
  -- * Return metadata associated with an instruction value.
  --  

   function Get_Metadata (Val : LLVM.Types.Value_T; Kind_ID : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3188
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetMetadata";

  --*
  -- * Set metadata associated with an instruction value.
  --  

   procedure Set_Metadata
     (Val : LLVM.Types.Value_T;
      Kind_ID : unsigned;
      Node : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:3193
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetMetadata";

  --*
  -- * Returns the metadata associated with an instruction value, but filters out
  -- * all the debug locations.
  -- *
  -- * @see llvm::Instruction::getAllMetadataOtherThanDebugLoc()
  --  

   function Instruction_Get_All_Metadata_Other_Than_Debug_Loc (Instr : LLVM.Types.Value_T; Num_Entries : access stddef_h.size_t) return access LLVM.Types.Opaque_Value_Metadata_Entry_Impl_T  -- install/include/llvm-c/Core.h:3202
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInstructionGetAllMetadataOtherThanDebugLoc";

  --*
  -- * Obtain the basic block to which an instruction belongs.
  -- *
  -- * @see llvm::Instruction::getParent()
  --  

   function Get_Instruction_Parent (Inst : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T  -- install/include/llvm-c/Core.h:3210
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetInstructionParent";

  --*
  -- * Obtain the instruction that occurs after the one specified.
  -- *
  -- * The next instruction will be from the same basic block.
  -- *
  -- * If this is the last instruction in a basic block, NULL will be
  -- * returned.
  --  

   function Get_Next_Instruction (Inst : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3220
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNextInstruction";

  --*
  -- * Obtain the instruction that occurred before this one.
  -- *
  -- * If the instruction is the first instruction in a basic block, NULL
  -- * will be returned.
  --  

   function Get_Previous_Instruction (Inst : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3228
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetPreviousInstruction";

  --*
  -- * Remove an instruction.
  -- *
  -- * The instruction specified is removed from its containing building
  -- * block but is kept alive.
  -- *
  -- * @see llvm::Instruction::removeFromParent()
  --  

   procedure Instruction_Remove_From_Parent (Inst : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:3238
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInstructionRemoveFromParent";

  --*
  -- * Remove and delete an instruction.
  -- *
  -- * The instruction specified is removed from its containing building
  -- * block and then deleted.
  -- *
  -- * @see llvm::Instruction::eraseFromParent()
  --  

   procedure Instruction_Erase_From_Parent (Inst : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:3248
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInstructionEraseFromParent";

  --*
  -- * Delete an instruction.
  -- *
  -- * The instruction specified is deleted. It must have previously been
  -- * removed from its containing building block.
  -- *
  -- * @see llvm::Value::deleteValue()
  --  

   procedure Delete_Instruction (Inst : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:3258
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDeleteInstruction";

  --*
  -- * Obtain the code opcode for an individual instruction.
  -- *
  -- * @see llvm::Instruction::getOpCode()
  --  

   function Get_Instruction_Opcode (Inst : LLVM.Types.Value_T) return Opcode_T  -- install/include/llvm-c/Core.h:3265
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetInstructionOpcode";

  --*
  -- * Obtain the predicate of an instruction.
  -- *
  -- * This is only valid for instructions that correspond to llvm::ICmpInst
  -- * or llvm::ConstantExpr whose opcode is llvm::Instruction::ICmp.
  -- *
  -- * @see llvm::ICmpInst::getPredicate()
  --  

   function Get_I_Cmp_Predicate (Inst : LLVM.Types.Value_T) return Int_Predicate_T  -- install/include/llvm-c/Core.h:3275
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetICmpPredicate";

  --*
  -- * Obtain the float predicate of an instruction.
  -- *
  -- * This is only valid for instructions that correspond to llvm::FCmpInst
  -- * or llvm::ConstantExpr whose opcode is llvm::Instruction::FCmp.
  -- *
  -- * @see llvm::FCmpInst::getPredicate()
  --  

   function Get_F_Cmp_Predicate (Inst : LLVM.Types.Value_T) return Real_Predicate_T  -- install/include/llvm-c/Core.h:3285
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetFCmpPredicate";

  --*
  -- * Create a copy of 'this' instruction that is identical in all ways
  -- * except the following:
  -- *   * The instruction has no parent
  -- *   * The instruction has no name
  -- *
  -- * @see llvm::Instruction::clone()
  --  

   function Instruction_Clone (Inst : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3295
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInstructionClone";

  --*
  -- * Determine whether an instruction is a terminator. This routine is named to
  -- * be compatible with historical functions that did this by querying the
  -- * underlying C++ type.
  -- *
  -- * @see llvm::Instruction::isTerminator()
  --  

   function Is_A_Terminator_Inst (Inst : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3304
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIsATerminatorInst";

  --*
  -- * @defgroup LLVMCCoreValueInstructionCall Call Sites and Invocations
  -- *
  -- * Functions in this group apply to instructions that refer to call
  -- * sites and invocations. These correspond to C++ types in the
  -- * llvm::CallInst class tree.
  -- *
  -- * @{
  --  

  --*
  -- * Obtain the argument count for a call instruction.
  -- *
  -- * This expects an LLVMValueRef that corresponds to a llvm::CallInst,
  -- * llvm::InvokeInst, or llvm:FuncletPadInst.
  -- *
  -- * @see llvm::CallInst::getNumArgOperands()
  -- * @see llvm::InvokeInst::getNumArgOperands()
  -- * @see llvm::FuncletPadInst::getNumArgOperands()
  --  

   function Get_Num_Arg_Operands (Instr : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:3326
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNumArgOperands";

  --*
  -- * Set the calling convention for a call instruction.
  -- *
  -- * This expects an LLVMValueRef that corresponds to a llvm::CallInst or
  -- * llvm::InvokeInst.
  -- *
  -- * @see llvm::CallInst::setCallingConv()
  -- * @see llvm::InvokeInst::setCallingConv()
  --  

   procedure Set_Instruction_Call_Conv (Instr : LLVM.Types.Value_T; CC : unsigned)  -- install/include/llvm-c/Core.h:3337
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetInstructionCallConv";

  --*
  -- * Obtain the calling convention for a call instruction.
  -- *
  -- * This is the opposite of LLVMSetInstructionCallConv(). Reads its
  -- * usage.
  -- *
  -- * @see LLVMSetInstructionCallConv()
  --  

   function Get_Instruction_Call_Conv (Instr : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:3347
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetInstructionCallConv";

   procedure Set_Instr_Param_Alignment
     (Instr : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Align : unsigned)  -- install/include/llvm-c/Core.h:3349
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetInstrParamAlignment";

   procedure Add_Call_Site_Attribute
     (C : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      A : LLVM.Types.Attribute_T)  -- install/include/llvm-c/Core.h:3352
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddCallSiteAttribute";

   function Get_Call_Site_Attribute_Count (C : LLVM.Types.Value_T; Idx : Attribute_Index_T) return unsigned  -- install/include/llvm-c/Core.h:3354
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetCallSiteAttributeCount";

   procedure Get_Call_Site_Attributes
     (C : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Attrs : System.Address)  -- install/include/llvm-c/Core.h:3355
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetCallSiteAttributes";

   function Get_Call_Site_Enum_Attribute
     (C : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Kind_ID : unsigned) return LLVM.Types.Attribute_T  -- install/include/llvm-c/Core.h:3357
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetCallSiteEnumAttribute";

function Get_Call_Site_String_Attribute
     (C     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned)
      return LLVM.Types.Attribute_T;

   procedure Remove_Call_Site_Enum_Attribute
     (C : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Kind_ID : unsigned)  -- install/include/llvm-c/Core.h:3363
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRemoveCallSiteEnumAttribute";

procedure Remove_Call_Site_String_Attribute
     (C     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned);

  --*
  -- * Obtain the function type called by this instruction.
  -- *
  -- * @see llvm::CallBase::getFunctionType()
  --  

   function Get_Called_Function_Type (C : LLVM.Types.Value_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:3373
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetCalledFunctionType";

  --*
  -- * Obtain the pointer to the function invoked by this instruction.
  -- *
  -- * This expects an LLVMValueRef that corresponds to a llvm::CallInst or
  -- * llvm::InvokeInst.
  -- *
  -- * @see llvm::CallInst::getCalledOperand()
  -- * @see llvm::InvokeInst::getCalledOperand()
  --  

   function Get_Called_Value (Instr : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3384
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetCalledValue";

  --*
  -- * Obtain whether a call instruction is a tail call.
  -- *
  -- * This only works on llvm::CallInst instructions.
  -- *
  -- * @see llvm::CallInst::isTailCall()
  --  

function Is_Tail_Call
     (Call_Inst : LLVM.Types.Value_T)
      return Boolean;

  --*
  -- * Set whether a call instruction is a tail call.
  -- *
  -- * This only works on llvm::CallInst instructions.
  -- *
  -- * @see llvm::CallInst::setTailCall()
  --  

procedure Set_Tail_Call
     (Call_Inst    : LLVM.Types.Value_T;
      Is_Tail_Call : Boolean);

  --*
  -- * Return the normal destination basic block.
  -- *
  -- * This only works on llvm::InvokeInst instructions.
  -- *
  -- * @see llvm::InvokeInst::getNormalDest()
  --  

   function Get_Normal_Dest (Invoke_Inst : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T  -- install/include/llvm-c/Core.h:3411
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNormalDest";

  --*
  -- * Return the unwind destination basic block.
  -- *
  -- * Works on llvm::InvokeInst, llvm::CleanupReturnInst, and
  -- * llvm::CatchSwitchInst instructions.
  -- *
  -- * @see llvm::InvokeInst::getUnwindDest()
  -- * @see llvm::CleanupReturnInst::getUnwindDest()
  -- * @see llvm::CatchSwitchInst::getUnwindDest()
  --  

   function Get_Unwind_Dest (Invoke_Inst : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T  -- install/include/llvm-c/Core.h:3423
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetUnwindDest";

  --*
  -- * Set the normal destination basic block.
  -- *
  -- * This only works on llvm::InvokeInst instructions.
  -- *
  -- * @see llvm::InvokeInst::setNormalDest()
  --  

   procedure Set_Normal_Dest (Invoke_Inst : LLVM.Types.Value_T; B : LLVM.Types.Basic_Block_T)  -- install/include/llvm-c/Core.h:3432
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetNormalDest";

  --*
  -- * Set the unwind destination basic block.
  -- *
  -- * Works on llvm::InvokeInst, llvm::CleanupReturnInst, and
  -- * llvm::CatchSwitchInst instructions.
  -- *
  -- * @see llvm::InvokeInst::setUnwindDest()
  -- * @see llvm::CleanupReturnInst::setUnwindDest()
  -- * @see llvm::CatchSwitchInst::setUnwindDest()
  --  

   procedure Set_Unwind_Dest (Invoke_Inst : LLVM.Types.Value_T; B : LLVM.Types.Basic_Block_T)  -- install/include/llvm-c/Core.h:3444
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetUnwindDest";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueInstructionTerminator Terminators
  -- *
  -- * Functions in this group only apply to instructions for which
  -- * LLVMIsATerminatorInst returns true.
  -- *
  -- * @{
  --  

  --*
  -- * Return the number of successors that this terminator has.
  -- *
  -- * @see llvm::Instruction::getNumSuccessors
  --  

   function Get_Num_Successors (Term : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:3464
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNumSuccessors";

  --*
  -- * Return the specified successor.
  -- *
  -- * @see llvm::Instruction::getSuccessor
  --  

   function Get_Successor (Term : LLVM.Types.Value_T; I : unsigned) return LLVM.Types.Basic_Block_T  -- install/include/llvm-c/Core.h:3471
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetSuccessor";

  --*
  -- * Update the specified successor to point at the provided block.
  -- *
  -- * @see llvm::Instruction::setSuccessor
  --  

   procedure Set_Successor
     (Term : LLVM.Types.Value_T;
      I : unsigned;
      Block : LLVM.Types.Basic_Block_T)  -- install/include/llvm-c/Core.h:3478
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetSuccessor";

  --*
  -- * Return if a branch is conditional.
  -- *
  -- * This only works on llvm::BranchInst instructions.
  -- *
  -- * @see llvm::BranchInst::isConditional
  --  

function Is_Conditional
     (Branch : LLVM.Types.Value_T)
      return Boolean;

  --*
  -- * Return the condition of a branch instruction.
  -- *
  -- * This only works on llvm::BranchInst instructions.
  -- *
  -- * @see llvm::BranchInst::getCondition
  --  

   function Get_Condition (Branch : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3496
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetCondition";

  --*
  -- * Set the condition of a branch instruction.
  -- *
  -- * This only works on llvm::BranchInst instructions.
  -- *
  -- * @see llvm::BranchInst::setCondition
  --  

   procedure Set_Condition (Branch : LLVM.Types.Value_T; Cond : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:3505
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetCondition";

  --*
  -- * Obtain the default destination basic block of a switch instruction.
  -- *
  -- * This only works on llvm::SwitchInst instructions.
  -- *
  -- * @see llvm::SwitchInst::getDefaultDest()
  --  

   function Get_Switch_Default_Dest (Switch_Instr : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T  -- install/include/llvm-c/Core.h:3514
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetSwitchDefaultDest";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueInstructionAlloca Allocas
  -- *
  -- * Functions in this group only apply to instructions that map to
  -- * llvm::AllocaInst instances.
  -- *
  -- * @{
  --  

  --*
  -- * Obtain the type that is being allocated by the alloca instruction.
  --  

   function Get_Allocated_Type (Alloca : LLVM.Types.Value_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:3532
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetAllocatedType";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueInstructionGetElementPointer GEPs
  -- *
  -- * Functions in this group only apply to instructions that map to
  -- * llvm::GetElementPtrInst instances.
  -- *
  -- * @{
  --  

  --*
  -- * Check whether the given GEP operator is inbounds.
  --  

function Is_In_Bounds
     (GEP : LLVM.Types.Value_T)
      return Boolean;

  --*
  -- * Set the given GEP instruction to be inbounds or not.
  --  

procedure Set_Is_In_Bounds
     (GEP       : LLVM.Types.Value_T;
      In_Bounds : Boolean);

  --*
  -- * Get the source element type of the given GEP operator.
  --  

   function Get_GEP_Source_Element_Type (GEP : LLVM.Types.Value_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Core.h:3560
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetGEPSourceElementType";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueInstructionPHINode PHI Nodes
  -- *
  -- * Functions in this group only apply to instructions that map to
  -- * llvm::PHINode instances.
  -- *
  -- * @{
  --  

  --*
  -- * Add an incoming value to the end of a PHI list.
  --  

   procedure Add_Incoming
     (Phi_Node : LLVM.Types.Value_T;
      Incoming_Values : System.Address;
      Incoming_Blocks : System.Address;
      Count : unsigned)  -- install/include/llvm-c/Core.h:3578
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddIncoming";

  --*
  -- * Obtain the number of incoming basic blocks to a PHI node.
  --  

   function Count_Incoming (Phi_Node : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:3584
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCountIncoming";

  --*
  -- * Obtain an incoming value to a PHI node as an LLVMValueRef.
  --  

   function Get_Incoming_Value (Phi_Node : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3589
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetIncomingValue";

  --*
  -- * Obtain an incoming value to a PHI node as an LLVMBasicBlockRef.
  --  

   function Get_Incoming_Block (Phi_Node : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Basic_Block_T  -- install/include/llvm-c/Core.h:3594
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetIncomingBlock";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreValueInstructionExtractValue ExtractValue
  -- * @defgroup LLVMCCoreValueInstructionInsertValue InsertValue
  -- *
  -- * Functions in this group only apply to instructions that map to
  -- * llvm::ExtractValue and llvm::InsertValue instances.
  -- *
  -- * @{
  --  

  --*
  -- * Obtain the number of indices.
  -- * NB: This also works on GEP operators.
  --  

   function Get_Num_Indices (Inst : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:3614
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNumIndices";

  --*
  -- * Obtain the indices as an array.
  --  

   function Get_Indices (Inst : LLVM.Types.Value_T) return access unsigned  -- install/include/llvm-c/Core.h:3619
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetIndices";

  --*
  -- * @}
  --  

  --*
  -- * @}
  --  

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreInstructionBuilder Instruction Builders
  -- *
  -- * An instruction builder represents a point within a basic block and is
  -- * the exclusive means of building instructions using the C interface.
  -- *
  -- * @{
  --  

   function Create_Builder_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Builder_T  -- install/include/llvm-c/Core.h:3642
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateBuilderInContext";

   function Create_Builder return LLVM.Types.Builder_T  -- install/include/llvm-c/Core.h:3643
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateBuilder";

   procedure Position_Builder
     (Builder : LLVM.Types.Builder_T;
      Block : LLVM.Types.Basic_Block_T;
      Instr : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:3644
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPositionBuilder";

   procedure Position_Builder_Before (Builder : LLVM.Types.Builder_T; Instr : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:3646
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPositionBuilderBefore";

   procedure Position_Builder_At_End (Builder : LLVM.Types.Builder_T; Block : LLVM.Types.Basic_Block_T)  -- install/include/llvm-c/Core.h:3647
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPositionBuilderAtEnd";

   function Get_Insert_Block (Builder : LLVM.Types.Builder_T) return LLVM.Types.Basic_Block_T  -- install/include/llvm-c/Core.h:3648
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetInsertBlock";

   procedure Clear_Insertion_Position (Builder : LLVM.Types.Builder_T)  -- install/include/llvm-c/Core.h:3649
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMClearInsertionPosition";

   procedure Insert_Into_Builder (Builder : LLVM.Types.Builder_T; Instr : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:3650
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInsertIntoBuilder";

procedure Insert_Into_With_Name
     (Builder : LLVM.Types.Builder_T;
      Instr   : LLVM.Types.Value_T;
      Name    : String);

   procedure Dispose_Builder (Builder : LLVM.Types.Builder_T)  -- install/include/llvm-c/Core.h:3653
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeBuilder";

  -- Metadata  
  --*
  -- * Get location information used by debugging information.
  -- *
  -- * @see llvm::IRBuilder::getCurrentDebugLocation()
  --  

   function Get_Current_Debug_Location_2 (Builder : LLVM.Types.Builder_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/Core.h:3662
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetCurrentDebugLocation2";

  --*
  -- * Set location information used by debugging information.
  -- *
  -- * To clear the location metadata of the given instruction, pass NULL to \p Loc.
  -- *
  -- * @see llvm::IRBuilder::SetCurrentDebugLocation()
  --  

   procedure Set_Current_Debug_Location_2 (Builder : LLVM.Types.Builder_T; Loc : LLVM.Types.Metadata_T)  -- install/include/llvm-c/Core.h:3671
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetCurrentDebugLocation2";

  --*
  -- * Attempts to set the debug location for the given instruction using the
  -- * current debug location for the given builder.  If the builder has no current
  -- * debug location, this function is a no-op.
  -- *
  -- * @deprecated LLVMSetInstDebugLocation is deprecated in favor of the more general
  -- *             LLVMAddMetadataToInst.
  -- *
  -- * @see llvm::IRBuilder::SetInstDebugLocation()
  --  

   procedure Set_Inst_Debug_Location (Builder : LLVM.Types.Builder_T; Inst : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:3683
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetInstDebugLocation";

  --*
  -- * Adds the metadata registered with the given builder to the given instruction.
  -- *
  -- * @see llvm::IRBuilder::AddMetadataToInst()
  --  

   procedure Add_Metadata_To_Inst (Builder : LLVM.Types.Builder_T; Inst : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:3690
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddMetadataToInst";

  --*
  -- * Get the dafult floating-point math metadata for a given builder.
  -- *
  -- * @see llvm::IRBuilder::getDefaultFPMathTag()
  --  

   function Builder_Get_Default_FP_Math_Tag (Builder : LLVM.Types.Builder_T) return LLVM.Types.Metadata_T  -- install/include/llvm-c/Core.h:3697
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuilderGetDefaultFPMathTag";

  --*
  -- * Set the default floating-point math metadata for the given builder.
  -- *
  -- * To clear the metadata, pass NULL to \p FPMathTag.
  -- *
  -- * @see llvm::IRBuilder::setDefaultFPMathTag()
  --  

   procedure Builder_Set_Default_FP_Math_Tag (Builder : LLVM.Types.Builder_T; FP_Math_Tag : LLVM.Types.Metadata_T)  -- install/include/llvm-c/Core.h:3706
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuilderSetDefaultFPMathTag";

  --*
  -- * Deprecated: Passing the NULL location will crash.
  -- * Use LLVMGetCurrentDebugLocation2 instead.
  --  

   procedure Set_Current_Debug_Location (Builder : LLVM.Types.Builder_T; L : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:3713
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetCurrentDebugLocation";

  --*
  -- * Deprecated: Returning the NULL location will crash.
  -- * Use LLVMGetCurrentDebugLocation2 instead.
  --  

   function Get_Current_Debug_Location (Builder : LLVM.Types.Builder_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3718
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetCurrentDebugLocation";

  -- Terminators  
   function Build_Ret_Void (Arg_1 : LLVM.Types.Builder_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3721
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildRetVoid";

   function Build_Ret (Arg_1 : LLVM.Types.Builder_T; V : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3722
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildRet";

   function Build_Aggregate_Ret
     (Arg_1 : LLVM.Types.Builder_T;
      Ret_Vals : System.Address;
      N : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3723
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildAggregateRet";

   function Build_Br (Arg_1 : LLVM.Types.Builder_T; Dest : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3725
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildBr";

   function Build_Cond_Br
     (Arg_1 : LLVM.Types.Builder_T;
      C_If : LLVM.Types.Value_T;
      C_Then : LLVM.Types.Basic_Block_T;
      C_Else : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3726
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildCondBr";

   function Build_Switch
     (Arg_1 : LLVM.Types.Builder_T;
      V : LLVM.Types.Value_T;
      C_Else : LLVM.Types.Basic_Block_T;
      Num_Cases : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3728
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildSwitch";

   function Build_Indirect_Br
     (B : LLVM.Types.Builder_T;
      Addr : LLVM.Types.Value_T;
      Num_Dests : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3730
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildIndirectBr";

function Invoke_2
     (Arg_1    : LLVM.Types.Builder_T;
      Ty       : LLVM.Types.Type_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      C_Then   : LLVM.Types.Basic_Block_T;
      Catch    : LLVM.Types.Basic_Block_T;
      Name     : String)
      return LLVM.Types.Value_T;

   function Build_Unreachable (Arg_1 : LLVM.Types.Builder_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3736
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildUnreachable";

  -- Exception Handling  
   function Build_Resume (B : LLVM.Types.Builder_T; Exn : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3739
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildResume";

function Landing_Pad
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pers_Fn     : LLVM.Types.Value_T;
      Num_Clauses : unsigned;
      Name        : String)
      return LLVM.Types.Value_T;

   function Build_Cleanup_Ret
     (B : LLVM.Types.Builder_T;
      Catch_Pad : LLVM.Types.Value_T;
      BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3743
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildCleanupRet";

   function Build_Catch_Ret
     (B : LLVM.Types.Builder_T;
      Catch_Pad : LLVM.Types.Value_T;
      BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3745
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildCatchRet";

function Catch_Pad
     (B          : LLVM.Types.Builder_T;
      Parent_Pad : LLVM.Types.Value_T;
      Args       : System.Address;
      Num_Args   : unsigned;
      Name       : String)
      return LLVM.Types.Value_T;

function Cleanup_Pad
     (B          : LLVM.Types.Builder_T;
      Parent_Pad : LLVM.Types.Value_T;
      Args       : System.Address;
      Num_Args   : unsigned;
      Name       : String)
      return LLVM.Types.Value_T;

function Catch_Switch
     (B            : LLVM.Types.Builder_T;
      Parent_Pad   : LLVM.Types.Value_T;
      Unwind_BB    : LLVM.Types.Basic_Block_T;
      Num_Handlers : unsigned;
      Name         : String)
      return LLVM.Types.Value_T;

  -- Add a case to the switch instruction  
   procedure Add_Case
     (Switch : LLVM.Types.Value_T;
      On_Val : LLVM.Types.Value_T;
      Dest : LLVM.Types.Basic_Block_T)  -- install/include/llvm-c/Core.h:3758
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddCase";

  -- Add a destination to the indirectbr instruction  
   procedure Add_Destination (Indirect_Br : LLVM.Types.Value_T; Dest : LLVM.Types.Basic_Block_T)  -- install/include/llvm-c/Core.h:3762
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddDestination";

  -- Get the number of clauses on the landingpad instruction  
   function Get_Num_Clauses (Landing_Pad : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:3765
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNumClauses";

  -- Get the value of the clause at index Idx on the landingpad instruction  
   function Get_Clause (Landing_Pad : LLVM.Types.Value_T; Idx : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3768
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetClause";

  -- Add a catch or filter clause to the landingpad instruction  
   procedure Add_Clause (Landing_Pad : LLVM.Types.Value_T; Clause_Val : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:3771
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddClause";

  -- Get the 'cleanup' flag in the landingpad instruction  
function Is_Cleanup
     (Landing_Pad : LLVM.Types.Value_T)
      return Boolean;

  -- Set the 'cleanup' flag in the landingpad instruction  
procedure Set_Cleanup
     (Landing_Pad : LLVM.Types.Value_T;
      Val         : Boolean);

  -- Add a destination to the catchswitch instruction  
   procedure Add_Handler (Catch_Switch : LLVM.Types.Value_T; Dest : LLVM.Types.Basic_Block_T)  -- install/include/llvm-c/Core.h:3780
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddHandler";

  -- Get the number of handlers on the catchswitch instruction  
   function Get_Num_Handlers (Catch_Switch : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:3783
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNumHandlers";

  --*
  -- * Obtain the basic blocks acting as handlers for a catchswitch instruction.
  -- *
  -- * The Handlers parameter should point to a pre-allocated array of
  -- * LLVMBasicBlockRefs at least LLVMGetNumHandlers() large. On return, the
  -- * first LLVMGetNumHandlers() entries in the array will be populated
  -- * with LLVMBasicBlockRef instances.
  -- *
  -- * @param CatchSwitch The catchswitch instruction to operate on.
  -- * @param Handlers Memory address of an array to be filled with basic blocks.
  --  

   procedure Get_Handlers (Catch_Switch : LLVM.Types.Value_T; Handlers : System.Address)  -- install/include/llvm-c/Core.h:3796
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetHandlers";

  -- Funclets  
  -- Get the number of funcletpad arguments.  
   function Get_Arg_Operand (Funclet : LLVM.Types.Value_T; I : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3801
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetArgOperand";

  -- Set a funcletpad argument at the given index.  
   procedure Set_Arg_Operand
     (Funclet : LLVM.Types.Value_T;
      I : unsigned;
      Value : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:3804
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetArgOperand";

  --*
  -- * Get the parent catchswitch instruction of a catchpad instruction.
  -- *
  -- * This only works on llvm::CatchPadInst instructions.
  -- *
  -- * @see llvm::CatchPadInst::getCatchSwitch()
  --  

   function Get_Parent_Catch_Switch (Catch_Pad : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3813
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetParentCatchSwitch";

  --*
  -- * Set the parent catchswitch instruction of a catchpad instruction.
  -- *
  -- * This only works on llvm::CatchPadInst instructions.
  -- *
  -- * @see llvm::CatchPadInst::setCatchSwitch()
  --  

   procedure Set_Parent_Catch_Switch (Catch_Pad : LLVM.Types.Value_T; Catch_Switch : LLVM.Types.Value_T)  -- install/include/llvm-c/Core.h:3822
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetParentCatchSwitch";

  -- Arithmetic  
function Add
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function NSW_Add
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function NUW_Add
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function F_Add
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Sub
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function NSW_Sub
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function NUW_Sub
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function F_Sub
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Mul
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function NSW_Mul
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function NUW_Mul
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function F_Mul
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function U_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Exact_U_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function S_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Exact_S_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function F_Div
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function U_Rem
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function S_Rem
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function F_Rem
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Shl
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function L_Shr
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function A_Shr
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Build_And
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Build_Or
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Build_Xor
     (Arg_1 : LLVM.Types.Builder_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Bin_Op
     (B    : LLVM.Types.Builder_T;
      Op   : Opcode_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;

function Neg
     (Arg_1 : LLVM.Types.Builder_T;
      V     : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function NSW_Neg
     (B    : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;

function NUW_Neg
     (B    : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;

function F_Neg
     (Arg_1 : LLVM.Types.Builder_T;
      V     : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Build_Not
     (Arg_1 : LLVM.Types.Builder_T;
      V     : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

  -- Memory  
function Malloc
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Array_Malloc
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Val   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

  --*
  -- * Creates and inserts a memset to the specified pointer and the
  -- * specified value.
  -- *
  -- * @see llvm::IRRBuilder::CreateMemSet()
  --  

   function Build_Mem_Set
     (B : LLVM.Types.Builder_T;
      Ptr : LLVM.Types.Value_T;
      Val : LLVM.Types.Value_T;
      Len : LLVM.Types.Value_T;
      Align : unsigned) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3899
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildMemSet";

  --*
  -- * Creates and inserts a memcpy between the specified pointers.
  -- *
  -- * @see llvm::IRRBuilder::CreateMemCpy()
  --  

   function Build_Mem_Cpy
     (B : LLVM.Types.Builder_T;
      Dst : LLVM.Types.Value_T;
      Dst_Align : unsigned;
      Src : LLVM.Types.Value_T;
      Src_Align : unsigned;
      Size : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3907
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildMemCpy";

  --*
  -- * Creates and inserts a memmove between the specified pointers.
  -- *
  -- * @see llvm::IRRBuilder::CreateMemMove()
  --  

   function Build_Mem_Move
     (B : LLVM.Types.Builder_T;
      Dst : LLVM.Types.Value_T;
      Dst_Align : unsigned;
      Src : LLVM.Types.Value_T;
      Src_Align : unsigned;
      Size : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3916
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildMemMove";

function Alloca
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Array_Alloca
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Val   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

   function Build_Free (Arg_1 : LLVM.Types.Builder_T; Pointer_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3924
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildFree";

function Load_2
     (Arg_1       : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer_Val : LLVM.Types.Value_T;
      Name        : String)
      return LLVM.Types.Value_T;

   function Build_Store
     (Arg_1 : LLVM.Types.Builder_T;
      Val : LLVM.Types.Value_T;
      Ptr : LLVM.Types.Value_T) return LLVM.Types.Value_T  -- install/include/llvm-c/Core.h:3927
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBuildStore";

function GEP2
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : String)
      return LLVM.Types.Value_T;

function In_Bounds_GEP2
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : String)
      return LLVM.Types.Value_T;

function Struct_GEP2
     (B       : LLVM.Types.Builder_T;
      Ty      : LLVM.Types.Type_T;
      Pointer : LLVM.Types.Value_T;
      Idx     : unsigned;
      Name    : String)
      return LLVM.Types.Value_T;

function Global_String
     (B    : LLVM.Types.Builder_T;
      Str  : String;
      Name : String)
      return LLVM.Types.Value_T;

function Global_String_Ptr
     (B    : LLVM.Types.Builder_T;
      Str  : String;
      Name : String)
      return LLVM.Types.Value_T;

function Get_Volatile
     (Memory_Access_Inst : LLVM.Types.Value_T)
      return Boolean;

procedure Set_Volatile
     (Memory_Access_Inst : LLVM.Types.Value_T;
      Is_Volatile        : Boolean);

function Get_Weak
     (Cmp_Xchg_Inst : LLVM.Types.Value_T)
      return Boolean;

procedure Set_Weak
     (Cmp_Xchg_Inst : LLVM.Types.Value_T;
      Is_Weak       : Boolean);

   function Get_Ordering (Memory_Access_Inst : LLVM.Types.Value_T) return Atomic_Ordering_T  -- install/include/llvm-c/Core.h:3945
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetOrdering";

   procedure Set_Ordering (Memory_Access_Inst : LLVM.Types.Value_T; Ordering : Atomic_Ordering_T)  -- install/include/llvm-c/Core.h:3946
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetOrdering";

   function Get_Atomic_RMW_Bin_Op (Atomic_RMW_Inst : LLVM.Types.Value_T) return Atomic_RMW_Bin_Op_T  -- install/include/llvm-c/Core.h:3947
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetAtomicRMWBinOp";

   procedure Set_Atomic_RMW_Bin_Op (Atomic_RMW_Inst : LLVM.Types.Value_T; Bin_Op : Atomic_RMW_Bin_Op_T)  -- install/include/llvm-c/Core.h:3948
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetAtomicRMWBinOp";

  -- Casts  
function Trunc
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function Z_Ext
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function S_Ext
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function FP_To_UI
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function FP_To_SI
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function UI_To_FP
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function SI_To_FP
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function FP_Trunc
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function FP_Ext
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function Ptr_To_Int
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function Int_To_Ptr
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function Bit_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function Addr_Space_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function Z_Ext_Or_Bit_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function S_Ext_Or_Bit_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function Trunc_Or_Bit_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function Cast
     (B       : LLVM.Types.Builder_T;
      Op      : Opcode_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function Pointer_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function Int_Cast_2
     (Arg_1     : LLVM.Types.Builder_T;
      Val       : LLVM.Types.Value_T;
      Dest_Ty   : LLVM.Types.Type_T;
      Is_Signed : Boolean;
      Name      : String)
      return LLVM.Types.Value_T;

function FP_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

  --* Deprecated: This cast is always signed. Use LLVMBuildIntCast2 instead.  
  --Signed cast! 
function Int_Cast
     (Arg_1   : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;

function Get_Cast_Opcode
     (Src            : LLVM.Types.Value_T;
      Src_Is_Signed  : Boolean;
      Dest_Ty        : LLVM.Types.Type_T;
      Dest_Is_Signed : Boolean)
      return Opcode_T;

  -- Comparisons  
function I_Cmp
     (Arg_1 : LLVM.Types.Builder_T;
      Op    : Int_Predicate_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function F_Cmp
     (Arg_1 : LLVM.Types.Builder_T;
      Op    : Real_Predicate_T;
      LHS   : LLVM.Types.Value_T;
      RHS   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

  -- Miscellaneous instructions  
function Phi
     (Arg_1 : LLVM.Types.Builder_T;
      Ty    : LLVM.Types.Type_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Call_2
     (Arg_1    : LLVM.Types.Builder_T;
      Arg_2    : LLVM.Types.Type_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      Name     : String)
      return LLVM.Types.Value_T;

function Build_Select
     (Arg_1  : LLVM.Types.Builder_T;
      C_If   : LLVM.Types.Value_T;
      C_Then : LLVM.Types.Value_T;
      C_Else : LLVM.Types.Value_T;
      Name   : String)
      return LLVM.Types.Value_T;

function VA_Arg
     (Arg_1 : LLVM.Types.Builder_T;
      List  : LLVM.Types.Value_T;
      Ty    : LLVM.Types.Type_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Extract_Element
     (Arg_1   : LLVM.Types.Builder_T;
      Vec_Val : LLVM.Types.Value_T;
      Index   : LLVM.Types.Value_T;
      Name    : String)
      return LLVM.Types.Value_T;

function Insert_Element
     (Arg_1   : LLVM.Types.Builder_T;
      Vec_Val : LLVM.Types.Value_T;
      Elt_Val : LLVM.Types.Value_T;
      Index   : LLVM.Types.Value_T;
      Name    : String)
      return LLVM.Types.Value_T;

function Shuffle_Vector
     (Arg_1 : LLVM.Types.Builder_T;
      V1    : LLVM.Types.Value_T;
      V2    : LLVM.Types.Value_T;
      Mask  : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Extract_Value
     (Arg_1   : LLVM.Types.Builder_T;
      Agg_Val : LLVM.Types.Value_T;
      Index   : unsigned;
      Name    : String)
      return LLVM.Types.Value_T;

function Insert_Value
     (Arg_1   : LLVM.Types.Builder_T;
      Agg_Val : LLVM.Types.Value_T;
      Elt_Val : LLVM.Types.Value_T;
      Index   : unsigned;
      Name    : String)
      return LLVM.Types.Value_T;

function Freeze
     (Arg_1 : LLVM.Types.Builder_T;
      Val   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Is_Null
     (Arg_1 : LLVM.Types.Builder_T;
      Val   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Is_Not_Null
     (Arg_1 : LLVM.Types.Builder_T;
      Val   : LLVM.Types.Value_T;
      Name  : String)
      return LLVM.Types.Value_T;

function Ptr_Diff_2
     (Arg_1   : LLVM.Types.Builder_T;
      Elem_Ty : LLVM.Types.Type_T;
      LHS     : LLVM.Types.Value_T;
      RHS     : LLVM.Types.Value_T;
      Name    : String)
      return LLVM.Types.Value_T;

function Fence
     (B             : LLVM.Types.Builder_T;
      Ordering      : Atomic_Ordering_T;
      Single_Thread : Boolean;
      Name          : String)
      return LLVM.Types.Value_T;

function Atomic_RMW
     (B             : LLVM.Types.Builder_T;
      Op            : Atomic_RMW_Bin_Op_T;
      PTR           : LLVM.Types.Value_T;
      Val           : LLVM.Types.Value_T;
      Ordering      : Atomic_Ordering_T;
      Single_Thread : Boolean)
      return LLVM.Types.Value_T;

function Atomic_Cmp_Xchg
     (B                : LLVM.Types.Builder_T;
      Ptr              : LLVM.Types.Value_T;
      Cmp              : LLVM.Types.Value_T;
      C_New            : LLVM.Types.Value_T;
      Success_Ordering : Atomic_Ordering_T;
      Failure_Ordering : Atomic_Ordering_T;
      Single_Thread    : Boolean)
      return LLVM.Types.Value_T;

  --*
  -- * Get the number of elements in the mask of a ShuffleVector instruction.
  --  

   function Get_Num_Mask_Elements (Shuffle_Vector_Inst : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Core.h:4056
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNumMaskElements";

  --*
  -- * \returns a constant that specifies that the result of a \c ShuffleVectorInst
  -- * is undefined.
  --  

   function Get_Undef_Mask_Elem return int  -- install/include/llvm-c/Core.h:4062
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetUndefMaskElem";

  --*
  -- * Get the mask value at position Elt in the mask of a ShuffleVector
  -- * instruction.
  -- *
  -- * \Returns the result of \c LLVMGetUndefMaskElem() if the mask value is undef
  -- * at that position.
  --  

   function Get_Mask_Value (Shuffle_Vector_Inst : LLVM.Types.Value_T; Elt : unsigned) return int  -- install/include/llvm-c/Core.h:4071
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetMaskValue";

function Is_Atomic_Single_Thread
     (Atomic_Inst : LLVM.Types.Value_T)
      return Boolean;

procedure Set_Atomic_Single_Thread
     (Atomic_Inst   : LLVM.Types.Value_T;
      Single_Thread : Boolean);

   function Get_Cmp_Xchg_Success_Ordering (Cmp_Xchg_Inst : LLVM.Types.Value_T) return Atomic_Ordering_T  -- install/include/llvm-c/Core.h:4076
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetCmpXchgSuccessOrdering";

   procedure Set_Cmp_Xchg_Success_Ordering (Cmp_Xchg_Inst : LLVM.Types.Value_T; Ordering : Atomic_Ordering_T)  -- install/include/llvm-c/Core.h:4077
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetCmpXchgSuccessOrdering";

   function Get_Cmp_Xchg_Failure_Ordering (Cmp_Xchg_Inst : LLVM.Types.Value_T) return Atomic_Ordering_T  -- install/include/llvm-c/Core.h:4079
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetCmpXchgFailureOrdering";

   procedure Set_Cmp_Xchg_Failure_Ordering (Cmp_Xchg_Inst : LLVM.Types.Value_T; Ordering : Atomic_Ordering_T)  -- install/include/llvm-c/Core.h:4080
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetCmpXchgFailureOrdering";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreModuleProvider Module Providers
  -- *
  -- * @{
  --  

  --*
  -- * Changes the type of M so it can be passed to FunctionPassManagers and the
  -- * JIT.  They take ModuleProviders for historical reasons.
  --  

   function Create_Module_Provider_For_Existing_Module (M : LLVM.Types.Module_T) return LLVM.Types.Module_Provider_T  -- install/include/llvm-c/Core.h:4098
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateModuleProviderForExistingModule";

  --*
  -- * Destroys the module M.
  --  

   procedure Dispose_Module_Provider (M : LLVM.Types.Module_Provider_T)  -- install/include/llvm-c/Core.h:4103
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeModuleProvider";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreMemoryBuffers Memory Buffers
  -- *
  -- * @{
  --  

function Create_Memory_Buffer_With_Contents_Of_File
     (Path        : String;
      Out_Mem_Buf : System.Address;
      Out_Message : System.Address)
      return Boolean;

function Create_Memory_Buffer_With_STDIN
     (Out_Mem_Buf : System.Address;
      Out_Message : System.Address)
      return Boolean;

function Create_Memory_Buffer_With_Memory_Range
     (Input_Data               : String;
      Input_Data_Length        : stddef_h.size_t;
      Buffer_Name              : String;
      Requires_Null_Terminator : Boolean)
      return LLVM.Types.Memory_Buffer_T;

function Create_Memory_Buffer_With_Memory_Range_Copy
     (Input_Data        : String;
      Input_Data_Length : stddef_h.size_t;
      Buffer_Name       : String)
      return LLVM.Types.Memory_Buffer_T;

function Get_Buffer_Start
     (Mem_Buf : LLVM.Types.Memory_Buffer_T)
      return String;

   function Get_Buffer_Size (Mem_Buf : LLVM.Types.Memory_Buffer_T) return stddef_h.size_t  -- install/include/llvm-c/Core.h:4128
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetBufferSize";

   procedure Dispose_Memory_Buffer (Mem_Buf : LLVM.Types.Memory_Buffer_T)  -- install/include/llvm-c/Core.h:4129
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeMemoryBuffer";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCorePassRegistry Pass Registry
  -- * @ingroup LLVMCCore
  -- *
  -- * @{
  --  

  --* Return the global pass registry, for use with initialization functions.
  --    @see llvm::PassRegistry::getPassRegistry  

   function Get_Global_Pass_Registry return LLVM.Types.Pass_Registry_T  -- install/include/llvm-c/Core.h:4144
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetGlobalPassRegistry";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCorePassManagers Pass Managers
  -- * @ingroup LLVMCCore
  -- *
  -- * @{
  --  

  --* Constructs a new whole-module pass pipeline. This type of pipeline is
  --    suitable for link-time optimization and whole-module transformations.
  --    @see llvm::PassManager::PassManager  

   function Create_Pass_Manager return LLVM.Types.Pass_Manager_T  -- install/include/llvm-c/Core.h:4160
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreatePassManager";

  --* Constructs a new function-by-function pass pipeline over the module
  --    provider. It does not take ownership of the module provider. This type of
  --    pipeline is suitable for code generation and JIT compilation tasks.
  --    @see llvm::FunctionPassManager::FunctionPassManager  

   function Create_Function_Pass_Manager_For_Module (M : LLVM.Types.Module_T) return LLVM.Types.Pass_Manager_T  -- install/include/llvm-c/Core.h:4166
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateFunctionPassManagerForModule";

  --* Deprecated: Use LLVMCreateFunctionPassManagerForModule instead.  
   function Create_Function_Pass_Manager (MP : LLVM.Types.Module_Provider_T) return LLVM.Types.Pass_Manager_T  -- install/include/llvm-c/Core.h:4169
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateFunctionPassManager";

  --* Initializes, executes on the provided module, and finalizes all of the
  --    passes scheduled in the pass manager. Returns 1 if any of the passes
  --    modified the module, 0 otherwise.
  --    @see llvm::PassManager::run(Module&)  

function Run_Pass_Manager
     (PM : LLVM.Types.Pass_Manager_T;
      M  : LLVM.Types.Module_T)
      return Boolean;

  --* Initializes all of the function passes scheduled in the function pass
  --    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
  --    @see llvm::FunctionPassManager::doInitialization  

function Initialize_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T)
      return Boolean;

  --* Executes all of the function passes scheduled in the function pass manager
  --    on the provided function. Returns 1 if any of the passes modified the
  --    function, false otherwise.
  --    @see llvm::FunctionPassManager::run(Function&)  

function Run_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T;
      F   : LLVM.Types.Value_T)
      return Boolean;

  --* Finalizes all of the function passes scheduled in the function pass
  --    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
  --    @see llvm::FunctionPassManager::doFinalization  

function Finalize_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T)
      return Boolean;

  --* Frees the memory of a pass pipeline. For function pipelines, does not free
  --    the module provider.
  --    @see llvm::PassManagerBase::~PassManagerBase.  

   procedure Dispose_Pass_Manager (PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Core.h:4196
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposePassManager";

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCoreThreading Threading
  -- *
  -- * Handle the structures needed to make LLVM safe for multithreading.
  -- *
  -- * @{
  --  

  --* Deprecated: Multi-threading can only be enabled/disabled with the compile
  --    time define LLVM_ENABLE_THREADS.  This function always returns
  --    LLVMIsMultithreaded().  

function Start_Multithreaded
      return Boolean;

  --* Deprecated: Multi-threading can only be enabled/disabled with the compile
  --    time define LLVM_ENABLE_THREADS.  

   procedure Stop_Multithreaded  -- install/include/llvm-c/Core.h:4217
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMStopMultithreaded";

  --* Check whether LLVM is executing in thread-safe mode or not.
  --    @see llvm::llvm_is_multithreaded  

function Is_Multithreaded
      return Boolean;

  --*
  -- * @}
  --  

  --*
  -- * @}
  --  

  --*
  -- * @}
  --  

end LLVM.Core;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
