pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with Interfaces.C.Strings;
with System;
with stddef_h;
with stdint_h;
with Interfaces.C.Extensions;

package LLVM.Core is

   --  arg-macro: procedure LLVM_FOR_EACH_VALUE_SUBCLASS (macro)
   --    macro(Argument) macro(BasicBlock) macro(InlineAsm) macro(User) macro(Constant) macro(BlockAddress) macro(ConstantAggregateZero) macro(ConstantArray) macro(ConstantDataSequential) macro(ConstantDataArray) macro(ConstantDataVector) macro(ConstantExpr) macro(ConstantFP) macro(ConstantInt) macro(ConstantPointerNull) macro(ConstantStruct) macro(ConstantTokenNone) macro(ConstantVector) macro(GlobalValue) macro(GlobalAlias) macro(GlobalIFunc) macro(GlobalObject) macro(Function) macro(GlobalVariable) macro(UndefValue) macro(Instruction) macro(BinaryOperator) macro(CallInst) macro(IntrinsicInst) macro(DbgInfoIntrinsic) macro(DbgVariableIntrinsic) macro(DbgDeclareInst) macro(DbgLabelInst) macro(MemIntrinsic) macro(MemCpyInst) macro(MemMoveInst) macro(MemSetInst) macro(CmpInst) macro(FCmpInst) macro(ICmpInst) macro(ExtractElementInst) macro(GetElementPtrInst) macro(InsertElementInst) macro(InsertValueInst) macro(LandingPadInst) macro(PHINode) macro(SelectInst) macro(ShuffleVectorInst) macro(StoreInst) macro(BranchInst) macro(IndirectBrInst) macro(InvokeInst) macro(ReturnInst) macro(SwitchInst) macro(UnreachableInst) macro(ResumeInst) macro(CleanupReturnInst) macro(CatchReturnInst) macro(FuncletPadInst) macro(CatchPadInst) macro(CleanupPadInst) macro(UnaryInstruction) macro(AllocaInst) macro(CastInst) macro(AddrSpaceCastInst) macro(BitCastInst) macro(FPExtInst) macro(FPToSIInst) macro(FPToUIInst) macro(FPTruncInst) macro(IntToPtrInst) macro(PtrToIntInst) macro(SExtInst) macro(SIToFPInst) macro(TruncInst) macro(UIToFPInst) macro(ZExtInst) macro(ExtractValueInst) macro(LoadInst) macro(VAArgInst)
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
   Op_User_Op1 : constant Opcode_T := 47;
   Op_User_Op2 : constant Opcode_T := 48;
   Op_VA_Arg : constant Opcode_T := 49;
   Op_Extract_Element : constant Opcode_T := 50;
   Op_Insert_Element : constant Opcode_T := 51;
   Op_Shuffle_Vector : constant Opcode_T := 52;
   Op_Extract_Value : constant Opcode_T := 53;
   Op_Insert_Value : constant Opcode_T := 54;
   Op_Fence : constant Opcode_T := 55;
   Op_Atomic_Cmp_Xchg : constant Opcode_T := 56;
   Op_Atomic_RMW : constant Opcode_T := 57;
   Op_Resume : constant Opcode_T := 58;
   Op_Landing_Pad : constant Opcode_T := 59;
   Op_Cleanup_Ret : constant Opcode_T := 61;
   Op_Catch_Ret : constant Opcode_T := 62;
   Op_Catch_Pad : constant Opcode_T := 63;
   Op_Cleanup_Pad : constant Opcode_T := 64;
   Op_Catch_Switch : constant Opcode_T := 65;  -- llvm-9.0.0.src/include/llvm-c/Core.h:144

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
  --*< SIMD 'packed' format, or other vector type  
  --*< Metadata  
  --*< X86 MMX  
  --*< Tokens  
   type Type_Kind_T is 
     (Void_Type_Kind,
      Half_Type_Kind,
      Float_Type_Kind,
      Double_Type_Kind,
      X86_Fp80typekind,
      F_P128_Type_Kind,
      Ppc_Fp128typekind,
      Label_Type_Kind,
      Integer_Type_Kind,
      Function_Type_Kind,
      Struct_Type_Kind,
      Array_Type_Kind,
      Pointer_Type_Kind,
      Vector_Type_Kind,
      Metadata_Type_Kind,
      X86_Mmxtypekind,
      Token_Type_Kind);
   pragma Convention (C, Type_Kind_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:164

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
      Linker_Private_Weak_Linkage);
   pragma Convention (C, Linkage_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:187

  --*< The GV is visible  
  --*< The GV is hidden  
  --*< The GV is protected  
   type Visibility_T is 
     (Default_Visibility,
      Hidden_Visibility,
      Protected_Visibility);
   pragma Convention (C, Visibility_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:193

  --*< Address of the GV is significant.  
  --*< Address of the GV is locally insignificant.  
  --*< Address of the GV is globally insignificant.  
   type Unnamed_Addr_T is 
     (No_Unnamed_Addr,
      Local_Unnamed_Addr,
      Global_Unnamed_Addr);
   pragma Convention (C, Unnamed_Addr_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:199

  --*< Function to be imported from DLL.  
  --*< Function to be accessible from DLL.  
   type DLL_Storage_Class_T is 
     (Default_Storage_Class,
      DLL_Import_Storage_Class,
      DLL_Export_Storage_Class);
   pragma Convention (C, DLL_Storage_Class_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:205

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
   MS_P430_INTR_Call_Conv : constant Call_Conv_T := 69;
   X86_This_Call_Call_Conv : constant Call_Conv_T := 70;
   PTX_Kernel_Call_Conv : constant Call_Conv_T := 71;
   PTX_Device_Call_Conv : constant Call_Conv_T := 72;
   SPIRFUNC_Call_Conv : constant Call_Conv_T := 75;
   SPIRKERNEL_Call_Conv : constant Call_Conv_T := 76;
   Intel_OCLBI_Call_Conv : constant Call_Conv_T := 77;
   X8664_Sys_V_Call_Conv : constant Call_Conv_T := 78;
   Win64_Call_Conv : constant Call_Conv_T := 79;
   X86_Vector_Call_Call_Conv : constant Call_Conv_T := 80;
   HHVM_Call_Conv : constant Call_Conv_T := 81;
   HHVMC_Call_Conv : constant Call_Conv_T := 82;
   X86_INTR_Call_Conv : constant Call_Conv_T := 83;
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
   MS_P430_BUILTIN_Call_Conv : constant Call_Conv_T := 94;
   AMDGPULS_Call_Conv : constant Call_Conv_T := 95;
   AMDGPUES_Call_Conv : constant Call_Conv_T := 96;  -- llvm-9.0.0.src/include/llvm-c/Core.h:250

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
      Instruction_Value_Kind);
   pragma Convention (C, Value_Kind_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:282

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
   Int_SLE : constant Int_Predicate_T := 41;  -- llvm-9.0.0.src/include/llvm-c/Core.h:295

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
      Real_Predicate_True);
   pragma Convention (C, Real_Predicate_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:314

  --*< A catch clause    
  --*< A filter clause   
   type Landing_Pad_Clause_Ty_T is 
     (Landing_Pad_Catch,
      Landing_Pad_Filter);
   pragma Convention (C, Landing_Pad_Clause_Ty_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:319

   type Thread_Local_Mode_T is 
     (Not_Thread_Local,
      General_Dynamic_TLS_Model,
      Local_Dynamic_TLS_Model,
      Initial_Exec_TLS_Model,
      Local_Exec_TLS_Model);
   pragma Convention (C, Thread_Local_Mode_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:327

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
   Atomic_Ordering_Sequentially_Consistent : constant Atomic_Ordering_T := 7;  -- llvm-9.0.0.src/include/llvm-c/Core.h:354

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
  --                             original using an unsigned comparison  and return
  --                             the old one  

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
      Atomic_RMW_Bin_Op_U_Min);
   pragma Convention (C, Atomic_RMW_Bin_Op_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:376

   type Diagnostic_Severity_T is 
     (DS_Error,
      DS_Warning,
      DS_Remark,
      DS_Note);
   pragma Convention (C, Diagnostic_Severity_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:383

   type Inline_Asm_Dialect_T is 
     (Inline_Asm_Dialect_ATT,
      Inline_Asm_Dialect_Intel);
   pragma Convention (C, Inline_Asm_Dialect_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:388

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
      Module_Flag_Behavior_Append_Unique);
   pragma Convention (C, Module_Flag_Behavior_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:439

  --*
  -- * Attribute index are either LLVMAttributeReturnIndex,
  -- * LLVMAttributeFunctionIndex or a parameter number from 1 to N.
  --  

  -- ISO C restricts enumerator values to range of 'int'
  -- (4294967295 is too large)
  -- LLVMAttributeFunctionIndex = ~0U,
   subtype Attribute_Index_T is unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:453

  --*
  -- * @}
  --  

   procedure Initialize_Core (R : LLVM.Types.Pass_Registry_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:459
   pragma Import (C, Initialize_Core, "LLVMInitializeCore");

  --* Deallocate and destroy all ManagedStatic variables.
  --    @see llvm::llvm_shutdown
  --    @see ManagedStatic  

   procedure Shutdown;  -- llvm-9.0.0.src/include/llvm-c/Core.h:464
   pragma Import (C, Shutdown, "LLVMShutdown");

  --===-- Error handling ----------------------------------------------------=== 
   function Create_Message
     (Message : String)
      return String;
   function Create_Message_C
     (Message : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:468
   pragma Import (C, Create_Message_C, "LLVMCreateMessage");

   procedure Dispose_Message
     (Message : String);
   procedure Dispose_Message_C
     (Message : Interfaces.C.Strings.chars_ptr);  -- llvm-9.0.0.src/include/llvm-c/Core.h:469
   pragma Import (C, Dispose_Message_C, "LLVMDisposeMessage");

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

   type Diagnostic_Handler_T is access procedure  (arg1 : LLVM.Types.Diagnostic_Info_T; arg2 : System.Address);
   pragma Convention (C, Diagnostic_Handler_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:483

   type Yield_Callback_T is access procedure  (arg1 : LLVM.Types.Context_T; arg2 : System.Address);
   pragma Convention (C, Yield_Callback_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:484

  --*
  -- * Create a new context.
  -- *
  -- * Every call to this function should be paired with a call to
  -- * LLVMContextDispose() or the context will leak memory.
  --  

   function Context_Create return LLVM.Types.Context_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:492
   pragma Import (C, Context_Create, "LLVMContextCreate");

  --*
  -- * Obtain the global context instance.
  --  

   function Get_Global_Context return LLVM.Types.Context_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:497
   pragma Import (C, Get_Global_Context, "LLVMGetGlobalContext");

  --*
  -- * Set the diagnostic handler for this context.
  --  

   procedure Context_Set_Diagnostic_Handler
     (C : LLVM.Types.Context_T;
      Handler : Diagnostic_Handler_T;
      Diagnostic_Context : System.Address);  -- llvm-9.0.0.src/include/llvm-c/Core.h:502
   pragma Import (C, Context_Set_Diagnostic_Handler, "LLVMContextSetDiagnosticHandler");

  --*
  -- * Get the diagnostic handler of this context.
  --  

   function Context_Get_Diagnostic_Handler (C : LLVM.Types.Context_T) return Diagnostic_Handler_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:509
   pragma Import (C, Context_Get_Diagnostic_Handler, "LLVMContextGetDiagnosticHandler");

  --*
  -- * Get the diagnostic context of this context.
  --  

   function Context_Get_Diagnostic_Context (C : LLVM.Types.Context_T) return System.Address;  -- llvm-9.0.0.src/include/llvm-c/Core.h:514
   pragma Import (C, Context_Get_Diagnostic_Context, "LLVMContextGetDiagnosticContext");

  --*
  -- * Set the yield callback function for this context.
  -- *
  -- * @see LLVMContext::setYieldCallback()
  --  

   procedure Context_Set_Yield_Callback
     (C : LLVM.Types.Context_T;
      Callback : Yield_Callback_T;
      Opaque_Handle : System.Address);  -- llvm-9.0.0.src/include/llvm-c/Core.h:521
   pragma Import (C, Context_Set_Yield_Callback, "LLVMContextSetYieldCallback");

  --*
  -- * Retrieve whether the given context is set to discard all value names.
  -- *
  -- * @see LLVMContext::shouldDiscardValueNames()
  --  

   function Context_Should_Discard_Value_Names
     (C : LLVM.Types.Context_T)
      return Boolean;
   function Context_Should_Discard_Value_Names_C
     (C : LLVM.Types.Context_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:529
   pragma Import (C, Context_Should_Discard_Value_Names_C, "LLVMContextShouldDiscardValueNames");

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
   procedure Context_Set_Discard_Value_Names_C
     (C       : LLVM.Types.Context_T;
      Discard : LLVM.Types.Bool_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:539
   pragma Import (C, Context_Set_Discard_Value_Names_C, "LLVMContextSetDiscardValueNames");

  --*
  -- * Destroy a context instance.
  -- *
  -- * This should be called for every call to LLVMContextCreate() or memory
  -- * will be leaked.
  --  

   procedure Context_Dispose (C : LLVM.Types.Context_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:547
   pragma Import (C, Context_Dispose, "LLVMContextDispose");

  --*
  -- * Return a string representation of the DiagnosticInfo. Use
  -- * LLVMDisposeMessage to free the string.
  -- *
  -- * @see DiagnosticInfo::print()
  --  

   function Get_Diag_Info_Description
     (DI : LLVM.Types.Diagnostic_Info_T)
      return String;
   function Get_Diag_Info_Description_C
     (DI : LLVM.Types.Diagnostic_Info_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:555
   pragma Import (C, Get_Diag_Info_Description_C, "LLVMGetDiagInfoDescription");

  --*
  -- * Return an enum LLVMDiagnosticSeverity.
  -- *
  -- * @see DiagnosticInfo::getSeverity()
  --  

   function Get_Diag_Info_Severity (DI : LLVM.Types.Diagnostic_Info_T) return Diagnostic_Severity_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:562
   pragma Import (C, Get_Diag_Info_Severity, "LLVMGetDiagInfoSeverity");

function Get_MD_Kind_ID_In_Context
     (C     : LLVM.Types.Context_T;
      Name  : String;
      S_Len : unsigned)
      return unsigned;
   function Get_MD_Kind_ID_In_Context_C
     (C     : LLVM.Types.Context_T;
      Name  : Interfaces.C.Strings.chars_ptr;
      S_Len : unsigned)
      return unsigned;
   pragma Import (C, Get_MD_Kind_ID_In_Context_C, "LLVMGetMDKindIDInContext");

   function Get_MD_Kind_ID
     (Name  : String;
      S_Len : unsigned)
      return unsigned;
   function Get_MD_Kind_ID_C
     (Name  : Interfaces.C.Strings.chars_ptr;
      S_Len : unsigned)
      return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:566
   pragma Import (C, Get_MD_Kind_ID_C, "LLVMGetMDKindID");

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
   function Get_Enum_Attribute_Kind_For_Name_C
     (Name  : Interfaces.C.Strings.chars_ptr;
      S_Len : stddef_h.size_t)
      return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:579
   pragma Import (C, Get_Enum_Attribute_Kind_For_Name_C, "LLVMGetEnumAttributeKindForName");

   function Get_Last_Enum_Attribute_Kind return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:580
   pragma Import (C, Get_Last_Enum_Attribute_Kind, "LLVMGetLastEnumAttributeKind");

  --*
  -- * Create an enum attribute.
  --  

   function Create_Enum_Attribute
     (C : LLVM.Types.Context_T;
      Kind_ID : unsigned;
      Val : stdint_h.uint64_t) return LLVM.Types.Attribute_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:585
   pragma Import (C, Create_Enum_Attribute, "LLVMCreateEnumAttribute");

  --*
  -- * Get the unique id corresponding to the enum attribute
  -- * passed as argument.
  --  

   function Get_Enum_Attribute_Kind (A : LLVM.Types.Attribute_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:592
   pragma Import (C, Get_Enum_Attribute_Kind, "LLVMGetEnumAttributeKind");

  --*
  -- * Get the enum attribute's value. 0 is returned if none exists.
  --  

   function Get_Enum_Attribute_Value (A : LLVM.Types.Attribute_T) return stdint_h.uint64_t;  -- llvm-9.0.0.src/include/llvm-c/Core.h:597
   pragma Import (C, Get_Enum_Attribute_Value, "LLVMGetEnumAttributeValue");

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
   function Create_String_Attribute_C
     (C        : LLVM.Types.Context_T;
      K        : Interfaces.C.Strings.chars_ptr;
      K_Length : unsigned;
      V        : Interfaces.C.Strings.chars_ptr;
      V_Length : unsigned)
      return LLVM.Types.Attribute_T;
   pragma Import (C, Create_String_Attribute_C, "LLVMCreateStringAttribute");

  --*
  -- * Get the string attribute's kind.
  --  

   function Get_String_Attribute_Kind
     (A      : LLVM.Types.Attribute_T;
      Length : unsigned)
      return String;
   function Get_String_Attribute_Kind_C
     (A      : LLVM.Types.Attribute_T;
      Length : unsigned)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:609
   pragma Import (C, Get_String_Attribute_Kind_C, "LLVMGetStringAttributeKind");

  --*
  -- * Get the string attribute's value.
  --  

   function Get_String_Attribute_Value
     (A      : LLVM.Types.Attribute_T;
      Length : unsigned)
      return String;
   function Get_String_Attribute_Value_C
     (A      : LLVM.Types.Attribute_T;
      Length : unsigned)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:614
   pragma Import (C, Get_String_Attribute_Value_C, "LLVMGetStringAttributeValue");

  --*
  -- * Check for the different types of attributes.
  --  

   function Is_Enum_Attribute
     (A : LLVM.Types.Attribute_T)
      return Boolean;
   function Is_Enum_Attribute_C
     (A : LLVM.Types.Attribute_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:619
   pragma Import (C, Is_Enum_Attribute_C, "LLVMIsEnumAttribute");

   function Is_String_Attribute
     (A : LLVM.Types.Attribute_T)
      return Boolean;
   function Is_String_Attribute_C
     (A : LLVM.Types.Attribute_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:620
   pragma Import (C, Is_String_Attribute_C, "LLVMIsStringAttribute");

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
   function Module_Create_With_Name_C
     (Module_ID : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Module_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:645
   pragma Import (C, Module_Create_With_Name_C, "LLVMModuleCreateWithName");

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
   function Module_Create_With_Name_In_Context_C
     (Module_ID : Interfaces.C.Strings.chars_ptr;
      C         : LLVM.Types.Context_T)
      return LLVM.Types.Module_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:653
   pragma Import (C, Module_Create_With_Name_In_Context_C, "LLVMModuleCreateWithNameInContext");

  --*
  -- * Return an exact copy of the specified module.
  --  

   function Clone_Module (M : LLVM.Types.Module_T) return LLVM.Types.Module_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:658
   pragma Import (C, Clone_Module, "LLVMCloneModule");

  --*
  -- * Destroy a module instance.
  -- *
  -- * This must be called for every created module or memory will be
  -- * leaked.
  --  

   procedure Dispose_Module (M : LLVM.Types.Module_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:666
   pragma Import (C, Dispose_Module, "LLVMDisposeModule");

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
      Len : stddef_h.size_t)
      return String;
   function Get_Module_Identifier_C
     (M   : LLVM.Types.Module_T;
      Len : stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:676
   pragma Import (C, Get_Module_Identifier_C, "LLVMGetModuleIdentifier");

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
   procedure Set_Module_Identifier_C
     (M     : LLVM.Types.Module_T;
      Ident : Interfaces.C.Strings.chars_ptr;
      Len   : stddef_h.size_t);
   pragma Import (C, Set_Module_Identifier_C, "LLVMSetModuleIdentifier");

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
      Len : stddef_h.size_t)
      return String;
   function Get_Source_File_Name_C
     (M   : LLVM.Types.Module_T;
      Len : stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:696
   pragma Import (C, Get_Source_File_Name_C, "LLVMGetSourceFileName");

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
   procedure Set_Source_File_Name_C
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr;
      Len  : stddef_h.size_t);
   pragma Import (C, Set_Source_File_Name_C, "LLVMSetSourceFileName");

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
   function Get_Data_Layout_Str_C
     (M : LLVM.Types.Module_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:718
   pragma Import (C, Get_Data_Layout_Str_C, "LLVMGetDataLayoutStr");

   function Get_Data_Layout
     (M : LLVM.Types.Module_T)
      return String;
   function Get_Data_Layout_C
     (M : LLVM.Types.Module_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:719
   pragma Import (C, Get_Data_Layout_C, "LLVMGetDataLayout");

  --*
  -- * Set the data layout for a module.
  -- *
  -- * @see Module::setDataLayout()
  --  

   procedure Set_Data_Layout
     (M               : LLVM.Types.Module_T;
      Data_Layout_Str : String);
   procedure Set_Data_Layout_C
     (M               : LLVM.Types.Module_T;
      Data_Layout_Str : Interfaces.C.Strings.chars_ptr);  -- llvm-9.0.0.src/include/llvm-c/Core.h:726
   pragma Import (C, Set_Data_Layout_C, "LLVMSetDataLayout");

  --*
  -- * Obtain the target triple for a module.
  -- *
  -- * @see Module::getTargetTriple()
  --  

   function Get_Target
     (M : LLVM.Types.Module_T)
      return String;
   function Get_Target_C
     (M : LLVM.Types.Module_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:733
   pragma Import (C, Get_Target_C, "LLVMGetTarget");

  --*
  -- * Set the target triple for a module.
  -- *
  -- * @see Module::setTargetTriple()
  --  

   procedure Set_Target
     (M      : LLVM.Types.Module_T;
      Triple : String);
   procedure Set_Target_C
     (M      : LLVM.Types.Module_T;
      Triple : Interfaces.C.Strings.chars_ptr);  -- llvm-9.0.0.src/include/llvm-c/Core.h:740
   pragma Import (C, Set_Target_C, "LLVMSetTarget");

  --*
  -- * Returns the module flags as an array of flag-key-value triples.  The caller
  -- * is responsible for freeing this array by calling
  -- * \c LLVMDisposeModuleFlagsMetadata.
  -- *
  -- * @see Module::getModuleFlagsMetadata()
  --  

   function Copy_Module_Flags_Metadata (M : LLVM.Types.Module_T; Len : access stddef_h.size_t) return System.Address;  -- llvm-9.0.0.src/include/llvm-c/Core.h:749
   pragma Import (C, Copy_Module_Flags_Metadata, "LLVMCopyModuleFlagsMetadata");

  --*
  -- * Destroys module flags metadata entries.
  --  

   procedure Dispose_Module_Flags_Metadata (Entries : System.Address);  -- llvm-9.0.0.src/include/llvm-c/Core.h:754
   pragma Import (C, Dispose_Module_Flags_Metadata, "LLVMDisposeModuleFlagsMetadata");

  --*
  -- * Returns the flag behavior for a module flag entry at a specific index.
  -- *
  -- * @see Module::ModuleFlagEntry::Behavior
  --  

   function Module_Flag_Entries_Get_Flag_Behavior (Entries : System.Address; Index : unsigned) return Module_Flag_Behavior_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:762
   pragma Import (C, Module_Flag_Entries_Get_Flag_Behavior, "LLVMModuleFlagEntriesGetFlagBehavior");

  --*
  -- * Returns the key for a module flag entry at a specific index.
  -- *
  -- * @see Module::ModuleFlagEntry::Key
  --  

function Module_Flag_Entries_Get_Key
     (Entries : System.Address;
      Index   : unsigned;
      Len     : stddef_h.size_t)
      return String;
   function Module_Flag_Entries_Get_Key_C
     (Entries : System.Address;
      Index   : unsigned;
      Len     : stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Module_Flag_Entries_Get_Key_C, "LLVMModuleFlagEntriesGetKey");

  --*
  -- * Returns the metadata for a module flag entry at a specific index.
  -- *
  -- * @see Module::ModuleFlagEntry::Val
  --  

   function Module_Flag_Entries_Get_Metadata (Entries : System.Address; Index : unsigned) return LLVM.Types.Metadata_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:778
   pragma Import (C, Module_Flag_Entries_Get_Metadata, "LLVMModuleFlagEntriesGetMetadata");

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
   function Get_Module_Flag_C
     (M       : LLVM.Types.Module_T;
      Key     : Interfaces.C.Strings.chars_ptr;
      Key_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T;
   pragma Import (C, Get_Module_Flag_C, "LLVMGetModuleFlag");

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
   procedure Add_Module_Flag_C
     (M        : LLVM.Types.Module_T;
      Behavior : Module_Flag_Behavior_T;
      Key      : Interfaces.C.Strings.chars_ptr;
      Key_Len  : stddef_h.size_t;
      Val      : LLVM.Types.Metadata_T);
   pragma Import (C, Add_Module_Flag_C, "LLVMAddModuleFlag");

  --*
  -- * Dump a representation of a module to stderr.
  -- *
  -- * @see Module::dump()
  --  

   procedure Dump_Module (M : LLVM.Types.Module_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:805
   pragma Import (C, Dump_Module, "LLVMDumpModule");

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
   function Print_Module_To_File_C
     (M             : LLVM.Types.Module_T;
      Filename      : Interfaces.C.Strings.chars_ptr;
      Error_Message : System.Address)
      return LLVM.Types.Bool_T;
   pragma Import (C, Print_Module_To_File_C, "LLVMPrintModuleToFile");

  --*
  -- * Return a string representation of the module. Use
  -- * LLVMDisposeMessage to free the string.
  -- *
  -- * @see Module::print()
  --  

   function Print_Module_To_String
     (M : LLVM.Types.Module_T)
      return String;
   function Print_Module_To_String_C
     (M : LLVM.Types.Module_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:822
   pragma Import (C, Print_Module_To_String_C, "LLVMPrintModuleToString");

  --*
  -- * Get inline assembly for a module.
  -- *
  -- * @see Module::getModuleInlineAsm()
  --  

   function Get_Module_Inline_Asm
     (M   : LLVM.Types.Module_T;
      Len : stddef_h.size_t)
      return String;
   function Get_Module_Inline_Asm_C
     (M   : LLVM.Types.Module_T;
      Len : stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:829
   pragma Import (C, Get_Module_Inline_Asm_C, "LLVMGetModuleInlineAsm");

  --*
  -- * Set inline assembly for a module.
  -- *
  -- * @see Module::setModuleInlineAsm()
  --  

procedure Set_Module_Inline_Asm2
     (M   : LLVM.Types.Module_T;
      Asm : String;
      Len : stddef_h.size_t);
   procedure Set_Module_Inline_Asm2_C
     (M   : LLVM.Types.Module_T;
      Asm : Interfaces.C.Strings.chars_ptr;
      Len : stddef_h.size_t);
   pragma Import (C, Set_Module_Inline_Asm2_C, "LLVMSetModuleInlineAsm2");

  --*
  -- * Append inline assembly to a module.
  -- *
  -- * @see Module::appendModuleInlineAsm()
  --  

procedure Append_Module_Inline_Asm
     (M   : LLVM.Types.Module_T;
      Asm : String;
      Len : stddef_h.size_t);
   procedure Append_Module_Inline_Asm_C
     (M   : LLVM.Types.Module_T;
      Asm : Interfaces.C.Strings.chars_ptr;
      Len : stddef_h.size_t);
   pragma Import (C, Append_Module_Inline_Asm_C, "LLVMAppendModuleInlineAsm");

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
      Dialect          : Inline_Asm_Dialect_T)
      return LLVM.Types.Value_T;
   function Get_Inline_Asm_C
     (Ty               : LLVM.Types.Type_T;
      Asm_String       : Interfaces.C.Strings.chars_ptr;
      Asm_String_Size  : stddef_h.size_t;
      Constraints      : Interfaces.C.Strings.chars_ptr;
      Constraints_Size : stddef_h.size_t;
      Has_Side_Effects : LLVM.Types.Bool_T;
      Is_Align_Stack   : LLVM.Types.Bool_T;
      Dialect          : Inline_Asm_Dialect_T)
      return LLVM.Types.Value_T;
   pragma Import (C, Get_Inline_Asm_C, "LLVMGetInlineAsm");

  --*
  -- * Obtain the context to which this module is associated.
  -- *
  -- * @see Module::getContext()
  --  

   function Get_Module_Context (M : LLVM.Types.Module_T) return LLVM.Types.Context_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:861
   pragma Import (C, Get_Module_Context, "LLVMGetModuleContext");

  --*
  -- * Obtain a Type from a module by its registered name.
  --  

   function Get_Type_By_Name
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Type_T;
   function Get_Type_By_Name_C
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:866
   pragma Import (C, Get_Type_By_Name_C, "LLVMGetTypeByName");

  --*
  -- * Obtain an iterator to the first NamedMDNode in a Module.
  -- *
  -- * @see llvm::Module::named_metadata_begin()
  --  

   function Get_First_Named_Metadata (M : LLVM.Types.Module_T) return LLVM.Types.Named_MD_Node_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:873
   pragma Import (C, Get_First_Named_Metadata, "LLVMGetFirstNamedMetadata");

  --*
  -- * Obtain an iterator to the last NamedMDNode in a Module.
  -- *
  -- * @see llvm::Module::named_metadata_end()
  --  

   function Get_Last_Named_Metadata (M : LLVM.Types.Module_T) return LLVM.Types.Named_MD_Node_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:880
   pragma Import (C, Get_Last_Named_Metadata, "LLVMGetLastNamedMetadata");

  --*
  -- * Advance a NamedMDNode iterator to the next NamedMDNode.
  -- *
  -- * Returns NULL if the iterator was already at the end and there are no more
  -- * named metadata nodes.
  --  

   function Get_Next_Named_Metadata (Named_MD_Node : LLVM.Types.Named_MD_Node_T) return LLVM.Types.Named_MD_Node_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:888
   pragma Import (C, Get_Next_Named_Metadata, "LLVMGetNextNamedMetadata");

  --*
  -- * Decrement a NamedMDNode iterator to the previous NamedMDNode.
  -- *
  -- * Returns NULL if the iterator was already at the beginning and there are
  -- * no previous named metadata nodes.
  --  

   function Get_Previous_Named_Metadata (Named_MD_Node : LLVM.Types.Named_MD_Node_T) return LLVM.Types.Named_MD_Node_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:896
   pragma Import (C, Get_Previous_Named_Metadata, "LLVMGetPreviousNamedMetadata");

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
   function Get_Named_Metadata_C
     (M        : LLVM.Types.Module_T;
      Name     : Interfaces.C.Strings.chars_ptr;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Named_MD_Node_T;
   pragma Import (C, Get_Named_Metadata_C, "LLVMGetNamedMetadata");

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
   function Get_Or_Insert_Named_Metadata_C
     (M        : LLVM.Types.Module_T;
      Name     : Interfaces.C.Strings.chars_ptr;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Named_MD_Node_T;
   pragma Import (C, Get_Or_Insert_Named_Metadata_C, "LLVMGetOrInsertNamedMetadata");

  --*
  -- * Retrieve the name of a NamedMDNode.
  -- *
  -- * @see llvm::NamedMDNode::getName()
  --  

   function Get_Named_Metadata_Name
     (Named_MD : LLVM.Types.Named_MD_Node_T;
      Name_Len : stddef_h.size_t)
      return String;
   function Get_Named_Metadata_Name_C
     (Named_MD : LLVM.Types.Named_MD_Node_T;
      Name_Len : stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:922
   pragma Import (C, Get_Named_Metadata_Name_C, "LLVMGetNamedMetadataName");

  --*
  -- * Obtain the number of operands for named metadata in a module.
  -- *
  -- * @see llvm::Module::getNamedMetadata()
  --  

   function Get_Named_Metadata_Num_Operands
     (M    : LLVM.Types.Module_T;
      Name : String)
      return unsigned;
   function Get_Named_Metadata_Num_Operands_C
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:930
   pragma Import (C, Get_Named_Metadata_Num_Operands_C, "LLVMGetNamedMetadataNumOperands");

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
   procedure Get_Named_Metadata_Operands_C
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr;
      Dest : System.Address);
   pragma Import (C, Get_Named_Metadata_Operands_C, "LLVMGetNamedMetadataOperands");

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
   procedure Add_Named_Metadata_Operand_C
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr;
      Val  : LLVM.Types.Value_T);
   pragma Import (C, Add_Named_Metadata_Operand_C, "LLVMAddNamedMetadataOperand");

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
      Length : unsigned)
      return String;
   function Get_Debug_Loc_Directory_C
     (Val    : LLVM.Types.Value_T;
      Length : unsigned)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:963
   pragma Import (C, Get_Debug_Loc_Directory_C, "LLVMGetDebugLocDirectory");

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
      Length : unsigned)
      return String;
   function Get_Debug_Loc_Filename_C
     (Val    : LLVM.Types.Value_T;
      Length : unsigned)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:973
   pragma Import (C, Get_Debug_Loc_Filename_C, "LLVMGetDebugLocFilename");

  --*
  -- * Return the line number of the debug location for this value, which must be
  -- * an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
  -- *
  -- * @see llvm::Instruction::getDebugLoc()
  -- * @see llvm::GlobalVariable::getDebugInfo()
  -- * @see llvm::Function::getSubprogram()
  --  

   function Get_Debug_Loc_Line (Val : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:983
   pragma Import (C, Get_Debug_Loc_Line, "LLVMGetDebugLocLine");

  --*
  -- * Return the column number of the debug location for this value, which must be
  -- * an llvm::Instruction.
  -- *
  -- * @see llvm::Instruction::getDebugLoc()
  --  

   function Get_Debug_Loc_Column (Val : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:991
   pragma Import (C, Get_Debug_Loc_Column, "LLVMGetDebugLocColumn");

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
   function Add_Function_C
     (M           : LLVM.Types.Module_T;
      Name        : Interfaces.C.Strings.chars_ptr;
      Function_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Value_T;
   pragma Import (C, Add_Function_C, "LLVMAddFunction");

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
   function Get_Named_Function_C
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1008
   pragma Import (C, Get_Named_Function_C, "LLVMGetNamedFunction");

  --*
  -- * Obtain an iterator to the first Function in a Module.
  -- *
  -- * @see llvm::Module::begin()
  --  

   function Get_First_Function (M : LLVM.Types.Module_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1015
   pragma Import (C, Get_First_Function, "LLVMGetFirstFunction");

  --*
  -- * Obtain an iterator to the last Function in a Module.
  -- *
  -- * @see llvm::Module::end()
  --  

   function Get_Last_Function (M : LLVM.Types.Module_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1022
   pragma Import (C, Get_Last_Function, "LLVMGetLastFunction");

  --*
  -- * Advance a Function iterator to the next Function.
  -- *
  -- * Returns NULL if the iterator was already at the end and there are no more
  -- * functions.
  --  

   function Get_Next_Function (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1030
   pragma Import (C, Get_Next_Function, "LLVMGetNextFunction");

  --*
  -- * Decrement a Function iterator to the previous Function.
  -- *
  -- * Returns NULL if the iterator was already at the beginning and there are
  -- * no previous functions.
  --  

   function Get_Previous_Function (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1038
   pragma Import (C, Get_Previous_Function, "LLVMGetPreviousFunction");

  --* Deprecated: Use LLVMSetModuleInlineAsm2 instead.  
   procedure Set_Module_Inline_Asm
     (M   : LLVM.Types.Module_T;
      Asm : String);
   procedure Set_Module_Inline_Asm_C
     (M   : LLVM.Types.Module_T;
      Asm : Interfaces.C.Strings.chars_ptr);  -- llvm-9.0.0.src/include/llvm-c/Core.h:1041
   pragma Import (C, Set_Module_Inline_Asm_C, "LLVMSetModuleInlineAsm");

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

   function Get_Type_Kind (Ty : LLVM.Types.Type_T) return Type_Kind_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1081
   pragma Import (C, Get_Type_Kind, "LLVMGetTypeKind");

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
   function Type_Is_Sized_C
     (Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1090
   pragma Import (C, Type_Is_Sized_C, "LLVMTypeIsSized");

  --*
  -- * Obtain the context to which this type instance is associated.
  -- *
  -- * @see llvm::Type::getContext()
  --  

   function Get_Type_Context (Ty : LLVM.Types.Type_T) return LLVM.Types.Context_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1097
   pragma Import (C, Get_Type_Context, "LLVMGetTypeContext");

  --*
  -- * Dump a representation of a type to stderr.
  -- *
  -- * @see llvm::Type::dump()
  --  

   procedure Dump_Type (Val : LLVM.Types.Type_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:1104
   pragma Import (C, Dump_Type, "LLVMDumpType");

  --*
  -- * Return a string representation of the type. Use
  -- * LLVMDisposeMessage to free the string.
  -- *
  -- * @see llvm::Type::print()
  --  

   function Print_Type_To_String
     (Val : LLVM.Types.Type_T)
      return String;
   function Print_Type_To_String_C
     (Val : LLVM.Types.Type_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1112
   pragma Import (C, Print_Type_To_String_C, "LLVMPrintTypeToString");

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

   function Int1_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1125
   pragma Import (C, Int1_Type_In_Context, "LLVMInt1TypeInContext");

   function Int8_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1126
   pragma Import (C, Int8_Type_In_Context, "LLVMInt8TypeInContext");

   function Int16_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1127
   pragma Import (C, Int16_Type_In_Context, "LLVMInt16TypeInContext");

   function Int32_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1128
   pragma Import (C, Int32_Type_In_Context, "LLVMInt32TypeInContext");

   function Int64_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1129
   pragma Import (C, Int64_Type_In_Context, "LLVMInt64TypeInContext");

   function Int128_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1130
   pragma Import (C, Int128_Type_In_Context, "LLVMInt128TypeInContext");

   function Int_Type_In_Context (C : LLVM.Types.Context_T; Num_Bits : unsigned) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1131
   pragma Import (C, Int_Type_In_Context, "LLVMIntTypeInContext");

  --*
  -- * Obtain an integer type from the global context with a specified bit
  -- * width.
  --  

   function Int1_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1137
   pragma Import (C, Int1_Type, "LLVMInt1Type");

   function Int8_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1138
   pragma Import (C, Int8_Type, "LLVMInt8Type");

   function Int16_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1139
   pragma Import (C, Int16_Type, "LLVMInt16Type");

   function Int32_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1140
   pragma Import (C, Int32_Type, "LLVMInt32Type");

   function Int64_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1141
   pragma Import (C, Int64_Type, "LLVMInt64Type");

   function Int128_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1142
   pragma Import (C, Int128_Type, "LLVMInt128Type");

   function Int_Type (Num_Bits : unsigned) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1143
   pragma Import (C, Int_Type, "LLVMIntType");

   function Get_Int_Type_Width (Integer_Ty : LLVM.Types.Type_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1144
   pragma Import (C, Get_Int_Type_Width, "LLVMGetIntTypeWidth");

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

   function Half_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1159
   pragma Import (C, Half_Type_In_Context, "LLVMHalfTypeInContext");

  --*
  -- * Obtain a 32-bit floating point type from a context.
  --  

   function Float_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1164
   pragma Import (C, Float_Type_In_Context, "LLVMFloatTypeInContext");

  --*
  -- * Obtain a 64-bit floating point type from a context.
  --  

   function Double_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1169
   pragma Import (C, Double_Type_In_Context, "LLVMDoubleTypeInContext");

  --*
  -- * Obtain a 80-bit floating point type (X87) from a context.
  --  

   function X86_F_P80_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1174
   pragma Import (C, X86_F_P80_Type_In_Context, "LLVMX86FP80TypeInContext");

  --*
  -- * Obtain a 128-bit floating point type (112-bit mantissa) from a
  -- * context.
  --  

   function F_P128_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1180
   pragma Import (C, F_P128_Type_In_Context, "LLVMFP128TypeInContext");

  --*
  -- * Obtain a 128-bit floating point type (two 64-bits) from a context.
  --  

   function PPCF_P128_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1185
   pragma Import (C, PPCF_P128_Type_In_Context, "LLVMPPCFP128TypeInContext");

  --*
  -- * Obtain a floating point type from the global context.
  -- *
  -- * These map to the functions in this group of the same name.
  --  

   function Half_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1192
   pragma Import (C, Half_Type, "LLVMHalfType");

   function Float_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1193
   pragma Import (C, Float_Type, "LLVMFloatType");

   function Double_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1194
   pragma Import (C, Double_Type, "LLVMDoubleType");

   function X86_F_P80_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1195
   pragma Import (C, X86_F_P80_Type, "LLVMX86FP80Type");

   function F_P128_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1196
   pragma Import (C, F_P128_Type, "LLVMFP128Type");

   function PPCF_P128_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1197
   pragma Import (C, PPCF_P128_Type, "LLVMPPCFP128Type");

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
   function Function_Type_C
     (Return_Type : LLVM.Types.Type_T;
      Param_Types : System.Address;
      Param_Count : unsigned;
      Is_Var_Arg  : LLVM.Types.Bool_T)
      return LLVM.Types.Type_T;
   pragma Import (C, Function_Type_C, "LLVMFunctionType");

  --*
  -- * Returns whether a function type is variadic.
  --  

   function Is_Function_Var_Arg
     (Function_Ty : LLVM.Types.Type_T)
      return Boolean;
   function Is_Function_Var_Arg_C
     (Function_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1222
   pragma Import (C, Is_Function_Var_Arg_C, "LLVMIsFunctionVarArg");

  --*
  -- * Obtain the Type this function Type returns.
  --  

   function Get_Return_Type (Function_Ty : LLVM.Types.Type_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1227
   pragma Import (C, Get_Return_Type, "LLVMGetReturnType");

  --*
  -- * Obtain the number of parameters this function accepts.
  --  

   function Count_Param_Types (Function_Ty : LLVM.Types.Type_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1232
   pragma Import (C, Count_Param_Types, "LLVMCountParamTypes");

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

   procedure Get_Param_Types (Function_Ty : LLVM.Types.Type_T; Dest : System.Address);  -- llvm-9.0.0.src/include/llvm-c/Core.h:1245
   pragma Import (C, Get_Param_Types, "LLVMGetParamTypes");

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
   function Struct_Type_In_Context_C
     (C             : LLVM.Types.Context_T;
      Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : LLVM.Types.Bool_T)
      return LLVM.Types.Type_T;
   pragma Import (C, Struct_Type_In_Context_C, "LLVMStructTypeInContext");

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
   function Struct_Type_C
     (Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : LLVM.Types.Bool_T)
      return LLVM.Types.Type_T;
   pragma Import (C, Struct_Type_C, "LLVMStructType");

  --*
  -- * Create an empty structure in a context having a specified name.
  -- *
  -- * @see llvm::StructType::create()
  --  

   function Struct_Create_Named
     (C    : LLVM.Types.Context_T;
      Name : String)
      return LLVM.Types.Type_T;
   function Struct_Create_Named_C
     (C    : LLVM.Types.Context_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1285
   pragma Import (C, Struct_Create_Named_C, "LLVMStructCreateNamed");

  --*
  -- * Obtain the name of a structure.
  -- *
  -- * @see llvm::StructType::getName()
  --  

   function Get_Struct_Name
     (Ty : LLVM.Types.Type_T)
      return String;
   function Get_Struct_Name_C
     (Ty : LLVM.Types.Type_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1292
   pragma Import (C, Get_Struct_Name_C, "LLVMGetStructName");

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
   procedure Struct_Set_Body_C
     (Struct_Ty     : LLVM.Types.Type_T;
      Element_Types : System.Address;
      Element_Count : unsigned;
      Packed        : LLVM.Types.Bool_T);
   pragma Import (C, Struct_Set_Body_C, "LLVMStructSetBody");

  --*
  -- * Get the number of elements defined inside the structure.
  -- *
  -- * @see llvm::StructType::getNumElements()
  --  

   function Count_Struct_Element_Types (Struct_Ty : LLVM.Types.Type_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1307
   pragma Import (C, Count_Struct_Element_Types, "LLVMCountStructElementTypes");

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

   procedure Get_Struct_Element_Types (Struct_Ty : LLVM.Types.Type_T; Dest : System.Address);  -- llvm-9.0.0.src/include/llvm-c/Core.h:1319
   pragma Import (C, Get_Struct_Element_Types, "LLVMGetStructElementTypes");

  --*
  -- * Get the type of the element at a given index in the structure.
  -- *
  -- * @see llvm::StructType::getTypeAtIndex()
  --  

   function Struct_Get_Type_At_Index (Struct_Ty : LLVM.Types.Type_T; i : unsigned) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1326
   pragma Import (C, Struct_Get_Type_At_Index, "LLVMStructGetTypeAtIndex");

  --*
  -- * Determine whether a structure is packed.
  -- *
  -- * @see llvm::StructType::isPacked()
  --  

   function Is_Packed_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return Boolean;
   function Is_Packed_Struct_C
     (Struct_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1333
   pragma Import (C, Is_Packed_Struct_C, "LLVMIsPackedStruct");

  --*
  -- * Determine whether a structure is opaque.
  -- *
  -- * @see llvm::StructType::isOpaque()
  --  

   function Is_Opaque_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return Boolean;
   function Is_Opaque_Struct_C
     (Struct_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1340
   pragma Import (C, Is_Opaque_Struct_C, "LLVMIsOpaqueStruct");

  --*
  -- * Determine whether a structure is literal.
  -- *
  -- * @see llvm::StructType::isLiteral()
  --  

   function Is_Literal_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return Boolean;
   function Is_Literal_Struct_C
     (Struct_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1347
   pragma Import (C, Is_Literal_Struct_C, "LLVMIsLiteralStruct");

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
  -- * Obtain the type of elements within a sequential type.
  -- *
  -- * This works on array, vector, and pointer types.
  -- *
  -- * @see llvm::SequentialType::getElementType()
  --  

   function Get_Element_Type (Ty : LLVM.Types.Type_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1369
   pragma Import (C, Get_Element_Type, "LLVMGetElementType");

  --*
  -- * Returns type's subtypes
  -- *
  -- * @see llvm::Type::subtypes()
  --  

   procedure Get_Subtypes (Tp : LLVM.Types.Type_T; Arr : System.Address);  -- llvm-9.0.0.src/include/llvm-c/Core.h:1376
   pragma Import (C, Get_Subtypes, "LLVMGetSubtypes");

  --*
  -- *  Return the number of types in the derived type.
  -- *
  -- * @see llvm::Type::getNumContainedTypes()
  --  

   function Get_Num_Contained_Types (Tp : LLVM.Types.Type_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1383
   pragma Import (C, Get_Num_Contained_Types, "LLVMGetNumContainedTypes");

  --*
  -- * Create a fixed size array type that refers to a specific type.
  -- *
  -- * The created type will exist in the context that its element type
  -- * exists in.
  -- *
  -- * @see llvm::ArrayType::get()
  --  

   function Array_Type (Element_Type : LLVM.Types.Type_T; Element_Count : unsigned) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1393
   pragma Import (C, Array_Type, "LLVMArrayType");

  --*
  -- * Obtain the length of an array type.
  -- *
  -- * This only works on types that represent arrays.
  -- *
  -- * @see llvm::ArrayType::getNumElements()
  --  

   function Get_Array_Length (Array_Ty : LLVM.Types.Type_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1402
   pragma Import (C, Get_Array_Length, "LLVMGetArrayLength");

  --*
  -- * Create a pointer type that points to a defined type.
  -- *
  -- * The created type will exist in the context that its pointee type
  -- * exists in.
  -- *
  -- * @see llvm::PointerType::get()
  --  

   function Pointer_Type (Element_Type : LLVM.Types.Type_T; Address_Space : unsigned) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1412
   pragma Import (C, Pointer_Type, "LLVMPointerType");

  --*
  -- * Obtain the address space of a pointer type.
  -- *
  -- * This only works on types that represent pointers.
  -- *
  -- * @see llvm::PointerType::getAddressSpace()
  --  

   function Get_Pointer_Address_Space (Pointer_Ty : LLVM.Types.Type_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1421
   pragma Import (C, Get_Pointer_Address_Space, "LLVMGetPointerAddressSpace");

  --*
  -- * Create a vector type that contains a defined type and has a specific
  -- * number of elements.
  -- *
  -- * The created type will exist in the context thats its element type
  -- * exists in.
  -- *
  -- * @see llvm::VectorType::get()
  --  

   function Vector_Type (Element_Type : LLVM.Types.Type_T; Element_Count : unsigned) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1432
   pragma Import (C, Vector_Type, "LLVMVectorType");

  --*
  -- * Obtain the number of elements in a vector type.
  -- *
  -- * This only works on types that represent vectors.
  -- *
  -- * @see llvm::VectorType::getNumElements()
  --  

   function Get_Vector_Size (Vector_Ty : LLVM.Types.Type_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1441
   pragma Import (C, Get_Vector_Size, "LLVMGetVectorSize");

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

   function Void_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1456
   pragma Import (C, Void_Type_In_Context, "LLVMVoidTypeInContext");

  --*
  -- * Create a label type in a context.
  --  

   function Label_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1461
   pragma Import (C, Label_Type_In_Context, "LLVMLabelTypeInContext");

  --*
  -- * Create a X86 MMX type in a context.
  --  

   function X86_MMX_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1466
   pragma Import (C, X86_MMX_Type_In_Context, "LLVMX86MMXTypeInContext");

  --*
  -- * Create a token type in a context.
  --  

   function Token_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1471
   pragma Import (C, Token_Type_In_Context, "LLVMTokenTypeInContext");

  --*
  -- * Create a metadata type in a context.
  --  

   function Metadata_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1476
   pragma Import (C, Metadata_Type_In_Context, "LLVMMetadataTypeInContext");

  --*
  -- * These are similar to the above functions except they operate on the
  -- * global context.
  --  

   function Void_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1482
   pragma Import (C, Void_Type, "LLVMVoidType");

   function Label_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1483
   pragma Import (C, Label_Type, "LLVMLabelType");

   function X86_MMX_Type return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1484
   pragma Import (C, X86_MMX_Type, "LLVMX86MMXType");

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

   function Type_Of (Val : LLVM.Types.Value_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1612
   pragma Import (C, Type_Of, "LLVMTypeOf");

  --*
  -- * Obtain the enumerated type of a Value instance.
  -- *
  -- * @see llvm::Value::getValueID()
  --  

   function Get_Value_Kind (Val : LLVM.Types.Value_T) return Value_Kind_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1619
   pragma Import (C, Get_Value_Kind, "LLVMGetValueKind");

  --*
  -- * Obtain the string name of a value.
  -- *
  -- * @see llvm::Value::getName()
  --  

   function Get_Value_Name2
     (Val    : LLVM.Types.Value_T;
      Length : stddef_h.size_t)
      return String;
   function Get_Value_Name2_C
     (Val    : LLVM.Types.Value_T;
      Length : stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1626
   pragma Import (C, Get_Value_Name2_C, "LLVMGetValueName2");

  --*
  -- * Set the string name of a value.
  -- *
  -- * @see llvm::Value::setName()
  --  

procedure Set_Value_Name2
     (Val      : LLVM.Types.Value_T;
      Name     : String;
      Name_Len : stddef_h.size_t);
   procedure Set_Value_Name2_C
     (Val      : LLVM.Types.Value_T;
      Name     : Interfaces.C.Strings.chars_ptr;
      Name_Len : stddef_h.size_t);
   pragma Import (C, Set_Value_Name2_C, "LLVMSetValueName2");

  --*
  -- * Dump a representation of a value to stderr.
  -- *
  -- * @see llvm::Value::dump()
  --  

   procedure Dump_Value (Val : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:1640
   pragma Import (C, Dump_Value, "LLVMDumpValue");

  --*
  -- * Return a string representation of the value. Use
  -- * LLVMDisposeMessage to free the string.
  -- *
  -- * @see llvm::Value::print()
  --  

   function Print_Value_To_String
     (Val : LLVM.Types.Value_T)
      return String;
   function Print_Value_To_String_C
     (Val : LLVM.Types.Value_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1648
   pragma Import (C, Print_Value_To_String_C, "LLVMPrintValueToString");

  --*
  -- * Replace all uses of a value with another one.
  -- *
  -- * @see llvm::Value::replaceAllUsesWith()
  --  

   procedure Replace_All_Uses_With (Old_Val : LLVM.Types.Value_T; New_Val : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:1655
   pragma Import (C, Replace_All_Uses_With, "LLVMReplaceAllUsesWith");

  --*
  -- * Determine whether the specified value instance is constant.
  --  

   function Is_Constant
     (Val : LLVM.Types.Value_T)
      return Boolean;
   function Is_Constant_C
     (Val : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1660
   pragma Import (C, Is_Constant_C, "LLVMIsConstant");

  --*
  -- * Determine whether a value instance is undefined.
  --  

   function Is_Undef
     (Val : LLVM.Types.Value_T)
      return Boolean;
   function Is_Undef_C
     (Val : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1665
   pragma Import (C, Is_Undef_C, "LLVMIsUndef");

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

   function Is_AVA_Arg_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_AVA_Arg_Inst, "LLVMIsAVAArgInst");

   function Is_A_Load_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Load_Inst, "LLVMIsALoadInst");

   function Is_A_Extract_Value_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Extract_Value_Inst, "LLVMIsAExtractValueInst");

   function Is_AZ_Ext_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_AZ_Ext_Inst, "LLVMIsAZExtInst");

   function Is_AUI_To_FP_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_AUI_To_FP_Inst, "LLVMIsAUIToFPInst");

   function Is_A_Trunc_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Trunc_Inst, "LLVMIsATruncInst");

   function Is_ASI_To_FP_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_ASI_To_FP_Inst, "LLVMIsASIToFPInst");

   function Is_AS_Ext_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_AS_Ext_Inst, "LLVMIsASExtInst");

   function Is_A_Ptr_To_Int_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Ptr_To_Int_Inst, "LLVMIsAPtrToIntInst");

   function Is_A_Int_To_Ptr_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Int_To_Ptr_Inst, "LLVMIsAIntToPtrInst");

   function Is_AFP_Trunc_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_AFP_Trunc_Inst, "LLVMIsAFPTruncInst");

   function Is_AFP_To_UI_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_AFP_To_UI_Inst, "LLVMIsAFPToUIInst");

   function Is_AFP_To_SI_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_AFP_To_SI_Inst, "LLVMIsAFPToSIInst");

   function Is_AFP_Ext_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_AFP_Ext_Inst, "LLVMIsAFPExtInst");

   function Is_A_Bit_Cast_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Bit_Cast_Inst, "LLVMIsABitCastInst");

   function Is_A_Addr_Space_Cast_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Addr_Space_Cast_Inst, "LLVMIsAAddrSpaceCastInst");

   function Is_A_Cast_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Cast_Inst, "LLVMIsACastInst");

   function Is_A_Alloca_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Alloca_Inst, "LLVMIsAAllocaInst");

   function Is_A_Unary_Instruction (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Unary_Instruction, "LLVMIsAUnaryInstruction");

   function Is_A_Cleanup_Pad_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Cleanup_Pad_Inst, "LLVMIsACleanupPadInst");

   function Is_A_Catch_Pad_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Catch_Pad_Inst, "LLVMIsACatchPadInst");

   function Is_A_Funclet_Pad_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Funclet_Pad_Inst, "LLVMIsAFuncletPadInst");

   function Is_A_Catch_Return_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Catch_Return_Inst, "LLVMIsACatchReturnInst");

   function Is_A_Cleanup_Return_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Cleanup_Return_Inst, "LLVMIsACleanupReturnInst");

   function Is_A_Resume_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Resume_Inst, "LLVMIsAResumeInst");

   function Is_A_Unreachable_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Unreachable_Inst, "LLVMIsAUnreachableInst");

   function Is_A_Switch_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Switch_Inst, "LLVMIsASwitchInst");

   function Is_A_Return_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Return_Inst, "LLVMIsAReturnInst");

   function Is_A_Invoke_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Invoke_Inst, "LLVMIsAInvokeInst");

   function Is_A_Indirect_Br_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Indirect_Br_Inst, "LLVMIsAIndirectBrInst");

   function Is_A_Branch_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Branch_Inst, "LLVMIsABranchInst");

   function Is_A_Store_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Store_Inst, "LLVMIsAStoreInst");

   function Is_A_Shuffle_Vector_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Shuffle_Vector_Inst, "LLVMIsAShuffleVectorInst");

   function Is_A_Select_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Select_Inst, "LLVMIsASelectInst");

   function Is_APHI_Node (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_APHI_Node, "LLVMIsAPHINode");

   function Is_A_Landing_Pad_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Landing_Pad_Inst, "LLVMIsALandingPadInst");

   function Is_A_Insert_Value_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Insert_Value_Inst, "LLVMIsAInsertValueInst");

   function Is_A_Insert_Element_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Insert_Element_Inst, "LLVMIsAInsertElementInst");

   function Is_A_Get_Element_Ptr_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Get_Element_Ptr_Inst, "LLVMIsAGetElementPtrInst");

   function Is_A_Extract_Element_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Extract_Element_Inst, "LLVMIsAExtractElementInst");

   function Is_AI_Cmp_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_AI_Cmp_Inst, "LLVMIsAICmpInst");

   function Is_AF_Cmp_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_AF_Cmp_Inst, "LLVMIsAFCmpInst");

   function Is_A_Cmp_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Cmp_Inst, "LLVMIsACmpInst");

   function Is_A_Mem_Set_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Mem_Set_Inst, "LLVMIsAMemSetInst");

   function Is_A_Mem_Move_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Mem_Move_Inst, "LLVMIsAMemMoveInst");

   function Is_A_Mem_Cpy_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Mem_Cpy_Inst, "LLVMIsAMemCpyInst");

   function Is_A_Mem_Intrinsic (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Mem_Intrinsic, "LLVMIsAMemIntrinsic");

   function Is_A_Dbg_Label_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Dbg_Label_Inst, "LLVMIsADbgLabelInst");

   function Is_A_Dbg_Declare_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Dbg_Declare_Inst, "LLVMIsADbgDeclareInst");

   function Is_A_Dbg_Variable_Intrinsic (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Dbg_Variable_Intrinsic, "LLVMIsADbgVariableIntrinsic");

   function Is_A_Dbg_Info_Intrinsic (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Dbg_Info_Intrinsic, "LLVMIsADbgInfoIntrinsic");

   function Is_A_Intrinsic_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Intrinsic_Inst, "LLVMIsAIntrinsicInst");

   function Is_A_Call_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Call_Inst, "LLVMIsACallInst");

   function Is_A_Binary_Operator (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Binary_Operator, "LLVMIsABinaryOperator");

   function Is_A_Instruction (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Instruction, "LLVMIsAInstruction");

   function Is_A_Undef_Value (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Undef_Value, "LLVMIsAUndefValue");

   function Is_A_Global_Variable (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Global_Variable, "LLVMIsAGlobalVariable");

   function Is_A_Function (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Function, "LLVMIsAFunction");

   function Is_A_Global_Object (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Global_Object, "LLVMIsAGlobalObject");

   function Is_A_Global_I_Func (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Global_I_Func, "LLVMIsAGlobalIFunc");

   function Is_A_Global_Alias (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Global_Alias, "LLVMIsAGlobalAlias");

   function Is_A_Global_Value (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Global_Value, "LLVMIsAGlobalValue");

   function Is_A_Constant_Vector (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Constant_Vector, "LLVMIsAConstantVector");

   function Is_A_Constant_Token_None (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Constant_Token_None, "LLVMIsAConstantTokenNone");

   function Is_A_Constant_Struct (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Constant_Struct, "LLVMIsAConstantStruct");

   function Is_A_Constant_Pointer_Null (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Constant_Pointer_Null, "LLVMIsAConstantPointerNull");

   function Is_A_Constant_Int (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Constant_Int, "LLVMIsAConstantInt");

   function Is_A_Constant_FP (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Constant_FP, "LLVMIsAConstantFP");

   function Is_A_Constant_Expr (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Constant_Expr, "LLVMIsAConstantExpr");

   function Is_A_Constant_Data_Vector (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Constant_Data_Vector, "LLVMIsAConstantDataVector");

   function Is_A_Constant_Data_Array (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Constant_Data_Array, "LLVMIsAConstantDataArray");

   function Is_A_Constant_Data_Sequential (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Constant_Data_Sequential, "LLVMIsAConstantDataSequential");

   function Is_A_Constant_Array (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Constant_Array, "LLVMIsAConstantArray");

   function Is_A_Constant_Aggregate_Zero (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Constant_Aggregate_Zero, "LLVMIsAConstantAggregateZero");

   function Is_A_Block_Address (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Block_Address, "LLVMIsABlockAddress");

   function Is_A_Constant (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Constant, "LLVMIsAConstant");

   function Is_A_User (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_User, "LLVMIsAUser");

   function Is_A_Inline_Asm (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Inline_Asm, "LLVMIsAInlineAsm");

   function Is_A_Basic_Block (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Basic_Block, "LLVMIsABasicBlock");

   function Is_A_Argument (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1680
   pragma Import (C, Is_A_Argument, "LLVMIsAArgument");

   function Is_AMD_Node (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1682
   pragma Import (C, Is_AMD_Node, "LLVMIsAMDNode");

   function Is_AMD_String (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1683
   pragma Import (C, Is_AMD_String, "LLVMIsAMDString");

  --* Deprecated: Use LLVMGetValueName2 instead.  
   function Get_Value_Name
     (Val : LLVM.Types.Value_T)
      return String;
   function Get_Value_Name_C
     (Val : LLVM.Types.Value_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1686
   pragma Import (C, Get_Value_Name_C, "LLVMGetValueName");

  --* Deprecated: Use LLVMSetValueName2 instead.  
   procedure Set_Value_Name
     (Val  : LLVM.Types.Value_T;
      Name : String);
   procedure Set_Value_Name_C
     (Val  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr);  -- llvm-9.0.0.src/include/llvm-c/Core.h:1688
   pragma Import (C, Set_Value_Name_C, "LLVMSetValueName");

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

   function Get_First_Use (Val : LLVM.Types.Value_T) return LLVM.Types.Use_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1717
   pragma Import (C, Get_First_Use, "LLVMGetFirstUse");

  --*
  -- * Obtain the next use of a value.
  -- *
  -- * This effectively advances the iterator. It returns NULL if you are on
  -- * the final use and no more are available.
  --  

   function Get_Next_Use (U : LLVM.Types.Use_T) return LLVM.Types.Use_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1725
   pragma Import (C, Get_Next_Use, "LLVMGetNextUse");

  --*
  -- * Obtain the user value for a user.
  -- *
  -- * The returned value corresponds to a llvm::User type.
  -- *
  -- * @see llvm::Use::getUser()
  --  

   function Get_User (U : LLVM.Types.Use_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1734
   pragma Import (C, Get_User, "LLVMGetUser");

  --*
  -- * Obtain the value this use corresponds to.
  -- *
  -- * @see llvm::Use::get().
  --  

   function Get_Used_Value (U : LLVM.Types.Use_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1741
   pragma Import (C, Get_Used_Value, "LLVMGetUsedValue");

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

   function Get_Operand (Val : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1762
   pragma Import (C, Get_Operand, "LLVMGetOperand");

  --*
  -- * Obtain the use of an operand at a specific index in a llvm::User value.
  -- *
  -- * @see llvm::User::getOperandUse()
  --  

   function Get_Operand_Use (Val : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Use_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1769
   pragma Import (C, Get_Operand_Use, "LLVMGetOperandUse");

  --*
  -- * Set an operand at a specific index in a llvm::User value.
  -- *
  -- * @see llvm::User::setOperand()
  --  

   procedure Set_Operand
     (User : LLVM.Types.Value_T;
      Index : unsigned;
      Val : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:1776
   pragma Import (C, Set_Operand, "LLVMSetOperand");

  --*
  -- * Obtain the number of operands in a llvm::User value.
  -- *
  -- * @see llvm::User::getNumOperands()
  --  

   function Get_Num_Operands (Val : LLVM.Types.Value_T) return int;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1783
   pragma Import (C, Get_Num_Operands, "LLVMGetNumOperands");

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
   function Const_Null (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1806
   pragma Import (C, Const_Null, "LLVMConstNull");

  --*
  -- * Obtain a constant value referring to the instance of a type
  -- * consisting of all ones.
  -- *
  -- * This is only valid for integer types.
  -- *
  -- * @see llvm::Constant::getAllOnesValue()
  --  

   function Const_All_Ones (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1816
   pragma Import (C, Const_All_Ones, "LLVMConstAllOnes");

  --*
  -- * Obtain a constant value referring to an undefined value of a type.
  -- *
  -- * @see llvm::UndefValue::get()
  --  

   function Get_Undef (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1823
   pragma Import (C, Get_Undef, "LLVMGetUndef");

  --*
  -- * Determine whether a value instance is null.
  -- *
  -- * @see llvm::Constant::isNullValue()
  --  

   function Is_Null
     (Val : LLVM.Types.Value_T)
      return Boolean;
   function Is_Null_C
     (Val : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1830
   pragma Import (C, Is_Null_C, "LLVMIsNull");

  --*
  -- * Obtain a constant that is a constant pointer pointing to NULL for a
  -- * specified type.
  --  

   function Const_Pointer_Null (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1836
   pragma Import (C, Const_Pointer_Null, "LLVMConstPointerNull");

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
   function Const_Int_C
     (Int_Ty      : LLVM.Types.Type_T;
      N           : Extensions.unsigned_long_long;
      Sign_Extend : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T;
   pragma Import (C, Const_Int_C, "LLVMConstInt");

  --*
  -- * Obtain a constant value for an integer of arbitrary precision.
  -- *
  -- * @see llvm::ConstantInt::get()
  --  

   function Const_Int_Of_Arbitrary_Precision
     (Int_Ty : LLVM.Types.Type_T;
      Num_Words : unsigned;
      Words : access stdint_h.uint64_t) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1873
   pragma Import (C, Const_Int_Of_Arbitrary_Precision, "LLVMConstIntOfArbitraryPrecision");

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
   function Const_Int_Of_String_C
     (Int_Ty : LLVM.Types.Type_T;
      Text   : Interfaces.C.Strings.chars_ptr;
      Radix  : stdint_h.uint8_t)
      return LLVM.Types.Value_T;
   pragma Import (C, Const_Int_Of_String_C, "LLVMConstIntOfString");

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
   function Const_Int_Of_String_And_Size_C
     (Int_Ty : LLVM.Types.Type_T;
      Text   : Interfaces.C.Strings.chars_ptr;
      S_Len  : unsigned;
      Radix  : stdint_h.uint8_t)
      return LLVM.Types.Value_T;
   pragma Import (C, Const_Int_Of_String_And_Size_C, "LLVMConstIntOfStringAndSize");

  --*
  -- * Obtain a constant value referring to a double floating point value.
  --  

   function Const_Real (Real_Ty : LLVM.Types.Type_T; N : double) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1901
   pragma Import (C, Const_Real, "LLVMConstReal");

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
   function Const_Real_Of_String_C
     (Real_Ty : LLVM.Types.Type_T;
      Text    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1909
   pragma Import (C, Const_Real_Of_String_C, "LLVMConstRealOfString");

  --*
  -- * Obtain a constant for a floating point value parsed from a string.
  --  

function Const_Real_Of_String_And_Size
     (Real_Ty : LLVM.Types.Type_T;
      Text    : String;
      S_Len   : unsigned)
      return LLVM.Types.Value_T;
   function Const_Real_Of_String_And_Size_C
     (Real_Ty : LLVM.Types.Type_T;
      Text    : Interfaces.C.Strings.chars_ptr;
      S_Len   : unsigned)
      return LLVM.Types.Value_T;
   pragma Import (C, Const_Real_Of_String_And_Size_C, "LLVMConstRealOfStringAndSize");

  --*
  -- * Obtain the zero extended value for an integer constant value.
  -- *
  -- * @see llvm::ConstantInt::getZExtValue()
  --  

   function Const_Int_Get_Z_Ext_Value (Constant_Val : LLVM.Types.Value_T) return Extensions.unsigned_long_long;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1922
   pragma Import (C, Const_Int_Get_Z_Ext_Value, "LLVMConstIntGetZExtValue");

  --*
  -- * Obtain the sign extended value for an integer constant value.
  -- *
  -- * @see llvm::ConstantInt::getSExtValue()
  --  

   function Const_Int_Get_S_Ext_Value (Constant_Val : LLVM.Types.Value_T) return Long_Long_Integer;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1929
   pragma Import (C, Const_Int_Get_S_Ext_Value, "LLVMConstIntGetSExtValue");

  --*
  -- * Obtain the double value for an floating point constant value.
  -- * losesInfo indicates if some precision was lost in the conversion.
  -- *
  -- * @see llvm::ConstantFP::getDoubleValue
  --  

   function Const_Real_Get_Double
     (Constant_Val : LLVM.Types.Value_T;
      loses_Info   : Boolean)
      return double;
   function Const_Real_Get_Double_C
     (Constant_Val : LLVM.Types.Value_T;
      loses_Info   : LLVM.Types.Bool_T)
      return double;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1937
   pragma Import (C, Const_Real_Get_Double_C, "LLVMConstRealGetDouble");

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
   function Const_String_In_Context_C
     (C                   : LLVM.Types.Context_T;
      Str                 : Interfaces.C.Strings.chars_ptr;
      Length              : unsigned;
      Dont_Null_Terminate : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T;
   pragma Import (C, Const_String_In_Context_C, "LLVMConstStringInContext");

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
   function Const_String_C
     (Str                 : Interfaces.C.Strings.chars_ptr;
      Length              : unsigned;
      Dont_Null_Terminate : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T;
   pragma Import (C, Const_String_C, "LLVMConstString");

  --*
  -- * Returns true if the specified constant is an array of i8.
  -- *
  -- * @see ConstantDataSequential::getAsString()
  --  

   function Is_Constant_String
     (c : LLVM.Types.Value_T)
      return Boolean;
   function Is_Constant_String_C
     (c : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1976
   pragma Import (C, Is_Constant_String_C, "LLVMIsConstantString");

  --*
  -- * Get the given constant data sequential as a string.
  -- *
  -- * @see ConstantDataSequential::getAsString()
  --  

   function Get_As_String
     (c      : LLVM.Types.Value_T;
      Length : stddef_h.size_t)
      return String;
   function Get_As_String_C
     (c      : LLVM.Types.Value_T;
      Length : stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:1983
   pragma Import (C, Get_As_String_C, "LLVMGetAsString");

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
   function Const_Struct_In_Context_C
     (C             : LLVM.Types.Context_T;
      Constant_Vals : System.Address;
      Count         : unsigned;
      Packed        : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T;
   pragma Import (C, Const_Struct_In_Context_C, "LLVMConstStructInContext");

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
   function Const_Struct_C
     (Constant_Vals : System.Address;
      Count         : unsigned;
      Packed        : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T;
   pragma Import (C, Const_Struct_C, "LLVMConstStruct");

  --*
  -- * Create a ConstantArray from values.
  -- *
  -- * @see llvm::ConstantArray::get()
  --  

   function Const_Array
     (Element_Ty : LLVM.Types.Type_T;
      Constant_Vals : System.Address;
      Length : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2010
   pragma Import (C, Const_Array, "LLVMConstArray");

  --*
  -- * Create a non-anonymous ConstantStruct from values.
  -- *
  -- * @see llvm::ConstantStruct::get()
  --  

   function Const_Named_Struct
     (Struct_Ty : LLVM.Types.Type_T;
      Constant_Vals : System.Address;
      Count : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2018
   pragma Import (C, Const_Named_Struct, "LLVMConstNamedStruct");

  --*
  -- * Get an element at specified index as a constant.
  -- *
  -- * @see ConstantDataSequential::getElementAsConstant()
  --  

   function Get_Element_As_Constant (C : LLVM.Types.Value_T; idx : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2027
   pragma Import (C, Get_Element_As_Constant, "LLVMGetElementAsConstant");

  --*
  -- * Create a ConstantVector from values.
  -- *
  -- * @see llvm::ConstantVector::get()
  --  

   function Const_Vector (Scalar_Constant_Vals : System.Address; Size : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2034
   pragma Import (C, Const_Vector, "LLVMConstVector");

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

   function Get_Const_Opcode (Constant_Val : LLVM.Types.Value_T) return Opcode_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2049
   pragma Import (C, Get_Const_Opcode, "LLVMGetConstOpcode");

   function Align_Of (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2050
   pragma Import (C, Align_Of, "LLVMAlignOf");

   function Size_Of (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2051
   pragma Import (C, Size_Of, "LLVMSizeOf");

   function Const_Neg (Constant_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2052
   pragma Import (C, Const_Neg, "LLVMConstNeg");

   function Const_NSW_Neg (Constant_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2053
   pragma Import (C, Const_NSW_Neg, "LLVMConstNSWNeg");

   function Const_NUW_Neg (Constant_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2054
   pragma Import (C, Const_NUW_Neg, "LLVMConstNUWNeg");

   function Const_F_Neg (Constant_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2055
   pragma Import (C, Const_F_Neg, "LLVMConstFNeg");

   function Const_Not (Constant_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2056
   pragma Import (C, Const_Not, "LLVMConstNot");

   function Const_Add (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2057
   pragma Import (C, Const_Add, "LLVMConstAdd");

   function Const_NSW_Add (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2058
   pragma Import (C, Const_NSW_Add, "LLVMConstNSWAdd");

   function Const_NUW_Add (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2059
   pragma Import (C, Const_NUW_Add, "LLVMConstNUWAdd");

   function Const_F_Add (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2060
   pragma Import (C, Const_F_Add, "LLVMConstFAdd");

   function Const_Sub (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2061
   pragma Import (C, Const_Sub, "LLVMConstSub");

   function Const_NSW_Sub (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2062
   pragma Import (C, Const_NSW_Sub, "LLVMConstNSWSub");

   function Const_NUW_Sub (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2063
   pragma Import (C, Const_NUW_Sub, "LLVMConstNUWSub");

   function Const_F_Sub (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2064
   pragma Import (C, Const_F_Sub, "LLVMConstFSub");

   function Const_Mul (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2065
   pragma Import (C, Const_Mul, "LLVMConstMul");

   function Const_NSW_Mul (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2066
   pragma Import (C, Const_NSW_Mul, "LLVMConstNSWMul");

   function Const_NUW_Mul (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2067
   pragma Import (C, Const_NUW_Mul, "LLVMConstNUWMul");

   function Const_F_Mul (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2068
   pragma Import (C, Const_F_Mul, "LLVMConstFMul");

   function Const_U_Div (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2069
   pragma Import (C, Const_U_Div, "LLVMConstUDiv");

   function Const_Exact_U_Div (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2070
   pragma Import (C, Const_Exact_U_Div, "LLVMConstExactUDiv");

   function Const_S_Div (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2071
   pragma Import (C, Const_S_Div, "LLVMConstSDiv");

   function Const_Exact_S_Div (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2072
   pragma Import (C, Const_Exact_S_Div, "LLVMConstExactSDiv");

   function Const_F_Div (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2073
   pragma Import (C, Const_F_Div, "LLVMConstFDiv");

   function Const_U_Rem (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2074
   pragma Import (C, Const_U_Rem, "LLVMConstURem");

   function Const_S_Rem (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2075
   pragma Import (C, Const_S_Rem, "LLVMConstSRem");

   function Const_F_Rem (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2076
   pragma Import (C, Const_F_Rem, "LLVMConstFRem");

   function Const_And (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2077
   pragma Import (C, Const_And, "LLVMConstAnd");

   function Const_Or (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2078
   pragma Import (C, Const_Or, "LLVMConstOr");

   function Const_Xor (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2079
   pragma Import (C, Const_Xor, "LLVMConstXor");

   function Const_I_Cmp
     (Predicate : Int_Predicate_T;
      LHS_Constant : LLVM.Types.Value_T;
      RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2080
   pragma Import (C, Const_I_Cmp, "LLVMConstICmp");

   function Const_F_Cmp
     (Predicate : Real_Predicate_T;
      LHS_Constant : LLVM.Types.Value_T;
      RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2082
   pragma Import (C, Const_F_Cmp, "LLVMConstFCmp");

   function Const_Shl (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2084
   pragma Import (C, Const_Shl, "LLVMConstShl");

   function Const_L_Shr (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2085
   pragma Import (C, Const_L_Shr, "LLVMConstLShr");

   function Const_A_Shr (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2086
   pragma Import (C, Const_A_Shr, "LLVMConstAShr");

   function Const_GEP
     (Constant_Val : LLVM.Types.Value_T;
      Constant_Indices : System.Address;
      Num_Indices : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2087
   pragma Import (C, Const_GEP, "LLVMConstGEP");

   function Const_GE_P2
     (Ty : LLVM.Types.Type_T;
      Constant_Val : LLVM.Types.Value_T;
      Constant_Indices : System.Address;
      Num_Indices : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2089
   pragma Import (C, Const_GE_P2, "LLVMConstGEP2");

   function Const_In_Bounds_GEP
     (Constant_Val : LLVM.Types.Value_T;
      Constant_Indices : System.Address;
      Num_Indices : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2091
   pragma Import (C, Const_In_Bounds_GEP, "LLVMConstInBoundsGEP");

   function Const_In_Bounds_GE_P2
     (Ty : LLVM.Types.Type_T;
      Constant_Val : LLVM.Types.Value_T;
      Constant_Indices : System.Address;
      Num_Indices : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2094
   pragma Import (C, Const_In_Bounds_GE_P2, "LLVMConstInBoundsGEP2");

   function Const_Trunc (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2097
   pragma Import (C, Const_Trunc, "LLVMConstTrunc");

   function Const_S_Ext (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2098
   pragma Import (C, Const_S_Ext, "LLVMConstSExt");

   function Const_Z_Ext (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2099
   pragma Import (C, Const_Z_Ext, "LLVMConstZExt");

   function Const_FP_Trunc (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2100
   pragma Import (C, Const_FP_Trunc, "LLVMConstFPTrunc");

   function Const_FP_Ext (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2101
   pragma Import (C, Const_FP_Ext, "LLVMConstFPExt");

   function Const_UI_To_FP (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2102
   pragma Import (C, Const_UI_To_FP, "LLVMConstUIToFP");

   function Const_SI_To_FP (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2103
   pragma Import (C, Const_SI_To_FP, "LLVMConstSIToFP");

   function Const_FP_To_UI (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2104
   pragma Import (C, Const_FP_To_UI, "LLVMConstFPToUI");

   function Const_FP_To_SI (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2105
   pragma Import (C, Const_FP_To_SI, "LLVMConstFPToSI");

   function Const_Ptr_To_Int (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2106
   pragma Import (C, Const_Ptr_To_Int, "LLVMConstPtrToInt");

   function Const_Int_To_Ptr (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2107
   pragma Import (C, Const_Int_To_Ptr, "LLVMConstIntToPtr");

   function Const_Bit_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2108
   pragma Import (C, Const_Bit_Cast, "LLVMConstBitCast");

   function Const_Addr_Space_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2109
   pragma Import (C, Const_Addr_Space_Cast, "LLVMConstAddrSpaceCast");

   function Const_Z_Ext_Or_Bit_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2110
   pragma Import (C, Const_Z_Ext_Or_Bit_Cast, "LLVMConstZExtOrBitCast");

   function Const_S_Ext_Or_Bit_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2112
   pragma Import (C, Const_S_Ext_Or_Bit_Cast, "LLVMConstSExtOrBitCast");

   function Const_Trunc_Or_Bit_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2114
   pragma Import (C, Const_Trunc_Or_Bit_Cast, "LLVMConstTruncOrBitCast");

   function Const_Pointer_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2116
   pragma Import (C, Const_Pointer_Cast, "LLVMConstPointerCast");

function Const_Int_Cast
     (Constant_Val : LLVM.Types.Value_T;
      To_Type      : LLVM.Types.Type_T;
      is_Signed    : Boolean)
      return LLVM.Types.Value_T;
   function Const_Int_Cast_C
     (Constant_Val : LLVM.Types.Value_T;
      To_Type      : LLVM.Types.Type_T;
      is_Signed    : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T;
   pragma Import (C, Const_Int_Cast_C, "LLVMConstIntCast");

   function Const_FP_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2120
   pragma Import (C, Const_FP_Cast, "LLVMConstFPCast");

   function Const_Select
     (Constant_Condition : LLVM.Types.Value_T;
      Constant_If_True : LLVM.Types.Value_T;
      Constant_If_False : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2121
   pragma Import (C, Const_Select, "LLVMConstSelect");

   function Const_Extract_Element (Vector_Constant : LLVM.Types.Value_T; Index_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2124
   pragma Import (C, Const_Extract_Element, "LLVMConstExtractElement");

   function Const_Insert_Element
     (Vector_Constant : LLVM.Types.Value_T;
      Element_Value_Constant : LLVM.Types.Value_T;
      Index_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2126
   pragma Import (C, Const_Insert_Element, "LLVMConstInsertElement");

   function Const_Shuffle_Vector
     (Vector_A_Constant : LLVM.Types.Value_T;
      Vector_B_Constant : LLVM.Types.Value_T;
      Mask_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2129
   pragma Import (C, Const_Shuffle_Vector, "LLVMConstShuffleVector");

   function Const_Extract_Value
     (Agg_Constant : LLVM.Types.Value_T;
      Idx_List : access unsigned;
      Num_Idx : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2132
   pragma Import (C, Const_Extract_Value, "LLVMConstExtractValue");

   function Const_Insert_Value
     (Agg_Constant : LLVM.Types.Value_T;
      Element_Value_Constant : LLVM.Types.Value_T;
      Idx_List : access unsigned;
      Num_Idx : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2134
   pragma Import (C, Const_Insert_Value, "LLVMConstInsertValue");

   function Block_Address (F : LLVM.Types.Value_T; BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2137
   pragma Import (C, Block_Address, "LLVMBlockAddress");

  --* Deprecated: Use LLVMGetInlineAsm instead.  
function Const_Inline_Asm
     (Ty               : LLVM.Types.Type_T;
      Asm_String       : String;
      Constraints      : String;
      Has_Side_Effects : Boolean;
      Is_Align_Stack   : Boolean)
      return LLVM.Types.Value_T;
   function Const_Inline_Asm_C
     (Ty               : LLVM.Types.Type_T;
      Asm_String       : Interfaces.C.Strings.chars_ptr;
      Constraints      : Interfaces.C.Strings.chars_ptr;
      Has_Side_Effects : LLVM.Types.Bool_T;
      Is_Align_Stack   : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T;
   pragma Import (C, Const_Inline_Asm_C, "LLVMConstInlineAsm");

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

   function Get_Global_Parent (Global : LLVM.Types.Value_T) return LLVM.Types.Module_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2159
   pragma Import (C, Get_Global_Parent, "LLVMGetGlobalParent");

   function Is_Declaration
     (Global : LLVM.Types.Value_T)
      return Boolean;
   function Is_Declaration_C
     (Global : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2160
   pragma Import (C, Is_Declaration_C, "LLVMIsDeclaration");

   function Get_Linkage (Global : LLVM.Types.Value_T) return Linkage_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2161
   pragma Import (C, Get_Linkage, "LLVMGetLinkage");

   procedure Set_Linkage (Global : LLVM.Types.Value_T; Linkage : Linkage_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2162
   pragma Import (C, Set_Linkage, "LLVMSetLinkage");

   function Get_Section
     (Global : LLVM.Types.Value_T)
      return String;
   function Get_Section_C
     (Global : LLVM.Types.Value_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2163
   pragma Import (C, Get_Section_C, "LLVMGetSection");

   procedure Set_Section
     (Global  : LLVM.Types.Value_T;
      Section : String);
   procedure Set_Section_C
     (Global  : LLVM.Types.Value_T;
      Section : Interfaces.C.Strings.chars_ptr);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2164
   pragma Import (C, Set_Section_C, "LLVMSetSection");

   function Get_Visibility (Global : LLVM.Types.Value_T) return Visibility_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2165
   pragma Import (C, Get_Visibility, "LLVMGetVisibility");

   procedure Set_Visibility (Global : LLVM.Types.Value_T; Viz : Visibility_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2166
   pragma Import (C, Set_Visibility, "LLVMSetVisibility");

   function Get_DLL_Storage_Class (Global : LLVM.Types.Value_T) return DLL_Storage_Class_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2167
   pragma Import (C, Get_DLL_Storage_Class, "LLVMGetDLLStorageClass");

   procedure Set_DLL_Storage_Class (Global : LLVM.Types.Value_T; Class : DLL_Storage_Class_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2168
   pragma Import (C, Set_DLL_Storage_Class, "LLVMSetDLLStorageClass");

   function Get_Unnamed_Address (Global : LLVM.Types.Value_T) return Unnamed_Addr_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2169
   pragma Import (C, Get_Unnamed_Address, "LLVMGetUnnamedAddress");

   procedure Set_Unnamed_Address (Global : LLVM.Types.Value_T; Unnamed_Addr : Unnamed_Addr_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2170
   pragma Import (C, Set_Unnamed_Address, "LLVMSetUnnamedAddress");

  --*
  -- * Returns the "value type" of a global value.  This differs from the formal
  -- * type of a global value which is always a pointer type.
  -- *
  -- * @see llvm::GlobalValue::getValueType()
  --  

   function Global_Get_Value_Type (Global : LLVM.Types.Value_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2178
   pragma Import (C, Global_Get_Value_Type, "LLVMGlobalGetValueType");

  --* Deprecated: Use LLVMGetUnnamedAddress instead.  
   function Has_Unnamed_Addr
     (Global : LLVM.Types.Value_T)
      return Boolean;
   function Has_Unnamed_Addr_C
     (Global : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2181
   pragma Import (C, Has_Unnamed_Addr_C, "LLVMHasUnnamedAddr");

  --* Deprecated: Use LLVMSetUnnamedAddress instead.  
   procedure Set_Unnamed_Addr
     (Global           : LLVM.Types.Value_T;
      Has_Unnamed_Addr : Boolean);
   procedure Set_Unnamed_Addr_C
     (Global           : LLVM.Types.Value_T;
      Has_Unnamed_Addr : LLVM.Types.Bool_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2183
   pragma Import (C, Set_Unnamed_Addr_C, "LLVMSetUnnamedAddr");

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
  -- * @see llvm::GlobalValue::getAlignment()
  --  

   function Get_Alignment (V : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2199
   pragma Import (C, Get_Alignment, "LLVMGetAlignment");

  --*
  -- * Set the preferred alignment of the value.
  -- * @see llvm::AllocaInst::setAlignment()
  -- * @see llvm::LoadInst::setAlignment()
  -- * @see llvm::StoreInst::setAlignment()
  -- * @see llvm::GlobalValue::setAlignment()
  --  

   procedure Set_Alignment (V : LLVM.Types.Value_T; Bytes : unsigned);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2208
   pragma Import (C, Set_Alignment, "LLVMSetAlignment");

  --*
  -- * Sets a metadata attachment, erasing the existing metadata attachment if
  -- * it already exists for the given kind.
  -- *
  -- * @see llvm::GlobalObject::setMetadata()
  --  

   procedure Global_Set_Metadata
     (Global : LLVM.Types.Value_T;
      Kind : unsigned;
      MD : LLVM.Types.Metadata_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2216
   pragma Import (C, Global_Set_Metadata, "LLVMGlobalSetMetadata");

  --*
  -- * Erases a metadata attachment of the given kind if it exists.
  -- *
  -- * @see llvm::GlobalObject::eraseMetadata()
  --  

   procedure Global_Erase_Metadata (Global : LLVM.Types.Value_T; Kind : unsigned);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2224
   pragma Import (C, Global_Erase_Metadata, "LLVMGlobalEraseMetadata");

  --*
  -- * Removes all metadata attachments from this value.
  -- *
  -- * @see llvm::GlobalObject::clearMetadata()
  --  

   procedure Global_Clear_Metadata (Global : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2231
   pragma Import (C, Global_Clear_Metadata, "LLVMGlobalClearMetadata");

  --*
  -- * Retrieves an array of metadata entries representing the metadata attached to
  -- * this value. The caller is responsible for freeing this array by calling
  -- * \c LLVMDisposeValueMetadataEntries.
  -- *
  -- * @see llvm::GlobalObject::getAllMetadata()
  --  

   function Global_Copy_All_Metadata (Value : LLVM.Types.Value_T; Num_Entries : access stddef_h.size_t) return System.Address;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2240
   pragma Import (C, Global_Copy_All_Metadata, "LLVMGlobalCopyAllMetadata");

  --*
  -- * Destroys value metadata entries.
  --  

   procedure Dispose_Value_Metadata_Entries (Entries : System.Address);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2246
   pragma Import (C, Dispose_Value_Metadata_Entries, "LLVMDisposeValueMetadataEntries");

  --*
  -- * Returns the kind of a value metadata entry at a specific index.
  --  

   function Value_Metadata_Entries_Get_Kind (Entries : System.Address; Index : unsigned) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2251
   pragma Import (C, Value_Metadata_Entries_Get_Kind, "LLVMValueMetadataEntriesGetKind");

  --*
  -- * Returns the underlying metadata node of a value metadata entry at a
  -- * specific index.
  --  

   function Value_Metadata_Entries_Get_Metadata (Entries : System.Address; Index : unsigned) return LLVM.Types.Metadata_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2259
   pragma Import (C, Value_Metadata_Entries_Get_Metadata, "LLVMValueMetadataEntriesGetMetadata");

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
   function Add_Global_C
     (M    : LLVM.Types.Module_T;
      Ty   : LLVM.Types.Type_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Add_Global_C, "LLVMAddGlobal");

function Add_Global_In_Address_Space
     (M             : LLVM.Types.Module_T;
      Ty            : LLVM.Types.Type_T;
      Name          : String;
      Address_Space : unsigned)
      return LLVM.Types.Value_T;
   function Add_Global_In_Address_Space_C
     (M             : LLVM.Types.Module_T;
      Ty            : LLVM.Types.Type_T;
      Name          : Interfaces.C.Strings.chars_ptr;
      Address_Space : unsigned)
      return LLVM.Types.Value_T;
   pragma Import (C, Add_Global_In_Address_Space_C, "LLVMAddGlobalInAddressSpace");

   function Get_Named_Global
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Get_Named_Global_C
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2279
   pragma Import (C, Get_Named_Global_C, "LLVMGetNamedGlobal");

   function Get_First_Global (M : LLVM.Types.Module_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2280
   pragma Import (C, Get_First_Global, "LLVMGetFirstGlobal");

   function Get_Last_Global (M : LLVM.Types.Module_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2281
   pragma Import (C, Get_Last_Global, "LLVMGetLastGlobal");

   function Get_Next_Global (Global_Var : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2282
   pragma Import (C, Get_Next_Global, "LLVMGetNextGlobal");

   function Get_Previous_Global (Global_Var : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2283
   pragma Import (C, Get_Previous_Global, "LLVMGetPreviousGlobal");

   procedure Delete_Global (Global_Var : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2284
   pragma Import (C, Delete_Global, "LLVMDeleteGlobal");

   function Get_Initializer (Global_Var : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2285
   pragma Import (C, Get_Initializer, "LLVMGetInitializer");

   procedure Set_Initializer (Global_Var : LLVM.Types.Value_T; Constant_Val : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2286
   pragma Import (C, Set_Initializer, "LLVMSetInitializer");

   function Is_Thread_Local
     (Global_Var : LLVM.Types.Value_T)
      return Boolean;
   function Is_Thread_Local_C
     (Global_Var : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2287
   pragma Import (C, Is_Thread_Local_C, "LLVMIsThreadLocal");

   procedure Set_Thread_Local
     (Global_Var      : LLVM.Types.Value_T;
      Is_Thread_Local : Boolean);
   procedure Set_Thread_Local_C
     (Global_Var      : LLVM.Types.Value_T;
      Is_Thread_Local : LLVM.Types.Bool_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2288
   pragma Import (C, Set_Thread_Local_C, "LLVMSetThreadLocal");

   function Is_Global_Constant
     (Global_Var : LLVM.Types.Value_T)
      return Boolean;
   function Is_Global_Constant_C
     (Global_Var : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2289
   pragma Import (C, Is_Global_Constant_C, "LLVMIsGlobalConstant");

   procedure Set_Global_Constant
     (Global_Var  : LLVM.Types.Value_T;
      Is_Constant : Boolean);
   procedure Set_Global_Constant_C
     (Global_Var  : LLVM.Types.Value_T;
      Is_Constant : LLVM.Types.Bool_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2290
   pragma Import (C, Set_Global_Constant_C, "LLVMSetGlobalConstant");

   function Get_Thread_Local_Mode (Global_Var : LLVM.Types.Value_T) return Thread_Local_Mode_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2291
   pragma Import (C, Get_Thread_Local_Mode, "LLVMGetThreadLocalMode");

   procedure Set_Thread_Local_Mode (Global_Var : LLVM.Types.Value_T; Mode : Thread_Local_Mode_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2292
   pragma Import (C, Set_Thread_Local_Mode, "LLVMSetThreadLocalMode");

   function Is_Externally_Initialized
     (Global_Var : LLVM.Types.Value_T)
      return Boolean;
   function Is_Externally_Initialized_C
     (Global_Var : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2293
   pragma Import (C, Is_Externally_Initialized_C, "LLVMIsExternallyInitialized");

   procedure Set_Externally_Initialized
     (Global_Var  : LLVM.Types.Value_T;
      Is_Ext_Init : Boolean);
   procedure Set_Externally_Initialized_C
     (Global_Var  : LLVM.Types.Value_T;
      Is_Ext_Init : LLVM.Types.Bool_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2294
   pragma Import (C, Set_Externally_Initialized_C, "LLVMSetExternallyInitialized");

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

function Add_Alias
     (M       : LLVM.Types.Module_T;
      Ty      : LLVM.Types.Type_T;
      Aliasee : LLVM.Types.Value_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Add_Alias_C
     (M       : LLVM.Types.Module_T;
      Ty      : LLVM.Types.Type_T;
      Aliasee : LLVM.Types.Value_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Add_Alias_C, "LLVMAddAlias");

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
   function Get_Named_Global_Alias_C
     (M        : LLVM.Types.Module_T;
      Name     : Interfaces.C.Strings.chars_ptr;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Value_T;
   pragma Import (C, Get_Named_Global_Alias_C, "LLVMGetNamedGlobalAlias");

  --*
  -- * Obtain an iterator to the first GlobalAlias in a Module.
  -- *
  -- * @see llvm::Module::alias_begin()
  --  

   function Get_First_Global_Alias (M : LLVM.Types.Module_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2327
   pragma Import (C, Get_First_Global_Alias, "LLVMGetFirstGlobalAlias");

  --*
  -- * Obtain an iterator to the last GlobalAlias in a Module.
  -- *
  -- * @see llvm::Module::alias_end()
  --  

   function Get_Last_Global_Alias (M : LLVM.Types.Module_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2334
   pragma Import (C, Get_Last_Global_Alias, "LLVMGetLastGlobalAlias");

  --*
  -- * Advance a GlobalAlias iterator to the next GlobalAlias.
  -- *
  -- * Returns NULL if the iterator was already at the end and there are no more
  -- * global aliases.
  --  

   function Get_Next_Global_Alias (GA : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2342
   pragma Import (C, Get_Next_Global_Alias, "LLVMGetNextGlobalAlias");

  --*
  -- * Decrement a GlobalAlias iterator to the previous GlobalAlias.
  -- *
  -- * Returns NULL if the iterator was already at the beginning and there are
  -- * no previous global aliases.
  --  

   function Get_Previous_Global_Alias (GA : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2350
   pragma Import (C, Get_Previous_Global_Alias, "LLVMGetPreviousGlobalAlias");

  --*
  -- * Retrieve the target value of an alias.
  --  

   function Alias_Get_Aliasee (Alias : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2355
   pragma Import (C, Alias_Get_Aliasee, "LLVMAliasGetAliasee");

  --*
  -- * Set the target value of an alias.
  --  

   procedure Alias_Set_Aliasee (Alias : LLVM.Types.Value_T; Aliasee : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2360
   pragma Import (C, Alias_Set_Aliasee, "LLVMAliasSetAliasee");

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

   procedure Delete_Function (Fn : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2382
   pragma Import (C, Delete_Function, "LLVMDeleteFunction");

  --*
  -- * Check whether the given function has a personality function.
  -- *
  -- * @see llvm::Function::hasPersonalityFn()
  --  

   function Has_Personality_Fn
     (Fn : LLVM.Types.Value_T)
      return Boolean;
   function Has_Personality_Fn_C
     (Fn : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2389
   pragma Import (C, Has_Personality_Fn_C, "LLVMHasPersonalityFn");

  --*
  -- * Obtain the personality function attached to the function.
  -- *
  -- * @see llvm::Function::getPersonalityFn()
  --  

   function Get_Personality_Fn (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2396
   pragma Import (C, Get_Personality_Fn, "LLVMGetPersonalityFn");

  --*
  -- * Set the personality function attached to the function.
  -- *
  -- * @see llvm::Function::setPersonalityFn()
  --  

   procedure Set_Personality_Fn (Fn : LLVM.Types.Value_T; Personality_Fn : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2403
   pragma Import (C, Set_Personality_Fn, "LLVMSetPersonalityFn");

  --*
  -- * Obtain the intrinsic ID number which matches the given function name.
  -- *
  -- * @see llvm::Function::lookupIntrinsicID()
  --  

   function Lookup_Intrinsic_ID
     (Name     : String;
      Name_Len : stddef_h.size_t)
      return unsigned;
   function Lookup_Intrinsic_ID_C
     (Name     : Interfaces.C.Strings.chars_ptr;
      Name_Len : stddef_h.size_t)
      return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2410
   pragma Import (C, Lookup_Intrinsic_ID_C, "LLVMLookupIntrinsicID");

  --*
  -- * Obtain the ID number from a function instance.
  -- *
  -- * @see llvm::Function::getIntrinsicID()
  --  

   function Get_Intrinsic_ID (Fn : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2417
   pragma Import (C, Get_Intrinsic_ID, "LLVMGetIntrinsicID");

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
      Param_Count : stddef_h.size_t) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2425
   pragma Import (C, Get_Intrinsic_Declaration, "LLVMGetIntrinsicDeclaration");

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
      Param_Count : stddef_h.size_t) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2436
   pragma Import (C, Intrinsic_Get_Type, "LLVMIntrinsicGetType");

  --*
  -- * Retrieves the name of an intrinsic.
  -- *
  -- * @see llvm::Intrinsic::getName()
  --  

   function Intrinsic_Get_Name
     (ID          : unsigned;
      Name_Length : stddef_h.size_t)
      return String;
   function Intrinsic_Get_Name_C
     (ID          : unsigned;
      Name_Length : stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2444
   pragma Import (C, Intrinsic_Get_Name_C, "LLVMIntrinsicGetName");

  --*
  -- * Copies the name of an overloaded intrinsic identified by a given list of
  -- * parameter types.
  -- *
  -- * Unlike LLVMIntrinsicGetName, the caller is responsible for freeing the
  -- * returned string.
  -- *
  -- * @see llvm::Intrinsic::getName()
  --  

function Intrinsic_Copy_Overloaded_Name
     (ID          : unsigned;
      Param_Types : System.Address;
      Param_Count : stddef_h.size_t;
      Name_Length : stddef_h.size_t)
      return String;
   function Intrinsic_Copy_Overloaded_Name_C
     (ID          : unsigned;
      Param_Types : System.Address;
      Param_Count : stddef_h.size_t;
      Name_Length : stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Intrinsic_Copy_Overloaded_Name_C, "LLVMIntrinsicCopyOverloadedName");

  --*
  -- * Obtain if the intrinsic identified by the given ID is overloaded.
  -- *
  -- * @see llvm::Intrinsic::isOverloaded()
  --  

   function Intrinsic_Is_Overloaded
     (ID : unsigned)
      return Boolean;
   function Intrinsic_Is_Overloaded_C
     (ID : unsigned)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2465
   pragma Import (C, Intrinsic_Is_Overloaded_C, "LLVMIntrinsicIsOverloaded");

  --*
  -- * Obtain the calling function of a function.
  -- *
  -- * The returned value corresponds to the LLVMCallConv enumeration.
  -- *
  -- * @see llvm::Function::getCallingConv()
  --  

   function Get_Function_Call_Conv (Fn : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2474
   pragma Import (C, Get_Function_Call_Conv, "LLVMGetFunctionCallConv");

  --*
  -- * Set the calling convention of a function.
  -- *
  -- * @see llvm::Function::setCallingConv()
  -- *
  -- * @param Fn Function to operate on
  -- * @param CC LLVMCallConv to set calling convention to
  --  

   procedure Set_Function_Call_Conv (Fn : LLVM.Types.Value_T; CC : unsigned);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2484
   pragma Import (C, Set_Function_Call_Conv, "LLVMSetFunctionCallConv");

  --*
  -- * Obtain the name of the garbage collector to use during code
  -- * generation.
  -- *
  -- * @see llvm::Function::getGC()
  --  

   function Get_GC
     (Fn : LLVM.Types.Value_T)
      return String;
   function Get_GC_C
     (Fn : LLVM.Types.Value_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2492
   pragma Import (C, Get_GC_C, "LLVMGetGC");

  --*
  -- * Define the garbage collector to use during code generation.
  -- *
  -- * @see llvm::Function::setGC()
  --  

   procedure Set_GC
     (Fn   : LLVM.Types.Value_T;
      Name : String);
   procedure Set_GC_C
     (Fn   : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2499
   pragma Import (C, Set_GC_C, "LLVMSetGC");

  --*
  -- * Add an attribute to a function.
  -- *
  -- * @see llvm::Function::addAttribute()
  --  

   procedure Add_Attribute_At_Index
     (F : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      A : LLVM.Types.Attribute_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2506
   pragma Import (C, Add_Attribute_At_Index, "LLVMAddAttributeAtIndex");

   function Get_Attribute_Count_At_Index (F : LLVM.Types.Value_T; Idx : Attribute_Index_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2508
   pragma Import (C, Get_Attribute_Count_At_Index, "LLVMGetAttributeCountAtIndex");

   procedure Get_Attributes_At_Index
     (F : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Attrs : System.Address);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2509
   pragma Import (C, Get_Attributes_At_Index, "LLVMGetAttributesAtIndex");

   function Get_Enum_Attribute_At_Index
     (F : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Kind_ID : unsigned) return LLVM.Types.Attribute_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2511
   pragma Import (C, Get_Enum_Attribute_At_Index, "LLVMGetEnumAttributeAtIndex");

function Get_String_Attribute_At_Index
     (F     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned)
      return LLVM.Types.Attribute_T;
   function Get_String_Attribute_At_Index_C
     (F     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : Interfaces.C.Strings.chars_ptr;
      K_Len : unsigned)
      return LLVM.Types.Attribute_T;
   pragma Import (C, Get_String_Attribute_At_Index_C, "LLVMGetStringAttributeAtIndex");

   procedure Remove_Enum_Attribute_At_Index
     (F : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Kind_ID : unsigned);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2517
   pragma Import (C, Remove_Enum_Attribute_At_Index, "LLVMRemoveEnumAttributeAtIndex");

procedure Remove_String_Attribute_At_Index
     (F     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned);
   procedure Remove_String_Attribute_At_Index_C
     (F     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : Interfaces.C.Strings.chars_ptr;
      K_Len : unsigned);
   pragma Import (C, Remove_String_Attribute_At_Index_C, "LLVMRemoveStringAttributeAtIndex");

  --*
  -- * Add a target-dependent attribute to a function
  -- * @see llvm::AttrBuilder::addAttribute()
  --  

procedure Add_Target_Dependent_Function_Attr
     (Fn : LLVM.Types.Value_T;
      A  : String;
      V  : String);
   procedure Add_Target_Dependent_Function_Attr_C
     (Fn : LLVM.Types.Value_T;
      A  : Interfaces.C.Strings.chars_ptr;
      V  : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Add_Target_Dependent_Function_Attr_C, "LLVMAddTargetDependentFunctionAttr");

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

   function Count_Params (Fn : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2545
   pragma Import (C, Count_Params, "LLVMCountParams");

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

   procedure Get_Params (Fn : LLVM.Types.Value_T; Params : System.Address);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2558
   pragma Import (C, Get_Params, "LLVMGetParams");

  --*
  -- * Obtain the parameter at the specified index.
  -- *
  -- * Parameters are indexed from 0.
  -- *
  -- * @see llvm::Function::arg_begin()
  --  

   function Get_Param (Fn : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2567
   pragma Import (C, Get_Param, "LLVMGetParam");

  --*
  -- * Obtain the function to which this argument belongs.
  -- *
  -- * Unlike other functions in this group, this one takes an LLVMValueRef
  -- * that corresponds to a llvm::Attribute.
  -- *
  -- * The returned LLVMValueRef is the llvm::Function to which this
  -- * argument belongs.
  --  

   function Get_Param_Parent (Inst : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2578
   pragma Import (C, Get_Param_Parent, "LLVMGetParamParent");

  --*
  -- * Obtain the first parameter to a function.
  -- *
  -- * @see llvm::Function::arg_begin()
  --  

   function Get_First_Param (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2585
   pragma Import (C, Get_First_Param, "LLVMGetFirstParam");

  --*
  -- * Obtain the last parameter to a function.
  -- *
  -- * @see llvm::Function::arg_end()
  --  

   function Get_Last_Param (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2592
   pragma Import (C, Get_Last_Param, "LLVMGetLastParam");

  --*
  -- * Obtain the next parameter to a function.
  -- *
  -- * This takes an LLVMValueRef obtained from LLVMGetFirstParam() (which is
  -- * actually a wrapped iterator) and obtains the next parameter from the
  -- * underlying iterator.
  --  

   function Get_Next_Param (Arg : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2601
   pragma Import (C, Get_Next_Param, "LLVMGetNextParam");

  --*
  -- * Obtain the previous parameter to a function.
  -- *
  -- * This is the opposite of LLVMGetNextParam().
  --  

   function Get_Previous_Param (Arg : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2608
   pragma Import (C, Get_Previous_Param, "LLVMGetPreviousParam");

  --*
  -- * Set the alignment for a function parameter.
  -- *
  -- * @see llvm::Argument::addAttr()
  -- * @see llvm::AttrBuilder::addAlignmentAttr()
  --  

   procedure Set_Param_Alignment (Arg : LLVM.Types.Value_T; Align : unsigned);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2616
   pragma Import (C, Set_Param_Alignment, "LLVMSetParamAlignment");

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
   function Add_Global_I_Func_C
     (M          : LLVM.Types.Module_T;
      Name       : Interfaces.C.Strings.chars_ptr;
      Name_Len   : stddef_h.size_t;
      Ty         : LLVM.Types.Type_T;
      Addr_Space : unsigned;
      Resolver   : LLVM.Types.Value_T)
      return LLVM.Types.Value_T;
   pragma Import (C, Add_Global_I_Func_C, "LLVMAddGlobalIFunc");

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
   function Get_Named_Global_I_Func_C
     (M        : LLVM.Types.Module_T;
      Name     : Interfaces.C.Strings.chars_ptr;
      Name_Len : stddef_h.size_t)
      return LLVM.Types.Value_T;
   pragma Import (C, Get_Named_Global_I_Func_C, "LLVMGetNamedGlobalIFunc");

  --*
  -- * Obtain an iterator to the first GlobalIFunc in a Module.
  -- *
  -- * @see llvm::Module::ifunc_begin()
  --  

   function Get_First_Global_I_Func (M : LLVM.Types.Module_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2658
   pragma Import (C, Get_First_Global_I_Func, "LLVMGetFirstGlobalIFunc");

  --*
  -- * Obtain an iterator to the last GlobalIFunc in a Module.
  -- *
  -- * @see llvm::Module::ifunc_end()
  --  

   function Get_Last_Global_I_Func (M : LLVM.Types.Module_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2665
   pragma Import (C, Get_Last_Global_I_Func, "LLVMGetLastGlobalIFunc");

  --*
  -- * Advance a GlobalIFunc iterator to the next GlobalIFunc.
  -- *
  -- * Returns NULL if the iterator was already at the end and there are no more
  -- * global aliases.
  --  

   function Get_Next_Global_I_Func (I_Func : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2673
   pragma Import (C, Get_Next_Global_I_Func, "LLVMGetNextGlobalIFunc");

  --*
  -- * Decrement a GlobalIFunc iterator to the previous GlobalIFunc.
  -- *
  -- * Returns NULL if the iterator was already at the beginning and there are
  -- * no previous global aliases.
  --  

   function Get_Previous_Global_I_Func (I_Func : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2681
   pragma Import (C, Get_Previous_Global_I_Func, "LLVMGetPreviousGlobalIFunc");

  --*
  -- * Retrieves the resolver function associated with this indirect function, or
  -- * NULL if it doesn't not exist.
  -- *
  -- * @see llvm::GlobalIFunc::getResolver()
  --  

   function Get_Global_I_Func_Resolver (I_Func : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2689
   pragma Import (C, Get_Global_I_Func_Resolver, "LLVMGetGlobalIFuncResolver");

  --*
  -- * Sets the resolver function associated with this indirect function.
  -- *
  -- * @see llvm::GlobalIFunc::setResolver()
  --  

   procedure Set_Global_I_Func_Resolver (I_Func : LLVM.Types.Value_T; Resolver : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2696
   pragma Import (C, Set_Global_I_Func_Resolver, "LLVMSetGlobalIFuncResolver");

  --*
  -- * Remove a global indirect function from its parent module and delete it.
  -- *
  -- * @see llvm::GlobalIFunc::eraseFromParent()
  --  

   procedure Erase_Global_I_Func (I_Func : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2703
   pragma Import (C, Erase_Global_I_Func, "LLVMEraseGlobalIFunc");

  --*
  -- * Remove a global indirect function from its parent module.
  -- *
  -- * This unlinks the global indirect function from its containing module but
  -- * keeps it alive.
  -- *
  -- * @see llvm::GlobalIFunc::removeFromParent()
  --  

   procedure Remove_Global_I_Func (I_Func : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2713
   pragma Import (C, Remove_Global_I_Func, "LLVMRemoveGlobalIFunc");

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

function MD_String_In_Context2
     (C     : LLVM.Types.Context_T;
      Str   : String;
      S_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T;
   function MD_String_In_Context2_C
     (C     : LLVM.Types.Context_T;
      Str   : Interfaces.C.Strings.chars_ptr;
      S_Len : stddef_h.size_t)
      return LLVM.Types.Metadata_T;
   pragma Import (C, MD_String_In_Context2_C, "LLVMMDStringInContext2");

  --*
  -- * Create an MDNode value with the given array of operands.
  -- *
  -- * @see llvm::MDNode::get()
  --  

   function MD_Node_In_Context2
     (C : LLVM.Types.Context_T;
      M_Ds : System.Address;
      Count : stddef_h.size_t) return LLVM.Types.Metadata_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2753
   pragma Import (C, MD_Node_In_Context2, "LLVMMDNodeInContext2");

  --*
  -- * Obtain a Metadata as a Value.
  --  

   function Metadata_As_Value (C : LLVM.Types.Context_T; MD : LLVM.Types.Metadata_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2759
   pragma Import (C, Metadata_As_Value, "LLVMMetadataAsValue");

  --*
  -- * Obtain a Value as a Metadata.
  --  

   function Value_As_Metadata (Val : LLVM.Types.Value_T) return LLVM.Types.Metadata_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2764
   pragma Import (C, Value_As_Metadata, "LLVMValueAsMetadata");

  --*
  -- * Obtain the underlying string from a MDString value.
  -- *
  -- * @param V Instance to obtain string from.
  -- * @param Length Memory address which will hold length of returned string.
  -- * @return String data in MDString.
  --  

   function Get_MD_String
     (V      : LLVM.Types.Value_T;
      Length : unsigned)
      return String;
   function Get_MD_String_C
     (V      : LLVM.Types.Value_T;
      Length : unsigned)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2773
   pragma Import (C, Get_MD_String_C, "LLVMGetMDString");

  --*
  -- * Obtain the number of operands from an MDNode value.
  -- *
  -- * @param V MDNode to get number of operands from.
  -- * @return Number of operands of the MDNode.
  --  

   function Get_MD_Node_Num_Operands (V : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2781
   pragma Import (C, Get_MD_Node_Num_Operands, "LLVMGetMDNodeNumOperands");

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

   procedure Get_MD_Node_Operands (V : LLVM.Types.Value_T; Dest : System.Address);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2794
   pragma Import (C, Get_MD_Node_Operands, "LLVMGetMDNodeOperands");

  --* Deprecated: Use LLVMMDStringInContext2 instead.  
function MD_String_In_Context
     (C     : LLVM.Types.Context_T;
      Str   : String;
      S_Len : unsigned)
      return LLVM.Types.Value_T;
   function MD_String_In_Context_C
     (C     : LLVM.Types.Context_T;
      Str   : Interfaces.C.Strings.chars_ptr;
      S_Len : unsigned)
      return LLVM.Types.Value_T;
   pragma Import (C, MD_String_In_Context_C, "LLVMMDStringInContext");

  --* Deprecated: Use LLVMMDStringInContext2 instead.  
   function MD_String
     (Str   : String;
      S_Len : unsigned)
      return LLVM.Types.Value_T;
   function MD_String_C
     (Str   : Interfaces.C.Strings.chars_ptr;
      S_Len : unsigned)
      return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2800
   pragma Import (C, MD_String_C, "LLVMMDString");

  --* Deprecated: Use LLVMMDNodeInContext2 instead.  
   function MD_Node_In_Context
     (C : LLVM.Types.Context_T;
      Vals : System.Address;
      Count : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2802
   pragma Import (C, MD_Node_In_Context, "LLVMMDNodeInContext");

  --* Deprecated: Use LLVMMDNodeInContext2 instead.  
   function MD_Node (Vals : System.Address; Count : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2805
   pragma Import (C, MD_Node, "LLVMMDNode");

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

   function Basic_Block_As_Value (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2831
   pragma Import (C, Basic_Block_As_Value, "LLVMBasicBlockAsValue");

  --*
  -- * Determine whether an LLVMValueRef is itself a basic block.
  --  

   function Value_Is_Basic_Block
     (Val : LLVM.Types.Value_T)
      return Boolean;
   function Value_Is_Basic_Block_C
     (Val : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2836
   pragma Import (C, Value_Is_Basic_Block_C, "LLVMValueIsBasicBlock");

  --*
  -- * Convert an LLVMValueRef to an LLVMBasicBlockRef instance.
  --  

   function Value_As_Basic_Block (Val : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2841
   pragma Import (C, Value_As_Basic_Block, "LLVMValueAsBasicBlock");

  --*
  -- * Obtain the string name of a basic block.
  --  

   function Get_Basic_Block_Name
     (BB : LLVM.Types.Basic_Block_T)
      return String;
   function Get_Basic_Block_Name_C
     (BB : LLVM.Types.Basic_Block_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2846
   pragma Import (C, Get_Basic_Block_Name_C, "LLVMGetBasicBlockName");

  --*
  -- * Obtain the function to which a basic block belongs.
  -- *
  -- * @see llvm::BasicBlock::getParent()
  --  

   function Get_Basic_Block_Parent (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2853
   pragma Import (C, Get_Basic_Block_Parent, "LLVMGetBasicBlockParent");

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

   function Get_Basic_Block_Terminator (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2865
   pragma Import (C, Get_Basic_Block_Terminator, "LLVMGetBasicBlockTerminator");

  --*
  -- * Obtain the number of basic blocks in a function.
  -- *
  -- * @param Fn Function value to operate on.
  --  

   function Count_Basic_Blocks (Fn : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2872
   pragma Import (C, Count_Basic_Blocks, "LLVMCountBasicBlocks");

  --*
  -- * Obtain all of the basic blocks in a function.
  -- *
  -- * This operates on a function value. The BasicBlocks parameter is a
  -- * pointer to a pre-allocated array of LLVMBasicBlockRef of at least
  -- * LLVMCountBasicBlocks() in length. This array is populated with
  -- * LLVMBasicBlockRef instances.
  --  

   procedure Get_Basic_Blocks (Fn : LLVM.Types.Value_T; Basic_Blocks : System.Address);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2882
   pragma Import (C, Get_Basic_Blocks, "LLVMGetBasicBlocks");

  --*
  -- * Obtain the first basic block in a function.
  -- *
  -- * The returned basic block can be used as an iterator. You will likely
  -- * eventually call into LLVMGetNextBasicBlock() with it.
  -- *
  -- * @see llvm::Function::begin()
  --  

   function Get_First_Basic_Block (Fn : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2892
   pragma Import (C, Get_First_Basic_Block, "LLVMGetFirstBasicBlock");

  --*
  -- * Obtain the last basic block in a function.
  -- *
  -- * @see llvm::Function::end()
  --  

   function Get_Last_Basic_Block (Fn : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2899
   pragma Import (C, Get_Last_Basic_Block, "LLVMGetLastBasicBlock");

  --*
  -- * Advance a basic block iterator.
  --  

   function Get_Next_Basic_Block (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2904
   pragma Import (C, Get_Next_Basic_Block, "LLVMGetNextBasicBlock");

  --*
  -- * Go backwards in a basic block iterator.
  --  

   function Get_Previous_Basic_Block (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2909
   pragma Import (C, Get_Previous_Basic_Block, "LLVMGetPreviousBasicBlock");

  --*
  -- * Obtain the basic block that corresponds to the entry point of a
  -- * function.
  -- *
  -- * @see llvm::Function::getEntryBlock()
  --  

   function Get_Entry_Basic_Block (Fn : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2917
   pragma Import (C, Get_Entry_Basic_Block, "LLVMGetEntryBasicBlock");

  --*
  -- * Insert the given basic block after the insertion point of the given builder.
  -- *
  -- * The insertion point must be valid.
  -- *
  -- * @see llvm::Function::BasicBlockListType::insertAfter()
  --  

   procedure Insert_Existing_Basic_Block_After_Insert_Block (Builder : LLVM.Types.Builder_T; BB : LLVM.Types.Basic_Block_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2926
   pragma Import (C, Insert_Existing_Basic_Block_After_Insert_Block, "LLVMInsertExistingBasicBlockAfterInsertBlock");

  --*
  -- * Append the given basic block to the basic block list of the given function.
  -- *
  -- * @see llvm::Function::BasicBlockListType::push_back()
  --  

   procedure Append_Existing_Basic_Block (Fn : LLVM.Types.Value_T; BB : LLVM.Types.Basic_Block_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2934
   pragma Import (C, Append_Existing_Basic_Block, "LLVMAppendExistingBasicBlock");

  --*
  -- * Create a new basic block without inserting it into a function.
  -- *
  -- * @see llvm::BasicBlock::Create()
  --  

   function Create_Basic_Block_In_Context
     (C    : LLVM.Types.Context_T;
      Name : String)
      return LLVM.Types.Basic_Block_T;
   function Create_Basic_Block_In_Context_C
     (C    : LLVM.Types.Context_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2942
   pragma Import (C, Create_Basic_Block_In_Context_C, "LLVMCreateBasicBlockInContext");

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
   function Append_Basic_Block_In_Context_C
     (C    : LLVM.Types.Context_T;
      Fn   : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Basic_Block_T;
   pragma Import (C, Append_Basic_Block_In_Context_C, "LLVMAppendBasicBlockInContext");

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
   function Append_Basic_Block_C
     (Fn   : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2960
   pragma Import (C, Append_Basic_Block_C, "LLVMAppendBasicBlock");

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
   function Insert_Basic_Block_In_Context_C
     (C    : LLVM.Types.Context_T;
      BB   : LLVM.Types.Basic_Block_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Basic_Block_T;
   pragma Import (C, Insert_Basic_Block_In_Context_C, "LLVMInsertBasicBlockInContext");

  --*
  -- * Insert a basic block in a function using the global context.
  -- *
  -- * @see llvm::BasicBlock::Create()
  --  

   function Insert_Basic_Block
     (Insert_Before_BB : LLVM.Types.Basic_Block_T;
      Name             : String)
      return LLVM.Types.Basic_Block_T;
   function Insert_Basic_Block_C
     (Insert_Before_BB : LLVM.Types.Basic_Block_T;
      Name             : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:2979
   pragma Import (C, Insert_Basic_Block_C, "LLVMInsertBasicBlock");

  --*
  -- * Remove a basic block from a function and delete it.
  -- *
  -- * This deletes the basic block from its containing function and deletes
  -- * the basic block itself.
  -- *
  -- * @see llvm::BasicBlock::eraseFromParent()
  --  

   procedure Delete_Basic_Block (BB : LLVM.Types.Basic_Block_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:2990
   pragma Import (C, Delete_Basic_Block, "LLVMDeleteBasicBlock");

  --*
  -- * Remove a basic block from a function.
  -- *
  -- * This deletes the basic block from its containing function but keep
  -- * the basic block alive.
  -- *
  -- * @see llvm::BasicBlock::removeFromParent()
  --  

   procedure Remove_Basic_Block_From_Parent (BB : LLVM.Types.Basic_Block_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3000
   pragma Import (C, Remove_Basic_Block_From_Parent, "LLVMRemoveBasicBlockFromParent");

  --*
  -- * Move a basic block to before another one.
  -- *
  -- * @see llvm::BasicBlock::moveBefore()
  --  

   procedure Move_Basic_Block_Before (BB : LLVM.Types.Basic_Block_T; Move_Pos : LLVM.Types.Basic_Block_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3007
   pragma Import (C, Move_Basic_Block_Before, "LLVMMoveBasicBlockBefore");

  --*
  -- * Move a basic block to after another one.
  -- *
  -- * @see llvm::BasicBlock::moveAfter()
  --  

   procedure Move_Basic_Block_After (BB : LLVM.Types.Basic_Block_T; Move_Pos : LLVM.Types.Basic_Block_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3014
   pragma Import (C, Move_Basic_Block_After, "LLVMMoveBasicBlockAfter");

  --*
  -- * Obtain the first instruction in a basic block.
  -- *
  -- * The returned LLVMValueRef corresponds to a llvm::Instruction
  -- * instance.
  --  

   function Get_First_Instruction (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3022
   pragma Import (C, Get_First_Instruction, "LLVMGetFirstInstruction");

  --*
  -- * Obtain the last instruction in a basic block.
  -- *
  -- * The returned LLVMValueRef corresponds to an LLVM:Instruction.
  --  

   function Get_Last_Instruction (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3029
   pragma Import (C, Get_Last_Instruction, "LLVMGetLastInstruction");

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

   function Has_Metadata (Val : LLVM.Types.Value_T) return int;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3055
   pragma Import (C, Has_Metadata, "LLVMHasMetadata");

  --*
  -- * Return metadata associated with an instruction value.
  --  

   function Get_Metadata (Val : LLVM.Types.Value_T; Kind_ID : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3060
   pragma Import (C, Get_Metadata, "LLVMGetMetadata");

  --*
  -- * Set metadata associated with an instruction value.
  --  

   procedure Set_Metadata
     (Val : LLVM.Types.Value_T;
      Kind_ID : unsigned;
      Node : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3065
   pragma Import (C, Set_Metadata, "LLVMSetMetadata");

  --*
  -- * Returns the metadata associated with an instruction value, but filters out
  -- * all the debug locations.
  -- *
  -- * @see llvm::Instruction::getAllMetadataOtherThanDebugLoc()
  --  

   function Instruction_Get_All_Metadata_Other_Than_Debug_Loc (Instr : LLVM.Types.Value_T; Num_Entries : access stddef_h.size_t) return System.Address;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3074
   pragma Import (C, Instruction_Get_All_Metadata_Other_Than_Debug_Loc, "LLVMInstructionGetAllMetadataOtherThanDebugLoc");

  --*
  -- * Obtain the basic block to which an instruction belongs.
  -- *
  -- * @see llvm::Instruction::getParent()
  --  

   function Get_Instruction_Parent (Inst : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3082
   pragma Import (C, Get_Instruction_Parent, "LLVMGetInstructionParent");

  --*
  -- * Obtain the instruction that occurs after the one specified.
  -- *
  -- * The next instruction will be from the same basic block.
  -- *
  -- * If this is the last instruction in a basic block, NULL will be
  -- * returned.
  --  

   function Get_Next_Instruction (Inst : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3092
   pragma Import (C, Get_Next_Instruction, "LLVMGetNextInstruction");

  --*
  -- * Obtain the instruction that occurred before this one.
  -- *
  -- * If the instruction is the first instruction in a basic block, NULL
  -- * will be returned.
  --  

   function Get_Previous_Instruction (Inst : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3100
   pragma Import (C, Get_Previous_Instruction, "LLVMGetPreviousInstruction");

  --*
  -- * Remove and delete an instruction.
  -- *
  -- * The instruction specified is removed from its containing building
  -- * block but is kept alive.
  -- *
  -- * @see llvm::Instruction::removeFromParent()
  --  

   procedure Instruction_Remove_From_Parent (Inst : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3110
   pragma Import (C, Instruction_Remove_From_Parent, "LLVMInstructionRemoveFromParent");

  --*
  -- * Remove and delete an instruction.
  -- *
  -- * The instruction specified is removed from its containing building
  -- * block and then deleted.
  -- *
  -- * @see llvm::Instruction::eraseFromParent()
  --  

   procedure Instruction_Erase_From_Parent (Inst : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3120
   pragma Import (C, Instruction_Erase_From_Parent, "LLVMInstructionEraseFromParent");

  --*
  -- * Obtain the code opcode for an individual instruction.
  -- *
  -- * @see llvm::Instruction::getOpCode()
  --  

   function Get_Instruction_Opcode (Inst : LLVM.Types.Value_T) return Opcode_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3127
   pragma Import (C, Get_Instruction_Opcode, "LLVMGetInstructionOpcode");

  --*
  -- * Obtain the predicate of an instruction.
  -- *
  -- * This is only valid for instructions that correspond to llvm::ICmpInst
  -- * or llvm::ConstantExpr whose opcode is llvm::Instruction::ICmp.
  -- *
  -- * @see llvm::ICmpInst::getPredicate()
  --  

   function Get_I_Cmp_Predicate (Inst : LLVM.Types.Value_T) return Int_Predicate_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3137
   pragma Import (C, Get_I_Cmp_Predicate, "LLVMGetICmpPredicate");

  --*
  -- * Obtain the float predicate of an instruction.
  -- *
  -- * This is only valid for instructions that correspond to llvm::FCmpInst
  -- * or llvm::ConstantExpr whose opcode is llvm::Instruction::FCmp.
  -- *
  -- * @see llvm::FCmpInst::getPredicate()
  --  

   function Get_F_Cmp_Predicate (Inst : LLVM.Types.Value_T) return Real_Predicate_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3147
   pragma Import (C, Get_F_Cmp_Predicate, "LLVMGetFCmpPredicate");

  --*
  -- * Create a copy of 'this' instruction that is identical in all ways
  -- * except the following:
  -- *   * The instruction has no parent
  -- *   * The instruction has no name
  -- *
  -- * @see llvm::Instruction::clone()
  --  

   function Instruction_Clone (Inst : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3157
   pragma Import (C, Instruction_Clone, "LLVMInstructionClone");

  --*
  -- * Determine whether an instruction is a terminator. This routine is named to
  -- * be compatible with historical functions that did this by querying the
  -- * underlying C++ type.
  -- *
  -- * @see llvm::Instruction::isTerminator()
  --  

   function Is_A_Terminator_Inst (Inst : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3166
   pragma Import (C, Is_A_Terminator_Inst, "LLVMIsATerminatorInst");

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

   function Get_Num_Arg_Operands (Instr : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3188
   pragma Import (C, Get_Num_Arg_Operands, "LLVMGetNumArgOperands");

  --*
  -- * Set the calling convention for a call instruction.
  -- *
  -- * This expects an LLVMValueRef that corresponds to a llvm::CallInst or
  -- * llvm::InvokeInst.
  -- *
  -- * @see llvm::CallInst::setCallingConv()
  -- * @see llvm::InvokeInst::setCallingConv()
  --  

   procedure Set_Instruction_Call_Conv (Instr : LLVM.Types.Value_T; CC : unsigned);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3199
   pragma Import (C, Set_Instruction_Call_Conv, "LLVMSetInstructionCallConv");

  --*
  -- * Obtain the calling convention for a call instruction.
  -- *
  -- * This is the opposite of LLVMSetInstructionCallConv(). Reads its
  -- * usage.
  -- *
  -- * @see LLVMSetInstructionCallConv()
  --  

   function Get_Instruction_Call_Conv (Instr : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3209
   pragma Import (C, Get_Instruction_Call_Conv, "LLVMGetInstructionCallConv");

   procedure Set_Instr_Param_Alignment
     (Instr : LLVM.Types.Value_T;
      index : unsigned;
      Align : unsigned);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3211
   pragma Import (C, Set_Instr_Param_Alignment, "LLVMSetInstrParamAlignment");

   procedure Add_Call_Site_Attribute
     (C : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      A : LLVM.Types.Attribute_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3214
   pragma Import (C, Add_Call_Site_Attribute, "LLVMAddCallSiteAttribute");

   function Get_Call_Site_Attribute_Count (C : LLVM.Types.Value_T; Idx : Attribute_Index_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3216
   pragma Import (C, Get_Call_Site_Attribute_Count, "LLVMGetCallSiteAttributeCount");

   procedure Get_Call_Site_Attributes
     (C : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Attrs : System.Address);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3217
   pragma Import (C, Get_Call_Site_Attributes, "LLVMGetCallSiteAttributes");

   function Get_Call_Site_Enum_Attribute
     (C : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Kind_ID : unsigned) return LLVM.Types.Attribute_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3219
   pragma Import (C, Get_Call_Site_Enum_Attribute, "LLVMGetCallSiteEnumAttribute");

function Get_Call_Site_String_Attribute
     (C     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned)
      return LLVM.Types.Attribute_T;
   function Get_Call_Site_String_Attribute_C
     (C     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : Interfaces.C.Strings.chars_ptr;
      K_Len : unsigned)
      return LLVM.Types.Attribute_T;
   pragma Import (C, Get_Call_Site_String_Attribute_C, "LLVMGetCallSiteStringAttribute");

   procedure Remove_Call_Site_Enum_Attribute
     (C : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Kind_ID : unsigned);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3225
   pragma Import (C, Remove_Call_Site_Enum_Attribute, "LLVMRemoveCallSiteEnumAttribute");

procedure Remove_Call_Site_String_Attribute
     (C     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : String;
      K_Len : unsigned);
   procedure Remove_Call_Site_String_Attribute_C
     (C     : LLVM.Types.Value_T;
      Idx   : Attribute_Index_T;
      K     : Interfaces.C.Strings.chars_ptr;
      K_Len : unsigned);
   pragma Import (C, Remove_Call_Site_String_Attribute_C, "LLVMRemoveCallSiteStringAttribute");

  --*
  -- * Obtain the function type called by this instruction.
  -- *
  -- * @see llvm::CallBase::getFunctionType()
  --  

   function Get_Called_Function_Type (C : LLVM.Types.Value_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3235
   pragma Import (C, Get_Called_Function_Type, "LLVMGetCalledFunctionType");

  --*
  -- * Obtain the pointer to the function invoked by this instruction.
  -- *
  -- * This expects an LLVMValueRef that corresponds to a llvm::CallInst or
  -- * llvm::InvokeInst.
  -- *
  -- * @see llvm::CallInst::getCalledValue()
  -- * @see llvm::InvokeInst::getCalledValue()
  --  

   function Get_Called_Value (Instr : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3246
   pragma Import (C, Get_Called_Value, "LLVMGetCalledValue");

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
   function Is_Tail_Call_C
     (Call_Inst : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3255
   pragma Import (C, Is_Tail_Call_C, "LLVMIsTailCall");

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
   procedure Set_Tail_Call_C
     (Call_Inst    : LLVM.Types.Value_T;
      Is_Tail_Call : LLVM.Types.Bool_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3264
   pragma Import (C, Set_Tail_Call_C, "LLVMSetTailCall");

  --*
  -- * Return the normal destination basic block.
  -- *
  -- * This only works on llvm::InvokeInst instructions.
  -- *
  -- * @see llvm::InvokeInst::getNormalDest()
  --  

   function Get_Normal_Dest (Invoke_Inst : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3273
   pragma Import (C, Get_Normal_Dest, "LLVMGetNormalDest");

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

   function Get_Unwind_Dest (Invoke_Inst : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3285
   pragma Import (C, Get_Unwind_Dest, "LLVMGetUnwindDest");

  --*
  -- * Set the normal destination basic block.
  -- *
  -- * This only works on llvm::InvokeInst instructions.
  -- *
  -- * @see llvm::InvokeInst::setNormalDest()
  --  

   procedure Set_Normal_Dest (Invoke_Inst : LLVM.Types.Value_T; B : LLVM.Types.Basic_Block_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3294
   pragma Import (C, Set_Normal_Dest, "LLVMSetNormalDest");

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

   procedure Set_Unwind_Dest (Invoke_Inst : LLVM.Types.Value_T; B : LLVM.Types.Basic_Block_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3306
   pragma Import (C, Set_Unwind_Dest, "LLVMSetUnwindDest");

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

   function Get_Num_Successors (Term : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3326
   pragma Import (C, Get_Num_Successors, "LLVMGetNumSuccessors");

  --*
  -- * Return the specified successor.
  -- *
  -- * @see llvm::Instruction::getSuccessor
  --  

   function Get_Successor (Term : LLVM.Types.Value_T; i : unsigned) return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3333
   pragma Import (C, Get_Successor, "LLVMGetSuccessor");

  --*
  -- * Update the specified successor to point at the provided block.
  -- *
  -- * @see llvm::Instruction::setSuccessor
  --  

   procedure Set_Successor
     (Term : LLVM.Types.Value_T;
      i : unsigned;
      block : LLVM.Types.Basic_Block_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3340
   pragma Import (C, Set_Successor, "LLVMSetSuccessor");

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
   function Is_Conditional_C
     (Branch : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3349
   pragma Import (C, Is_Conditional_C, "LLVMIsConditional");

  --*
  -- * Return the condition of a branch instruction.
  -- *
  -- * This only works on llvm::BranchInst instructions.
  -- *
  -- * @see llvm::BranchInst::getCondition
  --  

   function Get_Condition (Branch : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3358
   pragma Import (C, Get_Condition, "LLVMGetCondition");

  --*
  -- * Set the condition of a branch instruction.
  -- *
  -- * This only works on llvm::BranchInst instructions.
  -- *
  -- * @see llvm::BranchInst::setCondition
  --  

   procedure Set_Condition (Branch : LLVM.Types.Value_T; Cond : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3367
   pragma Import (C, Set_Condition, "LLVMSetCondition");

  --*
  -- * Obtain the default destination basic block of a switch instruction.
  -- *
  -- * This only works on llvm::SwitchInst instructions.
  -- *
  -- * @see llvm::SwitchInst::getDefaultDest()
  --  

   function Get_Switch_Default_Dest (Switch_Instr : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3376
   pragma Import (C, Get_Switch_Default_Dest, "LLVMGetSwitchDefaultDest");

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

   function Get_Allocated_Type (Alloca : LLVM.Types.Value_T) return LLVM.Types.Type_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3394
   pragma Import (C, Get_Allocated_Type, "LLVMGetAllocatedType");

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
  -- * Check whether the given GEP instruction is inbounds.
  --  

   function Is_In_Bounds
     (GEP : LLVM.Types.Value_T)
      return Boolean;
   function Is_In_Bounds_C
     (GEP : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3412
   pragma Import (C, Is_In_Bounds_C, "LLVMIsInBounds");

  --*
  -- * Set the given GEP instruction to be inbounds or not.
  --  

   procedure Set_Is_In_Bounds
     (GEP       : LLVM.Types.Value_T;
      In_Bounds : Boolean);
   procedure Set_Is_In_Bounds_C
     (GEP       : LLVM.Types.Value_T;
      In_Bounds : LLVM.Types.Bool_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3417
   pragma Import (C, Set_Is_In_Bounds_C, "LLVMSetIsInBounds");

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
      Count : unsigned);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3435
   pragma Import (C, Add_Incoming, "LLVMAddIncoming");

  --*
  -- * Obtain the number of incoming basic blocks to a PHI node.
  --  

   function Count_Incoming (Phi_Node : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3441
   pragma Import (C, Count_Incoming, "LLVMCountIncoming");

  --*
  -- * Obtain an incoming value to a PHI node as an LLVMValueRef.
  --  

   function Get_Incoming_Value (Phi_Node : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3446
   pragma Import (C, Get_Incoming_Value, "LLVMGetIncomingValue");

  --*
  -- * Obtain an incoming value to a PHI node as an LLVMBasicBlockRef.
  --  

   function Get_Incoming_Block (Phi_Node : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3451
   pragma Import (C, Get_Incoming_Block, "LLVMGetIncomingBlock");

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
  -- * NB: This also works on GEP.
  --  

   function Get_Num_Indices (Inst : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3471
   pragma Import (C, Get_Num_Indices, "LLVMGetNumIndices");

  --*
  -- * Obtain the indices as an array.
  --  

   function Get_Indices (Inst : LLVM.Types.Value_T) return access unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3476
   pragma Import (C, Get_Indices, "LLVMGetIndices");

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

   function Create_Builder_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Builder_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3499
   pragma Import (C, Create_Builder_In_Context, "LLVMCreateBuilderInContext");

   function Create_Builder return LLVM.Types.Builder_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3500
   pragma Import (C, Create_Builder, "LLVMCreateBuilder");

   procedure Position_Builder
     (Builder : LLVM.Types.Builder_T;
      Block : LLVM.Types.Basic_Block_T;
      Instr : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3501
   pragma Import (C, Position_Builder, "LLVMPositionBuilder");

   procedure Position_Builder_Before (Builder : LLVM.Types.Builder_T; Instr : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3503
   pragma Import (C, Position_Builder_Before, "LLVMPositionBuilderBefore");

   procedure Position_Builder_At_End (Builder : LLVM.Types.Builder_T; Block : LLVM.Types.Basic_Block_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3504
   pragma Import (C, Position_Builder_At_End, "LLVMPositionBuilderAtEnd");

   function Get_Insert_Block (Builder : LLVM.Types.Builder_T) return LLVM.Types.Basic_Block_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3505
   pragma Import (C, Get_Insert_Block, "LLVMGetInsertBlock");

   procedure Clear_Insertion_Position (Builder : LLVM.Types.Builder_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3506
   pragma Import (C, Clear_Insertion_Position, "LLVMClearInsertionPosition");

   procedure Insert_Into_Builder (Builder : LLVM.Types.Builder_T; Instr : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3507
   pragma Import (C, Insert_Into_Builder, "LLVMInsertIntoBuilder");

procedure Insert_Into_With_Name
     (Builder : LLVM.Types.Builder_T;
      Instr   : LLVM.Types.Value_T;
      Name    : String);
   procedure Insert_Into_Builder_With_Name_C
     (Builder : LLVM.Types.Builder_T;
      Instr   : LLVM.Types.Value_T;
      Name    : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Insert_Into_Builder_With_Name_C, "LLVMInsertIntoBuilderWithName");

   procedure Dispose_Builder (Builder : LLVM.Types.Builder_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3510
   pragma Import (C, Dispose_Builder, "LLVMDisposeBuilder");

  -- Metadata  
  --*
  -- * Get location information used by debugging information.
  -- *
  -- * @see llvm::IRBuilder::getCurrentDebugLocation()
  --  

   function Get_Current_Debug_Location2 (Builder : LLVM.Types.Builder_T) return LLVM.Types.Metadata_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3519
   pragma Import (C, Get_Current_Debug_Location2, "LLVMGetCurrentDebugLocation2");

  --*
  -- * Set location information used by debugging information.
  -- *
  -- * To clear the location metadata of the given instruction, pass NULL to \p Loc.
  -- *
  -- * @see llvm::IRBuilder::SetCurrentDebugLocation()
  --  

   procedure Set_Current_Debug_Location2 (Builder : LLVM.Types.Builder_T; Loc : LLVM.Types.Metadata_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3528
   pragma Import (C, Set_Current_Debug_Location2, "LLVMSetCurrentDebugLocation2");

  --*
  -- * Attempts to set the debug location for the given instruction using the
  -- * current debug location for the given builder.  If the builder has no current
  -- * debug location, this function is a no-op.
  -- *
  -- * @see llvm::IRBuilder::SetInstDebugLocation()
  --  

   procedure Set_Inst_Debug_Location (Builder : LLVM.Types.Builder_T; Inst : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3537
   pragma Import (C, Set_Inst_Debug_Location, "LLVMSetInstDebugLocation");

  --*
  -- * Get the dafult floating-point math metadata for a given builder.
  -- *
  -- * @see llvm::IRBuilder::getDefaultFPMathTag()
  --  

   function Builder_Get_Default_FP_Math_Tag (Builder : LLVM.Types.Builder_T) return LLVM.Types.Metadata_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3544
   pragma Import (C, Builder_Get_Default_FP_Math_Tag, "LLVMBuilderGetDefaultFPMathTag");

  --*
  -- * Set the default floating-point math metadata for the given builder.
  -- *
  -- * To clear the metadata, pass NULL to \p FPMathTag.
  -- *
  -- * @see llvm::IRBuilder::setDefaultFPMathTag()
  --  

   procedure Builder_Set_Default_FP_Math_Tag (Builder : LLVM.Types.Builder_T; FP_Math_Tag : LLVM.Types.Metadata_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3553
   pragma Import (C, Builder_Set_Default_FP_Math_Tag, "LLVMBuilderSetDefaultFPMathTag");

  --*
  -- * Deprecated: Passing the NULL location will crash.
  -- * Use LLVMGetCurrentDebugLocation2 instead.
  --  

   procedure Set_Current_Debug_Location (Builder : LLVM.Types.Builder_T; L : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3560
   pragma Import (C, Set_Current_Debug_Location, "LLVMSetCurrentDebugLocation");

  --*
  -- * Deprecated: Returning the NULL location will crash.
  -- * Use LLVMGetCurrentDebugLocation2 instead.
  --  

   function Get_Current_Debug_Location (Builder : LLVM.Types.Builder_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3565
   pragma Import (C, Get_Current_Debug_Location, "LLVMGetCurrentDebugLocation");

  -- Terminators  
   function Build_Ret_Void (arg1 : LLVM.Types.Builder_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3568
   pragma Import (C, Build_Ret_Void, "LLVMBuildRetVoid");

   function Build_Ret (arg1 : LLVM.Types.Builder_T; V : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3569
   pragma Import (C, Build_Ret, "LLVMBuildRet");

   function Build_Aggregate_Ret
     (arg1 : LLVM.Types.Builder_T;
      Ret_Vals : System.Address;
      N : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3570
   pragma Import (C, Build_Aggregate_Ret, "LLVMBuildAggregateRet");

   function Build_Br (arg1 : LLVM.Types.Builder_T; Dest : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3572
   pragma Import (C, Build_Br, "LLVMBuildBr");

   function Build_Cond_Br
     (arg1 : LLVM.Types.Builder_T;
      C_If : LLVM.Types.Value_T;
      C_Then : LLVM.Types.Basic_Block_T;
      C_Else : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3573
   pragma Import (C, Build_Cond_Br, "LLVMBuildCondBr");

   function Build_Switch
     (arg1 : LLVM.Types.Builder_T;
      V : LLVM.Types.Value_T;
      C_Else : LLVM.Types.Basic_Block_T;
      Num_Cases : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3575
   pragma Import (C, Build_Switch, "LLVMBuildSwitch");

   function Build_Indirect_Br
     (B : LLVM.Types.Builder_T;
      Addr : LLVM.Types.Value_T;
      Num_Dests : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3577
   pragma Import (C, Build_Indirect_Br, "LLVMBuildIndirectBr");

  -- LLVMBuildInvoke is deprecated in favor of LLVMBuildInvoke2, in preparation
  -- for opaque pointer types.
function Invoke
     (arg1     : LLVM.Types.Builder_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      C_Then   : LLVM.Types.Basic_Block_T;
      Catch    : LLVM.Types.Basic_Block_T;
      Name     : String)
      return LLVM.Types.Value_T;
   function Build_Invoke_C
     (arg1     : LLVM.Types.Builder_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      C_Then   : LLVM.Types.Basic_Block_T;
      Catch    : LLVM.Types.Basic_Block_T;
      Name     : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Invoke_C, "LLVMBuildInvoke");

function Invoke2
     (arg1     : LLVM.Types.Builder_T;
      Ty       : LLVM.Types.Type_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      C_Then   : LLVM.Types.Basic_Block_T;
      Catch    : LLVM.Types.Basic_Block_T;
      Name     : String)
      return LLVM.Types.Value_T;
   function Build_Invoke2_C
     (arg1     : LLVM.Types.Builder_T;
      Ty       : LLVM.Types.Type_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      C_Then   : LLVM.Types.Basic_Block_T;
      Catch    : LLVM.Types.Basic_Block_T;
      Name     : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Invoke2_C, "LLVMBuildInvoke2");

   function Build_Unreachable (arg1 : LLVM.Types.Builder_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3589
   pragma Import (C, Build_Unreachable, "LLVMBuildUnreachable");

  -- Exception Handling  
   function Build_Resume (B : LLVM.Types.Builder_T; Exn : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3592
   pragma Import (C, Build_Resume, "LLVMBuildResume");

function Landing_Pad
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pers_Fn     : LLVM.Types.Value_T;
      Num_Clauses : unsigned;
      Name        : String)
      return LLVM.Types.Value_T;
   function Build_Landing_Pad_C
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pers_Fn     : LLVM.Types.Value_T;
      Num_Clauses : unsigned;
      Name        : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Landing_Pad_C, "LLVMBuildLandingPad");

   function Build_Cleanup_Ret
     (B : LLVM.Types.Builder_T;
      Catch_Pad : LLVM.Types.Value_T;
      BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3596
   pragma Import (C, Build_Cleanup_Ret, "LLVMBuildCleanupRet");

   function Build_Catch_Ret
     (B : LLVM.Types.Builder_T;
      Catch_Pad : LLVM.Types.Value_T;
      BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3598
   pragma Import (C, Build_Catch_Ret, "LLVMBuildCatchRet");

function Catch_Pad
     (B          : LLVM.Types.Builder_T;
      Parent_Pad : LLVM.Types.Value_T;
      Args       : System.Address;
      Num_Args   : unsigned;
      Name       : String)
      return LLVM.Types.Value_T;
   function Build_Catch_Pad_C
     (B          : LLVM.Types.Builder_T;
      Parent_Pad : LLVM.Types.Value_T;
      Args       : System.Address;
      Num_Args   : unsigned;
      Name       : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Catch_Pad_C, "LLVMBuildCatchPad");

function Cleanup_Pad
     (B          : LLVM.Types.Builder_T;
      Parent_Pad : LLVM.Types.Value_T;
      Args       : System.Address;
      Num_Args   : unsigned;
      Name       : String)
      return LLVM.Types.Value_T;
   function Build_Cleanup_Pad_C
     (B          : LLVM.Types.Builder_T;
      Parent_Pad : LLVM.Types.Value_T;
      Args       : System.Address;
      Num_Args   : unsigned;
      Name       : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Cleanup_Pad_C, "LLVMBuildCleanupPad");

function Catch_Switch
     (B            : LLVM.Types.Builder_T;
      Parent_Pad   : LLVM.Types.Value_T;
      Unwind_BB    : LLVM.Types.Basic_Block_T;
      Num_Handlers : unsigned;
      Name         : String)
      return LLVM.Types.Value_T;
   function Build_Catch_Switch_C
     (B            : LLVM.Types.Builder_T;
      Parent_Pad   : LLVM.Types.Value_T;
      Unwind_BB    : LLVM.Types.Basic_Block_T;
      Num_Handlers : unsigned;
      Name         : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Catch_Switch_C, "LLVMBuildCatchSwitch");

  -- Add a case to the switch instruction  
   procedure Add_Case
     (Switch : LLVM.Types.Value_T;
      On_Val : LLVM.Types.Value_T;
      Dest : LLVM.Types.Basic_Block_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3611
   pragma Import (C, Add_Case, "LLVMAddCase");

  -- Add a destination to the indirectbr instruction  
   procedure Add_Destination (Indirect_Br : LLVM.Types.Value_T; Dest : LLVM.Types.Basic_Block_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3615
   pragma Import (C, Add_Destination, "LLVMAddDestination");

  -- Get the number of clauses on the landingpad instruction  
   function Get_Num_Clauses (Landing_Pad : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3618
   pragma Import (C, Get_Num_Clauses, "LLVMGetNumClauses");

  -- Get the value of the clause at idnex Idx on the landingpad instruction  
   function Get_Clause (Landing_Pad : LLVM.Types.Value_T; Idx : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3621
   pragma Import (C, Get_Clause, "LLVMGetClause");

  -- Add a catch or filter clause to the landingpad instruction  
   procedure Add_Clause (Landing_Pad : LLVM.Types.Value_T; Clause_Val : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3624
   pragma Import (C, Add_Clause, "LLVMAddClause");

  -- Get the 'cleanup' flag in the landingpad instruction  
   function Is_Cleanup
     (Landing_Pad : LLVM.Types.Value_T)
      return Boolean;
   function Is_Cleanup_C
     (Landing_Pad : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3627
   pragma Import (C, Is_Cleanup_C, "LLVMIsCleanup");

  -- Set the 'cleanup' flag in the landingpad instruction  
   procedure Set_Cleanup
     (Landing_Pad : LLVM.Types.Value_T;
      Val         : Boolean);
   procedure Set_Cleanup_C
     (Landing_Pad : LLVM.Types.Value_T;
      Val         : LLVM.Types.Bool_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3630
   pragma Import (C, Set_Cleanup_C, "LLVMSetCleanup");

  -- Add a destination to the catchswitch instruction  
   procedure Add_Handler (Catch_Switch : LLVM.Types.Value_T; Dest : LLVM.Types.Basic_Block_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3633
   pragma Import (C, Add_Handler, "LLVMAddHandler");

  -- Get the number of handlers on the catchswitch instruction  
   function Get_Num_Handlers (Catch_Switch : LLVM.Types.Value_T) return unsigned;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3636
   pragma Import (C, Get_Num_Handlers, "LLVMGetNumHandlers");

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

   procedure Get_Handlers (Catch_Switch : LLVM.Types.Value_T; Handlers : System.Address);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3649
   pragma Import (C, Get_Handlers, "LLVMGetHandlers");

  -- Funclets  
  -- Get the number of funcletpad arguments.  
   function Get_Arg_Operand (Funclet : LLVM.Types.Value_T; i : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3654
   pragma Import (C, Get_Arg_Operand, "LLVMGetArgOperand");

  -- Set a funcletpad argument at the given index.  
   procedure Set_Arg_Operand
     (Funclet : LLVM.Types.Value_T;
      i : unsigned;
      value : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3657
   pragma Import (C, Set_Arg_Operand, "LLVMSetArgOperand");

  --*
  -- * Get the parent catchswitch instruction of a catchpad instruction.
  -- *
  -- * This only works on llvm::CatchPadInst instructions.
  -- *
  -- * @see llvm::CatchPadInst::getCatchSwitch()
  --  

   function Get_Parent_Catch_Switch (Catch_Pad : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3666
   pragma Import (C, Get_Parent_Catch_Switch, "LLVMGetParentCatchSwitch");

  --*
  -- * Set the parent catchswitch instruction of a catchpad instruction.
  -- *
  -- * This only works on llvm::CatchPadInst instructions.
  -- *
  -- * @see llvm::CatchPadInst::setCatchSwitch()
  --  

   procedure Set_Parent_Catch_Switch (Catch_Pad : LLVM.Types.Value_T; Catch_Switch : LLVM.Types.Value_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3675
   pragma Import (C, Set_Parent_Catch_Switch, "LLVMSetParentCatchSwitch");

  -- Arithmetic  
function Add
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Add_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Add_C, "LLVMBuildAdd");

function NSW_Add
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_NSW_Add_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_NSW_Add_C, "LLVMBuildNSWAdd");

function NUW_Add
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_NUW_Add_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_NUW_Add_C, "LLVMBuildNUWAdd");

function F_Add
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_F_Add_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_F_Add_C, "LLVMBuildFAdd");

function Sub
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Sub_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Sub_C, "LLVMBuildSub");

function NSW_Sub
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_NSW_Sub_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_NSW_Sub_C, "LLVMBuildNSWSub");

function NUW_Sub
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_NUW_Sub_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_NUW_Sub_C, "LLVMBuildNUWSub");

function F_Sub
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_F_Sub_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_F_Sub_C, "LLVMBuildFSub");

function Mul
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Mul_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Mul_C, "LLVMBuildMul");

function NSW_Mul
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_NSW_Mul_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_NSW_Mul_C, "LLVMBuildNSWMul");

function NUW_Mul
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_NUW_Mul_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_NUW_Mul_C, "LLVMBuildNUWMul");

function F_Mul
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_F_Mul_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_F_Mul_C, "LLVMBuildFMul");

function U_Div
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_U_Div_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_U_Div_C, "LLVMBuildUDiv");

function Exact_U_Div
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Exact_U_Div_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Exact_U_Div_C, "LLVMBuildExactUDiv");

function S_Div
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_S_Div_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_S_Div_C, "LLVMBuildSDiv");

function Exact_S_Div
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Exact_S_Div_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Exact_S_Div_C, "LLVMBuildExactSDiv");

function F_Div
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_F_Div_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_F_Div_C, "LLVMBuildFDiv");

function U_Rem
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_U_Rem_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_U_Rem_C, "LLVMBuildURem");

function S_Rem
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_S_Rem_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_S_Rem_C, "LLVMBuildSRem");

function F_Rem
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_F_Rem_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_F_Rem_C, "LLVMBuildFRem");

function Shl
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Shl_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Shl_C, "LLVMBuildShl");

function L_Shr
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_L_Shr_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_L_Shr_C, "LLVMBuildLShr");

function A_Shr
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_A_Shr_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_A_Shr_C, "LLVMBuildAShr");

function Build_And
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_And_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_And_C, "LLVMBuildAnd");

function Build_Or
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Or_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Or_C, "LLVMBuildOr");

function Build_Xor
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Xor_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Xor_C, "LLVMBuildXor");

function Bin_Op
     (B    : LLVM.Types.Builder_T;
      Op   : Opcode_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Bin_Op_C
     (B    : LLVM.Types.Builder_T;
      Op   : Opcode_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Bin_Op_C, "LLVMBuildBinOp");

function Neg
     (arg1 : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Neg_C
     (arg1 : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Neg_C, "LLVMBuildNeg");

function NSW_Neg
     (B    : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_NSW_Neg_C
     (B    : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_NSW_Neg_C, "LLVMBuildNSWNeg");

function NUW_Neg
     (B    : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_NUW_Neg_C
     (B    : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_NUW_Neg_C, "LLVMBuildNUWNeg");

function F_Neg
     (arg1 : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_F_Neg_C
     (arg1 : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_F_Neg_C, "LLVMBuildFNeg");

function Build_Not
     (arg1 : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Not_C
     (arg1 : LLVM.Types.Builder_T;
      V    : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Not_C, "LLVMBuildNot");

  -- Memory  
function Malloc
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Malloc_C
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Malloc_C, "LLVMBuildMalloc");

function Array_Malloc
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Val  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Array_Malloc_C
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Val  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Array_Malloc_C, "LLVMBuildArrayMalloc");

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
      Align : unsigned) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3752
   pragma Import (C, Build_Mem_Set, "LLVMBuildMemSet");

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
      Size : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3760
   pragma Import (C, Build_Mem_Cpy, "LLVMBuildMemCpy");

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
      Size : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3769
   pragma Import (C, Build_Mem_Move, "LLVMBuildMemMove");

function Alloca
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Alloca_C
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Alloca_C, "LLVMBuildAlloca");

function Array_Alloca
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Val  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Array_Alloca_C
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Val  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Array_Alloca_C, "LLVMBuildArrayAlloca");

   function Build_Free (arg1 : LLVM.Types.Builder_T; Pointer_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3777
   pragma Import (C, Build_Free, "LLVMBuildFree");

  -- LLVMBuildLoad is deprecated in favor of LLVMBuildLoad2, in preparation for
  -- opaque pointer types.
function Load
     (arg1        : LLVM.Types.Builder_T;
      Pointer_Val : LLVM.Types.Value_T;
      Name        : String)
      return LLVM.Types.Value_T;
   function Build_Load_C
     (arg1        : LLVM.Types.Builder_T;
      Pointer_Val : LLVM.Types.Value_T;
      Name        : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Load_C, "LLVMBuildLoad");

function Load2
     (arg1        : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer_Val : LLVM.Types.Value_T;
      Name        : String)
      return LLVM.Types.Value_T;
   function Build_Load2_C
     (arg1        : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer_Val : LLVM.Types.Value_T;
      Name        : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Load2_C, "LLVMBuildLoad2");

   function Build_Store
     (arg1 : LLVM.Types.Builder_T;
      Val : LLVM.Types.Value_T;
      Ptr : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3784
   pragma Import (C, Build_Store, "LLVMBuildStore");

  -- LLVMBuildGEP, LLVMBuildInBoundsGEP, and LLVMBuildStructGEP are deprecated in
  -- favor of LLVMBuild*GEP2, in preparation for opaque pointer types.
function GEP
     (B           : LLVM.Types.Builder_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : String)
      return LLVM.Types.Value_T;
   function Build_GEP_C
     (B           : LLVM.Types.Builder_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_GEP_C, "LLVMBuildGEP");

function In_Bounds_GEP
     (B           : LLVM.Types.Builder_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : String)
      return LLVM.Types.Value_T;
   function Build_In_Bounds_GEP_C
     (B           : LLVM.Types.Builder_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_In_Bounds_GEP_C, "LLVMBuildInBoundsGEP");

function Struct_GEP
     (B       : LLVM.Types.Builder_T;
      Pointer : LLVM.Types.Value_T;
      Idx     : unsigned;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Struct_GEP_C
     (B       : LLVM.Types.Builder_T;
      Pointer : LLVM.Types.Value_T;
      Idx     : unsigned;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Struct_GEP_C, "LLVMBuildStructGEP");

function GE_P2
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : String)
      return LLVM.Types.Value_T;
   function Build_GE_P2_C
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_GE_P2_C, "LLVMBuildGEP2");

function In_Bounds_GE_P2
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : String)
      return LLVM.Types.Value_T;
   function Build_In_Bounds_GE_P2_C
     (B           : LLVM.Types.Builder_T;
      Ty          : LLVM.Types.Type_T;
      Pointer     : LLVM.Types.Value_T;
      Indices     : System.Address;
      Num_Indices : unsigned;
      Name        : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_In_Bounds_GE_P2_C, "LLVMBuildInBoundsGEP2");

function Struct_GE_P2
     (B       : LLVM.Types.Builder_T;
      Ty      : LLVM.Types.Type_T;
      Pointer : LLVM.Types.Value_T;
      Idx     : unsigned;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Struct_GE_P2_C
     (B       : LLVM.Types.Builder_T;
      Ty      : LLVM.Types.Type_T;
      Pointer : LLVM.Types.Value_T;
      Idx     : unsigned;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Struct_GE_P2_C, "LLVMBuildStructGEP2");

function Global_String
     (B    : LLVM.Types.Builder_T;
      Str  : String;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Global_String_C
     (B    : LLVM.Types.Builder_T;
      Str  : Interfaces.C.Strings.chars_ptr;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Global_String_C, "LLVMBuildGlobalString");

function Global_String_Ptr
     (B    : LLVM.Types.Builder_T;
      Str  : String;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Global_String_Ptr_C
     (B    : LLVM.Types.Builder_T;
      Str  : Interfaces.C.Strings.chars_ptr;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Global_String_Ptr_C, "LLVMBuildGlobalStringPtr");

   function Get_Volatile
     (Memory_Access_Inst : LLVM.Types.Value_T)
      return Boolean;
   function Get_Volatile_C
     (Memory_Access_Inst : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3808
   pragma Import (C, Get_Volatile_C, "LLVMGetVolatile");

   procedure Set_Volatile
     (Memory_Access_Inst : LLVM.Types.Value_T;
      Is_Volatile        : Boolean);
   procedure Set_Volatile_C
     (Memory_Access_Inst : LLVM.Types.Value_T;
      Is_Volatile        : LLVM.Types.Bool_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3809
   pragma Import (C, Set_Volatile_C, "LLVMSetVolatile");

   function Get_Ordering (Memory_Access_Inst : LLVM.Types.Value_T) return Atomic_Ordering_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3810
   pragma Import (C, Get_Ordering, "LLVMGetOrdering");

   procedure Set_Ordering (Memory_Access_Inst : LLVM.Types.Value_T; Ordering : Atomic_Ordering_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3811
   pragma Import (C, Set_Ordering, "LLVMSetOrdering");

  -- Casts  
function Trunc
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Trunc_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Trunc_C, "LLVMBuildTrunc");

function Z_Ext
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Z_Ext_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Z_Ext_C, "LLVMBuildZExt");

function S_Ext
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_S_Ext_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_S_Ext_C, "LLVMBuildSExt");

function FP_To_UI
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_FP_To_UI_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_FP_To_UI_C, "LLVMBuildFPToUI");

function FP_To_SI
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_FP_To_SI_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_FP_To_SI_C, "LLVMBuildFPToSI");

function UI_To_FP
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_UI_To_FP_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_UI_To_FP_C, "LLVMBuildUIToFP");

function SI_To_FP
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_SI_To_FP_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_SI_To_FP_C, "LLVMBuildSIToFP");

function FP_Trunc
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_FP_Trunc_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_FP_Trunc_C, "LLVMBuildFPTrunc");

function FP_Ext
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_FP_Ext_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_FP_Ext_C, "LLVMBuildFPExt");

function Ptr_To_Int
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Ptr_To_Int_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Ptr_To_Int_C, "LLVMBuildPtrToInt");

function Int_To_Ptr
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Int_To_Ptr_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Int_To_Ptr_C, "LLVMBuildIntToPtr");

function Bit_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Bit_Cast_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Bit_Cast_C, "LLVMBuildBitCast");

function Addr_Space_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Addr_Space_Cast_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Addr_Space_Cast_C, "LLVMBuildAddrSpaceCast");

function Z_Ext_Or_Bit_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Z_Ext_Or_Bit_Cast_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Z_Ext_Or_Bit_Cast_C, "LLVMBuildZExtOrBitCast");

function S_Ext_Or_Bit_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_S_Ext_Or_Bit_Cast_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_S_Ext_Or_Bit_Cast_C, "LLVMBuildSExtOrBitCast");

function Trunc_Or_Bit_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Trunc_Or_Bit_Cast_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Trunc_Or_Bit_Cast_C, "LLVMBuildTruncOrBitCast");

function Cast
     (B       : LLVM.Types.Builder_T;
      Op      : Opcode_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Cast_C
     (B       : LLVM.Types.Builder_T;
      Op      : Opcode_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Cast_C, "LLVMBuildCast");

function Pointer_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Pointer_Cast_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Pointer_Cast_C, "LLVMBuildPointerCast");

function Int_Cast2
     (arg1      : LLVM.Types.Builder_T;
      Val       : LLVM.Types.Value_T;
      Dest_Ty   : LLVM.Types.Type_T;
      Is_Signed : Boolean;
      Name      : String)
      return LLVM.Types.Value_T;
   function Build_Int_Cast2_C
     (arg1      : LLVM.Types.Builder_T;
      Val       : LLVM.Types.Value_T;
      Dest_Ty   : LLVM.Types.Type_T;
      Is_Signed : LLVM.Types.Bool_T;
      Name      : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Int_Cast2_C, "LLVMBuildIntCast2");

function FP_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_FP_Cast_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_FP_Cast_C, "LLVMBuildFPCast");

  --* Deprecated: This cast is always signed. Use LLVMBuildIntCast2 instead.  
  --Signed cast! 
function Int_Cast
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Int_Cast_C
     (arg1    : LLVM.Types.Builder_T;
      Val     : LLVM.Types.Value_T;
      Dest_Ty : LLVM.Types.Type_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Int_Cast_C, "LLVMBuildIntCast");

  -- Comparisons  
function I_Cmp
     (arg1 : LLVM.Types.Builder_T;
      Op   : Int_Predicate_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_I_Cmp_C
     (arg1 : LLVM.Types.Builder_T;
      Op   : Int_Predicate_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_I_Cmp_C, "LLVMBuildICmp");

function F_Cmp
     (arg1 : LLVM.Types.Builder_T;
      Op   : Real_Predicate_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_F_Cmp_C
     (arg1 : LLVM.Types.Builder_T;
      Op   : Real_Predicate_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_F_Cmp_C, "LLVMBuildFCmp");

  -- Miscellaneous instructions  
function Phi
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Phi_C
     (arg1 : LLVM.Types.Builder_T;
      Ty   : LLVM.Types.Type_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Phi_C, "LLVMBuildPhi");

  -- LLVMBuildCall is deprecated in favor of LLVMBuildCall2, in preparation for
  -- opaque pointer types.
function Call
     (arg1     : LLVM.Types.Builder_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      Name     : String)
      return LLVM.Types.Value_T;
   function Build_Call_C
     (arg1     : LLVM.Types.Builder_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      Name     : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Call_C, "LLVMBuildCall");

function Call2
     (arg1     : LLVM.Types.Builder_T;
      arg2     : LLVM.Types.Type_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      Name     : String)
      return LLVM.Types.Value_T;
   function Build_Call2_C
     (arg1     : LLVM.Types.Builder_T;
      arg2     : LLVM.Types.Type_T;
      Fn       : LLVM.Types.Value_T;
      Args     : System.Address;
      Num_Args : unsigned;
      Name     : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Call2_C, "LLVMBuildCall2");

function Build_Select
     (arg1   : LLVM.Types.Builder_T;
      C_If   : LLVM.Types.Value_T;
      C_Then : LLVM.Types.Value_T;
      C_Else : LLVM.Types.Value_T;
      Name   : String)
      return LLVM.Types.Value_T;
   function Build_Select_C
     (arg1   : LLVM.Types.Builder_T;
      C_If   : LLVM.Types.Value_T;
      C_Then : LLVM.Types.Value_T;
      C_Else : LLVM.Types.Value_T;
      Name   : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Select_C, "LLVMBuildSelect");

function VA_Arg
     (arg1 : LLVM.Types.Builder_T;
      List : LLVM.Types.Value_T;
      Ty   : LLVM.Types.Type_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_VA_Arg_C
     (arg1 : LLVM.Types.Builder_T;
      List : LLVM.Types.Value_T;
      Ty   : LLVM.Types.Type_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_VA_Arg_C, "LLVMBuildVAArg");

function Extract_Element
     (arg1    : LLVM.Types.Builder_T;
      Vec_Val : LLVM.Types.Value_T;
      Index   : LLVM.Types.Value_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Extract_Element_C
     (arg1    : LLVM.Types.Builder_T;
      Vec_Val : LLVM.Types.Value_T;
      Index   : LLVM.Types.Value_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Extract_Element_C, "LLVMBuildExtractElement");

function Insert_Element
     (arg1    : LLVM.Types.Builder_T;
      Vec_Val : LLVM.Types.Value_T;
      Elt_Val : LLVM.Types.Value_T;
      Index   : LLVM.Types.Value_T;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Insert_Element_C
     (arg1    : LLVM.Types.Builder_T;
      Vec_Val : LLVM.Types.Value_T;
      Elt_Val : LLVM.Types.Value_T;
      Index   : LLVM.Types.Value_T;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Insert_Element_C, "LLVMBuildInsertElement");

function Shuffle_Vector
     (arg1 : LLVM.Types.Builder_T;
      V1   : LLVM.Types.Value_T;
      V2   : LLVM.Types.Value_T;
      Mask : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Shuffle_Vector_C
     (arg1 : LLVM.Types.Builder_T;
      V1   : LLVM.Types.Value_T;
      V2   : LLVM.Types.Value_T;
      Mask : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Shuffle_Vector_C, "LLVMBuildShuffleVector");

function Extract_Value
     (arg1    : LLVM.Types.Builder_T;
      Agg_Val : LLVM.Types.Value_T;
      Index   : unsigned;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Extract_Value_C
     (arg1    : LLVM.Types.Builder_T;
      Agg_Val : LLVM.Types.Value_T;
      Index   : unsigned;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Extract_Value_C, "LLVMBuildExtractValue");

function Insert_Value
     (arg1    : LLVM.Types.Builder_T;
      Agg_Val : LLVM.Types.Value_T;
      Elt_Val : LLVM.Types.Value_T;
      Index   : unsigned;
      Name    : String)
      return LLVM.Types.Value_T;
   function Build_Insert_Value_C
     (arg1    : LLVM.Types.Builder_T;
      Agg_Val : LLVM.Types.Value_T;
      Elt_Val : LLVM.Types.Value_T;
      Index   : unsigned;
      Name    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Insert_Value_C, "LLVMBuildInsertValue");

function Is_Null
     (arg1 : LLVM.Types.Builder_T;
      Val  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Is_Null_C
     (arg1 : LLVM.Types.Builder_T;
      Val  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Is_Null_C, "LLVMBuildIsNull");

function Is_Not_Null
     (arg1 : LLVM.Types.Builder_T;
      Val  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Is_Not_Null_C
     (arg1 : LLVM.Types.Builder_T;
      Val  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Is_Not_Null_C, "LLVMBuildIsNotNull");

function Ptr_Diff
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Build_Ptr_Diff_C
     (arg1 : LLVM.Types.Builder_T;
      LHS  : LLVM.Types.Value_T;
      RHS  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Ptr_Diff_C, "LLVMBuildPtrDiff");

function Fence
     (B             : LLVM.Types.Builder_T;
      ordering      : Atomic_Ordering_T;
      single_Thread : Boolean;
      Name          : String)
      return LLVM.Types.Value_T;
   function Build_Fence_C
     (B             : LLVM.Types.Builder_T;
      ordering      : Atomic_Ordering_T;
      single_Thread : LLVM.Types.Bool_T;
      Name          : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Fence_C, "LLVMBuildFence");

function Atomic_RMW
     (B             : LLVM.Types.Builder_T;
      op            : Atomic_RMW_Bin_Op_T;
      PTR           : LLVM.Types.Value_T;
      Val           : LLVM.Types.Value_T;
      ordering      : Atomic_Ordering_T;
      single_Thread : Boolean)
      return LLVM.Types.Value_T;
   function Build_Atomic_RMW_C
     (B             : LLVM.Types.Builder_T;
      op            : Atomic_RMW_Bin_Op_T;
      PTR           : LLVM.Types.Value_T;
      Val           : LLVM.Types.Value_T;
      ordering      : Atomic_Ordering_T;
      single_Thread : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Atomic_RMW_C, "LLVMBuildAtomicRMW");

function Atomic_Cmp_Xchg
     (B                : LLVM.Types.Builder_T;
      Ptr              : LLVM.Types.Value_T;
      Cmp              : LLVM.Types.Value_T;
      C_New            : LLVM.Types.Value_T;
      Success_Ordering : Atomic_Ordering_T;
      Failure_Ordering : Atomic_Ordering_T;
      Single_Thread    : Boolean)
      return LLVM.Types.Value_T;
   function Build_Atomic_Cmp_Xchg_C
     (B                : LLVM.Types.Builder_T;
      Ptr              : LLVM.Types.Value_T;
      Cmp              : LLVM.Types.Value_T;
      C_New            : LLVM.Types.Value_T;
      Success_Ordering : Atomic_Ordering_T;
      Failure_Ordering : Atomic_Ordering_T;
      Single_Thread    : LLVM.Types.Bool_T)
      return LLVM.Types.Value_T;
   pragma Import (C, Build_Atomic_Cmp_Xchg_C, "LLVMBuildAtomicCmpXchg");

   function Is_Atomic_Single_Thread
     (Atomic_Inst : LLVM.Types.Value_T)
      return Boolean;
   function Is_Atomic_Single_Thread_C
     (Atomic_Inst : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3915
   pragma Import (C, Is_Atomic_Single_Thread_C, "LLVMIsAtomicSingleThread");

   procedure Set_Atomic_Single_Thread
     (Atomic_Inst   : LLVM.Types.Value_T;
      Single_Thread : Boolean);
   procedure Set_Atomic_Single_Thread_C
     (Atomic_Inst   : LLVM.Types.Value_T;
      Single_Thread : LLVM.Types.Bool_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3916
   pragma Import (C, Set_Atomic_Single_Thread_C, "LLVMSetAtomicSingleThread");

   function Get_Cmp_Xchg_Success_Ordering (Cmp_Xchg_Inst : LLVM.Types.Value_T) return Atomic_Ordering_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3918
   pragma Import (C, Get_Cmp_Xchg_Success_Ordering, "LLVMGetCmpXchgSuccessOrdering");

   procedure Set_Cmp_Xchg_Success_Ordering (Cmp_Xchg_Inst : LLVM.Types.Value_T; Ordering : Atomic_Ordering_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3919
   pragma Import (C, Set_Cmp_Xchg_Success_Ordering, "LLVMSetCmpXchgSuccessOrdering");

   function Get_Cmp_Xchg_Failure_Ordering (Cmp_Xchg_Inst : LLVM.Types.Value_T) return Atomic_Ordering_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3921
   pragma Import (C, Get_Cmp_Xchg_Failure_Ordering, "LLVMGetCmpXchgFailureOrdering");

   procedure Set_Cmp_Xchg_Failure_Ordering (Cmp_Xchg_Inst : LLVM.Types.Value_T; Ordering : Atomic_Ordering_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3922
   pragma Import (C, Set_Cmp_Xchg_Failure_Ordering, "LLVMSetCmpXchgFailureOrdering");

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

   function Create_Module_Provider_For_Existing_Module (M : LLVM.Types.Module_T) return LLVM.Types.Module_Provider_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3940
   pragma Import (C, Create_Module_Provider_For_Existing_Module, "LLVMCreateModuleProviderForExistingModule");

  --*
  -- * Destroys the module M.
  --  

   procedure Dispose_Module_Provider (M : LLVM.Types.Module_Provider_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3945
   pragma Import (C, Dispose_Module_Provider, "LLVMDisposeModuleProvider");

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
   function Create_Memory_Buffer_With_Contents_Of_File_C
     (Path        : Interfaces.C.Strings.chars_ptr;
      Out_Mem_Buf : System.Address;
      Out_Message : System.Address)
      return LLVM.Types.Bool_T;
   pragma Import (C, Create_Memory_Buffer_With_Contents_Of_File_C, "LLVMCreateMemoryBufferWithContentsOfFile");

   function Create_Memory_Buffer_With_STDIN
     (Out_Mem_Buf : System.Address;
      Out_Message : System.Address)
      return Boolean;
   function Create_Memory_Buffer_With_STDIN_C
     (Out_Mem_Buf : System.Address;
      Out_Message : System.Address)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3960
   pragma Import (C, Create_Memory_Buffer_With_STDIN_C, "LLVMCreateMemoryBufferWithSTDIN");

function Create_Memory_Buffer_With_Memory_Range
     (Input_Data               : String;
      Input_Data_Length        : stddef_h.size_t;
      Buffer_Name              : String;
      Requires_Null_Terminator : Boolean)
      return LLVM.Types.Memory_Buffer_T;
   function Create_Memory_Buffer_With_Memory_Range_C
     (Input_Data               : Interfaces.C.Strings.chars_ptr;
      Input_Data_Length        : stddef_h.size_t;
      Buffer_Name              : Interfaces.C.Strings.chars_ptr;
      Requires_Null_Terminator : LLVM.Types.Bool_T)
      return LLVM.Types.Memory_Buffer_T;
   pragma Import (C, Create_Memory_Buffer_With_Memory_Range_C, "LLVMCreateMemoryBufferWithMemoryRange");

function Create_Memory_Buffer_With_Memory_Range_Copy
     (Input_Data        : String;
      Input_Data_Length : stddef_h.size_t;
      Buffer_Name       : String)
      return LLVM.Types.Memory_Buffer_T;
   function Create_Memory_Buffer_With_Memory_Range_Copy_C
     (Input_Data        : Interfaces.C.Strings.chars_ptr;
      Input_Data_Length : stddef_h.size_t;
      Buffer_Name       : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Memory_Buffer_T;
   pragma Import (C, Create_Memory_Buffer_With_Memory_Range_Copy_C, "LLVMCreateMemoryBufferWithMemoryRangeCopy");

   function Get_Buffer_Start
     (Mem_Buf : LLVM.Types.Memory_Buffer_T)
      return String;
   function Get_Buffer_Start_C
     (Mem_Buf : LLVM.Types.Memory_Buffer_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3969
   pragma Import (C, Get_Buffer_Start_C, "LLVMGetBufferStart");

   function Get_Buffer_Size (Mem_Buf : LLVM.Types.Memory_Buffer_T) return stddef_h.size_t;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3970
   pragma Import (C, Get_Buffer_Size, "LLVMGetBufferSize");

   procedure Dispose_Memory_Buffer (Mem_Buf : LLVM.Types.Memory_Buffer_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:3971
   pragma Import (C, Dispose_Memory_Buffer, "LLVMDisposeMemoryBuffer");

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCorePassRegistry Pass Registry
  -- *
  -- * @{
  --  

  --* Return the global pass registry, for use with initialization functions.
  --    @see llvm::PassRegistry::getPassRegistry  

   function Get_Global_Pass_Registry return LLVM.Types.Pass_Registry_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:3985
   pragma Import (C, Get_Global_Pass_Registry, "LLVMGetGlobalPassRegistry");

  --*
  -- * @}
  --  

  --*
  -- * @defgroup LLVMCCorePassManagers Pass Managers
  -- *
  -- * @{
  --  

  --* Constructs a new whole-module pass pipeline. This type of pipeline is
  --    suitable for link-time optimization and whole-module transformations.
  --    @see llvm::PassManager::PassManager  

   function Create_Pass_Manager return LLVM.Types.Pass_Manager_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:4000
   pragma Import (C, Create_Pass_Manager, "LLVMCreatePassManager");

  --* Constructs a new function-by-function pass pipeline over the module
  --    provider. It does not take ownership of the module provider. This type of
  --    pipeline is suitable for code generation and JIT compilation tasks.
  --    @see llvm::FunctionPassManager::FunctionPassManager  

   function Create_Function_Pass_Manager_For_Module (M : LLVM.Types.Module_T) return LLVM.Types.Pass_Manager_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:4006
   pragma Import (C, Create_Function_Pass_Manager_For_Module, "LLVMCreateFunctionPassManagerForModule");

  --* Deprecated: Use LLVMCreateFunctionPassManagerForModule instead.  
   function Create_Function_Pass_Manager (MP : LLVM.Types.Module_Provider_T) return LLVM.Types.Pass_Manager_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:4009
   pragma Import (C, Create_Function_Pass_Manager, "LLVMCreateFunctionPassManager");

  --* Initializes, executes on the provided module, and finalizes all of the
  --    passes scheduled in the pass manager. Returns 1 if any of the passes
  --    modified the module, 0 otherwise.
  --    @see llvm::PassManager::run(Module&)  

   function Run_Pass_Manager
     (PM : LLVM.Types.Pass_Manager_T;
      M  : LLVM.Types.Module_T)
      return Boolean;
   function Run_Pass_Manager_C
     (PM : LLVM.Types.Pass_Manager_T;
      M  : LLVM.Types.Module_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:4015
   pragma Import (C, Run_Pass_Manager_C, "LLVMRunPassManager");

  --* Initializes all of the function passes scheduled in the function pass
  --    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
  --    @see llvm::FunctionPassManager::doInitialization  

   function Initialize_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T)
      return Boolean;
   function Initialize_Function_Pass_Manager_C
     (FPM : LLVM.Types.Pass_Manager_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:4020
   pragma Import (C, Initialize_Function_Pass_Manager_C, "LLVMInitializeFunctionPassManager");

  --* Executes all of the function passes scheduled in the function pass manager
  --    on the provided function. Returns 1 if any of the passes modified the
  --    function, false otherwise.
  --    @see llvm::FunctionPassManager::run(Function&)  

   function Run_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T;
      F   : LLVM.Types.Value_T)
      return Boolean;
   function Run_Function_Pass_Manager_C
     (FPM : LLVM.Types.Pass_Manager_T;
      F   : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:4026
   pragma Import (C, Run_Function_Pass_Manager_C, "LLVMRunFunctionPassManager");

  --* Finalizes all of the function passes scheduled in the function pass
  --    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
  --    @see llvm::FunctionPassManager::doFinalization  

   function Finalize_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T)
      return Boolean;
   function Finalize_Function_Pass_Manager_C
     (FPM : LLVM.Types.Pass_Manager_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:4031
   pragma Import (C, Finalize_Function_Pass_Manager_C, "LLVMFinalizeFunctionPassManager");

  --* Frees the memory of a pass pipeline. For function pipelines, does not free
  --    the module provider.
  --    @see llvm::PassManagerBase::~PassManagerBase.  

   procedure Dispose_Pass_Manager (PM : LLVM.Types.Pass_Manager_T);  -- llvm-9.0.0.src/include/llvm-c/Core.h:4036
   pragma Import (C, Dispose_Pass_Manager, "LLVMDisposePassManager");

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
   function Start_Multithreaded_C
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:4053
   pragma Import (C, Start_Multithreaded_C, "LLVMStartMultithreaded");

  --* Deprecated: Multi-threading can only be enabled/disabled with the compile
  --    time define LLVM_ENABLE_THREADS.  

   procedure Stop_Multithreaded;  -- llvm-9.0.0.src/include/llvm-c/Core.h:4057
   pragma Import (C, Stop_Multithreaded, "LLVMStopMultithreaded");

  --* Check whether LLVM is executing in thread-safe mode or not.
  --    @see llvm::llvm_is_multithreaded  

   function Is_Multithreaded
      return Boolean;
   function Is_Multithreaded_C
      return LLVM.Types.Bool_T;  -- llvm-9.0.0.src/include/llvm-c/Core.h:4061
   pragma Import (C, Is_Multithreaded_C, "LLVMIsMultithreaded");

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

