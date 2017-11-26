pragma Ada_2005;
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
   --    macro(Argument) macro(BasicBlock) macro(InlineAsm) macro(User) macro(Constant) macro(BlockAddress) macro(ConstantAggregateZero) macro(ConstantArray) macro(ConstantDataSequential) macro(ConstantDataArray) macro(ConstantDataVector) macro(ConstantExpr) macro(ConstantFP) macro(ConstantInt) macro(ConstantPointerNull) macro(ConstantStruct) macro(ConstantTokenNone) macro(ConstantVector) macro(GlobalValue) macro(GlobalAlias) macro(GlobalObject) macro(Function) macro(GlobalVariable) macro(UndefValue) macro(Instruction) macro(BinaryOperator) macro(CallInst) macro(IntrinsicInst) macro(DbgInfoIntrinsic) macro(DbgDeclareInst) macro(MemIntrinsic) macro(MemCpyInst) macro(MemMoveInst) macro(MemSetInst) macro(CmpInst) macro(FCmpInst) macro(ICmpInst) macro(ExtractElementInst) macro(GetElementPtrInst) macro(InsertElementInst) macro(InsertValueInst) macro(LandingPadInst) macro(PHINode) macro(SelectInst) macro(ShuffleVectorInst) macro(StoreInst) macro(TerminatorInst) macro(BranchInst) macro(IndirectBrInst) macro(InvokeInst) macro(ReturnInst) macro(SwitchInst) macro(UnreachableInst) macro(ResumeInst) macro(CleanupReturnInst) macro(CatchReturnInst) macro(FuncletPadInst) macro(CatchPadInst) macro(CleanupPadInst) macro(UnaryInstruction) macro(AllocaInst) macro(CastInst) macro(AddrSpaceCastInst) macro(BitCastInst) macro(FPExtInst) macro(FPToSIInst) macro(FPToUIInst) macro(FPTruncInst) macro(IntToPtrInst) macro(PtrToIntInst) macro(SExtInst) macro(SIToFPInst) macro(TruncInst) macro(UIToFPInst) macro(ZExtInst) macro(ExtractValueInst) macro(LoadInst) macro(VAArgInst)
   --  unsupported macro: LLVM_DECLARE_VALUE_CAST(name) LLVMValueRef LLVMIsA ##name(LLVMValueRef Val);
   subtype Opcode_T is unsigned;
   Op_Ret : constant Opcode_T := 1;
   Op_Br : constant Opcode_T := 2;
   Op_Switch : constant Opcode_T := 3;
   Op_Indirect_Br : constant Opcode_T := 4;
   Op_Invoke : constant Opcode_T := 5;
   Op_Unreachable : constant Opcode_T := 7;
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
   Op_Catch_Switch : constant Opcode_T := 65;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:138

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
   pragma Convention (C, Type_Kind_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:158

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
   pragma Convention (C, Linkage_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:181

   type Visibility_T is 
     (Default_Visibility,
      Hidden_Visibility,
      Protected_Visibility);
   pragma Convention (C, Visibility_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:187

   type DLL_Storage_Class_T is 
     (Default_Storage_Class,
      DLL_Import_Storage_Class,
      DLL_Export_Storage_Class);
   pragma Convention (C, DLL_Storage_Class_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:193

   subtype Call_Conv_T is unsigned;
   C_Call_Conv : constant Call_Conv_T := 0;
   Fast_Call_Conv : constant Call_Conv_T := 8;
   Cold_Call_Conv : constant Call_Conv_T := 9;
   Web_Kit_JS_Call_Conv : constant Call_Conv_T := 12;
   Any_Reg_Call_Conv : constant Call_Conv_T := 13;
   X86_Stdcall_Call_Conv : constant Call_Conv_T := 64;
   X86_Fastcall_Call_Conv : constant Call_Conv_T := 65;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:203

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
   pragma Convention (C, Value_Kind_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:235

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
   Int_SLE : constant Int_Predicate_T := 41;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:248

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
   pragma Convention (C, Real_Predicate_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:267

   type Landing_Pad_Clause_Ty_T is 
     (Landing_Pad_Catch,
      Landing_Pad_Filter);
   pragma Convention (C, Landing_Pad_Clause_Ty_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:272

   type Thread_Local_Mode_T is 
     (Not_Thread_Local,
      General_Dynamic_TLS_Model,
      Local_Dynamic_TLS_Model,
      Initial_Exec_TLS_Model,
      Local_Exec_TLS_Model);
   pragma Convention (C, Thread_Local_Mode_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:280

   subtype Atomic_Ordering_T is unsigned;
   Atomic_Ordering_Not_Atomic : constant Atomic_Ordering_T := 0;
   Atomic_Ordering_Unordered : constant Atomic_Ordering_T := 1;
   Atomic_Ordering_Monotonic : constant Atomic_Ordering_T := 2;
   Atomic_Ordering_Acquire : constant Atomic_Ordering_T := 4;
   Atomic_Ordering_Release : constant Atomic_Ordering_T := 5;
   Atomic_Ordering_Acquire_Release : constant Atomic_Ordering_T := 6;
   Atomic_Ordering_Sequentially_Consistent : constant Atomic_Ordering_T := 7;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:307

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
   pragma Convention (C, Atomic_RMW_Bin_Op_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:329

   type Diagnostic_Severity_T is 
     (DS_Error,
      DS_Warning,
      DS_Remark,
      DS_Note);
   pragma Convention (C, Diagnostic_Severity_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:336

   subtype Attribute_Index_T is unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:350

   procedure Initialize_Core (R : LLVM.Types.Pass_Registry_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:356
   pragma Import (C, Initialize_Core, "LLVMInitializeCore");

   procedure Shutdown;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:361
   pragma Import (C, Shutdown, "LLVMShutdown");

   function Create_Message
     (Message : String)
      return String;
   function Create_Message_C
     (Message : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:365
   pragma Import (C, Create_Message_C, "LLVMCreateMessage");

   procedure Dispose_Message
     (Message : String);
   procedure Dispose_Message_C
     (Message : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:366
   pragma Import (C, Dispose_Message_C, "LLVMDisposeMessage");

   type Diagnostic_Handler_T is access procedure  (arg1 : LLVM.Types.Diagnostic_Info_T; arg2 : System.Address);
   pragma Convention (C, Diagnostic_Handler_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:380

   type Yield_Callback_T is access procedure  (arg1 : LLVM.Types.Context_T; arg2 : System.Address);
   pragma Convention (C, Yield_Callback_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:381

   function Context_Create return LLVM.Types.Context_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:389
   pragma Import (C, Context_Create, "LLVMContextCreate");

   function Get_Global_Context return LLVM.Types.Context_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:394
   pragma Import (C, Get_Global_Context, "LLVMGetGlobalContext");

   procedure Context_Set_Diagnostic_Handler
     (C : LLVM.Types.Context_T;
      Handler : Diagnostic_Handler_T;
      Diagnostic_Context : System.Address);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:399
   pragma Import (C, Context_Set_Diagnostic_Handler, "LLVMContextSetDiagnosticHandler");

   function Context_Get_Diagnostic_Handler (C : LLVM.Types.Context_T) return Diagnostic_Handler_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:406
   pragma Import (C, Context_Get_Diagnostic_Handler, "LLVMContextGetDiagnosticHandler");

   function Context_Get_Diagnostic_Context (C : LLVM.Types.Context_T) return System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:411
   pragma Import (C, Context_Get_Diagnostic_Context, "LLVMContextGetDiagnosticContext");

   procedure Context_Set_Yield_Callback
     (C : LLVM.Types.Context_T;
      Callback : Yield_Callback_T;
      Opaque_Handle : System.Address);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:418
   pragma Import (C, Context_Set_Yield_Callback, "LLVMContextSetYieldCallback");

   procedure Context_Dispose (C : LLVM.Types.Context_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:427
   pragma Import (C, Context_Dispose, "LLVMContextDispose");

   function Get_Diag_Info_Description
     (DI : LLVM.Types.Diagnostic_Info_T)
      return String;
   function Get_Diag_Info_Description_C
     (DI : LLVM.Types.Diagnostic_Info_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:435
   pragma Import (C, Get_Diag_Info_Description_C, "LLVMGetDiagInfoDescription");

   function Get_Diag_Info_Severity (DI : LLVM.Types.Diagnostic_Info_T) return Diagnostic_Severity_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:442
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
      return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:446
   pragma Import (C, Get_MD_Kind_ID_C, "LLVMGetMDKindID");

   function Get_Enum_Attribute_Kind_For_Name
     (Name  : String;
      S_Len : stddef_h.size_t)
      return unsigned;
   function Get_Enum_Attribute_Kind_For_Name_C
     (Name  : Interfaces.C.Strings.chars_ptr;
      S_Len : stddef_h.size_t)
      return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:459
   pragma Import (C, Get_Enum_Attribute_Kind_For_Name_C, "LLVMGetEnumAttributeKindForName");

   function Get_Last_Enum_Attribute_Kind return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:460
   pragma Import (C, Get_Last_Enum_Attribute_Kind, "LLVMGetLastEnumAttributeKind");

   function Create_Enum_Attribute
     (C : LLVM.Types.Context_T;
      Kind_ID : unsigned;
      Val : stdint_h.uint64_t) return LLVM.Types.Attribute_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:465
   pragma Import (C, Create_Enum_Attribute, "LLVMCreateEnumAttribute");

   function Get_Enum_Attribute_Kind (A : LLVM.Types.Attribute_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:472
   pragma Import (C, Get_Enum_Attribute_Kind, "LLVMGetEnumAttributeKind");

   function Get_Enum_Attribute_Value (A : LLVM.Types.Attribute_T) return stdint_h.uint64_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:477
   pragma Import (C, Get_Enum_Attribute_Value, "LLVMGetEnumAttributeValue");

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

   function Get_String_Attribute_Kind
     (A      : LLVM.Types.Attribute_T;
      Length : unsigned)
      return String;
   function Get_String_Attribute_Kind_C
     (A      : LLVM.Types.Attribute_T;
      Length : unsigned)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:489
   pragma Import (C, Get_String_Attribute_Kind_C, "LLVMGetStringAttributeKind");

   function Get_String_Attribute_Value
     (A      : LLVM.Types.Attribute_T;
      Length : unsigned)
      return String;
   function Get_String_Attribute_Value_C
     (A      : LLVM.Types.Attribute_T;
      Length : unsigned)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:494
   pragma Import (C, Get_String_Attribute_Value_C, "LLVMGetStringAttributeValue");

   function Is_Enum_Attribute
     (A : LLVM.Types.Attribute_T)
      return Boolean;
   function Is_Enum_Attribute_C
     (A : LLVM.Types.Attribute_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:499
   pragma Import (C, Is_Enum_Attribute_C, "LLVMIsEnumAttribute");

   function Is_String_Attribute
     (A : LLVM.Types.Attribute_T)
      return Boolean;
   function Is_String_Attribute_C
     (A : LLVM.Types.Attribute_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:500
   pragma Import (C, Is_String_Attribute_C, "LLVMIsStringAttribute");

   function Module_Create_With_Name
     (Module_ID : String)
      return LLVM.Types.Module_T;
   function Module_Create_With_Name_C
     (Module_ID : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Module_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:525
   pragma Import (C, Module_Create_With_Name_C, "LLVMModuleCreateWithName");

   function Module_Create_With_Name_In_Context
     (Module_ID : String;
      C         : LLVM.Types.Context_T)
      return LLVM.Types.Module_T;
   function Module_Create_With_Name_In_Context_C
     (Module_ID : Interfaces.C.Strings.chars_ptr;
      C         : LLVM.Types.Context_T)
      return LLVM.Types.Module_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:533
   pragma Import (C, Module_Create_With_Name_In_Context_C, "LLVMModuleCreateWithNameInContext");

   function Clone_Module (M : LLVM.Types.Module_T) return LLVM.Types.Module_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:538
   pragma Import (C, Clone_Module, "LLVMCloneModule");

   procedure Dispose_Module (M : LLVM.Types.Module_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:546
   pragma Import (C, Dispose_Module, "LLVMDisposeModule");

   function Get_Module_Identifier
     (M   : LLVM.Types.Module_T;
      Len : stddef_h.size_t)
      return String;
   function Get_Module_Identifier_C
     (M   : LLVM.Types.Module_T;
      Len : stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:556
   pragma Import (C, Get_Module_Identifier_C, "LLVMGetModuleIdentifier");

procedure Set_Module_Identifier
     (M     : LLVM.Types.Module_T;
      Ident : String;
      Len   : stddef_h.size_t);
   procedure Set_Module_Identifier_C
     (M     : LLVM.Types.Module_T;
      Ident : Interfaces.C.Strings.chars_ptr;
      Len   : stddef_h.size_t);
   pragma Import (C, Set_Module_Identifier_C, "LLVMSetModuleIdentifier");

   function Get_Data_Layout_Str
     (M : LLVM.Types.Module_T)
      return String;
   function Get_Data_Layout_Str_C
     (M : LLVM.Types.Module_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:577
   pragma Import (C, Get_Data_Layout_Str_C, "LLVMGetDataLayoutStr");

   function Get_Data_Layout
     (M : LLVM.Types.Module_T)
      return String;
   function Get_Data_Layout_C
     (M : LLVM.Types.Module_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:578
   pragma Import (C, Get_Data_Layout_C, "LLVMGetDataLayout");

   procedure Set_Data_Layout
     (M               : LLVM.Types.Module_T;
      Data_Layout_Str : String);
   procedure Set_Data_Layout_C
     (M               : LLVM.Types.Module_T;
      Data_Layout_Str : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:585
   pragma Import (C, Set_Data_Layout_C, "LLVMSetDataLayout");

   function Get_Target
     (M : LLVM.Types.Module_T)
      return String;
   function Get_Target_C
     (M : LLVM.Types.Module_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:592
   pragma Import (C, Get_Target_C, "LLVMGetTarget");

   procedure Set_Target
     (M      : LLVM.Types.Module_T;
      Triple : String);
   procedure Set_Target_C
     (M      : LLVM.Types.Module_T;
      Triple : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:599
   pragma Import (C, Set_Target_C, "LLVMSetTarget");

   procedure Dump_Module (M : LLVM.Types.Module_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:606
   pragma Import (C, Dump_Module, "LLVMDumpModule");

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

   function Print_Module_To_String
     (M : LLVM.Types.Module_T)
      return String;
   function Print_Module_To_String_C
     (M : LLVM.Types.Module_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:623
   pragma Import (C, Print_Module_To_String_C, "LLVMPrintModuleToString");

   procedure Set_Module_Inline_Asm
     (M   : LLVM.Types.Module_T;
      Asm : String);
   procedure Set_Module_Inline_Asm_C
     (M   : LLVM.Types.Module_T;
      Asm : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:630
   pragma Import (C, Set_Module_Inline_Asm_C, "LLVMSetModuleInlineAsm");

   function Get_Module_Context (M : LLVM.Types.Module_T) return LLVM.Types.Context_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:637
   pragma Import (C, Get_Module_Context, "LLVMGetModuleContext");

   function Get_Type_By_Name
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Type_T;
   function Get_Type_By_Name_C
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:642
   pragma Import (C, Get_Type_By_Name_C, "LLVMGetTypeByName");

   function Get_Named_Metadata_Num_Operands
     (M    : LLVM.Types.Module_T;
      Name : String)
      return unsigned;
   function Get_Named_Metadata_Num_Operands_C
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:649
   pragma Import (C, Get_Named_Metadata_Num_Operands_C, "LLVMGetNamedMetadataNumOperands");

procedure Get_Named_Metadata_Operands
     (M    : LLVM.Types.Module_T;
      Name : String;
      Dest : System.Address);
   procedure Get_Named_Metadata_Operands_C
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr;
      Dest : System.Address);
   pragma Import (C, Get_Named_Metadata_Operands_C, "LLVMGetNamedMetadataOperands");

procedure Add_Named_Metadata_Operand
     (M    : LLVM.Types.Module_T;
      Name : String;
      Val  : LLVM.Types.Value_T);
   procedure Add_Named_Metadata_Operand_C
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr;
      Val  : LLVM.Types.Value_T);
   pragma Import (C, Add_Named_Metadata_Operand_C, "LLVMAddNamedMetadataOperand");

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

   function Get_Named_Function
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Value_T;
   function Get_Named_Function_C
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:689
   pragma Import (C, Get_Named_Function_C, "LLVMGetNamedFunction");

   function Get_First_Function (M : LLVM.Types.Module_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:696
   pragma Import (C, Get_First_Function, "LLVMGetFirstFunction");

   function Get_Last_Function (M : LLVM.Types.Module_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:703
   pragma Import (C, Get_Last_Function, "LLVMGetLastFunction");

   function Get_Next_Function (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:711
   pragma Import (C, Get_Next_Function, "LLVMGetNextFunction");

   function Get_Previous_Function (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:719
   pragma Import (C, Get_Previous_Function, "LLVMGetPreviousFunction");

   function Get_Type_Kind (Ty : LLVM.Types.Type_T) return Type_Kind_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:759
   pragma Import (C, Get_Type_Kind, "LLVMGetTypeKind");

   function Type_Is_Sized
     (Ty : LLVM.Types.Type_T)
      return Boolean;
   function Type_Is_Sized_C
     (Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:768
   pragma Import (C, Type_Is_Sized_C, "LLVMTypeIsSized");

   function Get_Type_Context (Ty : LLVM.Types.Type_T) return LLVM.Types.Context_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:775
   pragma Import (C, Get_Type_Context, "LLVMGetTypeContext");

   procedure Dump_Type (Val : LLVM.Types.Type_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:782
   pragma Import (C, Dump_Type, "LLVMDumpType");

   function Print_Type_To_String
     (Val : LLVM.Types.Type_T)
      return String;
   function Print_Type_To_String_C
     (Val : LLVM.Types.Type_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:790
   pragma Import (C, Print_Type_To_String_C, "LLVMPrintTypeToString");

   function Int1_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:803
   pragma Import (C, Int1_Type_In_Context, "LLVMInt1TypeInContext");

   function Int8_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:804
   pragma Import (C, Int8_Type_In_Context, "LLVMInt8TypeInContext");

   function Int16_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:805
   pragma Import (C, Int16_Type_In_Context, "LLVMInt16TypeInContext");

   function Int32_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:806
   pragma Import (C, Int32_Type_In_Context, "LLVMInt32TypeInContext");

   function Int64_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:807
   pragma Import (C, Int64_Type_In_Context, "LLVMInt64TypeInContext");

   function Int128_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:808
   pragma Import (C, Int128_Type_In_Context, "LLVMInt128TypeInContext");

   function Int_Type_In_Context (C : LLVM.Types.Context_T; Num_Bits : unsigned) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:809
   pragma Import (C, Int_Type_In_Context, "LLVMIntTypeInContext");

   function Int1_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:815
   pragma Import (C, Int1_Type, "LLVMInt1Type");

   function Int8_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:816
   pragma Import (C, Int8_Type, "LLVMInt8Type");

   function Int16_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:817
   pragma Import (C, Int16_Type, "LLVMInt16Type");

   function Int32_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:818
   pragma Import (C, Int32_Type, "LLVMInt32Type");

   function Int64_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:819
   pragma Import (C, Int64_Type, "LLVMInt64Type");

   function Int128_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:820
   pragma Import (C, Int128_Type, "LLVMInt128Type");

   function Int_Type (Num_Bits : unsigned) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:821
   pragma Import (C, Int_Type, "LLVMIntType");

   function Get_Int_Type_Width (Integer_Ty : LLVM.Types.Type_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:822
   pragma Import (C, Get_Int_Type_Width, "LLVMGetIntTypeWidth");

   function Half_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:837
   pragma Import (C, Half_Type_In_Context, "LLVMHalfTypeInContext");

   function Float_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:842
   pragma Import (C, Float_Type_In_Context, "LLVMFloatTypeInContext");

   function Double_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:847
   pragma Import (C, Double_Type_In_Context, "LLVMDoubleTypeInContext");

   function X86_F_P80_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:852
   pragma Import (C, X86_F_P80_Type_In_Context, "LLVMX86FP80TypeInContext");

   function F_P128_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:858
   pragma Import (C, F_P128_Type_In_Context, "LLVMFP128TypeInContext");

   function PPCF_P128_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:863
   pragma Import (C, PPCF_P128_Type_In_Context, "LLVMPPCFP128TypeInContext");

   function Half_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:870
   pragma Import (C, Half_Type, "LLVMHalfType");

   function Float_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:871
   pragma Import (C, Float_Type, "LLVMFloatType");

   function Double_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:872
   pragma Import (C, Double_Type, "LLVMDoubleType");

   function X86_F_P80_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:873
   pragma Import (C, X86_F_P80_Type, "LLVMX86FP80Type");

   function F_P128_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:874
   pragma Import (C, F_P128_Type, "LLVMFP128Type");

   function PPCF_P128_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:875
   pragma Import (C, PPCF_P128_Type, "LLVMPPCFP128Type");

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

   function Is_Function_Var_Arg
     (Function_Ty : LLVM.Types.Type_T)
      return Boolean;
   function Is_Function_Var_Arg_C
     (Function_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:900
   pragma Import (C, Is_Function_Var_Arg_C, "LLVMIsFunctionVarArg");

   function Get_Return_Type (Function_Ty : LLVM.Types.Type_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:905
   pragma Import (C, Get_Return_Type, "LLVMGetReturnType");

   function Count_Param_Types (Function_Ty : LLVM.Types.Type_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:910
   pragma Import (C, Count_Param_Types, "LLVMCountParamTypes");

   procedure Get_Param_Types (Function_Ty : LLVM.Types.Type_T; Dest : System.Address);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:923
   pragma Import (C, Get_Param_Types, "LLVMGetParamTypes");

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

   function Struct_Create_Named
     (C    : LLVM.Types.Context_T;
      Name : String)
      return LLVM.Types.Type_T;
   function Struct_Create_Named_C
     (C    : LLVM.Types.Context_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:963
   pragma Import (C, Struct_Create_Named_C, "LLVMStructCreateNamed");

   function Get_Struct_Name
     (Ty : LLVM.Types.Type_T)
      return String;
   function Get_Struct_Name_C
     (Ty : LLVM.Types.Type_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:970
   pragma Import (C, Get_Struct_Name_C, "LLVMGetStructName");

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

   function Count_Struct_Element_Types (Struct_Ty : LLVM.Types.Type_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:985
   pragma Import (C, Count_Struct_Element_Types, "LLVMCountStructElementTypes");

   procedure Get_Struct_Element_Types (Struct_Ty : LLVM.Types.Type_T; Dest : System.Address);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:997
   pragma Import (C, Get_Struct_Element_Types, "LLVMGetStructElementTypes");

   function Struct_Get_Type_At_Index (Struct_Ty : LLVM.Types.Type_T; i : unsigned) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1004
   pragma Import (C, Struct_Get_Type_At_Index, "LLVMStructGetTypeAtIndex");

   function Is_Packed_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return Boolean;
   function Is_Packed_Struct_C
     (Struct_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1011
   pragma Import (C, Is_Packed_Struct_C, "LLVMIsPackedStruct");

   function Is_Opaque_Struct
     (Struct_Ty : LLVM.Types.Type_T)
      return Boolean;
   function Is_Opaque_Struct_C
     (Struct_Ty : LLVM.Types.Type_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1018
   pragma Import (C, Is_Opaque_Struct_C, "LLVMIsOpaqueStruct");

   function Get_Element_Type (Ty : LLVM.Types.Type_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1040
   pragma Import (C, Get_Element_Type, "LLVMGetElementType");

   procedure Get_Subtypes (Tp : LLVM.Types.Type_T; Arr : System.Address);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1047
   pragma Import (C, Get_Subtypes, "LLVMGetSubtypes");

   function Get_Num_Contained_Types (Tp : LLVM.Types.Type_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1054
   pragma Import (C, Get_Num_Contained_Types, "LLVMGetNumContainedTypes");

   function Array_Type (Element_Type : LLVM.Types.Type_T; Element_Count : unsigned) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1064
   pragma Import (C, Array_Type, "LLVMArrayType");

   function Get_Array_Length (Array_Ty : LLVM.Types.Type_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1073
   pragma Import (C, Get_Array_Length, "LLVMGetArrayLength");

   function Pointer_Type (Element_Type : LLVM.Types.Type_T; Address_Space : unsigned) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1083
   pragma Import (C, Pointer_Type, "LLVMPointerType");

   function Get_Pointer_Address_Space (Pointer_Ty : LLVM.Types.Type_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1092
   pragma Import (C, Get_Pointer_Address_Space, "LLVMGetPointerAddressSpace");

   function Vector_Type (Element_Type : LLVM.Types.Type_T; Element_Count : unsigned) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1103
   pragma Import (C, Vector_Type, "LLVMVectorType");

   function Get_Vector_Size (Vector_Ty : LLVM.Types.Type_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1112
   pragma Import (C, Get_Vector_Size, "LLVMGetVectorSize");

   function Void_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1127
   pragma Import (C, Void_Type_In_Context, "LLVMVoidTypeInContext");

   function Label_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1132
   pragma Import (C, Label_Type_In_Context, "LLVMLabelTypeInContext");

   function X86_MMX_Type_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1137
   pragma Import (C, X86_MMX_Type_In_Context, "LLVMX86MMXTypeInContext");

   function Void_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1143
   pragma Import (C, Void_Type, "LLVMVoidType");

   function Label_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1144
   pragma Import (C, Label_Type, "LLVMLabelType");

   function X86_MMX_Type return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1145
   pragma Import (C, X86_MMX_Type, "LLVMX86MMXType");

   function Type_Of (Val : LLVM.Types.Value_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1271
   pragma Import (C, Type_Of, "LLVMTypeOf");

   function Get_Value_Kind (Val : LLVM.Types.Value_T) return Value_Kind_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1278
   pragma Import (C, Get_Value_Kind, "LLVMGetValueKind");

   function Get_Value_Name
     (Val : LLVM.Types.Value_T)
      return String;
   function Get_Value_Name_C
     (Val : LLVM.Types.Value_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1285
   pragma Import (C, Get_Value_Name_C, "LLVMGetValueName");

   procedure Set_Value_Name
     (Val  : LLVM.Types.Value_T;
      Name : String);
   procedure Set_Value_Name_C
     (Val  : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1292
   pragma Import (C, Set_Value_Name_C, "LLVMSetValueName");

   procedure Dump_Value (Val : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1299
   pragma Import (C, Dump_Value, "LLVMDumpValue");

   function Print_Value_To_String
     (Val : LLVM.Types.Value_T)
      return String;
   function Print_Value_To_String_C
     (Val : LLVM.Types.Value_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1307
   pragma Import (C, Print_Value_To_String_C, "LLVMPrintValueToString");

   procedure Replace_All_Uses_With (Old_Val : LLVM.Types.Value_T; New_Val : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1314
   pragma Import (C, Replace_All_Uses_With, "LLVMReplaceAllUsesWith");

   function Is_Constant
     (Val : LLVM.Types.Value_T)
      return Boolean;
   function Is_Constant_C
     (Val : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1319
   pragma Import (C, Is_Constant_C, "LLVMIsConstant");

   function Is_Undef
     (Val : LLVM.Types.Value_T)
      return Boolean;
   function Is_Undef_C
     (Val : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1324
   pragma Import (C, Is_Undef_C, "LLVMIsUndef");

   function Is_AVA_Arg_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_AVA_Arg_Inst, "LLVMIsAVAArgInst");

   function Is_A_Load_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Load_Inst, "LLVMIsALoadInst");

   function Is_A_Extract_Value_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Extract_Value_Inst, "LLVMIsAExtractValueInst");

   function Is_AZ_Ext_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_AZ_Ext_Inst, "LLVMIsAZExtInst");

   function Is_AUI_To_FP_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_AUI_To_FP_Inst, "LLVMIsAUIToFPInst");

   function Is_A_Trunc_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Trunc_Inst, "LLVMIsATruncInst");

   function Is_ASI_To_FP_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_ASI_To_FP_Inst, "LLVMIsASIToFPInst");

   function Is_AS_Ext_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_AS_Ext_Inst, "LLVMIsASExtInst");

   function Is_A_Ptr_To_Int_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Ptr_To_Int_Inst, "LLVMIsAPtrToIntInst");

   function Is_A_Int_To_Ptr_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Int_To_Ptr_Inst, "LLVMIsAIntToPtrInst");

   function Is_AFP_Trunc_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_AFP_Trunc_Inst, "LLVMIsAFPTruncInst");

   function Is_AFP_To_UI_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_AFP_To_UI_Inst, "LLVMIsAFPToUIInst");

   function Is_AFP_To_SI_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_AFP_To_SI_Inst, "LLVMIsAFPToSIInst");

   function Is_AFP_Ext_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_AFP_Ext_Inst, "LLVMIsAFPExtInst");

   function Is_A_Bit_Cast_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Bit_Cast_Inst, "LLVMIsABitCastInst");

   function Is_A_Addr_Space_Cast_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Addr_Space_Cast_Inst, "LLVMIsAAddrSpaceCastInst");

   function Is_A_Cast_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Cast_Inst, "LLVMIsACastInst");

   function Is_A_Alloca_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Alloca_Inst, "LLVMIsAAllocaInst");

   function Is_A_Unary_Instruction (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Unary_Instruction, "LLVMIsAUnaryInstruction");

   function Is_A_Cleanup_Pad_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Cleanup_Pad_Inst, "LLVMIsACleanupPadInst");

   function Is_A_Catch_Pad_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Catch_Pad_Inst, "LLVMIsACatchPadInst");

   function Is_A_Funclet_Pad_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Funclet_Pad_Inst, "LLVMIsAFuncletPadInst");

   function Is_A_Catch_Return_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Catch_Return_Inst, "LLVMIsACatchReturnInst");

   function Is_A_Cleanup_Return_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Cleanup_Return_Inst, "LLVMIsACleanupReturnInst");

   function Is_A_Resume_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Resume_Inst, "LLVMIsAResumeInst");

   function Is_A_Unreachable_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Unreachable_Inst, "LLVMIsAUnreachableInst");

   function Is_A_Switch_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Switch_Inst, "LLVMIsASwitchInst");

   function Is_A_Return_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Return_Inst, "LLVMIsAReturnInst");

   function Is_A_Invoke_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Invoke_Inst, "LLVMIsAInvokeInst");

   function Is_A_Indirect_Br_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Indirect_Br_Inst, "LLVMIsAIndirectBrInst");

   function Is_A_Branch_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Branch_Inst, "LLVMIsABranchInst");

   function Is_A_Terminator_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Terminator_Inst, "LLVMIsATerminatorInst");

   function Is_A_Store_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Store_Inst, "LLVMIsAStoreInst");

   function Is_A_Shuffle_Vector_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Shuffle_Vector_Inst, "LLVMIsAShuffleVectorInst");

   function Is_A_Select_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Select_Inst, "LLVMIsASelectInst");

   function Is_APHI_Node (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_APHI_Node, "LLVMIsAPHINode");

   function Is_A_Landing_Pad_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Landing_Pad_Inst, "LLVMIsALandingPadInst");

   function Is_A_Insert_Value_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Insert_Value_Inst, "LLVMIsAInsertValueInst");

   function Is_A_Insert_Element_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Insert_Element_Inst, "LLVMIsAInsertElementInst");

   function Is_A_Get_Element_Ptr_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Get_Element_Ptr_Inst, "LLVMIsAGetElementPtrInst");

   function Is_A_Extract_Element_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Extract_Element_Inst, "LLVMIsAExtractElementInst");

   function Is_AI_Cmp_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_AI_Cmp_Inst, "LLVMIsAICmpInst");

   function Is_AF_Cmp_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_AF_Cmp_Inst, "LLVMIsAFCmpInst");

   function Is_A_Cmp_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Cmp_Inst, "LLVMIsACmpInst");

   function Is_A_Mem_Set_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Mem_Set_Inst, "LLVMIsAMemSetInst");

   function Is_A_Mem_Move_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Mem_Move_Inst, "LLVMIsAMemMoveInst");

   function Is_A_Mem_Cpy_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Mem_Cpy_Inst, "LLVMIsAMemCpyInst");

   function Is_A_Mem_Intrinsic (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Mem_Intrinsic, "LLVMIsAMemIntrinsic");

   function Is_A_Dbg_Declare_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Dbg_Declare_Inst, "LLVMIsADbgDeclareInst");

   function Is_A_Dbg_Info_Intrinsic (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Dbg_Info_Intrinsic, "LLVMIsADbgInfoIntrinsic");

   function Is_A_Intrinsic_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Intrinsic_Inst, "LLVMIsAIntrinsicInst");

   function Is_A_Call_Inst (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Call_Inst, "LLVMIsACallInst");

   function Is_A_Binary_Operator (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Binary_Operator, "LLVMIsABinaryOperator");

   function Is_A_Instruction (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Instruction, "LLVMIsAInstruction");

   function Is_A_Undef_Value (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Undef_Value, "LLVMIsAUndefValue");

   function Is_A_Global_Variable (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Global_Variable, "LLVMIsAGlobalVariable");

   function Is_A_Function (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Function, "LLVMIsAFunction");

   function Is_A_Global_Object (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Global_Object, "LLVMIsAGlobalObject");

   function Is_A_Global_Alias (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Global_Alias, "LLVMIsAGlobalAlias");

   function Is_A_Global_Value (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Global_Value, "LLVMIsAGlobalValue");

   function Is_A_Constant_Vector (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Constant_Vector, "LLVMIsAConstantVector");

   function Is_A_Constant_Token_None (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Constant_Token_None, "LLVMIsAConstantTokenNone");

   function Is_A_Constant_Struct (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Constant_Struct, "LLVMIsAConstantStruct");

   function Is_A_Constant_Pointer_Null (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Constant_Pointer_Null, "LLVMIsAConstantPointerNull");

   function Is_A_Constant_Int (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Constant_Int, "LLVMIsAConstantInt");

   function Is_A_Constant_FP (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Constant_FP, "LLVMIsAConstantFP");

   function Is_A_Constant_Expr (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Constant_Expr, "LLVMIsAConstantExpr");

   function Is_A_Constant_Data_Vector (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Constant_Data_Vector, "LLVMIsAConstantDataVector");

   function Is_A_Constant_Data_Array (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Constant_Data_Array, "LLVMIsAConstantDataArray");

   function Is_A_Constant_Data_Sequential (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Constant_Data_Sequential, "LLVMIsAConstantDataSequential");

   function Is_A_Constant_Array (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Constant_Array, "LLVMIsAConstantArray");

   function Is_A_Constant_Aggregate_Zero (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Constant_Aggregate_Zero, "LLVMIsAConstantAggregateZero");

   function Is_A_Block_Address (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Block_Address, "LLVMIsABlockAddress");

   function Is_A_Constant (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Constant, "LLVMIsAConstant");

   function Is_A_User (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_User, "LLVMIsAUser");

   function Is_A_Inline_Asm (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Inline_Asm, "LLVMIsAInlineAsm");

   function Is_A_Basic_Block (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Basic_Block, "LLVMIsABasicBlock");

   function Is_A_Argument (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1339
   pragma Import (C, Is_A_Argument, "LLVMIsAArgument");

   function Is_AMD_Node (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1341
   pragma Import (C, Is_AMD_Node, "LLVMIsAMDNode");

   function Is_AMD_String (Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1342
   pragma Import (C, Is_AMD_String, "LLVMIsAMDString");

   function Get_First_Use (Val : LLVM.Types.Value_T) return LLVM.Types.Use_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1371
   pragma Import (C, Get_First_Use, "LLVMGetFirstUse");

   function Get_Next_Use (U : LLVM.Types.Use_T) return LLVM.Types.Use_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1379
   pragma Import (C, Get_Next_Use, "LLVMGetNextUse");

   function Get_User (U : LLVM.Types.Use_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1388
   pragma Import (C, Get_User, "LLVMGetUser");

   function Get_Used_Value (U : LLVM.Types.Use_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1395
   pragma Import (C, Get_Used_Value, "LLVMGetUsedValue");

   function Get_Operand (Val : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1416
   pragma Import (C, Get_Operand, "LLVMGetOperand");

   function Get_Operand_Use (Val : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Use_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1423
   pragma Import (C, Get_Operand_Use, "LLVMGetOperandUse");

   procedure Set_Operand
     (User : LLVM.Types.Value_T;
      Index : unsigned;
      Val : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1430
   pragma Import (C, Set_Operand, "LLVMSetOperand");

   function Get_Num_Operands (Val : LLVM.Types.Value_T) return int;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1437
   pragma Import (C, Get_Num_Operands, "LLVMGetNumOperands");

   function Const_Null (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1460
   pragma Import (C, Const_Null, "LLVMConstNull");

   function Const_All_Ones (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1470
   pragma Import (C, Const_All_Ones, "LLVMConstAllOnes");

   function Get_Undef (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1477
   pragma Import (C, Get_Undef, "LLVMGetUndef");

   function Is_Null
     (Val : LLVM.Types.Value_T)
      return Boolean;
   function Is_Null_C
     (Val : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1484
   pragma Import (C, Is_Null_C, "LLVMIsNull");

   function Const_Pointer_Null (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1490
   pragma Import (C, Const_Pointer_Null, "LLVMConstPointerNull");

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

   function Const_Int_Of_Arbitrary_Precision
     (Int_Ty : LLVM.Types.Type_T;
      Num_Words : unsigned;
      Words : access stdint_h.uint64_t) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1527
   pragma Import (C, Const_Int_Of_Arbitrary_Precision, "LLVMConstIntOfArbitraryPrecision");

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

   function Const_Real (Real_Ty : LLVM.Types.Type_T; N : double) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1555
   pragma Import (C, Const_Real, "LLVMConstReal");

   function Const_Real_Of_String
     (Real_Ty : LLVM.Types.Type_T;
      Text    : String)
      return LLVM.Types.Value_T;
   function Const_Real_Of_String_C
     (Real_Ty : LLVM.Types.Type_T;
      Text    : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1563
   pragma Import (C, Const_Real_Of_String_C, "LLVMConstRealOfString");

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

   function Const_Int_Get_Z_Ext_Value (Constant_Val : LLVM.Types.Value_T) return Extensions.unsigned_long_long;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1576
   pragma Import (C, Const_Int_Get_Z_Ext_Value, "LLVMConstIntGetZExtValue");

   function Const_Int_Get_S_Ext_Value (Constant_Val : LLVM.Types.Value_T) return Long_Long_Integer;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1583
   pragma Import (C, Const_Int_Get_S_Ext_Value, "LLVMConstIntGetSExtValue");

   function Const_Real_Get_Double
     (Constant_Val : LLVM.Types.Value_T;
      loses_Info   : Boolean)
      return double;
   function Const_Real_Get_Double_C
     (Constant_Val : LLVM.Types.Value_T;
      loses_Info   : LLVM.Types.Bool_T)
      return double;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1591
   pragma Import (C, Const_Real_Get_Double_C, "LLVMConstRealGetDouble");

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

   function Is_Constant_String
     (c : LLVM.Types.Value_T)
      return Boolean;
   function Is_Constant_String_C
     (c : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1630
   pragma Import (C, Is_Constant_String_C, "LLVMIsConstantString");

   function Get_As_String
     (c      : LLVM.Types.Value_T;
      Length : stddef_h.size_t)
      return String;
   function Get_As_String_C
     (c      : LLVM.Types.Value_T;
      Length : stddef_h.size_t)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1637
   pragma Import (C, Get_As_String_C, "LLVMGetAsString");

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

   function Const_Array
     (Element_Ty : LLVM.Types.Type_T;
      Constant_Vals : System.Address;
      Length : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1664
   pragma Import (C, Const_Array, "LLVMConstArray");

   function Const_Named_Struct
     (Struct_Ty : LLVM.Types.Type_T;
      Constant_Vals : System.Address;
      Count : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1672
   pragma Import (C, Const_Named_Struct, "LLVMConstNamedStruct");

   function Get_Element_As_Constant (C : LLVM.Types.Value_T; idx : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1681
   pragma Import (C, Get_Element_As_Constant, "LLVMGetElementAsConstant");

   function Const_Vector (Scalar_Constant_Vals : System.Address; Size : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1688
   pragma Import (C, Const_Vector, "LLVMConstVector");

   function Get_Const_Opcode (Constant_Val : LLVM.Types.Value_T) return Opcode_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1703
   pragma Import (C, Get_Const_Opcode, "LLVMGetConstOpcode");

   function Align_Of (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1704
   pragma Import (C, Align_Of, "LLVMAlignOf");

   function Size_Of (Ty : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1705
   pragma Import (C, Size_Of, "LLVMSizeOf");

   function Const_Neg (Constant_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1706
   pragma Import (C, Const_Neg, "LLVMConstNeg");

   function Const_NSW_Neg (Constant_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1707
   pragma Import (C, Const_NSW_Neg, "LLVMConstNSWNeg");

   function Const_NUW_Neg (Constant_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1708
   pragma Import (C, Const_NUW_Neg, "LLVMConstNUWNeg");

   function Const_F_Neg (Constant_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1709
   pragma Import (C, Const_F_Neg, "LLVMConstFNeg");

   function Const_Not (Constant_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1710
   pragma Import (C, Const_Not, "LLVMConstNot");

   function Const_Add (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1711
   pragma Import (C, Const_Add, "LLVMConstAdd");

   function Const_NSW_Add (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1712
   pragma Import (C, Const_NSW_Add, "LLVMConstNSWAdd");

   function Const_NUW_Add (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1713
   pragma Import (C, Const_NUW_Add, "LLVMConstNUWAdd");

   function Const_F_Add (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1714
   pragma Import (C, Const_F_Add, "LLVMConstFAdd");

   function Const_Sub (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1715
   pragma Import (C, Const_Sub, "LLVMConstSub");

   function Const_NSW_Sub (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1716
   pragma Import (C, Const_NSW_Sub, "LLVMConstNSWSub");

   function Const_NUW_Sub (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1717
   pragma Import (C, Const_NUW_Sub, "LLVMConstNUWSub");

   function Const_F_Sub (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1718
   pragma Import (C, Const_F_Sub, "LLVMConstFSub");

   function Const_Mul (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1719
   pragma Import (C, Const_Mul, "LLVMConstMul");

   function Const_NSW_Mul (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1720
   pragma Import (C, Const_NSW_Mul, "LLVMConstNSWMul");

   function Const_NUW_Mul (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1721
   pragma Import (C, Const_NUW_Mul, "LLVMConstNUWMul");

   function Const_F_Mul (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1722
   pragma Import (C, Const_F_Mul, "LLVMConstFMul");

   function Const_U_Div (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1723
   pragma Import (C, Const_U_Div, "LLVMConstUDiv");

   function Const_Exact_U_Div (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1724
   pragma Import (C, Const_Exact_U_Div, "LLVMConstExactUDiv");

   function Const_S_Div (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1725
   pragma Import (C, Const_S_Div, "LLVMConstSDiv");

   function Const_Exact_S_Div (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1726
   pragma Import (C, Const_Exact_S_Div, "LLVMConstExactSDiv");

   function Const_F_Div (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1727
   pragma Import (C, Const_F_Div, "LLVMConstFDiv");

   function Const_U_Rem (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1728
   pragma Import (C, Const_U_Rem, "LLVMConstURem");

   function Const_S_Rem (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1729
   pragma Import (C, Const_S_Rem, "LLVMConstSRem");

   function Const_F_Rem (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1730
   pragma Import (C, Const_F_Rem, "LLVMConstFRem");

   function Const_And (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1731
   pragma Import (C, Const_And, "LLVMConstAnd");

   function Const_Or (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1732
   pragma Import (C, Const_Or, "LLVMConstOr");

   function Const_Xor (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1733
   pragma Import (C, Const_Xor, "LLVMConstXor");

   function Const_I_Cmp
     (Predicate : Int_Predicate_T;
      LHS_Constant : LLVM.Types.Value_T;
      RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1734
   pragma Import (C, Const_I_Cmp, "LLVMConstICmp");

   function Const_F_Cmp
     (Predicate : Real_Predicate_T;
      LHS_Constant : LLVM.Types.Value_T;
      RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1736
   pragma Import (C, Const_F_Cmp, "LLVMConstFCmp");

   function Const_Shl (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1738
   pragma Import (C, Const_Shl, "LLVMConstShl");

   function Const_L_Shr (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1739
   pragma Import (C, Const_L_Shr, "LLVMConstLShr");

   function Const_A_Shr (LHS_Constant : LLVM.Types.Value_T; RHS_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1740
   pragma Import (C, Const_A_Shr, "LLVMConstAShr");

   function Const_GEP
     (Constant_Val : LLVM.Types.Value_T;
      Constant_Indices : System.Address;
      Num_Indices : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1741
   pragma Import (C, Const_GEP, "LLVMConstGEP");

   function Const_In_Bounds_GEP
     (Constant_Val : LLVM.Types.Value_T;
      Constant_Indices : System.Address;
      Num_Indices : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1743
   pragma Import (C, Const_In_Bounds_GEP, "LLVMConstInBoundsGEP");

   function Const_Trunc (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1746
   pragma Import (C, Const_Trunc, "LLVMConstTrunc");

   function Const_S_Ext (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1747
   pragma Import (C, Const_S_Ext, "LLVMConstSExt");

   function Const_Z_Ext (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1748
   pragma Import (C, Const_Z_Ext, "LLVMConstZExt");

   function Const_FP_Trunc (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1749
   pragma Import (C, Const_FP_Trunc, "LLVMConstFPTrunc");

   function Const_FP_Ext (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1750
   pragma Import (C, Const_FP_Ext, "LLVMConstFPExt");

   function Const_UI_To_FP (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1751
   pragma Import (C, Const_UI_To_FP, "LLVMConstUIToFP");

   function Const_SI_To_FP (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1752
   pragma Import (C, Const_SI_To_FP, "LLVMConstSIToFP");

   function Const_FP_To_UI (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1753
   pragma Import (C, Const_FP_To_UI, "LLVMConstFPToUI");

   function Const_FP_To_SI (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1754
   pragma Import (C, Const_FP_To_SI, "LLVMConstFPToSI");

   function Const_Ptr_To_Int (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1755
   pragma Import (C, Const_Ptr_To_Int, "LLVMConstPtrToInt");

   function Const_Int_To_Ptr (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1756
   pragma Import (C, Const_Int_To_Ptr, "LLVMConstIntToPtr");

   function Const_Bit_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1757
   pragma Import (C, Const_Bit_Cast, "LLVMConstBitCast");

   function Const_Addr_Space_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1758
   pragma Import (C, Const_Addr_Space_Cast, "LLVMConstAddrSpaceCast");

   function Const_Z_Ext_Or_Bit_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1759
   pragma Import (C, Const_Z_Ext_Or_Bit_Cast, "LLVMConstZExtOrBitCast");

   function Const_S_Ext_Or_Bit_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1761
   pragma Import (C, Const_S_Ext_Or_Bit_Cast, "LLVMConstSExtOrBitCast");

   function Const_Trunc_Or_Bit_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1763
   pragma Import (C, Const_Trunc_Or_Bit_Cast, "LLVMConstTruncOrBitCast");

   function Const_Pointer_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1765
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

   function Const_FP_Cast (Constant_Val : LLVM.Types.Value_T; To_Type : LLVM.Types.Type_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1769
   pragma Import (C, Const_FP_Cast, "LLVMConstFPCast");

   function Const_Select
     (Constant_Condition : LLVM.Types.Value_T;
      Constant_If_True : LLVM.Types.Value_T;
      Constant_If_False : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1770
   pragma Import (C, Const_Select, "LLVMConstSelect");

   function Const_Extract_Element (Vector_Constant : LLVM.Types.Value_T; Index_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1773
   pragma Import (C, Const_Extract_Element, "LLVMConstExtractElement");

   function Const_Insert_Element
     (Vector_Constant : LLVM.Types.Value_T;
      Element_Value_Constant : LLVM.Types.Value_T;
      Index_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1775
   pragma Import (C, Const_Insert_Element, "LLVMConstInsertElement");

   function Const_Shuffle_Vector
     (Vector_A_Constant : LLVM.Types.Value_T;
      Vector_B_Constant : LLVM.Types.Value_T;
      Mask_Constant : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1778
   pragma Import (C, Const_Shuffle_Vector, "LLVMConstShuffleVector");

   function Const_Extract_Value
     (Agg_Constant : LLVM.Types.Value_T;
      Idx_List : access unsigned;
      Num_Idx : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1781
   pragma Import (C, Const_Extract_Value, "LLVMConstExtractValue");

   function Const_Insert_Value
     (Agg_Constant : LLVM.Types.Value_T;
      Element_Value_Constant : LLVM.Types.Value_T;
      Idx_List : access unsigned;
      Num_Idx : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1783
   pragma Import (C, Const_Insert_Value, "LLVMConstInsertValue");

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

   function Block_Address (F : LLVM.Types.Value_T; BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1789
   pragma Import (C, Block_Address, "LLVMBlockAddress");

   function Get_Global_Parent (Global : LLVM.Types.Value_T) return LLVM.Types.Module_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1806
   pragma Import (C, Get_Global_Parent, "LLVMGetGlobalParent");

   function Is_Declaration
     (Global : LLVM.Types.Value_T)
      return Boolean;
   function Is_Declaration_C
     (Global : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1807
   pragma Import (C, Is_Declaration_C, "LLVMIsDeclaration");

   function Get_Linkage (Global : LLVM.Types.Value_T) return Linkage_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1808
   pragma Import (C, Get_Linkage, "LLVMGetLinkage");

   procedure Set_Linkage (Global : LLVM.Types.Value_T; Linkage : Linkage_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1809
   pragma Import (C, Set_Linkage, "LLVMSetLinkage");

   function Get_Section
     (Global : LLVM.Types.Value_T)
      return String;
   function Get_Section_C
     (Global : LLVM.Types.Value_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1810
   pragma Import (C, Get_Section_C, "LLVMGetSection");

   procedure Set_Section
     (Global  : LLVM.Types.Value_T;
      Section : String);
   procedure Set_Section_C
     (Global  : LLVM.Types.Value_T;
      Section : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1811
   pragma Import (C, Set_Section_C, "LLVMSetSection");

   function Get_Visibility (Global : LLVM.Types.Value_T) return Visibility_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1812
   pragma Import (C, Get_Visibility, "LLVMGetVisibility");

   procedure Set_Visibility (Global : LLVM.Types.Value_T; Viz : Visibility_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1813
   pragma Import (C, Set_Visibility, "LLVMSetVisibility");

   function Get_DLL_Storage_Class (Global : LLVM.Types.Value_T) return DLL_Storage_Class_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1814
   pragma Import (C, Get_DLL_Storage_Class, "LLVMGetDLLStorageClass");

   procedure Set_DLL_Storage_Class (Global : LLVM.Types.Value_T; Class : DLL_Storage_Class_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1815
   pragma Import (C, Set_DLL_Storage_Class, "LLVMSetDLLStorageClass");

   function Has_Unnamed_Addr
     (Global : LLVM.Types.Value_T)
      return Boolean;
   function Has_Unnamed_Addr_C
     (Global : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1816
   pragma Import (C, Has_Unnamed_Addr_C, "LLVMHasUnnamedAddr");

   procedure Set_Unnamed_Addr
     (Global           : LLVM.Types.Value_T;
      Has_Unnamed_Addr : Boolean);
   procedure Set_Unnamed_Addr_C
     (Global           : LLVM.Types.Value_T;
      Has_Unnamed_Addr : LLVM.Types.Bool_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1817
   pragma Import (C, Set_Unnamed_Addr_C, "LLVMSetUnnamedAddr");

   function Get_Alignment (V : LLVM.Types.Value_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1833
   pragma Import (C, Get_Alignment, "LLVMGetAlignment");

   procedure Set_Alignment (V : LLVM.Types.Value_T; Bytes : unsigned);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1842
   pragma Import (C, Set_Alignment, "LLVMSetAlignment");

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
      return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1861
   pragma Import (C, Get_Named_Global_C, "LLVMGetNamedGlobal");

   function Get_First_Global (M : LLVM.Types.Module_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1862
   pragma Import (C, Get_First_Global, "LLVMGetFirstGlobal");

   function Get_Last_Global (M : LLVM.Types.Module_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1863
   pragma Import (C, Get_Last_Global, "LLVMGetLastGlobal");

   function Get_Next_Global (Global_Var : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1864
   pragma Import (C, Get_Next_Global, "LLVMGetNextGlobal");

   function Get_Previous_Global (Global_Var : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1865
   pragma Import (C, Get_Previous_Global, "LLVMGetPreviousGlobal");

   procedure Delete_Global (Global_Var : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1866
   pragma Import (C, Delete_Global, "LLVMDeleteGlobal");

   function Get_Initializer (Global_Var : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1867
   pragma Import (C, Get_Initializer, "LLVMGetInitializer");

   procedure Set_Initializer (Global_Var : LLVM.Types.Value_T; Constant_Val : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1868
   pragma Import (C, Set_Initializer, "LLVMSetInitializer");

   function Is_Thread_Local
     (Global_Var : LLVM.Types.Value_T)
      return Boolean;
   function Is_Thread_Local_C
     (Global_Var : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1869
   pragma Import (C, Is_Thread_Local_C, "LLVMIsThreadLocal");

   procedure Set_Thread_Local
     (Global_Var      : LLVM.Types.Value_T;
      Is_Thread_Local : Boolean);
   procedure Set_Thread_Local_C
     (Global_Var      : LLVM.Types.Value_T;
      Is_Thread_Local : LLVM.Types.Bool_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1870
   pragma Import (C, Set_Thread_Local_C, "LLVMSetThreadLocal");

   function Is_Global_Constant
     (Global_Var : LLVM.Types.Value_T)
      return Boolean;
   function Is_Global_Constant_C
     (Global_Var : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1871
   pragma Import (C, Is_Global_Constant_C, "LLVMIsGlobalConstant");

   procedure Set_Global_Constant
     (Global_Var  : LLVM.Types.Value_T;
      Is_Constant : Boolean);
   procedure Set_Global_Constant_C
     (Global_Var  : LLVM.Types.Value_T;
      Is_Constant : LLVM.Types.Bool_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1872
   pragma Import (C, Set_Global_Constant_C, "LLVMSetGlobalConstant");

   function Get_Thread_Local_Mode (Global_Var : LLVM.Types.Value_T) return Thread_Local_Mode_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1873
   pragma Import (C, Get_Thread_Local_Mode, "LLVMGetThreadLocalMode");

   procedure Set_Thread_Local_Mode (Global_Var : LLVM.Types.Value_T; Mode : Thread_Local_Mode_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1874
   pragma Import (C, Set_Thread_Local_Mode, "LLVMSetThreadLocalMode");

   function Is_Externally_Initialized
     (Global_Var : LLVM.Types.Value_T)
      return Boolean;
   function Is_Externally_Initialized_C
     (Global_Var : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1875
   pragma Import (C, Is_Externally_Initialized_C, "LLVMIsExternallyInitialized");

   procedure Set_Externally_Initialized
     (Global_Var  : LLVM.Types.Value_T;
      Is_Ext_Init : Boolean);
   procedure Set_Externally_Initialized_C
     (Global_Var  : LLVM.Types.Value_T;
      Is_Ext_Init : LLVM.Types.Bool_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1876
   pragma Import (C, Set_Externally_Initialized_C, "LLVMSetExternallyInitialized");

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

   procedure Delete_Function (Fn : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1914
   pragma Import (C, Delete_Function, "LLVMDeleteFunction");

   function Has_Personality_Fn
     (Fn : LLVM.Types.Value_T)
      return Boolean;
   function Has_Personality_Fn_C
     (Fn : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1921
   pragma Import (C, Has_Personality_Fn_C, "LLVMHasPersonalityFn");

   function Get_Personality_Fn (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1928
   pragma Import (C, Get_Personality_Fn, "LLVMGetPersonalityFn");

   procedure Set_Personality_Fn (Fn : LLVM.Types.Value_T; Personality_Fn : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1935
   pragma Import (C, Set_Personality_Fn, "LLVMSetPersonalityFn");

   function Get_Intrinsic_ID (Fn : LLVM.Types.Value_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1942
   pragma Import (C, Get_Intrinsic_ID, "LLVMGetIntrinsicID");

   function Get_Function_Call_Conv (Fn : LLVM.Types.Value_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1951
   pragma Import (C, Get_Function_Call_Conv, "LLVMGetFunctionCallConv");

   procedure Set_Function_Call_Conv (Fn : LLVM.Types.Value_T; CC : unsigned);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1961
   pragma Import (C, Set_Function_Call_Conv, "LLVMSetFunctionCallConv");

   function Get_GC
     (Fn : LLVM.Types.Value_T)
      return String;
   function Get_GC_C
     (Fn : LLVM.Types.Value_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1969
   pragma Import (C, Get_GC_C, "LLVMGetGC");

   procedure Set_GC
     (Fn   : LLVM.Types.Value_T;
      Name : String);
   procedure Set_GC_C
     (Fn   : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1976
   pragma Import (C, Set_GC_C, "LLVMSetGC");

   procedure Add_Attribute_At_Index
     (F : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      A : LLVM.Types.Attribute_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1983
   pragma Import (C, Add_Attribute_At_Index, "LLVMAddAttributeAtIndex");

   function Get_Attribute_Count_At_Index (F : LLVM.Types.Value_T; Idx : Attribute_Index_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1985
   pragma Import (C, Get_Attribute_Count_At_Index, "LLVMGetAttributeCountAtIndex");

   procedure Get_Attributes_At_Index
     (F : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Attrs : System.Address);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1986
   pragma Import (C, Get_Attributes_At_Index, "LLVMGetAttributesAtIndex");

   function Get_Enum_Attribute_At_Index
     (F : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Kind_ID : unsigned) return LLVM.Types.Attribute_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1988
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
      Kind_ID : unsigned);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:1994
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

procedure Add_Target_Dependent_Function_Attr
     (Fn : LLVM.Types.Value_T;
      A  : String;
      V  : String);
   procedure Add_Target_Dependent_Function_Attr_C
     (Fn : LLVM.Types.Value_T;
      A  : Interfaces.C.Strings.chars_ptr;
      V  : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Add_Target_Dependent_Function_Attr_C, "LLVMAddTargetDependentFunctionAttr");

   function Count_Params (Fn : LLVM.Types.Value_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2022
   pragma Import (C, Count_Params, "LLVMCountParams");

   procedure Get_Params (Fn : LLVM.Types.Value_T; Params : System.Address);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2035
   pragma Import (C, Get_Params, "LLVMGetParams");

   function Get_Param (Fn : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2044
   pragma Import (C, Get_Param, "LLVMGetParam");

   function Get_Param_Parent (Inst : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2055
   pragma Import (C, Get_Param_Parent, "LLVMGetParamParent");

   function Get_First_Param (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2062
   pragma Import (C, Get_First_Param, "LLVMGetFirstParam");

   function Get_Last_Param (Fn : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2069
   pragma Import (C, Get_Last_Param, "LLVMGetLastParam");

   function Get_Next_Param (Arg : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2078
   pragma Import (C, Get_Next_Param, "LLVMGetNextParam");

   function Get_Previous_Param (Arg : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2085
   pragma Import (C, Get_Previous_Param, "LLVMGetPreviousParam");

   procedure Set_Param_Alignment (Arg : LLVM.Types.Value_T; Align : unsigned);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2093
   pragma Import (C, Set_Param_Alignment, "LLVMSetParamAlignment");

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

   function MD_String
     (Str   : String;
      S_Len : unsigned)
      return LLVM.Types.Value_T;
   function MD_String_C
     (Str   : Interfaces.C.Strings.chars_ptr;
      S_Len : unsigned)
      return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2132
   pragma Import (C, MD_String_C, "LLVMMDString");

   function MD_Node_In_Context
     (C : LLVM.Types.Context_T;
      Vals : System.Address;
      Count : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2139
   pragma Import (C, MD_Node_In_Context, "LLVMMDNodeInContext");

   function MD_Node (Vals : System.Address; Count : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2145
   pragma Import (C, MD_Node, "LLVMMDNode");

   function Metadata_As_Value (C : LLVM.Types.Context_T; MD : LLVM.Types.Metadata_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2150
   pragma Import (C, Metadata_As_Value, "LLVMMetadataAsValue");

   function Value_As_Metadata (Val : LLVM.Types.Value_T) return LLVM.Types.Metadata_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2155
   pragma Import (C, Value_As_Metadata, "LLVMValueAsMetadata");

   function Get_MD_String
     (V      : LLVM.Types.Value_T;
      Length : unsigned)
      return String;
   function Get_MD_String_C
     (V      : LLVM.Types.Value_T;
      Length : unsigned)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2164
   pragma Import (C, Get_MD_String_C, "LLVMGetMDString");

   function Get_MD_Node_Num_Operands (V : LLVM.Types.Value_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2172
   pragma Import (C, Get_MD_Node_Num_Operands, "LLVMGetMDNodeNumOperands");

   procedure Get_MD_Node_Operands (V : LLVM.Types.Value_T; Dest : System.Address);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2185
   pragma Import (C, Get_MD_Node_Operands, "LLVMGetMDNodeOperands");

   function Basic_Block_As_Value (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2211
   pragma Import (C, Basic_Block_As_Value, "LLVMBasicBlockAsValue");

   function Value_Is_Basic_Block
     (Val : LLVM.Types.Value_T)
      return Boolean;
   function Value_Is_Basic_Block_C
     (Val : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2216
   pragma Import (C, Value_Is_Basic_Block_C, "LLVMValueIsBasicBlock");

   function Value_As_Basic_Block (Val : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2221
   pragma Import (C, Value_As_Basic_Block, "LLVMValueAsBasicBlock");

   function Get_Basic_Block_Name
     (BB : LLVM.Types.Basic_Block_T)
      return String;
   function Get_Basic_Block_Name_C
     (BB : LLVM.Types.Basic_Block_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2226
   pragma Import (C, Get_Basic_Block_Name_C, "LLVMGetBasicBlockName");

   function Get_Basic_Block_Parent (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2233
   pragma Import (C, Get_Basic_Block_Parent, "LLVMGetBasicBlockParent");

   function Get_Basic_Block_Terminator (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2245
   pragma Import (C, Get_Basic_Block_Terminator, "LLVMGetBasicBlockTerminator");

   function Count_Basic_Blocks (Fn : LLVM.Types.Value_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2252
   pragma Import (C, Count_Basic_Blocks, "LLVMCountBasicBlocks");

   procedure Get_Basic_Blocks (Fn : LLVM.Types.Value_T; Basic_Blocks : System.Address);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2262
   pragma Import (C, Get_Basic_Blocks, "LLVMGetBasicBlocks");

   function Get_First_Basic_Block (Fn : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2272
   pragma Import (C, Get_First_Basic_Block, "LLVMGetFirstBasicBlock");

   function Get_Last_Basic_Block (Fn : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2279
   pragma Import (C, Get_Last_Basic_Block, "LLVMGetLastBasicBlock");

   function Get_Next_Basic_Block (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2284
   pragma Import (C, Get_Next_Basic_Block, "LLVMGetNextBasicBlock");

   function Get_Previous_Basic_Block (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2289
   pragma Import (C, Get_Previous_Basic_Block, "LLVMGetPreviousBasicBlock");

   function Get_Entry_Basic_Block (Fn : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2297
   pragma Import (C, Get_Entry_Basic_Block, "LLVMGetEntryBasicBlock");

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

   function Append_Basic_Block
     (Fn   : LLVM.Types.Value_T;
      Name : String)
      return LLVM.Types.Basic_Block_T;
   function Append_Basic_Block_C
     (Fn   : LLVM.Types.Value_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2314
   pragma Import (C, Append_Basic_Block_C, "LLVMAppendBasicBlock");

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

   function Insert_Basic_Block
     (Insert_Before_BB : LLVM.Types.Basic_Block_T;
      Name             : String)
      return LLVM.Types.Basic_Block_T;
   function Insert_Basic_Block_C
     (Insert_Before_BB : LLVM.Types.Basic_Block_T;
      Name             : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2333
   pragma Import (C, Insert_Basic_Block_C, "LLVMInsertBasicBlock");

   procedure Delete_Basic_Block (BB : LLVM.Types.Basic_Block_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2344
   pragma Import (C, Delete_Basic_Block, "LLVMDeleteBasicBlock");

   procedure Remove_Basic_Block_From_Parent (BB : LLVM.Types.Basic_Block_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2354
   pragma Import (C, Remove_Basic_Block_From_Parent, "LLVMRemoveBasicBlockFromParent");

   procedure Move_Basic_Block_Before (BB : LLVM.Types.Basic_Block_T; Move_Pos : LLVM.Types.Basic_Block_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2361
   pragma Import (C, Move_Basic_Block_Before, "LLVMMoveBasicBlockBefore");

   procedure Move_Basic_Block_After (BB : LLVM.Types.Basic_Block_T; Move_Pos : LLVM.Types.Basic_Block_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2368
   pragma Import (C, Move_Basic_Block_After, "LLVMMoveBasicBlockAfter");

   function Get_First_Instruction (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2376
   pragma Import (C, Get_First_Instruction, "LLVMGetFirstInstruction");

   function Get_Last_Instruction (BB : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2383
   pragma Import (C, Get_Last_Instruction, "LLVMGetLastInstruction");

   function Has_Metadata (Val : LLVM.Types.Value_T) return int;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2409
   pragma Import (C, Has_Metadata, "LLVMHasMetadata");

   function Get_Metadata (Val : LLVM.Types.Value_T; Kind_ID : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2414
   pragma Import (C, Get_Metadata, "LLVMGetMetadata");

   procedure Set_Metadata
     (Val : LLVM.Types.Value_T;
      Kind_ID : unsigned;
      Node : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2419
   pragma Import (C, Set_Metadata, "LLVMSetMetadata");

   function Get_Instruction_Parent (Inst : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2426
   pragma Import (C, Get_Instruction_Parent, "LLVMGetInstructionParent");

   function Get_Next_Instruction (Inst : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2436
   pragma Import (C, Get_Next_Instruction, "LLVMGetNextInstruction");

   function Get_Previous_Instruction (Inst : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2444
   pragma Import (C, Get_Previous_Instruction, "LLVMGetPreviousInstruction");

   procedure Instruction_Remove_From_Parent (Inst : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2454
   pragma Import (C, Instruction_Remove_From_Parent, "LLVMInstructionRemoveFromParent");

   procedure Instruction_Erase_From_Parent (Inst : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2464
   pragma Import (C, Instruction_Erase_From_Parent, "LLVMInstructionEraseFromParent");

   function Get_Instruction_Opcode (Inst : LLVM.Types.Value_T) return Opcode_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2471
   pragma Import (C, Get_Instruction_Opcode, "LLVMGetInstructionOpcode");

   function Get_I_Cmp_Predicate (Inst : LLVM.Types.Value_T) return Int_Predicate_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2481
   pragma Import (C, Get_I_Cmp_Predicate, "LLVMGetICmpPredicate");

   function Get_F_Cmp_Predicate (Inst : LLVM.Types.Value_T) return Real_Predicate_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2491
   pragma Import (C, Get_F_Cmp_Predicate, "LLVMGetFCmpPredicate");

   function Instruction_Clone (Inst : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2501
   pragma Import (C, Instruction_Clone, "LLVMInstructionClone");

   function Get_Num_Arg_Operands (Instr : LLVM.Types.Value_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2522
   pragma Import (C, Get_Num_Arg_Operands, "LLVMGetNumArgOperands");

   procedure Set_Instruction_Call_Conv (Instr : LLVM.Types.Value_T; CC : unsigned);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2533
   pragma Import (C, Set_Instruction_Call_Conv, "LLVMSetInstructionCallConv");

   function Get_Instruction_Call_Conv (Instr : LLVM.Types.Value_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2543
   pragma Import (C, Get_Instruction_Call_Conv, "LLVMGetInstructionCallConv");

   procedure Set_Instr_Param_Alignment
     (Instr : LLVM.Types.Value_T;
      index : unsigned;
      Align : unsigned);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2545
   pragma Import (C, Set_Instr_Param_Alignment, "LLVMSetInstrParamAlignment");

   procedure Add_Call_Site_Attribute
     (C : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      A : LLVM.Types.Attribute_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2548
   pragma Import (C, Add_Call_Site_Attribute, "LLVMAddCallSiteAttribute");

   function Get_Call_Site_Attribute_Count (C : LLVM.Types.Value_T; Idx : Attribute_Index_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2550
   pragma Import (C, Get_Call_Site_Attribute_Count, "LLVMGetCallSiteAttributeCount");

   procedure Get_Call_Site_Attributes
     (C : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Attrs : System.Address);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2551
   pragma Import (C, Get_Call_Site_Attributes, "LLVMGetCallSiteAttributes");

   function Get_Call_Site_Enum_Attribute
     (C : LLVM.Types.Value_T;
      Idx : Attribute_Index_T;
      Kind_ID : unsigned) return LLVM.Types.Attribute_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2553
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
      Kind_ID : unsigned);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2559
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

   function Get_Called_Value (Instr : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2573
   pragma Import (C, Get_Called_Value, "LLVMGetCalledValue");

   function Is_Tail_Call
     (Call_Inst : LLVM.Types.Value_T)
      return Boolean;
   function Is_Tail_Call_C
     (Call_Inst : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2582
   pragma Import (C, Is_Tail_Call_C, "LLVMIsTailCall");

   procedure Set_Tail_Call
     (Call_Inst    : LLVM.Types.Value_T;
      Is_Tail_Call : Boolean);
   procedure Set_Tail_Call_C
     (Call_Inst    : LLVM.Types.Value_T;
      Is_Tail_Call : LLVM.Types.Bool_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2591
   pragma Import (C, Set_Tail_Call_C, "LLVMSetTailCall");

   function Get_Normal_Dest (Invoke_Inst : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2600
   pragma Import (C, Get_Normal_Dest, "LLVMGetNormalDest");

   function Get_Unwind_Dest (Invoke_Inst : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2609
   pragma Import (C, Get_Unwind_Dest, "LLVMGetUnwindDest");

   procedure Set_Normal_Dest (Invoke_Inst : LLVM.Types.Value_T; B : LLVM.Types.Basic_Block_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2618
   pragma Import (C, Set_Normal_Dest, "LLVMSetNormalDest");

   procedure Set_Unwind_Dest (Invoke_Inst : LLVM.Types.Value_T; B : LLVM.Types.Basic_Block_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2627
   pragma Import (C, Set_Unwind_Dest, "LLVMSetUnwindDest");

   function Get_Num_Successors (Term : LLVM.Types.Value_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2647
   pragma Import (C, Get_Num_Successors, "LLVMGetNumSuccessors");

   function Get_Successor (Term : LLVM.Types.Value_T; i : unsigned) return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2654
   pragma Import (C, Get_Successor, "LLVMGetSuccessor");

   procedure Set_Successor
     (Term : LLVM.Types.Value_T;
      i : unsigned;
      block : LLVM.Types.Basic_Block_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2661
   pragma Import (C, Set_Successor, "LLVMSetSuccessor");

   function Is_Conditional
     (Branch : LLVM.Types.Value_T)
      return Boolean;
   function Is_Conditional_C
     (Branch : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2670
   pragma Import (C, Is_Conditional_C, "LLVMIsConditional");

   function Get_Condition (Branch : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2679
   pragma Import (C, Get_Condition, "LLVMGetCondition");

   procedure Set_Condition (Branch : LLVM.Types.Value_T; Cond : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2688
   pragma Import (C, Set_Condition, "LLVMSetCondition");

   function Get_Switch_Default_Dest (Switch_Instr : LLVM.Types.Value_T) return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2697
   pragma Import (C, Get_Switch_Default_Dest, "LLVMGetSwitchDefaultDest");

   function Get_Allocated_Type (Alloca : LLVM.Types.Value_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2715
   pragma Import (C, Get_Allocated_Type, "LLVMGetAllocatedType");

   function Is_In_Bounds
     (GEP : LLVM.Types.Value_T)
      return Boolean;
   function Is_In_Bounds_C
     (GEP : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2733
   pragma Import (C, Is_In_Bounds_C, "LLVMIsInBounds");

   procedure Set_Is_In_Bounds
     (GEP       : LLVM.Types.Value_T;
      In_Bounds : Boolean);
   procedure Set_Is_In_Bounds_C
     (GEP       : LLVM.Types.Value_T;
      In_Bounds : LLVM.Types.Bool_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2738
   pragma Import (C, Set_Is_In_Bounds_C, "LLVMSetIsInBounds");

   procedure Add_Incoming
     (Phi_Node : LLVM.Types.Value_T;
      Incoming_Values : System.Address;
      Incoming_Blocks : System.Address;
      Count : unsigned);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2756
   pragma Import (C, Add_Incoming, "LLVMAddIncoming");

   function Count_Incoming (Phi_Node : LLVM.Types.Value_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2762
   pragma Import (C, Count_Incoming, "LLVMCountIncoming");

   function Get_Incoming_Value (Phi_Node : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2767
   pragma Import (C, Get_Incoming_Value, "LLVMGetIncomingValue");

   function Get_Incoming_Block (Phi_Node : LLVM.Types.Value_T; Index : unsigned) return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2772
   pragma Import (C, Get_Incoming_Block, "LLVMGetIncomingBlock");

   function Get_Num_Indices (Inst : LLVM.Types.Value_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2792
   pragma Import (C, Get_Num_Indices, "LLVMGetNumIndices");

   function Get_Indices (Inst : LLVM.Types.Value_T) return access unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2797
   pragma Import (C, Get_Indices, "LLVMGetIndices");

   function Create_Builder_In_Context (C : LLVM.Types.Context_T) return LLVM.Types.Builder_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2820
   pragma Import (C, Create_Builder_In_Context, "LLVMCreateBuilderInContext");

   function Create_Builder return LLVM.Types.Builder_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2821
   pragma Import (C, Create_Builder, "LLVMCreateBuilder");

   procedure Position_Builder
     (Builder : LLVM.Types.Builder_T;
      Block : LLVM.Types.Basic_Block_T;
      Instr : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2822
   pragma Import (C, Position_Builder, "LLVMPositionBuilder");

   procedure Position_Builder_Before (Builder : LLVM.Types.Builder_T; Instr : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2824
   pragma Import (C, Position_Builder_Before, "LLVMPositionBuilderBefore");

   procedure Position_Builder_At_End (Builder : LLVM.Types.Builder_T; Block : LLVM.Types.Basic_Block_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2825
   pragma Import (C, Position_Builder_At_End, "LLVMPositionBuilderAtEnd");

   function Get_Insert_Block (Builder : LLVM.Types.Builder_T) return LLVM.Types.Basic_Block_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2826
   pragma Import (C, Get_Insert_Block, "LLVMGetInsertBlock");

   procedure Clear_Insertion_Position (Builder : LLVM.Types.Builder_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2827
   pragma Import (C, Clear_Insertion_Position, "LLVMClearInsertionPosition");

   procedure Insert_Into_Builder (Builder : LLVM.Types.Builder_T; Instr : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2828
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

   procedure Dispose_Builder (Builder : LLVM.Types.Builder_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2831
   pragma Import (C, Dispose_Builder, "LLVMDisposeBuilder");

   procedure Set_Current_Debug_Location (Builder : LLVM.Types.Builder_T; L : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2834
   pragma Import (C, Set_Current_Debug_Location, "LLVMSetCurrentDebugLocation");

   function Get_Current_Debug_Location (Builder : LLVM.Types.Builder_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2835
   pragma Import (C, Get_Current_Debug_Location, "LLVMGetCurrentDebugLocation");

   procedure Set_Inst_Debug_Location (Builder : LLVM.Types.Builder_T; Inst : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2836
   pragma Import (C, Set_Inst_Debug_Location, "LLVMSetInstDebugLocation");

   function Build_Ret_Void (arg1 : LLVM.Types.Builder_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2839
   pragma Import (C, Build_Ret_Void, "LLVMBuildRetVoid");

   function Build_Ret (arg1 : LLVM.Types.Builder_T; V : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2840
   pragma Import (C, Build_Ret, "LLVMBuildRet");

   function Build_Aggregate_Ret
     (arg1 : LLVM.Types.Builder_T;
      Ret_Vals : System.Address;
      N : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2841
   pragma Import (C, Build_Aggregate_Ret, "LLVMBuildAggregateRet");

   function Build_Br (arg1 : LLVM.Types.Builder_T; Dest : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2843
   pragma Import (C, Build_Br, "LLVMBuildBr");

   function Build_Cond_Br
     (arg1 : LLVM.Types.Builder_T;
      C_If : LLVM.Types.Value_T;
      C_Then : LLVM.Types.Basic_Block_T;
      C_Else : LLVM.Types.Basic_Block_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2844
   pragma Import (C, Build_Cond_Br, "LLVMBuildCondBr");

   function Build_Switch
     (arg1 : LLVM.Types.Builder_T;
      V : LLVM.Types.Value_T;
      C_Else : LLVM.Types.Basic_Block_T;
      Num_Cases : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2846
   pragma Import (C, Build_Switch, "LLVMBuildSwitch");

   function Build_Indirect_Br
     (B : LLVM.Types.Builder_T;
      Addr : LLVM.Types.Value_T;
      Num_Dests : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2848
   pragma Import (C, Build_Indirect_Br, "LLVMBuildIndirectBr");

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

   function Build_Resume (B : LLVM.Types.Builder_T; Exn : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2857
   pragma Import (C, Build_Resume, "LLVMBuildResume");

   function Build_Unreachable (arg1 : LLVM.Types.Builder_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2858
   pragma Import (C, Build_Unreachable, "LLVMBuildUnreachable");

   procedure Add_Case
     (Switch : LLVM.Types.Value_T;
      On_Val : LLVM.Types.Value_T;
      Dest : LLVM.Types.Basic_Block_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2861
   pragma Import (C, Add_Case, "LLVMAddCase");

   procedure Add_Destination (Indirect_Br : LLVM.Types.Value_T; Dest : LLVM.Types.Basic_Block_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2865
   pragma Import (C, Add_Destination, "LLVMAddDestination");

   function Get_Num_Clauses (Landing_Pad : LLVM.Types.Value_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2868
   pragma Import (C, Get_Num_Clauses, "LLVMGetNumClauses");

   function Get_Clause (Landing_Pad : LLVM.Types.Value_T; Idx : unsigned) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2871
   pragma Import (C, Get_Clause, "LLVMGetClause");

   procedure Add_Clause (Landing_Pad : LLVM.Types.Value_T; Clause_Val : LLVM.Types.Value_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2874
   pragma Import (C, Add_Clause, "LLVMAddClause");

   function Is_Cleanup
     (Landing_Pad : LLVM.Types.Value_T)
      return Boolean;
   function Is_Cleanup_C
     (Landing_Pad : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2877
   pragma Import (C, Is_Cleanup_C, "LLVMIsCleanup");

   procedure Set_Cleanup
     (Landing_Pad : LLVM.Types.Value_T;
      Val         : Boolean);
   procedure Set_Cleanup_C
     (Landing_Pad : LLVM.Types.Value_T;
      Val         : LLVM.Types.Bool_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2880
   pragma Import (C, Set_Cleanup_C, "LLVMSetCleanup");

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

   function Build_Free (arg1 : LLVM.Types.Builder_T; Pointer_Val : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2953
   pragma Import (C, Build_Free, "LLVMBuildFree");

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

   function Build_Store
     (arg1 : LLVM.Types.Builder_T;
      Val : LLVM.Types.Value_T;
      Ptr : LLVM.Types.Value_T) return LLVM.Types.Value_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2956
   pragma Import (C, Build_Store, "LLVMBuildStore");

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
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2969
   pragma Import (C, Get_Volatile_C, "LLVMGetVolatile");

   procedure Set_Volatile
     (Memory_Access_Inst : LLVM.Types.Value_T;
      Is_Volatile        : Boolean);
   procedure Set_Volatile_C
     (Memory_Access_Inst : LLVM.Types.Value_T;
      Is_Volatile        : LLVM.Types.Bool_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2970
   pragma Import (C, Set_Volatile_C, "LLVMSetVolatile");

   function Get_Ordering (Memory_Access_Inst : LLVM.Types.Value_T) return Atomic_Ordering_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2971
   pragma Import (C, Get_Ordering, "LLVMGetOrdering");

   procedure Set_Ordering (Memory_Access_Inst : LLVM.Types.Value_T; Ordering : Atomic_Ordering_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:2972
   pragma Import (C, Set_Ordering, "LLVMSetOrdering");

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
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3066
   pragma Import (C, Is_Atomic_Single_Thread_C, "LLVMIsAtomicSingleThread");

   procedure Set_Atomic_Single_Thread
     (Atomic_Inst   : LLVM.Types.Value_T;
      Single_Thread : Boolean);
   procedure Set_Atomic_Single_Thread_C
     (Atomic_Inst   : LLVM.Types.Value_T;
      Single_Thread : LLVM.Types.Bool_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3067
   pragma Import (C, Set_Atomic_Single_Thread_C, "LLVMSetAtomicSingleThread");

   function Get_Cmp_Xchg_Success_Ordering (Cmp_Xchg_Inst : LLVM.Types.Value_T) return Atomic_Ordering_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3069
   pragma Import (C, Get_Cmp_Xchg_Success_Ordering, "LLVMGetCmpXchgSuccessOrdering");

   procedure Set_Cmp_Xchg_Success_Ordering (Cmp_Xchg_Inst : LLVM.Types.Value_T; Ordering : Atomic_Ordering_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3070
   pragma Import (C, Set_Cmp_Xchg_Success_Ordering, "LLVMSetCmpXchgSuccessOrdering");

   function Get_Cmp_Xchg_Failure_Ordering (Cmp_Xchg_Inst : LLVM.Types.Value_T) return Atomic_Ordering_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3072
   pragma Import (C, Get_Cmp_Xchg_Failure_Ordering, "LLVMGetCmpXchgFailureOrdering");

   procedure Set_Cmp_Xchg_Failure_Ordering (Cmp_Xchg_Inst : LLVM.Types.Value_T; Ordering : Atomic_Ordering_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3073
   pragma Import (C, Set_Cmp_Xchg_Failure_Ordering, "LLVMSetCmpXchgFailureOrdering");

   function Create_Module_Provider_For_Existing_Module (M : LLVM.Types.Module_T) return LLVM.Types.Module_Provider_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3091
   pragma Import (C, Create_Module_Provider_For_Existing_Module, "LLVMCreateModuleProviderForExistingModule");

   procedure Dispose_Module_Provider (M : LLVM.Types.Module_Provider_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3096
   pragma Import (C, Dispose_Module_Provider, "LLVMDisposeModuleProvider");

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
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3111
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
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3120
   pragma Import (C, Get_Buffer_Start_C, "LLVMGetBufferStart");

   function Get_Buffer_Size (Mem_Buf : LLVM.Types.Memory_Buffer_T) return stddef_h.size_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3121
   pragma Import (C, Get_Buffer_Size, "LLVMGetBufferSize");

   procedure Dispose_Memory_Buffer (Mem_Buf : LLVM.Types.Memory_Buffer_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3122
   pragma Import (C, Dispose_Memory_Buffer, "LLVMDisposeMemoryBuffer");

   function Get_Global_Pass_Registry return LLVM.Types.Pass_Registry_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3136
   pragma Import (C, Get_Global_Pass_Registry, "LLVMGetGlobalPassRegistry");

   function Create_Pass_Manager return LLVM.Types.Pass_Manager_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3151
   pragma Import (C, Create_Pass_Manager, "LLVMCreatePassManager");

   function Create_Function_Pass_Manager_For_Module (M : LLVM.Types.Module_T) return LLVM.Types.Pass_Manager_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3157
   pragma Import (C, Create_Function_Pass_Manager_For_Module, "LLVMCreateFunctionPassManagerForModule");

   function Create_Function_Pass_Manager (MP : LLVM.Types.Module_Provider_T) return LLVM.Types.Pass_Manager_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3160
   pragma Import (C, Create_Function_Pass_Manager, "LLVMCreateFunctionPassManager");

   function Run_Pass_Manager
     (PM : LLVM.Types.Pass_Manager_T;
      M  : LLVM.Types.Module_T)
      return Boolean;
   function Run_Pass_Manager_C
     (PM : LLVM.Types.Pass_Manager_T;
      M  : LLVM.Types.Module_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3166
   pragma Import (C, Run_Pass_Manager_C, "LLVMRunPassManager");

   function Initialize_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T)
      return Boolean;
   function Initialize_Function_Pass_Manager_C
     (FPM : LLVM.Types.Pass_Manager_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3171
   pragma Import (C, Initialize_Function_Pass_Manager_C, "LLVMInitializeFunctionPassManager");

   function Run_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T;
      F   : LLVM.Types.Value_T)
      return Boolean;
   function Run_Function_Pass_Manager_C
     (FPM : LLVM.Types.Pass_Manager_T;
      F   : LLVM.Types.Value_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3177
   pragma Import (C, Run_Function_Pass_Manager_C, "LLVMRunFunctionPassManager");

   function Finalize_Function_Pass_Manager
     (FPM : LLVM.Types.Pass_Manager_T)
      return Boolean;
   function Finalize_Function_Pass_Manager_C
     (FPM : LLVM.Types.Pass_Manager_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3182
   pragma Import (C, Finalize_Function_Pass_Manager_C, "LLVMFinalizeFunctionPassManager");

   procedure Dispose_Pass_Manager (PM : LLVM.Types.Pass_Manager_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3187
   pragma Import (C, Dispose_Pass_Manager, "LLVMDisposePassManager");

   function Start_Multithreaded
      return Boolean;
   function Start_Multithreaded_C
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3204
   pragma Import (C, Start_Multithreaded_C, "LLVMStartMultithreaded");

   procedure Stop_Multithreaded;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3208
   pragma Import (C, Stop_Multithreaded, "LLVMStopMultithreaded");

   function Is_Multithreaded
      return Boolean;
   function Is_Multithreaded_C
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Core.h:3212
   pragma Import (C, Is_Multithreaded_C, "LLVMIsMultithreaded");

end LLVM.Core;

