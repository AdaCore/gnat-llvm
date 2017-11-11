pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with stdint_h;
with Interfaces.C.Strings;
with LLVM.Types;
with LLVM.Target_Machine;

package LLVM.Orc_Bindings is

   --  skipped empty struct LLVMOpaqueSharedModule

   type Shared_Module_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:32

   --  skipped empty struct LLVMOpaqueSharedObjectBuffer

   type Shared_Object_Buffer_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:33

   --  skipped empty struct LLVMOrcOpaqueJITStack

   type Orc_JIT_Stack_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:34

   subtype Orc_Module_Handle_T is stdint_h.uint32_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:35

   subtype Orc_Target_Address_T is stdint_h.uint64_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:36

   type Orc_Symbol_Resolver_Fn_T is access function  (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : System.Address) return stdint_h.uint64_t;
   pragma Convention (C, Orc_Symbol_Resolver_Fn_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:37

   type Orc_Lazy_Compile_Callback_Fn_T is access function  (arg1 : Orc_JIT_Stack_T; arg2 : System.Address) return stdint_h.uint64_t;
   pragma Convention (C, Orc_Lazy_Compile_Callback_Fn_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:38

   type Orc_Error_Code_T is 
     (Orc_Err_Success,
      Orc_Err_Generic);
   pragma Convention (C, Orc_Error_Code_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:41

   function Orc_Make_Shared_Module (C_Mod : LLVM.Types.Module_T) return Shared_Module_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:58
   pragma Import (C, Orc_Make_Shared_Module, "LLVMOrcMakeSharedModule");

   procedure Orc_Dispose_Shared_Module_Ref (Shared_Mod : Shared_Module_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:68
   pragma Import (C, Orc_Dispose_Shared_Module_Ref, "LLVMOrcDisposeSharedModuleRef");

   function Orc_Make_Shared_Object_Buffer (Obj_Buffer : LLVM.Types.Memory_Buffer_T) return Shared_Object_Buffer_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:74
   pragma Import (C, Orc_Make_Shared_Object_Buffer, "LLVMOrcMakeSharedObjectBuffer");

   procedure Orc_Dispose_Shared_Object_Buffer_Ref (Shared_Obj_Buffer : Shared_Object_Buffer_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:80
   pragma Import (C, Orc_Dispose_Shared_Object_Buffer_Ref, "LLVMOrcDisposeSharedObjectBufferRef");

   function Orc_Create_Instance (TM : LLVM.Target_Machine.Target_Machine_T) return Orc_JIT_Stack_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:91
   pragma Import (C, Orc_Create_Instance, "LLVMOrcCreateInstance");

   function Orc_Get_Error_Msg
     (JIT_Stack : Orc_JIT_Stack_T)
      return String;
   function Orc_Get_Error_Msg_C
     (JIT_Stack : Orc_JIT_Stack_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:99
   pragma Import (C, Orc_Get_Error_Msg_C, "LLVMOrcGetErrorMsg");

procedure Orc_Get_Mangled_Symbol
     (JIT_Stack      : Orc_JIT_Stack_T;
      Mangled_Symbol : System.Address;
      Symbol         : String);
   procedure Orc_Get_Mangled_Symbol_C
     (JIT_Stack      : Orc_JIT_Stack_T;
      Mangled_Symbol : System.Address;
      Symbol         : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Orc_Get_Mangled_Symbol_C, "LLVMOrcGetMangledSymbol");

   procedure Orc_Dispose_Mangled_Symbol
     (Mangled_Symbol : String);
   procedure Orc_Dispose_Mangled_Symbol_C
     (Mangled_Symbol : Interfaces.C.Strings.chars_ptr);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:111
   pragma Import (C, Orc_Dispose_Mangled_Symbol_C, "LLVMOrcDisposeMangledSymbol");

   function Orc_Create_Lazy_Compile_Callback
     (JIT_Stack : Orc_JIT_Stack_T;
      Ret_Addr : access Orc_Target_Address_T;
      Callback : Orc_Lazy_Compile_Callback_Fn_T;
      Callback_Ctx : System.Address) return Orc_Error_Code_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:117
   pragma Import (C, Orc_Create_Lazy_Compile_Callback, "LLVMOrcCreateLazyCompileCallback");

function Orc_Create_Indirect_Stub
     (JIT_Stack : Orc_JIT_Stack_T;
      Stub_Name : String;
      Init_Addr : Orc_Target_Address_T)
      return Orc_Error_Code_T;
   function Orc_Create_Indirect_Stub_C
     (JIT_Stack : Orc_JIT_Stack_T;
      Stub_Name : Interfaces.C.Strings.chars_ptr;
      Init_Addr : Orc_Target_Address_T)
      return Orc_Error_Code_T;
   pragma Import (C, Orc_Create_Indirect_Stub_C, "LLVMOrcCreateIndirectStub");

function Orc_Set_Indirect_Stub_Pointer
     (JIT_Stack : Orc_JIT_Stack_T;
      Stub_Name : String;
      New_Addr  : Orc_Target_Address_T)
      return Orc_Error_Code_T;
   function Orc_Set_Indirect_Stub_Pointer_C
     (JIT_Stack : Orc_JIT_Stack_T;
      Stub_Name : Interfaces.C.Strings.chars_ptr;
      New_Addr  : Orc_Target_Address_T)
      return Orc_Error_Code_T;
   pragma Import (C, Orc_Set_Indirect_Stub_Pointer_C, "LLVMOrcSetIndirectStubPointer");

   function Orc_Add_Eagerly_Compiled_IR
     (JIT_Stack : Orc_JIT_Stack_T;
      Ret_Handle : access Orc_Module_Handle_T;
      C_Mod : Shared_Module_T;
      Symbol_Resolver : Orc_Symbol_Resolver_Fn_T;
      Symbol_Resolver_Ctx : System.Address) return Orc_Error_Code_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:140
   pragma Import (C, Orc_Add_Eagerly_Compiled_IR, "LLVMOrcAddEagerlyCompiledIR");

   function Orc_Add_Lazily_Compiled_IR
     (JIT_Stack : Orc_JIT_Stack_T;
      Ret_Handle : access Orc_Module_Handle_T;
      C_Mod : Shared_Module_T;
      Symbol_Resolver : Orc_Symbol_Resolver_Fn_T;
      Symbol_Resolver_Ctx : System.Address) return Orc_Error_Code_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:150
   pragma Import (C, Orc_Add_Lazily_Compiled_IR, "LLVMOrcAddLazilyCompiledIR");

   function Orc_Add_Object_File
     (JIT_Stack : Orc_JIT_Stack_T;
      Ret_Handle : access Orc_Module_Handle_T;
      Obj : Shared_Object_Buffer_T;
      Symbol_Resolver : Orc_Symbol_Resolver_Fn_T;
      Symbol_Resolver_Ctx : System.Address) return Orc_Error_Code_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:159
   pragma Import (C, Orc_Add_Object_File, "LLVMOrcAddObjectFile");

   function Orc_Remove_Module (JIT_Stack : Orc_JIT_Stack_T; H : Orc_Module_Handle_T) return Orc_Error_Code_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:171
   pragma Import (C, Orc_Remove_Module, "LLVMOrcRemoveModule");

function Orc_Get_Symbol_Address
     (JIT_Stack   : Orc_JIT_Stack_T;
      Ret_Addr    : Orc_Target_Address_T;
      Symbol_Name : String)
      return Orc_Error_Code_T;
   function Orc_Get_Symbol_Address_C
     (JIT_Stack   : Orc_JIT_Stack_T;
      Ret_Addr    : Orc_Target_Address_T;
      Symbol_Name : Interfaces.C.Strings.chars_ptr)
      return Orc_Error_Code_T;
   pragma Import (C, Orc_Get_Symbol_Address_C, "LLVMOrcGetSymbolAddress");

   function Orc_Dispose_Instance (JIT_Stack : Orc_JIT_Stack_T) return Orc_Error_Code_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/OrcBindings.h:184
   pragma Import (C, Orc_Dispose_Instance, "LLVMOrcDisposeInstance");

end LLVM.Orc_Bindings;

