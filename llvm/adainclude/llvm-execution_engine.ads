pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Target_Machine;
with LLVM.Types;
with Interfaces.C.Extensions;
with System;
with stddef_h;
with Interfaces.C.Strings;
with LLVM.Target;
with stdint_h;

package LLVM.Execution_Engine is

  --===-- llvm-c/ExecutionEngine.h - ExecutionEngine Lib C Iface --*- C++ -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to libLLVMExecutionEngine.o, which    *|
  --|* implements various analyses of the LLVM IR.                                *|
  --|*                                                                            *|
  --|* Many exotic languages can interoperate with C code but have a harder time  *|
  --|* with C++ due to name mangling. So in addition to C, this interface enables *|
  --|* tools written in such languages.                                           *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCExecutionEngine Execution Engine
  -- * @ingroup LLVMC
  -- *
  -- * @{
  --  

   procedure Link_In_MCJIT  -- install/include/llvm-c/ExecutionEngine.h:36
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMLinkInMCJIT";

   procedure Link_In_Interpreter  -- install/include/llvm-c/ExecutionEngine.h:37
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMLinkInInterpreter";

   type Opaque_Generic_Value_Impl_T is null record;   -- incomplete struct

   type Generic_Value_T is access all Opaque_Generic_Value_Impl_T;  -- install/include/llvm-c/ExecutionEngine.h:39

   type Opaque_Execution_Engine_Impl_T is null record;   -- incomplete struct

   type Execution_Engine_T is access all Opaque_Execution_Engine_Impl_T;  -- install/include/llvm-c/ExecutionEngine.h:40

   type Opaque_MCJIT_Memory_Manager_Impl_T is null record;   -- incomplete struct

   type MCJIT_Memory_Manager_T is access all Opaque_MCJIT_Memory_Manager_Impl_T;  -- install/include/llvm-c/ExecutionEngine.h:41

   type MCJIT_Compiler_Options_T is record
      OptLevel : aliased unsigned;  -- install/include/llvm-c/ExecutionEngine.h:44
      CodeModel : aliased LLVM.Target_Machine.Code_Model_T;  -- install/include/llvm-c/ExecutionEngine.h:45
      NoFramePointerElim : aliased LLVM.Types.Bool_T;  -- install/include/llvm-c/ExecutionEngine.h:46
      EnableFastISel : aliased LLVM.Types.Bool_T;  -- install/include/llvm-c/ExecutionEngine.h:47
      MCJMM : MCJIT_Memory_Manager_T;  -- install/include/llvm-c/ExecutionEngine.h:48
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/llvm-c/ExecutionEngine.h:43

  --===-- Operations on generic values --------------------------------------=== 
function Create_Generic_Value_Of_Int
     (Ty        : LLVM.Types.Type_T;
      N         : Extensions.unsigned_long_long;
      Is_Signed : Boolean)
      return Generic_Value_T;

   function Create_Generic_Value_Of_Pointer (P : System.Address) return Generic_Value_T  -- install/include/llvm-c/ExecutionEngine.h:57
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateGenericValueOfPointer";

   function Create_Generic_Value_Of_Float (Ty : LLVM.Types.Type_T; N : double) return Generic_Value_T  -- install/include/llvm-c/ExecutionEngine.h:59
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateGenericValueOfFloat";

   function Generic_Value_Int_Width (Gen_Val_Ref : Generic_Value_T) return unsigned  -- install/include/llvm-c/ExecutionEngine.h:61
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGenericValueIntWidth";

function Generic_Value_To_Int
     (Gen_Val   : Generic_Value_T;
      Is_Signed : Boolean)
      return Extensions.unsigned_long_long;

   function Generic_Value_To_Pointer (Gen_Val : Generic_Value_T) return System.Address  -- install/include/llvm-c/ExecutionEngine.h:66
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGenericValueToPointer";

   function Generic_Value_To_Float (Ty_Ref : LLVM.Types.Type_T; Gen_Val : Generic_Value_T) return double  -- install/include/llvm-c/ExecutionEngine.h:68
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGenericValueToFloat";

   procedure Dispose_Generic_Value (Gen_Val : Generic_Value_T)  -- install/include/llvm-c/ExecutionEngine.h:70
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeGenericValue";

  --===-- Operations on execution engines -----------------------------------=== 
function Create_Execution_Engine_For_Module
     (Out_EE    : System.Address;
      M         : LLVM.Types.Module_T;
      Out_Error : System.Address)
      return Boolean;

function Create_Interpreter_For_Module
     (Out_Interp : System.Address;
      M          : LLVM.Types.Module_T;
      Out_Error  : System.Address)
      return Boolean;

function Create_JIT_Compiler_For_Module
     (Out_JIT   : System.Address;
      M         : LLVM.Types.Module_T;
      Opt_Level : unsigned;
      Out_Error : System.Address)
      return Boolean;

   procedure Initialize_MCJIT_Compiler_Options (Options : access MCJIT_Compiler_Options_T; Size_Of_Options : stddef_h.size_t)  -- install/include/llvm-c/ExecutionEngine.h:87
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeMCJITCompilerOptions";

  --*
  -- * Create an MCJIT execution engine for a module, with the given options. It is
  -- * the responsibility of the caller to ensure that all fields in Options up to
  -- * the given SizeOfOptions are initialized. It is correct to pass a smaller
  -- * value of SizeOfOptions that omits some fields. The canonical way of using
  -- * this is:
  -- *
  -- * LLVMMCJITCompilerOptions options;
  -- * LLVMInitializeMCJITCompilerOptions(&options, sizeof(options));
  -- * ... fill in those options you care about
  -- * LLVMCreateMCJITCompilerForModule(&jit, mod, &options, sizeof(options),
  -- *                                  &error);
  -- *
  -- * Note that this is also correct, though possibly suboptimal:
  -- *
  -- * LLVMCreateMCJITCompilerForModule(&jit, mod, 0, 0, &error);
  --  

function Create_MCJIT_Compiler_For_Module
     (Out_JIT         : System.Address;
      M               : LLVM.Types.Module_T;
      Options         : access MCJIT_Compiler_Options_T;
      Size_Of_Options : stddef_h.size_t;
      Out_Error       : System.Address)
      return Boolean;

   procedure Dispose_Execution_Engine (EE : Execution_Engine_T)  -- install/include/llvm-c/ExecutionEngine.h:112
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeExecutionEngine";

   procedure Run_Static_Constructors (EE : Execution_Engine_T)  -- install/include/llvm-c/ExecutionEngine.h:114
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRunStaticConstructors";

   procedure Run_Static_Destructors (EE : Execution_Engine_T)  -- install/include/llvm-c/ExecutionEngine.h:116
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRunStaticDestructors";

   function Run_Function_As_Main
     (EE : Execution_Engine_T;
      F : LLVM.Types.Value_T;
      Arg_C : unsigned;
      Arg_V : System.Address;
      Env_P : System.Address) return int  -- install/include/llvm-c/ExecutionEngine.h:118
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRunFunctionAsMain";

   function Run_Function
     (EE : Execution_Engine_T;
      F : LLVM.Types.Value_T;
      Num_Args : unsigned;
      Args : System.Address) return Generic_Value_T  -- install/include/llvm-c/ExecutionEngine.h:122
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRunFunction";

   procedure Free_Machine_Code_For_Function (EE : Execution_Engine_T; F : LLVM.Types.Value_T)  -- install/include/llvm-c/ExecutionEngine.h:126
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMFreeMachineCodeForFunction";

   procedure Add_Module (EE : Execution_Engine_T; M : LLVM.Types.Module_T)  -- install/include/llvm-c/ExecutionEngine.h:128
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddModule";

function Remove_Module
     (EE        : Execution_Engine_T;
      M         : LLVM.Types.Module_T;
      Out_Mod   : System.Address;
      Out_Error : System.Address)
      return Boolean;

function Find_Function
     (EE     : Execution_Engine_T;
      Name   : String;
      Out_Fn : System.Address)
      return Boolean;

   function Recompile_And_Relink_Function (EE : Execution_Engine_T; Fn : LLVM.Types.Value_T) return System.Address  -- install/include/llvm-c/ExecutionEngine.h:136
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMRecompileAndRelinkFunction";

   function Get_Execution_Engine_Target_Data (EE : Execution_Engine_T) return LLVM.Target.Target_Data_T  -- install/include/llvm-c/ExecutionEngine.h:139
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetExecutionEngineTargetData";

   function Get_Execution_Engine_Target_Machine (EE : Execution_Engine_T) return LLVM.Target_Machine.Target_Machine_T  -- install/include/llvm-c/ExecutionEngine.h:141
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetExecutionEngineTargetMachine";

   procedure Add_Global_Mapping
     (EE : Execution_Engine_T;
      Global : LLVM.Types.Value_T;
      Addr : System.Address)  -- install/include/llvm-c/ExecutionEngine.h:143
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddGlobalMapping";

   function Get_Pointer_To_Global (EE : Execution_Engine_T; Global : LLVM.Types.Value_T) return System.Address  -- install/include/llvm-c/ExecutionEngine.h:146
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetPointerToGlobal";

function Get_Global_Value_Address
     (EE   : Execution_Engine_T;
      Name : String)
      return stdint_h.uint64_t;

function Get_Function_Address
     (EE   : Execution_Engine_T;
      Name : String)
      return stdint_h.uint64_t;

  --/ Returns true on error, false on success. If true is returned then the error
  --/ message is copied to OutStr and cleared in the ExecutionEngine instance.
function Execution_Engine_Get_Err_Msg
     (EE        : Execution_Engine_T;
      Out_Error : System.Address)
      return Boolean;

  --===-- Operations on memory managers -------------------------------------=== 
   type Memory_Manager_Allocate_Code_Section_Callback_T is access function
        (Arg_1 : System.Address;
         Arg_2 : stdint_h.uintptr_t;
         Arg_3 : unsigned;
         Arg_4 : unsigned;
         Arg_5 : Interfaces.C.Strings.chars_ptr) return access stdint_h.uint8_t
   with Convention => C;  -- install/include/llvm-c/ExecutionEngine.h:159

   type Memory_Manager_Allocate_Data_Section_Callback_T is access function
        (Arg_1 : System.Address;
         Arg_2 : stdint_h.uintptr_t;
         Arg_3 : unsigned;
         Arg_4 : unsigned;
         Arg_5 : Interfaces.C.Strings.chars_ptr;
         Arg_6 : LLVM.Types.Bool_T) return access stdint_h.uint8_t
   with Convention => C;  -- install/include/llvm-c/ExecutionEngine.h:162

   type Memory_Manager_Finalize_Memory_Callback_T is access function (Arg_1 : System.Address; Arg_2 : System.Address) return LLVM.Types.Bool_T
   with Convention => C;  -- install/include/llvm-c/ExecutionEngine.h:165

   type Memory_Manager_Destroy_Callback_T is access procedure (Arg_1 : System.Address)
   with Convention => C;  -- install/include/llvm-c/ExecutionEngine.h:167

  --*
  -- * Create a simple custom MCJIT memory manager. This memory manager can
  -- * intercept allocations in a module-oblivious way. This will return NULL
  -- * if any of the passed functions are NULL.
  -- *
  -- * @param Opaque An opaque client object to pass back to the callbacks.
  -- * @param AllocateCodeSection Allocate a block of memory for executable code.
  -- * @param AllocateDataSection Allocate a block of memory for data.
  -- * @param FinalizeMemory Set page permissions and flush cache. Return 0 on
  -- *   success, 1 on error.
  --  

   function Create_Simple_MCJIT_Memory_Manager
     (Opaque : System.Address;
      Allocate_Code_Section : Memory_Manager_Allocate_Code_Section_Callback_T;
      Allocate_Data_Section : Memory_Manager_Allocate_Data_Section_Callback_T;
      Finalize_Memory : Memory_Manager_Finalize_Memory_Callback_T;
      Destroy : Memory_Manager_Destroy_Callback_T) return MCJIT_Memory_Manager_T  -- install/include/llvm-c/ExecutionEngine.h:180
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateSimpleMCJITMemoryManager";

   procedure Dispose_MCJIT_Memory_Manager (MM : MCJIT_Memory_Manager_T)  -- install/include/llvm-c/ExecutionEngine.h:187
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeMCJITMemoryManager";

  --===-- JIT Event Listener functions -------------------------------------=== 
   function Create_GDB_Registration_Listener return LLVM.Types.JIT_Event_Listener_T  -- install/include/llvm-c/ExecutionEngine.h:191
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateGDBRegistrationListener";

   function Create_Intel_JIT_Event_Listener return LLVM.Types.JIT_Event_Listener_T  -- install/include/llvm-c/ExecutionEngine.h:192
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateIntelJITEventListener";

   function Create_O_Profile_JIT_Event_Listener return LLVM.Types.JIT_Event_Listener_T  -- install/include/llvm-c/ExecutionEngine.h:193
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateOProfileJITEventListener";

   function Create_Perf_JIT_Event_Listener return LLVM.Types.JIT_Event_Listener_T  -- install/include/llvm-c/ExecutionEngine.h:194
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreatePerfJITEventListener";

  --*
  -- * @}
  --  

end LLVM.Execution_Engine;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
