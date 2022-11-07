pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with LLVM.Orc;
with Interfaces.C.Strings;
with LLVM.Error;
with LLVM.Types;

package LLVM.LLJIT is

  --===----------- llvm-c/LLJIT.h - OrcV2 LLJIT C bindings --------*- C++ -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to the LLJIT class in                 *|
  --|* libLLVMOrcJIT.a, which provides a simple MCJIT-like ORC JIT.               *|
  --|*                                                                            *|
  --|* Many exotic languages can interoperate with C code but have a harder time  *|
  --|* with C++ due to name mangling. So in addition to C, this interface enables *|
  --|* tools written in such languages.                                           *|
  --|*                                                                            *|
  --|* Note: This interface is experimental. It is *NOT* stable, and may be       *|
  --|*       changed without warning. Only C API usage documentation is           *|
  --|*       provided. See the C++ documentation for all higher level ORC API     *|
  --|*       details.                                                             *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCExecutionEngineLLJIT LLJIT
  -- * @ingroup LLVMCExecutionEngine
  -- *
  -- * @{
  --  

  --*
  -- * A function for constructing an ObjectLinkingLayer instance to be used
  -- * by an LLJIT instance.
  -- *
  -- * Clients can call LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator to
  -- * set the creator function to use when constructing an LLJIT instance.
  -- * This can be used to override the default linking layer implementation
  -- * that would otherwise be chosen by LLJITBuilder.
  -- *
  -- * Object linking layers returned by this function will become owned by the
  -- * LLJIT instance. The client is not responsible for managing their lifetimes
  -- * after the function returns.
  --  

   type Orc_LLJIT_Builder_Object_Linking_Layer_Creator_Function_T is access function
        (Arg_1 : System.Address;
         Arg_2 : LLVM.Orc.Orc_Execution_Session_T;
         Arg_3 : Interfaces.C.Strings.chars_ptr) return LLVM.Orc.Orc_Object_Layer_T
   with Convention => C;  -- install/include/llvm-c/LLJIT.h:55

  --*
  -- * A reference to an orc::LLJITBuilder instance.
  --  

   type Orc_Opaque_LLJIT_Builder_Impl_T is null record;   -- incomplete struct

   type Orc_LLJIT_Builder_T is access all Orc_Opaque_LLJIT_Builder_Impl_T;  -- install/include/llvm-c/LLJIT.h:61

  --*
  -- * A reference to an orc::LLJIT instance.
  --  

   type Orc_Opaque_LLJIT_Impl_T is null record;   -- incomplete struct

   type Orc_LLJIT_T is access all Orc_Opaque_LLJIT_Impl_T;  -- install/include/llvm-c/LLJIT.h:66

  --*
  -- * Create an LLVMOrcLLJITBuilder.
  -- *
  -- * The client owns the resulting LLJITBuilder and should dispose of it using
  -- * LLVMOrcDisposeLLJITBuilder once they are done with it.
  --  

   function Orc_Create_LLJIT_Builder return Orc_LLJIT_Builder_T  -- install/include/llvm-c/LLJIT.h:74
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateLLJITBuilder";

  --*
  -- * Dispose of an LLVMOrcLLJITBuilderRef. This should only be called if ownership
  -- * has not been passed to LLVMOrcCreateLLJIT (e.g. because some error prevented
  -- * that function from being called).
  --  

   procedure Orc_Dispose_LLJIT_Builder (Builder : Orc_LLJIT_Builder_T)  -- install/include/llvm-c/LLJIT.h:81
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeLLJITBuilder";

  --*
  -- * Set the JITTargetMachineBuilder to be used when constructing the LLJIT
  -- * instance. Calling this function is optional: if it is not called then the
  -- * LLJITBuilder will use JITTargeTMachineBuilder::detectHost to construct a
  -- * JITTargetMachineBuilder.
  -- *
  -- * This function takes ownership of the JTMB argument: clients should not
  -- * dispose of the JITTargetMachineBuilder after calling this function.
  --  

   procedure Orc_LLJIT_Builder_Set_JIT_Target_Machine_Builder (Builder : Orc_LLJIT_Builder_T; JTMB : LLVM.Orc.Orc_JIT_Target_Machine_Builder_T)  -- install/include/llvm-c/LLJIT.h:92
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITBuilderSetJITTargetMachineBuilder";

  --*
  -- * Set an ObjectLinkingLayer creator function for this LLJIT instance.
  --  

   procedure Orc_LLJIT_Builder_Set_Object_Linking_Layer_Creator
     (Builder : Orc_LLJIT_Builder_T;
      F : Orc_LLJIT_Builder_Object_Linking_Layer_Creator_Function_T;
      Ctx : System.Address)  -- install/include/llvm-c/LLJIT.h:98
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator";

  --*
  -- * Create an LLJIT instance from an LLJITBuilder.
  -- *
  -- * This operation takes ownership of the Builder argument: clients should not
  -- * dispose of the builder after calling this function (even if the function
  -- * returns an error). If a null Builder argument is provided then a
  -- * default-constructed LLJITBuilder will be used.
  -- *
  -- * On success the resulting LLJIT instance is uniquely owned by the client and
  -- * automatically manages the memory of all JIT'd code and all modules that are
  -- * transferred to it (e.g. via LLVMOrcLLJITAddLLVMIRModule). Disposing of the
  -- * LLJIT instance will free all memory managed by the JIT, including JIT'd code
  -- * and not-yet compiled modules.
  --  

   function Orc_Create_LLJIT (Result : System.Address; Builder : Orc_LLJIT_Builder_T) return LLVM.Error.Error_T  -- install/include/llvm-c/LLJIT.h:116
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateLLJIT";

  --*
  -- * Dispose of an LLJIT instance.
  --  

   function Orc_Dispose_LLJIT (J : Orc_LLJIT_T) return LLVM.Error.Error_T  -- install/include/llvm-c/LLJIT.h:122
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeLLJIT";

  --*
  -- * Get a reference to the ExecutionSession for this LLJIT instance.
  -- *
  -- * The ExecutionSession is owned by the LLJIT instance. The client is not
  -- * responsible for managing its memory.
  --  

   function Orc_LLJIT_Get_Execution_Session (J : Orc_LLJIT_T) return LLVM.Orc.Orc_Execution_Session_T  -- install/include/llvm-c/LLJIT.h:130
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITGetExecutionSession";

  --*
  -- * Return a reference to the Main JITDylib.
  -- *
  -- * The JITDylib is owned by the LLJIT instance. The client is not responsible
  -- * for managing its memory.
  --  

   function Orc_LLJIT_Get_Main_JIT_Dylib (J : Orc_LLJIT_T) return LLVM.Orc.Orc_JIT_Dylib_T  -- install/include/llvm-c/LLJIT.h:138
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITGetMainJITDylib";

  --*
  -- * Return the target triple for this LLJIT instance. This string is owned by
  -- * the LLJIT instance and should not be freed by the client.
  --  

function Orc_LLJIT_Get_Triple_String
     (J : Orc_LLJIT_T)
      return String;

  --*
  -- * Returns the global prefix character according to the LLJIT's DataLayout.
  --  

   function Orc_LLJIT_Get_Global_Prefix (J : Orc_LLJIT_T) return char  -- install/include/llvm-c/LLJIT.h:149
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITGetGlobalPrefix";

  --*
  -- * Mangles the given string according to the LLJIT instance's DataLayout, then
  -- * interns the result in the SymbolStringPool and returns a reference to the
  -- * pool entry. Clients should call LLVMOrcReleaseSymbolStringPoolEntry to
  -- * decrement the ref-count on the pool entry once they are finished with this
  -- * value.
  --  

function Orc_LLJIT_Mangle_And_Intern
     (J              : Orc_LLJIT_T;
      Unmangled_Name : String)
      return LLVM.Orc.Orc_Symbol_String_Pool_Entry_T;

  --*
  -- * Add a buffer representing an object file to the given JITDylib in the given
  -- * LLJIT instance. This operation transfers ownership of the buffer to the
  -- * LLJIT instance. The buffer should not be disposed of or referenced once this
  -- * function returns.
  -- *
  -- * Resources associated with the given object will be tracked by the given
  -- * JITDylib's default resource tracker.
  --  

   function Orc_LLJIT_Add_Object_File
     (J : Orc_LLJIT_T;
      JD : LLVM.Orc.Orc_JIT_Dylib_T;
      Obj_Buffer : LLVM.Types.Memory_Buffer_T) return LLVM.Error.Error_T  -- install/include/llvm-c/LLJIT.h:170
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITAddObjectFile";

  --*
  -- * Add a buffer representing an object file to the given ResourceTracker's
  -- * JITDylib in the given LLJIT instance. This operation transfers ownership of
  -- * the buffer to the LLJIT instance. The buffer should not be disposed of or
  -- * referenced once this function returns.
  -- *
  -- * Resources associated with the given object will be tracked by ResourceTracker
  -- * RT.
  --  

   function Orc_LLJIT_Add_Object_File_With_RT
     (J : Orc_LLJIT_T;
      RT : LLVM.Orc.Orc_Resource_Tracker_T;
      Obj_Buffer : LLVM.Types.Memory_Buffer_T) return LLVM.Error.Error_T  -- install/include/llvm-c/LLJIT.h:182
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITAddObjectFileWithRT";

  --*
  -- * Add an IR module to the given JITDylib in the given LLJIT instance. This
  -- * operation transfers ownership of the TSM argument to the LLJIT instance.
  -- * The TSM argument should not be disposed of or referenced once this
  -- * function returns.
  -- *
  -- * Resources associated with the given Module will be tracked by the given
  -- * JITDylib's default resource tracker.
  --  

   function Orc_LLJIT_Add_LLVMIR_Module
     (J : Orc_LLJIT_T;
      JD : LLVM.Orc.Orc_JIT_Dylib_T;
      TSM : LLVM.Orc.Orc_Thread_Safe_Module_T) return LLVM.Error.Error_T  -- install/include/llvm-c/LLJIT.h:195
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITAddLLVMIRModule";

  --*
  -- * Add an IR module to the given ResourceTracker's JITDylib in the given LLJIT
  -- * instance. This operation transfers ownership of the TSM argument to the LLJIT
  -- * instance. The TSM argument should not be disposed of or referenced once this
  -- * function returns.
  -- *
  -- * Resources associated with the given Module will be tracked by ResourceTracker
  -- * RT.
  --  

   function Orc_LLJIT_Add_LLVMIR_Module_With_RT
     (J : Orc_LLJIT_T;
      JD : LLVM.Orc.Orc_Resource_Tracker_T;
      TSM : LLVM.Orc.Orc_Thread_Safe_Module_T) return LLVM.Error.Error_T  -- install/include/llvm-c/LLJIT.h:208
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITAddLLVMIRModuleWithRT";

  --*
  -- * Look up the given symbol in the main JITDylib of the given LLJIT instance.
  -- *
  -- * This operation does not take ownership of the Name argument.
  --  

function Orc_LLJIT_Lookup
     (J      : Orc_LLJIT_T;
      Result : access LLVM.Orc.Orc_Executor_Address_T;
      Name   : String)
      return LLVM.Error.Error_T;

  --*
  -- * Returns a non-owning reference to the LLJIT instance's object linking layer.
  --  

   function Orc_LLJIT_Get_Obj_Linking_Layer (J : Orc_LLJIT_T) return LLVM.Orc.Orc_Object_Layer_T  -- install/include/llvm-c/LLJIT.h:224
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITGetObjLinkingLayer";

  --*
  -- * Returns a non-owning reference to the LLJIT instance's object linking layer.
  --  

   function Orc_LLJIT_Get_Obj_Transform_Layer (J : Orc_LLJIT_T) return LLVM.Orc.Orc_Object_Transform_Layer_T  -- install/include/llvm-c/LLJIT.h:230
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITGetObjTransformLayer";

  --*
  -- * Returns a non-owning reference to the LLJIT instance's IR transform layer.
  --  

   function Orc_LLJIT_Get_IR_Transform_Layer (J : Orc_LLJIT_T) return LLVM.Orc.Orc_IR_Transform_Layer_T  -- install/include/llvm-c/LLJIT.h:235
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITGetIRTransformLayer";

  --*
  -- * Get the LLJIT instance's default data layout string.
  -- *
  -- * This string is owned by the LLJIT instance and does not need to be freed
  -- * by the caller.
  --  

function Orc_LLJIT_Get_Data_Layout_Str
     (J : Orc_LLJIT_T)
      return String;

  --*
  -- * @}
  --  

end LLVM.LLJIT;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
