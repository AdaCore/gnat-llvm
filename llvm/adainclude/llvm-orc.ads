pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with stdint_h;
with System;
with Interfaces.C.Strings;
with LLVM.Error;
with LLVM.Types;
with LLVM.Target_Machine;

package LLVM.Orc is

  --===---------------- llvm-c/Orc.h - OrcV2 C bindings -----------*- C++ -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to libLLVMOrcJIT.a, which implements  *|
  --|* JIT compilation of LLVM IR. Minimal documentation of C API specific issues *|
  --|* (especially memory ownership rules) is provided. Core Orc concepts are     *|
  --|* documented in llvm/docs/ORCv2.rst and APIs are documented in the C++       *|
  --|* headers                                                                    *|
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
  -- * Represents an address in the target process.
  --  

   subtype Orc_JIT_Target_Address_T is stdint_h.uint64_t;  -- llvm-11.0.1.src/include/llvm-c/Orc.h:39

  --*
  -- * A reference to an orc::ExecutionSession instance.
  --  

   type Orc_Opaque_Execution_Session_Impl_T is null record;   -- incomplete struct

   type Orc_Execution_Session_T is access all Orc_Opaque_Execution_Session_Impl_T;  -- llvm-11.0.1.src/include/llvm-c/Orc.h:44

  --*
  -- * A reference to an orc::SymbolStringPool table entry.
  --  

   type Orc_Quaque_Symbol_String_Pool_Entry_Ptr_Impl_T is null record;   -- incomplete struct

   type Orc_Symbol_String_Pool_Entry_T is access all Orc_Quaque_Symbol_String_Pool_Entry_Ptr_Impl_T;  -- llvm-11.0.1.src/include/llvm-c/Orc.h:50

  --*
  -- * A reference to an orc::JITDylib instance.
  --  

   type Orc_Opaque_JIT_Dylib_Impl_T is null record;   -- incomplete struct

   type Orc_JIT_Dylib_T is access all Orc_Opaque_JIT_Dylib_Impl_T;  -- llvm-11.0.1.src/include/llvm-c/Orc.h:55

  --*
  -- * A reference to an orc::JITDylib::DefinitionGenerator.
  --  

   type Orc_Opaque_JIT_Dylib_Definition_Generator_Impl_T is null record;   -- incomplete struct

   type Orc_JIT_Dylib_Definition_Generator_T is access all Orc_Opaque_JIT_Dylib_Definition_Generator_Impl_T;  -- llvm-11.0.1.src/include/llvm-c/Orc.h:61

  --*
  -- * Predicate function for SymbolStringPoolEntries.
  --  

   type Orc_Symbol_Predicate_T is access function (Arg_1 : Orc_Symbol_String_Pool_Entry_T; Arg_2 : System.Address) return int
   with Convention => C;  -- llvm-11.0.1.src/include/llvm-c/Orc.h:66

  --*
  -- * A reference to an orc::ThreadSafeContext instance.
  --  

   type Orc_Opaque_Thread_Safe_Context_Impl_T is null record;   -- incomplete struct

   type Orc_Thread_Safe_Context_T is access all Orc_Opaque_Thread_Safe_Context_Impl_T;  -- llvm-11.0.1.src/include/llvm-c/Orc.h:72

  --*
  -- * A reference to an orc::ThreadSafeModule instance.
  --  

   type Orc_Opaque_Thread_Safe_Module_Impl_T is null record;   -- incomplete struct

   type Orc_Thread_Safe_Module_T is access all Orc_Opaque_Thread_Safe_Module_Impl_T;  -- llvm-11.0.1.src/include/llvm-c/Orc.h:77

  --*
  -- * A reference to an orc::JITTargetMachineBuilder instance.
  --  

   type Orc_Opaque_JIT_Target_Machine_Builder_Impl_T is null record;   -- incomplete struct

   type Orc_JIT_Target_Machine_Builder_T is access all Orc_Opaque_JIT_Target_Machine_Builder_Impl_T;  -- llvm-11.0.1.src/include/llvm-c/Orc.h:83

  --*
  -- * A reference to an orc::LLJITBuilder instance.
  --  

   type Orc_Opaque_LLJIT_Builder_Impl_T is null record;   -- incomplete struct

   type Orc_LLJIT_Builder_T is access all Orc_Opaque_LLJIT_Builder_Impl_T;  -- llvm-11.0.1.src/include/llvm-c/Orc.h:88

  --*
  -- * A reference to an orc::LLJIT instance.
  --  

   type Orc_Opaque_LLJIT_Impl_T is null record;   -- incomplete struct

   type Orc_LLJIT_T is access all Orc_Opaque_LLJIT_Impl_T;  -- llvm-11.0.1.src/include/llvm-c/Orc.h:93

  --*
  -- * Intern a string in the ExecutionSession's SymbolStringPool and return a
  -- * reference to it. This increments the ref-count of the pool entry, and the
  -- * returned value should be released once the client is done with it by
  -- * calling LLVMOrReleaseSymbolStringPoolEntry.
  -- *
  -- * Since strings are uniqued within the SymbolStringPool
  -- * LLVMOrcSymbolStringPoolEntryRefs can be compared by value to test string
  -- * equality.
  -- *
  -- * Note that this function does not perform linker-mangling on the string.
  --  

function Orc_Execution_Session_Intern
     (ES   : Orc_Execution_Session_T;
      Name : String)
      return Orc_Symbol_String_Pool_Entry_T;
   function Orc_Execution_Session_Intern_C
     (ES   : Orc_Execution_Session_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return Orc_Symbol_String_Pool_Entry_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcExecutionSessionIntern";

  --*
  -- * Reduces the ref-count for of a SymbolStringPool entry.
  --  

   procedure Orc_Release_Symbol_String_Pool_Entry (S : Orc_Symbol_String_Pool_Entry_T)  -- llvm-11.0.1.src/include/llvm-c/Orc.h:113
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcReleaseSymbolStringPoolEntry";

  --*
  -- * Dispose of a JITDylib::DefinitionGenerator. This should only be called if
  -- * ownership has not been passed to a JITDylib (e.g. because some error
  -- * prevented the client from calling LLVMOrcJITDylibAddGenerator).
  --  

   procedure Orc_Dispose_JIT_Dylib_Definition_Generator (DG : Orc_JIT_Dylib_Definition_Generator_T)  -- llvm-11.0.1.src/include/llvm-c/Orc.h:120
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeJITDylibDefinitionGenerator";

  --*
  -- * Add a JITDylib::DefinitionGenerator to the given JITDylib.
  -- *
  -- * The JITDylib will take ownership of the given generator: The client is no
  -- * longer responsible for managing its memory.
  --  

   procedure Orc_JIT_Dylib_Add_Generator (JD : Orc_JIT_Dylib_T; DG : Orc_JIT_Dylib_Definition_Generator_T)  -- llvm-11.0.1.src/include/llvm-c/Orc.h:129
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITDylibAddGenerator";

  --*
  -- * Get a DynamicLibrarySearchGenerator that will reflect process symbols into
  -- * the JITDylib. On success the resulting generator is owned by the client.
  -- * Ownership is typically transferred by adding the instance to a JITDylib
  -- * using LLVMOrcJITDylibAddGenerator,
  -- *
  -- * The GlobalPrefix argument specifies the character that appears on the front
  -- * of linker-mangled symbols for the target platform (e.g. '_' on MachO).
  -- * If non-null, this character will be stripped from the start of all symbol
  -- * strings before passing the remaining substring to dlsym.
  -- *
  -- * The optional Filter and Ctx arguments can be used to supply a symbol name
  -- * filter: Only symbols for which the filter returns true will be visible to
  -- * JIT'd code. If the Filter argument is null then all process symbols will
  -- * be visible to JIT'd code. Note that the symbol name passed to the Filter
  -- * function is the full mangled symbol: The client is responsible for stripping
  -- * the global prefix if present.
  --  

   function Orc_Create_Dynamic_Library_Search_Generator_For_Process
     (Result : System.Address;
      Global_Prefx : char;
      Filter : Orc_Symbol_Predicate_T;
      Filter_Ctx : System.Address) return LLVM.Error.Error_T  -- llvm-11.0.1.src/include/llvm-c/Orc.h:150
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess";

  --*
  -- * Create a ThreadSafeContext containing a new LLVMContext.
  -- *
  -- * Ownership of the underlying ThreadSafeContext data is shared: Clients
  -- * can and should dispose of their ThreadSafeContext as soon as they no longer
  -- * need to refer to it directly. Other references (e.g. from ThreadSafeModules
  -- * will keep the data alive as long as it is needed.
  --  

   function Orc_Create_New_Thread_Safe_Context return Orc_Thread_Safe_Context_T  -- llvm-11.0.1.src/include/llvm-c/Orc.h:162
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateNewThreadSafeContext";

  --*
  -- * Get a reference to the wrapped LLVMContext.
  --  

   function Orc_Thread_Safe_Context_Get_Context (TS_Ctx : Orc_Thread_Safe_Context_T) return LLVM.Types.Context_T  -- llvm-11.0.1.src/include/llvm-c/Orc.h:168
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcThreadSafeContextGetContext";

  --*
  -- * Dispose of a ThreadSafeContext.
  --  

   procedure Orc_Dispose_Thread_Safe_Context (TS_Ctx : Orc_Thread_Safe_Context_T)  -- llvm-11.0.1.src/include/llvm-c/Orc.h:173
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeThreadSafeContext";

  --*
  -- * Create a ThreadSafeModule wrapper around the given LLVM module. This takes
  -- * ownership of the M argument which should not be disposed of or referenced
  -- * after this function returns.
  -- *
  -- * Ownership of the ThreadSafeModule is unique: If it is transferred to the JIT
  -- * (e.g. by LLVMOrcLLJITAddLLVMIRModule), in which case the client is no longer
  -- * responsible for it. If it is not transferred to the JIT then the client
  -- * should call LLVMOrcDisposeThreadSafeModule to dispose of it.
  --  

   function Orc_Create_New_Thread_Safe_Module (M : LLVM.Types.Module_T; TS_Ctx : Orc_Thread_Safe_Context_T) return Orc_Thread_Safe_Module_T  -- llvm-11.0.1.src/include/llvm-c/Orc.h:186
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateNewThreadSafeModule";

  --*
  -- * Dispose of a ThreadSafeModule. This should only be called if ownership has
  -- * not been passed to LLJIT (e.g. because some error prevented the client from
  -- * adding this to the JIT).
  --  

   procedure Orc_Dispose_Thread_Safe_Module (TSM : Orc_Thread_Safe_Module_T)  -- llvm-11.0.1.src/include/llvm-c/Orc.h:194
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeThreadSafeModule";

  --*
  -- * Create a JITTargetMachineBuilder by detecting the host.
  -- *
  -- * On success the client owns the resulting JITTargetMachineBuilder. It must be
  -- * passed to a consuming operation (e.g. LLVMOrcCreateLLJITBuilder) or disposed
  -- * of by calling LLVMOrcDisposeJITTargetMachineBuilder.
  --  

   function Orc_JIT_Target_Machine_Builder_Detect_Host (Result : System.Address) return LLVM.Error.Error_T  -- llvm-11.0.1.src/include/llvm-c/Orc.h:203
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITTargetMachineBuilderDetectHost";

  --*
  -- * Create a JITTargetMachineBuilder from the given TargetMachine template.
  -- *
  -- * This operation takes ownership of the given TargetMachine and destroys it
  -- * before returing. The resulting JITTargetMachineBuilder is owned by the client
  -- * and must be passed to a consuming operation (e.g. LLVMOrcCreateLLJITBuilder)
  -- * or disposed of by calling LLVMOrcDisposeJITTargetMachineBuilder.
  --  

   function Orc_JIT_Target_Machine_Builder_Create_From_Target_Machine (TM : LLVM.Target_Machine.Target_Machine_T) return Orc_JIT_Target_Machine_Builder_T  -- llvm-11.0.1.src/include/llvm-c/Orc.h:215
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITTargetMachineBuilderCreateFromTargetMachine";

  --*
  -- * Dispose of a JITTargetMachineBuilder.
  --  

   procedure Orc_Dispose_JIT_Target_Machine_Builder (JTMB : Orc_JIT_Target_Machine_Builder_T)  -- llvm-11.0.1.src/include/llvm-c/Orc.h:220
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeJITTargetMachineBuilder";

  --*
  -- * Create an LLJITTargetMachineBuilder.
  -- *
  -- * The client owns the resulting LLJITBuilder and should dispose of it using
  -- * LLVMOrcDisposeLLJITBuilder once they are done with it.
  --  

   function Orc_Create_LLJIT_Builder return Orc_LLJIT_Builder_T  -- llvm-11.0.1.src/include/llvm-c/Orc.h:229
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateLLJITBuilder";

  --*
  -- * Dispose of an LLVMOrcLLJITBuilderRef. This should only be called if ownership
  -- * has not been passed to LLVMOrcCreateLLJIT (e.g. because some error prevented
  -- * that function from being called).
  --  

   procedure Orc_Dispose_LLJIT_Builder (Builder : Orc_LLJIT_Builder_T)  -- llvm-11.0.1.src/include/llvm-c/Orc.h:236
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeLLJITBuilder";

  --*
  -- * Set the JITTargetMachineBuilder to be used when constructing the LLJIT
  -- * instance. Calling this function is optional: if it is not called then the
  -- * LLJITBuilder will use JITTargeTMachineBuilder::detectHost to construct a
  -- * JITTargetMachineBuilder.
  --  

   procedure Orc_LLJIT_Builder_Set_JIT_Target_Machine_Builder (Builder : Orc_LLJIT_Builder_T; JTMB : Orc_JIT_Target_Machine_Builder_T)  -- llvm-11.0.1.src/include/llvm-c/Orc.h:244
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITBuilderSetJITTargetMachineBuilder";

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

   function Orc_Create_LLJIT (Result : System.Address; Builder : Orc_LLJIT_Builder_T) return LLVM.Error.Error_T  -- llvm-11.0.1.src/include/llvm-c/Orc.h:261
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateLLJIT";

  --*
  -- * Dispose of an LLJIT instance.
  --  

   function Orc_Dispose_LLJIT (J : Orc_LLJIT_T) return LLVM.Error.Error_T  -- llvm-11.0.1.src/include/llvm-c/Orc.h:267
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeLLJIT";

  --*
  -- * Get a reference to the ExecutionSession for this LLJIT instance.
  -- *
  -- * The ExecutionSession is owned by the LLJIT instance. The client is not
  -- * responsible for managing its memory.
  --  

   function Orc_LLJIT_Get_Execution_Session (J : Orc_LLJIT_T) return Orc_Execution_Session_T  -- llvm-11.0.1.src/include/llvm-c/Orc.h:275
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITGetExecutionSession";

  --*
  -- * Return a reference to the Main JITDylib.
  -- *
  -- * The JITDylib is owned by the LLJIT instance. The client is not responsible
  -- * for managing its memory.
  --  

   function Orc_LLJIT_Get_Main_JIT_Dylib (J : Orc_LLJIT_T) return Orc_JIT_Dylib_T  -- llvm-11.0.1.src/include/llvm-c/Orc.h:283
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
   function Orc_LLJIT_Get_Triple_String_C
     (J : Orc_LLJIT_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcLLJITGetTripleString";

  --*
  -- * Returns the global prefix character according to the LLJIT's DataLayout.
  --  

   function Orc_LLJIT_Get_Global_Prefix (J : Orc_LLJIT_T) return char  -- llvm-11.0.1.src/include/llvm-c/Orc.h:294
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
      return Orc_Symbol_String_Pool_Entry_T;
   function Orc_LLJIT_Mangle_And_Intern_C
     (J              : Orc_LLJIT_T;
      Unmangled_Name : Interfaces.C.Strings.chars_ptr)
      return Orc_Symbol_String_Pool_Entry_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcLLJITMangleAndIntern";

  --*
  -- * Add a buffer representing an object file to the given JITDylib in the given
  -- * LLJIT instance. This operation transfers ownership of the buffer to the
  -- * LLJIT instance. The buffer should not be disposed of or referenced once this
  -- * function returns.
  --  

   function Orc_LLJIT_Add_Object_File
     (J : Orc_LLJIT_T;
      JD : Orc_JIT_Dylib_T;
      Obj_Buffer : LLVM.Types.Memory_Buffer_T) return LLVM.Error.Error_T  -- llvm-11.0.1.src/include/llvm-c/Orc.h:312
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITAddObjectFile";

  --*
  -- * Add an IR module to the given JITDylib of the given LLJIT instance. This
  -- * operation transfers ownership of the TSM argument to the LLJIT instance.
  -- * The TSM argument should not be 3disposed of or referenced once this
  -- * function returns.
  --  

   function Orc_LLJIT_Add_LLVMIR_Module
     (J : Orc_LLJIT_T;
      JD : Orc_JIT_Dylib_T;
      TSM : Orc_Thread_Safe_Module_T) return LLVM.Error.Error_T  -- llvm-11.0.1.src/include/llvm-c/Orc.h:321
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLLJITAddLLVMIRModule";

  --*
  -- * Look up the given symbol in the main JITDylib of the given LLJIT instance.
  -- *
  -- * This operation does not take ownership of the Name argument.
  --  

function Orc_LLJIT_Lookup
     (J      : Orc_LLJIT_T;
      Result : access Orc_JIT_Target_Address_T;
      Name   : String)
      return LLVM.Error.Error_T;
   function Orc_LLJIT_Lookup_C
     (J      : Orc_LLJIT_T;
      Result : access Orc_JIT_Target_Address_T;
      Name   : Interfaces.C.Strings.chars_ptr)
      return LLVM.Error.Error_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcLLJITLookup";

end LLVM.Orc;

