pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with stdint_h;
with System;
with LLVM.Error;
with stddef_h;
with LLVM.Types;
with Interfaces.C.Strings;
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
  -- * @defgroup LLVMCExecutionEngineORC On-Request-Compilation
  -- * @ingroup LLVMCExecutionEngine
  -- *
  -- * @{
  --  

  --*
  -- * Represents an address in the executor process.
  --  

   subtype Orc_JIT_Target_Address_T is stdint_h.uint64_t;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:46

  --*
  -- * Represents an address in the executor process.
  --  

   subtype Orc_Executor_Address_T is stdint_h.uint64_t;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:51

  --*
  -- * Represents generic linkage flags for a symbol definition.
  --  

   subtype JIT_Symbol_Generic_Flags_T is unsigned;
   JIT_Symbol_Generic_Flags_Exported : constant JIT_Symbol_Generic_Flags_T := 1;
   JIT_Symbol_Generic_Flags_Weak : constant JIT_Symbol_Generic_Flags_T := 2;
   JIT_Symbol_Generic_Flags_Callable : constant JIT_Symbol_Generic_Flags_T := 4;
   JIT_Symbol_Generic_Flags_Materialization_Side_Effects_Only : constant JIT_Symbol_Generic_Flags_T := 8;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:61

  --*
  -- * Represents target specific flags for a symbol definition.
  --  

   subtype JIT_Symbol_Target_Flags_T is stdint_h.uint8_t;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:66

  --*
  -- * Represents the linkage flags for a symbol definition.
  --  

   type JIT_Symbol_Flags_T is record
      GenericFlags : aliased stdint_h.uint8_t;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:72
      TargetFlags : aliased stdint_h.uint8_t;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:73
   end record
   with Convention => C_Pass_By_Copy;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:74

  --*
  -- * Represents an evaluated symbol address and flags.
  --  

   type JIT_Evaluated_Symbol_T is record
      Address : aliased Orc_Executor_Address_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:80
      Flags : aliased JIT_Symbol_Flags_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:81
   end record
   with Convention => C_Pass_By_Copy;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:82

  --*
  -- * A reference to an orc::ExecutionSession instance.
  --  

   type Orc_Opaque_Execution_Session_Impl_T is null record;   -- incomplete struct

   type Orc_Execution_Session_T is access all Orc_Opaque_Execution_Session_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:87

  --*
  -- * Error reporter function.
  --  

   type Orc_Error_Reporter_Function_T is access procedure (Arg_1 : System.Address; Arg_2 : LLVM.Error.Error_T)
   with Convention => C;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:92

  --*
  -- * A reference to an orc::SymbolStringPool.
  --  

   type Orc_Opaque_Symbol_String_Pool_Impl_T is null record;   -- incomplete struct

   type Orc_Symbol_String_Pool_T is access all Orc_Opaque_Symbol_String_Pool_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:97

  --*
  -- * A reference to an orc::SymbolStringPool table entry.
  --  

   type Orc_Opaque_Symbol_String_Pool_Entry_Impl_T is null record;   -- incomplete struct

   type Orc_Symbol_String_Pool_Entry_T is access all Orc_Opaque_Symbol_String_Pool_Entry_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:103

  --*
  -- * Represents a pair of a symbol name and LLVMJITSymbolFlags.
  --  

   type Orc_C_Symbol_Flags_Map_Pair_T is record
      Name : Orc_Symbol_String_Pool_Entry_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:109
      Flags : aliased JIT_Symbol_Flags_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:110
   end record
   with Convention => C_Pass_By_Copy;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:111

  --*
  -- * Represents a list of (SymbolStringPtr, JITSymbolFlags) pairs that can be used
  -- * to construct a SymbolFlagsMap.
  --  

   type Orc_C_Symbol_Flags_Map_Pairs_T is access all Orc_C_Symbol_Flags_Map_Pair_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:117

  --*
  -- * Represents a pair of a symbol name and an evaluated symbol.
  --  

   type JITC_Symbol_Map_Pair_T is record
      Name : Orc_Symbol_String_Pool_Entry_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:123
      Sym : aliased JIT_Evaluated_Symbol_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:124
   end record
   with Convention => C_Pass_By_Copy;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:125

  --*
  -- * Represents a list of (SymbolStringPtr, JITEvaluatedSymbol) pairs that can be
  -- * used to construct a SymbolMap.
  --  

   type Orc_C_Symbol_Map_Pairs_T is access all JITC_Symbol_Map_Pair_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:131

  --*
  -- * Represents a SymbolAliasMapEntry
  --  

   type Orc_C_Symbol_Alias_Map_Entry_T is record
      Name : Orc_Symbol_String_Pool_Entry_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:137
      Flags : aliased JIT_Symbol_Flags_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:138
   end record
   with Convention => C_Pass_By_Copy;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:139

  --*
  -- * Represents a pair of a symbol name and SymbolAliasMapEntry.
  --  

   type Orc_C_Symbol_Alias_Map_Pair_T is record
      Name : Orc_Symbol_String_Pool_Entry_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:145
      c_Entry : aliased Orc_C_Symbol_Alias_Map_Entry_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:146
   end record
   with Convention => C_Pass_By_Copy;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:147

  --*
  -- * Represents a list of (SymbolStringPtr, (SymbolStringPtr, JITSymbolFlags))
  -- * pairs that can be used to construct a SymbolFlagsMap.
  --  

   type Orc_C_Symbol_Alias_Map_Pairs_T is access all Orc_C_Symbol_Alias_Map_Pair_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:153

  --*
  -- * A reference to an orc::JITDylib instance.
  --  

   type Orc_Opaque_JIT_Dylib_Impl_T is null record;   -- incomplete struct

   type Orc_JIT_Dylib_T is access all Orc_Opaque_JIT_Dylib_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:158

  --*
  -- * Represents a list of LLVMOrcSymbolStringPoolEntryRef and the associated
  -- * length.
  --  

   type Orc_C_Symbols_List_T is record
      Symbols : System.Address;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:165
      Length : aliased stddef_h.size_t;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:166
   end record
   with Convention => C_Pass_By_Copy;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:167

  --*
  -- * Represents a pair of a JITDylib and LLVMOrcCSymbolsList.
  --  

   type Orc_C_Dependence_Map_Pair_T is record
      JD : Orc_JIT_Dylib_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:173
      Names : aliased Orc_C_Symbols_List_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:174
   end record
   with Convention => C_Pass_By_Copy;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:175

  --*
  -- * Represents a list of (JITDylibRef, (LLVMOrcSymbolStringPoolEntryRef*,
  -- * size_t)) pairs that can be used to construct a SymbolDependenceMap.
  --  

   type Orc_C_Dependence_Map_Pairs_T is access all Orc_C_Dependence_Map_Pair_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:181

  --*
  -- * Lookup kind. This can be used by definition generators when deciding whether
  -- * to produce a definition for a requested symbol.
  -- *
  -- * This enum should be kept in sync with llvm::orc::LookupKind.
  --  

   type Orc_Lookup_Kind_T is 
     (Orc_Lookup_Kind_Static,
      Orc_Lookup_Kind_DL_Sym)
   with Convention => C;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:192

  --*
  -- * JITDylib lookup flags. This can be used by definition generators when
  -- * deciding whether to produce a definition for a requested symbol.
  -- *
  -- * This enum should be kept in sync with llvm::orc::JITDylibLookupFlags.
  --  

   type Orc_JIT_Dylib_Lookup_Flags_T is 
     (Orc_JIT_Dylib_Lookup_Flags_Match_Exported_Symbols_Only,
      Orc_JIT_Dylib_Lookup_Flags_Match_All_Symbols)
   with Convention => C;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:203

  --*
  -- * Symbol lookup flags for lookup sets. This should be kept in sync with
  -- * llvm::orc::SymbolLookupFlags.
  --  

   type Orc_Symbol_Lookup_Flags_T is 
     (Orc_Symbol_Lookup_Flags_Required_Symbol,
      Orc_Symbol_Lookup_Flags_Weakly_Referenced_Symbol)
   with Convention => C;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:212

  --*
  -- * An element type for a symbol lookup set.
  --  

   type Orc_C_Lookup_Set_Element_T is record
      Name : Orc_Symbol_String_Pool_Entry_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:218
      LookupFlags : aliased Orc_Symbol_Lookup_Flags_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:219
   end record
   with Convention => C_Pass_By_Copy;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:220

  --*
  -- * A set of symbols to look up / generate.
  -- *
  -- * The list is terminated with an element containing a null pointer for the
  -- * Name field.
  -- *
  -- * If a client creates an instance of this type then they are responsible for
  -- * freeing it, and for ensuring that all strings have been retained over the
  -- * course of its life. Clients receiving a copy from a callback are not
  -- * responsible for managing lifetime or retain counts.
  --  

   type Orc_C_Lookup_Set_T is access all Orc_C_Lookup_Set_Element_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:233

  --*
  -- * A reference to a uniquely owned orc::MaterializationUnit instance.
  --  

   type Orc_Opaque_Materialization_Unit_Impl_T is null record;   -- incomplete struct

   type Orc_Materialization_Unit_T is access all Orc_Opaque_Materialization_Unit_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:238

  --*
  -- * A reference to a uniquely owned orc::MaterializationResponsibility instance.
  -- *
  -- * Ownership must be passed to a lower-level layer in a JIT stack.
  --  

   type Orc_Opaque_Materialization_Responsibility_Impl_T is null record;   -- incomplete struct

   type Orc_Materialization_Responsibility_T is access all Orc_Opaque_Materialization_Responsibility_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:246

  --*
  -- * A MaterializationUnit materialize callback.
  -- *
  -- * Ownership of the Ctx and MR arguments passes to the callback which must
  -- * adhere to the LLVMOrcMaterializationResponsibilityRef contract (see comment
  -- * for that type).
  -- *
  -- * If this callback is called then the LLVMOrcMaterializationUnitDestroy
  -- * callback will NOT be called.
  --  

   type Orc_Materialization_Unit_Materialize_Function_T is access procedure (Arg_1 : System.Address; Arg_2 : Orc_Materialization_Responsibility_T)
   with Convention => C;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:258

  --*
  -- * A MaterializationUnit discard callback.
  -- *
  -- * Ownership of JD and Symbol remain with the caller: These arguments should
  -- * not be disposed of or released.
  --  

   type Orc_Materialization_Unit_Discard_Function_T is access procedure
        (Arg_1 : System.Address;
         Arg_2 : Orc_JIT_Dylib_T;
         Arg_3 : Orc_Symbol_String_Pool_Entry_T)
   with Convention => C;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:267

  --*
  -- * A MaterializationUnit destruction callback.
  -- *
  -- * If a custom MaterializationUnit is destroyed before its Materialize
  -- * function is called then this function will be called to provide an
  -- * opportunity for the underlying program representation to be destroyed.
  --  

   type Orc_Materialization_Unit_Destroy_Function_T is access procedure (Arg_1 : System.Address)
   with Convention => C;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:277

  --*
  -- * A reference to an orc::ResourceTracker instance.
  --  

   type Orc_Opaque_Resource_Tracker_Impl_T is null record;   -- incomplete struct

   type Orc_Resource_Tracker_T is access all Orc_Opaque_Resource_Tracker_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:282

  --*
  -- * A reference to an orc::DefinitionGenerator.
  --  

   type Orc_Opaque_Definition_Generator_Impl_T is null record;   -- incomplete struct

   type Orc_Definition_Generator_T is access all Orc_Opaque_Definition_Generator_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:288

  --*
  -- * An opaque lookup state object. Instances of this type can be captured to
  -- * suspend a lookup while a custom generator function attempts to produce a
  -- * definition.
  -- *
  -- * If a client captures a lookup state object then they must eventually call
  -- * LLVMOrcLookupStateContinueLookup to restart the lookup. This is required
  -- * in order to release memory allocated for the lookup state, even if errors
  -- * have occurred while the lookup was suspended (if these errors have made the
  -- * lookup impossible to complete then it will issue its own error before
  -- * destruction).
  --  

   type Orc_Opaque_Lookup_State_Impl_T is null record;   -- incomplete struct

   type Orc_Lookup_State_T is access all Orc_Opaque_Lookup_State_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:302

  --*
  -- * A custom generator function. This can be used to create a custom generator
  -- * object using LLVMOrcCreateCustomCAPIDefinitionGenerator. The resulting
  -- * object can be attached to a JITDylib, via LLVMOrcJITDylibAddGenerator, to
  -- * receive callbacks when lookups fail to match existing definitions.
  -- *
  -- * GeneratorObj will contain the address of the custom generator object.
  -- *
  -- * Ctx will contain the context object passed to
  -- * LLVMOrcCreateCustomCAPIDefinitionGenerator.
  -- *
  -- * LookupState will contain a pointer to an LLVMOrcLookupStateRef object. This
  -- * can optionally be modified to make the definition generation process
  -- * asynchronous: If the LookupStateRef value is copied, and the original
  -- * LLVMOrcLookupStateRef set to null, the lookup will be suspended. Once the
  -- * asynchronous definition process has been completed clients must call
  -- * LLVMOrcLookupStateContinueLookup to continue the lookup (this should be
  -- * done unconditionally, even if errors have occurred in the mean time, to
  -- * free the lookup state memory and notify the query object of the failures).
  -- * If LookupState is captured this function must return LLVMErrorSuccess.
  -- *
  -- * The Kind argument can be inspected to determine the lookup kind (e.g.
  -- * as-if-during-static-link, or as-if-during-dlsym).
  -- *
  -- * The JD argument specifies which JITDylib the definitions should be generated
  -- * into.
  -- *
  -- * The JDLookupFlags argument can be inspected to determine whether the original
  -- * lookup included non-exported symobls.
  -- *
  -- * Finally, the LookupSet argument contains the set of symbols that could not
  -- * be found in JD already (the set of generation candidates).
  --  

   type Orc_CAPI_Definition_Generator_Try_To_Generate_Function_T is access function
        (Arg_1 : Orc_Definition_Generator_T;
         Arg_2 : System.Address;
         Arg_3 : System.Address;
         Arg_4 : Orc_Lookup_Kind_T;
         Arg_5 : Orc_JIT_Dylib_T;
         Arg_6 : Orc_JIT_Dylib_Lookup_Flags_T;
         Arg_7 : Orc_C_Lookup_Set_T;
         Arg_8 : stddef_h.size_t) return LLVM.Error.Error_T
   with Convention => C;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:337

  --*
  -- * Predicate function for SymbolStringPoolEntries.
  --  

   type Orc_Symbol_Predicate_T is access function (Arg_1 : System.Address; Arg_2 : Orc_Symbol_String_Pool_Entry_T) return int
   with Convention => C;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:346

  --*
  -- * A reference to an orc::ThreadSafeContext instance.
  --  

   type Orc_Opaque_Thread_Safe_Context_Impl_T is null record;   -- incomplete struct

   type Orc_Thread_Safe_Context_T is access all Orc_Opaque_Thread_Safe_Context_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:352

  --*
  -- * A reference to an orc::ThreadSafeModule instance.
  --  

   type Orc_Opaque_Thread_Safe_Module_Impl_T is null record;   -- incomplete struct

   type Orc_Thread_Safe_Module_T is access all Orc_Opaque_Thread_Safe_Module_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:357

  --*
  -- * A function for inspecting/mutating IR modules, suitable for use with
  -- * LLVMOrcThreadSafeModuleWithModuleDo.
  --  

   type Orc_Generic_IR_Module_Operation_Function_T is access function (Arg_1 : System.Address; Arg_2 : LLVM.Types.Module_T) return LLVM.Error.Error_T
   with Convention => C;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:363

  --*
  -- * A reference to an orc::JITTargetMachineBuilder instance.
  --  

   type Orc_Opaque_JIT_Target_Machine_Builder_Impl_T is null record;   -- incomplete struct

   type Orc_JIT_Target_Machine_Builder_T is access all Orc_Opaque_JIT_Target_Machine_Builder_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:370

  --*
  -- * A reference to an orc::ObjectLayer instance.
  --  

   type Orc_Opaque_Object_Layer_Impl_T is null record;   -- incomplete struct

   type Orc_Object_Layer_T is access all Orc_Opaque_Object_Layer_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:375

  --*
  -- * A reference to an orc::ObjectLinkingLayer instance.
  --  

   type Orc_Opaque_Object_Linking_Layer_Impl_T is null record;   -- incomplete struct

   type Orc_Object_Linking_Layer_T is access all Orc_Opaque_Object_Linking_Layer_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:380

  --*
  -- * A reference to an orc::IRTransformLayer instance.
  --  

   type Orc_Opaque_IR_Transform_Layer_Impl_T is null record;   -- incomplete struct

   type Orc_IR_Transform_Layer_T is access all Orc_Opaque_IR_Transform_Layer_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:385

  --*
  -- * A function for applying transformations as part of an transform layer.
  -- *
  -- * Implementations of this type are responsible for managing the lifetime
  -- * of the Module pointed to by ModInOut: If the LLVMModuleRef value is
  -- * overwritten then the function is responsible for disposing of the incoming
  -- * module. If the module is simply accessed/mutated in-place then ownership
  -- * returns to the caller and the function does not need to do any lifetime
  -- * management.
  -- *
  -- * Clients can call LLVMOrcLLJITGetIRTransformLayer to obtain the transform
  -- * layer of a LLJIT instance, and use LLVMOrcIRTransformLayerSetTransform
  -- * to set the function. This can be used to override the default transform
  -- * layer.
  --  

   type Orc_IR_Transform_Layer_Transform_Function_T is access function
        (Arg_1 : System.Address;
         Arg_2 : System.Address;
         Arg_3 : Orc_Materialization_Responsibility_T) return LLVM.Error.Error_T
   with Convention => C;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:402

  --*
  -- * A reference to an orc::ObjectTransformLayer instance.
  --  

   type Orc_Opaque_Object_Transform_Layer_Impl_T is null record;   -- incomplete struct

   type Orc_Object_Transform_Layer_T is access all Orc_Opaque_Object_Transform_Layer_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:410

  --*
  -- * A function for applying transformations to an object file buffer.
  -- *
  -- * Implementations of this type are responsible for managing the lifetime
  -- * of the memory buffer pointed to by ObjInOut: If the LLVMMemoryBufferRef
  -- * value is overwritten then the function is responsible for disposing of the
  -- * incoming buffer. If the buffer is simply accessed/mutated in-place then
  -- * ownership returns to the caller and the function does not need to do any
  -- * lifetime management.
  -- *
  -- * The transform is allowed to return an error, in which case the ObjInOut
  -- * buffer should be disposed of and set to null.
  --  

   type Orc_Object_Transform_Layer_Transform_Function_T is access function (Arg_1 : System.Address; Arg_2 : System.Address) return LLVM.Error.Error_T
   with Convention => C;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:425

  --*
  -- * A reference to an orc::IndirectStubsManager instance.
  --  

   type Orc_Opaque_Indirect_Stubs_Manager_Impl_T is null record;   -- incomplete struct

   type Orc_Indirect_Stubs_Manager_T is access all Orc_Opaque_Indirect_Stubs_Manager_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:432

  --*
  -- * A reference to an orc::LazyCallThroughManager instance.
  --  

   type Orc_Opaque_Lazy_Call_Through_Manager_Impl_T is null record;   -- incomplete struct

   type Orc_Lazy_Call_Through_Manager_T is access all Orc_Opaque_Lazy_Call_Through_Manager_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:438

  --*
  -- * A reference to an orc::DumpObjects object.
  -- *
  -- * Can be used to dump object files to disk with unique names. Useful as an
  -- * ObjectTransformLayer transform.
  --  

   type Orc_Opaque_Dump_Objects_Impl_T is null record;   -- incomplete struct

   type Orc_Dump_Objects_T is access all Orc_Opaque_Dump_Objects_Impl_T;  -- llvm-14.0.1.install/include/llvm-c/Orc.h:446

  --*
  -- * Attach a custom error reporter function to the ExecutionSession.
  -- *
  -- * The error reporter will be called to deliver failure notices that can not be
  -- * directly reported to a caller. For example, failure to resolve symbols in
  -- * the JIT linker is typically reported via the error reporter (callers
  -- * requesting definitions from the JIT will typically be delivered a
  -- * FailureToMaterialize error instead).
  --  

   procedure Orc_Execution_Session_Set_Error_Reporter
     (ES : Orc_Execution_Session_T;
      Report_Error : Orc_Error_Reporter_Function_T;
      Ctx : System.Address)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:457
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcExecutionSessionSetErrorReporter";

  --*
  -- * Return a reference to the SymbolStringPool for an ExecutionSession.
  -- *
  -- * Ownership of the pool remains with the ExecutionSession: The caller is
  -- * not required to free the pool.
  --  

   function Orc_Execution_Session_Get_Symbol_String_Pool (ES : Orc_Execution_Session_T) return Orc_Symbol_String_Pool_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:468
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcExecutionSessionGetSymbolStringPool";

  --*
  -- * Clear all unreferenced symbol string pool entries.
  -- *
  -- * This can be called at any time to release unused entries in the
  -- * ExecutionSession's string pool. Since it locks the pool (preventing
  -- * interning of any new strings) it is recommended that it only be called
  -- * infrequently, ideally when the caller has reason to believe that some
  -- * entries will have become unreferenced, e.g. after removing a module or
  -- * closing a JITDylib.
  --  

   procedure Orc_Symbol_String_Pool_Clear_Dead_Entries (SSP : Orc_Symbol_String_Pool_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:480
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcSymbolStringPoolClearDeadEntries";

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

  --*
  -- * Increments the ref-count for a SymbolStringPool entry.
  --  

   procedure Orc_Retain_Symbol_String_Pool_Entry (S : Orc_Symbol_String_Pool_Entry_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:500
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcRetainSymbolStringPoolEntry";

  --*
  -- * Reduces the ref-count for of a SymbolStringPool entry.
  --  

   procedure Orc_Release_Symbol_String_Pool_Entry (S : Orc_Symbol_String_Pool_Entry_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:505
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcReleaseSymbolStringPoolEntry";

function Orc_Symbol_String_Pool_Entry_Str
     (S : Orc_Symbol_String_Pool_Entry_T)
      return String;

  --*
  -- * Reduces the ref-count of a ResourceTracker.
  --  

   procedure Orc_Release_Resource_Tracker (RT : Orc_Resource_Tracker_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:512
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcReleaseResourceTracker";

  --*
  -- * Transfers tracking of all resources associated with resource tracker SrcRT
  -- * to resource tracker DstRT.
  --  

   procedure Orc_Resource_Tracker_Transfer_To (Src_RT : Orc_Resource_Tracker_T; Dst_RT : Orc_Resource_Tracker_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:518
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcResourceTrackerTransferTo";

  --*
  -- * Remove all resources associated with the given tracker. See
  -- * ResourceTracker::remove().
  --  

   function Orc_Resource_Tracker_Remove (RT : Orc_Resource_Tracker_T) return LLVM.Error.Error_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:525
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcResourceTrackerRemove";

  --*
  -- * Dispose of a JITDylib::DefinitionGenerator. This should only be called if
  -- * ownership has not been passed to a JITDylib (e.g. because some error
  -- * prevented the client from calling LLVMOrcJITDylibAddGenerator).
  --  

   procedure Orc_Dispose_Definition_Generator (DG : Orc_Definition_Generator_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:532
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeDefinitionGenerator";

  --*
  -- * Dispose of a MaterializationUnit.
  --  

   procedure Orc_Dispose_Materialization_Unit (MU : Orc_Materialization_Unit_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:537
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeMaterializationUnit";

  --*
  -- * Create a custom MaterializationUnit.
  -- *
  -- * Name is a name for this MaterializationUnit to be used for identification
  -- * and logging purposes (e.g. if this MaterializationUnit produces an
  -- * object buffer then the name of that buffer will be derived from this name).
  -- *
  -- * The Syms list contains the names and linkages of the symbols provided by this
  -- * unit. This function takes ownership of the elements of the Syms array. The
  -- * Name fields of the array elements are taken to have been retained for this
  -- * function. The client should *not* release the elements of the array, but is
  -- * still responsible for destroyingthe array itself.
  -- *
  -- * The InitSym argument indicates whether or not this MaterializationUnit
  -- * contains static initializers. If three are no static initializers (the common
  -- * case) then this argument should be null. If there are static initializers
  -- * then InitSym should be set to a unique name that also appears in the Syms
  -- * list with the LLVMJITSymbolGenericFlagsMaterializationSideEffectsOnly flag
  -- * set. This function takes ownership of the InitSym, which should have been
  -- * retained twice on behalf of this function: once for the Syms entry and once
  -- * for InitSym. If clients wish to use the InitSym value after this function
  -- * returns they must retain it once more for themselves.
  -- *
  -- * If any of the symbols in the Syms list is looked up then the Materialize
  -- * function will be called.
  -- *
  -- * If any of the symbols in the Syms list is overridden then the Discard
  -- * function will be called.
  -- *
  -- * The caller owns the underling MaterializationUnit and is responsible for
  -- * either passing it to a JITDylib (via LLVMOrcJITDylibDefine) or disposing
  -- * of it by calling LLVMOrcDisposeMaterializationUnit.
  --  

function Orc_Create_Custom_Materialization_Unit
     (Name        : String;
      Ctx         : System.Address;
      Syms        : Orc_C_Symbol_Flags_Map_Pairs_T;
      Num_Syms    : stddef_h.size_t;
      Init_Sym    : Orc_Symbol_String_Pool_Entry_T;
      Materialize : Orc_Materialization_Unit_Materialize_Function_T;
      Discard     : Orc_Materialization_Unit_Discard_Function_T;
      Destroy     : Orc_Materialization_Unit_Destroy_Function_T)
      return Orc_Materialization_Unit_T;

  --*
  -- * Create a MaterializationUnit to define the given symbols as pointing to
  -- * the corresponding raw addresses.
  -- *
  -- * This function takes ownership of the elements of the Syms array. The Name
  -- * fields of the array elements are taken to have been retained for this
  -- * function. This allows the following pattern...
  -- *
  -- *   size_t NumPairs;
  -- *   LLVMOrcCSymbolMapPairs Sym;
  -- *   -- Build Syms array --
  -- *   LLVMOrcMaterializationUnitRef MU =
  -- *       LLVMOrcAbsoluteSymbols(Syms, NumPairs);
  -- *
  -- * ... without requiring cleanup of the elements of the Sym array afterwards.
  -- *
  -- * The client is still responsible for deleting the Sym array itself.
  -- *
  -- * If a client wishes to reuse elements of the Sym array after this call they
  -- * must explicitly retain each of the elements for themselves.
  --  

   function Orc_Absolute_Symbols (Syms : Orc_C_Symbol_Map_Pairs_T; Num_Pairs : stddef_h.size_t) return Orc_Materialization_Unit_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:601
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcAbsoluteSymbols";

  --*
  -- * Create a MaterializationUnit to define lazy re-expots. These are callable
  -- * entry points that call through to the given symbols.
  -- *
  -- * This function takes ownership of the CallableAliases array. The Name
  -- * fields of the array elements are taken to have been retained for this
  -- * function. This allows the following pattern...
  -- *
  -- *   size_t NumPairs;
  -- *   LLVMOrcCSymbolAliasMapPairs CallableAliases;
  -- *   -- Build CallableAliases array --
  -- *   LLVMOrcMaterializationUnitRef MU =
  -- *      LLVMOrcLazyReexports(LCTM, ISM, JD, CallableAliases, NumPairs);
  -- *
  -- * ... without requiring cleanup of the elements of the CallableAliases array afterwards.
  -- *
  -- * The client is still responsible for deleting the CallableAliases array itself.
  -- *
  -- * If a client wishes to reuse elements of the CallableAliases array after this call they
  -- * must explicitly retain each of the elements for themselves.
  --  

   function Orc_Lazy_Reexports
     (LCTM : Orc_Lazy_Call_Through_Manager_T;
      ISM : Orc_Indirect_Stubs_Manager_T;
      Source_Ref : Orc_JIT_Dylib_T;
      Callable_Aliases : Orc_C_Symbol_Alias_Map_Pairs_T;
      Num_Pairs : stddef_h.size_t) return Orc_Materialization_Unit_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:624
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcLazyReexports";

  -- TODO: ImplSymbolMad SrcJDLoc
  --*
  -- * Disposes of the passed MaterializationResponsibility object.
  -- *
  -- * This should only be done after the symbols covered by the object have either
  -- * been resolved and emitted (via
  -- * LLVMOrcMaterializationResponsibilityNotifyResolved and
  -- * LLVMOrcMaterializationResponsibilityNotifyEmitted) or failed (via
  -- * LLVMOrcMaterializationResponsibilityFailMaterialization).
  --  

   procedure Orc_Dispose_Materialization_Responsibility (MR : Orc_Materialization_Responsibility_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:639
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeMaterializationResponsibility";

  --*
  -- * Returns the target JITDylib that these symbols are being materialized into.
  --  

   function Orc_Materialization_Responsibility_Get_Target_Dylib (MR : Orc_Materialization_Responsibility_T) return Orc_JIT_Dylib_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:645
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcMaterializationResponsibilityGetTargetDylib";

  --*
  -- * Returns the ExecutionSession for this MaterializationResponsibility.
  --  

   function Orc_Materialization_Responsibility_Get_Execution_Session (MR : Orc_Materialization_Responsibility_T) return Orc_Execution_Session_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:652
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcMaterializationResponsibilityGetExecutionSession";

  --*
  -- * Returns the symbol flags map for this responsibility instance.
  -- *
  -- * The length of the array is returned in NumPairs and the caller is responsible
  -- * for the returned memory and needs to call LLVMOrcDisposeCSymbolFlagsMap.
  -- *
  -- * To use the returned symbols beyond the livetime of the
  -- * MaterializationResponsibility requires the caller to retain the symbols
  -- * explicitly.
  --  

   function Orc_Materialization_Responsibility_Get_Symbols (MR : Orc_Materialization_Responsibility_T; Num_Pairs : access unsigned_long) return Orc_C_Symbol_Flags_Map_Pairs_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:665
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcMaterializationResponsibilityGetSymbols";

  --*
  -- * Disposes of the passed LLVMOrcCSymbolFlagsMap.
  -- *
  -- * Does not release the entries themselves.
  --  

   procedure Orc_Dispose_C_Symbol_Flags_Map (Pairs : Orc_C_Symbol_Flags_Map_Pairs_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:673
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeCSymbolFlagsMap";

  --*
  -- * Returns the initialization pseudo-symbol, if any. This symbol will also
  -- * be present in the SymbolFlagsMap for this MaterializationResponsibility
  -- * object.
  -- *
  -- * The returned symbol is not retained over any mutating operation of the
  -- * MaterializationResponsbility or beyond the lifetime thereof.
  --  

   function Orc_Materialization_Responsibility_Get_Initializer_Symbol (MR : Orc_Materialization_Responsibility_T) return Orc_Symbol_String_Pool_Entry_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:684
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcMaterializationResponsibilityGetInitializerSymbol";

  --*
  -- * Returns the names of any symbols covered by this
  -- * MaterializationResponsibility object that have queries pending. This
  -- * information can be used to return responsibility for unrequested symbols
  -- * back to the JITDylib via the delegate method.
  --  

   function Orc_Materialization_Responsibility_Get_Requested_Symbols (MR : Orc_Materialization_Responsibility_T; Num_Symbols : access unsigned_long) return System.Address  -- llvm-14.0.1.install/include/llvm-c/Orc.h:694
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcMaterializationResponsibilityGetRequestedSymbols";

  --*
  -- * Disposes of the passed LLVMOrcSymbolStringPoolEntryRef* .
  -- *
  -- * Does not release the symbols themselves.
  --  

   procedure Orc_Dispose_Symbols (Symbols : System.Address)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:702
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeSymbols";

  -- * Notifies the target JITDylib that the given symbols have been resolved.
  -- * This will update the given symbols' addresses in the JITDylib, and notify
  -- * any pending queries on the given symbols of their resolution. The given
  -- * symbols must be ones covered by this MaterializationResponsibility
  -- * instance. Individual calls to this method may resolve a subset of the
  -- * symbols, but all symbols must have been resolved prior to calling emit.
  -- *
  -- * This method will return an error if any symbols being resolved have been
  -- * moved to the error state due to the failure of a dependency. If this
  -- * method returns an error then clients should log it and call
  -- * LLVMOrcMaterializationResponsibilityFailMaterialization. If no dependencies
  -- * have been registered for the symbols covered by this
  -- * MaterializationResponsibiility then this method is guaranteed to return
  -- * LLVMErrorSuccess.
  --  

   function Orc_Materialization_Responsibility_Notify_Resolved
     (MR : Orc_Materialization_Responsibility_T;
      Symbols : Orc_C_Symbol_Map_Pairs_T;
      Num_Pairs : stddef_h.size_t) return LLVM.Error.Error_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:720
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcMaterializationResponsibilityNotifyResolved";

  --*
  -- * Notifies the target JITDylib (and any pending queries on that JITDylib)
  -- * that all symbols covered by this MaterializationResponsibility instance
  -- * have been emitted.
  -- *
  -- * This method will return an error if any symbols being resolved have been
  -- * moved to the error state due to the failure of a dependency. If this
  -- * method returns an error then clients should log it and call
  -- * LLVMOrcMaterializationResponsibilityFailMaterialization.
  -- * If no dependencies have been registered for the symbols covered by this
  -- * MaterializationResponsibiility then this method is guaranteed to return
  -- * LLVMErrorSuccess.
  --  

   function Orc_Materialization_Responsibility_Notify_Emitted (MR : Orc_Materialization_Responsibility_T) return LLVM.Error.Error_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:737
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcMaterializationResponsibilityNotifyEmitted";

  --*
  -- * Attempt to claim responsibility for new definitions. This method can be
  -- * used to claim responsibility for symbols that are added to a
  -- * materialization unit during the compilation process (e.g. literal pool
  -- * symbols). Symbol linkage rules are the same as for symbols that are
  -- * defined up front: duplicate strong definitions will result in errors.
  -- * Duplicate weak definitions will be discarded (in which case they will
  -- * not be added to this responsibility instance).
  -- *
  -- * This method can be used by materialization units that want to add
  -- * additional symbols at materialization time (e.g. stubs, compile
  -- * callbacks, metadata)
  --  

   function Orc_Materialization_Responsibility_Define_Materializing
     (MR : Orc_Materialization_Responsibility_T;
      Pairs : Orc_C_Symbol_Flags_Map_Pairs_T;
      Num_Pairs : stddef_h.size_t) return LLVM.Error.Error_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:753
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcMaterializationResponsibilityDefineMaterializing";

  --*
  -- * Notify all not-yet-emitted covered by this MaterializationResponsibility
  -- * instance that an error has occurred.
  -- * This will remove all symbols covered by this MaterializationResponsibilty
  -- * from the target JITDylib, and send an error to any queries waiting on
  -- * these symbols.
  --  

   procedure Orc_Materialization_Responsibility_Fail_Materialization (MR : Orc_Materialization_Responsibility_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:764
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcMaterializationResponsibilityFailMaterialization";

  --*
  -- * Transfers responsibility to the given MaterializationUnit for all
  -- * symbols defined by that MaterializationUnit. This allows
  -- * materializers to break up work based on run-time information (e.g.
  -- * by introspecting which symbols have actually been looked up and
  -- * materializing only those).
  --  

   function Orc_Materialization_Responsibility_Replace (MR : Orc_Materialization_Responsibility_T; MU : Orc_Materialization_Unit_T) return LLVM.Error.Error_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:774
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcMaterializationResponsibilityReplace";

  --*
  -- * Delegates responsibility for the given symbols to the returned
  -- * materialization responsibility. Useful for breaking up work between
  -- * threads, or different kinds of materialization processes.
  -- *
  -- * The caller retains responsibility of the the passed
  -- * MaterializationResponsibility.
  --  

   function Orc_Materialization_Responsibility_Delegate
     (MR : Orc_Materialization_Responsibility_T;
      Symbols : System.Address;
      Num_Symbols : stddef_h.size_t;
      Result : System.Address) return LLVM.Error.Error_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:786
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcMaterializationResponsibilityDelegate";

  --*
  -- * Adds dependencies to a symbol that the MaterializationResponsibility is
  -- * responsible for.
  -- *
  -- * This function takes ownership of Dependencies struct. The Names
  -- * array have been retained for this function. This allows the following
  -- * pattern...
  -- *
  -- *   LLVMOrcSymbolStringPoolEntryRef Names[] = {...};
  -- *   LLVMOrcCDependenceMapPair Dependence = {JD, {Names, sizeof(Names)}}
  -- *   LLVMOrcMaterializationResponsibilityAddDependencies(JD, Name, &Dependence,
  -- * 1);
  -- *
  -- * ... without requiring cleanup of the elements of the Names array afterwards.
  -- *
  -- * The client is still responsible for deleting the Dependencies.Names array
  -- * itself.
  --  

   procedure Orc_Materialization_Responsibility_Add_Dependencies
     (MR : Orc_Materialization_Responsibility_T;
      Name : Orc_Symbol_String_Pool_Entry_T;
      Dependencies : Orc_C_Dependence_Map_Pairs_T;
      Num_Pairs : stddef_h.size_t)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:809
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcMaterializationResponsibilityAddDependencies";

  --*
  -- * Adds dependencies to all symbols that the MaterializationResponsibility is
  -- * responsible for. See LLVMOrcMaterializationResponsibilityAddDependencies for
  -- * notes about memory responsibility.
  --  

   procedure Orc_Materialization_Responsibility_Add_Dependencies_For_All
     (MR : Orc_Materialization_Responsibility_T;
      Dependencies : Orc_C_Dependence_Map_Pairs_T;
      Num_Pairs : stddef_h.size_t)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:819
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcMaterializationResponsibilityAddDependenciesForAll";

  --*
  -- * Create a "bare" JITDylib.
  -- *
  -- * The client is responsible for ensuring that the JITDylib's name is unique,
  -- * e.g. by calling LLVMOrcExecutionSessionGetJTIDylibByName first.
  -- *
  -- * This call does not install any library code or symbols into the newly
  -- * created JITDylib. The client is responsible for all configuration.
  --  

function Orc_Execution_Session_Create_Bare_JIT_Dylib
     (ES   : Orc_Execution_Session_T;
      Name : String)
      return Orc_JIT_Dylib_T;

  --*
  -- * Create a JITDylib.
  -- *
  -- * The client is responsible for ensuring that the JITDylib's name is unique,
  -- * e.g. by calling LLVMOrcExecutionSessionGetJTIDylibByName first.
  -- *
  -- * If a Platform is attached to the ExecutionSession then
  -- * Platform::setupJITDylib will be called to install standard platform symbols
  -- * (e.g. standard library interposes). If no Platform is installed then this
  -- * call is equivalent to LLVMExecutionSessionRefCreateBareJITDylib and will
  -- * always return success.
  --  

function Orc_Execution_Session_Create_JIT_Dylib
     (ES     : Orc_Execution_Session_T;
      Result : System.Address;
      Name   : String)
      return LLVM.Error.Error_T;

  --*
  -- * Returns the JITDylib with the given name, or NULL if no such JITDylib
  -- * exists.
  --  

function Orc_Execution_Session_Get_JIT_Dylib_By_Name
     (ES   : Orc_Execution_Session_T;
      Name : String)
      return Orc_JIT_Dylib_T;

  --*
  -- * Return a reference to a newly created resource tracker associated with JD.
  -- * The tracker is returned with an initial ref-count of 1, and must be released
  -- * with LLVMOrcReleaseResourceTracker when no longer needed.
  --  

   function Orc_JIT_Dylib_Create_Resource_Tracker (JD : Orc_JIT_Dylib_T) return Orc_Resource_Tracker_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:867
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITDylibCreateResourceTracker";

  --*
  -- * Return a reference to the default resource tracker for the given JITDylib.
  -- * This operation will increase the retain count of the tracker: Clients should
  -- * call LLVMOrcReleaseResourceTracker when the result is no longer needed.
  --  

   function Orc_JIT_Dylib_Get_Default_Resource_Tracker (JD : Orc_JIT_Dylib_T) return Orc_Resource_Tracker_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:875
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITDylibGetDefaultResourceTracker";

  --*
  -- * Add the given MaterializationUnit to the given JITDylib.
  -- *
  -- * If this operation succeeds then JITDylib JD will take ownership of MU.
  -- * If the operation fails then ownership remains with the caller who should
  -- * call LLVMOrcDisposeMaterializationUnit to destroy it.
  --  

   function Orc_JIT_Dylib_Define (JD : Orc_JIT_Dylib_T; MU : Orc_Materialization_Unit_T) return LLVM.Error.Error_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:884
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITDylibDefine";

  --*
  -- * Calls remove on all trackers associated with this JITDylib, see
  -- * JITDylib::clear().
  --  

   function Orc_JIT_Dylib_Clear (JD : Orc_JIT_Dylib_T) return LLVM.Error.Error_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:891
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITDylibClear";

  --*
  -- * Add a DefinitionGenerator to the given JITDylib.
  -- *
  -- * The JITDylib will take ownership of the given generator: The client is no
  -- * longer responsible for managing its memory.
  --  

   procedure Orc_JIT_Dylib_Add_Generator (JD : Orc_JIT_Dylib_T; DG : Orc_Definition_Generator_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:899
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITDylibAddGenerator";

  --*
  -- * Create a custom generator.
  --  

   function Orc_Create_Custom_CAPI_Definition_Generator (F : Orc_CAPI_Definition_Generator_Try_To_Generate_Function_T; Ctx : System.Address) return Orc_Definition_Generator_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:905
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateCustomCAPIDefinitionGenerator";

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
      Filter_Ctx : System.Address) return LLVM.Error.Error_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:926
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess";

  --*
  -- * Get a LLVMOrcCreateDynamicLibararySearchGeneratorForPath that will reflect
  -- * library symbols into the JITDylib. On success the resulting generator is
  -- * owned by the client. Ownership is typically transferred by adding the
  -- * instance to a JITDylib using LLVMOrcJITDylibAddGenerator,
  -- *
  -- * The GlobalPrefix argument specifies the character that appears on the front
  -- * of linker-mangled symbols for the target platform (e.g. '_' on MachO).
  -- * If non-null, this character will be stripped from the start of all symbol
  -- * strings before passing the remaining substring to dlsym.
  -- *
  -- * The optional Filter and Ctx arguments can be used to supply a symbol name
  -- * filter: Only symbols for which the filter returns true will be visible to
  -- * JIT'd code. If the Filter argument is null then all library symbols will
  -- * be visible to JIT'd code. Note that the symbol name passed to the Filter
  -- * function is the full mangled symbol: The client is responsible for stripping
  -- * the global prefix if present.
  -- * 
  -- * THIS API IS EXPERIMENTAL AND LIKELY TO CHANGE IN THE NEAR FUTURE!
  -- * 
  --  

function Orc_Create_Dynamic_Library_Search_Generator_For_Path
     (Result        : System.Address;
      File_Name     : String;
      Global_Prefix : char;
      Filter        : Orc_Symbol_Predicate_T;
      Filter_Ctx    : System.Address)
      return LLVM.Error.Error_T;

  --*
  -- * Get a LLVMOrcCreateStaticLibrarySearchGeneratorForPath that will reflect
  -- * static library symbols into the JITDylib. On success the resulting
  -- * generator is owned by the client. Ownership is typically transferred by
  -- * adding the instance to a JITDylib using LLVMOrcJITDylibAddGenerator,
  -- *
  -- * Call with the optional TargetTriple argument will succeed if the file at
  -- * the given path is a static library or a MachO universal binary containing a
  -- * static library that is compatible with the given triple. Otherwise it will
  -- * return an error.
  -- *
  -- * THIS API IS EXPERIMENTAL AND LIKELY TO CHANGE IN THE NEAR FUTURE!
  -- * 
  --  

function Orc_Create_Static_Library_Search_Generator_For_Path
     (Result        : System.Address;
      Obj_Layer     : Orc_Object_Layer_T;
      File_Name     : String;
      Target_Triple : String)
      return LLVM.Error.Error_T;

  --*
  -- * Create a ThreadSafeContext containing a new LLVMContext.
  -- *
  -- * Ownership of the underlying ThreadSafeContext data is shared: Clients
  -- * can and should dispose of their ThreadSafeContext as soon as they no longer
  -- * need to refer to it directly. Other references (e.g. from ThreadSafeModules)
  -- * will keep the data alive as long as it is needed.
  --  

   function Orc_Create_New_Thread_Safe_Context return Orc_Thread_Safe_Context_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:981
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateNewThreadSafeContext";

  --*
  -- * Get a reference to the wrapped LLVMContext.
  --  

   function Orc_Thread_Safe_Context_Get_Context (TS_Ctx : Orc_Thread_Safe_Context_T) return LLVM.Types.Context_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:987
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcThreadSafeContextGetContext";

  --*
  -- * Dispose of a ThreadSafeContext.
  --  

   procedure Orc_Dispose_Thread_Safe_Context (TS_Ctx : Orc_Thread_Safe_Context_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:992
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeThreadSafeContext";

  --*
  -- * Create a ThreadSafeModule wrapper around the given LLVM module. This takes
  -- * ownership of the M argument which should not be disposed of or referenced
  -- * after this function returns.
  -- *
  -- * Ownership of the ThreadSafeModule is unique: If it is transferred to the JIT
  -- * (e.g. by LLVMOrcLLJITAddLLVMIRModule) then the client is no longer
  -- * responsible for it. If it is not transferred to the JIT then the client
  -- * should call LLVMOrcDisposeThreadSafeModule to dispose of it.
  --  

   function Orc_Create_New_Thread_Safe_Module (M : LLVM.Types.Module_T; TS_Ctx : Orc_Thread_Safe_Context_T) return Orc_Thread_Safe_Module_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1005
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateNewThreadSafeModule";

  --*
  -- * Dispose of a ThreadSafeModule. This should only be called if ownership has
  -- * not been passed to LLJIT (e.g. because some error prevented the client from
  -- * adding this to the JIT).
  --  

   procedure Orc_Dispose_Thread_Safe_Module (TSM : Orc_Thread_Safe_Module_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1013
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeThreadSafeModule";

  --*
  -- * Apply the given function to the module contained in this ThreadSafeModule.
  --  

   function Orc_Thread_Safe_Module_With_Module_Do
     (TSM : Orc_Thread_Safe_Module_T;
      F : Orc_Generic_IR_Module_Operation_Function_T;
      Ctx : System.Address) return LLVM.Error.Error_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1019
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcThreadSafeModuleWithModuleDo";

  --*
  -- * Create a JITTargetMachineBuilder by detecting the host.
  -- *
  -- * On success the client owns the resulting JITTargetMachineBuilder. It must be
  -- * passed to a consuming operation (e.g.
  -- * LLVMOrcLLJITBuilderSetJITTargetMachineBuilder) or disposed of by calling
  -- * LLVMOrcDisposeJITTargetMachineBuilder.
  --  

   function Orc_JIT_Target_Machine_Builder_Detect_Host (Result : System.Address) return LLVM.Error.Error_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1031
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITTargetMachineBuilderDetectHost";

  --*
  -- * Create a JITTargetMachineBuilder from the given TargetMachine template.
  -- *
  -- * This operation takes ownership of the given TargetMachine and destroys it
  -- * before returing. The resulting JITTargetMachineBuilder is owned by the client
  -- * and must be passed to a consuming operation (e.g.
  -- * LLVMOrcLLJITBuilderSetJITTargetMachineBuilder) or disposed of by calling
  -- * LLVMOrcDisposeJITTargetMachineBuilder.
  --  

   function Orc_JIT_Target_Machine_Builder_Create_From_Target_Machine (TM : LLVM.Target_Machine.Target_Machine_T) return Orc_JIT_Target_Machine_Builder_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1044
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITTargetMachineBuilderCreateFromTargetMachine";

  --*
  -- * Dispose of a JITTargetMachineBuilder.
  --  

   procedure Orc_Dispose_JIT_Target_Machine_Builder (JTMB : Orc_JIT_Target_Machine_Builder_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1049
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeJITTargetMachineBuilder";

  --*
  -- * Returns the target triple for the given JITTargetMachineBuilder as a string.
  -- *
  -- * The caller owns the resulting string as must dispose of it by calling
  -- * LLVMDisposeMessage
  --  

function Orc_JIT_Target_Machine_Get_Target_Triple
     (JTMB : Orc_JIT_Target_Machine_Builder_T)
      return String;

  --*
  -- * Sets the target triple for the given JITTargetMachineBuilder to the given
  -- * string.
  --  

procedure Orc_JIT_Target_Machine_Set_Target_Triple
     (JTMB          : Orc_JIT_Target_Machine_Builder_T;
      Target_Triple : String);

  --*
  -- * Add an object to an ObjectLayer to the given JITDylib.
  -- *
  -- * Adds a buffer representing an object file to the given JITDylib using the
  -- * given ObjectLayer instance. This operation transfers ownership of the buffer
  -- * to the ObjectLayer instance. The buffer should not be disposed of or
  -- * referenced once this function returns.
  -- *
  -- * Resources associated with the given object will be tracked by the given
  -- * JITDylib's default ResourceTracker.
  --  

   function Orc_Object_Layer_Add_Object_File
     (Obj_Layer : Orc_Object_Layer_T;
      JD : Orc_JIT_Dylib_T;
      Obj_Buffer : LLVM.Types.Memory_Buffer_T) return LLVM.Error.Error_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1079
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcObjectLayerAddObjectFile";

  --*
  -- * Add an object to an ObjectLayer using the given ResourceTracker.
  -- *
  -- * Adds a buffer representing an object file to the given ResourceTracker's
  -- * JITDylib using the given ObjectLayer instance. This operation transfers
  -- * ownership of the buffer to the ObjectLayer instance. The buffer should not
  -- * be disposed of or referenced once this function returns.
  -- *
  -- * Resources associated with the given object will be tracked by
  -- * ResourceTracker RT.
  --  

   function Orc_Object_Layer_Add_Object_File_With_RT
     (Obj_Layer : Orc_Object_Layer_T;
      RT : Orc_Resource_Tracker_T;
      Obj_Buffer : LLVM.Types.Memory_Buffer_T) return LLVM.Error.Error_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1095
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcObjectLayerAddObjectFileWithRT";

  --*
  -- * Emit an object buffer to an ObjectLayer.
  -- *
  -- * Ownership of the responsibility object and object buffer pass to this
  -- * function. The client is not responsible for cleanup.
  --  

   procedure Orc_Object_Layer_Emit
     (Obj_Layer : Orc_Object_Layer_T;
      R : Orc_Materialization_Responsibility_T;
      Obj_Buffer : LLVM.Types.Memory_Buffer_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1105
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcObjectLayerEmit";

  --*
  -- * Dispose of an ObjectLayer.
  --  

   procedure Orc_Dispose_Object_Layer (Obj_Layer : Orc_Object_Layer_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1112
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeObjectLayer";

   procedure Orc_IR_Transform_Layer_Emit
     (IR_Transform_Layer : Orc_IR_Transform_Layer_T;
      MR : Orc_Materialization_Responsibility_T;
      TSM : Orc_Thread_Safe_Module_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1114
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcIRTransformLayerEmit";

  --*
  -- * Set the transform function of the provided transform layer, passing through a
  -- * pointer to user provided context.
  --  

   procedure Orc_IR_Transform_Layer_Set_Transform
     (IR_Transform_Layer : Orc_IR_Transform_Layer_T;
      Transform_Function : Orc_IR_Transform_Layer_Transform_Function_T;
      Ctx : System.Address)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1122
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcIRTransformLayerSetTransform";

  --*
  -- * Set the transform function on an LLVMOrcObjectTransformLayer.
  --  

   procedure Orc_Object_Transform_Layer_Set_Transform
     (Obj_Transform_Layer : Orc_Object_Transform_Layer_T;
      Transform_Function : Orc_Object_Transform_Layer_Transform_Function_T;
      Ctx : System.Address)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1129
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcObjectTransformLayerSetTransform";

  --*
  -- * Create a LocalIndirectStubsManager from the given target triple.
  -- *
  -- * The resulting IndirectStubsManager is owned by the client
  -- * and must be disposed of by calling LLVMOrcDisposeDisposeIndirectStubsManager.
  --  

function Orc_Create_Local_Indirect_Stubs_Manager
     (Target_Triple : String)
      return Orc_Indirect_Stubs_Manager_T;

  --*
  -- * Dispose of an IndirectStubsManager.
  --  

   procedure Orc_Dispose_Indirect_Stubs_Manager (ISM : Orc_Indirect_Stubs_Manager_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1145
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeIndirectStubsManager";

function Orc_Create_Local_Lazy_Call_Through_Manager
     (Target_Triple      : String;
      ES                 : Orc_Execution_Session_T;
      Error_Handler_Addr : Orc_JIT_Target_Address_T;
      LCTM               : System.Address)
      return LLVM.Error.Error_T;

  --*
  -- * Dispose of an LazyCallThroughManager.
  --  

   procedure Orc_Dispose_Lazy_Call_Through_Manager (LCTM : Orc_Lazy_Call_Through_Manager_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1155
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeLazyCallThroughManager";

  --*
  -- * Create a DumpObjects instance.
  -- *
  -- * DumpDir specifies the path to write dumped objects to. DumpDir may be empty
  -- * in which case files will be dumped to the working directory.
  -- *
  -- * IdentifierOverride specifies a file name stem to use when dumping objects.
  -- * If empty then each MemoryBuffer's identifier will be used (with a .o suffix
  -- * added if not already present). If an identifier override is supplied it will
  -- * be used instead, along with an incrementing counter (since all buffers will
  -- * use the same identifier, the resulting files will be named <ident>.o,
  -- * <ident>.2.o, <ident>.3.o, and so on). IdentifierOverride should not contain
  -- * an extension, as a .o suffix will be added by DumpObjects.
  --  

function Orc_Create_Dump_Objects
     (Dump_Dir            : String;
      Identifier_Override : String)
      return Orc_Dump_Objects_T;

  --*
  -- * Dispose of a DumpObjects instance.
  --  

   procedure Orc_Dispose_Dump_Objects (Dump_Objects : Orc_Dump_Objects_T)  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1178
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeDumpObjects";

  --*
  -- * Dump the contents of the given MemoryBuffer.
  --  

   function Orc_Dump_Objects_Call_Operator (Dump_Objects : Orc_Dump_Objects_T; Obj_Buffer : System.Address) return LLVM.Error.Error_T  -- llvm-14.0.1.install/include/llvm-c/Orc.h:1183
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDumpObjects_CallOperator";

  --*
  -- * @}
  --  

end LLVM.Orc;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
