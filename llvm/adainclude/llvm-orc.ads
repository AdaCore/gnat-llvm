pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with stdint_h;
with System;
with LLVM.Error;
with stddef_h;
with Interfaces.C.Strings;
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

   subtype Orc_JIT_Target_Address_T is stdint_h.uint64_t;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:39

  --*
  -- * Represents generic linkage flags for a symbol definition.
  --  

   subtype JIT_Symbol_Generic_Flags_T is unsigned;
   JIT_Symbol_Generic_Flags_Exported : constant JIT_Symbol_Generic_Flags_T := 1;
   JIT_Symbol_Generic_Flags_Weak : constant JIT_Symbol_Generic_Flags_T := 2;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:47

  --*
  -- * Represents target specific flags for a symbol definition.
  --  

   subtype JIT_Target_Symbol_Flags_T is stdint_h.uint8_t;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:52

  --*
  -- * Represents the linkage flags for a symbol definition.
  --  

   type JIT_Symbol_Flags_T is record
      GenericFlags : aliased stdint_h.uint8_t;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:58
      TargetFlags : aliased stdint_h.uint8_t;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:59
   end record
   with Convention => C_Pass_By_Copy;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:60

  --*
  -- * Represents an evaluated symbol address and flags.
  --  

   type JIT_Evaluated_Symbol_T is record
      Address : aliased Orc_JIT_Target_Address_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:66
      Flags : aliased JIT_Symbol_Flags_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:67
   end record
   with Convention => C_Pass_By_Copy;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:68

  --*
  -- * A reference to an orc::ExecutionSession instance.
  --  

   type Orc_Opaque_Execution_Session_Impl_T is null record;   -- incomplete struct

   type Orc_Execution_Session_T is access all Orc_Opaque_Execution_Session_Impl_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:73

  --*
  -- * Error reporter function.
  --  

   type Orc_Error_Reporter_Function_T is access procedure (Arg_1 : System.Address; Arg_2 : LLVM.Error.Error_T)
   with Convention => C;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:78

  --*
  -- * A reference to an orc::SymbolStringPool.
  --  

   type Orc_Opaque_Symbol_String_Pool_Impl_T is null record;   -- incomplete struct

   type Orc_Symbol_String_Pool_T is access all Orc_Opaque_Symbol_String_Pool_Impl_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:83

  --*
  -- * A reference to an orc::SymbolStringPool table entry.
  --  

   type Orc_Opaque_Symbol_String_Pool_Entry_Impl_T is null record;   -- incomplete struct

   type Orc_Symbol_String_Pool_Entry_T is access all Orc_Opaque_Symbol_String_Pool_Entry_Impl_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:89

  --*
  -- * Represents a pair of a symbol name and an evaluated symbol.
  --  

   type JITC_Symbol_Map_Pair_T is record
      Name : Orc_Symbol_String_Pool_Entry_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:95
      Sym : aliased JIT_Evaluated_Symbol_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:96
   end record
   with Convention => C_Pass_By_Copy;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:97

  --*
  -- * Represents a list of (SymbolStringPtr, JITEvaluatedSymbol) pairs that can be
  -- * used to construct a SymbolMap.
  --  

   type Orc_C_Symbol_Map_Pairs_T is access all JITC_Symbol_Map_Pair_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:103

  --*
  -- * Lookup kind. This can be used by definition generators when deciding whether
  -- * to produce a definition for a requested symbol.
  -- *
  -- * This enum should be kept in sync with llvm::orc::LookupKind.
  --  

   type Orc_Lookup_Kind_T is 
     (Orc_Lookup_Kind_Static,
      Orc_Lookup_Kind_DL_Sym)
   with Convention => C;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:114

  --*
  -- * JITDylib lookup flags. This can be used by definition generators when
  -- * deciding whether to produce a definition for a requested symbol.
  -- *
  -- * This enum should be kept in sync with llvm::orc::JITDylibLookupFlags.
  --  

   type Orc_JIT_Dylib_Lookup_Flags_T is 
     (Orc_JIT_Dylib_Lookup_Flags_Match_Exported_Symbols_Only,
      Orc_JIT_Dylib_Lookup_Flags_Match_All_Symbols)
   with Convention => C;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:125

  --*
  -- * Symbol lookup flags for lookup sets. This should be kept in sync with
  -- * llvm::orc::SymbolLookupFlags.
  --  

   type Orc_Symbol_Lookup_Flags_T is 
     (Orc_Symbol_Lookup_Flags_Required_Symbol,
      Orc_Symbol_Lookup_Flags_Weakly_Referenced_Symbol)
   with Convention => C;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:134

  --*
  -- * An element type for a symbol lookup set.
  --  

   type Orc_C_Lookup_Set_Element_T is record
      Name : Orc_Symbol_String_Pool_Entry_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:140
      LookupFlags : aliased Orc_Symbol_Lookup_Flags_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:141
   end record
   with Convention => C_Pass_By_Copy;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:142

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

   type Orc_C_Lookup_Set_T is access all Orc_C_Lookup_Set_Element_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:155

  --*
  -- * A reference to an orc::MaterializationUnit.
  --  

   type Orc_Opaque_Materialization_Unit_Impl_T is null record;   -- incomplete struct

   type Orc_Materialization_Unit_T is access all Orc_Opaque_Materialization_Unit_Impl_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:160

  --*
  -- * A reference to an orc::JITDylib instance.
  --  

   type Orc_Opaque_JIT_Dylib_Impl_T is null record;   -- incomplete struct

   type Orc_JIT_Dylib_T is access all Orc_Opaque_JIT_Dylib_Impl_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:165

  --*
  -- * A reference to an orc::ResourceTracker instance.
  --  

   type Orc_Opaque_Resource_Tracker_Impl_T is null record;   -- incomplete struct

   type Orc_Resource_Tracker_T is access all Orc_Opaque_Resource_Tracker_Impl_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:170

  --*
  -- * A reference to an orc::DefinitionGenerator.
  --  

   type Orc_Opaque_Definition_Generator_Impl_T is null record;   -- incomplete struct

   type Orc_Definition_Generator_T is access all Orc_Opaque_Definition_Generator_Impl_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:176

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

   type Orc_Lookup_State_T is access all Orc_Opaque_Lookup_State_Impl_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:190

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
  -- * free the lookup state memory and notify the query object of the failures. If
  -- * LookupState is captured this function must return LLVMErrorSuccess.
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
   with Convention => C;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:225

  --*
  -- * Predicate function for SymbolStringPoolEntries.
  --  

   type Orc_Symbol_Predicate_T is access function (Arg_1 : System.Address; Arg_2 : Orc_Symbol_String_Pool_Entry_T) return int
   with Convention => C;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:234

  --*
  -- * A reference to an orc::ThreadSafeContext instance.
  --  

   type Orc_Opaque_Thread_Safe_Context_Impl_T is null record;   -- incomplete struct

   type Orc_Thread_Safe_Context_T is access all Orc_Opaque_Thread_Safe_Context_Impl_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:240

  --*
  -- * A reference to an orc::ThreadSafeModule instance.
  --  

   type Orc_Opaque_Thread_Safe_Module_Impl_T is null record;   -- incomplete struct

   type Orc_Thread_Safe_Module_T is access all Orc_Opaque_Thread_Safe_Module_Impl_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:245

  --*
  -- * A reference to an orc::JITTargetMachineBuilder instance.
  --  

   type Orc_Opaque_JIT_Target_Machine_Builder_Impl_T is null record;   -- incomplete struct

   type Orc_JIT_Target_Machine_Builder_T is access all Orc_Opaque_JIT_Target_Machine_Builder_Impl_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:251

  --*
  -- * A reference to an orc::ObjectLayer instance.
  --  

   type Orc_Opaque_Object_Layer_Impl_T is null record;   -- incomplete struct

   type Orc_Object_Layer_T is access all Orc_Opaque_Object_Layer_Impl_T;  -- llvm-12.0.0.src/include/llvm-c/Orc.h:256

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
      Ctx : System.Address)  -- llvm-12.0.0.src/include/llvm-c/Orc.h:267
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcExecutionSessionSetErrorReporter";

  --*
  -- * Return a reference to the SymbolStringPool for an ExecutionSession.
  -- *
  -- * Ownership of the pool remains with the ExecutionSession: The caller is
  -- * not required to free the pool.
  --  

   function Orc_Execution_Session_Get_Symbol_String_Pool (ES : Orc_Execution_Session_T) return Orc_Symbol_String_Pool_T  -- llvm-12.0.0.src/include/llvm-c/Orc.h:278
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

   procedure Orc_Symbol_String_Pool_Clear_Dead_Entries (SSP : Orc_Symbol_String_Pool_T)  -- llvm-12.0.0.src/include/llvm-c/Orc.h:290
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
   function Orc_Execution_Session_Intern_C
     (ES   : Orc_Execution_Session_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return Orc_Symbol_String_Pool_Entry_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcExecutionSessionIntern";

  --*
  -- * Increments the ref-count for a SymbolStringPool entry.
  --  

   procedure Orc_Retain_Symbol_String_Pool_Entry (S : Orc_Symbol_String_Pool_Entry_T)  -- llvm-12.0.0.src/include/llvm-c/Orc.h:310
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcRetainSymbolStringPoolEntry";

  --*
  -- * Reduces the ref-count for of a SymbolStringPool entry.
  --  

   procedure Orc_Release_Symbol_String_Pool_Entry (S : Orc_Symbol_String_Pool_Entry_T)  -- llvm-12.0.0.src/include/llvm-c/Orc.h:315
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcReleaseSymbolStringPoolEntry";

function Orc_Symbol_String_Pool_Entry_Str
     (S : Orc_Symbol_String_Pool_Entry_T)
      return String;
   function Orc_Symbol_String_Pool_Entry_Str_C
     (S : Orc_Symbol_String_Pool_Entry_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcSymbolStringPoolEntryStr";

  --*
  -- * Reduces the ref-count of a ResourceTracker.
  --  

   procedure Orc_Release_Resource_Tracker (RT : Orc_Resource_Tracker_T)  -- llvm-12.0.0.src/include/llvm-c/Orc.h:322
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcReleaseResourceTracker";

  --*
  -- * Transfers tracking of all resources associated with resource tracker SrcRT
  -- * to resource tracker DstRT.
  --  

   procedure Orc_Resource_Tracker_Transfer_To (Src_RT : Orc_Resource_Tracker_T; Dst_RT : Orc_Resource_Tracker_T)  -- llvm-12.0.0.src/include/llvm-c/Orc.h:328
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcResourceTrackerTransferTo";

  --*
  -- * Remove all resources associated with the given tracker. See
  -- * ResourceTracker::remove().
  --  

   function Orc_Resource_Tracker_Remove (RT : Orc_Resource_Tracker_T) return LLVM.Error.Error_T  -- llvm-12.0.0.src/include/llvm-c/Orc.h:335
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcResourceTrackerRemove";

  --*
  -- * Dispose of a JITDylib::DefinitionGenerator. This should only be called if
  -- * ownership has not been passed to a JITDylib (e.g. because some error
  -- * prevented the client from calling LLVMOrcJITDylibAddGenerator).
  --  

   procedure Orc_Dispose_Definition_Generator (DG : Orc_Definition_Generator_T)  -- llvm-12.0.0.src/include/llvm-c/Orc.h:342
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeDefinitionGenerator";

  --*
  -- * Dispose of a MaterializationUnit.
  --  

   procedure Orc_Dispose_Materialization_Unit (MU : Orc_Materialization_Unit_T)  -- llvm-12.0.0.src/include/llvm-c/Orc.h:347
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeMaterializationUnit";

  --*
  -- * Create a MaterializationUnit to define the given symbols as pointing to
  -- * the corresponding raw addresses.
  --  

   function Orc_Absolute_Symbols (Syms : Orc_C_Symbol_Map_Pairs_T; Num_Pairs : stddef_h.size_t) return Orc_Materialization_Unit_T  -- llvm-12.0.0.src/include/llvm-c/Orc.h:354
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcAbsoluteSymbols";

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
   function Orc_Execution_Session_Create_Bare_JIT_Dylib_C
     (ES   : Orc_Execution_Session_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return Orc_JIT_Dylib_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcExecutionSessionCreateBareJITDylib";

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
   function Orc_Execution_Session_Create_JIT_Dylib_C
     (ES     : Orc_Execution_Session_T;
      Result : System.Address;
      Name   : Interfaces.C.Strings.chars_ptr)
      return LLVM.Error.Error_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcExecutionSessionCreateJITDylib";

  --*
  -- * Returns the JITDylib with the given name, or NULL if no such JITDylib
  -- * exists.
  --  

function Orc_Execution_Session_Get_JIT_Dylib_By_Name
     (ES   : Orc_Execution_Session_T;
      Name : String)
      return Orc_JIT_Dylib_T;
   function Orc_Execution_Session_Get_JIT_Dylib_By_Name_C
     (ES   : Orc_Execution_Session_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return Orc_JIT_Dylib_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMOrcExecutionSessionGetJITDylibByName";

  --*
  -- * Return a reference to a newly created resource tracker associated with JD.
  -- * The tracker is returned with an initial ref-count of 1, and must be released
  -- * with LLVMOrcReleaseResourceTracker when no longer needed.
  --  

   function Orc_JIT_Dylib_Create_Resource_Tracker (JD : Orc_JIT_Dylib_T) return Orc_Resource_Tracker_T  -- llvm-12.0.0.src/include/llvm-c/Orc.h:400
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITDylibCreateResourceTracker";

  --*
  -- * Return a reference to the default resource tracker for the given JITDylib.
  -- * This operation will increase the retain count of the tracker: Clients should
  -- * call LLVMOrcReleaseResourceTracker when the result is no longer needed.
  --  

   function Orc_JIT_Dylib_Get_Default_Resource_Tracker (JD : Orc_JIT_Dylib_T) return Orc_Resource_Tracker_T  -- llvm-12.0.0.src/include/llvm-c/Orc.h:408
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

   function Orc_JIT_Dylib_Define (JD : Orc_JIT_Dylib_T; MU : Orc_Materialization_Unit_T) return LLVM.Error.Error_T  -- llvm-12.0.0.src/include/llvm-c/Orc.h:417
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITDylibDefine";

  --*
  -- * Calls remove on all trackers associated with this JITDylib, see
  -- * JITDylib::clear().
  --  

   function Orc_JIT_Dylib_Clear (JD : Orc_JIT_Dylib_T) return LLVM.Error.Error_T  -- llvm-12.0.0.src/include/llvm-c/Orc.h:424
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITDylibClear";

  --*
  -- * Add a DefinitionGenerator to the given JITDylib.
  -- *
  -- * The JITDylib will take ownership of the given generator: The client is no
  -- * longer responsible for managing its memory.
  --  

   procedure Orc_JIT_Dylib_Add_Generator (JD : Orc_JIT_Dylib_T; DG : Orc_Definition_Generator_T)  -- llvm-12.0.0.src/include/llvm-c/Orc.h:432
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITDylibAddGenerator";

  --*
  -- * Create a custom generator.
  --  

   function Orc_Create_Custom_CAPI_Definition_Generator (F : Orc_CAPI_Definition_Generator_Try_To_Generate_Function_T; Ctx : System.Address) return Orc_Definition_Generator_T  -- llvm-12.0.0.src/include/llvm-c/Orc.h:438
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
      Filter_Ctx : System.Address) return LLVM.Error.Error_T  -- llvm-12.0.0.src/include/llvm-c/Orc.h:459
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess";

  --*
  -- * Create a ThreadSafeContext containing a new LLVMContext.
  -- *
  -- * Ownership of the underlying ThreadSafeContext data is shared: Clients
  -- * can and should dispose of their ThreadSafeContext as soon as they no longer
  -- * need to refer to it directly. Other references (e.g. from ThreadSafeModules)
  -- * will keep the data alive as long as it is needed.
  --  

   function Orc_Create_New_Thread_Safe_Context return Orc_Thread_Safe_Context_T  -- llvm-12.0.0.src/include/llvm-c/Orc.h:471
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateNewThreadSafeContext";

  --*
  -- * Get a reference to the wrapped LLVMContext.
  --  

   function Orc_Thread_Safe_Context_Get_Context (TS_Ctx : Orc_Thread_Safe_Context_T) return LLVM.Types.Context_T  -- llvm-12.0.0.src/include/llvm-c/Orc.h:477
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcThreadSafeContextGetContext";

  --*
  -- * Dispose of a ThreadSafeContext.
  --  

   procedure Orc_Dispose_Thread_Safe_Context (TS_Ctx : Orc_Thread_Safe_Context_T)  -- llvm-12.0.0.src/include/llvm-c/Orc.h:482
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

   function Orc_Create_New_Thread_Safe_Module (M : LLVM.Types.Module_T; TS_Ctx : Orc_Thread_Safe_Context_T) return Orc_Thread_Safe_Module_T  -- llvm-12.0.0.src/include/llvm-c/Orc.h:495
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateNewThreadSafeModule";

  --*
  -- * Dispose of a ThreadSafeModule. This should only be called if ownership has
  -- * not been passed to LLJIT (e.g. because some error prevented the client from
  -- * adding this to the JIT).
  --  

   procedure Orc_Dispose_Thread_Safe_Module (TSM : Orc_Thread_Safe_Module_T)  -- llvm-12.0.0.src/include/llvm-c/Orc.h:503
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

   function Orc_JIT_Target_Machine_Builder_Detect_Host (Result : System.Address) return LLVM.Error.Error_T  -- llvm-12.0.0.src/include/llvm-c/Orc.h:512
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

   function Orc_JIT_Target_Machine_Builder_Create_From_Target_Machine (TM : LLVM.Target_Machine.Target_Machine_T) return Orc_JIT_Target_Machine_Builder_T  -- llvm-12.0.0.src/include/llvm-c/Orc.h:524
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcJITTargetMachineBuilderCreateFromTargetMachine";

  --*
  -- * Dispose of a JITTargetMachineBuilder.
  --  

   procedure Orc_Dispose_JIT_Target_Machine_Builder (JTMB : Orc_JIT_Target_Machine_Builder_T)  -- llvm-12.0.0.src/include/llvm-c/Orc.h:529
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeJITTargetMachineBuilder";

  --*
  -- * Dispose of an ObjectLayer.
  --  

   procedure Orc_Dispose_Object_Layer (Obj_Layer : Orc_Object_Layer_T)  -- llvm-12.0.0.src/include/llvm-c/Orc.h:535
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcDisposeObjectLayer";

end LLVM.Orc;

