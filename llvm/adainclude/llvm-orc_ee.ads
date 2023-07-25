pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with LLVM.Orc;
with LLVM.Execution_Engine;
with LLVM.Types;

package LLVM.Orc_EE is

  --===-- llvm-c/OrcEE.h - OrcV2 C bindings ExecutionEngine utils -*- C++ -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to ExecutionEngine based utils, e.g.  *|
  --|* RTDyldObjectLinkingLayer (based on RuntimeDyld) in Orc.                    *|
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

   type Memory_Manager_Create_Context_Callback_T is access function (Arg_1 : System.Address) return System.Address
   with Convention => C;  -- install/include/llvm-c/OrcEE.h:35

   type Memory_Manager_Notify_Terminating_Callback_T is access procedure (Arg_1 : System.Address)
   with Convention => C;  -- install/include/llvm-c/OrcEE.h:36

  --*
  -- * @defgroup LLVMCExecutionEngineORCEE ExecutionEngine-based ORC Utils
  -- * @ingroup LLVMCExecutionEngine
  -- *
  -- * @{
  --  

  --*
  -- * Create a RTDyldObjectLinkingLayer instance using the standard
  -- * SectionMemoryManager for memory management.
  --  

   function Orc_Create_RT_Dyld_Object_Linking_Layer_With_Section_Memory_Manager (ES : LLVM.Orc.Orc_Execution_Session_T) return LLVM.Orc.Orc_Object_Layer_T  -- install/include/llvm-c/OrcEE.h:50
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager";

  --*
  -- * Create a RTDyldObjectLinkingLayer instance using MCJIT-memory-manager-like
  -- * callbacks.
  -- *
  -- * This is intended to simplify transitions for existing MCJIT clients. The
  -- * callbacks used are similar (but not identical) to the callbacks for
  -- * LLVMCreateSimpleMCJITMemoryManager: Unlike MCJIT, RTDyldObjectLinkingLayer
  -- * will create a new memory manager for each object linked by calling the given
  -- * CreateContext callback. This allows for code removal by destroying each
  -- * allocator individually. Every allocator will be destroyed (if it has not been
  -- * already) at RTDyldObjectLinkingLayer destruction time, and the
  -- * NotifyTerminating callback will be called to indicate that no further
  -- * allocation contexts will be created.
  -- *
  -- * To implement MCJIT-like behavior clients can implement CreateContext,
  -- * NotifyTerminating, and Destroy as:
  -- *
  -- *   void *CreateContext(void *CtxCtx) { return CtxCtx; }
  -- *   void NotifyTerminating(void *CtxCtx) { MyOriginalDestroy(CtxCtx); }
  -- *   void Destroy(void *Ctx) { }
  -- *
  -- * This scheme simply reuses the CreateContextCtx pointer as the one-and-only
  -- * allocation context.
  --  

   function Orc_Create_RT_Dyld_Object_Linking_Layer_With_MCJIT_Memory_Manager_Like_Callbacks
     (ES : LLVM.Orc.Orc_Execution_Session_T;
      Create_Context_Ctx : System.Address;
      Create_Context : Memory_Manager_Create_Context_Callback_T;
      Notify_Terminating : Memory_Manager_Notify_Terminating_Callback_T;
      Allocate_Code_Section : LLVM.Execution_Engine.Memory_Manager_Allocate_Code_Section_Callback_T;
      Allocate_Data_Section : LLVM.Execution_Engine.Memory_Manager_Allocate_Data_Section_Callback_T;
      Finalize_Memory : LLVM.Execution_Engine.Memory_Manager_Finalize_Memory_Callback_T;
      Destroy : LLVM.Execution_Engine.Memory_Manager_Destroy_Callback_T) return LLVM.Orc.Orc_Object_Layer_T  -- install/include/llvm-c/OrcEE.h:78
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateRTDyldObjectLinkingLayerWithMCJITMemoryManagerLikeCallbacks";

  --*
  -- * Add the given listener to the given RTDyldObjectLinkingLayer.
  -- *
  -- * Note: Layer must be an RTDyldObjectLinkingLayer instance or
  -- * behavior is undefined.
  --  

   procedure Orc_RT_Dyld_Object_Linking_Layer_Register_JIT_Event_Listener (RT_Dyld_Obj_Linking_Layer : LLVM.Orc.Orc_Object_Layer_T; Listener : LLVM.Types.JIT_Event_Listener_T)  -- install/include/llvm-c/OrcEE.h:93
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcRTDyldObjectLinkingLayerRegisterJITEventListener";

  --*
  -- * @}
  --  

end LLVM.Orc_EE;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
