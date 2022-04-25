pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Orc;
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

   function Orc_Create_RT_Dyld_Object_Linking_Layer_With_Section_Memory_Manager (ES : LLVM.Orc.Orc_Execution_Session_T) return LLVM.Orc.Orc_Object_Layer_T  -- llvm-14.0.1.install/include/llvm-c/OrcEE.h:47
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager";

  --*
  -- * Add the given listener to the given RTDyldObjectLinkingLayer.
  -- *
  -- * Note: Layer must be an RTDyldObjectLinkingLayer instance or
  -- * behavior is undefined.
  --  

   procedure Orc_RT_Dyld_Object_Linking_Layer_Register_JIT_Event_Listener (RT_Dyld_Obj_Linking_Layer : LLVM.Orc.Orc_Object_Layer_T; Listener : LLVM.Types.JIT_Event_Listener_T)  -- llvm-14.0.1.install/include/llvm-c/OrcEE.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOrcRTDyldObjectLinkingLayerRegisterJITEventListener";

  --*
  -- * @}
  --  

end LLVM.Orc_EE;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
