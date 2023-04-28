pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;

package LLVM.Initialization is

  --===-- llvm-c/Initialization.h - Initialization C Interface ------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to LLVM initialization routines,      *|
  --|* which must be called before you can use the functionality provided by      *|
  --|* the corresponding LLVM library.                                            *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCInitialization Initialization Routines
  -- * @ingroup LLVMC
  -- *
  -- * This module contains routines used to initialize the LLVM system.
  -- *
  -- * @{
  --  

   procedure Initialize_Core (R : LLVM.Types.Pass_Registry_T)  -- install/include/llvm-c/Initialization.h:33
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeCore";

   procedure Initialize_Transform_Utils (R : LLVM.Types.Pass_Registry_T)  -- install/include/llvm-c/Initialization.h:34
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeTransformUtils";

   procedure Initialize_Scalar_Opts (R : LLVM.Types.Pass_Registry_T)  -- install/include/llvm-c/Initialization.h:35
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeScalarOpts";

   procedure Initialize_Vectorization (R : LLVM.Types.Pass_Registry_T)  -- install/include/llvm-c/Initialization.h:36
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeVectorization";

   procedure Initialize_Inst_Combine (R : LLVM.Types.Pass_Registry_T)  -- install/include/llvm-c/Initialization.h:37
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeInstCombine";

   procedure Initialize_IPO (R : LLVM.Types.Pass_Registry_T)  -- install/include/llvm-c/Initialization.h:38
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeIPO";

   procedure Initialize_Analysis (R : LLVM.Types.Pass_Registry_T)  -- install/include/llvm-c/Initialization.h:39
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeAnalysis";

   procedure Initialize_IPA (R : LLVM.Types.Pass_Registry_T)  -- install/include/llvm-c/Initialization.h:40
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeIPA";

   procedure Initialize_Code_Gen (R : LLVM.Types.Pass_Registry_T)  -- install/include/llvm-c/Initialization.h:41
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeCodeGen";

   procedure Initialize_Target (R : LLVM.Types.Pass_Registry_T)  -- install/include/llvm-c/Initialization.h:42
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeTarget";

  --*
  -- * @}
  --  

end LLVM.Initialization;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
