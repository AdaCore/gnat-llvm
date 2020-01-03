pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with Interfaces.C.Strings;

package LLVM.Link_Time_Optimizer is

  --===-- llvm/LinkTimeOptimizer.h - LTO Public C Interface -------*- C++ -*-===//
  -- Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
  -- See https://llvm.org/LICENSE.txt for license information.
  -- SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
  --===----------------------------------------------------------------------===//
  -- This header provides a C API to use the LLVM link time optimization
  -- library. This is intended to be used by linkers which are C-only in
  -- their implementation for performing LTO.
  --===----------------------------------------------------------------------===//
  --*
  -- * @defgroup LLVMCLinkTimeOptimizer Link Time Optimization
  -- * @ingroup LLVMC
  -- *
  -- * @{
  --  

  --/ This provides a dummy type for pointers to the LTO object.
   type Lto_T_T is new System.Address;  -- llvm-9.0.1.src/include/llvm-c/LinkTimeOptimizer.h:30

  --/ This provides a C-visible enumerator to manage status codes.
  --/ This should map exactly onto the C++ enumerator LTOStatus.
   type Lto_Status_T is 
     (LTO_UNKNOWN,
      LTO_OPT_SUCCESS,
      LTO_READ_SUCCESS,
      LTO_READ_FAILURE,
      LTO_WRITE_FAILURE,
      LTO_NO_TARGET,
      LTO_NO_WORK,
      LTO_MODULE_MERGE_FAILURE,
      LTO_ASM_FAILURE,
      LTO_NULL_OBJECT);
   pragma Convention (C, Lto_Status_T);  -- llvm-9.0.1.src/include/llvm-c/LinkTimeOptimizer.h:34

  --  Added C-specific error codes
   subtype Lto_Status_T_T is Lto_Status_T;  -- llvm-9.0.1.src/include/llvm-c/LinkTimeOptimizer.h:47

  --/ This provides C interface to initialize link time optimizer. This allows
  --/ linker to use dlopen() interface to dynamically load LinkTimeOptimizer.
  --/ extern "C" helps, because dlopen() interface uses name to find the symbol.
   function Create_Optimizer return Lto_T_T;  -- llvm-9.0.1.src/include/llvm-c/LinkTimeOptimizer.h:52
   pragma Import (C, Create_Optimizer, "llvm_create_optimizer");

   procedure Destroy_Optimizer (lto : Lto_T_T);  -- llvm-9.0.1.src/include/llvm-c/LinkTimeOptimizer.h:53
   pragma Import (C, Destroy_Optimizer, "llvm_destroy_optimizer");

   function Read_Object_File
     (lto            : Lto_T_T;
      Input_Filename : String)
      return Lto_Status_T_T;
   function Read_Object_File_C
     (lto            : Lto_T_T;
      Input_Filename : Interfaces.C.Strings.chars_ptr)
      return Lto_Status_T_T;  -- llvm-9.0.1.src/include/llvm-c/LinkTimeOptimizer.h:55
   pragma Import (C, Read_Object_File_C, "llvm_read_object_file");

   function Optimize_Modules
     (lto             : Lto_T_T;
      Output_Filename : String)
      return Lto_Status_T_T;
   function Optimize_Modules_C
     (lto             : Lto_T_T;
      Output_Filename : Interfaces.C.Strings.chars_ptr)
      return Lto_Status_T_T;  -- llvm-9.0.1.src/include/llvm-c/LinkTimeOptimizer.h:57
   pragma Import (C, Optimize_Modules_C, "llvm_optimize_modules");

  --*
  -- * @}
  --  

end LLVM.Link_Time_Optimizer;

