pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with System;

package LLVM.Bit_Reader is

  --===-- llvm-c/BitReader.h - BitReader Library C Interface ------*- C++ -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to libLLVMBitReader.a, which          *|
  --|* implements input of the LLVM bitcode format.                               *|
  --|*                                                                            *|
  --|* Many exotic languages can interoperate with C code but have a harder time  *|
  --|* with C++ due to name mangling. So in addition to C, this interface enables *|
  --|* tools written in such languages.                                           *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCBitReader Bit Reader
  -- * @ingroup LLVMC
  -- *
  -- * @{
  --  

  -- Builds a module from the bitcode in the specified memory buffer, returning a
  --   reference to the module via the OutModule parameter. Returns 0 on success.
  --   Optionally returns a human-readable error message via OutMessage.
  --   This is deprecated. Use LLVMParseBitcode2.  

function Parse_Bitcode
     (Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_Module  : System.Address;
      Out_Message : System.Address)
      return Boolean;

  -- Builds a module from the bitcode in the specified memory buffer, returning a
  --   reference to the module via the OutModule parameter. Returns 0 on success.  

function Parse_Bitcode_2
     (Mem_Buf    : LLVM.Types.Memory_Buffer_T;
      Out_Module : System.Address)
      return Boolean;

  -- This is deprecated. Use LLVMParseBitcodeInContext2.  
function Parse_Bitcode_In_Context
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_Module  : System.Address;
      Out_Message : System.Address)
      return Boolean;

function Parse_Bitcode_In_Context_2
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_Module  : System.Address)
      return Boolean;

  --* Reads a module from the specified path, returning via the OutMP parameter
  --    a module provider which performs lazy deserialization. Returns 0 on success.
  --    Optionally returns a human-readable error message via OutMessage.
  --    This is deprecated. Use LLVMGetBitcodeModuleInContext2.  

function Get_Bitcode_Module_In_Context
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address;
      Out_Message : System.Address)
      return Boolean;

  --* Reads a module from the given memory buffer, returning via the OutMP
  -- * parameter a module provider which performs lazy deserialization.
  -- *
  -- * Returns 0 on success.
  -- *
  -- * Takes ownership of \p MemBuf if (and only if) the module was read
  -- * successfully.  

function Get_Bitcode_Module_In_Context_2
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address)
      return Boolean;

  -- This is deprecated. Use LLVMGetBitcodeModule2.  
function Get_Bitcode_Module
     (Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address;
      Out_Message : System.Address)
      return Boolean;

function Get_Bitcode_Module_2
     (Mem_Buf : LLVM.Types.Memory_Buffer_T;
      Out_M   : System.Address)
      return Boolean;

  --*
  -- * @}
  --  

end LLVM.Bit_Reader;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
