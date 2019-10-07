pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with System;

package LLVM.IR_Reader is

  --===-- llvm-c/IRReader.h - IR Reader C Interface -----------------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This file defines the C interface to the IR Reader.                        *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * Read LLVM IR from a memory buffer and convert it into an in-memory Module
  -- * object. Returns 0 on success.
  -- * Optionally returns a human-readable description of any errors that
  -- * occurred during parsing IR. OutMessage must be disposed with
  -- * LLVMDisposeMessage.
  -- *
  -- * @see llvm::ParseIR()
  --  

function Parse_IR_In_Context
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address;
      Out_Message : System.Address)
      return Boolean;
   function Parse_IR_In_Context_C
     (Context_Ref : LLVM.Types.Context_T;
      Mem_Buf     : LLVM.Types.Memory_Buffer_T;
      Out_M       : System.Address;
      Out_Message : System.Address)
      return LLVM.Types.Bool_T;
   pragma Import (C, Parse_IR_In_Context_C, "LLVMParseIRInContext");

end LLVM.IR_Reader;

