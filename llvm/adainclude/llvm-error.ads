pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with Interfaces.C.Strings;

package LLVM.Error is

   LLVMErrorSuccess : constant := 0;  --  llvm-10.0.0.src/include/llvm-c/Error.h:21

  --===------- llvm-c/Error.h - llvm::Error class C Interface -------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This file defines the C interface to LLVM's Error class.                   *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * Opaque reference to an error instance. Null serves as the 'success' value.
  --  

   --  skipped empty struct LLVMOpaqueError

   type Error_T is new System.Address;  -- llvm-10.0.0.src/include/llvm-c/Error.h:26

  --*
  -- * Error type identifier.
  --  

   type Error_Type_Id_T is new System.Address;  -- llvm-10.0.0.src/include/llvm-c/Error.h:31

  --*
  -- * Returns the type id for the given error instance, which must be a failure
  -- * value (i.e. non-null).
  --  

   function Get_Error_Type_Id (Err : Error_T) return Error_Type_Id_T;  -- llvm-10.0.0.src/include/llvm-c/Error.h:37
   pragma Import (C, Get_Error_Type_Id, "LLVMGetErrorTypeId");

  --*
  -- * Dispose of the given error without handling it. This operation consumes the
  -- * error, and the given LLVMErrorRef value is not usable once this call returns.
  -- * Note: This method *only* needs to be called if the error is not being passed
  -- * to some other consuming operation, e.g. LLVMGetErrorMessage.
  --  

   procedure Consume_Error (Err : Error_T);  -- llvm-10.0.0.src/include/llvm-c/Error.h:45
   pragma Import (C, Consume_Error, "LLVMConsumeError");

  --*
  -- * Returns the given string's error message. This operation consumes the error,
  -- * and the given LLVMErrorRef value is not usable once this call returns.
  -- * The caller is responsible for disposing of the string by calling
  -- * LLVMDisposeErrorMessage.
  --  

   function Get_Error_Message
     (Err : Error_T)
      return String;
   function Get_Error_Message_C
     (Err : Error_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-10.0.0.src/include/llvm-c/Error.h:53
   pragma Import (C, Get_Error_Message_C, "LLVMGetErrorMessage");

  --*
  -- * Dispose of the given error message.
  --  

   procedure Dispose_Error_Message
     (Err_Msg : String);
   procedure Dispose_Error_Message_C
     (Err_Msg : Interfaces.C.Strings.chars_ptr);  -- llvm-10.0.0.src/include/llvm-c/Error.h:58
   pragma Import (C, Dispose_Error_Message_C, "LLVMDisposeErrorMessage");

  --*
  -- * Returns the type id for llvm StringError.
  --  

   function Get_String_Error_Type_Id return Error_Type_Id_T;  -- llvm-10.0.0.src/include/llvm-c/Error.h:63
   pragma Import (C, Get_String_Error_Type_Id, "LLVMGetStringErrorTypeId");

end LLVM.Error;

