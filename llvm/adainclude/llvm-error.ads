pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with Interfaces.C.Strings;

package LLVM.Error is

   LLVMErrorSuccess : constant := 0;  --  install/include/llvm-c/Error.h:28

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
  -- * @defgroup LLVMCError Error Handling
  -- * @ingroup LLVMC
  -- *
  -- * @{
  --  

  --*
  -- * Opaque reference to an error instance. Null serves as the 'success' value.
  --  

   type Opaque_Error_Impl_T is null record;   -- incomplete struct

   type Error_T is access all Opaque_Error_Impl_T;  -- install/include/llvm-c/Error.h:33

  --*
  -- * Error type identifier.
  --  

   type Error_Type_Id_T is new System.Address;  -- install/include/llvm-c/Error.h:38

  --*
  -- * Returns the type id for the given error instance, which must be a failure
  -- * value (i.e. non-null).
  --  

   function Get_Error_Type_Id (Err : Error_T) return Error_Type_Id_T  -- install/include/llvm-c/Error.h:44
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetErrorTypeId";

  --*
  -- * Dispose of the given error without handling it. This operation consumes the
  -- * error, and the given LLVMErrorRef value is not usable once this call returns.
  -- * Note: This method *only* needs to be called if the error is not being passed
  -- * to some other consuming operation, e.g. LLVMGetErrorMessage.
  --  

   procedure Consume_Error (Err : Error_T)  -- install/include/llvm-c/Error.h:52
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMConsumeError";

  --*
  -- * Returns the given string's error message. This operation consumes the error,
  -- * and the given LLVMErrorRef value is not usable once this call returns.
  -- * The caller is responsible for disposing of the string by calling
  -- * LLVMDisposeErrorMessage.
  --  

function Get_Error_Message
     (Err : Error_T)
      return String;

  --*
  -- * Dispose of the given error message.
  --  

procedure Dispose_Error_Message
     (Err_Msg : String);

  --*
  -- * Returns the type id for llvm StringError.
  --  

   function Get_String_Error_Type_Id return Error_Type_Id_T  -- install/include/llvm-c/Error.h:70
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetStringErrorTypeId";

  --*
  -- * Create a StringError.
  --  

function Create_String_Error
     (Err_Msg : String)
      return Error_T;

  --*
  -- * @}
  --  

end LLVM.Error;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
