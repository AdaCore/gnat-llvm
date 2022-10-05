pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);

package Clang.CX_Error_Code is

  --===-- clang-c/CXErrorCode.h - C Index Error Codes  --------------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header provides the CXErrorCode enumerators.                          *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * Error codes returned by libclang routines.
  -- *
  -- * Zero (\c CXError_Success) is the only error code indicating success.  Other
  -- * error codes, including not yet assigned non-zero values, indicate errors.
  --  

   type Error_Code_T is 
     (Error_Success,
      Error_Failure,
      Error_Crashed,
      Error_Invalid_Arguments,
      Error_AST_Read_Error)
   with Convention => C;  -- install/include/clang-c/CXErrorCode.h:28

  --*
  --   * No error.
  --    

  --*
  --   * A generic error code, no further details are available.
  --   *
  --   * Errors of this kind can get their own specific error codes in future
  --   * libclang versions.
  --    

  --*
  --   * libclang crashed while performing the requested operation.
  --    

  --*
  --   * The function detected that the arguments violate the function
  --   * contract.
  --    

  --*
  --   * An AST deserialization error has occurred.
  --    

end Clang.CX_Error_Code;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
