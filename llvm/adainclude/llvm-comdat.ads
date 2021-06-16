pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with Interfaces.C.Strings;

package LLVM.Comdat is

  --===-- llvm-c/Comdat.h - Module Comdat C Interface -------------*- C++ -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This file defines the C interface to COMDAT.                               *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --/< The linker may choose any COMDAT.
  --/< The data referenced by the COMDAT must
  --/< be the same.
  --/< The linker will choose the largest
  --/< COMDAT.
  --/< No other Module may specify this
  --/< COMDAT.
  --/< The data referenced by the COMDAT must be
  --/< the same size.
   type Comdat_Selection_Kind_T is 
     (Any_Comdat_Selection_Kind,
      Exact_Match_Comdat_Selection_Kind,
      Largest_Comdat_Selection_Kind,
      No_Duplicates_Comdat_Selection_Kind,
      Same_Size_Comdat_Selection_Kind)
   with Convention => C;  -- llvm-12.0.0.src/include/llvm-c/Comdat.h:32

  --*
  -- * Return the Comdat in the module with the specified name. It is created
  -- * if it didn't already exist.
  -- *
  -- * @see llvm::Module::getOrInsertComdat()
  --  

function Get_Or_Insert_Comdat
     (M    : LLVM.Types.Module_T;
      Name : String)
      return LLVM.Types.Comdat_T;
   function Get_Or_Insert_Comdat_C
     (M    : LLVM.Types.Module_T;
      Name : Interfaces.C.Strings.chars_ptr)
      return LLVM.Types.Comdat_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetOrInsertComdat";

  --*
  -- * Get the Comdat assigned to the given global object.
  -- *
  -- * @see llvm::GlobalObject::getComdat()
  --  

   function Get_Comdat (V : LLVM.Types.Value_T) return LLVM.Types.Comdat_T  -- llvm-12.0.0.src/include/llvm-c/Comdat.h:47
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetComdat";

  --*
  -- * Assign the Comdat to the given global object.
  -- *
  -- * @see llvm::GlobalObject::setComdat()
  --  

   procedure Set_Comdat (V : LLVM.Types.Value_T; C : LLVM.Types.Comdat_T)  -- llvm-12.0.0.src/include/llvm-c/Comdat.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetComdat";

  -- * Get the conflict resolution selection kind for the Comdat.
  -- *
  -- * @see llvm::Comdat::getSelectionKind()
  --  

   function Get_Comdat_Selection_Kind (C : LLVM.Types.Comdat_T) return Comdat_Selection_Kind_T  -- llvm-12.0.0.src/include/llvm-c/Comdat.h:61
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetComdatSelectionKind";

  -- * Set the conflict resolution selection kind for the Comdat.
  -- *
  -- * @see llvm::Comdat::setSelectionKind()
  --  

   procedure Set_Comdat_Selection_Kind (C : LLVM.Types.Comdat_T; Kind : Comdat_Selection_Kind_T)  -- llvm-12.0.0.src/include/llvm-c/Comdat.h:68
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetComdatSelectionKind";

end LLVM.Comdat;

