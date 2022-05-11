pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);

package LLVM.Types is

  --===-- llvm-c/Support.h - C Interface Types declarations ---------*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This file defines types used by the C interface to LLVM.                   *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * @defgroup LLVMCSupportTypes Types and Enumerations
  -- *
  -- * @{
  --  

   subtype Bool_T is int;  -- install/include/llvm-c/Types.h:28

  -- Opaque types.  
  --*
  -- * LLVM uses a polymorphic type hierarchy which C cannot represent, therefore
  -- * parameters must be passed as base types. Despite the declared types, most
  -- * of the functions provided operate only on branches of the type hierarchy.
  -- * The declared parameter names are descriptive and specify which type is
  -- * required. Additionally, each type hierarchy is documented along with the
  -- * functions that operate upon it. For more detail, refer to LLVM's C++ code.
  -- * If in doubt, refer to Core.cpp, which performs parameter downcasts in the
  -- * form unwrap<RequiredType>(Param).
  --  

  --*
  -- * Used to pass regions of memory through LLVM interfaces.
  -- *
  -- * @see llvm::MemoryBuffer
  --  

   type Opaque_Memory_Buffer_Impl_T is null record;   -- incomplete struct

   type Memory_Buffer_T is access all Opaque_Memory_Buffer_Impl_T;  -- install/include/llvm-c/Types.h:48

  --*
  -- * The top-level container for all LLVM global data. See the LLVMContext class.
  --  

   type Opaque_Context_Impl_T is null record;   -- incomplete struct

   type Context_T is access all Opaque_Context_Impl_T;  -- install/include/llvm-c/Types.h:53

  --*
  -- * The top-level container for all other LLVM Intermediate Representation (IR)
  -- * objects.
  -- *
  -- * @see llvm::Module
  --  

   type Opaque_Module_Impl_T is null record;   -- incomplete struct

   type Module_T is access all Opaque_Module_Impl_T;  -- install/include/llvm-c/Types.h:61

  --*
  -- * Each value in the LLVM IR has a type, an LLVMTypeRef.
  -- *
  -- * @see llvm::Type
  --  

   type Opaque_Type_Impl_T is null record;   -- incomplete struct

   type Type_T is access all Opaque_Type_Impl_T;  -- install/include/llvm-c/Types.h:68

  --*
  -- * Represents an individual value in LLVM IR.
  -- *
  -- * This models llvm::Value.
  --  

   type Opaque_Value_Impl_T is null record;   -- incomplete struct

   type Value_T is access all Opaque_Value_Impl_T;  -- install/include/llvm-c/Types.h:75

  --*
  -- * Represents a basic block of instructions in LLVM IR.
  -- *
  -- * This models llvm::BasicBlock.
  --  

   type Opaque_Basic_Block_Impl_T is null record;   -- incomplete struct

   type Basic_Block_T is access all Opaque_Basic_Block_Impl_T;  -- install/include/llvm-c/Types.h:82

  --*
  -- * Represents an LLVM Metadata.
  -- *
  -- * This models llvm::Metadata.
  --  

   type Opaque_Metadata_Impl_T is null record;   -- incomplete struct

   type Metadata_T is access all Opaque_Metadata_Impl_T;  -- install/include/llvm-c/Types.h:89

  --*
  -- * Represents an LLVM Named Metadata Node.
  -- *
  -- * This models llvm::NamedMDNode.
  --  

   type Opaque_Named_MD_Node_Impl_T is null record;   -- incomplete struct

   type Named_MD_Node_T is access all Opaque_Named_MD_Node_Impl_T;  -- install/include/llvm-c/Types.h:96

  --*
  -- * Represents an entry in a Global Object's metadata attachments.
  -- *
  -- * This models std::pair<unsigned, MDNode *>
  --  

   type Opaque_Value_Metadata_Entry_Impl_T is null record;   -- incomplete struct

   subtype Value_Metadata_Entry_T is Opaque_Value_Metadata_Entry_Impl_T;  -- install/include/llvm-c/Types.h:103

  --*
  -- * Represents an LLVM basic block builder.
  -- *
  -- * This models llvm::IRBuilder.
  --  

   type Opaque_Builder_Impl_T is null record;   -- incomplete struct

   type Builder_T is access all Opaque_Builder_Impl_T;  -- install/include/llvm-c/Types.h:110

  --*
  -- * Represents an LLVM debug info builder.
  -- *
  -- * This models llvm::DIBuilder.
  --  

   type Opaque_DI_Builder_Impl_T is null record;   -- incomplete struct

   type DI_Builder_T is access all Opaque_DI_Builder_Impl_T;  -- install/include/llvm-c/Types.h:117

  --*
  -- * Interface used to provide a module to JIT or interpreter.
  -- * This is now just a synonym for llvm::Module, but we have to keep using the
  -- * different type to keep binary compatibility.
  --  

   type Opaque_Module_Provider_Impl_T is null record;   -- incomplete struct

   type Module_Provider_T is access all Opaque_Module_Provider_Impl_T;  -- install/include/llvm-c/Types.h:124

  --* @see llvm::PassManagerBase  
   type Opaque_Pass_Manager_Impl_T is null record;   -- incomplete struct

   type Pass_Manager_T is access all Opaque_Pass_Manager_Impl_T;  -- install/include/llvm-c/Types.h:127

  --* @see llvm::PassRegistry  
   type Opaque_Pass_Registry_Impl_T is null record;   -- incomplete struct

   type Pass_Registry_T is access all Opaque_Pass_Registry_Impl_T;  -- install/include/llvm-c/Types.h:130

  --*
  -- * Used to get the users and usees of a Value.
  -- *
  -- * @see llvm::Use  

   type Opaque_Use_Impl_T is null record;   -- incomplete struct

   type Use_T is access all Opaque_Use_Impl_T;  -- install/include/llvm-c/Types.h:136

  --*
  -- * Used to represent an attributes.
  -- *
  -- * @see llvm::Attribute
  --  

   type Opaque_Attribute_Ref_Impl_T is null record;   -- incomplete struct

   type Attribute_T is access all Opaque_Attribute_Ref_Impl_T;  -- install/include/llvm-c/Types.h:143

  --*
  -- * @see llvm::DiagnosticInfo
  --  

   type Opaque_Diagnostic_Info_Impl_T is null record;   -- incomplete struct

   type Diagnostic_Info_T is access all Opaque_Diagnostic_Info_Impl_T;  -- install/include/llvm-c/Types.h:148

  --*
  -- * @see llvm::Comdat
  --  

   type Comdat_Impl_T is null record;   -- incomplete struct

   type Comdat_T is access all Comdat_Impl_T;  -- install/include/llvm-c/Types.h:153

  --*
  -- * @see llvm::Module::ModuleFlagEntry
  --  

   type Opaque_Module_Flag_Entry_Impl_T is null record;   -- incomplete struct

   subtype Module_Flag_Entry_T is Opaque_Module_Flag_Entry_Impl_T;  -- install/include/llvm-c/Types.h:158

  --*
  -- * @see llvm::JITEventListener
  --  

   type Opaque_JIT_Event_Listener_Impl_T is null record;   -- incomplete struct

   type JIT_Event_Listener_T is access all Opaque_JIT_Event_Listener_Impl_T;  -- install/include/llvm-c/Types.h:163

  --*
  -- * @see llvm::object::Binary
  --  

   type Opaque_Binary_Impl_T is null record;   -- incomplete struct

   type Binary_T is access all Opaque_Binary_Impl_T;  -- install/include/llvm-c/Types.h:168

  --*
  -- * @}
  --  

end LLVM.Types;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
