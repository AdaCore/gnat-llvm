pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with LLVM.Types;
with Interfaces.C.Strings;
with stddef_h;
with stdint_h;

package LLVM.Object is

  --===-- llvm-c/Object.h - Object Lib C Iface --------------------*- C++ -*-=== 
  --                                                                             
  -- Part of the LLVM Project, under the Apache License v2.0 with LLVM           
  -- Exceptions.                                                                 
  -- See https://llvm.org/LICENSE.txt for license information.                   
  -- SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                     
  --                                                                             
  --===----------------------------------------------------------------------=== 
  --                                                                             
  -- This header declares the C interface to libLLVMObject.a, which              
  -- implements object file reading and writing.                                 
  --                                                                             
  -- Many exotic languages can interoperate with C code but have a harder time   
  -- with C++ due to name mangling. So in addition to C, this interface enables  
  -- tools written in such languages.                                            
  --                                                                             
  --===----------------------------------------------------------------------=== 
  --*
  -- * @defgroup LLVMCObject Object file reading and writing
  -- * @ingroup LLVMC
  -- *
  -- * @{
  --  

  -- Opaque type wrappers
   --  skipped empty struct LLVMOpaqueSectionIterator

   type Section_Iterator_T is new System.Address;  -- llvm-9.0.1.src/include/llvm-c/Object.h:37

   --  skipped empty struct LLVMOpaqueSymbolIterator

   type Symbol_Iterator_T is new System.Address;  -- llvm-9.0.1.src/include/llvm-c/Object.h:38

   --  skipped empty struct LLVMOpaqueRelocationIterator

   type Relocation_Iterator_T is new System.Address;  -- llvm-9.0.1.src/include/llvm-c/Object.h:39

  --*< Archive file.  
  --*< Mach-O Universal Binary file.  
  --*< COFF Import file.  
  --*< LLVM IR.  
  --*< Windows resource (.res) file.  
  --*< COFF Object file.  
  --*< ELF 32-bit, little endian.  
  --*< ELF 32-bit, big endian.  
  --*< ELF 64-bit, little endian.  
  --*< ELF 64-bit, big endian.  
  --*< MachO 32-bit, little endian.  
  --*< MachO 32-bit, big endian.  
  --*< MachO 64-bit, little endian.  
  --*< MachO 64-bit, big endian.  
  --*< Web Assembly.  
   type Binary_Type_T is 
     (Binary_Type_Archive,
      Binary_Type_Mach_O_Universal_Binary,
      Binary_Type_COFF_Import_File,
      Binary_Type_IR,
      Binary_Type_Win_Res,
      Binary_Type_COFF,
      Binary_Type_EL_F32_L,
      Binary_Type_EL_F32_B,
      Binary_Type_EL_F64_L,
      Binary_Type_EL_F64_B,
      Binary_Type_Mach_O32_L,
      Binary_Type_Mach_O32_B,
      Binary_Type_Mach_O64_L,
      Binary_Type_Mach_O64_B,
      Binary_Type_Wasm);
   pragma Convention (C, Binary_Type_T);  -- llvm-9.0.1.src/include/llvm-c/Object.h:57

  --*
  -- * Create a binary file from the given memory buffer.
  -- *
  -- * The exact type of the binary file will be inferred automatically, and the
  -- * appropriate implementation selected.  The context may be NULL except if
  -- * the resulting file is an LLVM IR file.
  -- *
  -- * The memory buffer is not consumed by this function.  It is the responsibilty
  -- * of the caller to free it with \c LLVMDisposeMemoryBuffer.
  -- *
  -- * If NULL is returned, the \p ErrorMessage parameter is populated with the
  -- * error's description.  It is then the caller's responsibility to free this
  -- * message by calling \c LLVMDisposeMessage.
  -- *
  -- * @see llvm::object::createBinary
  --  

   function Create_Binary
     (Mem_Buf : LLVM.Types.Memory_Buffer_T;
      Context : LLVM.Types.Context_T;
      Error_Message : System.Address) return LLVM.Types.Binary_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:75
   pragma Import (C, Create_Binary, "LLVMCreateBinary");

  --*
  -- * Dispose of a binary file.
  -- *
  -- * The binary file does not own its backing buffer.  It is the responsibilty
  -- * of the caller to free it with \c LLVMDisposeMemoryBuffer.
  --  

   procedure Dispose_Binary (BR : LLVM.Types.Binary_T);  -- llvm-9.0.1.src/include/llvm-c/Object.h:85
   pragma Import (C, Dispose_Binary, "LLVMDisposeBinary");

  --*
  -- * Retrieves a copy of the memory buffer associated with this object file.
  -- *
  -- * The returned buffer is merely a shallow copy and does not own the actual
  -- * backing buffer of the binary. Nevertheless, it is the responsibility of the
  -- * caller to free it with \c LLVMDisposeMemoryBuffer.
  -- *
  -- * @see llvm::object::getMemoryBufferRef
  --  

   function Binary_Copy_Memory_Buffer (BR : LLVM.Types.Binary_T) return LLVM.Types.Memory_Buffer_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:96
   pragma Import (C, Binary_Copy_Memory_Buffer, "LLVMBinaryCopyMemoryBuffer");

  --*
  -- * Retrieve the specific type of a binary.
  -- *
  -- * @see llvm::object::Binary::getType
  --  

   function Binary_Get_Type (BR : LLVM.Types.Binary_T) return Binary_Type_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:103
   pragma Import (C, Binary_Get_Type, "LLVMBinaryGetType");

  -- * For a Mach-O universal binary file, retrieves the object file corresponding
  -- * to the given architecture if it is present as a slice.
  -- *
  -- * If NULL is returned, the \p ErrorMessage parameter is populated with the
  -- * error's description.  It is then the caller's responsibility to free this
  -- * message by calling \c LLVMDisposeMessage.
  -- *
  -- * It is the responsiblity of the caller to free the returned object file by
  -- * calling \c LLVMDisposeBinary.
  --  

function Mach_O_Universal_Binary_Copy_Object_For_Arch
     (BR            : LLVM.Types.Binary_T;
      Arch          : String;
      Arch_Len      : stddef_h.size_t;
      Error_Message : System.Address)
      return LLVM.Types.Binary_T;
   function Mach_O_Universal_Binary_Copy_Object_For_Arch_C
     (BR            : LLVM.Types.Binary_T;
      Arch          : Interfaces.C.Strings.chars_ptr;
      Arch_Len      : stddef_h.size_t;
      Error_Message : System.Address)
      return LLVM.Types.Binary_T;
   pragma Import (C, Mach_O_Universal_Binary_Copy_Object_For_Arch_C, "LLVMMachOUniversalBinaryCopyObjectForArch");

  --*
  -- * Retrieve a copy of the section iterator for this object file.
  -- *
  -- * If there are no sections, the result is NULL.
  -- *
  -- * The returned iterator is merely a shallow copy. Nevertheless, it is
  -- * the responsibility of the caller to free it with
  -- * \c LLVMDisposeSectionIterator.
  -- *
  -- * @see llvm::object::sections()
  --  

   function Object_File_Copy_Section_Iterator (BR : LLVM.Types.Binary_T) return Section_Iterator_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:132
   pragma Import (C, Object_File_Copy_Section_Iterator, "LLVMObjectFileCopySectionIterator");

  --*
  -- * Returns whether the given section iterator is at the end.
  -- *
  -- * @see llvm::object::section_end
  --  

   function Object_File_Is_Section_Iterator_At_End
     (BR : LLVM.Types.Binary_T;
      SI : Section_Iterator_T)
      return Boolean;
   function Object_File_Is_Section_Iterator_At_End_C
     (BR : LLVM.Types.Binary_T;
      SI : Section_Iterator_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:139
   pragma Import (C, Object_File_Is_Section_Iterator_At_End_C, "LLVMObjectFileIsSectionIteratorAtEnd");

  --*
  -- * Retrieve a copy of the symbol iterator for this object file.
  -- *
  -- * If there are no symbols, the result is NULL.
  -- *
  -- * The returned iterator is merely a shallow copy. Nevertheless, it is
  -- * the responsibility of the caller to free it with
  -- * \c LLVMDisposeSymbolIterator.
  -- *
  -- * @see llvm::object::symbols()
  --  

   function Object_File_Copy_Symbol_Iterator (BR : LLVM.Types.Binary_T) return Symbol_Iterator_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:153
   pragma Import (C, Object_File_Copy_Symbol_Iterator, "LLVMObjectFileCopySymbolIterator");

  --*
  -- * Returns whether the given symbol iterator is at the end.
  -- *
  -- * @see llvm::object::symbol_end
  --  

   function Object_File_Is_Symbol_Iterator_At_End
     (BR : LLVM.Types.Binary_T;
      SI : Symbol_Iterator_T)
      return Boolean;
   function Object_File_Is_Symbol_Iterator_At_End_C
     (BR : LLVM.Types.Binary_T;
      SI : Symbol_Iterator_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:160
   pragma Import (C, Object_File_Is_Symbol_Iterator_At_End_C, "LLVMObjectFileIsSymbolIteratorAtEnd");

   procedure Dispose_Section_Iterator (SI : Section_Iterator_T);  -- llvm-9.0.1.src/include/llvm-c/Object.h:163
   pragma Import (C, Dispose_Section_Iterator, "LLVMDisposeSectionIterator");

   procedure Move_To_Next_Section (SI : Section_Iterator_T);  -- llvm-9.0.1.src/include/llvm-c/Object.h:165
   pragma Import (C, Move_To_Next_Section, "LLVMMoveToNextSection");

   procedure Move_To_Containing_Section (Sect : Section_Iterator_T; Sym : Symbol_Iterator_T);  -- llvm-9.0.1.src/include/llvm-c/Object.h:166
   pragma Import (C, Move_To_Containing_Section, "LLVMMoveToContainingSection");

  -- ObjectFile Symbol iterators
   procedure Dispose_Symbol_Iterator (SI : Symbol_Iterator_T);  -- llvm-9.0.1.src/include/llvm-c/Object.h:170
   pragma Import (C, Dispose_Symbol_Iterator, "LLVMDisposeSymbolIterator");

   procedure Move_To_Next_Symbol (SI : Symbol_Iterator_T);  -- llvm-9.0.1.src/include/llvm-c/Object.h:171
   pragma Import (C, Move_To_Next_Symbol, "LLVMMoveToNextSymbol");

  -- SectionRef accessors
   function Get_Section_Name
     (SI : Section_Iterator_T)
      return String;
   function Get_Section_Name_C
     (SI : Section_Iterator_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.1.src/include/llvm-c/Object.h:174
   pragma Import (C, Get_Section_Name_C, "LLVMGetSectionName");

   function Get_Section_Size (SI : Section_Iterator_T) return stdint_h.uint64_t;  -- llvm-9.0.1.src/include/llvm-c/Object.h:175
   pragma Import (C, Get_Section_Size, "LLVMGetSectionSize");

   function Get_Section_Contents
     (SI : Section_Iterator_T)
      return String;
   function Get_Section_Contents_C
     (SI : Section_Iterator_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.1.src/include/llvm-c/Object.h:176
   pragma Import (C, Get_Section_Contents_C, "LLVMGetSectionContents");

   function Get_Section_Address (SI : Section_Iterator_T) return stdint_h.uint64_t;  -- llvm-9.0.1.src/include/llvm-c/Object.h:177
   pragma Import (C, Get_Section_Address, "LLVMGetSectionAddress");

   function Get_Section_Contains_Symbol
     (SI  : Section_Iterator_T;
      Sym : Symbol_Iterator_T)
      return Boolean;
   function Get_Section_Contains_Symbol_C
     (SI  : Section_Iterator_T;
      Sym : Symbol_Iterator_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:178
   pragma Import (C, Get_Section_Contains_Symbol_C, "LLVMGetSectionContainsSymbol");

  -- Section Relocation iterators
   function Get_Relocations (Section : Section_Iterator_T) return Relocation_Iterator_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:182
   pragma Import (C, Get_Relocations, "LLVMGetRelocations");

   procedure Dispose_Relocation_Iterator (RI : Relocation_Iterator_T);  -- llvm-9.0.1.src/include/llvm-c/Object.h:183
   pragma Import (C, Dispose_Relocation_Iterator, "LLVMDisposeRelocationIterator");

   function Is_Relocation_Iterator_At_End
     (Section : Section_Iterator_T;
      RI      : Relocation_Iterator_T)
      return Boolean;
   function Is_Relocation_Iterator_At_End_C
     (Section : Section_Iterator_T;
      RI      : Relocation_Iterator_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:184
   pragma Import (C, Is_Relocation_Iterator_At_End_C, "LLVMIsRelocationIteratorAtEnd");

   procedure Move_To_Next_Relocation (RI : Relocation_Iterator_T);  -- llvm-9.0.1.src/include/llvm-c/Object.h:186
   pragma Import (C, Move_To_Next_Relocation, "LLVMMoveToNextRelocation");

  -- SymbolRef accessors
   function Get_Symbol_Name
     (SI : Symbol_Iterator_T)
      return String;
   function Get_Symbol_Name_C
     (SI : Symbol_Iterator_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.1.src/include/llvm-c/Object.h:190
   pragma Import (C, Get_Symbol_Name_C, "LLVMGetSymbolName");

   function Get_Symbol_Address (SI : Symbol_Iterator_T) return stdint_h.uint64_t;  -- llvm-9.0.1.src/include/llvm-c/Object.h:191
   pragma Import (C, Get_Symbol_Address, "LLVMGetSymbolAddress");

   function Get_Symbol_Size (SI : Symbol_Iterator_T) return stdint_h.uint64_t;  -- llvm-9.0.1.src/include/llvm-c/Object.h:192
   pragma Import (C, Get_Symbol_Size, "LLVMGetSymbolSize");

  -- RelocationRef accessors
   function Get_Relocation_Offset (RI : Relocation_Iterator_T) return stdint_h.uint64_t;  -- llvm-9.0.1.src/include/llvm-c/Object.h:195
   pragma Import (C, Get_Relocation_Offset, "LLVMGetRelocationOffset");

   function Get_Relocation_Symbol (RI : Relocation_Iterator_T) return Symbol_Iterator_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:196
   pragma Import (C, Get_Relocation_Symbol, "LLVMGetRelocationSymbol");

   function Get_Relocation_Type (RI : Relocation_Iterator_T) return stdint_h.uint64_t;  -- llvm-9.0.1.src/include/llvm-c/Object.h:197
   pragma Import (C, Get_Relocation_Type, "LLVMGetRelocationType");

  -- NOTE: Caller takes ownership of returned string of the two
  -- following functions.
   function Get_Relocation_Type_Name
     (RI : Relocation_Iterator_T)
      return String;
   function Get_Relocation_Type_Name_C
     (RI : Relocation_Iterator_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.1.src/include/llvm-c/Object.h:200
   pragma Import (C, Get_Relocation_Type_Name_C, "LLVMGetRelocationTypeName");

   function Get_Relocation_Value_String
     (RI : Relocation_Iterator_T)
      return String;
   function Get_Relocation_Value_String_C
     (RI : Relocation_Iterator_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-9.0.1.src/include/llvm-c/Object.h:201
   pragma Import (C, Get_Relocation_Value_String_C, "LLVMGetRelocationValueString");

  --* Deprecated: Use LLVMBinaryRef instead.  
   --  skipped empty struct LLVMOpaqueObjectFile

   type Object_File_T is new System.Address;  -- llvm-9.0.1.src/include/llvm-c/Object.h:204

  --* Deprecated: Use LLVMCreateBinary instead.  
   function Create_Object_File (Mem_Buf : LLVM.Types.Memory_Buffer_T) return Object_File_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:207
   pragma Import (C, Create_Object_File, "LLVMCreateObjectFile");

  --* Deprecated: Use LLVMDisposeBinary instead.  
   procedure Dispose_Object_File (Object_File : Object_File_T);  -- llvm-9.0.1.src/include/llvm-c/Object.h:210
   pragma Import (C, Dispose_Object_File, "LLVMDisposeObjectFile");

  --* Deprecated: Use LLVMObjectFileCopySectionIterator instead.  
   function Get_Sections (Object_File : Object_File_T) return Section_Iterator_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:213
   pragma Import (C, Get_Sections, "LLVMGetSections");

  --* Deprecated: Use LLVMObjectFileIsSectionIteratorAtEnd instead.  
   function Is_Section_Iterator_At_End
     (Object_File : Object_File_T;
      SI          : Section_Iterator_T)
      return Boolean;
   function Is_Section_Iterator_At_End_C
     (Object_File : Object_File_T;
      SI          : Section_Iterator_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:216
   pragma Import (C, Is_Section_Iterator_At_End_C, "LLVMIsSectionIteratorAtEnd");

  --* Deprecated: Use LLVMObjectFileCopySymbolIterator instead.  
   function Get_Symbols (Object_File : Object_File_T) return Symbol_Iterator_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:220
   pragma Import (C, Get_Symbols, "LLVMGetSymbols");

  --* Deprecated: Use LLVMObjectFileIsSymbolIteratorAtEnd instead.  
   function Is_Symbol_Iterator_At_End
     (Object_File : Object_File_T;
      SI          : Symbol_Iterator_T)
      return Boolean;
   function Is_Symbol_Iterator_At_End_C
     (Object_File : Object_File_T;
      SI          : Symbol_Iterator_T)
      return LLVM.Types.Bool_T;  -- llvm-9.0.1.src/include/llvm-c/Object.h:223
   pragma Import (C, Is_Symbol_Iterator_At_End_C, "LLVMIsSymbolIteratorAtEnd");

  --*
  -- * @}
  --  

end LLVM.Object;

