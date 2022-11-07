pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with System;
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
   type Opaque_Section_Iterator_Impl_T is null record;   -- incomplete struct

   type Section_Iterator_T is access all Opaque_Section_Iterator_Impl_T;  -- install/include/llvm-c/Object.h:36

   type Opaque_Symbol_Iterator_Impl_T is null record;   -- incomplete struct

   type Symbol_Iterator_T is access all Opaque_Symbol_Iterator_Impl_T;  -- install/include/llvm-c/Object.h:37

   type Opaque_Relocation_Iterator_Impl_T is null record;   -- incomplete struct

   type Relocation_Iterator_T is access all Opaque_Relocation_Iterator_Impl_T;  -- install/include/llvm-c/Object.h:38

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
  --*< Offloading fatbinary.  
   type Binary_Type_T is 
     (Binary_Type_Archive,
      Binary_Type_Mach_O_Universal_Binary,
      Binary_Type_COFF_Import_File,
      Binary_Type_IR,
      Binary_Type_Win_Res,
      Binary_Type_COFF,
      Binary_Type_ELF32L,
      Binary_Type_ELF32B,
      Binary_Type_ELF64L,
      Binary_Type_ELF64B,
      Binary_Type_Mach_O32L,
      Binary_Type_Mach_O32B,
      Binary_Type_Mach_O64L,
      Binary_Type_Mach_O64B,
      Binary_Type_Wasm,
      Binary_Type_Offload)
   with Convention => C;  -- install/include/llvm-c/Object.h:58

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
      Error_Message : System.Address) return LLVM.Types.Binary_T  -- install/include/llvm-c/Object.h:76
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateBinary";

  --*
  -- * Dispose of a binary file.
  -- *
  -- * The binary file does not own its backing buffer.  It is the responsibilty
  -- * of the caller to free it with \c LLVMDisposeMemoryBuffer.
  --  

   procedure Dispose_Binary (BR : LLVM.Types.Binary_T)  -- install/include/llvm-c/Object.h:86
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeBinary";

  --*
  -- * Retrieves a copy of the memory buffer associated with this object file.
  -- *
  -- * The returned buffer is merely a shallow copy and does not own the actual
  -- * backing buffer of the binary. Nevertheless, it is the responsibility of the
  -- * caller to free it with \c LLVMDisposeMemoryBuffer.
  -- *
  -- * @see llvm::object::getMemoryBufferRef
  --  

   function Binary_Copy_Memory_Buffer (BR : LLVM.Types.Binary_T) return LLVM.Types.Memory_Buffer_T  -- install/include/llvm-c/Object.h:97
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBinaryCopyMemoryBuffer";

  --*
  -- * Retrieve the specific type of a binary.
  -- *
  -- * @see llvm::object::Binary::getType
  --  

   function Binary_Get_Type (BR : LLVM.Types.Binary_T) return Binary_Type_T  -- install/include/llvm-c/Object.h:104
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMBinaryGetType";

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

   function Object_File_Copy_Section_Iterator (BR : LLVM.Types.Binary_T) return Section_Iterator_T  -- install/include/llvm-c/Object.h:133
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMObjectFileCopySectionIterator";

  --*
  -- * Returns whether the given section iterator is at the end.
  -- *
  -- * @see llvm::object::section_end
  --  

function Object_File_Is_Section_Iterator_At_End
     (BR : LLVM.Types.Binary_T;
      SI : Section_Iterator_T)
      return Boolean;

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

   function Object_File_Copy_Symbol_Iterator (BR : LLVM.Types.Binary_T) return Symbol_Iterator_T  -- install/include/llvm-c/Object.h:154
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMObjectFileCopySymbolIterator";

  --*
  -- * Returns whether the given symbol iterator is at the end.
  -- *
  -- * @see llvm::object::symbol_end
  --  

function Object_File_Is_Symbol_Iterator_At_End
     (BR : LLVM.Types.Binary_T;
      SI : Symbol_Iterator_T)
      return Boolean;

   procedure Dispose_Section_Iterator (SI : Section_Iterator_T)  -- install/include/llvm-c/Object.h:164
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeSectionIterator";

   procedure Move_To_Next_Section (SI : Section_Iterator_T)  -- install/include/llvm-c/Object.h:166
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMMoveToNextSection";

   procedure Move_To_Containing_Section (Sect : Section_Iterator_T; Sym : Symbol_Iterator_T)  -- install/include/llvm-c/Object.h:167
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMMoveToContainingSection";

  -- ObjectFile Symbol iterators
   procedure Dispose_Symbol_Iterator (SI : Symbol_Iterator_T)  -- install/include/llvm-c/Object.h:171
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeSymbolIterator";

   procedure Move_To_Next_Symbol (SI : Symbol_Iterator_T)  -- install/include/llvm-c/Object.h:172
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMMoveToNextSymbol";

  -- SectionRef accessors
function Get_Section_Name
     (SI : Section_Iterator_T)
      return String;

   function Get_Section_Size (SI : Section_Iterator_T) return stdint_h.uint64_t  -- install/include/llvm-c/Object.h:176
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetSectionSize";

function Get_Section_Contents
     (SI : Section_Iterator_T)
      return String;

   function Get_Section_Address (SI : Section_Iterator_T) return stdint_h.uint64_t  -- install/include/llvm-c/Object.h:178
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetSectionAddress";

function Get_Section_Contains_Symbol
     (SI  : Section_Iterator_T;
      Sym : Symbol_Iterator_T)
      return Boolean;

  -- Section Relocation iterators
   function Get_Relocations (Section : Section_Iterator_T) return Relocation_Iterator_T  -- install/include/llvm-c/Object.h:183
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetRelocations";

   procedure Dispose_Relocation_Iterator (RI : Relocation_Iterator_T)  -- install/include/llvm-c/Object.h:184
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeRelocationIterator";

function Is_Relocation_Iterator_At_End
     (Section : Section_Iterator_T;
      RI      : Relocation_Iterator_T)
      return Boolean;

   procedure Move_To_Next_Relocation (RI : Relocation_Iterator_T)  -- install/include/llvm-c/Object.h:187
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMMoveToNextRelocation";

  -- SymbolRef accessors
function Get_Symbol_Name
     (SI : Symbol_Iterator_T)
      return String;

   function Get_Symbol_Address (SI : Symbol_Iterator_T) return stdint_h.uint64_t  -- install/include/llvm-c/Object.h:192
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetSymbolAddress";

   function Get_Symbol_Size (SI : Symbol_Iterator_T) return stdint_h.uint64_t  -- install/include/llvm-c/Object.h:193
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetSymbolSize";

  -- RelocationRef accessors
   function Get_Relocation_Offset (RI : Relocation_Iterator_T) return stdint_h.uint64_t  -- install/include/llvm-c/Object.h:196
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetRelocationOffset";

   function Get_Relocation_Symbol (RI : Relocation_Iterator_T) return Symbol_Iterator_T  -- install/include/llvm-c/Object.h:197
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetRelocationSymbol";

   function Get_Relocation_Type (RI : Relocation_Iterator_T) return stdint_h.uint64_t  -- install/include/llvm-c/Object.h:198
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetRelocationType";

  -- NOTE: Caller takes ownership of returned string of the two
  -- following functions.
function Get_Relocation_Type_Name
     (RI : Relocation_Iterator_T)
      return String;

function Get_Relocation_Value_String
     (RI : Relocation_Iterator_T)
      return String;

  --* Deprecated: Use LLVMBinaryRef instead.  
   type Opaque_Object_File_Impl_T is null record;   -- incomplete struct

   type Object_File_T is access all Opaque_Object_File_Impl_T;  -- install/include/llvm-c/Object.h:205

  --* Deprecated: Use LLVMCreateBinary instead.  
   function Create_Object_File (Mem_Buf : LLVM.Types.Memory_Buffer_T) return Object_File_T  -- install/include/llvm-c/Object.h:208
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateObjectFile";

  --* Deprecated: Use LLVMDisposeBinary instead.  
   procedure Dispose_Object_File (Object_File : Object_File_T)  -- install/include/llvm-c/Object.h:211
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeObjectFile";

  --* Deprecated: Use LLVMObjectFileCopySectionIterator instead.  
   function Get_Sections (Object_File : Object_File_T) return Section_Iterator_T  -- install/include/llvm-c/Object.h:214
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetSections";

  --* Deprecated: Use LLVMObjectFileIsSectionIteratorAtEnd instead.  
function Is_Section_Iterator_At_End
     (Object_File : Object_File_T;
      SI          : Section_Iterator_T)
      return Boolean;

  --* Deprecated: Use LLVMObjectFileCopySymbolIterator instead.  
   function Get_Symbols (Object_File : Object_File_T) return Symbol_Iterator_T  -- install/include/llvm-c/Object.h:221
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetSymbols";

  --* Deprecated: Use LLVMObjectFileIsSymbolIteratorAtEnd instead.  
function Is_Symbol_Iterator_At_End
     (Object_File : Object_File_T;
      SI          : Symbol_Iterator_T)
      return Boolean;

  --*
  -- * @}
  --  

end LLVM.Object;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
