pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with LLVM.Types;
with Interfaces.C.Strings;
with stdint_h;

package LLVM.Object is

   --  skipped empty struct LLVMOpaqueObjectFile

   type Object_File_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:37

   --  skipped empty struct LLVMOpaqueSectionIterator

   type Section_Iterator_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:38

   --  skipped empty struct LLVMOpaqueSymbolIterator

   type Symbol_Iterator_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:39

   --  skipped empty struct LLVMOpaqueRelocationIterator

   type Relocation_Iterator_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:40

   function Create_Object_File (Mem_Buf : LLVM.Types.Memory_Buffer_T) return Object_File_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:43
   pragma Import (C, Create_Object_File, "LLVMCreateObjectFile");

   procedure Dispose_Object_File (Object_File : Object_File_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:44
   pragma Import (C, Dispose_Object_File, "LLVMDisposeObjectFile");

   function Get_Sections (Object_File : Object_File_T) return Section_Iterator_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:47
   pragma Import (C, Get_Sections, "LLVMGetSections");

   procedure Dispose_Section_Iterator (SI : Section_Iterator_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:48
   pragma Import (C, Dispose_Section_Iterator, "LLVMDisposeSectionIterator");

   function Is_Section_Iterator_At_End
     (Object_File : Object_File_T;
      SI          : Section_Iterator_T)
      return Boolean;
   function Is_Section_Iterator_At_End_C
     (Object_File : Object_File_T;
      SI          : Section_Iterator_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:49
   pragma Import (C, Is_Section_Iterator_At_End_C, "LLVMIsSectionIteratorAtEnd");

   procedure Move_To_Next_Section (SI : Section_Iterator_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:51
   pragma Import (C, Move_To_Next_Section, "LLVMMoveToNextSection");

   procedure Move_To_Containing_Section (Sect : Section_Iterator_T; Sym : Symbol_Iterator_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:52
   pragma Import (C, Move_To_Containing_Section, "LLVMMoveToContainingSection");

   function Get_Symbols (Object_File : Object_File_T) return Symbol_Iterator_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:56
   pragma Import (C, Get_Symbols, "LLVMGetSymbols");

   procedure Dispose_Symbol_Iterator (SI : Symbol_Iterator_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:57
   pragma Import (C, Dispose_Symbol_Iterator, "LLVMDisposeSymbolIterator");

   function Is_Symbol_Iterator_At_End
     (Object_File : Object_File_T;
      SI          : Symbol_Iterator_T)
      return Boolean;
   function Is_Symbol_Iterator_At_End_C
     (Object_File : Object_File_T;
      SI          : Symbol_Iterator_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:58
   pragma Import (C, Is_Symbol_Iterator_At_End_C, "LLVMIsSymbolIteratorAtEnd");

   procedure Move_To_Next_Symbol (SI : Symbol_Iterator_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:60
   pragma Import (C, Move_To_Next_Symbol, "LLVMMoveToNextSymbol");

   function Get_Section_Name
     (SI : Section_Iterator_T)
      return String;
   function Get_Section_Name_C
     (SI : Section_Iterator_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:63
   pragma Import (C, Get_Section_Name_C, "LLVMGetSectionName");

   function Get_Section_Size (SI : Section_Iterator_T) return stdint_h.uint64_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:64
   pragma Import (C, Get_Section_Size, "LLVMGetSectionSize");

   function Get_Section_Contents
     (SI : Section_Iterator_T)
      return String;
   function Get_Section_Contents_C
     (SI : Section_Iterator_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:65
   pragma Import (C, Get_Section_Contents_C, "LLVMGetSectionContents");

   function Get_Section_Address (SI : Section_Iterator_T) return stdint_h.uint64_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:66
   pragma Import (C, Get_Section_Address, "LLVMGetSectionAddress");

   function Get_Section_Contains_Symbol
     (SI  : Section_Iterator_T;
      Sym : Symbol_Iterator_T)
      return Boolean;
   function Get_Section_Contains_Symbol_C
     (SI  : Section_Iterator_T;
      Sym : Symbol_Iterator_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:67
   pragma Import (C, Get_Section_Contains_Symbol_C, "LLVMGetSectionContainsSymbol");

   function Get_Relocations (Section : Section_Iterator_T) return Relocation_Iterator_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:71
   pragma Import (C, Get_Relocations, "LLVMGetRelocations");

   procedure Dispose_Relocation_Iterator (RI : Relocation_Iterator_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:72
   pragma Import (C, Dispose_Relocation_Iterator, "LLVMDisposeRelocationIterator");

   function Is_Relocation_Iterator_At_End
     (Section : Section_Iterator_T;
      RI      : Relocation_Iterator_T)
      return Boolean;
   function Is_Relocation_Iterator_At_End_C
     (Section : Section_Iterator_T;
      RI      : Relocation_Iterator_T)
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:73
   pragma Import (C, Is_Relocation_Iterator_At_End_C, "LLVMIsRelocationIteratorAtEnd");

   procedure Move_To_Next_Relocation (RI : Relocation_Iterator_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:75
   pragma Import (C, Move_To_Next_Relocation, "LLVMMoveToNextRelocation");

   function Get_Symbol_Name
     (SI : Symbol_Iterator_T)
      return String;
   function Get_Symbol_Name_C
     (SI : Symbol_Iterator_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:79
   pragma Import (C, Get_Symbol_Name_C, "LLVMGetSymbolName");

   function Get_Symbol_Address (SI : Symbol_Iterator_T) return stdint_h.uint64_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:80
   pragma Import (C, Get_Symbol_Address, "LLVMGetSymbolAddress");

   function Get_Symbol_Size (SI : Symbol_Iterator_T) return stdint_h.uint64_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:81
   pragma Import (C, Get_Symbol_Size, "LLVMGetSymbolSize");

   function Get_Relocation_Offset (RI : Relocation_Iterator_T) return stdint_h.uint64_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:84
   pragma Import (C, Get_Relocation_Offset, "LLVMGetRelocationOffset");

   function Get_Relocation_Symbol (RI : Relocation_Iterator_T) return Symbol_Iterator_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:85
   pragma Import (C, Get_Relocation_Symbol, "LLVMGetRelocationSymbol");

   function Get_Relocation_Type (RI : Relocation_Iterator_T) return stdint_h.uint64_t;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:86
   pragma Import (C, Get_Relocation_Type, "LLVMGetRelocationType");

   function Get_Relocation_Type_Name
     (RI : Relocation_Iterator_T)
      return String;
   function Get_Relocation_Type_Name_C
     (RI : Relocation_Iterator_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:89
   pragma Import (C, Get_Relocation_Type_Name_C, "LLVMGetRelocationTypeName");

   function Get_Relocation_Value_String
     (RI : Relocation_Iterator_T)
      return String;
   function Get_Relocation_Value_String_C
     (RI : Relocation_Iterator_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Object.h:90
   pragma Import (C, Get_Relocation_Value_String_C, "LLVMGetRelocationValueString");

end LLVM.Object;

