pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with LLVM.Types;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;

package LLVM.Target is

  --===-- llvm-c/Target.h - Target Lib C Iface --------------------*- C++ -*-=== 
  --                                                                             
  -- Part of the LLVM Project, under the Apache License v2.0 with LLVM           
  -- Exceptions.                                                                 
  -- See https://llvm.org/LICENSE.txt for license information.                   
  -- SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                     
  --                                                                             
  --===----------------------------------------------------------------------=== 
  --                                                                             
  -- This header declares the C interface to libLLVMTarget.a, which              
  -- implements target information.                                              
  --                                                                             
  -- Many exotic languages can interoperate with C code but have a harder time   
  -- with C++ due to name mangling. So in addition to C, this interface enables  
  -- tools written in such languages.                                            
  --                                                                             
  --===----------------------------------------------------------------------=== 
  --*
  -- * @defgroup LLVMCTarget Target information
  -- * @ingroup LLVMC
  -- *
  -- * @{
  --  

   type Byte_Ordering_T is 
     (Big_Endian,
      Little_Endian)
   with Convention => C;  -- install/include/llvm-c/Target.h:35

   type Opaque_Target_Data_Impl_T is null record;   -- incomplete struct

   type Target_Data_T is access all Opaque_Target_Data_Impl_T;  -- install/include/llvm-c/Target.h:37

   type Opaque_Target_Library_Infot_Data_Impl_T is null record;   -- incomplete struct

   type Target_Library_Info_T is access all Opaque_Target_Library_Infot_Data_Impl_T;  -- install/include/llvm-c/Target.h:38

  -- Declare all of the target-initialization functions that are available.  
  -- Declare all of the available assembly printer initialization functions.  
  -- Declare all of the available assembly parser initialization functions.  
  -- Declare all of the available disassembler initialization functions.  
  --* LLVMInitializeAllTargetInfos - The main program should call this function if
  --    it wants access to all available targets that LLVM is configured to
  --    support.  

   procedure Initialize_All_Target_Infos  -- install/include/llvm-c/Target.h:76
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeAllTargetInfos";

  --* LLVMInitializeAllTargets - The main program should call this function if it
  --    wants to link in all available targets that LLVM is configured to
  --    support.  

   procedure Initialize_All_Targets  -- install/include/llvm-c/Target.h:85
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeAllTargets";

  --* LLVMInitializeAllTargetMCs - The main program should call this function if
  --    it wants access to all available target MC that LLVM is configured to
  --    support.  

   procedure Initialize_All_Target_M_Cs  -- install/include/llvm-c/Target.h:94
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeAllTargetMCs";

  --* LLVMInitializeAllAsmPrinters - The main program should call this function if
  --    it wants all asm printers that LLVM is configured to support, to make them
  --    available via the TargetRegistry.  

   procedure Initialize_All_Asm_Printers  -- install/include/llvm-c/Target.h:103
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeAllAsmPrinters";

  --* LLVMInitializeAllAsmParsers - The main program should call this function if
  --    it wants all asm parsers that LLVM is configured to support, to make them
  --    available via the TargetRegistry.  

   procedure Initialize_All_Asm_Parsers  -- install/include/llvm-c/Target.h:112
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeAllAsmParsers";

  --* LLVMInitializeAllDisassemblers - The main program should call this function
  --    if it wants all disassemblers that LLVM is configured to support, to make
  --    them available via the TargetRegistry.  

   procedure Initialize_All_Disassemblers  -- install/include/llvm-c/Target.h:121
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeAllDisassemblers";

  --* LLVMInitializeNativeTarget - The main program should call this function to
  --    initialize the native target corresponding to the host.  This is useful
  --    for JIT applications to ensure that the target gets linked in correctly.  

   function Initialize_Native_Target return LLVM.Types.Bool_T  -- install/include/llvm-c/Target.h:131
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeNativeTarget";

  -- If we have a native target, initialize it to ensure it is linked in.  
  --* LLVMInitializeNativeTargetAsmParser - The main program should call this
  --    function to initialize the parser for the native target corresponding to the
  --    host.  

   function Initialize_Native_Asm_Parser return LLVM.Types.Bool_T  -- install/include/llvm-c/Target.h:146
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeNativeAsmParser";

  --* LLVMInitializeNativeTargetAsmPrinter - The main program should call this
  --    function to initialize the printer for the native target corresponding to
  --    the host.  

   function Initialize_Native_Asm_Printer return LLVM.Types.Bool_T  -- install/include/llvm-c/Target.h:158
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeNativeAsmPrinter";

  --* LLVMInitializeNativeTargetDisassembler - The main program should call this
  --    function to initialize the disassembler for the native target corresponding
  --    to the host.  

   function Initialize_Native_Disassembler return LLVM.Types.Bool_T  -- install/include/llvm-c/Target.h:170
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMInitializeNativeDisassembler";

  --===-- Target Data -------------------------------------------------------=== 
  --*
  -- * Obtain the data layout for a module.
  -- *
  -- * @see Module::getDataLayout()
  --  

   function Get_Module_Data_Layout (M : LLVM.Types.Module_T) return Target_Data_T  -- install/include/llvm-c/Target.h:186
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetModuleDataLayout";

  --*
  -- * Set the data layout for a module.
  -- *
  -- * @see Module::setDataLayout()
  --  

   procedure Set_Module_Data_Layout (M : LLVM.Types.Module_T; DL : Target_Data_T)  -- install/include/llvm-c/Target.h:193
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSetModuleDataLayout";

  --* Creates target data from a target layout string.
  --    See the constructor llvm::DataLayout::DataLayout.  

function Create_Target_Data
     (String_Rep : String)
      return Target_Data_T;

  --* Deallocates a TargetData.
  --    See the destructor llvm::DataLayout::~DataLayout.  

   procedure Dispose_Target_Data (TD : Target_Data_T)  -- install/include/llvm-c/Target.h:201
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeTargetData";

  --* Adds target library information to a pass manager. This does not take
  --    ownership of the target library info.
  --    See the method llvm::PassManagerBase::add.  

   procedure Add_Target_Library_Info (TLI : Target_Library_Info_T; PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/Target.h:206
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddTargetLibraryInfo";

  --* Converts target data to a target layout string. The string must be disposed
  --    with LLVMDisposeMessage.
  --    See the constructor llvm::DataLayout::DataLayout.  

function Copy_String_Rep_Of_Target_Data
     (TD : Target_Data_T)
      return String;

  --* Returns the byte order of a target, either LLVMBigEndian or
  --    LLVMLittleEndian.
  --    See the method llvm::DataLayout::isLittleEndian.  

   function Byte_Order (TD : Target_Data_T) return Byte_Ordering_T  -- install/include/llvm-c/Target.h:217
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMByteOrder";

  --* Returns the pointer size in bytes for a target.
  --    See the method llvm::DataLayout::getPointerSize.  

   function Pointer_Size (TD : Target_Data_T) return unsigned  -- install/include/llvm-c/Target.h:221
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPointerSize";

  --* Returns the pointer size in bytes for a target for a specified
  --    address space.
  --    See the method llvm::DataLayout::getPointerSize.  

   function Pointer_Size_For_AS (TD : Target_Data_T; AS : unsigned) return unsigned  -- install/include/llvm-c/Target.h:226
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPointerSizeForAS";

  --* Returns the integer type that is the same size as a pointer on a target.
  --    See the method llvm::DataLayout::getIntPtrType.  

   function Int_Ptr_Type (TD : Target_Data_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Target.h:230
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIntPtrType";

  --* Returns the integer type that is the same size as a pointer on a target.
  --    This version allows the address space to be specified.
  --    See the method llvm::DataLayout::getIntPtrType.  

   function Int_Ptr_Type_For_AS (TD : Target_Data_T; AS : unsigned) return LLVM.Types.Type_T  -- install/include/llvm-c/Target.h:235
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIntPtrTypeForAS";

  --* Returns the integer type that is the same size as a pointer on a target.
  --    See the method llvm::DataLayout::getIntPtrType.  

   function Int_Ptr_Type_In_Context (C : LLVM.Types.Context_T; TD : Target_Data_T) return LLVM.Types.Type_T  -- install/include/llvm-c/Target.h:239
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIntPtrTypeInContext";

  --* Returns the integer type that is the same size as a pointer on a target.
  --    This version allows the address space to be specified.
  --    See the method llvm::DataLayout::getIntPtrType.  

   function Int_Ptr_Type_For_AS_In_Context
     (C : LLVM.Types.Context_T;
      TD : Target_Data_T;
      AS : unsigned) return LLVM.Types.Type_T  -- install/include/llvm-c/Target.h:244
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMIntPtrTypeForASInContext";

  --* Computes the size of a type in bytes for a target.
  --    See the method llvm::DataLayout::getTypeSizeInBits.  

   function Size_Of_Type_In_Bits (TD : Target_Data_T; Ty : LLVM.Types.Type_T) return Extensions.unsigned_long_long  -- install/include/llvm-c/Target.h:249
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMSizeOfTypeInBits";

  --* Computes the storage size of a type in bytes for a target.
  --    See the method llvm::DataLayout::getTypeStoreSize.  

   function Store_Size_Of_Type (TD : Target_Data_T; Ty : LLVM.Types.Type_T) return Extensions.unsigned_long_long  -- install/include/llvm-c/Target.h:253
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMStoreSizeOfType";

  --* Computes the ABI size of a type in bytes for a target.
  --    See the method llvm::DataLayout::getTypeAllocSize.  

   function ABI_Size_Of_Type (TD : Target_Data_T; Ty : LLVM.Types.Type_T) return Extensions.unsigned_long_long  -- install/include/llvm-c/Target.h:257
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMABISizeOfType";

  --* Computes the ABI alignment of a type in bytes for a target.
  --    See the method llvm::DataLayout::getTypeABISize.  

   function ABI_Alignment_Of_Type (TD : Target_Data_T; Ty : LLVM.Types.Type_T) return unsigned  -- install/include/llvm-c/Target.h:261
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMABIAlignmentOfType";

  --* Computes the call frame alignment of a type in bytes for a target.
  --    See the method llvm::DataLayout::getTypeABISize.  

   function Call_Frame_Alignment_Of_Type (TD : Target_Data_T; Ty : LLVM.Types.Type_T) return unsigned  -- install/include/llvm-c/Target.h:265
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCallFrameAlignmentOfType";

  --* Computes the preferred alignment of a type in bytes for a target.
  --    See the method llvm::DataLayout::getTypeABISize.  

   function Preferred_Alignment_Of_Type (TD : Target_Data_T; Ty : LLVM.Types.Type_T) return unsigned  -- install/include/llvm-c/Target.h:269
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPreferredAlignmentOfType";

  --* Computes the preferred alignment of a global variable in bytes for a target.
  --    See the method llvm::DataLayout::getPreferredAlignment.  

   function Preferred_Alignment_Of_Global (TD : Target_Data_T; Global_Var : LLVM.Types.Value_T) return unsigned  -- install/include/llvm-c/Target.h:273
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMPreferredAlignmentOfGlobal";

  --* Computes the structure element that contains the byte offset for a target.
  --    See the method llvm::StructLayout::getElementContainingOffset.  

   function Element_At_Offset
     (TD : Target_Data_T;
      Struct_Ty : LLVM.Types.Type_T;
      Offset : Extensions.unsigned_long_long) return unsigned  -- install/include/llvm-c/Target.h:278
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMElementAtOffset";

  --* Computes the byte offset of the indexed struct element for a target.
  --    See the method llvm::StructLayout::getElementContainingOffset.  

   function Offset_Of_Element
     (TD : Target_Data_T;
      Struct_Ty : LLVM.Types.Type_T;
      Element : unsigned) return Extensions.unsigned_long_long  -- install/include/llvm-c/Target.h:283
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMOffsetOfElement";

  --*
  -- * @}
  --  

end LLVM.Target;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
