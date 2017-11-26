pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with LLVM.Types;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;

package LLVM.Target is

   type Byte_Ordering_T is 
     (Big_Endian,
      Little_Endian);
   pragma Convention (C, Byte_Ordering_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:40

   --  skipped empty struct LLVMOpaqueTargetData

   type Target_Data_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:42

   --  skipped empty struct LLVMOpaqueTargetLibraryInfotData

   type Target_Library_Info_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:43

   procedure Initialize_All_Target_Infos;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:81
   pragma Import (C, Initialize_All_Target_Infos, "LLVMInitializeAllTargetInfos");

   procedure Initialize_All_Targets;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:90
   pragma Import (C, Initialize_All_Targets, "LLVMInitializeAllTargets");

   procedure Initialize_All_Target_M_Cs;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:99
   pragma Import (C, Initialize_All_Target_M_Cs, "LLVMInitializeAllTargetMCs");

   procedure Initialize_All_Asm_Printers;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:108
   pragma Import (C, Initialize_All_Asm_Printers, "LLVMInitializeAllAsmPrinters");

   procedure Initialize_All_Asm_Parsers;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:117
   pragma Import (C, Initialize_All_Asm_Parsers, "LLVMInitializeAllAsmParsers");

   procedure Initialize_All_Disassemblers;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:126
   pragma Import (C, Initialize_All_Disassemblers, "LLVMInitializeAllDisassemblers");

   function Initialize_Native_Target_C
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:136
   pragma Import (C, Initialize_Native_Target_C, "LLVMInitializeNativeTarget");

   function Initialize_Native_Asm_Parser_C
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:151
   pragma Import (C, Initialize_Native_Asm_Parser_C, "LLVMInitializeNativeAsmParser");

   function Initialize_Native_Asm_Printer_C
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:163
   pragma Import (C, Initialize_Native_Asm_Printer_C, "LLVMInitializeNativeAsmPrinter");

   function Initialize_Native_Disassembler_C
      return LLVM.Types.Bool_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:175
   pragma Import (C, Initialize_Native_Disassembler_C, "LLVMInitializeNativeDisassembler");

   function Get_Module_Data_Layout (M : LLVM.Types.Module_T) return Target_Data_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:191
   pragma Import (C, Get_Module_Data_Layout, "LLVMGetModuleDataLayout");

   procedure Set_Module_Data_Layout (M : LLVM.Types.Module_T; DL : Target_Data_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:198
   pragma Import (C, Set_Module_Data_Layout, "LLVMSetModuleDataLayout");

   function Create_Target_Data
     (String_Rep : String)
      return Target_Data_T;
   function Create_Target_Data_C
     (String_Rep : Interfaces.C.Strings.chars_ptr)
      return Target_Data_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:202
   pragma Import (C, Create_Target_Data_C, "LLVMCreateTargetData");

   procedure Dispose_Target_Data (TD : Target_Data_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:206
   pragma Import (C, Dispose_Target_Data, "LLVMDisposeTargetData");

   procedure Add_Target_Library_Info (TLI : Target_Library_Info_T; PM : LLVM.Types.Pass_Manager_T);  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:211
   pragma Import (C, Add_Target_Library_Info, "LLVMAddTargetLibraryInfo");

   function Copy_String_Rep_Of_Target_Data
     (TD : Target_Data_T)
      return String;
   function Copy_String_Rep_Of_Target_Data_C
     (TD : Target_Data_T)
      return Interfaces.C.Strings.chars_ptr;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:217
   pragma Import (C, Copy_String_Rep_Of_Target_Data_C, "LLVMCopyStringRepOfTargetData");

   function Byte_Order (TD : Target_Data_T) return Byte_Ordering_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:222
   pragma Import (C, Byte_Order, "LLVMByteOrder");

   function Pointer_Size (TD : Target_Data_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:226
   pragma Import (C, Pointer_Size, "LLVMPointerSize");

   function Pointer_Size_For_AS (TD : Target_Data_T; AS : unsigned) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:231
   pragma Import (C, Pointer_Size_For_AS, "LLVMPointerSizeForAS");

   function Int_Ptr_Type (TD : Target_Data_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:235
   pragma Import (C, Int_Ptr_Type, "LLVMIntPtrType");

   function Int_Ptr_Type_For_AS (TD : Target_Data_T; AS : unsigned) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:240
   pragma Import (C, Int_Ptr_Type_For_AS, "LLVMIntPtrTypeForAS");

   function Int_Ptr_Type_In_Context (C : LLVM.Types.Context_T; TD : Target_Data_T) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:244
   pragma Import (C, Int_Ptr_Type_In_Context, "LLVMIntPtrTypeInContext");

   function Int_Ptr_Type_For_AS_In_Context
     (C : LLVM.Types.Context_T;
      TD : Target_Data_T;
      AS : unsigned) return LLVM.Types.Type_T;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:249
   pragma Import (C, Int_Ptr_Type_For_AS_In_Context, "LLVMIntPtrTypeForASInContext");

   function Size_Of_Type_In_Bits (TD : Target_Data_T; Ty : LLVM.Types.Type_T) return Extensions.unsigned_long_long;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:254
   pragma Import (C, Size_Of_Type_In_Bits, "LLVMSizeOfTypeInBits");

   function Store_Size_Of_Type (TD : Target_Data_T; Ty : LLVM.Types.Type_T) return Extensions.unsigned_long_long;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:258
   pragma Import (C, Store_Size_Of_Type, "LLVMStoreSizeOfType");

   function ABI_Size_Of_Type (TD : Target_Data_T; Ty : LLVM.Types.Type_T) return Extensions.unsigned_long_long;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:262
   pragma Import (C, ABI_Size_Of_Type, "LLVMABISizeOfType");

   function ABI_Alignment_Of_Type (TD : Target_Data_T; Ty : LLVM.Types.Type_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:266
   pragma Import (C, ABI_Alignment_Of_Type, "LLVMABIAlignmentOfType");

   function Call_Frame_Alignment_Of_Type (TD : Target_Data_T; Ty : LLVM.Types.Type_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:270
   pragma Import (C, Call_Frame_Alignment_Of_Type, "LLVMCallFrameAlignmentOfType");

   function Preferred_Alignment_Of_Type (TD : Target_Data_T; Ty : LLVM.Types.Type_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:274
   pragma Import (C, Preferred_Alignment_Of_Type, "LLVMPreferredAlignmentOfType");

   function Preferred_Alignment_Of_Global (TD : Target_Data_T; Global_Var : LLVM.Types.Value_T) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:278
   pragma Import (C, Preferred_Alignment_Of_Global, "LLVMPreferredAlignmentOfGlobal");

   function Element_At_Offset
     (TD : Target_Data_T;
      Struct_Ty : LLVM.Types.Type_T;
      Offset : Extensions.unsigned_long_long) return unsigned;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:283
   pragma Import (C, Element_At_Offset, "LLVMElementAtOffset");

   function Offset_Of_Element
     (TD : Target_Data_T;
      Struct_Ty : LLVM.Types.Type_T;
      Element : unsigned) return Extensions.unsigned_long_long;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Target.h:288
   pragma Import (C, Offset_Of_Element, "LLVMOffsetOfElement");

end LLVM.Target;

