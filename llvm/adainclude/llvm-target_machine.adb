pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Target_Machine is

   function Get_Target_From_Name
     (Name : Interfaces.C.Strings.chars_ptr)
      return Target_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetTargetFromName";
   function Get_Target_From_Name
     (Name : String)
      return Target_T
   is
      Return_Value : Target_T;
      Name_Array   : aliased char_array := To_C (Name);
      Name_String  : constant chars_ptr := To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      Return_Value := Get_Target_From_Name (Name_String);
      return Return_Value;
   end Get_Target_From_Name;

   function Get_Target_From_Triple
     (Triple        : Interfaces.C.Strings.chars_ptr;
      T             : System.Address;
      Error_Message : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetTargetFromTriple";
   function Get_Target_From_Triple
     (Triple        : String;
      T             : System.Address;
      Error_Message : System.Address)
      return LLVM.Types.Bool_T
   is
      Return_Value  : LLVM.Types.Bool_T;
      Triple_Array  : aliased char_array := To_C (Triple);
      Triple_String : constant chars_ptr := To_Chars_Ptr (Triple_Array'Unchecked_Access);
   begin
      Return_Value := Get_Target_From_Triple (Triple_String, T, Error_Message);
      return Return_Value;
   end Get_Target_From_Triple;

   function Get_Target_From_Triple
     (Triple        : String;
      T             : System.Address;
      Error_Message : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Get_Target_From_Triple (Triple, T, Error_Message);
      return Return_Value /= 0;
   end Get_Target_From_Triple;

   function Get_Target_Name
     (T : Target_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetTargetName";
   function Get_Target_Name
     (T : Target_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Target_Name (T);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Target_Name;

   function Get_Target_Description
     (T : Target_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetTargetDescription";
   function Get_Target_Description
     (T : Target_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Target_Description (T);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Target_Description;

   function Target_Has_JIT
     (T : Target_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMTargetHasJIT";
   function Target_Has_JIT
     (T : Target_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Target_Has_JIT (T);
      return Return_Value /= 0;
   end Target_Has_JIT;

   function Target_Has_Target_Machine
     (T : Target_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMTargetHasTargetMachine";
   function Target_Has_Target_Machine
     (T : Target_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Target_Has_Target_Machine (T);
      return Return_Value /= 0;
   end Target_Has_Target_Machine;

   function Target_Has_Asm_Backend
     (T : Target_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMTargetHasAsmBackend";
   function Target_Has_Asm_Backend
     (T : Target_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Target_Has_Asm_Backend (T);
      return Return_Value /= 0;
   end Target_Has_Asm_Backend;

   function Create_Target_Machine
     (T          : Target_T;
      Triple     : Interfaces.C.Strings.chars_ptr;
      CPU        : Interfaces.C.Strings.chars_ptr;
      Features   : Interfaces.C.Strings.chars_ptr;
      Level      : Code_Gen_Opt_Level_T;
      Reloc      : Reloc_Mode_T;
      Code_Model : Code_Model_T)
      return Target_Machine_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateTargetMachine";
   function Create_Target_Machine
     (T          : Target_T;
      Triple     : String;
      CPU        : String;
      Features   : String;
      Level      : Code_Gen_Opt_Level_T;
      Reloc      : Reloc_Mode_T;
      Code_Model : Code_Model_T)
      return Target_Machine_T
   is
      Return_Value    : Target_Machine_T;
      Triple_Array    : aliased char_array := To_C (Triple);
      Triple_String   : constant chars_ptr := To_Chars_Ptr (Triple_Array'Unchecked_Access);
      CPU_Array       : aliased char_array := To_C (CPU);
      CPU_String      : constant chars_ptr := To_Chars_Ptr (CPU_Array'Unchecked_Access);
      Features_Array  : aliased char_array := To_C (Features);
      Features_String : constant chars_ptr := To_Chars_Ptr (Features_Array'Unchecked_Access);
   begin
      Return_Value := Create_Target_Machine (T, Triple_String, CPU_String, Features_String, Level, Reloc, Code_Model);
      return Return_Value;
   end Create_Target_Machine;

   function Create_Target_Machine_With_ABI
     (T          : Target_T;
      Triple     : Interfaces.C.Strings.chars_ptr;
      CPU        : Interfaces.C.Strings.chars_ptr;
      Features   : Interfaces.C.Strings.chars_ptr;
      ABI        : Interfaces.C.Strings.chars_ptr;
      Level      : Code_Gen_Opt_Level_T;
      Reloc      : Reloc_Mode_T;
      Code_Model : Code_Model_T)
      return Target_Machine_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMCreateTargetMachineWithABI";
   function Create_Target_Machine_With_ABI
     (T          : Target_T;
      Triple     : String;
      CPU        : String;
      Features   : String;
      ABI        : String;
      Level      : Code_Gen_Opt_Level_T;
      Reloc      : Reloc_Mode_T;
      Code_Model : Code_Model_T)
      return Target_Machine_T
   is
      Return_Value    : Target_Machine_T;
      Triple_Array    : aliased char_array := To_C (Triple);
      Triple_String   : constant chars_ptr := To_Chars_Ptr (Triple_Array'Unchecked_Access);
      CPU_Array       : aliased char_array := To_C (CPU);
      CPU_String      : constant chars_ptr := To_Chars_Ptr (CPU_Array'Unchecked_Access);
      Features_Array  : aliased char_array := To_C (Features);
      Features_String : constant chars_ptr := To_Chars_Ptr (Features_Array'Unchecked_Access);
      ABI_Array       : aliased char_array := To_C (ABI);
      ABI_String      : constant chars_ptr := To_Chars_Ptr (ABI_Array'Unchecked_Access);
   begin
      Return_Value := Create_Target_Machine_With_ABI (T, Triple_String, CPU_String, Features_String, ABI_String, Level, Reloc, Code_Model);
      return Return_Value;
   end Create_Target_Machine_With_ABI;

   function Get_Target_Machine_Triple
     (T : Target_Machine_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetTargetMachineTriple";
   function Get_Target_Machine_Triple
     (T : Target_Machine_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Target_Machine_Triple (T);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Target_Machine_Triple;

   function Get_Target_Machine_CPU
     (T : Target_Machine_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetTargetMachineCPU";
   function Get_Target_Machine_CPU
     (T : Target_Machine_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Target_Machine_CPU (T);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Target_Machine_CPU;

   function Get_Target_Machine_Feature_String
     (T : Target_Machine_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetTargetMachineFeatureString";
   function Get_Target_Machine_Feature_String
     (T : Target_Machine_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Target_Machine_Feature_String (T);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Target_Machine_Feature_String;

   procedure Set_Target_Machine_Asm_Verbosity
     (T           : Target_Machine_T;
      Verbose_Asm : LLVM.Types.Bool_T)
   with Import => True,
        Convention => C,
        External_Name => "LLVMSetTargetMachineAsmVerbosity";
   procedure Set_Target_Machine_Asm_Verbosity
     (T           : Target_Machine_T;
      Verbose_Asm : Boolean)
   is
      Verbose_Asm_Bool : constant LLVM.Types.Bool_T := Boolean'Pos (Verbose_Asm);
   begin
      Set_Target_Machine_Asm_Verbosity (T, Verbose_Asm_Bool);
   end Set_Target_Machine_Asm_Verbosity;

   function Target_Machine_Emit_To_File
     (T             : Target_Machine_T;
      M             : LLVM.Types.Module_T;
      Filename      : Interfaces.C.Strings.chars_ptr;
      Codegen       : Code_Gen_File_Type_T;
      Error_Message : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMTargetMachineEmitToFile";
   function Target_Machine_Emit_To_File
     (T             : Target_Machine_T;
      M             : LLVM.Types.Module_T;
      Filename      : String;
      Codegen       : Code_Gen_File_Type_T;
      Error_Message : System.Address)
      return LLVM.Types.Bool_T
   is
      Return_Value    : LLVM.Types.Bool_T;
      Filename_Array  : aliased char_array := To_C (Filename);
      Filename_String : constant chars_ptr := To_Chars_Ptr (Filename_Array'Unchecked_Access);
   begin
      Return_Value := Target_Machine_Emit_To_File (T, M, Filename_String, Codegen, Error_Message);
      return Return_Value;
   end Target_Machine_Emit_To_File;

   function Target_Machine_Emit_To_File
     (T             : Target_Machine_T;
      M             : LLVM.Types.Module_T;
      Filename      : String;
      Codegen       : Code_Gen_File_Type_T;
      Error_Message : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Target_Machine_Emit_To_File (T, M, Filename, Codegen, Error_Message);
      return Return_Value /= 0;
   end Target_Machine_Emit_To_File;

   function Target_Machine_Emit_To_Memory_Buffer
     (T             : Target_Machine_T;
      M             : LLVM.Types.Module_T;
      Codegen       : Code_Gen_File_Type_T;
      Error_Message : System.Address;
      Out_Mem_Buf   : System.Address)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMTargetMachineEmitToMemoryBuffer";
   function Target_Machine_Emit_To_Memory_Buffer
     (T             : Target_Machine_T;
      M             : LLVM.Types.Module_T;
      Codegen       : Code_Gen_File_Type_T;
      Error_Message : System.Address;
      Out_Mem_Buf   : System.Address)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Target_Machine_Emit_To_Memory_Buffer (T, M, Codegen, Error_Message, Out_Mem_Buf);
      return Return_Value /= 0;
   end Target_Machine_Emit_To_Memory_Buffer;

   function Get_Default_Target_Triple
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetDefaultTargetTriple";
   function Get_Default_Target_Triple
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Default_Target_Triple;
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Default_Target_Triple;

   function Normalize_Target_Triple
     (Triple : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMNormalizeTargetTriple";
   function Normalize_Target_Triple
     (Triple : String)
      return String
   is
      Return_Value  : Interfaces.C.Strings.chars_ptr;
      Triple_Array  : aliased char_array := To_C (Triple);
      Triple_String : constant chars_ptr := To_Chars_Ptr (Triple_Array'Unchecked_Access);
   begin
      Return_Value := Normalize_Target_Triple (Triple_String);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Normalize_Target_Triple;

   function Get_Host_CPU_Name
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetHostCPUName";
   function Get_Host_CPU_Name
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Host_CPU_Name;
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Host_CPU_Name;

   function Get_Host_CPU_Features
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetHostCPUFeatures";
   function Get_Host_CPU_Features
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Host_CPU_Features;
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Host_CPU_Features;

end LLVM.Target_Machine;
