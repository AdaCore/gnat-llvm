pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with Interfaces.C.Strings;
with LLVM.Types;
with LLVM.Target;

package LLVM.Target_Machine is

  --===-- llvm-c/TargetMachine.h - Target Machine Library C Interface - C++ -*-=*|*                                                                            *|
  --|
  --|*                     The LLVM Compiler Infrastructure                       *|
  --|*                                                                            *|
  --|* This file is distributed under the University of Illinois Open Source      *|
  --|* License. See LICENSE.TXT for details.                                      *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to the Target and TargetMachine       *|
  --|* classes, which can be used to generate assembly or object files.           *|
  --|*                                                                            *|
  --|* Many exotic languages can interoperate with C code but have a harder time  *|
  --|* with C++ due to name mangling. So in addition to C, this interface enables *|
  --|* tools written in such languages.                                           *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

   --  skipped empty struct LLVMOpaqueTargetMachine

   type Target_Machine_T is new System.Address;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:28

   --  skipped empty struct LLVMTarget

   type Target_T is new System.Address;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:29

   type Code_Gen_Opt_Level_T is 
     (Code_Gen_Level_None,
      Code_Gen_Level_Less,
      Code_Gen_Level_Default,
      Code_Gen_Level_Aggressive);
   pragma Convention (C, Code_Gen_Opt_Level_T);  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:36

   type Reloc_Mode_T is 
     (Reloc_Default,
      Reloc_Static,
      Reloc_PIC,
      Reloc_Dynamic_No_Pic);
   pragma Convention (C, Reloc_Mode_T);  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:43

   type Code_Model_T is 
     (Code_Model_Default,
      Code_Model_JIT_Default,
      Code_Model_Small,
      Code_Model_Kernel,
      Code_Model_Medium,
      Code_Model_Large);
   pragma Convention (C, Code_Model_T);  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:52

   type Code_Gen_File_Type_T is 
     (Assembly_File,
      Object_File);
   pragma Convention (C, Code_Gen_File_Type_T);  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:57

  --* Returns the first llvm::Target in the registered targets list.  
   function Get_First_Target return Target_T;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:60
   pragma Import (C, Get_First_Target, "LLVMGetFirstTarget");

  --* Returns the next llvm::Target given a previous one (or null if there's none)  
   function Get_Next_Target (T : Target_T) return Target_T;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:62
   pragma Import (C, Get_Next_Target, "LLVMGetNextTarget");

  --===-- Target ------------------------------------------------------------=== 
  --* Finds the target corresponding to the given name and stores it in \p T.
  --  Returns 0 on success.  

   function Get_Target_From_Name
     (Name : String)
      return Target_T;
   function Get_Target_From_Name_C
     (Name : Interfaces.C.Strings.chars_ptr)
      return Target_T;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:67
   pragma Import (C, Get_Target_From_Name_C, "LLVMGetTargetFromName");

  --* Finds the target corresponding to the given triple and stores it in \p T.
  --  Returns 0 on success. Optionally returns any error in ErrorMessage.
  --  Use LLVMDisposeMessage to dispose the message.  

function Get_Target_From_Triple
     (Triple        : String;
      T             : System.Address;
      Error_Message : System.Address)
      return Boolean;
   function Get_Target_From_Triple_C
     (Triple        : Interfaces.C.Strings.chars_ptr;
      T             : System.Address;
      Error_Message : System.Address)
      return LLVM.Types.Bool_T;
   pragma Import (C, Get_Target_From_Triple_C, "LLVMGetTargetFromTriple");

  --* Returns the name of a target. See llvm::Target::getName  
   function Get_Target_Name
     (T : Target_T)
      return String;
   function Get_Target_Name_C
     (T : Target_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:76
   pragma Import (C, Get_Target_Name_C, "LLVMGetTargetName");

  --* Returns the description  of a target. See llvm::Target::getDescription  
   function Get_Target_Description
     (T : Target_T)
      return String;
   function Get_Target_Description_C
     (T : Target_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:79
   pragma Import (C, Get_Target_Description_C, "LLVMGetTargetDescription");

  --* Returns if the target has a JIT  
   function Target_Has_JIT
     (T : Target_T)
      return Boolean;
   function Target_Has_JIT_C
     (T : Target_T)
      return LLVM.Types.Bool_T;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:82
   pragma Import (C, Target_Has_JIT_C, "LLVMTargetHasJIT");

  --* Returns if the target has a TargetMachine associated  
   function Target_Has_Target_Machine
     (T : Target_T)
      return Boolean;
   function Target_Has_Target_Machine_C
     (T : Target_T)
      return LLVM.Types.Bool_T;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:85
   pragma Import (C, Target_Has_Target_Machine_C, "LLVMTargetHasTargetMachine");

  --* Returns if the target as an ASM backend (required for emitting output)  
   function Target_Has_Asm_Backend
     (T : Target_T)
      return Boolean;
   function Target_Has_Asm_Backend_C
     (T : Target_T)
      return LLVM.Types.Bool_T;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:88
   pragma Import (C, Target_Has_Asm_Backend_C, "LLVMTargetHasAsmBackend");

  --===-- Target Machine ----------------------------------------------------=== 
  --* Creates a new llvm::TargetMachine. See llvm::Target::createTargetMachine  
function Create_Target_Machine
     (T          : Target_T;
      Triple     : String;
      CPU        : String;
      Features   : String;
      Level      : Code_Gen_Opt_Level_T;
      Reloc      : Reloc_Mode_T;
      Code_Model : Code_Model_T)
      return Target_Machine_T;
   function Create_Target_Machine_C
     (T          : Target_T;
      Triple     : Interfaces.C.Strings.chars_ptr;
      CPU        : Interfaces.C.Strings.chars_ptr;
      Features   : Interfaces.C.Strings.chars_ptr;
      Level      : Code_Gen_Opt_Level_T;
      Reloc      : Reloc_Mode_T;
      Code_Model : Code_Model_T)
      return Target_Machine_T;
   pragma Import (C, Create_Target_Machine_C, "LLVMCreateTargetMachine");

  --* Dispose the LLVMTargetMachineRef instance generated by
  --  LLVMCreateTargetMachine.  

   procedure Dispose_Target_Machine (T : Target_Machine_T);  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:98
   pragma Import (C, Dispose_Target_Machine, "LLVMDisposeTargetMachine");

  --* Returns the Target used in a TargetMachine  
   function Get_Target_Machine_Target (T : Target_Machine_T) return Target_T;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:101
   pragma Import (C, Get_Target_Machine_Target, "LLVMGetTargetMachineTarget");

  --* Returns the triple used creating this target machine. See
  --  llvm::TargetMachine::getTriple. The result needs to be disposed with
  --  LLVMDisposeMessage.  

   function Get_Target_Machine_Triple
     (T : Target_Machine_T)
      return String;
   function Get_Target_Machine_Triple_C
     (T : Target_Machine_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:106
   pragma Import (C, Get_Target_Machine_Triple_C, "LLVMGetTargetMachineTriple");

  --* Returns the cpu used creating this target machine. See
  --  llvm::TargetMachine::getCPU. The result needs to be disposed with
  --  LLVMDisposeMessage.  

   function Get_Target_Machine_CPU
     (T : Target_Machine_T)
      return String;
   function Get_Target_Machine_CPU_C
     (T : Target_Machine_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:111
   pragma Import (C, Get_Target_Machine_CPU_C, "LLVMGetTargetMachineCPU");

  --* Returns the feature string used creating this target machine. See
  --  llvm::TargetMachine::getFeatureString. The result needs to be disposed with
  --  LLVMDisposeMessage.  

   function Get_Target_Machine_Feature_String
     (T : Target_Machine_T)
      return String;
   function Get_Target_Machine_Feature_String_C
     (T : Target_Machine_T)
      return Interfaces.C.Strings.chars_ptr;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:116
   pragma Import (C, Get_Target_Machine_Feature_String_C, "LLVMGetTargetMachineFeatureString");

  --* Create a DataLayout based on the targetMachine.  
   function Create_Target_Data_Layout (T : Target_Machine_T) return LLVM.Target.Target_Data_T;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:119
   pragma Import (C, Create_Target_Data_Layout, "LLVMCreateTargetDataLayout");

  --* Set the target machine's ASM verbosity.  
   procedure Set_Target_Machine_Asm_Verbosity
     (T           : Target_Machine_T;
      Verbose_Asm : Boolean);
   procedure Set_Target_Machine_Asm_Verbosity_C
     (T           : Target_Machine_T;
      Verbose_Asm : LLVM.Types.Bool_T);  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:122
   pragma Import (C, Set_Target_Machine_Asm_Verbosity_C, "LLVMSetTargetMachineAsmVerbosity");

  --* Emits an asm or object file for the given module to the filename. This
  --  wraps several c++ only classes (among them a file stream). Returns any
  --  error in ErrorMessage. Use LLVMDisposeMessage to dispose the message.  

function Target_Machine_Emit_To_File
     (T             : Target_Machine_T;
      M             : LLVM.Types.Module_T;
      Filename      : String;
      codegen       : Code_Gen_File_Type_T;
      Error_Message : System.Address)
      return Boolean;
   function Target_Machine_Emit_To_File_C
     (T             : Target_Machine_T;
      M             : LLVM.Types.Module_T;
      Filename      : Interfaces.C.Strings.chars_ptr;
      codegen       : Code_Gen_File_Type_T;
      Error_Message : System.Address)
      return LLVM.Types.Bool_T;
   pragma Import (C, Target_Machine_Emit_To_File_C, "LLVMTargetMachineEmitToFile");

  --* Compile the LLVM IR stored in \p M and store the result in \p OutMemBuf.  
function Target_Machine_Emit_To_Memory_Buffer
     (T             : Target_Machine_T;
      M             : LLVM.Types.Module_T;
      codegen       : Code_Gen_File_Type_T;
      Error_Message : System.Address;
      Out_Mem_Buf   : System.Address)
      return Boolean;
   function Target_Machine_Emit_To_Memory_Buffer_C
     (T             : Target_Machine_T;
      M             : LLVM.Types.Module_T;
      codegen       : Code_Gen_File_Type_T;
      Error_Message : System.Address;
      Out_Mem_Buf   : System.Address)
      return LLVM.Types.Bool_T;
   pragma Import (C, Target_Machine_Emit_To_Memory_Buffer_C, "LLVMTargetMachineEmitToMemoryBuffer");

  --===-- Triple ------------------------------------------------------------=== 
  --* Get a triple for the host machine as a string. The result needs to be
  --  disposed with LLVMDisposeMessage.  

   function Get_Default_Target_Triple
      return String;
   function Get_Default_Target_Triple_C
      return Interfaces.C.Strings.chars_ptr;  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:138
   pragma Import (C, Get_Default_Target_Triple_C, "LLVMGetDefaultTargetTriple");

  --* Adds the target-specific analysis passes to the pass manager.  
   procedure Add_Analysis_Passes (T : Target_Machine_T; PM : LLVM.Types.Pass_Manager_T);  -- llvm-5.0.0.src/include/llvm-c/TargetMachine.h:141
   pragma Import (C, Add_Analysis_Passes, "LLVMAddAnalysisPasses");

end LLVM.Target_Machine;

