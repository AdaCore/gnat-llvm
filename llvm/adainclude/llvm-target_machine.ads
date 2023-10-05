pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with Interfaces.C.Strings;
with System;
with LLVM.Types;
with LLVM.Target;

package LLVM.Target_Machine is

  --===-- llvm-c/TargetMachine.h - Target Machine Library C Interface - C++ -*-=*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
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

  --*
  -- * @addtogroup LLVMCTarget
  -- *
  -- * @{
  --  

   type Opaque_Target_Machine_Impl_T is null record;   -- incomplete struct

   type Target_Machine_T is access all Opaque_Target_Machine_Impl_T;  -- install/include/llvm-c/TargetMachine.h:34

   type Target_Impl_T is null record;   -- incomplete struct

   type Target_T is access all Target_Impl_T;  -- install/include/llvm-c/TargetMachine.h:35

   type Code_Gen_Opt_Level_T is 
     (Code_Gen_Level_None,
      Code_Gen_Level_Less,
      Code_Gen_Level_Default,
      Code_Gen_Level_Aggressive)
   with Convention => C;  -- install/include/llvm-c/TargetMachine.h:42

   type Reloc_Mode_T is 
     (Reloc_Default,
      Reloc_Static,
      Reloc_PIC,
      Reloc_Dynamic_No_Pic,
      Reloc_ROPI,
      Reloc_RWPI,
      Reloc_ROPI_RWPI)
   with Convention => C;  -- install/include/llvm-c/TargetMachine.h:52

   type Code_Model_T is 
     (Code_Model_Default,
      Code_Model_JIT_Default,
      Code_Model_Tiny,
      Code_Model_Small,
      Code_Model_Kernel,
      Code_Model_Medium,
      Code_Model_Large)
   with Convention => C;  -- install/include/llvm-c/TargetMachine.h:62

   type Code_Gen_File_Type_T is 
     (Assembly_File,
      Object_File)
   with Convention => C;  -- install/include/llvm-c/TargetMachine.h:67

  --* Returns the first llvm::Target in the registered targets list.  
   function Get_First_Target return Target_T  -- install/include/llvm-c/TargetMachine.h:70
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetFirstTarget";

  --* Returns the next llvm::Target given a previous one (or null if there's none)  
   function Get_Next_Target (T : Target_T) return Target_T  -- install/include/llvm-c/TargetMachine.h:72
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetNextTarget";

  --===-- Target ------------------------------------------------------------=== 
  --* Finds the target corresponding to the given name and stores it in \p T.
  --  Returns 0 on success.  

function Get_Target_From_Name
     (Name : String)
      return Target_T;

  --* Finds the target corresponding to the given triple and stores it in \p T.
  --  Returns 0 on success. Optionally returns any error in ErrorMessage.
  --  Use LLVMDisposeMessage to dispose the message.  

function Get_Target_From_Triple
     (Triple        : String;
      T             : System.Address;
      Error_Message : System.Address)
      return Boolean;

  --* Returns the name of a target. See llvm::Target::getName  
function Get_Target_Name
     (T : Target_T)
      return String;

  --* Returns the description  of a target. See llvm::Target::getDescription  
function Get_Target_Description
     (T : Target_T)
      return String;

  --* Returns if the target has a JIT  
function Target_Has_JIT
     (T : Target_T)
      return Boolean;

  --* Returns if the target has a TargetMachine associated  
function Target_Has_Target_Machine
     (T : Target_T)
      return Boolean;

  --* Returns if the target as an ASM backend (required for emitting output)  
function Target_Has_Asm_Backend
     (T : Target_T)
      return Boolean;

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

  --* Creates a new llvm::TargetMachine. See llvm::Target::createTargetMachine  
function Create_Target_Machine_With_ABI
     (T          : Target_T;
      Triple     : String;
      CPU        : String;
      Features   : String;
      ABI        : String;
      Level      : Code_Gen_Opt_Level_T;
      Reloc      : Reloc_Mode_T;
      Code_Model : Code_Model_T)
      return Target_Machine_T;

  --* Dispose the LLVMTargetMachineRef instance generated by
  --  LLVMCreateTargetMachine.  

   procedure Dispose_Target_Machine (T : Target_Machine_T)  -- install/include/llvm-c/TargetMachine.h:115
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMDisposeTargetMachine";

  --* Returns the Target used in a TargetMachine  
   function Get_Target_Machine_Target (T : Target_Machine_T) return Target_T  -- install/include/llvm-c/TargetMachine.h:118
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMGetTargetMachineTarget";

  --* Returns the triple used creating this target machine. See
  --  llvm::TargetMachine::getTriple. The result needs to be disposed with
  --  LLVMDisposeMessage.  

function Get_Target_Machine_Triple
     (T : Target_Machine_T)
      return String;

  --* Returns the cpu used creating this target machine. See
  --  llvm::TargetMachine::getCPU. The result needs to be disposed with
  --  LLVMDisposeMessage.  

function Get_Target_Machine_CPU
     (T : Target_Machine_T)
      return String;

  --* Returns the feature string used creating this target machine. See
  --  llvm::TargetMachine::getFeatureString. The result needs to be disposed with
  --  LLVMDisposeMessage.  

function Get_Target_Machine_Feature_String
     (T : Target_Machine_T)
      return String;

  --* Create a DataLayout based on the targetMachine.  
   function Create_Target_Data_Layout (T : Target_Machine_T) return LLVM.Target.Target_Data_T  -- install/include/llvm-c/TargetMachine.h:136
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMCreateTargetDataLayout";

  --* Set the target machine's ASM verbosity.  
procedure Set_Target_Machine_Asm_Verbosity
     (T           : Target_Machine_T;
      Verbose_Asm : Boolean);

  --* Emits an asm or object file for the given module to the filename. This
  --  wraps several c++ only classes (among them a file stream). Returns any
  --  error in ErrorMessage. Use LLVMDisposeMessage to dispose the message.  

function Target_Machine_Emit_To_File
     (T             : Target_Machine_T;
      M             : LLVM.Types.Module_T;
      Filename      : String;
      Codegen       : Code_Gen_File_Type_T;
      Error_Message : System.Address)
      return Boolean;

  --* Compile the LLVM IR stored in \p M and store the result in \p OutMemBuf.  
function Target_Machine_Emit_To_Memory_Buffer
     (T             : Target_Machine_T;
      M             : LLVM.Types.Module_T;
      Codegen       : Code_Gen_File_Type_T;
      Error_Message : System.Address;
      Out_Mem_Buf   : System.Address)
      return Boolean;

  --===-- Triple ------------------------------------------------------------=== 
  --* Get a triple for the host machine as a string. The result needs to be
  --  disposed with LLVMDisposeMessage.  

function Get_Default_Target_Triple
      return String;

  --* Normalize a target triple. The result needs to be disposed with
  --  LLVMDisposeMessage.  

function Normalize_Target_Triple
     (Triple : String)
      return String;

  --* Get the host CPU as a string. The result needs to be disposed with
  --  LLVMDisposeMessage.  

function Get_Host_CPU_Name
      return String;

  --* Get the host CPU's features as a string. The result needs to be disposed
  --  with LLVMDisposeMessage.  

function Get_Host_CPU_Features
      return String;

  --* Adds the target-specific analysis passes to the pass manager.  
   procedure Add_Analysis_Passes (T : Target_Machine_T; PM : LLVM.Types.Pass_Manager_T)  -- install/include/llvm-c/TargetMachine.h:172
   with Import => True, 
        Convention => C, 
        External_Name => "LLVMAddAnalysisPasses";

  --*
  -- * @}
  --  

end LLVM.Target_Machine;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
