------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2023, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Options; use Options;

package GNATLLVM.Codegen is

   type Code_Generation_Kind is
     (Dump_IR, Write_IR, Write_BC, Write_Assembly, Write_Object, Write_C,
      None);

   Filename        : String_Access := new String'("");
   --  Filename to compile.

   Code_Generation : Code_Generation_Kind := Write_Object;
   --  Type of code generation we're doing

   Emit_C          : Boolean        := CCG;
   --  True if -emit-c was specified explicitly or CCG set

   Use_FE_Data     : Boolean        := Emit_C;
   --  Use Front End data to help C code generation

   Use_GNAT_Allocs : Boolean        := False;
   --  True if we should emit calls to __gnat_malloc and __gnat_free even
   --  if generating C.

   CPU             : String_Access  := new String'("generic");
   --  Name of the specific CPU for this compilation.

   ABI             : String_Access := new String'("");
   --  Name of the ABI to use during code generation.

   Features        : String_Access  := new String'("");
   --  Features to enable or disable for this target

   Target_Triple   : String_Access  :=
     new String'(Get_Default_Target_Triple);
   --  Name of the target for this compilation

   Normalized_Target_Triple : String_Access := null;
   --  Target for this compilation, normalized for LLVM

   Target_Layout   : String_Access  := null;
   --  Target data layout, if specified

   Code_Gen_Level  : Code_Gen_Opt_Level_T := Code_Gen_Level_None;
   --  Optimization level for codegen

   Code_Model        : Code_Model_T  := Code_Model_Default;
   Reloc_Mode        : Reloc_Mode_T  := Reloc_Default;
   PIC_Level         : PIC_PIE_Level := 0;
   PIE_Level         : PIC_PIE_Level := 0;
   No_Implicit_Float : Boolean       := False;
   --  Code generation options

   Code_Opt_Level  : Int            := 0;
   Size_Opt_Level  : Int            := 0;
   --  Optimization levels

   SEH             : Boolean        := False;
   --  True if the target supports Structured Exception Handling

   DSO_Preemptable : Boolean        := False;
   --  Indicates that the function or variable may be replaced by a symbol
   --  from outside the linkage unit at runtime. clang derives this from
   --  a complex set of machine-dependent criterial, but the need for
   --  this is rare enough that we'll just provide a switch instead.

   Optimize_IR     : Boolean := True;
   --  True if we should optimize IR before writing it out when optimization
   --  is enabled.

   Enable_Execute_Stack : Boolean := False;
   --  True if we have to explicitly make the stack executable when we need
   --  it to be (e.g., when using stack-allocated trampolines).

   No_Strict_Aliasing_Flag : Boolean       := False;
   C_Style_Aliasing        : Boolean       := False;
   No_Inlining             : Boolean       := False;
   No_Unroll_Loops         : Boolean       := False;
   No_Loop_Vectorization   : Boolean       := False;
   No_SLP_Vectorization    : Boolean       := False;
   Merge_Functions         : Boolean       := True;
   Prepare_For_Thin_LTO    : Boolean       := False;
   Prepare_For_LTO         : Boolean       := False;
   Reroll_Loops            : Boolean       := False;
   No_Tail_Calls           : Boolean       := False;
   Pass_Plugin_Name        : String_Access := null;
   --  Switch options for optimization

   Enable_Fuzzer            : Boolean       := False;
   Enable_Address_Sanitizer : Boolean       := False;
   San_Cov_Allow_List       : String_Access := null;
   San_Cov_Ignore_List      : String_Access := null;
   --  Sanitizer options (including the fuzzer, which implies coverage
   --  sanitizer)

   Force_Activation_Record_Parameter : Boolean := False;
   --  Indicates that we need to force all subprograms to have an activation
   --  record parameter. We need to do this for targets, such as WebAssembly,
   --  that require strict parameter agreement between calls and declarations.

   procedure Scan_Command_Line;
   --  Scan operands relevant to code generation

   procedure Initialize_GNAT_LLVM;
   --  Perform initializations that need to be done before calling the
   --  front end.

   procedure Initialize_LLVM_Target;
   --  Initialize all the data structures specific to the LLVM target code
   --  generation.

   procedure Generate_Code (GNAT_Root : N_Compilation_Unit_Id);
   --  Generate LLVM code from what we've compiled with a node for error
   --  messages.

   function Is_Back_End_Switch (Switch : String) return Boolean;
   --  Return True if Switch is a switch known to the back end

   function Output_File_Name (Extension : String) return String;
   --  Return the name of the output file, using the given Extension

   procedure Early_Error (S : String);
   --  This is called too early to call Error_Msg (because we haven't
   --  initialized the source input structure), so we have to use a
   --  low-level mechanism to emit errors here.

   function Get_LLVM_Error_Msg (Msg : Ptr_Err_Msg_Type) return String;
   --  Get the LLVM error message that was stored in Msg

   Libdevice_Filename : String_Access :=
     new String'("/usr/local/cuda/nvvm/libdevice/libdevice.10.bc");
   --  Location for libdevice for CUDA.
   --  ??? This should be moved back to the body once VC21-031 is fixed

end GNATLLVM.Codegen;
