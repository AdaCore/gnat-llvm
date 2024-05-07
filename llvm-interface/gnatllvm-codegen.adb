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

with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces;
with Interfaces.C;      use Interfaces.C;
with System;            use System;
with System.OS_Lib;     use System.OS_Lib;

with LLVM.Analysis;   use LLVM.Analysis;
with LLVM.Bit_Reader; use LLVM.Bit_Reader;
with LLVM.Bit_Writer; use LLVM.Bit_Writer;
with LLVM.Debug_Info; use LLVM.Debug_Info;
with LLVM.Linker;     use LLVM.Linker;
with LLVM.Support;    use LLVM.Support;

with CCG; use CCG;

with Debug;    use Debug;
with Errout;   use Errout;
with Set_Targ; use Set_Targ;
with Lib;      use Lib;
with Opt;      use Opt;
with Osint;    use Osint;
with Osint.C;  use Osint.C;
with Output;   use Output;
with Switch;   use Switch;
with Table;
with Targparm; use Targparm;

with GNATLLVM.Helper;  use GNATLLVM.Helper;
with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

package body GNATLLVM.Codegen is

   package Switches is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Interfaces.C.int,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 1,
      Table_Name           => "Switches");

   Target_Triple_Set            : Boolean := False;
   --  Set to True by Process_Switch if Target_Triple was modified

   PIC_PIE_Set                   : Boolean := False;
   --  True if any of the PIC/PIE options was specified

   Output_Assembly               : Boolean := False;
   --  True if -S was specified

   Emit_LLVM                      : Boolean := False;
   --  True if -emit-llvm was specified

   GNAT_LLVM_Initialized          : Boolean := False;
   --  Set when Initialize_GNAT_LLVM has done something

   Arch                           : String_Access;
   --  Name of the architecture requested with -march.

   Dump_Targets                   : Boolean := False;

   procedure Process_Switch (S : String);
   --  Process one command-line switch

   --------------------------
   -- Initialize_GNAT_LLVM --
   --------------------------

   procedure Initialize_GNAT_LLVM is
   begin
      if not GNAT_LLVM_Initialized then
         Switches.Init;
         Scan_Command_Line;
         Initialize_LLVM_Target;
         Always_Compatible_Rep_On_Target := False;
         GNAT_LLVM_Initialized := True;
      end if;
   end Initialize_GNAT_LLVM;

   --------------------
   -- Process_Switch --
   --------------------

   procedure Process_Switch (S : String) is
      First   : constant Integer := S'First;
      Last    : constant Integer := Switch_Last (S);
      Len     : constant Integer := Last - First + 1;
      Idx     : Natural;
      To_Free : String_Access    := null;

      function Add_Maybe_With_Comma (S1, S2 : String) return String is
        ((if S1 = "" then S1 else S1 & ",") & S2);
      --  Concatenate S1 and S2, putting a comma in between if S1 is empty

   begin
      --  ??? At some point, this and Is_Back_End_Switch need to have
      --  some sort of common code.

      if Len > 0 and then S (First) /= '-' then
         if Is_Regular_File (S) then
            Free (Filename);
            Filename := new String'(S);
         end if;

      elsif S = "--dump-ir" then
         Code_Generation := Dump_IR;
      elsif S in "--dump-bc" | "--write-bc" then
         Code_Generation := Write_BC;
      elsif S = "-emit-c" then
         Emit_C := True;
      elsif S = "-emit-llvm" then
         Emit_LLVM := True;
      elsif S = "-S" then
         Output_Assembly := True;
      elsif S = "-fuse-gnat-allocs" then
         Use_GNAT_Allocs := True;
      elsif S = "-g"
        or else (Starts_With (S, "-g") and then not Starts_With (S, "-gnat"))
      then
         Emit_Debug_Info      := True;
         Emit_Full_Debug_Info := True;
      elsif S = "-fstack-check" then
         Do_Stack_Check := True;
      elsif S = "-fshort-enums" then
         Short_Enums := True;
      elsif S = "-foptimize-ir" then
         Optimize_IR := True;
      elsif S = "-fno-optimize-ir" then
         Optimize_IR := False;
      elsif S = "--dump-targets" then
         Dump_Targets := True;
      elsif Starts_With (S, "--target=") then
         To_Free           := Target_Triple;
         Target_Triple     := new String'(Switch_Value (S, "--target="));
         Target_Triple_Set := True;
         Idx               := Index (Target_Triple.all, ":");

         if Idx /= 0 then
            Free (To_Free);
            To_Free       := Target_Triple;
            Target_Layout :=
              new String'(Target_Triple.all (Idx + 1 .. Target_Triple'Last));
            Target_Triple :=
              new String'(Target_Triple.all (Target_Triple'First .. Idx - 1));
         end if;

      elsif Starts_With (S, "-mtriple=") then
         To_Free           := Target_Triple;
         Target_Triple_Set := True;
         Target_Triple     := new String'(Switch_Value (S, "-mtriple="));

      elsif Starts_With (S, "-mcuda-libdevice=") then
         Free (Libdevice_Filename);
         Libdevice_Filename :=
           new String'(Switch_Value (S, "-mcuda-libdevice="));

      elsif Starts_With (S, "--layout=") then
         To_Free           := Target_Layout;
         Target_Layout := new String'(Switch_Value (S, "--layout="));

      --  -march= and -mcpu= set the architecture and CPU to be used.
      --  -mtune= does likewise, but only if we haven't already seen one of
      --  the previous two switches

      elsif Starts_With (S, "-march=") then
         To_Free       := Arch;
         Arch          := new String'(Switch_Value (S, "-march="));
      elsif Starts_With (S, "-mcpu=") then
         To_Free       := CPU;
         CPU           := new String'(Switch_Value (S, "-mcpu="));
      elsif Starts_With (S, "-mtune=") then
         if CPU.all = "generic" and then Arch = null then
            To_Free    := CPU;
            CPU        := new String'(Switch_Value (S, "-march="));
         end if;

      --  -mabi= tells the code generator which ABI to use on some
      --  platforms

      elsif Starts_With (S, "-mabi=") then
         To_Free         := ABI;
         ABI             := new String'(Switch_Value (S, "-mabi="));
         Tagged_Pointers := ABI.all = "purecap";

      elsif S = "-mdso-preemptable" then
         DSO_Preemptable := True;
      elsif S = "-mdso-local" then
         DSO_Preemptable := False;
      elsif S = "-mcode-model=small" then
         Code_Model := Code_Model_Small;
      elsif S = "-mcode-model=kernel" then
         Code_Model := Code_Model_Kernel;
      elsif S = "-mcode-model=medium" then
         Code_Model := Code_Model_Medium;
      elsif S = "-mcode-model=large" then
         Code_Model := Code_Model_Large;
      elsif S = "-mcode-model=default" then
         Code_Model := Code_Model_Default;
      elsif S = "-mrelocation-model=static" then
         Reloc_Mode := Reloc_Static;
      elsif S = "-mrelocation-model=pic" then
         Reloc_Mode := Reloc_PIC;
      elsif S = "-mrelocation-model=dynamic-no-pic" then
         Reloc_Mode := Reloc_Dynamic_No_Pic;
      elsif S = "-mrelocation-model=default" then
         Reloc_Mode := Reloc_Default;
      elsif S = "-mno-implicit-float" then
         No_Implicit_Float := True;

      --  We support -mXXX and -mno-XXX by adding +XXX or -XXX, respectively,
      --  to the list of features.

      elsif Starts_With (S, "-mno-") then
         To_Free       := Features;
         Features      :=
           new String'(Add_Maybe_With_Comma (Features.all,
                                       "-" & Switch_Value (S, "-mno-")));
      elsif Starts_With (S, "-m") then
         To_Free       := Features;
         Features      :=
           new String'(Add_Maybe_With_Comma (Features.all,
                                             "+" & Switch_Value (S, "-m")));
      elsif S = "-O" then
            Code_Opt_Level := 1;
            Code_Gen_Level := Code_Gen_Level_Less;
      elsif Starts_With (S, "-O") then
         case S (First + 2) is
            when '1' =>
               Code_Gen_Level := Code_Gen_Level_Less;
               Code_Opt_Level := 1;
            when '2'  =>
               Code_Gen_Level := Code_Gen_Level_Default;
               Code_Opt_Level := 2;
            when '3' =>
               Code_Gen_Level := Code_Gen_Level_Aggressive;
               Code_Opt_Level := 3;
            when '0' =>
               Code_Gen_Level := Code_Gen_Level_None;
               Code_Opt_Level := 0;
            when 's' =>
               Code_Gen_Level := Code_Gen_Level_Default;
               Code_Opt_Level := 2;
               Size_Opt_Level := 1;
            when 'z' =>
               Code_Gen_Level := Code_Gen_Level_Default;
               Code_Opt_Level := 2;
               Size_Opt_Level := 2;
            when 'f' =>
               if Switch_Value (S, "-O") = "fast" then
                  Code_Gen_Level := Code_Gen_Level_Aggressive;
                  Code_Opt_Level := 3;
               end if;
            when others =>
               Early_Error ("unsupported optimization switch: " & S);
         end case;
      elsif S = "-fno-strict-aliasing" then
         No_Strict_Aliasing_Flag := True;
      elsif S = "-fc-style-aliasing" then
         C_Style_Aliasing := True;
      elsif S = "-fno-unroll-loops" then
         No_Unroll_Loops := True;
      elsif S = "-funroll-loops" then
         No_Unroll_Loops := False;
      elsif S = "-fno-vectorize" then
         No_Loop_Vectorization := True;
      elsif S = "-fvectorize" then
         No_Loop_Vectorization := False;
      elsif S = "-fno-slp-vectorize" then
         No_SLP_Vectorization := True;
      elsif S = "-fslp-vectorize" then
         No_SLP_Vectorization := False;
      elsif S = "-fno-inline" then
         No_Inlining := True;
      elsif S = "-fmerge-functions" then
         Merge_Functions := True;
      elsif S in "-fno-merge-functions" | "-fno-toplevel-reorder" then
         Merge_Functions := False;
      elsif S = "-fno-lto" then
         Prepare_For_Thin_LTO := False;
         Prepare_For_LTO      := False;
      elsif S in "-flto" | "-flto=full" then
         Prepare_For_Thin_LTO := False;
         Prepare_For_LTO      := True;
      elsif S = "-flto=thin" then
         Prepare_For_Thin_LTO  := True;
         Prepare_For_LTO       := False;
      elsif S = "-freroll-loops" then
         Reroll_Loops := True;
      elsif S = "-fno-reroll-loops" then
         Reroll_Loops := False;
      elsif S = "-fno-optimize-sibling-calls" then
         No_Tail_Calls := True;
      elsif S = "-fforce-activation-record-parameter" then
         Force_Activation_Record_Parameter := True;
      elsif S = "-fno-force-activation-record-parameter" then
         Force_Activation_Record_Parameter := False;

      --  PIC and PIE options are handled like in Clang: The last option wins;
      --  PIE implies PIC at the same level; any of the "-fno-X" options
      --  disable both PIC and PIE.

      elsif S = "-fpic" then
         PIC_PIE_Set := True;
         PIC_Level := 1;
         PIE_Level := 0;
      elsif S = "-fPIC" then
         PIC_PIE_Set := True;
         PIC_Level := 2;
         PIE_Level := 0;
      elsif S = "-fpie" then
         PIC_PIE_Set := True;
         PIC_Level := 1;
         PIE_Level := 1;
      elsif S = "-fPIE" then
         PIC_PIE_Set := True;
         PIC_Level := 2;
         PIE_Level := 2;
      elsif S in "-fno-pic" | "-fno-PIC" | "-fno-pie" | "-fno-PIE" then
         PIC_PIE_Set := True;
         PIC_Level := 0;
         PIE_Level := 0;

      elsif Starts_With (S, "-fsanitize=") then
         declare
            Sanitizers : constant String :=
              Switch_Value (S, "-fsanitize=");
            --  Comma-separated list of sanitizers.

            Current_Start : Positive := Sanitizers'First;
            Current_End   : Positive := Current_Start;

         begin
            while Current_Start < Sanitizers'Last loop

               --  Advance until the next comma or the end of the string

               for J in Current_Start + 1 .. Sanitizers'Last loop
                  if Sanitizers (J) = ',' then
                     Current_End := J - 1;
                     exit;
                  elsif J = Sanitizers'Last then
                     Current_End := J;
                  end if;
               end loop;

               --  Parse the sanitizer

               if Sanitizers (Current_Start .. Current_End) = "fuzzer" then
                  Enable_Fuzzer := True;
               elsif Sanitizers (Current_Start .. Current_End) = "address" then
                  Enable_Address_Sanitizer := True;
               else
                  Early_Error
                    ("unsupported sanitizer: " &
                     Sanitizers
                       (Current_Start .. Current_End));
               end if;

               exit when Current_End = Sanitizers'Last;
               Current_Start := Current_End + 2;
            end loop;
         end;

      elsif Starts_With (S, "-fsanitize-coverage-allowlist=") then
         declare
            Name : constant String :=
              Switch_Value (S, "-fsanitize-coverage-allowlist=");

         begin
            if not Is_Regular_File (Name) then
               Early_Error ("allow list file not found: " & Name);
            end if;

            To_Free            := San_Cov_Allow_List;
            San_Cov_Allow_List := new String'(Name);
         end;

      elsif Starts_With (S, "-fsanitize-coverage-ignorelist=") then
         declare
            Name : constant String :=
              Switch_Value (S, "-fsanitize-coverage-ignorelist=");

         begin
            if not Is_Regular_File (Name) then
               Early_Error ("ignore list file not found: " & Name);
            end if;

            To_Free             := San_Cov_Ignore_List;
            San_Cov_Ignore_List := new String'(Name);
         end;
      elsif Starts_With (S, "-fpass-plugin=") then
         To_Free := Pass_Plugin_Name;
         Pass_Plugin_Name := new String'(Switch_Value (S, "-fpass-plugin="));
      elsif Starts_With (S, "-llvm-") then
         Switches.Append (new String'(Switch_Value (S, "-llvm")));
      elsif C_Process_Switch (S) then
         null;
      end if;

      --  Free string that we replaced above, if any

      Free (To_Free);
   end Process_Switch;

   -----------------------
   -- Scan_Command_Line --
   -----------------------

   procedure Scan_Command_Line is
   begin
      --  Scan command line for relevant switches and initialize LLVM
      --  target.

      for J in 1 .. Argument_Count loop
         Process_Switch (Argument (J));
      end loop;

      --  If emitting C, change some other defaults

      if Emit_C then

         --  Building static dispatch tables causes circular references
         --  in initializers, which there's no way to handle in C.

         Building_Static_Dispatch_Tables := False;

         --  Disable 128bits support for C code generation for now

         Debug_Flag_Dot_HH := True;

         --  Don't merge functions, since this can cause references to
         --  pad fields and also generate messier C code.

         Merge_Functions := False;

         --  Use a simple 32bits target by default for C code generation

         if not Target_Triple_Set then
            Free (Target_Triple);
            Target_Triple := new String'("i386-linux");
         end if;
      end if;

      --  LLVM functions that receive triple strings usually expect them to be
      --  normalized, so let's normalize our target triple. This is especially
      --  important for Windows targets, where LLVM doesn't understand our
      --  GCC-style triples without normalization. (For example,
      --  x86_64-w64-mingw32 is misinterpreted as a non-Windows system running
      --  ELF binaries.)

      Normalized_Target_Triple :=
        new String'(Normalize_Target_Triple (Target_Triple.all));

      if Tagged_Pointers then

         --  The merge-functions pass currently can't handle architectures
         --  where the size type doesn't have pointer length.

         Merge_Functions := False;
      end if;
   end Scan_Command_Line;

   -----------------
   -- Early_Error --
   -----------------

   procedure Early_Error (S : String) is
   begin
      Write_Str ("error: ");
      Write_Line (S);
      OS_Exit (4);
   end Early_Error;

   ------------------------
   -- Get_LLVM_Error_Msg --
   ------------------------

   function Get_LLVM_Error_Msg (Msg : Ptr_Err_Msg_Type) return String is
      Err_Msg_Length : Integer := Msg'Length;
   begin
      for J in Err_Msg_Type'Range loop
         if Msg (J) = ASCII.NUL then
            Err_Msg_Length := J - 1;
            exit;
         end if;
      end loop;

      return Msg (1 .. Err_Msg_Length);
   end Get_LLVM_Error_Msg;

   ----------------------------
   -- Initialize_LLVM_Target --
   ----------------------------

   procedure Initialize_LLVM_Target is
      Num_Builtin : constant Interfaces.C.int :=
        (if Emit_C or else ABI.all = "purecap" then 4 else 3);
      type    Addr_Arr     is array (Interfaces.C.int range <>) of Address;
      subtype Switch_Addrs is Addr_Arr (1 .. Switches.Last + Num_Builtin);

      Opt1         : constant String   := "filename" & ASCII.NUL;
      Opt2         : constant String   := "-enable-shrink-wrap=0" & ASCII.NUL;
      Opt3         : constant String   :=
        "-generate-arange-section" & ASCII.NUL;
      Opt4_C       : constant String   :=
        "-disable-loop-idiom-all" & ASCII.NUL;
      Opt4_Purecap : constant String   :=
        "-cheri-landing-pad-encoding=indirect" & ASCII.NUL;
      Addrs        : Switch_Addrs      :=
        (1 => Opt1'Address, 2 => Opt2'Address, 3 => Opt3'Address,
         others => <>);
      Ptr_Err_Msg  : aliased Ptr_Err_Msg_Type;
      TT_First     : constant Integer  := Target_Triple'First;
      Success      : Boolean;

   begin
      if Emit_C and then Long_Long_Long_Size > 64 then
         Early_Error ("Long_Long_Long_Size greater than 64 not supported");
      end if;

      --  If emitting C, don't make builtins for loop idioms. If emitting
      --  for the Morello purecap ABI, use the indirect landing-pad
      --  encoding to match GCC.

      if Emit_C then
         Addrs (4) := Opt4_C'Address;
      elsif ABI.all = "purecap" then
         Addrs (4) := Opt4_Purecap'Address;
      end if;

      --  Add any LLVM parameters to the list of switches

      for J in 1 .. Switches.Last loop
         Addrs (J + Num_Builtin) := Switches.Table (J).all'Address;
      end loop;

      Parse_Command_Line_Options (Switches.Last + Num_Builtin,
                                  Addrs'Address, "");

      --  Finalize our compilation mode now that all switches are parsed

      if Emit_LLVM then
         Code_Generation := (if Output_Assembly then Write_IR else Write_BC);

      elsif Emit_C then
         Code_Generation := Write_C;

         if Output_Assembly then
            Early_Error ("cannot specify both -emit-c and -S flags");
         end if;
      elsif Output_Assembly then
         Code_Generation := Write_Assembly;
      end if;

      --  Initialize the translation environment

      Initialize_LLVM;
      Context_Set_Opaque_Pointers (Get_Global_Context, not Emit_C);
      IR_Builder     := Create_Builder;
      MD_Builder     := Create_MDBuilder;
      Module         := Module_Create_With_Name (Filename.all);
      Convert_Module := Module_Create_With_Name ("Convert_Constant");

      if Get_Target_From_Triple
          (Normalized_Target_Triple.all, LLVM_Target'Address,
           Ptr_Err_Msg'Address)
      then
         Early_Error ("cannot set target to " & Target_Triple.all & ": " &
                        Get_LLVM_Error_Msg (Ptr_Err_Msg));
      end if;

      SEH := Has_SEH (Normalized_Target_Triple.all);
      Enable_Execute_Stack :=
        Need_Enable_Execute_Stack (Normalized_Target_Triple.all);

      if not PIC_PIE_Set then
         --  Set the same target-dependent default as Clang: PIE level 2 for
         --  Linux and 64-bit Windows, and neither PIC nor PIE otherwise. (The
         --  computation of the default values in Clang is a bit more involved,
         --  see ParsePICArgs in clang/lib/Driver/ToolChains/CommonArgs.cpp,
         --  but the other cases are for targets that we don't support. For
         --  example, Clang defaults to PIE level 1 when targeting x86_64
         --  OpenBSD, and PIC level 1 on ARM/MIPS Android.)

         if Has_Default_PIE (Normalized_Target_Triple.all) then
            PIC_Level := 2;
            PIE_Level := 2;
         end if;
      end if;

      if PIC_Level /= 0 and then Reloc_Mode = Reloc_Default then
         Reloc_Mode := Reloc_PIC;
      end if;

      --  The CPU and the list of features are determined based on various
      --  settings, such as the user-specified target architecture and CPU,
      --  and target-specific defaults.

      if Starts_With (Normalized_Target_Triple.all, "nvptx") then
         --  For Nvidia GPU targets, the architecture name is also the CPU
         --  name (see tools::getCPUName in
         --  clang/lib/Driver/ToolChains/CommonArgs.cpp).
         CPU := (if Arch = null then new String'("") else Arch);
      end if;

      declare
         Arch_Features : constant String :=
           Get_Features
             (Normalized_Target_Triple.all,
              (if Arch = null then "" else Arch.all), CPU.all);
         New_Features  : String_Access;

      begin
         if Arch_Features /= "" then
            New_Features :=
              new String'(Arch_Features & "," & Features.all);
            Free (Features);
            Features := New_Features;
         end if;
      end;

      Target_Machine    :=
        Create_Target_Machine_With_ABI
          (T          => LLVM_Target,
           Triple     => Normalized_Target_Triple.all,
           CPU        => CPU.all,
           Features   => Features.all,
           ABI        => ABI.all,
           Level      => Code_Gen_Level,
           Reloc      => Reloc_Mode,
           Code_Model => Code_Model);

      Get_Target_C_Types
        (Normalized_Target_Triple.all, CPU.all, ABI.all, Features.all,
         Target_C_Types, Success);

      if not Success then
         Early_Error ("cannot get C type information from LLVM");
      end if;

      --  If a target layout was specified, use it. Otherwise, use the default
      --  layout for the specified target.

      if Target_Layout /= null then
         Module_Data_Layout := Create_Target_Data (Target_Layout.all);
      else
         Module_Data_Layout := Create_Target_Data_Layout (Target_Machine);
      end if;

      Address_Space := Get_Default_Address_Space (Module_Data_Layout);

      Set_Target             (Module, Normalized_Target_Triple.all);
      Set_Module_Data_Layout (Module, Module_Data_Layout);

      Set_Target             (Convert_Module, Normalized_Target_Triple.all);
      Set_Module_Data_Layout (Convert_Module, Module_Data_Layout);

      if PIC_Level > 0 then
         Set_Module_PIC_PIE (Module, PIC_Level, PIE_Level);
      end if;

      --  ??? Replace this by a parameter in system.ads or target.atp

      if Target_Triple (TT_First .. TT_First + 3) = "wasm" then
         Force_Activation_Record_Parameter := True;
      end if;

      --  Dump the list of supported targets if requested

      if Dump_Targets then
         Write_Str ("  Current target triple: ");
         Write_Str (Normalized_Target_Triple.all);
         Write_Eol;
         Print_Targets;
      end if;
   end Initialize_LLVM_Target;

   -------------------
   -- Generate_Code --
   -------------------

   procedure Generate_Code (GNAT_Root : N_Compilation_Unit_Id) is
      TT_First : constant Integer  := Target_Triple'First;
      Verified : Boolean           := True;
      Err_Msg  : aliased Ptr_Err_Msg_Type;

   begin
      --  We always want to write IR, even if there were errors.
      --  First verify the translation unless we're just processing
      --  for decls.

      if not Decls_Only then
         Verified :=
           not Verify_Module (Module, Print_Message_Action, Null_Address);
      end if;

      --  Unless just writing IR, suppress doing anything else if it fails
      --  or there's an error.

      if (Serious_Errors_Detected /= 0 or else not Verified)
        and then Code_Generation not in Dump_IR | Write_IR | Write_BC
      then
         Code_Generation := None;
      end if;

      --  If we're generating code or being asked to optimize IR before
      --  writing it, perform optimization. But don't do this if just
      --  generating decls.

      if not Decls_Only
        and then (Code_Generation in Write_Assembly | Write_Object | Write_C
                    or else Optimize_IR)
      then
         --  For nvptx, include the math library in a form where we can
         --  inline from it.

         if Target_Triple'Length >= 7
           and then Target_Triple (TT_First .. TT_First + 6) = "nvptx64"
         then
            declare
               Mem_Buffer    : aliased Memory_Buffer_T;
               Libdev_Module : aliased Module_T;
               Func          : Value_T;

            begin
               --  Read the file into a new module

               if Create_Memory_Buffer_With_Contents_Of_File
                 (Libdevice_Filename.all, Mem_Buffer'Address, Err_Msg'Address)
               then
                  Error_Msg_N ("could not read `" & Libdevice_Filename.all &
                               "`: " & Get_LLVM_Error_Msg (Err_Msg),
                               GNAT_Root);
               elsif Parse_Bitcode_2 (Mem_Buffer, Libdev_Module'Address) then
                  Error_Msg_N ("could not parse `" & Libdevice_Filename.all &
                               "`", GNAT_Root);
               else
                  --  Set the data layout and target triple of the math
                  --  library to agree with us and set the linkage of all
                  --  functions so that we won't retain unused ones. Also
                  --  mark each as Inline_Always.

                  Set_Data_Layout (Libdev_Module, Get_Data_Layout (Module));
                  Set_Target      (Libdev_Module, Get_Target (Module));
                  Func := Get_First_Function (Libdev_Module);
                  while Present (Func) loop
                     Set_Linkage (Func, Available_Externally_Linkage);
                     Add_Inline_Always_Attribute (Func);
                     Func := Get_Next_Function (Func);
                  end loop;

                  --  Finally, put all of the math library into our module

                  if Link_Modules_2 (Module, Libdev_Module) then
                     Error_Msg_N ("could not merge `" &
                                  Libdevice_Filename.all & "`",
                                  GNAT_Root);
                  end if;
               end if;
            end;
         end if;

         if Verified then
            if LLVM_Optimize_Module
              (Module, Target_Machine,
               Code_Opt_Level           => Code_Opt_Level,
               Size_Opt_Level           => Size_Opt_Level,
               Need_Loop_Info           => Emit_C,
               No_Unroll_Loops          => No_Unroll_Loops,
               No_Loop_Vectorization    => No_Loop_Vectorization or Emit_C,
               No_SLP_Vectorization     => No_SLP_Vectorization or Emit_C,
               Merge_Functions          => Merge_Functions,
               Prepare_For_Thin_LTO     => Prepare_For_Thin_LTO,
               Prepare_For_LTO          => Prepare_For_LTO,
               Reroll_Loops             => Reroll_Loops,
               Enable_Fuzzer            => Enable_Fuzzer,
               Enable_Address_Sanitizer => Enable_Address_Sanitizer,
               San_Cov_Allow_List       => San_Cov_Allow_List,
               San_Cov_Ignore_List      => San_Cov_Ignore_List,
               Pass_Plugin_Name         => Pass_Plugin_Name,
               Error_Message            => Err_Msg'Address)
            then
               Error_Msg_N ("could not optimize: " &
                              Get_LLVM_Error_Msg (Err_Msg),
                            GNAT_Root);
            end if;
         end if;
      end if;

      --  Output the translation

      case Code_Generation is
         when Dump_IR =>
            Dump_Module (Module);

         when Write_BC => BC : declare
            S : constant String := Output_File_Name (".bc");

         begin
            if Integer (Write_Bitcode_To_File (Module, S)) /= 0 then
               Error_Msg_N ("could not write `" & S & "`", GNAT_Root);
            end if;
         end BC;

         when Write_IR => IR : declare
            S : constant String := Output_File_Name (".ll");

         begin
            if Print_Module_To_File (Module, S, Err_Msg'Address) then
               Error_Msg_N ("could not write `" & S & "`: " &
                              Get_LLVM_Error_Msg (Err_Msg),
                            GNAT_Root);
            end if;
         end IR;

         when Write_Assembly => Assembly : declare
            S : constant String := Output_File_Name (".s");

         begin
            if Target_Machine_Emit_To_File
              (Target_Machine, Module, S, Assembly_File, Err_Msg'Address)
            then
               Error_Msg_N ("could not write `" & S & "`: " &
                              Get_LLVM_Error_Msg (Err_Msg), GNAT_Root);
            end if;
         end Assembly;

         when Write_Object => Object : declare
            S : constant String := Output_File_Name (".o");

         begin
            if Target_Machine_Emit_To_File (Target_Machine, Module, S,
                                            Object_File, Err_Msg'Address)
            then
               Error_Msg_N ("could not write `" & S & "`: " &
                              Get_LLVM_Error_Msg (Err_Msg), GNAT_Root);
            end if;
         end Object;

         when Write_C =>
            C_Generate (Module);

         when None =>
            null;
      end case;

      --  Release the environment

      if Emit_Debug_Info then
         Dispose_DI_Builder (DI_Builder);
      end if;

      Dispose_Builder (IR_Builder);
      Dispose_Module (Module);
      Dispose_Target_Data (Module_Data_Layout);
      pragma Assert (Verified);
   end Generate_Code;

   ------------------------
   -- Is_Back_End_Switch --
   ------------------------

   function Is_Back_End_Switch (Switch : String) return Boolean is
      First : constant Integer := Switch'First;
      Last  : constant Integer := Switch_Last (Switch);
      Len   : constant Integer := Last - First + 1;

      function Starts_With (S : String) return Boolean is
        (Len > S'Length and then Switch (First .. First + S'Length - 1) = S);
      --  Return True if Switch starts with S

   begin
      if not Is_Switch (Switch) then
         return False;
      elsif Switch = "--dump-ir"
        or else Switch = "--dump-bc"
        or else Switch = "--write-bc"
        or else Switch = "--dump-targets"
        or else Switch = "-S"
        or else Switch = "-g"
        or else (Starts_With ("-g") and then not Starts_With ("-gnat"))
        or else Starts_With ("--target=")
        or else Starts_With ("--layout=")
        or else Starts_With ("-llvm-")
        or else Starts_With ("-emit-")
        or else C_Is_Switch (Switch)
      then
         return True;
      end if;

      --  For now we allow the -f/-m/-W/-w, -nostdlib and -pipe switches,
      --  even though they will have no effect, though some are handled in
      --  Scan_Command_Line above. This permits compatibility with
      --  existing scripts.

      return Switch (First + 1) in 'f' | 'm' | 'W' | 'w'
        or else Switch = "-nostdlib" or else Switch = "-pipe";
   end Is_Back_End_Switch;

   ----------------------
   -- Output_File_Name --
   ----------------------

   function Output_File_Name (Extension : String) return String is
   begin
      if not Output_File_Name_Present then
         return
           Ada.Directories.Base_Name
             (Get_Name_String (Name_Id (Unit_File_Name (Main_Unit))))
           & Extension;

      --  The Output file name was specified in the -o argument

      else
         --  Locate the last dot to remove the extension of native platforms
         --  (for example, file.o).

         declare
            S : constant String := Get_Output_Object_File_Name;
         begin
            for J in reverse S'Range loop
               if S (J) = '.' then
                  return S (S'First .. J - 1) & Extension;
               end if;
            end loop;

            return S & Extension;
         end;
      end if;
   end Output_File_Name;

begin
   Initialize_GNAT_LLVM;

end GNATLLVM.Codegen;
