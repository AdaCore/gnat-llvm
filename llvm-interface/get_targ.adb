------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G E T _ T A R G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
------------------------------------------------------------------------------

--  This is the LLVM version of this package
--  ??? Will need to replace hardcoded values by target specific information
--  coming from the LLVM backend for the relevant target.

with Output; use Output;
with Table;

with System;           use System;
with System.OS_Lib;    use System.OS_Lib;
with Interfaces.C;
with Ada.Command_Line; use Ada.Command_Line;

with LLVM.Core;           use LLVM.Core;
with LLVM.Support;        use LLVM.Support;
with LLVM.Target_Machine; use LLVM.Target_Machine;

with GNATLLVM;         use GNATLLVM;
with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

package body Get_Targ is

   Filename : String_Access := new String'("");

   type Pstring is access String;

   package Switch_Table is new Table.Table
     (Table_Component_Type => Pstring,
      Table_Index_Type     => Interfaces.C.int,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 1,
      Table_Name           => "Switch_Table");

   procedure Initialize_LLVM_Target;
   --  Initialize all the data structures specific to the LLVM target code
   --  generation.

   -----------------------
   -- Get_Bits_Per_Unit --
   -----------------------

   function Get_Bits_Per_Unit return Pos is
   begin
      return 8;
   end Get_Bits_Per_Unit;

   -----------------------
   -- Get_Bits_Per_Word --
   -----------------------

   function Get_Bits_Per_Word return Pos is
   begin
      return 32;
   end Get_Bits_Per_Word;

   -------------------
   -- Get_Char_Size --
   -------------------

   function Get_Char_Size return Pos is
   begin
      return 8;
   end Get_Char_Size;

   ----------------------
   -- Get_Wchar_T_Size --
   ----------------------

   function Get_Wchar_T_Size return Pos is
   begin
      return 16;
   end Get_Wchar_T_Size;

   --------------------
   -- Get_Short_Size --
   --------------------

   function Get_Short_Size return Pos is
   begin
      return 16;
   end Get_Short_Size;

   ------------------
   -- Get_Int_Size --
   ------------------

   function Get_Int_Size return Pos is
   begin
      return 32;
   end Get_Int_Size;

   -------------------
   -- Get_Long_Size --
   -------------------

   function Get_Long_Size return Pos is
   begin
      return 64;
   end Get_Long_Size;

   ------------------------
   -- Get_Long_Long_Size --
   ------------------------

   function Get_Long_Long_Size return Pos is
   begin
      return 64;
   end Get_Long_Long_Size;

   ----------------------
   -- Get_Pointer_Size --
   ----------------------

   function Get_Pointer_Size return Pos is
   begin
      return 64;
   end Get_Pointer_Size;

   ---------------------------
   -- Get_Maximum_Alignment --
   ---------------------------

   function Get_Maximum_Alignment return Pos is
   begin
      return 8;
   end Get_Maximum_Alignment;

   ------------------------------------
   -- Get_System_Allocator_Alignment --
   ------------------------------------

   function Get_System_Allocator_Alignment return Nat is
   begin
      return 8;
   end Get_System_Allocator_Alignment;

   ------------------------
   -- Get_Float_Words_BE --
   ------------------------

   function Get_Float_Words_BE return Nat is
   begin
      return 0;
   end Get_Float_Words_BE;

   ------------------
   -- Get_Words_BE --
   ------------------

   function Get_Words_BE return Nat is
   begin
      return 0;
   end Get_Words_BE;

   ------------------
   -- Get_Bytes_BE --
   ------------------

   function Get_Bytes_BE return Nat is
   begin
      return 0;
   end Get_Bytes_BE;

   -----------------
   -- Get_Bits_BE --
   -----------------

   function Get_Bits_BE return Nat is
   begin
      return 0;
   end Get_Bits_BE;

   ---------------------
   -- Get_Short_Enums --
   ---------------------

   function Get_Short_Enums return Int is
   begin
      return 0;
   end Get_Short_Enums;

   --------------------------
   -- Get_Strict_Alignment --
   --------------------------

   function Get_Strict_Alignment return Nat is
   begin
      return 0;
   end Get_Strict_Alignment;

   --------------------------------
   -- Get_Double_Float_Alignment --
   --------------------------------

   function Get_Double_Float_Alignment return Nat is
   begin
      return 0;
   end Get_Double_Float_Alignment;

   ---------------------------------
   -- Get_Double_Scalar_Alignment --
   ---------------------------------

   function Get_Double_Scalar_Alignment return Nat is
   begin
      return 0;
   end Get_Double_Scalar_Alignment;

   -----------------------------
   -- Get_Max_Unaligned_Field --
   -----------------------------

   function Get_Max_Unaligned_Field return Pos is
   begin
      return 64;  -- Can be different on some targets (e.g., AAMP)
   end Get_Max_Unaligned_Field;

   ----------------------
   -- Digits_From_Size --
   ----------------------

   function Digits_From_Size (Size : Pos) return Pos is
   begin
      case Size is
         when  32    => return  6;
         when  48    => return  9;
         when  64    => return 15;
         when  96    => return 18;
         when 128    => return 18;
         when others => raise Program_Error;
      end case;
   end Digits_From_Size;

   -----------------------------
   -- Register_Back_End_Types --
   -----------------------------

   procedure Register_Back_End_Types (Call_Back : Register_Type_Proc) is
      Float_Str  : C_String := (others => ASCII.NUL);
      Double_Str : C_String := (others => ASCII.NUL);

   begin
      Float_Str (Float_Str'First .. Float_Str'First + 4) := "float";
      Call_Back
        (C_Name => Float_Str, Digs => 6, Complex => False, Count  => 0,
         Float_Rep => IEEE_Binary,
         Precision => 32, Size => 32, Alignment => 32);

      Double_Str (Double_Str'First .. Double_Str'First + 5) := "double";
      Call_Back
        (C_Name    => Double_Str,
         Digs      => 15,
         Complex   => False,
         Count     => 0,
         Float_Rep => IEEE_Binary,
         Precision => 64,
         Size      => 64,
         Alignment => 64);
   end Register_Back_End_Types;

   ---------------------
   -- Width_From_Size --
   ---------------------

   function Width_From_Size  (Size : Pos) return Pos is
   begin
      case Size is
         when  8     => return  4;
         when 16     => return  6;
         when 32     => return 11;
         when 64     => return 21;
         when others => raise Program_Error;
      end case;
   end Width_From_Size;

   ----------------------------
   -- Initialize_LLVM_Target --
   ----------------------------

   procedure Initialize_LLVM_Target is
      use Interfaces.C;

      Num_Builtin : constant := 2;

      type    Addr_Arr     is array (Interfaces.C.int range <>) of Address;
      subtype Switch_Addrs is Addr_Arr (1 .. Switch_Table.Last + Num_Builtin);

      Opt0        : constant String   := "filename" & ASCII.NUL;
      Opt1        : constant String   := "-enable-shrink-wrap=0" & ASCII.NUL;
      Addrs       : Switch_Addrs      :=
        (1 => Opt0'Address, 2 => Opt1'Address, others => <>);
      Ptr_Err_Msg : aliased Ptr_Err_Msg_Type;

   begin
      --  Add any LLVM parameters to the list of switches

      for J in 1 .. Switch_Table.Last loop
         Addrs (J + Num_Builtin) := Switch_Table.Table (J).all'Address;
      end loop;

      Parse_Command_Line_Options (Switch_Table.Last + Num_Builtin,
                                  Addrs'Address, "");

      --  Finalize our compilation mode now that all switches are parsed

      if Emit_LLVM then
         Code_Generation := (if Output_Assembly then Write_IR else Write_BC);
      elsif Output_Assembly then
         Code_Generation := Write_Assembly;
      end if;

      --  Initialize the translation environment

      Initialize_LLVM;
      Context    := Get_Global_Context;
      IR_Builder := Create_Builder_In_Context (Context);
      MD_Builder := Create_MDBuilder_In_Context (Context);
      Module     := Module_Create_With_Name_In_Context (Filename.all, Context);

      if Get_Target_From_Triple
        (Target_Triple.all, LLVM_Target'Address, Ptr_Err_Msg'Address)
      then
         Write_Str
           ("cannot set target to " & Target_Triple.all & ": " &
            Get_LLVM_Error_Msg (Ptr_Err_Msg));
         Write_Eol;
         OS_Exit (4);
      end if;

      Target_Machine    :=
        Create_Target_Machine (T          => LLVM_Target,
                               Triple     => Target_Triple.all,
                               CPU        => "generic",
                               Features   => "",
                               Level      => Code_Gen_Level,
                               Reloc      => Reloc_Default,
                               Code_Model => Code_Model_Default);

      Module_Data_Layout := Create_Target_Data_Layout (Target_Machine);
      TBAA_Root          := Create_TBAA_Root (MD_Builder);
      Set_Target       (Module, Target_Triple.all);
   end Initialize_LLVM_Target;

   ------------------------------
   -- Get_Back_End_Config_File --
   ------------------------------

   First_Call : Boolean := True;

   function Get_Back_End_Config_File return String_Ptr is
      Compile_Only : Boolean := False;
   begin
      if First_Call then
         First_Call := False;

         --  Scan command line for relevant switches and initialize LLVM
         --  target if -c/-S was specified.

         for J in 1 .. Argument_Count loop
            declare
               Switch : constant String := Argument (J);
            begin
               if Switch'Length > 0
                 and then Switch (1) /= '-'
               then
                  if Is_Regular_File (Switch) then
                     Free (Filename);
                     Filename := new String'(Switch);
                  end if;

               elsif Switch = "-c" or else Switch = "-S" then
                  Compile_Only := True;
               elsif Switch = "--dump-ir" then
                  Code_Generation := Dump_IR;
               elsif Switch = "--dump-bc" or else Switch = "--write-bc" then
                  Code_Generation := Write_BC;
               elsif Switch = "-emit-llvm" then
                  Emit_LLVM := True;
               elsif Switch = "-S" then
                  Output_Assembly := True;
               elsif Switch = "-g" then
                  Emit_Debug_Info := True;
               elsif Switch = "-fstack-check" then
                  Do_Stack_Check := True;
               elsif Switch'Length > 9
                 and then Switch (1 .. 9) = "--target="
               then
                  Target_Triple :=
                    new String'(Switch (10 .. Switch'Last));
               elsif Switch'Length > 1
                 and then Switch (1 .. 2) = "-O"
               then
                  if Switch'Length = 2 then
                     Code_Gen_Level := Code_Gen_Level_Less;
                  else
                     case Switch (3) is
                        when '1' =>
                           Code_Gen_Level := Code_Gen_Level_Less;
                        when '2' | 's' =>
                           Code_Gen_Level := Code_Gen_Level_Default;
                        when '3' =>
                           Code_Gen_Level := Code_Gen_Level_Aggressive;
                        when others =>
                           Code_Gen_Level := Code_Gen_Level_None;
                     end case;
                  end if;

               elsif Switch'Length > 6
                 and then Switch (1 .. 6) = "-llvm-"
               then
                  Switch_Table.Append (new String'(Switch (6 .. Switch'Last)));
               end if;
            end;
         end loop;

         if Compile_Only then
            Initialize_LLVM_Target;
         end if;
      end if;

      return null;
   end Get_Back_End_Config_File;

end Get_Targ;
