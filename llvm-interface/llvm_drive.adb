------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with Ada.Directories;
with Interfaces.C; use Interfaces.C;
with System;       use System;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with LLVM.Analysis; use LLVM.Analysis;
with LLVM.Target; use LLVM.Target;
with LLVM.Types; use LLVM.Types;
with LLVM.Bit_Writer;
with LLVM.Core;     use LLVM.Core;

with Atree;    use Atree;
with Errout;   use Errout;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Osint.C;  use Osint.C;
with Sem;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Switch;   use Switch;

with Get_Targ;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Utils;        use GNATLLVM.Utils;

package body LLVM_Drive is

   type Code_Generation_Kind is (Dump_IR, Dump_BC, Dump_Assembly, Dump_Object);

   Code_Generation : Code_Generation_Kind := Dump_Object;

   function Output_File_Name (Extension : String) return String;
   --  Return the name of the output file, using the given Extension

   ------------------
   -- GNAT_To_LLVM --
   ------------------

   procedure GNAT_To_LLVM (GNAT_Root : Node_Id) is
      Env : constant Environ :=
        new Environ_Record'(Max_Nodes => Last_Node_Id,
                            Ctx => Get_Global_Context, others => <>);
      Result : Integer;

      procedure Emit_Library_Item (U : Node_Id);
      --  Generate code for the given library item

      -----------------------
      -- Emit_Library_Item --
      -----------------------

      procedure Emit_Library_Item (U : Node_Id) is
         procedure Emit_Aux (Compilation_Unit : Node_Id);
         --  Process any pragmas and declarations preceding the unit

         --------------
         -- Emit_Aux --
         --------------

         procedure Emit_Aux (Compilation_Unit : Node_Id) is
            Prag : Node_Id;
         begin
            Prag := First (Context_Items (Compilation_Unit));
            while Present (Prag) loop
               if Nkind (Prag) = N_Pragma then
                  Emit (Env, Prag);
               end if;

               Prag := Next (Prag);
            end loop;

            Emit_List (Env, Declarations (Aux_Decls_Node (Compilation_Unit)));
         end Emit_Aux;

      begin
         --  Ignore Standard and ASCII packages

         if Sloc (U) <= Standard_Location then
            return;
         end if;

         --  Current_Unit := Get_Cunit_Unit_Number (Parent (U));
         --  Current_Source_File := Source_Index (Current_Unit);

         if In_Extended_Main_Code_Unit (U) then
            Env.In_Main_Unit := True;

            --  ??? Has_No_Elaboration_Code is supposed to be set by default
            --  on subprogram bodies, but this is apparently not the case,
            --  so force the flag here. Ditto for subprogram decls.

            if Nkind_In (U, N_Subprogram_Body, N_Subprogram_Declaration) then
               Set_Has_No_Elaboration_Code (Parent (U), True);
            end if;

            --  Process any pragmas and declarations preceding the unit

            Emit_Aux (Parent (U));

            --  Process the unit itself

            Emit (Env, U);

         else
            --  Should we instead skip these units completely, and generate
            --  referenced items on the fly???

            Env.In_Main_Unit := False;
            Emit_Aux (Parent (U));
            Emit (Env, U);
         end if;
      end Emit_Library_Item;

      procedure Walk_All_Units is
        new Sem.Walk_Library_Items (Action => Emit_Library_Item);

   begin
      pragma Assert (Nkind (GNAT_Root) = N_Compilation_Unit);

      --  Initialize the translation environment

      Env.Bld := Create_Builder_In_Context (Env.Ctx);
      Env.MDBld := Create_MDBuilder_In_Context (Env.Ctx);
      Env.TBAA_Root := Create_TBAA_Root (Env.MDBld);
      Env.Mdl := Module_Create_With_Name_In_Context
        (Get_Name (Defining_Entity (Unit (GNAT_Root))),
         Env.Ctx);
      Result := LLVM_Init_Module
        (Env.Mdl,
         Get_Name_String (Name_Id (Unit_File_Name (Main_Unit))));
      pragma Assert (Result = 0);
      Env.Module_Data_Layout := Get_Module_Data_Layout (Env.Mdl);
      Env.LLVM_Info := (others => Empty_LLVM_Info_Id);

      Set_Value (Env, Empty, No_Value_T);
      --  Do an initial Set_Value here so that the first real LLVM_Info entry
      --  isn't Empty_LLVM_Info_Id.

      declare
         Void_Ptr_Type : constant Type_T := Pointer_Type (Int_Ty (8), 0);
         Size_Type     : constant Type_T :=
           Int_Ty (Integer (Get_Targ.Get_Pointer_Size));
         C_Int_Type    : constant Type_T :=
           Int_Ty (Integer (Get_Targ.Get_Int_Size));
         Memcopy_Type  : constant Type_T := Fn_Ty
           ((Void_Ptr_Type, Void_Ptr_Type, Size_Type, Int_Ty (32), Int_Ty (1)),
            Void_Type_In_Context (Env.Ctx));
         Memset_Type   : constant Type_T := Fn_Ty
           ((Void_Ptr_Type, Int_Ty (8), Size_Type, Int_Ty (32), Int_Ty (1)),
            Void_Type_In_Context (Env.Ctx));

      begin
         --  Add malloc function to the env

         Env.Default_Alloc_Fn := Add_Function
           (Env.Mdl, "malloc",
            Fn_Ty ((1 => Size_Type), Void_Ptr_Type));

         --  Likewise for memcmp/memcpy/memmove/memset

         Env.Memory_Cmp_Fn := Add_Function
           (Env.Mdl, "memcmp",
            Fn_Ty ((Void_Ptr_Type, Void_Ptr_Type, Size_Type), C_Int_Type));

         if Get_Targ.Get_Pointer_Size = 32 then
            Env.Memory_Copy_Fn := Add_Function
              (Env.Mdl, "llvm.memcpy.p0i8.p0i8.i32", Memcopy_Type);
            Env.Memory_Move_Fn := Add_Function
              (Env.Mdl, "llvm.memmove.p0i8.p0i8.i32", Memcopy_Type);
            Env.Memory_Set_Fn := Add_Function
              (Env.Mdl, "llvm.memset.p0i8.i32", Memset_Type);
         else
            pragma Assert (Get_Targ.Get_Pointer_Size = 64);

            Env.Memory_Copy_Fn := Add_Function
              (Env.Mdl, "llvm.memcpy.p0i8.p0i8.i64", Memcopy_Type);
            Env.Memory_Move_Fn := Add_Function
              (Env.Mdl, "llvm.memmove.p0i8.p0i8.i64", Memcopy_Type);
            Env.Memory_Set_Fn := Add_Function
              (Env.Mdl, "llvm.memset.p0i8.i64", Memset_Type);
         end if;

         --  Likewise for stacksave/stackrestore

         Env.Stack_Save_Fn := Add_Function
           (Env.Mdl, "llvm.stacksave",
            Fn_Ty ((1 .. 0 => <>), Void_Ptr_Type));
         Env.Stack_Restore_Fn := Add_Function
           (Env.Mdl,
            "llvm.stackrestore",
            Fn_Ty ((1 => Void_Ptr_Type), Void_Type_In_Context (Env.Ctx)));

         --  Likewise for __gnat_last_chance_handler

         Env.LCH_Fn := Add_Function
           (Env.Mdl,
            "__gnat_last_chance_handler",
            Fn_Ty ((Void_Ptr_Type, C_Int_Type),
                   Void_Type_In_Context (Env.Ctx)));
      end;

      --  Actually translate

      Walk_All_Units;

      --  Output the translation

      if Verify_Module (Env.Mdl, Print_Message_Action, Null_Address) then
         Error_Msg_N ("the backend generated bad `LLVM` code", GNAT_Root);

      else
         case Code_Generation is
            when Dump_IR =>
               Dump_Module (Env.Mdl);
            when Dump_BC =>
               declare
                  S : constant String := Output_File_Name (".bc");
               begin
                  if LLVM.Bit_Writer.Write_Bitcode_To_File (Env.Mdl, S) /= 0
                  then
                     Error_Msg_N ("could not write `" & S & "`", GNAT_Root);
                  end if;
               end;

            when Dump_Assembly =>
               declare
                  S : constant String := Output_File_Name (".s");
               begin
                  if LLVM_Write_Module (Env.Mdl, False, S) /= 0 then
                     Error_Msg_N ("could not write `" & S & "`", GNAT_Root);
                  end if;
               end;

            when Dump_Object =>
               declare
                  S : constant String := Output_File_Name (".o");
               begin
                  if LLVM_Write_Module (Env.Mdl, True, S) /= 0 then
                     Error_Msg_N ("could not write `" & S & "`", GNAT_Root);
                  end if;
               end;
         end case;
      end if;

      --  Release the environment

      Dispose_Builder (Env.Bld);
      Dispose_Module (Env.Mdl);
   end GNAT_To_LLVM;

   ------------------------
   -- Is_Back_End_Switch --
   ------------------------

   function Is_Back_End_Switch (Switch : String) return Boolean is
      First : constant Positive := Switch'First + 1;
      Last  : constant Natural  := Switch_Last (Switch);
   begin
      if Switch = "--dump-ir" then
         Code_Generation := Dump_IR;
         return True;
      elsif Switch = "--dump-bc" then
         Code_Generation := Dump_BC;
         return True;
      elsif Switch = "-S" then
         Code_Generation := Dump_Assembly;
         return True;
      end if;

      --  For now we allow the -g/-O/-f/-m/-W/-w and -pipe switches, even
      --  though they will have no effect.
      --  This permits compatibility with existing scripts.
      --  ??? Should take into account -g and -O

      return
        Is_Switch (Switch)
          and then (Switch (First) in 'f' | 'g' | 'm' | 'O' | 'W' | 'w'
                    or else Switch (First .. Last) = "pipe");
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
         --  (for example, file.o)

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

end LLVM_Drive;
