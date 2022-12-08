------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2022, AdaCore                     --
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

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Codegen; use GNATLLVM.Codegen;
with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with CCG.Helper;      use CCG.Helper;
with CCG.Output;      use CCG.Output;
with CCG.Subprograms; use CCG.Subprograms;
with CCG.Target;      use CCG.Target;
with CCG.Utils;       use CCG.Utils;
with CCG.Write;       use CCG.Write;

use CCG.Value_Sets;

package body CCG.Codegen is

   -----------------------
   -- Initialize_Output --
   -----------------------

   procedure Initialize_Output is
   begin
      --  Can't dump C parameters if not generating C

      if Dump_C_Parameters and then not Emit_C then
         Early_Error
           ("cannot specify -fdump-c-parameters unless generating C");
      end if;

      --  When emitting C, we don't want to write variable-specific debug
      --  info, just line number information. But we do want to write #line
      --  info if -g was specified. We always want to write location
      --  information into the LLVM IR specified.

      Emit_Full_Debug_Info := False;
      Emit_C_Line          := Emit_Debug_Info;
      Emit_Debug_Info      := True;

      --  If we're to read C parameter values, do so

      if Target_Info_File /= null then
         Read_C_Parameters (Target_Info_File.all);
      end if;

      --  If we're to dump the C parameters, do so

      if Dump_C_Parameters then
         Output_C_Parameters;
      end if;

   end Initialize_Output;

   --------------
   -- Generate --
   --------------

   procedure Generate (Module : Module_T) is
      function Output_To_Header (F : Value_T) return Boolean is
        (Emit_Header
         and then (case Header_Inline is
                        when None          => False,
                        when Inline_Always => Has_Inline_Always_Attribute (F),
                        when Inline        => Has_Inline_Always_Attribute (F)
                                              or else Has_Inline_Attribute
                                                        (F)))
        with Pre => Is_A_Function (F);
      --  True if we should output F to the header file

      function Is_Public (V : Value_T) return Boolean;
      --  True if V is publically-visible

      procedure Maybe_Decl_Func (V : Value_T)
        with Pre => Present (V);
      --  Called for each value in an inline function

      procedure Scan_For_Func_To_Decl is new Walk_Object (Maybe_Decl_Func);

      Func      : Value_T;
      Glob      : Value_T;
      Must_Decl : Set;

      ---------------
      -- Is_Public --
      ---------------

      function Is_Public (V : Value_T) return Boolean is
        (Get_Linkage (V) not in Internal_Linkage | Private_Linkage);

      ---------------------
      -- Maybe_Decl_Func --
      ---------------------

      procedure Maybe_Decl_Func (V : Value_T) is
      begin
         if Is_A_Function (V) then
            Include (Must_Decl, V);
         end if;
      end Maybe_Decl_Func;

   begin
      --  If we're writing headers, scan inline-always functions to see if
      --  we need to declare any functions used by them.

      if Emit_Header then
         Func := Get_First_Function (Module);
         while Present (Func) loop
            if not Is_Declaration (Func)
              and then Output_To_Header (Func)
            then
               Inlines_In_Header := True;
               Scan_For_Func_To_Decl (Func);
            end if;

            Func := Get_Next_Function (Func);
         end loop;
      end if;

      --  Declare functions first, since they may be referenced in
      --  globals. Put public functions that we define into the header file,
      --  as well as inline_always functions.

      Func := Get_First_Function (Module);
      while Present (Func) loop
         if not Emit_Header
           or else (not Is_Declaration (Func)
                    and then (Is_Public (Func)
                              or else Output_To_Header (Func)))
           or else Contains (Must_Decl, Func)
         then
            Declare_Subprogram (Func);
         end if;

         Func := Get_Next_Function (Func);
      end loop;

      --  Output declarations for all globals with initializers if writing
      --  C code and all public globals if writing a header.

      Glob := Get_First_Global (Module);
      while Present (Glob) loop
         if Present (Get_Initializer (Glob))
           and then (Is_Public (Glob) or else not Emit_Header)
         then
            Maybe_Decl (Glob);
         end if;

         Glob := Get_Next_Global (Glob);
      end loop;

      --  Process all functions, writing referenced globals and
      --  typedefs on the fly and queueing the rest for later output.
      --  Write inline_always functions to the header file.

      Func := Get_First_Function (Module);
      while Present (Func) loop
         if not Emit_Header or else Output_To_Header (Func) then
            Output_Subprogram (Func);
         end if;

         Func := Get_Next_Function (Func);
      end loop;

      --  Now that we know if we have any inline_always functions, set up
      --  for writing the desired file. Finally, write all the code we
      --  generated and finalize the writing process.

      Initialize_Writing;
      Write_C_File;
      Finalize_Writing;

   end Generate;

   --------------------
   -- Process_Switch --
   --------------------

   function Process_Switch (S : String) return Boolean is
      To_Free : String_Access    := null;

   begin
      if S = "-emit-header" then
         Emit_C      := True;
         Emit_Header := True;
         return True;
      elsif Starts_With (S, "-header-inline=") then
         if Switch_Value (S, "-header-inline=") = "none" then
            Header_Inline := None;
         elsif Switch_Value (S, "-header-inline=") = "inline-always" then
            Header_Inline := Inline_Always;
         elsif Switch_Value (S, "-header-inline=") = "inline" then
            Header_Inline := Inline;
         else
            Early_Error ("invalid -header-inline option: " &
                           Switch_Value (S, "-header-inline="));
         end if;

         return True;
      elsif S = "--dump-c-parameters" then
         Dump_C_Parameters := True;
         return True;
      elsif Starts_With (S, "--dump-c-parameters=") then
         Dump_C_Parameters := True;
         To_Free           := C_Parameter_File;
         C_Parameter_File  :=
           new String'(Switch_Value (S, "--dump-c-parameters="));
      elsif Starts_With (S, "-c-compiler=") then
         Set_C_Compiler (Switch_Value (S, "-c-compiler="));
         return True;
      elsif Starts_With (S, "-c-target-file=") then
         To_Free          := Target_Info_File;
         Target_Info_File := new String'(Switch_Value (S, "-c-target-file="));
      elsif Starts_With (S, "-c-target-") then
         Set_C_Parameter (Switch_Value (S, "-c-target-"));
         return True;
      end if;

      --  If we were to free an old string value, do so and show that
      --  we processed this parameter. Otherwise, show we didn't.

      if To_Free /= null then
         Free (To_Free);
      end if;

      return False;
   end Process_Switch;

   ---------------
   -- Is_Switch --
   ---------------

   function Is_Switch (S : String) return Boolean is
   begin
      return Starts_With    (S, "-header-inline=")
        or else Starts_With (S, "-c-target-")
        or else Starts_With (S, "-c-compiler=")
        or else S = "--dump-c-parameters"
        or else Starts_With (S, "--dump-c-parameters=");
   end Is_Switch;

end CCG.Codegen;
