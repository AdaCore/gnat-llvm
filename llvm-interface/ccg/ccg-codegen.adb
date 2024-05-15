------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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

with Atree;       use Atree;
with Debug;       use Debug;
with Einfo.Utils; use Einfo.Utils;
with Set_Targ;    use Set_Targ;
with Table;
with Uintp;       use Uintp;

with GNATLLVM.Codegen; use GNATLLVM.Codegen;
with GNATLLVM.Types;   use GNATLLVM.Types;
with GNATLLVM.Utils;   use GNATLLVM.Utils;
with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with CCG.Aggregates;  use CCG.Aggregates;
with CCG.Environment; use CCG.Environment;
with CCG.Helper;      use CCG.Helper;
with CCG.Output;      use CCG.Output;
with CCG.Subprograms; use CCG.Subprograms;
with CCG.Strs;        use CCG.Strs;
with CCG.Target;      use CCG.Target;
with CCG.Utils;       use CCG.Utils;
with CCG.Write;       use CCG.Write;

use CCG.Value_Sets;

package body CCG.Codegen is

   type Enum_List_Idx is new Nat;
   package Enum_List is new Table.Table
     (Table_Component_Type => E_Enumeration_Type_Id,
      Table_Index_Type     => Enum_List_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Enum_List");

   ---------------
   -- Note_Enum --
   ---------------

   procedure Note_Enum (TE : E_Enumeration_Type_Id) is
   begin
      Enum_List.Append (TE);
   end Note_Enum;

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

      --  Use data from front end unless -gnatd_w

      Use_FE_Data          := not Debug_Flag_Underscore_W;

      --  If we're to read C parameter values, do so

      if Target_Info_File /= null then
         Read_C_Parameters (Target_Info_File.all);
      end if;

      --  If we're to dump the C parameters, do so

      if Dump_C_Parameters then
         Output_C_Parameters;
      end if;

      --  Do validity testing on C target parameters

      if Packed_Mechanism.all not in "modifier" | "pragma" | "none" then
         Early_Error
           ("packed-mechanism must be 'modifier', 'pragma', or 'none'");
      elsif Parens.all not in "always" | "normal" | "warns" then
         Early_Error ("parens must be 'always', 'normal' or 'warns'");
      elsif Use_Stdint and then C_Version < 1999 then
         Early_Error ("-fuse-stdint only supported on C99 or later");
      elsif Prefer_Packed and then Packed_Mechanism.all = "none" then
         Early_Error
           ("-fprefer-packed not allowed when packed not supported by target");
      end if;

   end Initialize_Output;

   --------------
   -- Generate --
   --------------

   procedure Generate (Module : Module_T) is
      procedure Output_Enum_Decl (TE : E_Enumeration_Type_Id);
      --  Output a declaration for TE, a enumeration type

      function Must_Output_To_Header (F : Value_T) return Boolean is
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

      procedure Mark_Struct_Fields (T : Type_T)
        with Pre => Present (T);
      --  If T is a struct type, mark the types of all fields in it as
      --  used in a struct.

      procedure Mark_Structs_Used (V : Value_T)
        with Pre => Present (V);
      --  Likewise, but for the type of V

      procedure Mark_All_Structs_Used is new Walk_Object (Mark_Structs_Used);

      procedure Maybe_Decl_Func (V : Value_T)
        with Pre => Present (V);
      --  See if V is a value in a function that must be declared

      procedure Scan_For_Func_To_Decl is new Walk_Object (Maybe_Decl_Func);

      procedure Maybe_Globalize_Constant (V : Value_T)
        with Pre => Present (V);
      --  See if V is a constant that must be globalize. We only call this
      --  for inlined functions.

      procedure Scan_For_Constant_To_Globalize
        is new Walk_Object (Maybe_Globalize_Constant);

      Func      : Value_T;
      Glob      : Value_T;
      Must_Decl : Set;

      ----------------------
      -- Output_Enum_Decl --
      ----------------------

      procedure Output_Enum_Decl (TE : E_Enumeration_Type_Id) is
         TE_Name      : constant Str                 := +Get_Ext_Name (TE);
         Lit          : Opt_E_Enumeration_Literal_Id := First_Literal (TE);
         Need_Typedef : Boolean                      := False;
         Name_Part    : Str;

      begin
         --  We have three cases. If we're using C23 or later, we can
         --  write the name of the enum followed by a colon and the C
         --  type used to represent the enum.

         if C_Version >= 2023 then
            Name_Part := TE_Name & " : " & Type_Of (TE);

         --  Otherwise, if the width of the type used to represent the
         --  enum is equal to or greater than the width of an integer,
         --  C will use that type, so we can just write the enum name.

         elsif Esize (TE) >= Int_Size then
            Name_Part := TE_Name;

            --  Otherwise, don't write the enum name and instead write a
            --  typedef when we're done.

         else
            Name_Part    := +"";
            Need_Typedef := True;
         end if;

         --  Now write the declaration of the enum, including all of its
         --  values.

         Output_Decl ("enum " & Name_Part & " {",
                      Is_Typedef => True,
                      Semicolon  => False);
         while Present (Lit) loop
            Output_Decl ("    " & Get_Ext_Name (Lit) & " = " &
                           UI_Image (Enumeration_Rep (Lit)) &
                           (if Present (Next_Literal (Lit)) then "," else ""),
                         Is_Typedef => True,
                         Semicolon  => False);
            Next_Literal (Lit);
         end loop;

         Output_Decl ("}", Is_Typedef => True);

         --  If we need to write a typedef for this enum, do it now

         if Need_Typedef then
            Output_Decl ("typedef " & Type_Of (TE) & " " & TE_Name,
                         Is_Typedef => True);
         end if;

         Output_Decl ("", Is_Typedef => True, Semicolon => False);
      end Output_Enum_Decl;

      ---------------
      -- Is_Public --
      ---------------

      function Is_Public (V : Value_T) return Boolean is
        (Get_Linkage (V) not in Internal_Linkage | Private_Linkage);

      ------------------------
      -- Mark_Struct_Fields --
      ------------------------

      procedure Mark_Struct_Fields (T : Type_T) is
      begin
         if Is_Struct_Type (T) then
            declare
               Types : constant Nat := Count_Struct_Element_Types (T);

            begin
               for J in 0 .. Types - 1 loop
                  declare
                     ST : constant Type_T := Struct_Get_Type_At_Index (T, J);

                  begin
                     if not Get_Used_In_Struct (ST) then
                        Set_Used_In_Struct (ST);
                        Mark_Struct_Fields (ST);
                     end if;
                  end;
               end loop;
            end;

         elsif Is_Array_Type (T) or else Is_Pointer_Type (T) then
            Mark_Struct_Fields (Get_Element_Type (T));
         end if;
      end Mark_Struct_Fields;

      -----------------------
      -- Mark_Structs_Used --
      -----------------------

      procedure Mark_Structs_Used (V : Value_T) is
      begin
         Mark_Struct_Fields (Type_Of (V));
      end Mark_Structs_Used;

      ---------------------
      -- Maybe_Decl_Func --
      ---------------------

      procedure Maybe_Decl_Func (V : Value_T) is
      begin
         if Is_A_Function (V) then
            Include (Must_Decl, V);
         end if;
      end Maybe_Decl_Func;

      ------------------------------
      -- Maybe_Globalize_Constant --
      ------------------------------

      procedure Maybe_Globalize_Constant (V : Value_T) is
      begin
         if Is_A_Constant (V) and then not Is_Simple_Constant (V)
           and then not Is_A_Function (V) and then not Is_A_Global_Variable (V)
         then
            Set_Must_Globalize (V);
         end if;
      end Maybe_Globalize_Constant;

   begin -- Start of processing for Generate

      --  Scan all global decls and functions to mark any types that are
      --  used as the type of struct fields. For inlined functions, mark
      --  any non-simple constants.

      Glob := Get_First_Global (Module);
      while Present (Glob) loop
         Mark_All_Structs_Used (Glob);
         Glob := Get_Next_Global (Glob);
      end loop;

      Func := Get_First_Function (Module);
      while Present (Func) loop
         Mark_All_Structs_Used (Func);

         if Has_Inline_Attribute (Func)
           or else Has_Inline_Always_Attribute (Func)
         then
            Scan_For_Constant_To_Globalize (Func);
         end if;

         Func := Get_Next_Function (Func);
      end loop;

      --  Scan all public enums and add declarations for them

      for J in 1 .. Enum_List.Last loop
         Output_Enum_Decl (Enum_List.Table (J));
      end loop;

      --  If we're writing headers, scan inline-always functions to see if
      --  we need to declare any functions used by them.

      if Emit_Header then
         Func := Get_First_Function (Module);
         while Present (Func) loop
            if not Is_Declaration (Func)
              and then Must_Output_To_Header (Func)
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
                                  or else Must_Output_To_Header (Func)))
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
         if not Emit_Header or else Must_Output_To_Header (Func) then
            Output_Subprogram (Func);
         end if;

         Func := Get_Next_Function (Func);
      end loop;

      --  Output any ccg_iXX structs that we're using

      Output_IXX_Structs;

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
      elsif S = "-fuse-stdint" then
         Use_Stdint  := True;
      elsif S = "-fno-use-stdint" then
         Use_Stdint := False;
      elsif S = "-fprefer-packed" then
         Prefer_Packed := True;
      elsif S = "-fno-prefer-packed" then
         Prefer_Packed := False;
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
