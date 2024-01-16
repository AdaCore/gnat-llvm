------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;

with System.OS_Lib; use System.OS_Lib;

with Osint;   use Osint;
with Osint.C; use Osint.C;
with Output;  use Output;
with Table;

with GNATLLVM.Codegen; use GNATLLVM.Codegen;

with CCG.Output; use CCG.Output;

package body CCG.Target is

   --  We have a table of parameters, which are (for now at least) either
   --  booleans or integers. We create a record type to describe each
   --  parameter.

   subtype String_Access        is GNATLLVM.String_Access;
   type    Boolean_Access       is access all Boolean;
   type    Integer_Access       is access all Integer;
   type    String_Access_Access is access all String_Access;
   type    Param_Type           is (Bool, Num, P_Str);
   subtype Str_Len              is Integer range 1 .. 25;

   type Parameter_Desc (PT : Param_Type := Num; SL : Str_Len := 25) is record
      Is_Version : Boolean;
      Name       : String (1 .. SL);
      case PT is
         when Bool =>
            Bool_Ptr : Boolean_Access;
         when Num =>
            Int_Ptr  : Integer_Access;
         when P_Str =>
            Str_Ptr  : String_Access_Access;
      end case;
   end record;

   package Parameters is new Table.Table
     (Table_Component_Type => Parameter_Desc,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Parameters");

   procedure Add_Param
     (Name       : String;
      PT         : Param_Type;
      Is_Version : Boolean              := False;
      Bool_Ptr   : Boolean_Access       := null;
      Int_Ptr    : Integer_Access       := null;
      Str_Ptr    : String_Access_Access := null)
     with Pre => (case PT is
                  when Bool => Bool_Ptr /= null and then Int_Ptr = null
                               and then Str_Ptr = null,
                  when Num => Bool_Ptr = null and then Int_Ptr /= null
                       and then Str_Ptr = null,
                  when P_Str => Bool_Ptr = null and then Int_Ptr = null
                       and then Str_Ptr /= null);
   --  Add a table entry for the specified parameter

   --  The user can specify the template to use for various declaration
   --  modifiers. We record the name and the template value in a table.
   --  The template contains the text to write for a modifier of the
   --  specified name, with the string "%d" used to represent a value
   --  in the cases where a value is needed (e.g., for alignment) or
   --  just a dollar sign if this modifier isn't supported and we shouldn't
   --  output anything. Percent_Pos is non-negative if there's a % in the
   --  string and indicates its position if so.

   type Modifier is record
      Name        : String_Access;
      Value       : String_Access;
      Percent_Pos : Integer;
   end record;

   package Modifiers is new Table.Table
     (Table_Component_Type => Modifier,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Modifiers");

   procedure Add_Modifier (Name, Value : String);
   --  Add Value as the template for modifier Name

   procedure Set_One_Parameter (PD : Parameter_Desc; Value : String);
   --  Set the parameter denoted by PD to Value

   function Compiler_To_Parameters (S : String) return String;
   --  Return a string corresponding to the changes to the target
   --  parameters for the compiler named S.

   --  We don't expect to have many sections that need declaring, so
   --  we can use a table for them and accept the quadratic behavior.

   package Sections is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Sections");

   ---------------
   -- Add_Param --
   ---------------

   procedure Add_Param
     (Name       : String;
      PT         : Param_Type;
      Is_Version : Boolean              := False;
      Bool_Ptr   : Boolean_Access       := null;
      Int_Ptr    : Integer_Access       := null;
      Str_Ptr    : String_Access_Access := null)
   is
   begin
      case PT is
         when Bool =>
            Parameters.Append ((Bool, Name'Length, Is_Version, Name,
                                Bool_Ptr));
         when Num =>
            Parameters.Append ((Num,   Name'Length, Is_Version, Name,
                                Int_Ptr));
         when P_Str =>
            Parameters.Append ((P_Str, Name'Length, Is_Version, Name,
                                Str_Ptr));
      end case;
   end Add_Param;

   ------------------
   -- Add_Modifier --
   ------------------

   procedure Add_Modifier (Name, Value : String) is
      Percent_Pos : Integer := 0;

   begin
      --  Check for duplicate entry

      for J in 1 .. Modifiers.Last loop
         if Modifiers.Table (J).Name.all = Name then
            Early_Error ("duplicate modifier '" & Name & "'");
         end if;
      end loop;

      --  See if there's a % in Value and record where it is, if so.
      --  Value'First should be 1, but if it's less than one we can have an
      --  issue with Percent_Pos and it's not worth worrying about.

      pragma Assert (Value'First >= 0);
      for J in Value'First .. Value'Last loop
         if Value (J) = '%' then
            Percent_Pos := J;
         end if;
      end loop;

      --  Now add the entry

      Modifiers.Append ((new String'(Name), new String'(Value), Percent_Pos));
   end Add_Modifier;

   ---------------------
   -- Output_Modifier --
   ---------------------

   function Output_Modifier
     (M     : String;
      Blank : OM_Blank := After;
      Val   : Int      := -1;
      S     : String   := "") return Str
   is
      Prefix : constant Str := (if Blank = Before then +" " else +"");
      Suffix : constant Str := (if Blank = After  then +" " else +"");
      Idx    : Integer      := 0;

   begin
      --  First see if we have a template specified for this modifier

      for J in 1 .. Modifiers.Last loop
         if Modifiers.Table (J).Name.all = M then
            Idx := J;
         end if;
      end loop;

      --  If we didn't find a template, we use the GCC/LLVM syntax and
      --  have different cases whether or not we have a value.

      if Idx = 0 and then Val >= 0 then
         return
           Prefix & "__attribute__ ((" & M & " (" & Nat (Val) & ")))" & Suffix;
      elsif Idx = 0 and then S /= "" then
         return
           Prefix & "__attribute__ ((" & M & " (""" & S & """)))" & Suffix;
      elsif Idx = 0 and then Val < 0 then
         return Prefix & "__attribute__ ((" & M & "))" & Suffix;

      --  If the template is a single dollar sign, that means our target
      --  doesn't support this modifier, so output nothing.

      elsif Modifiers.Table (Idx).Value.all = "$" then
         return +"";

      --  If we don't have a value or string, we just output the template

      elsif Val < 0 and then S = "" then
         return Prefix & Modifiers.Table (Idx).Value.all & Suffix;

      --  Otherwise, we see if there's a % in the string

      elsif Modifiers.Table (Idx).Percent_Pos >= 0 then
         declare
            Value : constant String  := Modifiers.Table (Idx).Value.all;
            Pos   : constant Integer := Modifiers.Table (Idx).Percent_Pos;

         begin
            return
              Prefix & Value (Value'First .. Pos - 1) &
              (if Val >= 0 then +Nat (Val) else +("""" & S & """")) &
              Value (Pos + 1 .. Value'Last) & Suffix;
         end;
      else
         return Prefix & Modifiers.Table (Idx).Value.all & Suffix;
      end if;
   end Output_Modifier;

   ---------------------------
   -- Maybe_Declare_Section --
   ---------------------------

   procedure Maybe_Declare_Section (S : String) is
   begin
      --  If we don't need to declare sections, do nothing

      if Declare_Section_Modifier.all = "$" then
         return;
      end if;

      --  If it's already been declared, do nothing

      for J in 1 .. Sections.Last loop
         if Sections.Table (J).all = S then
            return;
         end if;
      end loop;

      --  Otherwise, add it to the table and declare it

      Sections.Append (new String'(S));
      Output_Decl (Output_Modifier (Declare_Section_Modifier.all,
                                    Blank => None,
                                    S     => S),
                   Is_Typedef => True, Semicolon => False);
   end Maybe_Declare_Section;

   ---------------------
   -- Set_One_Parameter --
   ---------------------

   procedure Set_One_Parameter (PD : Parameter_Desc; Value : String) is
   begin
      case PD.PT is
         when Bool =>
            if Value in "True" | "true" then
               PD.Bool_Ptr.all := True;
            elsif Value in "False" | "false" then
               PD.Bool_Ptr.all := False;
            else
               Early_Error ("illegal value '" & Value & "' for parameter '" &
                            PD.Name & "'");
            end if;

         when Num =>

            --  Make a block here to catch an exception

            declare
               Value_First : Integer := Value'First;
               Int_Val     : Integer;

            begin

               --  For "version", we allow a leading "C", which we ignore

               if PD.Is_Version and then Value (Value_First) in 'c' | 'C' then
                  Value_First := Value_First + 1;
               end if;

               Int_Val := Integer'Value (Value (Value_First .. Value'Last));

               --  For "version", handle either full or partial year

               if PD.Is_Version then
                  if Int_Val < 80 then
                     Int_Val := Int_Val + 2000;
                  elsif Int_Val < 1900 then
                     Int_Val := Int_Val + 1900;
                  end if;
               end if;

               PD.Int_Ptr.all := Int_Val;

            exception
               when Constraint_Error =>
                  Early_Error ("illegal value '" & Value &
                               "' for parameter '" & PD.Name & "'");
            end;

         when P_Str =>
            Free (PD.Str_Ptr.all);
            PD.Str_Ptr.all := new String'(To_Lower (Value));
      end case;
   end Set_One_Parameter;

   ----------------------
   --  Set_C_Parameter --
   ----------------------

   procedure Set_C_Parameter (S : String) is
      Equal_Pos : Integer := S'First - 1;

   begin
      for J in S'Range loop
         if S (J) = '=' then
            Equal_Pos := J;
         end if;
      end loop;

      if Equal_Pos = S'First - 1 then
         Early_Error ("missing equal sign in parameter specification");
      elsif Equal_Pos = S'Last then
         Early_Error ("missing value in parameter specification");
      elsif Starts_With (S (S'First .. Equal_Pos - 1), "modifier-") then
         Add_Modifier (Switch_Value (S (S'First .. Equal_Pos - 1),
                                     "modifier-"),
                       S (Equal_Pos + 1 .. S'Last));
      else
         for J in 1 .. Parameters.Last loop
            if S (S'First .. Equal_Pos - 1) = Parameters.Table (J).Name then
               Set_One_Parameter (Parameters.Table (J),
                                  S (Equal_Pos + 1 .. S'Last));
               return;
            end if;
         end loop;

         Early_Error
           ("unknown C parameter '" & S (S'First .. Equal_Pos - 1) & "'");
      end if;

   end Set_C_Parameter;

   -----------------------
   -- Read_C_Parameters --
   -----------------------

   procedure Read_C_Parameters (Name : String) is
      Desc        : constant File_Descriptor := Open_Read (Name, Text);
      F_Length    : constant Integer         :=
        Integer ((if Desc = Invalid_FD then 0 else File_Length (Desc)));
      Length      : Natural;
      Line_Ptr    : Natural                  := 1;
      N           : Natural                  := 1;
      Buffer      : aliased String (1 .. F_Length);

   begin
      --  ??? We probably need to do something to handle CR for Windows

      if Desc = Invalid_FD then
         Early_Error ("cannot read file " & Name);
      end if;

      Length := Read (Desc, Buffer'Address, F_Length);
      Close (Desc);

      --  Now scan the file, looking for ends of lines. Ignore blank lines.

      while N <= Length loop
         if Buffer (N) = ASCII.LF then
            if Line_Ptr /= N then
               Set_C_Parameter (Buffer (Line_Ptr .. N - 1));
            end if;

            Line_Ptr := N + 1;
         end if;

         N := N + 1;
      end loop;

      --  If the last character isn't a newline, the file is misformed

      if Length > 0 and then Buffer (Length) /= ASCII.LF then
         Early_Error (Name & " doesn't end with a newline");
      end if;

   end Read_C_Parameters;

   ----------------------------
   -- Compiler_To_Parameters --
   ----------------------------

   function Compiler_To_Parameters (S : String) return String is
   begin
      if To_Lower (S) = "gcc" then
         return "";
      elsif To_Lower (S) = "clang" then
         return "inline-always-must=false";
      elsif To_Lower (S) = "msvc" then
         return
           "packed-mechanism=pragma;" &
           "modifier-section=__declspec(allocate(%));" &
           "code-section-modifier=code-seg;" &
           "modifier-code-seg=__declspec(code_seg(%));" &
           "declare-section-modifier=decl_sect;" &
           "modifier-decl_sect=#pragma section(%);" &
           "modifier-always_inline=$;" &
           "modifier-noreturn=__declspec(noreturn);" &
           "modifier-aligned=__declspec(align(%));";
      elsif To_Lower (S) = "generic" then
         return
           "packed-mechanism=none;" &
           "modifier-section=$;" &
           "modifier-always_inline=$;" &
           "modifier-noreturn=$;" &
           "modifier-aligned=$;";
      else
         Early_Error ("unsupported C compiler: " & S);
         return "";
      end if;

   end Compiler_To_Parameters;

   --------------------
   -- Set_C_Compiler --
   --------------------

   procedure Set_C_Compiler (S : String) is
      Parameter_List : constant String := Compiler_To_Parameters (S);
      N              : Natural         := Parameter_List'First;
      Line_Ptr       : Natural         := N;

   begin
      --  Scan the string of parameters, looking for semicolons

      while N < Parameter_List'Last loop
         if Parameter_List (N) = ';' then
            Set_C_Parameter (Parameter_List (Line_Ptr .. N - 1));
            Line_Ptr := N + 1;
         end if;

         N := N + 1;
      end loop;

      --  If there's something left, that's the last parameter to set

      if Line_Ptr < Parameter_List'Last then
         Set_C_Parameter (Parameter_List (Line_Ptr .. Parameter_List'Last));
      end if;

   end Set_C_Compiler;

   -------------------------
   -- Output_C_Parameters --
   -------------------------

   procedure Output_C_Parameters is
   begin
      Push_Output;
      if C_Parameter_File /= null then
         Namet.Unlock;
         Create_List_File (C_Parameter_File.all);
         Set_Output (Output_FD);
         Namet.Lock;
      end if;

      for J in 1 .. Parameters.Last loop
         declare
            PD : constant Parameter_Desc := Parameters.Table (J);

         begin
            Write_Str (PD.Name);
            Write_Str ("=");
            case PD.PT is
               when Bool =>
                  Write_Str ((if PD.Bool_Ptr.all then "True" else "False"));
               when Num =>
                  Write_Int (Int (PD.Int_Ptr.all));
               when P_Str =>
                  Write_Str (PD.Str_Ptr.all.all);
            end case;

            Write_Eol;
         end;
      end loop;

      for J in 1 .. Modifiers.Last loop
         Write_Str ("modifier-");
         Write_Str (Modifiers.Table (J).Name.all);
         Write_Str ("=");
         Write_Str (Modifiers.Table (J).Value.all);
         Write_Eol;
      end loop;

      Pop_Output;
   end Output_C_Parameters;

begin
   Add_Param ("version",            Num,   Int_Ptr  => C_Version'Access,
              Is_Version => True);
   Add_Param ("indent",             Num,   Int_Ptr  => C_Indent'Access);
   Add_Param ("max-depth",          Num,   Int_Ptr  => Max_Depth'Access);
   Add_Param ("always-brace",       Bool,  Bool_Ptr => Always_Brace'Access);
   Add_Param ("parens",             P_Str, Str_Ptr  => Parens'Access);
   Add_Param ("have-includes",      Bool,  Bool_Ptr => Have_Includes'Access);
   Add_Param ("inline-always-must", Bool,
              Bool_Ptr => Inline_Always_Must'Access);

   Add_Param ("code-section-modifier",    P_Str,
              Str_Ptr => Code_Section_Modifier'Access);
   Add_Param ("declare-section-modifier", P_Str,
              Str_Ptr => Declare_Section_Modifier'Access);
   Add_Param ("packed-mechanism",         P_Str,
              Str_Ptr => Packed_Mechanism'Access);
end CCG.Target;
