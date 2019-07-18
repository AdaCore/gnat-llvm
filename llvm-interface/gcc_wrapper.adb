------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           G C C _ W r a p p e r                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1998-2019, AdaCore                     --
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

--  Simple GCC wrapper to provide a GCC-like interface for tools such as
--  ASIS-based tools (or automated tests) which expect xxx-gcc as the Ada
--  driver.

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.OS_Lib;       use GNAT.OS_Lib;
with Gnatvsn;

procedure GCC_Wrapper is

   function Base_Name (S : String) return String;

   function Base_Name (S : String) return String is
      First : Natural := S'First;
      Last  : Natural := S'Last;
   begin
      if Index (S, ".exe") in S'Range then
         Last := Index (S, ".exe") - 1;
      end if;

      for J in reverse S'Range loop
         if S (J) = '/'
           or else S (J) = '\'
         then
            First := J + 1;
            exit;
         end if;
      end loop;

      return S (First .. Last);
   end Base_Name;

   GCC                : constant String := Command_Name;
   Args               : Argument_List (1 .. Argument_Count);
   Arg_Count          : Natural := 0;
   Status             : Boolean;
   Last               : Natural;
   Compile            : Boolean := False;
   Compile_With_Clang : Boolean := False;
   Compile_Ada        : Boolean := True;
   Verbose            : Boolean := False;
   Dash_O_Index       : Natural := 0;
   Dash_w_Index       : Natural := 0;
   Dash_Wall_Index    : Natural := 0;
   Dump_SCOs_Index    : Natural := 0;

   procedure Spawn (S : String; Args : Argument_List; Status : out Boolean);
   --  Call GNAT.OS_Lib.Spawn and take Verbose into account

   -----------
   -- Spawn --
   -----------

   procedure Spawn (S : String; Args : Argument_List; Status : out Boolean) is
   begin
      if Verbose then
         Put (S);

         for J in Args'Range loop
            Put (" " & Args (J).all);
         end loop;

         New_Line;
      end if;

      GNAT.OS_Lib.Spawn (S, Args, Status);
   end Spawn;

begin
   if Args'Last = 1 then
      declare
         Arg : constant String := Argument (1);
      begin
         if Arg = "-v" then
            Put_Line ("Target: llvm");
            Put_Line (Base_Name (GCC) & " version " &
                      Gnatvsn.Library_Version & " for GNAT " &
                      Gnatvsn.Gnat_Version_String);
            OS_Exit (0);

         elsif Arg = "-dumpversion" then

            --  Fixed bugs expect a version of the form x.y.z

            Put_Line ("1.1.1");
            OS_Exit (0);

         elsif Arg = "-dumpmachine" then

            --  ??? At some point, this should be the real target triple

            Put_Line ("llvm");
            OS_Exit (0);

         elsif Arg = "--version" then
            Put_Line (Base_Name (GCC) & " " &
                      Gnatvsn.Library_Version & " for GNAT " &
                      Gnatvsn.Gnat_Version_String);
            Put_Line ("Copyright (C) 2018 Free Software Foundation, Inc.");
            Put_Line ("This is free software; see the source for copying " &
                      "conditions.");
            Put_Line ("See your AdaCore support agreement for details of " &
                      "warranty and support.");
            Put_Line ("If you do not have a current support agreement, then " &
                      "there is absolutely");
            Put_Line ("no warranty; not even for MERCHANTABILITY or FITNESS " &
                      "FOR A PARTICULAR");
            Put_Line ("PURPOSE.");

            OS_Exit (0);
         end if;
      end;
   end if;

   for J in Args'Range loop
      declare
         Arg  : constant String := Argument (J);
      begin
         if Arg'Length > 0 and then Arg (1) /= '-' then
            if (Arg'Length > 2
                and then Arg (Arg'Last - 1 .. Arg'Last) = ".c")
              or else (Arg'Length > 3
                       and then Arg (Arg'Last - 2 .. Arg'Last) = ".cc")
              or else (Arg'Length > 4
                       and then Arg (Arg'Last - 3 .. Arg'Last) = ".cpp")
            then
               Compile_With_Clang := True;
               Compile_Ada := False;
            end if;
         end if;
      end;
   end loop;

   for J in Args'Range loop
      declare
         Arg  : constant String := Argument (J);
         Skip : Boolean := False;
      begin
         if Arg'Length > 0 and then Arg (1) /= '-' then
            null;

         elsif Arg = "-x" then
            if J < Args'Last
              and then (Argument (J + 1) = "c"
                        or else Argument (J + 1) = "c++")
            then
               Compile_With_Clang := True;
            end if;

         elsif Arg = "-c" or else Arg = "-S" then
            Compile := True;

         --  If compiling Ada, Ignore -Dxxx switches for compatibility with GCC

         elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-D"
           and then Compile_Ada
         then
            Skip := True;

         --  Recognize -fdump-scos specially

         elsif Arg = "-fdump-scos" then
            Dump_SCOs_Index := Arg_Count + 1;

         --  Recognize -o specially

         elsif Arg = "-o" then
            Dash_O_Index := Arg_Count + 1;

         --  Recognize -w specially

         elsif Arg = "-w" then
            Dash_w_Index := Arg_Count + 1;

         elsif Arg = "-Wall" then
            Dash_Wall_Index := Arg_Count + 1;

         elsif Arg = "-v" then
            Verbose := True;
            Skip := True;
         end if;

         if not Skip then
            Arg_Count := Arg_Count + 1;
            Args (Arg_Count) := new String'(Arg);
         end if;
      end;
   end loop;

   --  Replace -fdump-scos by -gnateS when compiling Ada code

   if Dump_SCOs_Index /= 0 and then Compile and then Compile_Ada then
      Args (Dump_SCOs_Index) := new String'("-gnateS");
   end if;

   --  Replace -o by -gnatO when compiling Ada code

   if Dash_O_Index /= 0 and then Compile and then Compile_Ada then
      Args (Dash_O_Index) := new String'("-gnatO");
   end if;

   --  Replace -w by -gnatws when compiling Ada code

   if Dash_w_Index /= 0 and then Compile and then Compile_Ada then
      Args (Dash_w_Index) := new String'("-gnatws");
   end if;

   --  Replace -Wall by -gnatwa when compiling Ada code

   if Dash_Wall_Index /= 0 and then Compile and then Compile_Ada then
      Args (Dash_Wall_Index) := new String'("-gnatwa");
   end if;

   if GCC'Length >= 3
     and then GCC (GCC'Last - 2 .. GCC'Last) = "gcc"
   then
      Last := GCC'Last - 3;

   elsif GCC'Length >= 7
     and then GCC (GCC'Last - 6 .. GCC'Last) = "gcc.exe"
   then
      Last := GCC'Last - 7;

   else
      Put_Line ("unexpected program name: " & GCC & ", exiting.");
      Set_Exit_Status (Failure);
      return;
   end if;

   --  Compile c/c++ files with clang

   if Compile_With_Clang then
      Spawn (Locate_Exec_On_Path ("clang").all, Args (1 .. Arg_Count), Status);

      if Status then
         Set_Exit_Status (Success);
      else
         Set_Exit_Status (Failure);
      end if;

      return;
   end if;

   if Compile then
      declare
         Compiler : constant String := GCC (GCC'First .. Last) & "gnat1";
         S        : String_Access;
      begin
         S := Locate_Exec_On_Path (Compiler);

         if S = null then
            Put_Line (Compiler & " not found");
            Set_Exit_Status (Failure);
            return;
         end if;

         --  ??? delete previous .o file

         Spawn (S.all, Args (1 .. Arg_Count), Status);
         Free (S);
      end;
   else
      declare
         Linker : constant String := GCC (GCC'First .. Last) & "ld";
         S      : String_Access;
      begin
         S := Locate_Exec_On_Path (Linker);

         if S = null then
            --  If llvm-ld is not found, default to "clang" for now
            S := Locate_Exec_On_Path ("clang");
         end if;

         if S = null then
            Put_Line (Linker & " not found");
            Set_Exit_Status (Failure);
            return;
         end if;

         Spawn (S.all, Args (1 .. Arg_Count), Status);
         Free (S);
      end;
   end if;

   if Status then
      Set_Exit_Status (Success);
   else
      Set_Exit_Status (Failure);
   end if;
end GCC_Wrapper;
