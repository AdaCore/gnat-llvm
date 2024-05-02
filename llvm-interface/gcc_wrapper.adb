------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           G C C _ W r a p p e r                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1998-2023, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with GNAT.OS_Lib;             use GNAT.OS_Lib;

with LLVM.Target_Machine; use LLVM.Target_Machine;

with Gnatvsn; use Gnatvsn;

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

   type Compiler_Type is (Ada_Frontend, Bundled_Clang, External_Clang);
   --  The type of compiler that we're delegating the work to. Ada_Frontend is
   --  our Ada frontend (followed by the LLVM pipeline), Bundled_Clang is a
   --  renamed Clang that we ship for linking and assembly, and External_Clang
   --  is a Clang instance that we expect to be on the PATH, like GNAT-LLVM C.

   GCC                  : constant String := Command_Name;
   Args                 : Argument_List (1 .. Argument_Count + 1);
   Arg_Count            : Natural := 0;
   Status               : Boolean;
   Last                 : Natural;
   Compile              : Boolean := False;
   Compiler             : Compiler_Type := Ada_Frontend;
   Verbose              : Boolean := False;
   Dash_O_Index         : Natural := 0;
   Dash_w_Index         : Natural := 0;
   Dash_Wall_Index      : Natural := 0;
   Dump_SCOs_Index      : Natural := 0;
   Static_Libasan_Index : Natural := 0;
   S                    : String_Access;

   procedure Spawn (S : String; Args : Argument_List; Status : out Boolean);
   --  Call GNAT.OS_Lib.Spawn and take Verbose into account

   function Executable_Location return String;
   --  Return the name of the parent directory where the executable is stored
   --  (so if you are running "prefix"/bin/gcc, you would get "prefix").
   --  A special case is done for "bin" directories, which are skipped.
   --  The returned directory always ends up with a directory separator.

   function Locate_Exec_In_Libexec (Exec : String) return String_Access;
   --  Locate Exec in <prefix>/libexec/gnat-llvm/<arch>/bin. The function
   --  allocates memory that needs to be freed by the caller.

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

   -------------------------
   -- Executable_Location --
   -------------------------

   function Executable_Location return String is
      Exec_Name : constant String := Ada.Command_Line.Command_Name;

      function Get_Install_Dir (S : String) return String;
      --  S is the executable name preceeded by the absolute or relative path,
      --  e.g. "c:\usr\bin\gcc.exe" or "..\bin\gcc". Returns the absolute or
      --  relative directory where "bin" lies (in the example "C:\usr" or
      --  ".."). If the executable is not a "bin" directory, return "".

      function Is_Directory_Separator (C : Character) return Boolean;
      --  Return True if C is a directory separator

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (S : String) return String is
         Exec      : constant String :=
           Normalize_Pathname (S, Resolve_Links => True);
         Path_Last : Integer         := 0;

      begin
         for J in reverse Exec'Range loop
            if Is_Directory_Separator (Exec (J)) then
               Path_Last := J - 1;
               exit;
            end if;
         end loop;

         --  If we are not in a bin/ directory

         if Path_Last < Exec'First + 2
           or else To_Lower (Exec (Path_Last - 2 .. Path_Last)) /= "bin"
           or else (Path_Last - 3 >= Exec'First
                     and then
                       not Is_Directory_Separator (Exec (Path_Last - 3)))
         then
            return Exec (Exec'First .. Path_Last) & Directory_Separator;

         else
            --  Skip bin/, but keep the last directory separator

            return Exec (Exec'First .. Path_Last - 3);
         end if;
      end Get_Install_Dir;

      ----------------------------
      -- Is_Directory_Separator --
      ----------------------------

      function Is_Directory_Separator (C : Character) return Boolean is
      begin
         --  In addition to the default directory_separator allow the '/' to
         --  act as separator.

         return C = Directory_Separator or else C = '/';
      end Is_Directory_Separator;

   --  Start of processing for Executable_Location

   begin
      --  First determine if a path prefix was placed in front of the
      --  executable name.

      for J in reverse Exec_Name'Range loop
         if Is_Directory_Separator (Exec_Name (J)) then
            return Get_Install_Dir (Exec_Name);
         end if;
      end loop;

      --  If you are here, the user has typed the executable name with no
      --  directory prefix.

      declare
         Ex  : String_Access   := Locate_Exec_On_Path (Exec_Name);
         Dir : constant String := Get_Install_Dir (Ex.all);

      begin
         Free (Ex);
         return Dir;
      end;
   end Executable_Location;

   -----------------
   -- Locate_Exec --
   -----------------

   function Locate_Exec_In_Libexec (Exec : String) return String_Access is
      Suffix  : String_Access    := Get_Target_Executable_Suffix;
      Result  : constant String  :=
        Executable_Location & "libexec/gnat-llvm/" &
        Get_Default_Target_Triple & "/bin/" & Exec;
      Is_Exec : constant Boolean :=
        Is_Executable_File (Result & Suffix.all);

   begin
      Free (Suffix);
      return (if Is_Exec then new String'(Result) else null);
   end Locate_Exec_In_Libexec;

begin
   if Argument_Count = 1 then
      declare
         Arg : constant String := Argument (1);
      begin
         if Arg = "-v" then
            declare
               Version : constant String := Gnat_Version_String;
            begin
               Put_Line ("Target: llvm");

               if Version = "1.0" then
                  Put_Line (Base_Name (GCC) & " version " &
                            Gnatvsn.Library_Version);
               else
                  Put_Line (Base_Name (GCC) & " version " &
                            Gnatvsn.Library_Version & " (for GNAT " &
                            Gnatvsn.Gnat_Version_String & ")");
               end if;

               OS_Exit (0);
            end;

         elsif Arg = "-dumpversion" then

            --  Fixed bugs expect a version of the form x.y.z

            Put_Line ("1.1.1");
            OS_Exit (0);

         elsif Arg = "-dumpmachine" then

            Put_Line (Get_Default_Target_Triple);
            OS_Exit (0);

         elsif Arg = "--version" then
            declare
               Version : constant String := Gnatvsn.Gnat_Version_String;
            begin
               if Version = "1.0" then
                  Put_Line (Base_Name (GCC) & " " & Gnatvsn.Library_Version);
               else
                  Put_Line (Base_Name (GCC) & " " &
                            Gnatvsn.Library_Version & " (for GNAT " &
                            Version & ")");
               end if;
            end;

            Put_Line ("Copyright (C) 2018-" & Gnatvsn.Current_Year &
                      " Free Software Foundation, Inc.");
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

   for J in 1 .. Argument_Count loop
      declare
         Arg  : constant String := Argument (J);
      begin
         if Arg'Length > 0 and then Arg (1) /= '-' then
            if ((Arg'Length > 2
                  and then Arg (Arg'Last - 1 .. Arg'Last) = ".c")
                or else (Arg'Length > 3
                          and then Arg (Arg'Last - 2 .. Arg'Last) = ".cc")
                or else (Arg'Length > 4
                          and then Arg (Arg'Last - 3 .. Arg'Last) = ".cpp"))
              and then (J = Args'First or else Argument (J - 1) /= "-o")
            then
               Compiler := External_Clang;
            end if;
         end if;
      end;
   end loop;

   for J in 1 .. Argument_Count loop
      declare
         Arg  : constant String := Argument (J);
         Skip : Boolean := False;
      begin
         if Arg'Length > 0 and then Arg (1) /= '-' then
            null;

         elsif Arg = "-x" then
            if J < Args'Last then

               --  While we can use our embedded Clang ("llvm-helper") to
               --  compile assembly code, we need to forward C and C++ code to
               --  an externally installed Clang like GNAT-LLVM C.

               if Argument (J + 1) in "assembler" | "assembler-with-cpp" then
                  Compiler := Bundled_Clang;
               elsif Argument (J + 1) in "c" | "c++" then
                  Compiler := External_Clang;
               end if;
            end if;

         elsif Arg = "-c" or else Arg = "-S" then
            Compile := True;

         --  If compiling Ada, Ignore -Dxxx and -E switches for compatibility
         --  with GCC

         elsif Compiler = Ada_Frontend
           and then (Arg = "-E"
                     or else (Arg'Length >= 2 and then Arg (1 .. 2) = "-D"))
         then
            Skip := True;

         --  If compiling with Clang, ignore GCC switches that Clang doesn't
         --  support

         elsif (Compiler = Bundled_Clang or else Compiler = External_Clang)
           and then Arg = "-fno-tree-loop-distribute-patterns"
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

         elsif Arg = "-static-libasan" then
            Static_Libasan_Index := Arg_Count + 1;

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

   if Dump_SCOs_Index /= 0
     and then Compile
     and then Compiler = Ada_Frontend
   then
      Args (Dump_SCOs_Index) := new String'("-gnateS");
   end if;

   --  Replace -o by -gnatO when compiling Ada code

   if Dash_O_Index /= 0
     and then Compile
     and then Compiler = Ada_Frontend
   then
      Args (Dash_O_Index) := new String'("-gnatO");
   end if;

   --  Replace -w by -gnatws when compiling Ada code

   if Dash_w_Index /= 0
     and then Compile
     and then Compiler = Ada_Frontend
   then
      Args (Dash_w_Index) := new String'("-gnatws");
   end if;

   --  Replace -Wall by -gnatwa when compiling Ada code

   if Dash_Wall_Index /= 0
     and then Compile
     and then Compiler = Ada_Frontend
   then
      Args (Dash_Wall_Index) := new String'("-gnatwa");
   end if;

   --  Replace -static-libasan with the LLVM equivalent -static-libsan

   if Static_Libasan_Index /= 0 then
      Args (Static_Libasan_Index) := new String'("-static-libsan");
   end if;

   if GCC'Length >= 3
     and then GCC (GCC'Last - 2 .. GCC'Last) = "gcc"
   then
      Last := GCC'Last - 3;

   elsif GCC'Length >= 7
     and then To_Lower (GCC (GCC'Last - 6 .. GCC'Last)) = "gcc.exe"
   then
      Last := GCC'Last - 7;

   else
      Put_Line ("unexpected program name: " & GCC & ", exiting.");
      Set_Exit_Status (Failure);
      return;
   end if;

   --  Translate the environment variable ENV_PREFIX to a --sysroot
   --  argument if we're linking or compiling with Clang.

   if (Compile and then Compiler in Bundled_Clang | External_Clang)
     or else not Compile
   then
      declare
         Sysroot : String_Access := Getenv ("ENV_PREFIX");
      begin
         if Sysroot.all /= "" then
            Arg_Count        := Arg_Count + 1;
            Args (Arg_Count) := new String'("--sysroot=" & Sysroot.all);
         end if;

         Free (Sysroot);
      end;
   end if;

   --  Compile c/c++ files with clang

   if Compiler = Bundled_Clang or else Compiler = External_Clang then
      if Compiler = Bundled_Clang then
         S := Locate_Exec_In_Libexec ("llvm-helper");
      end if;

      if S = null then
         S := Locate_Exec_On_Path ("clang");
      end if;

      if S = null then
         Put_Line ("warning: clang not found, using gcc.");
         S := Locate_Exec_On_Path ("gcc");
      end if;

      Spawn (S.all, Args (1 .. Arg_Count), Status);
      Set_Exit_Status (if Status then Success else Failure);

      return;
   end if;

   if Compile then
      declare
         Compiler : constant String := GCC (GCC'First .. Last) & "gnat1";
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
         S : String_Access;
      begin
         --  We use the bundled Clang for linking by default.

         S := Locate_Exec_In_Libexec ("llvm-helper");

         if S = null then
            --  Our own Clang is not found, default to system Clang for now
            S := Locate_Exec_On_Path ("clang");
         end if;

         if S = null then
            --  Last fallback is gcc
            S := Locate_Exec_On_Path ("gcc");
         end if;

         if S = null then
            Put_Line ("clang or gcc not found: cannot link.");
            Set_Exit_Status (Failure);
            return;
         end if;

         Spawn (S.all, Args (1 .. Arg_Count), Status);
         Free (S);
      end;
   end if;

   Set_Exit_Status (if Status then Success else Failure);
end GCC_Wrapper;
