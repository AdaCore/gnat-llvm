------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020, AdaCore                          --
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

with Output; use Output;
with Table;

with CCG.Blocks;       use CCG.Blocks;
with CCG.Instructions; use CCG.Instructions;

package body CCG.Subprograms is

   type Decl_Idx is new Nat;
   type Stmt_Idx is new Nat;

   No_Decl_Idx : constant Decl_Idx := 0;
   No_Stmt_Idx : constant Stmt_Idx := 0;

   function Present (J : Decl_Idx) return Boolean is (J /= No_Decl_Idx);
   function Present (J : Stmt_Idx) return Boolean is (J /= No_Stmt_Idx);
   function No      (J : Decl_Idx) return Boolean is (J = No_Decl_Idx);
   function No      (J : Stmt_Idx) return Boolean is (J = No_Stmt_Idx);

   --  Tables for decls and statements

   package Decl_Table is new Table.Table
     (Table_Component_Type => Str,
      Table_Index_Type     => Decl_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 500,
      Table_Increment      => 100,
      Table_Name           => "Decl_Table");

   package Stmt_Table is new Table.Table
     (Table_Component_Type => Str,
      Table_Index_Type     => Stmt_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 1000,
      Table_Increment      => 1000,
      Table_Name           => "Stmt_Table");

   --  For each subprogram, we record the first and last decl and statement
   --  belonging to that subprogram.

   type Subprogram_Data is record
      Func       : Value_T;
      First_Decl : Decl_Idx;
      Last_Decl  : Decl_Idx;
      First_Stmt : Stmt_Idx;
      Last_Stmt  : Stmt_Idx;
   end record;

   type Subprogram_Idx is new Nat;

   package Subprogram_Table is new Table.Table
     (Table_Component_Type => Subprogram_Data,
      Table_Index_Type     => Subprogram_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 50,
      Table_Name           => "Subprogram_Table");

   function Is_Builtin_Name (S : String) return Boolean is
     (S'Length > 5 and then S (S'First .. S'First + 4) = "llvm.");

   procedure Call_Builtin (V : Value_T; S : String; Ops : Value_Array)
     with Pre => Get_Opcode (V) = Op_Call;
   --  Call V, a call to a builtin function whose name is S, with operands
   --  of Ops.

   -----------------
   -- Output_Decl --
   ----------------

   procedure Output_Decl (S : Str; Semicolon : Boolean := True) is
      SD : Subprogram_Data renames
        Subprogram_Table.Table (Subprogram_Table.Last);

   begin
      Decl_Table.Append ((if Semicolon then S & ";" else S));
      SD.Last_Decl := Decl_Table.Last;
      if No (SD.First_Decl) then
         SD.First_Decl := Decl_Table.Last;
      end if;
   end Output_Decl;

   -----------------
   -- Output_Decl --
   ----------------

   procedure Output_Decl (S : String; Semicolon : Boolean := True) is
   begin
      Output_Decl (+S, Semicolon);
   end Output_Decl;

   -----------------
   -- Output_Stmt --
   ----------------

   procedure Output_Stmt (S : Str; Semicolon : Boolean := True) is
      SD : Subprogram_Data renames
        Subprogram_Table.Table (Subprogram_Table.Last);
   begin
      Stmt_Table.Append ((if Semicolon then S & ";" else S));
      SD.Last_Stmt := Stmt_Table.Last;
      if No (SD.First_Stmt) then
         SD.First_Stmt := Stmt_Table.Last;
      end if;
   end Output_Stmt;

   -----------------
   -- Output_Stmt --
   ----------------

   procedure Output_Stmt (S : String; Semicolon : Boolean := True) is
   begin
      Output_Stmt (+S, Semicolon);
   end Output_Stmt;

   --------------------
   -- New_Subprogram --
   --------------------

   procedure New_Subprogram (V : Value_T) is
   begin
      Subprogram_Table.Append ((Func       => V,
                                First_Decl => No_Decl_Idx,
                                Last_Decl  => No_Decl_Idx,
                                First_Stmt => No_Stmt_Idx,
                                Last_Stmt  => No_Stmt_Idx));
   end New_Subprogram;

   --------------------
   -- Function_Proto --
   --------------------

   function Function_Proto
     (V : Value_T; Extern : Boolean := False) return Str
   is
      Num_Params : constant Nat    := Count_Params (V);
      Fn_Typ     : constant Type_T := Get_Element_Type (Type_Of (V));
      Ret_Typ    : constant Type_T := Get_Return_Type (Fn_Typ);
      Result     : Str             := Ret_Typ & " " & V & " (";

   begin
      --  If this is an internal subprogram, mark it as static

      if Get_Linkage (V) = Internal_Linkage then
         Result := "static " & Result;
      end if;

      --  Then output the list of parameter types, if any.  If this isn't
      --  for an extern definition, include the parameter names.

      if Num_Params = 0 then
         Result := Result & "void";
      else
         for J in 0 .. Num_Params - 1 loop
            declare
               Param : constant Value_T := Get_Param (V, J);

            begin
               Result :=
                 Result & (if J = 0 then "" else ", ") & Type_Of (Param);

               if not Extern then
                  Result := Result & " " & Param;
               end if;
            end;
         end loop;
      end if;

      return Result & ")";

   end Function_Proto;

   --------------------
   -- Function_Proto --
   --------------------

   function Function_Proto (T : Type_T; S : Str) return Str is
      Num_Params : constant Nat    := Count_Param_Types (T);
      P_Types    : Type_Array (1 .. Num_Params);
      Result     : Str             := Get_Return_Type (T) & " " & S & " (";
      First      : Boolean         := True;

   begin
      if Num_Params = 0 then
         Result := Result & "void";
      else
         Get_Param_Types (T, P_Types'Address);
         for T of P_Types loop
            begin
               Result := Result & (if First then "" else ", ") & T;
               First := False;
            end;
         end loop;
      end if;

      return Result & ")";

   end Function_Proto;

   -------------------------------
   -- Generate_C_For_Subprogram --
   -------------------------------

   procedure Generate_C_For_Subprogram (V : Value_T) is
      First_BB : constant Basic_Block_T := Get_First_Basic_Block (V);
      S        : constant String        := Get_Value_Name (V);

   begin
      --  Ignore LLVM builtin functions. The most we can have is a declaration
      --  but we don't want them to be emitted: we either handle or don't
      --  handle the builtin, but will never actually call it.

      if Is_Builtin_Name (S) then
         return;

      --  We also don't emit declarations for C functions that are
      --  defined in string.h.
      --  ??? Exactly how to get that list is far from clear, but let's
      --  approximate for now.

      elsif S = "memcpy" or else S = "memmove" or else S = "memset"
        or else S = "memcmp"
      then
         return;
      end if;

      --  Otherwise, write the definition of this function. If it has no
      --  basic blocks, it must be an extern.

      Write_Str ((if No (First_BB) then "extern " else "") &
        Function_Proto (V, Extern => True) & ";" & Eol_Str);

      --  If there is an entry basic block, start a new function and
      --  output, starting from that block.

      if Present (First_BB) then
         New_Subprogram (V);
         Set_Is_Entry (Get_Entry_Basic_Block (V));
         Output_BB (Get_Entry_Basic_Block (V));
      end if;

   end Generate_C_For_Subprogram;

   ------------------
   -- Call_Builtin --
   ------------------

   procedure Call_Builtin (V : Value_T; S : String; Ops : Value_Array) is
      Op1 : constant Value_T  :=
        (if Ops'Length >= 1 then Ops (Ops'First) else No_Value_T);
      Op2 : constant Value_T  :=
        (if Ops'Length >= 2 then Ops (Ops'First + 1) else No_Value_T);
      Op3 : constant Value_T  :=
        (if Ops'Length >= 3 then Ops (Ops'First + 2) else No_Value_T);
      pragma Unreferenced (Ops);

   begin
      --  We ignore lifetime start/end calls

      if S (S'First + 5 .. S'First + 12) = "lifetime" then
         return;

      --  We process memcpy, memmove, and memset by calling the corresponding
      --  C library function.

      elsif S (S'First + 5 .. S'First + 10) = "memcpy" then
         Output_Stmt ("memcpy (" & Op1 & ", " & Op2 & ", " & Op3 & ")");
      elsif S (S'First + 5 .. S'First + 11) = "memmove" then
         Output_Stmt ("memmove (" & Op1 & ", " & Op2 & ", " & Op3 & ")");
      elsif S (S'First + 5 .. S'First + 10) = "memset" then
         Output_Stmt ("memset (" & Op1 & ", " & Op2 & ", " & Op3 & ")");

      --  And we don't process the rest

      else
         Output_Stmt ("<unsupported builtin: " & S & ">");
      end if;
   end Call_Builtin;

   ----------------------
   -- Call_Instruction --
   ----------------------

   procedure Call_Instruction (V : Value_T; Ops : Value_Array) is
      Func  : constant Value_T := Ops (Ops'Last);
      S     : constant String  := Get_Value_Name (Func);
      Call  : Str              := Func & " (";
      First : Boolean          := True;

   begin
      --  If this is a builtin, handle that

      if Is_Builtin_Name (S) then
         Call_Builtin (V, S, Ops);
         return;
      end if;

      --  Otherwise, generate the argument list for the call

      for Op of Ops (Ops'First .. Ops'Last - 1) loop
         if First then
            Call  := Call & Op;
            First := False;
         else
            Call := Call & ", " & Op;
         end if;
      end loop;

      --  Add the final close paren. If this is a procedure call,
      --  output it. Otherwise, set this as the value of V.

      Call := (Call & ")") + Assign;
      if Get_Type_Kind (Type_Of (V)) = Void_Type_Kind then
         Output_Stmt (Call);
      else
         Assignment (V, Call);
      end if;
   end Call_Instruction;

   -----------------------
   -- Write_Subprograms --
   -----------------------

   procedure Write_Subprograms is
   begin
      for Sidx in 1 .. Subprogram_Table.Last loop
         declare
            SD : constant Subprogram_Data := Subprogram_Table.Table (Sidx);

         begin
            --  First write the prototype

            Write_Str (Function_Proto (SD.Func), Eol => True);
            Write_Str ("{");
            Write_Eol;

            --  Next write the decls, if any

            if Present (SD.First_Decl) then
               for Didx in SD.First_Decl .. SD.Last_Decl loop
                  Write_Str ("    " & Decl_Table.Table (Didx), Eol => True);
               end loop;

               Write_Eol;
            end if;

            --  Then write the statements, if any

            if Present (SD.First_Stmt) then
               for Sidx in SD.First_Stmt .. SD.Last_Stmt loop
                  Write_Str ("    " & Stmt_Table.Table (Sidx), Eol => True);
               end loop;
            end if;

            Write_Str ("}");
            Write_Eol;
         end;
      end loop;
   end Write_Subprograms;

end CCG.Subprograms;
