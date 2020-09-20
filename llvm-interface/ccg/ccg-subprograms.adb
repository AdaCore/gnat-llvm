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

   procedure Output_BB (BB : Basic_Block_T)
     with Pre => Present (BB);
   procedure Output_BB (V : Value_T)
     with Pre => Is_A_Basic_Block (V), Inline;
   --  Generate the code for basic block unless already output

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

   ---------------
   -- Output_BB --
   ---------------

   procedure Output_BB (V : Value_T) is
   begin
      Output_BB (Value_As_Basic_Block (V));
   end Output_BB;

   ---------------
   -- Output_BB --
   ---------------

   procedure Output_BB (BB : Basic_Block_T) is
      V          : Value_T          := Get_First_Instruction (BB);
      Terminator : constant Value_T := Get_Basic_Block_Terminator (BB);

   begin
      --  If we already processed this basic block, mark that we did

      if Get_Was_Output (BB) then
         return;
      end if;

      --  Otherwise, if this isn't the entry block, output a label for it

      if not Get_Is_Entry (BB) then
         Output_Stmt (BB & ":", Semicolon => False);
      end if;

      --  Mark that we're outputing this block and process each
      --  instruction it.

      Set_Was_Output (BB);
      while Present (V) loop
         Process_Instruction (V);
         V := Get_Next_Instruction (V);
      end loop;

      --  Now process any block referenced by the terminator

      case Get_Instruction_Opcode (Terminator) is
         when Op_Ret =>
            null;

         when Op_Br =>
            if Get_Num_Operands (Terminator) = 1 then
               Output_BB (Get_Operand (Terminator, Nat (0)));
            else
               Output_BB (Get_Operand (Terminator, Nat (2)));
               Output_BB (Get_Operand (Terminator, Nat (1)));
            end if;

         when others =>
            Output_Stmt
              (+("<unsupported terminator: " &
                   Get_Opcode_Name (Terminator) & ">"));

      end case;

   end Output_BB;

   -------------------------------
   -- Generate_C_For_Subprogram --
   -------------------------------

   procedure Generate_C_For_Subprogram (V : Value_T) is
   begin
      --  If this fucntion has no basic blocks, it must be an extern, so
      --  write out the declaration.

      if No (Get_First_Basic_Block (V)) then
         Write_Str ("extern " & Function_Proto (V, Extern => True) & ";",
                    Eol => True);
      else
         --  Otherwise, convert blocks starting with the entry block.
         --  The entry block is the first one, but we don't want to rely
         --  on that here.

         declare
            Entry_BB : constant Basic_Block_T := Get_Entry_Basic_Block (V);

         begin
            New_Subprogram (V);
            Set_Is_Entry (Entry_BB);
            Output_BB (Entry_BB);
         end;
      end if;

   end Generate_C_For_Subprogram;

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
