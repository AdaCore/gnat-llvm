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

with Table;

with GNATLLVM.Types; use GNATLLVM.Types;

with CCG.Aggregates;   use CCG.Aggregates;
with CCG.Instructions; use CCG.Instructions;
with CCG.Subprograms;  use CCG.Subprograms;
with CCG.Target;       use CCG.Target;
with CCG.Utils;        use CCG.Utils;

package body CCG.Output is

   --  Tables for typedefs, and global and local decls, and statements

   package Typedefs is new Table.Table
     (Table_Component_Type => Out_Line,
      Table_Index_Type     => Typedef_Idx,
      Table_Low_Bound      => Typedef_Idx_Start,
      Table_Initial        => 100,
      Table_Increment      => 100,
      Table_Name           => "Typedefs");

   package Global_Decls is new Table.Table
     (Table_Component_Type => Out_Line,
      Table_Index_Type     => Global_Decl_Idx,
      Table_Low_Bound      => Global_Decl_Idx_Low_Bound,
      Table_Initial        => 100,
      Table_Increment      => 100,
      Table_Name           => "Global_Decls");

   package Local_Decls is new Table.Table
     (Table_Component_Type => Out_Line,
      Table_Index_Type     => Local_Decl_Idx,
      Table_Low_Bound      => Local_Decl_Idx_Low_Bound,
      Table_Initial        => 500,
      Table_Increment      => 100,
      Table_Name           => "Local_Decls");

   package Stmts is new Table.Table
     (Table_Component_Type => Out_Line,
      Table_Index_Type     => Stmt_Idx,
      Table_Low_Bound      => Stmt_Idx_Low_Bound,
      Table_Initial        => 1000,
      Table_Increment      => 1000,
      Table_Name           => "Stmts");

   Next_Block_Style : Block_Style := None;
   --  The Block_Style to use for the next line written using Output_Decl
   --  or Output_Stmt.

   procedure Maybe_Output_Typedef_And_Decl (V : Value_T)
     with Pre => Is_A_Constant (V);
   --  Ensure that we're output typedefs for any types within V and
   --  declarations for anything in V that needs it

   --------------------
   -- Output_Typedef --
   --------------------

   procedure Output_Typedef (T : Type_T; Incomplete : Boolean := False) is
   begin
      --  Show we're outputting the typedef (so we know not to do it
      --  recursively).

      Set_Are_Outputting_Typedef (T);

      --  See what type of type this is

      if Is_Struct_Type (T) then
         Output_Struct_Typedef (T, Incomplete => Incomplete);
      elsif Is_Array_Type (T) then
         Output_Array_Typedef (T);
      elsif Is_Pointer_Type (T) then

         --  We don't have typedefs for function types, just pointer to
         --  function types. But for normal pointer types, make sure we've
         --  written at least an incomplete version of the typedef for the
         --  pointed-to type. Always write the complete definition if writing
         --  a header file

         if Is_Function_Type (Get_Element_Type (T)) then
            Output_Function_Type_Typedef (T);
         else
            Maybe_Output_Typedef (Get_Element_Type (T),
                                  Incomplete => not Emit_Header);
         end if;
      end if;

      --  Show we've written the typedef unless this is a struct type and
      --  we're only writing an incomplete definition.

      Set_Are_Outputting_Typedef (T, False);

      if not Incomplete or else not Is_Struct_Type (T) then
         Set_Is_Typedef_Output   (T);
      end if;
   end Output_Typedef;

   ----------------
   -- Maybe_Decl --
   ----------------

   procedure Maybe_Decl (V : Value_T; For_Initializer : Boolean := False) is
      T   : constant Type_T :=
        (if   Is_A_Global_Variable (V) or else Is_A_Alloca_Inst (V)
         then Get_Element_Type (Type_Of (V)) else Type_Of (V));

   begin
      --  If this is metadata, we do nothing. This can occur for some
      --  builtins, but we don't process those.

      if Is_Metadata (V) then
         return;

      --  Be sure that we've output a typedef for V's type. But don't do this
      --  for a function since the reference to the function doesn't require
      --  us to declare its type separately.

      elsif not Is_A_Function (V) then
         Maybe_Output_Typedef (T);
      end if;

      --  If this is an unprocessed constant expression, process it as an
      --  instruction

      if Is_A_Constant_Expr (V) and then No (Get_C_Value (V)) then
         Process_Instruction (V);
      end if;

      --  If we've already processed this or if it's a simple constant
      --  (any constant if this is for an initializer), we don't need
      --  to do anything unless it's a global variable, which always
      --  has to be declared.

      if Get_Is_Decl_Output (V)
         or else (not Is_A_Global_Variable (V)
                  and then (Is_Simple_Constant (V)
                            or else (For_Initializer
                                     and then Is_A_Constant (V))))
      then
         return;
      end if;

      --  We need to write a declaration for this if we haven't already
      --  assigned a value to it, it's not a function, an argument, a basic
      --  block or simple undef.

      if No (Get_C_Value (V)) and then not Is_A_Function (V)
        and then not Is_A_Argument (V) and then not Is_A_Basic_Block (V)
        and then not (Is_Undef (V) and then Is_Simple_Type (V))
      then
         Set_Is_Decl_Output (V);

         --  We ensure in GNAT LLVM that we never have a variable of zero
         --  size, but the optimizer might create one for us if it
         --  determines than an alloca has a count that's known to be zero.
         --  If we're already planning to write this as an array of 1, we
         --  don't need to do anything. Otherwise kludge this case by
         --  making it an array of size 1.

         if Is_Zero_Length_Array (T) and then Effective_Array_Length (T) = 0
         then
            Output_Decl (Get_Element_Type (T) & " " & (V + LHS) & "[1]");
            return;

         --  If this is a global, mark it as an LHS

         elsif Is_A_Global_Variable (V) then
            Set_Is_LHS (V);
         end if;

         declare
            Decl : Str :=
              (V + (+Write_Type or +LHS)) & " " &
               (if Is_Volatile (V) then "volatile " else "") & (V + LHS);

         begin
            --  If this is a global variable or alloca, see if the
            --  specified alignment is more than the default alignment for
            --  the variable's type.

            --  ??? We'd like to refine this later by understanding the
            --  preferred alignment rules of the user's compiler.

            if Is_A_Global_Variable (V) or else Is_A_Alloca_Inst (V) then
               declare
                  Align  : constant Nat :=
                    To_Bits (Nat (Get_Alignment (V)));
                  Actual : constant Nat := Actual_Alignment (T);

               begin
                  if Align > Actual then
                     Decl :=
                       Output_Modifier ("aligned", Val => To_Bytes (Align)) &
                       Decl;
                  end if;
               end;
            end if;

            --  If this is a global variable, see if it has a linker
            --  section and write it and declare the section if so.

            if Is_A_Global_Variable (V) and then Get_Section (V) /= "" then
               Maybe_Declare_Section (Get_Section (V));
               Decl :=
                 Output_Modifier ("section", S => Get_Section (V)) & Decl;
            end if;

            --  For globals, we write the decl immediately. Otherwise, it's
            --  part of the decls for the subprogram.  Figure out whether this
            --  is static or extern. It's extern if there's no initializer.

            if Is_A_Global_Variable (V) then
               declare
                  Init : constant Value_T := Get_Initializer (V);

               begin
                  if No (Init) or else Emit_Header then
                     Decl := "extern " & Decl;
                  elsif Get_Linkage (V) in Internal_Linkage | Private_Linkage
                  then
                     Decl := "static " & Decl;
                  end if;

                  --  If this is a constant, denote that

                  if Is_Global_Constant (V) then
                     Decl := "const " & Decl;
                     Set_Is_Constant (V);
                  end if;

                  --  Don't write an initializer if it's undef or a
                  --  zeroinitializer. In the latter case, it means to apply
                  --  the default initialization, which is defined by the
                  --  C standard as being all zeros (hence the name).

                  if not Emit_Header and then Present (Init)
                    and then not Is_Undef (Init)
                    and then not Is_A_Constant_Aggregate_Zero (Init)
                  then
                     Maybe_Output_Typedef_And_Decl (Init);
                     Decl := Decl & " = " & (Init + Initializer);
                  end if;

                  Output_Decl (Decl, Is_Global => True, V => V);
               end;

            --  If this is a constant (we know that it can't be a simple
            --  constant), we need to initialize the value to that of the
            --  constant and put it at the top level.

            elsif Is_A_Constant (V) then
               Maybe_Output_Typedef_And_Decl (V);
               Output_Decl ((if   Get_Must_Globalize (V) then "const "
                             else "static const ") &
                            Decl & " = " & (V + Initializer),
                            Is_Global => True, V => V);
               Set_Is_Constant (V);
            else
               Output_Decl (Decl, V => V);
            end if;
         end;
      end if;
   end Maybe_Decl;

   -----------------------------------
   -- Maybe_Output_Typedef_And_Decl --
   -----------------------------------

   procedure Maybe_Output_Typedef_And_Decl (V : Value_T) is
   begin
      --  First, ensure that V has been declared

      Maybe_Decl (V, For_Initializer => True);

      --  Next, we'll take care of typedefs for V's type and for types
      --  recursively within it, but we also have to be concerned about
      --  constant expressions, which may reference objects that could be
      --  of types for which we haven't yet written a typedef.

      Maybe_Output_Typedef (Type_Of (V));

      if Is_A_Constant_Array (V) or else Is_A_Constant_Struct (V)
        or else Is_A_Constant_Expr (V)
      then
         for J in 0 .. Nat'(Get_Num_Operands (V)) - 1 loop
            Maybe_Output_Typedef_And_Decl (Get_Operand (V, J));
         end loop;

      elsif Is_A_Constant_Data_Array (V) then
         for J in 0 .. Nat'(Get_Num_CDA_Elements (V)) - 1 loop
            Maybe_Output_Typedef_And_Decl (Get_Element_As_Constant (V, J));
         end loop;
      end if;
   end Maybe_Output_Typedef_And_Decl;

   -----------------
   -- Output_Decl --
   ----------------

   procedure Output_Decl
     (S             : Str;
      Semicolon     : Boolean      := True;
      Is_Typedef    : Boolean      := False;
      Is_Global     : Boolean      := False;
      Start_Block   : Block_Style  := None;
      End_Block     : Block_Style  := None;
      Indent_Type   : Indent_Style := Normal;
      V             : Value_T      := No_Value_T;
      No_Debug_Info : Boolean      := False)
   is
      OL : constant Out_Line :=
        (Line_Text      => (if Semicolon then S & ";" else S),
         Start_Block    =>
          (if Present (Start_Block) then Start_Block else Next_Block_Style),
         Indent_Type    => Indent_Type,
         End_Block      => End_Block,
         V              => V,
         No_Debug_Info  => No_Debug_Info);

   begin
      Next_Block_Style := None;

      if Is_Typedef then
         Typedefs.Append (OL);
      elsif Is_Global then
         Global_Decls.Append (OL);
      else
         Local_Decls.Append (OL);
         Add_Decl_Line (Local_Decls.Last);
      end if;
   end Output_Decl;

   -----------------
   -- Output_Decl --
   ----------------

   procedure Output_Decl
     (S             : String;
      Semicolon     : Boolean      := True;
      Is_Typedef    : Boolean      := False;
      Is_Global     : Boolean      := False;
      Start_Block   : Block_Style  := None;
      End_Block     : Block_Style  := None;
      Indent_Type   : Indent_Style := Normal;
      V             : Value_T      := No_Value_T;
      No_Debug_Info : Boolean      := False)
   is
   begin
      Output_Decl (+S, Semicolon, Is_Typedef, Is_Global, Start_Block,
                   End_Block, Indent_Type, V, No_Debug_Info);
   end Output_Decl;

   -----------------
   -- Output_Stmt --
   ----------------

   procedure Output_Stmt
     (S             : Str;
      Semicolon     : Boolean       := True;
      Indent_Type   : Indent_Style  := Normal;
      V             : Value_T       := No_Value_T;
      No_Debug_Info : Boolean       := False)
   is
   begin
      --  Add the statement to the current subprogram

      Stmts.Append ((Line_Text      =>
                       (if    No (S) then No_Str
                        elsif Semicolon then S & ";" else S),
                     Start_Block    => Next_Block_Style,
                     End_Block      => None,
                     Indent_Type    => Indent_Type,
                     V              => V,
                     No_Debug_Info  => No_Debug_Info));
      Add_Stmt_Line (Stmts.Last);
      Next_Block_Style := None;

   end Output_Stmt;

   -----------------
   -- Output_Stmt --
   ----------------

   procedure Output_Stmt
     (S             : String;
      Semicolon     : Boolean       := True;
      Indent_Type   : Indent_Style  := Normal;
      V             : Value_T       := No_Value_T;
      No_Debug_Info : Boolean       := False)
   is
   begin
      Output_Stmt (+S, Semicolon, Indent_Type, V, No_Debug_Info);
   end Output_Stmt;

   ------------------------
   -- Start_Output_Block --
   ------------------------

   procedure Start_Output_Block (BS : Block_Style) is
   begin
      --  We don't allow consecutive block openers

      pragma Assert (No (Next_Block_Style));
      Next_Block_Style := BS;
   end Start_Output_Block;

   --------------------
   -- End_Decl_Block --
   --------------------

   procedure End_Decl_Block
     (BS         : Block_Style;
      Is_Typedef : Boolean := False;
      Is_Global  : Boolean := False)
   is
   begin
      if No (BS) then
         null;
      elsif Is_Typedef then
         declare
            OL : Out_Line renames Typedefs.Table (Typedefs.Last);
         begin
            pragma Assert (No (OL.End_Block));
            OL.End_Block := BS;
         end;
      elsif Is_Global then
         declare
            OL : Out_Line renames Global_Decls.Table (Global_Decls.Last);
         begin
            pragma Assert (No (OL.End_Block));
            OL.End_Block := BS;
         end;
      else
         declare
            OL : Out_Line renames Local_Decls.Table (Local_Decls.Last);
         begin
            pragma Assert (No (OL.End_Block));
            OL.End_Block := BS;
         end;
      end if;
   end End_Decl_Block;

   --------------------
   -- End_Stmt_Block --
   --------------------

   procedure End_Stmt_Block (BS : Block_Style) is
   begin
      --  We allow consecutive closing of blocks and implement that by
      --  writing a blank entry.

      if Present (BS) then
         if Present (Stmts.Table (Stmts.Last).End_Block) then
            Output_Stmt (No_Str);
         end if;

         Stmts.Table (Stmts.Last).End_Block := BS;
      end if;
   end End_Stmt_Block;

   ----------------------
   -- Get_Typedef_Line --
   ----------------------

   function Get_Typedef_Line (Idx : Typedef_Idx) return Out_Line is
     (Typedefs.Table (Idx));

   --------------------------
   -- Get_Global_Decl_Line --
   --------------------------

   function Get_Global_Decl_Line (Idx : Global_Decl_Idx) return Out_Line is
     (Global_Decls.Table (Idx));

   ---------------------------
   -- Get_Global_Decl_Value --
   ---------------------------

   function Get_Global_Decl_Value (Idx : Global_Decl_Idx) return Value_T is
     (Global_Decls.Table (Idx).V);

   -------------------------
   -- Get_Local_Decl_Line --
   -------------------------

   function Get_Local_Decl_Line (Idx : Local_Decl_Idx) return Out_Line is
     (Local_Decls.Table (Idx));

   -------------------
   -- Get_Stmt_Line --
   -------------------

   function Get_Stmt_Line (Idx : Stmt_Idx) return Out_Line is
     (Stmts.Table (Idx));

   ----------------------
   -- Get_Last_Typedef --
   ----------------------

   function Get_Last_Typedef return Typedef_Idx is
     (Typedefs.Last);

   --------------------------
   -- Get_Last_Global_Decl --
   --------------------------

   function Get_Last_Global_Decl return Global_Decl_Idx is
     (Global_Decls.Last);

begin
   --  Ensure we have an empty entry in the tables that support empty
   --  entries.

   Global_Decls.Increment_Last;
   Local_Decls.Increment_Last;
   Stmts.Increment_Last;

end CCG.Output;
