------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Get_Targ; use Get_Targ;

with Debug;   use Debug;
with Lib;     use Lib;
with Opt;     use Opt;
with Osint;   use Osint;
with Osint.C; use Osint.C;
with Output;  use Output;
with Sinput;  use Sinput;
with Table;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with CCG.Aggregates;   use CCG.Aggregates;
with CCG.Instructions; use CCG.Instructions;
with CCG.Subprograms;  use CCG.Subprograms;
with CCG.Target;       use CCG.Target;
with CCG.Transform;    use CCG.Transform;
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
      Table_Low_Bound      => Global_Decl_Idx_Start,
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

   Current_BB : Basic_Block_T := No_BB_T;
   --  The basic block for which we're outputting statements

   Next_Block_Style : Block_Style := None;
   --  The Block_Style to use for the next line written using Output_Decl
   --  or Output_Stmt.

   procedure Maybe_Output_Typedef_And_Decl (V : Value_T)
     with Pre => Is_A_Constant (V);
   --  Ensure that we're output typedefs for any types within V and
   --  declarations for anything in V that needs it

   ---------------------
   -- Int_Type_String --
   ---------------------

   function Int_Type_String (Size : Pos) return Str is
   begin
      --  ??? There are a number of issues here: Ada supports a
      --  "long long long" type, which could correspond to C's
      --  int128_t.  We also may want to generate intXX_t types
      --  instead of the standard types based on a switch.  But for
      --  now we'll keep it simple.

      if Size > Long_Size and then Size > Int_Size
        and then Size <= Long_Long_Size
      then
         return +"long long";
      elsif Size > Int_Size and then Size <= Long_Size then
         return +"long";
      elsif Size > Short_Size and then Size <= Int_Size then
         return +"int";
      elsif Size > Char_Size and then Size <= Short_Size then
         return +"short";
      elsif Size <= Char_Size then
         return +"char";
      else
         return +"<unknown int type:" & Size'Image & ">";
      end if;
   end Int_Type_String;

   --------------------
   -- Output_Typedef --
   --------------------

   procedure Output_Typedef (T : Type_T; Incomplete : Boolean := False) is
   begin
      --  Show we're outputting the typedef (so we know not to do it
      --  recursively).

      Set_Are_Outputting_Typedef (T);

      --  See what type of type this is

      if Get_Type_Kind (T) = Struct_Type_Kind then
         Output_Struct_Typedef (T, Incomplete => Incomplete);
      elsif Get_Type_Kind (T) = Array_Type_Kind then
         Output_Array_Typedef (T);
      elsif Get_Type_Kind (T) = Pointer_Type_Kind then

         --  We don't have typedefs for function types, just pointer to
         --  function types. But for normal pointer types, make sure we've
         --  written at least an incomplete version of the typedef for the
         --  pointed-to type.

         if Get_Type_Kind (Get_Element_Type (T)) = Function_Type_Kind then
            Output_Function_Type_Typedef (T);
         else
            Maybe_Output_Typedef (Get_Element_Type (T), Incomplete => True);
         end if;
      end if;

      --  Show we've written the typedef unless this is a struct type and
      --  we're only writing an incomplete definition.

      Set_Are_Outputting_Typedef (T, False);
      if not Incomplete or else Get_Type_Kind (T) /= Struct_Type_Kind then
         Set_Is_Typedef_Output   (T);
      end if;
   end Output_Typedef;

   ----------------
   -- Maybe_Decl --
   ----------------

   procedure Maybe_Decl (V : Value_T; For_Initializer : Boolean := False) is
   begin
      --  If this is metadata, we do nothing. This can occur for some
      --  builtins, but we don't process those.

      if Is_Metadata (V) then
         return;

      --  Be sure that we've output a typedef for V's type. But don't do this
      --  for a function since the reference to the function doesn't require
      --  us to declare its type separately.

      elsif not Is_A_Function (V) then
         Maybe_Output_Typedef (Type_Of (V));
      end if;

      --  If this is an unprocessed constant expression, process it as an
      --  instruction

      if Is_A_Constant_Expr (V) and then No (Get_C_Value (V)) then
         Process_Instruction (V);
      end if;

      --  If we've already processed this or if it's a simple constant
      --  (any constant if this is for an initializer), we don't need
      --  to do anything.

      if Get_Is_Decl_Output (V)
        or else Is_Simple_Constant (V)
        or else (For_Initializer and then Is_A_Constant (V))
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

         --  If this is a global, mark it as an LHS

         if Is_A_Global_Variable (V) then
            Set_Is_LHS (V);
         end if;

         --  The relevant type is the type of V unless V is an LHS, in
         --  which case the type of V is a pointer and we want what it
         --  points to.

         declare
            Typ  : constant Type_T :=
              (if Get_Is_LHS (V) then Get_Element_Type (V) else Type_Of (V));
            Decl : Str             := Typ & " " & (V + LHS);

         begin
            --  If this is known to be unsigned, indicate that

            if Get_Is_Unsigned (V) then
               Decl := "unsigned " & Decl;
            end if;

            --  For globals, we write the decl immediately. Otherwise, it's
            --  part of the decls for the subprogram.  Figure out whether
            --  this is static or extern.  It's extern if there's no
            --  initializer.

            if Is_A_Global_Variable (V) then
               declare
                  Init : constant Value_T := Get_Initializer (V);

               begin
                  if No (Init) then
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

                  if Present (Init) and then not Is_Undef (Init)
                    and then not Is_A_Constant_Aggregate_Zero (Init)
                  then
                     Maybe_Output_Typedef_And_Decl (Init);
                     Decl := Decl & " = " & (Init + Initializer);
                  end if;

                  Output_Decl (Decl, Is_Global => True, V => V);
               end;
            else
               --  If this is a constant (we know that it can't be a simple
               --  constant), we need to initialize the value to that of the
               --  constant and put it at the top level.

               if Is_A_Constant (V) then
                  Maybe_Output_Typedef_And_Decl (V);
                  Output_Decl ("static const " & Decl & " = " &
                                 (V + Initializer),
                               Is_Global => True, V => V);
                  Set_Is_Constant (V);
               else
                  Output_Decl (Decl, V => V);
               end if;
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
      End_Block     : Block_Style  := None;
      Indent_Type   : Indent_Style := Normal;
      No_Indent     : Boolean      := False;
      Indent_Before : Integer      := 0;
      Indent_After  : Integer      := 0;
      V             : Value_T      := No_Value_T)
   is
      OL : constant Out_Line :=
        (Line_Text      => (if Semicolon then S & ";" else S),
         Start_Block    => Next_Block_Style,
         Indent_Type    => Indent_Type,
         No_Indent      => No_Indent,
         End_Block      => End_Block,
         Indent_Before  => Indent_Before,
         Indent_After   => Indent_After,
         V              => V,
         BB             => No_BB_T);
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
      End_Block     : Block_Style  := None;
      Indent_Type   : Indent_Style := Normal;
      No_Indent     : Boolean      := False;
      Indent_Before : Integer      := 0;
      Indent_After  : Integer      := 0;
      V             : Value_T      := No_Value_T)
   is
   begin
      Output_Decl (+S, Semicolon, Is_Typedef, Is_Global, End_Block,
                   Indent_Type, No_Indent, Indent_Before, Indent_After, V);
   end Output_Decl;

   -----------------
   -- Output_Stmt --
   ----------------

   procedure Output_Stmt
     (S             : Str;
      Semicolon     : Boolean       := True;
      End_Block     : Block_Style   := None;
      Indent_Type   : Indent_Style  := Normal;
      No_Indent     : Boolean       := False;
      Indent_Before : Integer       := 0;
      Indent_After  : Integer       := 0;
      V             : Value_T       := No_Value_T;
      BB            : Basic_Block_T := No_BB_T)
   is
   begin
      --  If we've been given an instruction corresponding to this
      --  statement and it has side-effects, first flush any pending
      --  assignments.

      if Present (V) and then Has_Side_Effects (V) then
         Process_Pending_Values;
      end if;

      --  Add the statement to the appropriate block

      Stmts.Append ((Line_Text      => (if Semicolon then S & ";" else S),
                     Start_Block    => Next_Block_Style,
                     End_Block      => End_Block,
                     Indent_Type    => Indent_Type,
                     No_Indent      => No_Indent,
                     Indent_Before  => Indent_Before,
                     Indent_After   => Indent_After,
                     V              => V,
                     BB             => BB));
      Next_Block_Style := None;
      Set_Last_Stmt (Current_BB, Stmts.Last);
      if No (Get_First_Stmt (Current_BB)) then
         Set_First_Stmt (Current_BB, Stmts.Last);
      end if;
   end Output_Stmt;

   -----------------
   -- Output_Stmt --
   ----------------

   procedure Output_Stmt
     (S             : String;
      Semicolon     : Boolean       := True;
      End_Block     : Block_Style    := None;
      Indent_Type   : Indent_Style  := Normal;
      No_Indent     : Boolean       := False;
      Indent_Before : Integer       := 0;
      Indent_After  : Integer       := 0;
      V             : Value_T       := No_Value_T;
      BB            : Basic_Block_T := No_BB_T)
   is
   begin
      Output_Stmt (+S, Semicolon, End_Block, Indent_Type, No_Indent,
                   Indent_Before, Indent_After, V, BB);
   end Output_Stmt;

   ------------------------
   -- Start_Output_Block --
   ------------------------

   procedure Start_Output_Block (BS : Block_Style) is
   begin
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
            pragma Assert (OL.End_Block = None);
            OL.End_Block := BS;
         end;
      elsif Is_Global then
         declare
            OL : Out_Line renames Global_Decls.Table (Global_Decls.Last);
         begin
            pragma Assert (OL.End_Block = None);
            OL.End_Block := BS;
         end;
      else
         declare
            OL : Out_Line renames Local_Decls.Table (Local_Decls.Last);
         begin
            pragma Assert (OL.End_Block = None);
            OL.End_Block := BS;
         end;
      end if;
   end End_Decl_Block;

   --------------------
   -- End_Stmt_Block --
   --------------------

   procedure End_Stmt_Block (BS : Block_Style) is
      OL : Out_Line renames Stmts.Table (Stmts.Last);
   begin
      if Present (BS) then
         pragma Assert (OL.End_Block = None);
         OL.End_Block := BS;
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

   ---------------
   -- Output_BB --
   ---------------

   procedure Output_BB (BB : Basic_Block_T) is
      V          : Value_T          :=
        (if Present (BB) then Get_First_Instruction (BB) else No_Value_T);
      Terminator : constant Value_T :=
        (if Present (BB) then Get_Basic_Block_Terminator (BB) else No_Value_T);

   begin
      --  Set which block we're dealing with

      Current_BB := BB;

      --  If this isn't really a basic block or we already processed it, do
      --  nothing.

      if No (BB) or else Get_Was_Output (BB) then
         return;
      end if;

      --  Mark that we're outputing this block and process each
      --  instruction it.

      Set_Was_Output (BB);
      while Present (V) loop
         Process_Instruction (V);
         V := Get_Next_Instruction (V);
      end loop;

      --  Now process any block referenced by the terminator

      for J in Nat range 0 .. Get_Num_Successors (Terminator) - 1 loop
         Output_BB (Get_Successor (Terminator, J));
      end loop;

      Current_BB := No_BB_T;
   end Output_BB;

   -------------------
   -- Output_Branch --
   -------------------

   procedure Output_Branch
     (From : Value_T; To : Value_T; BS : Block_Style := None)
   is
   begin
      Output_Branch (From, Value_As_Basic_Block (To), BS);
   end Output_Branch;

   -------------------
   -- Output_Branch --
   -------------------

   procedure Output_Branch
     (From : Value_T; To : Basic_Block_T; BS : Block_Style := None)
   is
   begin
      Start_Output_Block (BS);
      Output_Stmt ("goto " & To, V => From, BB => To);
      End_Stmt_Block (BS);
   end Output_Branch;

begin
   --  Ensure we have an empty entry in the tables that support empty
   --  entries.

   Local_Decls.Increment_Last;
   Stmts.Increment_Last;

end CCG.Output;
