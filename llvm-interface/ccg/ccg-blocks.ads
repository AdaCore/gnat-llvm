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

with LLVM.Core; use LLVM.Core;

with CCG.Helper; use CCG.Helper;
with CCG.Strs;   use CCG.Strs;

with Table;

package CCG.Blocks is

   --  This package contains subprograms used in the handling of blocks

   --  We represent each line being output as an Str, but also record
   --  whether this line is to be unindented, deltas to the indentation
   --  before and after we output the line and also a possible value from
   --  which to obtain a possible debug filename and line number.

   type Out_Line is record
      Line_Text      : Str;
      No_Indent      : Boolean;
      Indent_Before  : Integer;
      Indent_After   : Integer;
      V              : Value_T;
   end record;

   type Decl_Idx is new Nat;
   type Stmt_Idx is new Nat;

   No_Decl_Idx : constant Decl_Idx := 0;
   No_Stmt_Idx : constant Stmt_Idx := 0;

   function Present (J : Decl_Idx) return Boolean is (J /= No_Decl_Idx);
   function Present (J : Stmt_Idx) return Boolean is (J /= No_Stmt_Idx);
   function No      (J : Decl_Idx) return Boolean is (J = No_Decl_Idx);
   function No      (J : Stmt_Idx) return Boolean is (J = No_Stmt_Idx);

   --  Tables for global and local decls and statements

   package Global_Decl_Table is new Table.Table
     (Table_Component_Type => Out_Line,
      Table_Index_Type     => Decl_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 500,
      Table_Increment      => 100,
      Table_Name           => "Global_Decl_Table");

   package Local_Decl_Table is new Table.Table
     (Table_Component_Type => Out_Line,
      Table_Index_Type     => Decl_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 500,
      Table_Increment      => 100,
      Table_Name           => "Local_Decl_Table");

   package Stmt_Table is new Table.Table
     (Table_Component_Type => Out_Line,
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

   --  We write any typedefs at the time we decide that we need it and
   --  also write decls for any global variables at a similar time.  However,
   --  we keep lists of subprograms and decls and statements for each and
   --  only write those after we've finished processing the module so that
   --  all typedefs and globals are written first.  These procedures manage
   --  those lists.

   procedure Output_Decl
     (S             : Str;
      Semicolon     : Boolean := True;
      Is_Global     : Boolean := False;
      No_Indent     : Boolean := False;
      Indent_Before : Integer := 0;
      Indent_After  : Integer := 0;
      V             : Value_T := No_Value_T)
     with Pre => Present (S);
   procedure Output_Decl
     (S             : String;
      Semicolon     : Boolean := True;
      Is_Global     : Boolean := False;
      No_Indent     : Boolean := False;
      Indent_Before : Integer := 0;
      Indent_After  : Integer := 0;
      V             : Value_T := No_Value_T);
   --  Save S as a decl for the current subprogram. Append a semicolon to
   --  the string if requested (the default) and specify indentation
   --  parameters. V, if Present, is a value that we may be able to get
   --  debug information from. If Is_Global is True, this is for the global
   --  section, in front of all subprograms; otherwise it's local to the
   --  current subprogram.

   procedure Output_Stmt
     (S             : Str;
      Semicolon     : Boolean := True;
      No_Indent     : Boolean := False;
      Indent_Before : Integer := 0;
      Indent_After  : Integer := 0;
      V             : Value_T := No_Value_T)
     with Pre => Present (S);
   procedure Output_Stmt
     (S             : String;
      Semicolon     : Boolean := True;
      No_Indent     : Boolean := False;
      Indent_Before : Integer := 0;
      Indent_After  : Integer := 0;
      V             : Value_T := No_Value_T);
   --  Like Output_Decl, but for the statement part of the current subprogram

   function Is_Entry_Block (BB : Basic_Block_T) return Boolean is
     (Get_Entry_Basic_Block (Get_Basic_Block_Parent (BB)) = BB)
     with Pre => Present (BB);
   function Is_Entry_Block (V : Value_T) return Boolean is
     ((if   Is_A_Basic_Block (V) then Is_Entry_Block (Value_As_Basic_Block (V))
       else Is_Entry_Block (Get_Instruction_Parent (V))))
     with Pre => Is_A_Basic_Block (V) or else Is_A_Instruction (V);
   --  Determine whether something is the entry block or an instruction
   --  within the entry block

   procedure Output_BB (BB : Basic_Block_T)
     with Pre => Present (BB);
   procedure Output_BB (V : Value_T)
     with Pre => Is_A_Basic_Block (V), Inline;
   --  Generate the code for basic block unless already output

   procedure Branch_Instruction (V : Value_T; Ops : Value_Array)
     with Pre => Is_A_Branch_Inst (V);
   --  Process V, a branch instruction

   procedure Switch_Instruction (V : Value_T; Ops : Value_Array)
     with Pre => Is_A_Switch_Inst (V);
   --  Process V, a switch instruction

   procedure Output_Branch
     (From       : Value_T;
      To         : Value_T;
      Need_Block : Boolean := False;
      Had_Phi    : Boolean := False)
     with Pre => Present (From) and then Present (To);
   procedure Output_Branch
     (From       : Value_T;
      To         : Basic_Block_T;
      Need_Block : Boolean := False;
      Had_Phi    : Boolean := False)
     with Pre => Present (From) and then Present (To);
   --  Generate code to jump from instruction From to instruction or basic
   --  block To, taking care of any phi instructions at the target.
   --  Need_Block says whether we need to generate a "{ ... }" construct.

end CCG.Blocks;
