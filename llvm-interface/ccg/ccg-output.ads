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

with CCG.Environment; use CCG.Environment;
with CCG.Helper;      use CCG.Helper;
with CCG.Strs;        use CCG.Strs;

package CCG.Output is

   --  This package contains subprograms used to output segments of C
   --  code into structures that are later written to the output file.

   --  We represent each line being output as an Str, but also record
   --  other information that helps us output the line.

   type Out_Line is record
      Line_Text      : Str;
      --  The actual string to output

      No_Indent      : Boolean;
      --  If True, don't indent this to the current indentation level

      Indent_Before  : Integer;
      --  Number of spaces to increase indent before outputting this line.
      --  This line is indented by the new count.

      Indent_After   : Integer;
      --  Number of spaces to increase indent after outputting this line.
      --  This line is indented by the old count.

      V              : Value_T;
      --  An LLVM value that may contain debug information denoting the
      --  position of this line in the source.

      BB             : Basic_Block_T;
      --  If line is "goto <label>;", the block this is branching to

      Need_Brace     : Boolean;
      --  In the goto case, True if we'll need to write a brace if we're
      --  replacing this by more than one statement.
   end record;

   function Int_Type_String (Size : Pos) return Str;
   --  Return the string corresponding to the C name of an integer type of
   --  Size bits.

   procedure Output_Decl
     (S             : Str;
      Semicolon     : Boolean := True;
      Is_Typedef    : Boolean := False;
      Is_Global     : Boolean := False;
      No_Indent     : Boolean := False;
      Indent_Before : Integer := 0;
      Indent_After  : Integer := 0;
      V             : Value_T := No_Value_T)
     with Pre => Present (S);
   procedure Output_Decl
     (S             : String;
      Semicolon     : Boolean := True;
      Is_Typedef    : Boolean := False;
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
      Semicolon     : Boolean       := True;
      No_Indent     : Boolean       := False;
      Indent_Before : Integer       := 0;
      Indent_After  : Integer       := 0;
      V             : Value_T       := No_Value_T;
      BB            : Basic_Block_T := No_BB_T;
      Need_Brace    : Boolean       := False)
     with Pre => Present (S);
   procedure Output_Stmt
     (S             : String;
      Semicolon     : Boolean       := True;
      No_Indent     : Boolean       := False;
      Indent_Before : Integer       := 0;
      Indent_After  : Integer       := 0;
      V             : Value_T       := No_Value_T;
      BB            : Basic_Block_T := No_BB_T;
      Need_Brace    : Boolean       := False);
   --  Like Output_Decl, but for the statement part of the current subprogram

   function Get_Global_Decl_Line (Idx : Global_Decl_Idx) return Out_Line
     with Inline;
   function Get_Local_Decl_Line  (Idx : Local_Decl_Idx)  return Out_Line
     with Inline;
   function Get_Stmt_Line        (Idx : Stmt_Idx)        return Out_Line
     with Inline;
   --  Given an index to a decl or statement, return the data for it

   function Get_Last_Global_Decl return Global_Decl_Idx;
   --  Return the index of the last global decl that was output

   function Is_Entry_Block (BB : Basic_Block_T) return Boolean is
     (Get_Entry_Basic_Block (Get_Basic_Block_Parent (BB)) = BB)
     with Pre => Present (BB);
   function Is_Entry_Block (V : Value_T) return Boolean is
     ((if   Is_A_Basic_Block (V) then Is_Entry_Block (Value_As_Basic_Block (V))
       else Is_Entry_Block (Get_Instruction_Parent (V))))
     with Pre => Is_A_Basic_Block (V) or else Is_A_Instruction (V);
   --  Determine whether something is the entry block or an instruction
   --  within the entry block

   procedure Output_BB (BB : Basic_Block_T);
   --  Generate the code for basic block unless already output

   procedure Output_Branch
     (From       : Value_T;
      To         : Value_T;
      Orig_From  : Value_T := No_Value_T;
      Need_Brace : Boolean := False)
     with Pre => Present (From) and then Present (To);
   procedure Output_Branch
     (From       : Value_T;
      To         : Basic_Block_T;
      Orig_From  : Value_T := No_Value_T;
      Need_Brace : Boolean := False)
     with Pre => Present (From) and then Present (To);
   --  Generate code to jump from instruction From to instruction or
   --  basic block To. Need_Brace says whether we need to generate a
   --  "{ ... }" construct. Orig_From is used in recursive calls to
   --  track the original instruction leading to this branch.

   procedure Maybe_Decl (V : Value_T; For_Initializer : Boolean := False)
     with Pre => Present (V);
   --  See if we need to write a declaration for V and write one if so.
   --  If For_Initializer, we can allow any constants, not just simple ones.

end CCG.Output;
