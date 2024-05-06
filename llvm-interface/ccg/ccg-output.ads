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

with CCG.Codegen;     use CCG.Codegen;
with CCG.Environment; use CCG.Environment;
with CCG.Helper;      use CCG.Helper;
with CCG.Strs;        use CCG.Strs;

package CCG.Output is

   --  This package contains subprograms used to output segments of C
   --  code into structures that are later written to the output file.

   --  When we write a C block structure, we have various indentation
   --  rules and we may or may not need an open and close brace. We
   --  have various types of blocks, which we name here according to
   --  the statement type.

   type Block_Style is (None, Decl, If_Part, Switch);

   --  There are three possible indentations for a line: normal indentation,
   --  all the way on the left (labels) and aligned with the brace (switch
   --  statement).

   type Indent_Style is (Normal, Left, Under_Brace);

   function Present (BS   : Block_Style)  return Boolean is
      (BS /= None);
   function Present (INDS : Indent_Style) return Boolean is
      (INDS /= Normal);

   function No (BS   : Block_Style)  return Boolean is
      (BS = None);
   function No (INDS : Indent_Style) return Boolean is
      (INDS = Normal);

   --  We represent each line being output as an Str, but also record
   --  other information that helps us output the line.

   type Out_Line is record
      Line_Text      : Str;
      --  The actual string to output

      Start_Block    : Block_Style;
      --  The type of block, if any, started by this line

      End_Block      : Block_Style;
      --  The type of block, if any, ended by this line. If the line starts
      --  with a brace, we use this line instead of writing a line with
      --  just a brace.

      Indent_Type    : Indent_Style;
      --  The indentation desired for this line

      V              : Value_T;
      --  An LLVM value corresponding to this line, if any

      No_Debug_Info  : Boolean;
      --  If True, don't use V for debug information

   end record;

   procedure Output_Typedef (T : Type_T; Incomplete : Boolean := False)
     with Pre =>  Present (T),
          Post => Get_Is_Typedef_Output (T)
                  or else (Incomplete and then Get_Is_Incomplete_Output (T));
   --  Output the typedef for T, if any. If Incomplete an T is a struct type,

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
     with Pre => Present (S);
   procedure Output_Decl
     (S             : String;
      Semicolon     : Boolean      := True;
      Is_Typedef    : Boolean      := False;
      Is_Global     : Boolean      := False;
      Start_Block   : Block_Style  := None;
      End_Block     : Block_Style  := None;
      Indent_Type   : Indent_Style := Normal;
      V             : Value_T      := No_Value_T;
      No_Debug_Info : Boolean      := False);
   --  Save S as a decl for the current subprogram. Append a semicolon to
   --  the string if requested (the default) and specify indentation
   --  parameters. V, if Present, is a value that we may be able to get
   --  debug information from. If Is_Global is True, this is for the global
   --  section, in front of all subprograms; otherwise it's local to the
   --  current subprogram.

   procedure Output_Stmt
     (S             : Str;
      Semicolon     : Boolean       := True;
      Indent_Type   : Indent_Style  := Normal;
      V             : Value_T       := No_Value_T;
      No_Debug_Info : Boolean       := False);
   procedure Output_Stmt
     (S             : String;
      Semicolon     : Boolean       := True;
      Indent_Type   : Indent_Style  := Normal;
      V             : Value_T       := No_Value_T;
      No_Debug_Info : Boolean       := False);
   --  Like Output_Decl, but for the statement part of the current subprogram

   procedure Start_Output_Block (BS : Block_Style);
   --  Indicate that the next call to Output_Decl or Output_Stmt is the
   --  start of a block of the specified style.

   procedure End_Decl_Block
     (BS         : Block_Style;
      Is_Typedef : Boolean := False;
      Is_Global  : Boolean := False);
   procedure End_Stmt_Block (BS : Block_Style);
   --  Flag the last line output via Output_Decl or Output_Stmt as being
   --  the last in its block.

   function Get_Typedef_Line     (Idx : Typedef_Idx)     return Out_Line;
   function Get_Local_Decl_Line  (Idx : Local_Decl_Idx)  return Out_Line;
   function Get_Stmt_Line        (Idx : Stmt_Idx)        return Out_Line;
   function Get_Global_Decl_Line (Idx : Global_Decl_Idx) return Out_Line
     with Pre => Present (Idx);
   --  Given an index to a decl or statement, return the data for it

   function Get_Global_Decl_Value (Idx : Global_Decl_Idx) return Value_T
     with Pre => Present (Idx);
   --  Get the value, if any, being declared by a global decl line

   function Get_Last_Typedef     return Typedef_Idx;
   function Get_Last_Global_Decl return Global_Decl_Idx;
   --  Return the index of the last typedef or global decl that was output

   function Is_Entry_Block (BB : Basic_Block_T) return Boolean is
     (Get_Entry_Basic_Block (Get_Basic_Block_Parent (BB)) = BB)
     with Pre => Present (BB);
   function Is_Entry_Block (V : Value_T) return Boolean is
     ((if   Is_A_Basic_Block (V) then Is_Entry_Block (Value_As_Basic_Block (V))
       else Is_Entry_Block (Get_Instruction_Parent (V))))
     with Pre => Is_A_Basic_Block (V) or else Is_A_Instruction (V);
   --  Determine whether something is the entry block or an instruction
   --  within the entry block

   procedure Maybe_Decl (V : Value_T; For_Initializer : Boolean := False)
     with Pre => Present (V);
   --  See if we need to write a declaration for V and write one if so.
   --  If For_Initializer, we can allow any constants, not just simple ones.

   function Generic_Ptr return Str is
     (if Use_Stdint then +"int8_t *" else +"signed char *");
   --  Return the string to use for a "generic" pointer

end CCG.Output;
