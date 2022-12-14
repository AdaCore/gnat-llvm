------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                     --
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

with CCG.Strs;  use CCG.Strs;

package CCG.Target is

   --  This package contains target information about the C compiler used
   --  and how to format and generate code.

   Target_Info_File  : String_Access  := null;
   --  If non-null, the name of a file from which to read C target parameters

   Dump_C_Parameters : Boolean        := False;
   --  True if we should dump the values of the C target parameters

   C_Parameter_File  : String_Access  := null;
   --  If non-null, the name of a file to dump the C parameters

   procedure Set_C_Compiler (S : String);
   --  Set the parameters corresponding to the C compiler given in S

   procedure Read_C_Parameters (Name : String);
   --  Read C parameters from file Name

   procedure Set_C_Parameter (S : String);
   --  S is of the form "name=value". Use it to set parameter "name" to "value"

   procedure Output_C_Parameters;
   --  Output all the C parameters

   type OM_Blank is (Before, After);
   function Output_Modifier
     (M : String; Blank : OM_Blank := Before; Val : Int := -1) return Str
     with Post => Present (Output_Modifier'Result);
   --  Return a Str corresponding to the way we write modifier M on our
   --  target. If Val is non-negative, we expect the template to contain a
   --  way to write an integer. Blank says whether we're to write a blank
   --  before or after the value. If we're writing the null string, we
   --  don't write a blank at all.

   --  These are the parameters themselves

   Version            : aliased Integer := 1999;
   --  C standard for which we're to write output

   C_Indent           : aliased Integer := 2;
   --  Number of characters to indent at each level

   Max_Depth          : aliased Integer := (80 / 2) / (2 * C_Indent);
   --  Maximum allowable nesting depth of constructs

   Always_Brace       : aliased Boolean := False;
   --  True if we're to always write C lexical blocks using braces even
   --  if they're only a single line.

   Have_Includes      : aliased Boolean := True;
   --  True if we're to write #include lines for the standard C includes

   Warns_Parens       : aliased Boolean := True;
   --  True if this C compiler will issue warning in cases where the
   --  precedence is correct but looks suspicious.

   Inline_Always_Must : aliased Boolean := True;
   --  In some C compilers (e.g., clang), Inline_Always means to make a
   --  best try at inlining, but be silent if the function can't be inlned.
   --  In others (e.g., gcc), if the function can't be inlined, it issues
   --  a warning (or error, depending on the warning mode). The value of
   --  this option says which is the case.

end CCG.Target;
