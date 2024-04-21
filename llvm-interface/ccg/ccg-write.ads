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

with CCG.Output;      use CCG.Output;
with CCG.Strs;        use CCG.Strs;

package CCG.Write is

   --  This package contains subprograms and data used to output the saved
   --  C statements into the output file.

   Needs_Malloc_H : Boolean := False;
   --  True if we need to add an include for <malloc.h> because we
   --  use alloca.

   procedure Write_Value
     (V              : Value_T;
      Flags          : Value_Flags := Default_Value_Flags;
      For_Precedence : Precedence  := Primary)
     with Pre => Present (V);
   procedure Write_Type
     (T     : Type_T;
      Flags : Type_Flags := Default_Type_Flags;
      E     : Entity_Id  := Empty;
      V     : Value_T    := No_Value_T)
     with Pre => Present (T);
   procedure Write_BB_Value  (BB : Basic_Block_T)
     with Pre => Present (BB);
   --  Write the name of a value, type, or basic block. For types, possibly
   --  use entity or value to help say something about the type.

   procedure Write_C_Name (S : String; Need_Suffix : Boolean := False)
     with Pre => S'Length > 0;
   --  Write S as a valid name in C. If Need_Suffix is True, force a
   --  suffix to distinguish it from a normal C name.

   --  just write the initial definition of the struct, with no fields.

   procedure Initialize_Writing;
   procedure Finalize_Writing;
   --  Set up for writing lines of C and finalize writing them

   procedure Write_C_Line (OL : Out_Line);
   procedure Write_C_Line
     (Idx : Stmt_Idx; Start_Block, End_Block : Block_Style := None)
     with Pre => Present (Idx);
   procedure Write_C_Line (Idx : Typedef_Idx);
   procedure Write_C_Line (Idx : Global_Decl_Idx);
   procedure Write_C_Line (Idx : Local_Decl_Idx)
     with Pre => Present (Idx);
   procedure Write_C_Line
     (S             : Str;
      Indent_Type   : Indent_Style  := Normal;
      End_Block     : Block_Style   := None;
      V             : Value_T       := No_Value_T;
      No_Debug_Info : Boolean       := False)
     with Pre => Present (S);
   procedure Write_C_Line
     (S             : String;
      Indent_Type   : Indent_Style  := Normal;
      End_Block     : Block_Style   := None;
      V             : Value_T       := No_Value_T;
      No_Debug_Info : Boolean       := False);
   --  Write one line to our output file, taking care of any required
   --  debug data, source line writing, and #line directives.

end CCG.Write;
