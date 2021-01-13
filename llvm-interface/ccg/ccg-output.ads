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

with CCG.Strs;   use CCG.Strs;
with CCG.Tables; use CCG.Tables;

package CCG.Output is

   --  This package contains subprograms used to output segments of C
   --  code.

   function Int_String (Size : Pos) return String;
   --  Return the string corresponding to an integer type of Size bits

   procedure Write_Value
     (V              : Value_T;
      Flags          : Value_Flags := Default_Flags;
      For_Precedence : Precedence  := Primary)
     with Pre => Present (V);
   procedure Write_Type  (T : Type_T)
     with Pre => Present (T);
   procedure Write_BB    (BB : Basic_Block_T)
     with Pre => Present (BB);
   --  Write the name of a value, type, or basic block

   procedure Write_C_Name (S : String)
     with Pre => S'Length > 0;
   --  Write S as a valid name in C

   procedure Maybe_Decl (V : Value_T; For_Initializer : Boolean := False)
     with Pre => Present (V);
   --  See if we need to write a declaration for V and write one if so.
   --  If For_Initializer, we can allow any constants, not just simple ones.

   procedure Write_Typedef (T : Type_T; Incomplete : Boolean := False)
     with Pre =>  Present (T),
          Post => Get_Is_Typedef_Output (T)
                  or else (Incomplete and then Get_Is_Incomplete_Output (T));
   --  Write the typedef for T, if any. If Incomplete an T is a struct type,
   --  just write the initial definition of the struct, with no fields.

end CCG.Output;
