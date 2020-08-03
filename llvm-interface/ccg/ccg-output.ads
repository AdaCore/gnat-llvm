------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2013-2020, AdaCore                     --
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

with LLVM.Types; use LLVM.Types;

with GNATLLVM; use GNATLLVM;

with CCG.Tables; use CCG.Tables;

package CCG.Output is

   --  This package contains subprograms used to output segments of C
   --  code.

   procedure Write_Value (V : Value_T)
     with Pre => Present (V);
   procedure Write_Type  (T : Type_T)
     with Pre => Present (T);
   procedure Write_BB    (B : Basic_Block_T)
     with Pre => Present (B);
   --  Write the name of a value, type, or basic block

   procedure Write_Typedef (T : Type_T)
     with Pre => Present (T), Post => Get_Is_Typedef_Output (T);
   --  Write the typedef for T, if any

end CCG.Output;
