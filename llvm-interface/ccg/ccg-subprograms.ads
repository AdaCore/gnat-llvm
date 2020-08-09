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

with LLVM.Core;  use LLVM.Core;
with LLVM.Types; use LLVM.Types;

with GNATLLVM; use GNATLLVM;

with CCG.Tables; use CCG.Tables;

package CCG.Subprograms is

   --  This package contains subprograms used in the handling of subprograms

   function Function_Proto (V : Value_T) return Str
     with Pre  => Present (Is_A_Function (V)),
          Post => Present (Function_Proto'Result);
   --  Return the prototype for function V

   function Function_Proto (T : Type_T; S : Str) return Str
     with Pre  => Get_Type_Kind (T) = Function_Type_Kind
                  and then Present (S),
          Post => Present (Function_Proto'Result);
   --  Return the prototype for function type T, using S for where the name
   --  of the function would be.

end CCG.Subprograms;
