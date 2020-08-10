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

with GNATLLVM; use GNATLLVM;

with CCG.Helper; use CCG.Helper;
with CCG.Tables; use CCG.Tables;

package CCG.Subprograms is

   --  This package contains subprograms used in the handling of subprograms

   --  We write any typedefs at the time we decide that we need it and
   --  also write decls for any global variables at a similar time.  However,
   --  we keep lists of subprograms and decls and statements for each and
   --  only write those after we've finished processing the module so that
   --  all typedefs and globals are written first.  These procedures manage
   --  those lists.

   procedure Output_Decl (S : Str);
   --  Save S as a decl for the current subprogram

   procedure Output_Stmt (S : Str);
   --  Save S as a statement for the current subprogram

   procedure New_Subprogram (V : Value_T)
     with Pre => Present (Is_A_Function (V));
   --  Switch to a new subprogram V

   procedure Write_Subprograms;
   --  Write all the decls and statements for all subprograms

   procedure Generate_C_For_Subprogram (V : Value_T)
     with Pre => Is_A_Function (V);
   --  Generate the C statements and decls for V, a function

   function Function_Proto (V : Value_T) return Str
     with Pre  => Is_A_Function (V),
          Post => Present (Function_Proto'Result);
   --  Return the prototype for function V

   function Function_Proto (T : Type_T; S : Str) return Str
     with Pre  => Get_Type_Kind (T) = Function_Type_Kind
                  and then Present (S),
          Post => Present (Function_Proto'Result);
   --  Return the prototype for function type T, using S for where the name
   --  of the function would be.

end CCG.Subprograms;
