------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020, AdaCore                          --
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

   procedure Output_Decl (S : Str; Semicolon : Boolean := True)
     with Pre => Present (S);
   procedure Output_Decl (S : String; Semicolon : Boolean := True);
   --  Save S as a decl for the current subprogram

   procedure Output_Stmt (S : Str; Semicolon : Boolean := True)
     with Pre => Present (S);
   procedure Output_Stmt (S : String; Semicolon : Boolean := True);
   --  Save S as a statement for the current subprogram

   procedure New_Subprogram (V : Value_T)
     with Pre => Present (Is_A_Function (V));
   --  Switch to a new subprogram V

   function Current_Func return Value_T
     with Post => Present (Current_Func'Result);
   --  Return the decl for the function being converted to C

   procedure Call_Instruction (V : Value_T; Ops : Value_Array)
     with Pre => Is_A_Call_Inst (V);
   --  Process a call instruction

   procedure Write_Subprograms;
   --  Write all the decls and statements for all subprograms

   procedure Declare_Subprogram (V : Value_T)
     with Pre => Is_A_Function (V);
   --  Write a declaration for subprogram V

   procedure Generate_C_For_Subprogram (V : Value_T)
     with Pre => Is_A_Function (V);
   --  Generate the C statements and decls for V, a function

   procedure Write_Function_Type_Typedef (T : Type_T)
     with Pre => Get_Type_Kind (Get_Element_Type (T)) = Function_Type_Kind;
   --  Write a typedef for T, which is a pointer to a function type

   function Function_Proto (V : Value_T; Extern : Boolean := False) return Str
     with Pre  => Is_A_Function (V),
          Post => Present (Function_Proto'Result);
   --  Return the prototype for function V. Extern is True if we're writing
   --  a declaration of the function (meaning we don't have parameter names).

   function Function_Proto (T : Type_T; S : Str) return Str
     with Pre  => Get_Type_Kind (T) = Function_Type_Kind
                  and then Present (S),
          Post => Present (Function_Proto'Result);
   --  Return the prototype for function type T, using S for where the name
   --  of the function would be.

end CCG.Subprograms;
