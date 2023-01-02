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

with LLVM.Core;  use LLVM.Core;

with CCG.Helper; use CCG.Helper;
with CCG.Utils;  use CCG.Utils;

package CCG.Subprograms is

   --  This package contains subprograms and data used in the handling of
   --  subprograms and writing out the final C code.

   procedure Add_To_Source_Order (N : Node_Id)
     with Pre => Nkind (N) in N_Pragma | N_Subprogram_Declaration |
                              N_Subprogram_Body | N_Object_Declaration |
                              N_Object_Renaming_Declaration |
                              N_Exception_Declaration |
                              N_Exception_Renaming_Declaration;
   --  Add N to the list of file-level objects present in the source if
   --  it indeed does come from the source.

   procedure Protect_Source_Order;
   --  Make a pass over everything we added to the source order and
   --  set up to be notified if any of them have been deleted.

   procedure New_Subprogram (V : Value_T)
     with Pre => Present (Is_A_Function (V));
   --  Switch to a new subprogram V

   function Curr_Func return Value_T
     with Post => Present (Curr_Func'Result);
   --  Return the decl for the function being converted to C

   procedure Call_Instruction (V : Value_T; Ops : Value_Array)
     with Pre => Is_A_Call_Inst (V);
   --  Process a call instruction

   procedure Declare_Subprogram (V : Value_T)
     with Pre => Is_A_Function (V);
   --  Write a declaration for subprogram V

   procedure Output_Subprogram (V : Value_T)
     with Pre => Is_A_Function (V);
   --  Generate the C statements and decls for V, a subprogram

   procedure Output_Function_Type_Typedef (T : Type_T)
     with Pre => Is_Function_Type (Get_Element_Type (T));
   --  Output a typedef for T, which is a pointer to a function type

   procedure Add_Decl_Line (Idx : Local_Decl_Idx)
     with Pre => Present (Idx);
   procedure Add_Stmt_Line (Idx : Stmt_Idx)
     with Pre => Present (Idx);
   --  Add a declaration or statement line to the current subprogram

   procedure Write_C_File;
   --  Write all the typedefs, globals, and decls and statements for
   --  all subprograms.

end CCG.Subprograms;
