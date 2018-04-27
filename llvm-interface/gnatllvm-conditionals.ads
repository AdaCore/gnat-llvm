------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with Atree;  use Atree;
with Einfo;  use Einfo;
with Nlists; use Nlists;
with Sinfo;  use Sinfo;
with Types;  use Types;
with Uintp;  use Uintp;

with LLVM.Types; use LLVM.Types;

with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package GNATLLVM.Conditionals is

   function Build_Short_Circuit_Op
     (Left, Right : Node_Id; Op : Node_Kind) return GL_Value
     with Pre  => Present (Left) and then Present (Right),
          Post => Present (Build_Short_Circuit_Op'Result);
   --  Emit the LLVM IR for a short circuit operator ("or else", "and then")

   function Emit_Comparison
     (Kind : Node_Kind; LHS, RHS : Node_Id) return GL_Value
     with Pre  => Present (LHS) and then Present (RHS),
          Post => Present (Emit_Comparison'Result);
   --  Generate a result which is a comparison of two expressions

   procedure Emit_Comparison_And_Branch
     (Kind              : Node_Kind;
      LHS, RHS          : Node_Id;
      BB_True, BB_False : Basic_Block_T)
     with Pre => Present (LHS) and then Present (RHS)
                 and then Present (BB_True) and then Present (BB_False);
   --  Similar, but generate comparison and branch to one of the basic
   --  blocks depending on the result

   function Emit_Elementary_Comparison
     (Kind               : Node_Kind;
      Orig_LHS, Orig_RHS : GL_Value) return GL_Value
     with Pre  => Is_Elementary_Type (Orig_LHS)
                  and then Is_Elementary_Type (Orig_RHS),
          Post => Present (Emit_Elementary_Comparison'Result);
   --  Helpers for Emit_Expression: handle comparison operations for
   --  elementary types.  The second form only supports discrete or pointer
   --  types.

   procedure Emit_If (N : Node_Id)
     with Pre => Nkind (N) = N_If_Statement;
   --  Helper for Emit: handle if statements

   procedure Emit_If_Cond (N : Node_Id; BB_True, BB_False : Basic_Block_T)
     with Pre => Present (N)
                 and then Present (BB_True) and then Present (BB_False);
   --  Helper for Emit_If to generate branch to BB_True or BB_False
   --  depending on whether Node is true or false.

   function Emit_If_Expression (N : Node_Id) return GL_Value
     with Pre  => Nkind (N) = N_If_Expression,
          Post => Present (Emit_If_Expression'Result);
   --  Helper for Emit_Expression: handle if expressions

   procedure Emit_If_Range
     (N                 : Node_Id;
      LHS               : GL_Value;
      Low, High         : Uint;
      BB_True, BB_False : Basic_Block_T)
     with Pre => Present (N) and then Present (LHS)
                 and then Present (BB_True) and then Present (BB_False);
   --  Emit code to branch to BB_True or BB_False depending on whether LHS,
   --  which is of type Operand_Type, is in the range from Low to High.  Node
   --  is used only for error messages.

   procedure Emit_Case (N : Node_Id)
     with Pre => Nkind (N) = N_Case_Statement;
   --  Handle case statements

   function Emit_Min_Max
     (Exprs : List_Id; Compute_Max : Boolean) return GL_Value
     with Pre  => List_Length (Exprs) = 2
                 and then Is_Scalar_Type (Full_Etype (First (Exprs))),
          Post => Present (Emit_Min_Max'Result);
   --  Exprs must be a list of two scalar expressions with compatible types.
   --  Emit code to evaluate both expressions. If Compute_Max, return the
   --  maximum value and return the minimum otherwise.

end GNATLLVM.Conditionals;
