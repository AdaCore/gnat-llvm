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

with Sinfo;    use Sinfo;

with LLVM.Core;  use LLVM.Core;

with GNATLLVM.GLValue;     use GNATLLVM.GLValue;

package GNATLLVM.Exprs is
   --  This can't be named GNATLLVM.Expressions because it conflicts
   --  with Expressions in Sinfo,

   procedure Emit_Overflow_Check (V : GL_Value; N : Node_Id)
     with Pre => Nkind (N) = N_Type_Conversion and then Present (V)
                 and then Is_Elementary_Type (V);
   --  Check that V is within the bounds of N's type.

   function Emit_Shift
     (Operation           : Node_Kind;
      LHS_Node, RHS_Node  : Node_Id) return GL_Value
     with Pre  => Operation in N_Op_Shift and then Present (LHS_Node)
                  and then Present (RHS_Node),
          Post => Present (Emit_Shift'Result);
   --  Handle shift and rotate operations

   function Emit_Binary_Operation (N : Node_Id) return GL_Value
     with Pre  => Nkind (N) in N_Binary_Op,
          Post => Present (Emit_Binary_Operation'Result);
   --  Handle other binary operations

   function Emit_Unary_Operation (N : Node_Id) return GL_Value
     with Pre  => Nkind (N) in N_Unary_Op,
          Post => Present (Emit_Unary_Operation'Result);
   --  Handle unary operations

   function Emit_Literal (N : Node_Id) return GL_Value
     with Pre => Present (N), Post => Present (Emit_Literal'Result);
   --  Generate code for a literal

   function Emit_Undef (TE : Entity_Id) return GL_Value
     with Pre => Is_Type (TE), Post => Present (Emit_Undef'Result);
   --  Emit an undef appropriate for a return value of type TE

   function Emit_Attribute_Reference (N : Node_Id) return GL_Value
     with Pre  => Nkind (N) = N_Attribute_Reference,
          Post => Present (Emit_Attribute_Reference'Result);
   --  Handle N_Attribute_Reference nodes

   procedure Emit_Assignment
     (LValue                    : GL_Value;
      Orig_E                    : Node_Id;
      E_Value                   : GL_Value;
      Forwards_OK, Backwards_OK : Boolean)
     with Pre => Present (LValue) or else Present (Orig_E);
   --  Copy the value of the expression E to LValue with the specified
   --  destination and expression types.

   procedure Emit_Code_Statement (N : Node_Id)
     with Pre => Nkind (N) = N_Code_Statement;
   --  Generate code for inline asm

   function Build_Max (LHS, RHS : GL_Value) return GL_Value is
     (Build_Select (I_Cmp (Int_SGT, LHS, RHS), LHS, RHS))
     with Pre  => Present (LHS) and then Present (RHS),
          Post => Present (Build_Max'Result);
   function Build_Min (LHS, RHS : GL_Value) return GL_Value is
     (Build_Select (I_Cmp (Int_SLT, LHS, RHS), LHS, RHS))
     with Pre  => Present (LHS) and then Present (RHS),
          Post => Present (Build_Min'Result);

end GNATLLVM.Exprs;
