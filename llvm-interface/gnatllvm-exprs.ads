------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2023, AdaCore                     --
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

with GNATLLVM.GLValue;      use GNATLLVM.GLValue;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;

package GNATLLVM.Exprs is
   --  This can't be named GNATLLVM.Expressions because it conflicts
   --  with Expressions in Sinfo,

   function Is_Safe_From (LHS : GL_Value; N : N_Subexpr_Id) return Boolean
     with Pre => Present (LHS);
   --  True if we know that clobbering LHS won't change the value of N

   procedure LHS_And_Component_For_Assignment
     (N             : N_Subexpr_Id;
      LHS           : out GL_Value;
      F             : out Opt_Record_Field_Kind_Id;
      Idxs          : out Access_GL_Value_Array;
      For_LHS       : Boolean := False;
      Only_Bitfield : Boolean := False);
   --  N is an expression that's used in a LHS context, either the LHS side
   --  of an N_Assignment_Statement or an actual corresponding to an Out
   --  (or in Out) parameter. If N represents an field selection (if
   --  Only_Bitfield then only if that field is a bitfield), then LHS is
   --  the Prefix of that selection and F is the field being selected. If
   --  N is an indexed reference, Idxs is a pointer to the list of indices.
   --  Otherwise, F is Empty, Idxs is null, and LHS is the LValue form of N.

   procedure Emit_Overflow_Check (V : GL_Value; N : N_Type_Conversion_Id)
     with Pre => Present (V) and then Is_Elementary_Type (V);
   --  Check that V is within the bounds of N's type.

   function Emit_Shift
     (Operation           : Node_Kind;
      LHS_Node, RHS_Node  : N_Subexpr_Id) return GL_Value
     with Pre  => Operation in N_Op_Shift, Post => Present (Emit_Shift'Result);
   --  Handle shift and rotate operations

   function Emit_Binary_Operation (N : N_Binary_Op_Id) return GL_Value
     with Post => Present (Emit_Binary_Operation'Result);
   --  Handle other binary operations

   function Emit_Unary_Operation (N : N_Unary_Op_Id) return GL_Value
     with Post => Present (Emit_Unary_Operation'Result);
   --  Handle unary operations

   function Emit_Literal (N : N_Subexpr_Id) return GL_Value
     with Post => Present (Emit_Literal'Result);
   --  Generate code for a literal

   function Emit_Undef (GT : GL_Type) return GL_Value
     with Pre => Present (GT), Post => Present (Emit_Undef'Result);
   --  Emit an undef appropriate for a return value of type TE

   procedure Emit_Assignment_Statement (N : N_Assignment_Statement_Id);
   procedure Emit_Pragma (N : N_Pragma_Id);

   function Emit_Attribute_Reference
     (N : N_Attribute_Reference_Id) return GL_Value
     with Post => Present (Emit_Attribute_Reference'Result);
   --  Handle N_Attribute_Reference nodes

   procedure Emit_Assignment
     (LValue       : GL_Value;
      Expr         : Opt_N_Subexpr_Id := Empty;
      Value        : GL_Value         := No_GL_Value;
      Forwards_OK  : Boolean          := True;
      Backwards_OK : Boolean          := True;
      VFA          : Boolean          := False)
     with Pre => Present (LValue)
                 and then (Present (Expr) or else Present (Value));
   --  Copy the value of the expression Expr or Value to LValue

   procedure Emit_Code_Statement (N : N_Code_Statement_Id);
   --  Generate code for inline asm

   function Build_Max
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Build_Select
        ((if Is_Floating_Point_Type (LHS) then
             F_Cmp (Real_OGT, LHS, RHS)
          else
             I_Cmp ((if Is_Unsigned_Type (LHS) then Int_UGT else Int_SGT),
                    LHS, RHS)),
         LHS, RHS, Name))
     with Pre  => Present (LHS) and then Present (RHS),
          Post => Present (Build_Max'Result);

   function Build_Min
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Build_Select
        ((if Is_Floating_Point_Type (LHS) then
             F_Cmp (Real_OLT, LHS, RHS)
          else
             I_Cmp ((if Is_Unsigned_Type (LHS) then Int_ULT else Int_SLT),
                     LHS, RHS)),
         LHS, RHS, Name))
     with Pre  => Present (LHS) and then Present (RHS),
          Post => Present (Build_Min'Result);

end GNATLLVM.Exprs;
