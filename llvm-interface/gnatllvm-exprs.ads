------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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
with GNATLLVM.Variables;   use GNATLLVM.Variables;

package GNATLLVM.Exprs is
   --  This can't be named GNATLLVM.Expressions because it conflicts
   --  with Expressions in Sinfo,

   function Is_Safe_From (LHS : GL_Value; RHS : Node_Id) return Boolean is
     (Is_Pristine (LHS) or else Is_No_Elab_Needed (RHS))
     with Pre => Present (LHS) and then Present (RHS);
   --  True if we know that clobbering LHS won't change the value of
   --  RHS.  That's certainly true if LHS is pristine or RHS is a constant.
   --  Perhaps we can test more cases later, so this is really a placeholder.

   procedure LHS_And_Component_For_Assignment
     (N             : Node_Id;
      LHS           : out GL_Value;
      F             : out Entity_Id;
      Idxs          : out Access_GL_Value_Array;
      For_LHS       : Boolean := False;
      Only_Bitfield : Boolean := False)
     with Pre  => Present (N),
          Post => Present (LHS)
                  and then (No (F) or else Ekind_In (F, E_Component,
                                                     E_Discriminant));
   --  N is an expression that's used in a LHS context, either the LHS side
   --  of an N_Assignment_Statement or an actual corresponding to an Out
   --  (or in Out) parameter.  If N represents an field selection (if
   --  Only_Bitfield then only if that field is a bitfield), then LHS is
   --  the Prefix of that selection and F is the field being selected.  If
   --  N is an indexed reference, Idxs is a pointer to the list of indices.
   --  Otherwise, F is Empty, Idxs is null, and LHS is the LValue form of N.

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

   function Emit_Undef (GT : GL_Type) return GL_Value
     with Pre => Present (GT), Post => Present (Emit_Undef'Result);
   --  Emit an undef appropriate for a return value of type TE

   function Emit_Attribute_Reference (N : Node_Id) return GL_Value
     with Pre  => Nkind (N) = N_Attribute_Reference,
          Post => Present (Emit_Attribute_Reference'Result);
   --  Handle N_Attribute_Reference nodes

   procedure Emit_Assignment
     (LValue       : GL_Value;
      Expr         : Node_Id  := Empty;
      Value        : GL_Value := No_GL_Value;
      Forwards_OK  : Boolean  := True;
      Backwards_OK : Boolean  := True;
      VFA          : Boolean  := False)
     with Pre => Present (LValue)
                 and then (Present (Expr) or else Present (Value));
   --  Copy the value of the expression Expr or Value to LValue

   procedure Emit_Code_Statement (N : Node_Id)
     with Pre => Nkind (N) = N_Code_Statement;
   --  Generate code for inline asm

   function Build_Max
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Build_Select
        (I_Cmp ((if Is_Unsigned_Type (LHS) then Int_UGT else Int_SGT),
                LHS, RHS),
         LHS, RHS, Name))
     with Pre  => Present (LHS) and then Present (RHS),
          Post => Present (Build_Max'Result);

   function Build_Min
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Build_Select
        (I_Cmp ((if Is_Unsigned_Type (LHS) then Int_ULT else Int_SLT),
                LHS, RHS),
         LHS, RHS, Name))
     with Pre  => Present (LHS) and then Present (RHS),
          Post => Present (Build_Min'Result);

end GNATLLVM.Exprs;
