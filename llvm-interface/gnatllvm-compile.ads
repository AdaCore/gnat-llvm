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

with GNATLLVM.GLType;  use GNATLLVM.GLType;
with GNATLLVM.GLValue; use GNATLLVM.GLValue;

package GNATLLVM.Compile is

   procedure GNAT_To_LLVM (GNAT_Root : N_Compilation_Unit_Id);
   --  Generate LLVM from GNAT_Root and then compile it

   procedure Emit (N : Node_Id)
     with Pre => Present (N);
   --  Emit code for the tree starting at N

   procedure Emit (List : List_Id);
   --  Emit a node and every element of a (possibly empty) List

   function Emit
     (N          : N_Subexpr_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False;
      Prefer_LHS : Boolean  := False) return GL_Value
     with Post => Present (Emit'Result);
   --  Compile an expression node to an LLVM value or a reference to the
   --  value, whichever involves the least work. LHS may be an expression
   --  to which the value should be assigned. If the assignment was done,
   --  return LHS. For_LHS is true if we're evaluating this for the LHS of
   --  an assignment. Prefer_LHS is true if we're in a context (like
   --  'Address) where we prefer returning an LValue if we can, but we are
   --  allowed to have a context where the result isn't an LHS.

   procedure Push_Suppress_Overflow;
   procedure Pop_Suppress_Overflow;
   --  Push and pop the level of supressing overflow messages. This is used
   --  during trial elaborations, such as in Is_No_Elab_Needed to avoid
   --  producing error messages for values that may not be used and
   --  certainly will not be used in that context.

   function Emit_LValue
     (N          : N_Subexpr_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False) return GL_Value
     with Post => Present (Emit_LValue'Result);
   --  Compile an expression node to an LLVM value that's a reference. If
   --  N corresponds to an LValue in the language, then the result will
   --  also be an LValue. LHS, For_LHS is like for Emit.

   function Emit_Safe_LValue
     (N          : N_Subexpr_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False) return GL_Value
     with Post => Present (Emit_Safe_LValue'Result);
   --  Likewise, but push the LValue pair table so we compute this as
   --  a safe subexpression. LHS is like for Emit.

   function Emit_Expression
     (N       : N_Subexpr_Id;
      LHS     : GL_Value := No_GL_Value) return GL_Value
   is
     (Get (To_Primitive (Emit (N, LHS => LHS)), Object))
     with Post => Is_Primitive_GL_Type (Emit_Expression'Result);
   --  Likewise, but return something that's to be used as a value (but
   --  may nevertheless be a reference if its type is of variable size).
   --  LHS is like for Emit. It will always be the primitive form.

   function Emit_Safe_Expr
     (N : N_Subexpr_Id; LHS : GL_Value := No_GL_Value) return GL_Value
     with Post => Present (Emit_Safe_Expr'Result);
   --  Like Emit_Primitive_Expression, but push the LValue pair table
   --  so we compute this as a safe subexpression. LHS is like for
   --  Emit.

   function Simple_Value_Action
     (N : N_Expression_With_Actions_Id; Has_All : out Boolean)
     return Opt_N_Subexpr_Id;
   --  If N just declares the value it returns, return the initializer
   --  of that value; otherwise return Empty. Has_All is True if we
   --  have an N_Explicit_Dereference of the expression.

   procedure Process_Freeze_Entity (N : N_Freeze_Entity_Id);
   --  Process the actual freezing denoted by node N

   procedure Record_Code_Position (E : E_Package_Id);
   procedure Insert_Code_For      (E : E_Package_Id);
   --  When we have a package body with a Freeze_Node, we need to record the
   --  position in the code to place that code for that package body and
   --  then insert it at the location of the Freeze_Node.

end GNATLLVM.Compile;
