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

with Sinfo; use Sinfo;

with GNATLLVM.GLType;       use GNATLLVM.GLType;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;

package GNATLLVM.Compile is

   procedure GNAT_To_LLVM (GNAT_Root : Node_Id)
     with Pre => Nkind (GNAT_Root) = N_Compilation_Unit;
   --  Generate LLVM from GNAT_Root and then compile it

   procedure Emit (N : Node_Id)
     with Pre => Present (N);
   --  Emit code for the tree starting at N.

   procedure Emit (List : List_Id; Starting_At : Node_Id := Empty);
   --  Emit a node and every element of a (possibly empty) List.  In the
   --  latter case, if Starting_At is Present, it indicates the starting
   --  point of nodes to emit; otherwise the entire list is emitted.

   function Emit
     (N          : Node_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False;
      Prefer_LHS : Boolean  := False) return GL_Value
     with Pre => Present (N), Post => Present (Emit'Result);
   --  Compile an expression node to an LLVM value or a reference to the
   --  value, whichever involves the least work.  LHS may be an expression
   --  to which the value should be assigned.  If the assignment was done,
   --  return LHS.  For_LHS is true if we're evaluating this for the LHS
   --  of an assignment.  Prefer_LHS is true if we're in a context (like
   --  'Address) where we prefer returning an LValue if we can, but we are
   --  allowed to have a context where the result isn't an LHS.

   function Emit_LValue
     (N          : Node_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False) return GL_Value
     with Pre => Present (N), Post => Present (Emit_LValue'Result);
   --  Compile an expression node to an LLVM value that's a reference.
   --  If N corresponds to an LValue in the language, then the result
   --  will also be an LValue.  LHS, For_LHS is like for Emit.

   function Emit_Safe_LValue
     (N          : Node_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False) return GL_Value
     with Pre => Present (N), Post => Present (Emit_Safe_LValue'Result);
   --  Likewise, but push the LValue pair table so we compute this as
   --  a safe subexpression.  LHS is like for Emit.

   function Emit_Expression
     (N       : Node_Id;
      LHS     : GL_Value := No_GL_Value) return GL_Value
   is
     (To_Primitive (Get (Emit (N, LHS => LHS), Object)))
     with Pre  => Present (N),
          Post => Is_Primitive_GL_Type (Emit_Expression'Result);
   --  Likewise, but return something that's to be used as a value (but
   --  may nevertheless be a reference if its type is of variable size).
   --  LHS is like for Emit.  It will always be the primitive form.

   function Emit_Safe_Expr
     (N : Node_Id; LHS : GL_Value := No_GL_Value) return GL_Value
     with Pre => Present (N), Post => Present (Emit_Safe_Expr'Result);
   --  Like Emit_Primitive_Expression, but push the LValue pair table
   --  so we compute this as a safe subexpression.  LHS is like for
   --  Emit.

   procedure Process_Freeze_Entity (N : Node_Id)
     with Pre => Nkind (N) = N_Freeze_Entity;
   --  Process the actual freezing denoted by node N

end GNATLLVM.Compile;
