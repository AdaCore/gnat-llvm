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

with Sinfo; use Sinfo;

with GNATLLVM.GLValue;     use GNATLLVM.GLValue;

package GNATLLVM.Compile is

   procedure Emit (N : Node_Id)
     with Pre => Present (N);
   procedure Emit (List : List_Id; Starting_At : Node_Id := Empty);
   --  Emit a node and every element of a (possibly empty) List.  In the
   --  latter case, if Starting_At is Present, it indicates the starting
   --  point of nodes to emit; otherwise the entire list is emitted.

   function Emit_Expression (N : Node_Id) return GL_Value
     with Pre => Present (N), Post => Present (Emit_Expression'Result);
   --  Compile an expression node to an LLVM value

   function Emit_Safe_Expr (N : Node_Id) return GL_Value
     with Pre => Present (N), Post => Present (Emit_Safe_Expr'Result);
   --  Likewise, but push the LValue pair table so we compute this as
   --  a safe subexpression.

   function Emit_LValue (N : Node_Id; Clear : Boolean := True) return GL_Value
     with Pre  => Present (N),
          Post => Present (Emit_LValue'Result);
   --  Compile an expression node to an LLVM value that can be used as an
   --  LValue. This function can be used to get a pointer to a value rather
   --  than the value itself (out parameters, simple accesses, etc).  If
   --  Clear is False, we don't reset the list used by Get_Matching_Value.

   procedure Process_Freeze_Entity (N : Node_Id)
     with Pre => Nkind (N) = N_Freeze_Entity;
   --  Process the actual freezing denoted by node N

end GNATLLVM.Compile;
