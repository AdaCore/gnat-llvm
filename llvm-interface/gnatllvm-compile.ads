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

with GNATLLVM.GLValue;     use GNATLLVM.GLValue;

package GNATLLVM.Compile is

   procedure Emit_Library_Item (U : Node_Id)
     with Pre => Present (U);
   --  Generate code for the given library item

   procedure Emit (N : Node_Id)
     with Pre => Present (N);
   procedure Emit (List : List_Id);
   --  Emit a node and every element of a (possibly empty) List

   function Emit_Expression (N : Node_Id) return GL_Value
     with Pre => Present (N), Post => Present (Emit_Expression'Result);
   --  Compile an expression node to an LLVM value

   procedure Emit_Assignment
     (LValue                    : GL_Value;
      Orig_E                    : Node_Id;
      E_Value                   : GL_Value;
      Forwards_OK, Backwards_OK : Boolean)
     with Pre => Present (LValue) or else Present (Orig_E);
   --  Copy the value of the expression E to LValue with the specified
   --  destination and expression types.

   function Emit_LValue (N : Node_Id; Clear : Boolean := True) return GL_Value
     with Pre  => Present (N),
          Post => Present (Emit_LValue'Result);
   --  Compile an expression node to an LLVM value that can be used as an
   --  LValue. This function can be used to get a pointer to a value rather
   --  than the value itself (out parameters, simple accesses, etc).  If
   --  Clear is False, we don't reset the list used by Get_Matching_Value.

   In_Main_Unit             : Boolean := False;
   --  True if we're currently processing the main unit

end GNATLLVM.Compile;
