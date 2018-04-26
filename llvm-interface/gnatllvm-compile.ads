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

with Atree; use Atree;
with Einfo; use Einfo;
with Table;
with Types; use Types;

with GNATLLVM.Environment; use GNATLLVM.Environment;

package GNATLLVM.Compile is

   package Elaboration_Table is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 1024,
      Table_Increment      => 100,
      Table_Name           => "Elaboration_Table");
   --  Table of statements part of the current elaboration procedure

   procedure Emit_Library_Item (U : Node_Id);
   --  Generate code for the given library item

   procedure Emit (Node : Node_Id)
     with Pre => Present (Node);
   --  General compilation routine, called at the top-level

   procedure Emit_List (List : List_Id);
   --  Call Emit on every element of List

   function Emit_Expression (Node : Node_Id) return GL_Value
     with Pre  => Present (Node),
          Post => Present (Emit_Expression'Result);
   --  Compile an expression node to an LLVM value

   function Emit_LValue
     (Node : Node_Id; Clear : Boolean := True) return GL_Value
     with Pre  => Present (Node),
          Post => Present (Emit_LValue'Result);
   --  Compile an expression node to an LLVM value that can be used as an
   --  LValue. This function can be used to get a pointer to a value rather
   --  than the value itself (out parameters, simple accesses, etc).  If
   --  Clear is False, we don't reset the list used by Get_Matching_Value.

   procedure Add_To_LValue_List (V : GL_Value)
     with Pre => Present (V);
   --  Add V to the list that's searched by Get_Matching_Value

   function Get_Matching_Value (T : Entity_Id) return GL_Value
     with Pre  => Is_Type (T),
          Post => Present (Get_Matching_Value'Result);
   --  Find a value that's being computed by the current Emit_LValue
   --  recursion that has the same base type as T.

end GNATLLVM.Compile;
