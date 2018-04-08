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

with Atree;    use Atree;
with Sinfo;    use Sinfo;
with Table;    use Table;
with Types;    use Types;

with LLVM.Types;    use LLVM.Types;

with GNATLLVM.Environment; use GNATLLVM.Environment;

package GNATLLVM.Subprograms is

   package Nested_Functions_Table is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Nested_Function_Table");
   --  Table of nested functions to elaborate

   function Get_Static_Link (Env : Environ; Node : Entity_Id) return GL_Value
     with Pre  => Env /= null and then Present (Node),
          Post => Present (Get_Static_Link'Result);
   --  Build and return the static link to pass to a call to Node

   function Emit_Call
     (Env : Environ; Call_Node : Node_Id) return Value_T
     with Pre  => Env /= null and then Nkind (Call_Node) in N_Subprogram_Call,
          Post => Present (Emit_Call'Result);
   --  Helper for Emit/Emit_Expression: compile a call statement/expression and
   --  return its result value.

   procedure Emit_LCH_Call (Env : Environ; Node : Node_Id)
     with Pre  => Env /= null and then Present (Node);
   --  Generate a call to __gnat_last_chance_handler

   procedure Emit_One_Body (Env : Environ; Node : Node_Id)
     with Pre => Env /= null and then Present (Node);
   --  Generate code for one given subprogram body

   function Emit_Subprogram_Decl
     (Env : Environ; Subp_Spec : Node_Id) return GL_Value
     with Pre  => Env /= null,
          Post => Present (Emit_Subprogram_Decl'Result);
   --  Compile a subprogram declaration, save the corresponding LLVM value to
   --  the environment and return it.

   procedure Emit_Subprogram_Body (Env : Environ; Node : Node_Id)
     with Pre => Env /= null and then Present (Node);
   --  Compile a subprogram body and save it in the environment

   function Node_Enclosing_Subprogram (Node : Node_Id) return Node_Id
     with Pre  => Present (Node),
          Post => Present (Node_Enclosing_Subprogram'Result);
   --  Return the enclosing subprogram containing Node.

end GNATLLVM.Subprograms;
