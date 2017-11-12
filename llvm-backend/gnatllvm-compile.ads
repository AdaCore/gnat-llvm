------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2017, AdaCore                     --
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
with Sinfo; use Sinfo;
with Types; use Types;

with LLVM.Types; use LLVM.Types;

with GNATLLVM.Environment; use GNATLLVM.Environment;

package GNATLLVM.Compile is

   pragma Annotate (Xcov, Exempt_On, "Defensive programming");
   procedure Emit_Compilation_Unit
     (Env : Environ; Node : Node_Id; Emit_Library_Unit : Boolean)
     with Pre => Env /= null
       and then Nkind (Node) = N_Compilation_Unit;

   procedure Emit
     (Env : Environ; Node : Node_Id)
     with Pre => Env /= null;
   --  General compilation routine, called at the top-level.

   function Emit_Expression
     (Env : Environ; Node : Node_Id) return Value_T
     with Pre => Env /= null;
   --  Compile an expression node to an LLVM value.

   function Emit_LValue (Env : Environ; Node : Node_Id) return Value_T
     with Pre => Env /= null;
   --  Compile an expression node to an LLVM value that can be used as an
   --  LValue. This function can be used to get a pointer to a value rather
   --  than the value itself (out parameters, simple accesses, etc.)
   pragma Annotate (Xcov, Exempt_Off, "Defensive programming");

end GNATLLVM.Compile;
