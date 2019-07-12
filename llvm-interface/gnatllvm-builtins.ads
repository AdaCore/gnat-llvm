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

with GNATLLVM.GLType;  use GNATLLVM.GLType;
with GNATLLVM.GLValue; use GNATLLVM.GLValue;

package GNATLLVM.Builtins is

   --  When we want to create an overloaded intrinsic, we need to specify
   --  what operand signature the intrinsic has.  The following are those
   --  that we currently support.

   type Overloaded_Intrinsic_Kind is
     (Unary, Binary, Boolean_And_Data, Memcpy, Memset);

   function Build_Intrinsic
     (Kind : Overloaded_Intrinsic_Kind;
      Name : String;
      GT   : GL_Type) return GL_Value
     with Pre  => Is_Primitive_GL_Type (GT) and then not Unknown_RM_Size (GT),
          Post => Present (Build_Intrinsic'Result);
   --  Build an intrinsic function of the specified type, name, and kind

   function Emit_Intrinsic_Call (N : Node_Id; Subp : Entity_Id) return GL_Value
     with Pre  => Nkind (N) in N_Subprogram_Call;
   --  If Subp is an intrinsic that we know how to handle, emit the LLVM
   --  for it and return the result.  Otherwise, No_GL_Value.

end GNATLLVM.Builtins;
