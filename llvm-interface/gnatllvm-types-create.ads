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

with Ada.Unchecked_Conversion;

package GNATLLVM.Types.Create is

   function Create_Type (TE : Entity_Id) return Type_T
     with Pre  => Present (TE) and then TE = Get_Fullest_View (TE),
          Post => Present (Create_Type'Result);
   --  Given a GNAT type TE, build the corresponding LLVM type, building
   --  a GL_Type first if necessary.

   procedure Validate_And_Set_Alignment
     (E : Entity_Id; Align : Uint; Current_Align : ULL)
     with Pre => Present (E);
   --  Current_Align is the current alignment of E, either because it's
   --  the alignment of the LLVM type (if E is a type) or because it's the
   --  alignment of E's type (if E if an object).  Align is a proposed
   --  alignment for E.  See if it's valid (possibly issuing an error
   --  message if not) and set the alignment of E to that value or an
   --  acceptable value if Align isn't valid.

end GNATLLVM.Types.Create;
