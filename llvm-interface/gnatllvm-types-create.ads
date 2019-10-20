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

   procedure Copy_Annotations (In_TE, Out_TE : Entity_Id)
     with Pre => Is_Type (In_TE) and then Is_Type (Out_TE)
                 and then In_TE = Get_Fullest_View (Out_TE);
   --  Copy any annotations we made from In_TE to Out_TE

   procedure Annotate_Object_Size_And_Alignment
     (Def_Ident : Entity_Id; GT : GL_Type; Want_Max : Boolean := True)
     with Pre => not Is_Type (Def_Ident);
     --  Peform back-annotation of size and alignment of Def_Ident.  If
     --  Want_Max is True, we want the maximum size of GT, in case
     --  its an unconstrained record type.

   function Validate_Alignment
     (E : Entity_Id; Align : Uint; Current_Align : Nat) return Uint
     with Pre  => Present (E),
          Post => Present (Validate_Alignment'Result);
   --  Current_Align is the current alignment of E, either because it's the
   --  alignment of the LLVM type (if E is a type) or because it's the
   --  alignment of E's type (if E if an object).  Align is a proposed
   --  alignment for E.  See if it's valid (possibly issuing an error
   --  message if not) and return it if so or some other acceptable value
   --  if not.

   function Validate_Size
     (E             : Entity_Id;
      GT            : GL_Type;
      Size          : Uint;
      For_Type      : Boolean := False;
      For_Component : Boolean := False;
      Zero_Allowed  : Boolean := False;
      Is_RM_Size    : Boolean := False) return Uint
     with Pre => Present (E) and then Present (GT);
   --  Validate that size Size is valid for entity E of type GT.  For_Type
   --  is True if we're doing this for a type, For_Component if this is
   --  for the component of an array and Zero_Allowed if a size of zero is
   --  considered a valid size.  Give an error message if needed and return
   --  a valid size.  Is_RM_Size indicates this size is from RM_Size;
   --  this may change the text of the error message.

end GNATLLVM.Types.Create;
