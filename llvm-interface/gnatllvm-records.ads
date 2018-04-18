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

with Atree;  use Atree;
with Einfo;  use Einfo;
with Types;  use Types;

with LLVM.Types; use LLVM.Types;

with GNATLLVM.Environment; use GNATLLVM.Environment;

package GNATLLVM.Records is

   function Create_Record_Type (Def_Ident : Entity_Id) return Type_T
     with Pre  => Present (Def_Ident),
          Post => Present (Create_Record_Type'Result);
   --  Create a type for the record denoted by Def_Ident

   function Record_Field_Offset
     (Record_Ptr : Value_T; Record_Field : Node_Id) return Value_T
     with Pre  => Present (Record_Ptr) and then Present (Record_Field),
          Post => Present (Record_Field_Offset'Result);
   --  Compute the offset of a given record field

   function Record_With_Dynamic_Size (T : Entity_Id) return Boolean
     with Pre => Is_Type (T);
   --  Return True is T denotes a record type with a dynamic size

   function Get_Record_Type_Size
     (TE       : Entity_Id;
      V        : GL_Value;
      For_Type : Boolean := False) return GL_Value
     with Pre  => Present (TE), Post => Present (Get_Record_Type_Size'Result);

end GNATLLVM.Records;
