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
with Sinfo;  use Sinfo;
with Types;  use Types;

with LLVM.Types; use LLVM.Types;

with GNATLLVM.Core;        use GNATLLVM.Core;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Types;       use GNATLLVM.Types;

package GNATLLVM.Records is

   function Create_Record_Type (TE : Entity_Id) return Type_T
     with Pre => Is_Record_Type (TE),
          Post => Present (Create_Record_Type'Result);
   --  Create a type for the record denoted by Def_Ident

   function Record_Field_Offset
     (V : GL_Value; Field : Entity_Id) return GL_Value
     with Pre  => Present (V)
                  and then Ekind_In (Field, E_Discriminant, E_Component),
          Post => Present (Record_Field_Offset'Result);
   --  Compute the offset of a given record field

   function Get_Record_Size_Complexity
     (TE : Entity_Id; For_Type : Boolean := False) return Natural
     with Pre => Is_Record_Type (TE);
   --  Return the complexity of computing the size of a record.  This roughly
   --  gives the number of "things" needed to access to compute the size.
   --  This returns zero iff the record type is of a constant size.

   function Get_Record_Type_Size
     (TE       : Entity_Id;
      V        : GL_Value;
      For_Type : Boolean := False) return GL_Value
     with Pre  => Is_Record_Type (TE),
          Post => Present (Get_Record_Type_Size'Result);

   function Emit_Record_Aggregate
     (Node : Node_Id; Result_So_Far : GL_Value) return GL_Value
     with Pre  => Nkind_In (Node, N_Aggregate, N_Extension_Aggregate)
                  and then Is_Record_Type (Full_Etype (Node))
                  and then Present (Result_So_Far),
          Post => Present (Emit_Record_Aggregate'Result);

end GNATLLVM.Records;
