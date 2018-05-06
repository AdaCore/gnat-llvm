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

with Sinfo;  use Sinfo;

with LLVM.Core;  use LLVM.Core;

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
   --  Like Get_Type_Size, but only for record types

   function Emit_Record_Aggregate
     (Node : Node_Id; Result_So_Far : GL_Value) return GL_Value
     with Pre  => Nkind_In (Node, N_Aggregate, N_Extension_Aggregate)
                  and then Is_Record_Type (Full_Etype (Node))
                  and then Present (Result_So_Far),
          Post => Present (Emit_Record_Aggregate'Result);
   --  Emit code for a record aggregate at Node.  Result_So_Far are any
   --  fields already filled in for the record or undef if none have.

   function Get_Field_Offset (T : Type_T; Idx : Nat) return GL_Value
     with Pre  => Get_Type_Kind (T) = Struct_Type_Kind,
          Post => Type_Of (Get_Field_Offset'Result) = LLVM_Size_Type
                  and then Is_Constant (LLVM_Value (Get_Field_Offset'Result));
   --  Given an LLVM record type T and an index Idx giving the ordinal of
   --  the field in the record, return a constant of Size_Type representing
   --  the position (in bytes) of that field in the record.

   function Emit_Field_Position (E : Entity_Id; V : GL_Value) return GL_Value
     with Pre  => Ekind_In (E, E_Discriminant, E_Component)
                  and then Present (V),
          Post => Type_Of (Emit_Field_Position'Result) = LLVM_Size_Type;
   --  Compute and return the position in bytes of the field specified by E
   --  from the start of its type as a value of Size_Type.  V is a value
   --  of that type, which is used in the case of a discriminated record.

end GNATLLVM.Records;
