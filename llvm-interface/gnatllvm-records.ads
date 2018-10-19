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

with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Types;       use GNATLLVM.Types;

package GNATLLVM.Records is

   function Create_Record_Type (TE : Entity_Id) return Type_T
     with Pre => Is_Record_Type (TE),
          Post => Present (Create_Record_Type'Result);
   --  Create a type for the record denoted by Def_Ident

   function Use_Discriminant_For_Bound (E : Entity_Id) return GL_Value
     with Pre  => Ekind (E) = E_Discriminant,
          Post => Present (Use_Discriminant_For_Bound'Result);
   --  E is an E_Discriminant that we've run into while emitting an expression.
   --  If we are expecting one as a possible bound, evaluate this discriminant
   --  as required to compute that bound.

   function Record_Field_Offset
     (V : GL_Value; Field : Entity_Id) return GL_Value
     with Pre  => Present (V)
                  and then Ekind_In (Field, E_Discriminant, E_Component),
          Post => Present (Record_Field_Offset'Result);
   --  Compute the offset of a given record field

   function Get_Record_Size_Complexity
     (TE : Entity_Id; Max_Size : Boolean := False) return Nat
     with Pre => Is_Record_Type (TE);
   --  Return the complexity of computing the size of a record.  This roughly
   --  gives the number of "things" needed to access to compute the size.
   --  This returns zero iff the record type is of a constant size.

   function Get_Record_Type_Size
     (TE       : Entity_Id;
      V        : GL_Value;
      Max_Size : Boolean := False) return GL_Value
     with Pre  => Is_Record_Type (TE),
          Post => Present (Get_Record_Type_Size'Result);
   --  Like Get_Type_Size, but only for record types

   function IDS_Record_Type_Size
     (TE       : Entity_Id;
      V        : GL_Value;
      Max_Size : Boolean := False) return IDS
     with Pre  => Is_Record_Type (TE),
          Post => Present (IDS_Record_Type_Size'Result);

   function Emit_Record_Aggregate
     (N : Node_Id; Result_So_Far : GL_Value) return GL_Value
     with Pre  => Nkind_In (N, N_Aggregate, N_Extension_Aggregate)
                  and then Is_Record_Type (Full_Etype (N)),
          Post => Present (Emit_Record_Aggregate'Result);
   --  Emit code for a record aggregate at Node.  Result_So_Far, if
   --  Present, contain any fields already filled in for the record.

   function Emit_Field_Position (E : Entity_Id; V : GL_Value) return GL_Value
     with Pre  => Ekind_In (E, E_Discriminant, E_Component),
          Post => No (Emit_Field_Position'Result)
                  or else (Type_Of (Emit_Field_Position'Result) =
                             LLVM_Size_Type);
   --  Compute and return the position in bytes of the field specified
   --  by E from the start of its type as a value of Size_Type.  If
   --  Present, V is a value of that type, which is used in the case
   --  of a discriminated record.

   function Get_Field_Ordinal
     (F_Idx : Field_Info_Id; TE : Entity_Id) return unsigned
   with Pre => Present (F_Idx) and then Is_Record_Type (TE);
   --  Return the index of the field denoted by F_Idx in type TE.

   function Align_To (V, Cur_Align, Must_Align : GL_Value) return GL_Value
     with Pre => Present (V), Post => Present (Align_To'Result);
   --  V is a value aligned to Cur_Align.  Ensure that it's aligned to
   --  Align_To.

   --  The following are debug procedures to print information about records
   --  and fields.

   procedure Print_Field_Info (E : Entity_Id);
   procedure Print_Record_Info (TE : Entity_Id);
   pragma Export (Ada, Print_Field_Info,  "dfi");
   pragma Export (Ada, Print_Record_Info, "dri");

end GNATLLVM.Records;
