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

with GNATLLVM.GLType;  use GNATLLVM.GLType;
with GNATLLVM.GLValue; use GNATLLVM.GLValue;

package GNATLLVM.Conversions is

   function Convert
     (V              : GL_Value;
      GT             : GL_Type;
      Float_Truncate : Boolean := False;
      Is_Unchecked   : Boolean := False) return GL_Value
     with Pre  => Is_Data (V) and then Present (GT)
                  and then Is_Elementary_Type (V),
          Post => Is_Data (Convert'Result)
                  and then Is_Elementary_Type (Convert'Result);
   --  Convert V to the type TE, with both the types of V and TE being
   --  elementary.

   function Convert
     (V, T           : GL_Value;
      Float_Truncate : Boolean := False;
      Is_Unchecked   : Boolean := False) return GL_Value is
     (Convert (V, Related_Type (T), Float_Truncate, Is_Unchecked))
     with Pre  => Is_Data (V) and then Is_Elementary_Type (V)
                  and then Is_Elementary_Type (T),
          Post => Is_Data (Convert'Result)
                  and then Is_Elementary_Type (Convert'Result);
   --  Variant of above where the type is that of another value (T)

   function Convert_Ref (V : GL_Value; GT : GL_Type) return GL_Value
     with Pre  => Is_Reference (V) and then Present (GT),
          Post => Is_Reference (Convert_Ref'Result);
   --  Convert V, which should be a reference, into a reference to GT

   function Convert_Ref
     (V : GL_Value; T : GL_Value) return GL_Value is
     (Convert_Ref (V, Related_Type (T)))
     with Pre  => Present (V) and then Present (T),
          Post => Is_Access_Type (Convert_Ref'Result);
   --  Likewise, but get type from V

   function Convert_To_Access (V : GL_Value; GT : GL_Type) return GL_Value
     with Pre  => Present (V) and then Present (GT),
          Post => Is_Access_Type (Convert_To_Access'Result);
   --  Convert Src, which should be an access or reference, into an access
   --  type GT

   function Convert_To_Access
     (V : GL_Value; T : GL_Value) return GL_Value is
     (Convert_To_Access (V, Related_Type (T)))
     with Pre  => Present (V) and then Present (T),
          Post => Is_Access_Type (Convert_To_Access'Result);
   --  Likewise, but get type from V

   function Convert_GT (V : GL_Value; GT : GL_Type) return GL_Value is
     (if    Is_Reference (V) then Convert_Ref (V, GT)
      elsif Full_Etype (Related_Type (V)) = Full_Etype (GT)
      then  From_Primitive (To_Primitive (V), GT)
      else  V)
     with Pre  => Is_Reference (V)
                  or else Full_Etype (Related_Type (V)) = Full_Etype (GT)
                  or else Is_Layout_Identical (V, GT),
          Post => Is_Layout_Identical (Related_Type (Convert_GT'Result), GT);
   --  Convert V, which is either a reference or whose current GL_Type
   --  is the same GNAT type as GT or has the same layout, to GT.

   function Emit_Conversion
     (N                   : Node_Id;
      GT                  : GL_Type;
      From_N              : Node_Id := Empty;
      For_LHS             : Boolean := False;
      Is_Unchecked        : Boolean := False;
      Need_Overflow_Check : Boolean := False;
      Float_Truncate      : Boolean := False;
      No_Truncation       : Boolean := False) return GL_Value
     with Pre  => Present (GT) and then Present (N)
                  and then not (Is_Unchecked and Need_Overflow_Check),
          Post => Present (Emit_Conversion'Result);
   --  Emit code to convert N to GT, optionally in unchecked mode
   --  and optionally with an overflow check.  From_N is the conversion node,
   --  if there is a corresponding source node.

   function Emit_Convert_Value (N : Node_Id; GT : GL_Type) return GL_Value is
     (Get (Emit_Conversion (N, GT), Object))
     with Pre  => Present (GT) and then Present (N),
          Post => Present (Emit_Convert_Value'Result);
   --  Emit code to convert N to GT and get it as a value

   function Convert_Pointer (V : GL_Value; GT : GL_Type) return GL_Value
     with Pre  => Is_Access_Type (V) and then Present (GT),
          Post => Is_Access_Type (Convert_Pointer'Result);
   --  V is a reference to some object.  Convert it to a reference to GT
   --  with the same relationship.

   function Strip_Complex_Conversions (N : Node_Id) return Node_Id;
   --  Remove any conversion from N, if Present, if they are record or array
   --  conversions that increase the complexity of the size of the
   --  type because the caller will be doing any needed conversions.

   function Strip_Conversions (N : Node_Id) return Node_Id;
   --  Likewise, but remove all conversions

   function Is_Unsigned_For_Convert (GT : GL_Type) return Boolean
     with Pre => Present (GT);
   --  True if we are to treate GT as unsigned for the purpose of a
   --  conversion.

   function Is_Parent_Of (T_Need, T_Have : Entity_Id) return Boolean
     with Pre => Is_Record_Type (T_Need) and then Is_Record_Type (T_Have);
   --  True if T_Have is a parent type of T_Need

end GNATLLVM.Conversions;
