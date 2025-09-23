------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                  Copyright (C) 2025, AdaCore                             --
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

with Repinfo;    use Repinfo;

package GNATLLVM.Records.Debug is
   --  Subpackage for creating the LLVM debuginfo for a given record.

   function Hash (F : Record_Field_Kind_Id) return Ada.Containers.Hash_Type
     is (Hash_Type (F));
   --  A hash function for use in the discriminant map.

   package Discriminant_Map_Pkg is new Ada.Containers.Hashed_Maps
     (Key_Type        => Record_Field_Kind_Id,
      Element_Type    => Metadata_T,
      Hash            => Hash,
      Equivalent_Keys => "=");
   --  A map from a discriminant's (canonical) entity to the LLVM debuginfo.

   subtype Discriminant_Map is Discriminant_Map_Pkg.Map;
   --  The type of a discriminant map.

   function Canonical_Discriminant_For (E : Entity_Id) return Entity_Id;
   --  A helper function to find the canonical discriminant (for the
   --  purposes of debuginfo generation) given some discriminant.

   function Create_Record_Debug_Info (TE : Void_Or_Type_Kind_Id;
                                      Original_Type : Entity_Id;
                                      Debug_Scope : Metadata_T;
                                      Name : String;
                                      Size : ULL;
                                      Align : Nat;
                                      S : Source_Ptr) return Metadata_T;
   --  Create the LLVM debuginfo for a given record.

   function Convert_To_Dwarf_Expression (Expr : Node_Ref_Or_Val;
                                         Original_Type : Entity_Id)
     return Metadata_T;
   --  Convert a back annotation expression to a DWARF expression.
   --  Returns the LLVM metadata for the expression.  Note that this
   --  may return a DIExpression, but if the expression it is just a
   --  constant it will return a Constant.

end GNATLLVM.Records.Debug;
