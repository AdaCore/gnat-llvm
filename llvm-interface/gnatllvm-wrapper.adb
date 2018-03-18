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

with Interfaces.C.Strings;    use Interfaces.C.Strings;

package body GNATLLVM.Wrapper is

   function Create_TBAA_Scalar_Type_Node
     (MDBld : MD_Builder_T; Name : String; Root : Metadata_T)
     return Metadata_T is
      function Create_TBAA_Scalar_Type_Node_C
        (MDBld : MD_Builder_T;
         Name  : Interfaces.C.Strings.chars_ptr;
         Root  : Metadata_T)
        return Metadata_T;
      pragma Import (C, Create_TBAA_Scalar_Type_Node_C,
                     "Create_TBAA_Scalar_Type_Node_C");

      Name_Array  : aliased char_array := To_C (Name);
      Name_String : constant chars_ptr :=
        To_Chars_Ptr (Name_Array'Unchecked_Access);
   begin
      return Create_TBAA_Scalar_Type_Node_C (MDBld, Name_String, Root);
   end Create_TBAA_Scalar_Type_Node;

end GNATLLVM.Wrapper;
