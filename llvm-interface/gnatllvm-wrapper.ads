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

with System;

with LLVM.Types; use LLVM.Types;

with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

package GNATLLVM.Wrapper is

   type MD_Builder_T is new System.Address;
   --  Metadata builder type: opaque for us.

   function Create_MDBuilder_In_Context
     (Ctx : LLVM.Types.Context_T) return MD_Builder_T;
   pragma Import (C, Create_MDBuilder_In_Context,
                  "Create_MDBuilder_In_Context");

   function Create_TBAA_Root (MDBld : MD_Builder_T)
     return LLVM.Types.Metadata_T;
   pragma Import (C, Create_TBAA_Root, "Create_TBAA_Root");
   --  Create the root of the TBAA metadata tree

   function Create_TBAA_Scalar_Type_Node
     (MDBld : MD_Builder_T;
      Name  : String;
      Parent  : Metadata_T) return Metadata_T;
   --  Create a TBAA metadata node for a scalar type

   function Create_TBAA_Access_Tag
     (MDBld                  : MD_Builder_T;
      Base_Type, Access_Type : Metadata_T;
      Offset                 : unsigned_long_long) return Metadata_T;
   pragma Import (C, Create_TBAA_Access_Tag, "Create_TBAA_Access_Tag");

   procedure Set_Volatile (Value : Value_T);
   pragma Import (C, Set_Volatile, "Set_Volatile");

   procedure Add_TBAA_Access (Value : Value_T; TBAA : Metadata_T);
   pragma Import (C, Add_TBAA_Access, "Add_TBAA_Access");

   function LLVM_Init_Module
     (Module   : LLVM.Types.Module_T;
      Filename : String) return Integer;
   --  Initialize the LLVM module.  Returns 0 if it succeeds.

   function LLVM_Write_Module
     (Module   : LLVM.Types.Module_T;
      Object   : Boolean;
      Filename : String) return Integer;

end GNATLLVM.Wrapper;
