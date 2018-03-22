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

package body GNATLLVM.Wrapper is

   function Create_TBAA_Scalar_Type_Node
     (MDBld  : MD_Builder_T;
      Name   : String;
      Parent : Metadata_T)
     return Metadata_T is
      function Create_TBAA_Scalar_Type_Node_C
        (MDBld  : MD_Builder_T;
         Name   : String;
         Parent : Metadata_T)
        return Metadata_T;
      pragma Import (C, Create_TBAA_Scalar_Type_Node_C,
                     "Create_TBAA_Scalar_Type_Node_C");

   begin
      return Create_TBAA_Scalar_Type_Node_C (MDBld, Name & ASCII.NUL, Parent);
   end Create_TBAA_Scalar_Type_Node;

   ----------------------
   -- LLVM_Init_Module --
   ----------------------

   function LLVM_Init_Module
     (Module   : LLVM.Types.Module_T;
      Filename : String) return Integer
   is
      function LLVM_Init_Module_C
        (Module   : LLVM.Types.Module_T;
         Filename : String) return Integer;
      pragma Import (C, LLVM_Init_Module_C, "LLVM_Init_Module");
   begin
      return LLVM_Init_Module_C (Module, Filename & ASCII.NUL);
   end LLVM_Init_Module;

   -----------------------
   -- LLVM_Write_Module --
   -----------------------

   function LLVM_Write_Module
     (Module   : LLVM.Types.Module_T;
      Object   : Boolean;
      Filename : String) return Integer
   is
      function LLVM_Write_Module_C
        (Module   : LLVM.Types.Module_T;
         Object   : Integer;
         Filename : String) return Integer;
      pragma Import (C, LLVM_Write_Module_C, "LLVM_Write_Module");

   begin
      return LLVM_Write_Module_C
        (Module, Boolean'Pos (Object), Filename & ASCII.NUL);
   end LLVM_Write_Module;

end GNATLLVM.Wrapper;
