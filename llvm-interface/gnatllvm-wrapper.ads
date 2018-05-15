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

with Interfaces.C; use Interfaces.C;

package GNATLLVM.Wrapper is

   function Create_MDBuilder_In_Context (Ctx : Context_T) return MD_Builder_T;
   pragma Import (C, Create_MDBuilder_In_Context,
                  "Create_MDBuilder_In_Context");

   function Create_TBAA_Root (MDBld : MD_Builder_T) return Metadata_T;
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

   procedure Set_Alloca_Align (Inst : Value_T; Align : unsigned);
   pragma Import (C, Set_Alloca_Align, "Set_Alloca_Align");

   function Build_Extract_Value
     (Bld      : Builder_T;
      Aggr     : Value_T;
      Idx_List : System.Address;
      Num_Idx  : unsigned;
      Name     : String) return Value_T;

   function Build_Insert_Value
     (Bld      : Builder_T;
      Aggr     : Value_T;
      Elt      : Value_T;
      Idx_List : System.Address;
      Num_Idx  : unsigned;
      Name     : String) return Value_T;

   function LLVM_Init_Module
     (Module   : Module_T; Filename : String) return Integer;
   --  Initialize the LLVM module.  Returns 0 if it succeeds.

   function LLVM_Write_Module
     (Module   : Module_T;
      Object   : Boolean;
      Filename : String) return Integer;

   --  Functions for creating debug information

   function Create_Debug_Builder (Module : Module_T) return DI_Builder_T;
   pragma Import (C, Create_Debug_Builder, "Create_Debug_Builder");
   --  Create a DIBuilder and return it

   function Create_Debug_File
     (Bld : DI_Builder_T; Name, Dir : String) return Metadata_T;

   function Create_Debug_Compile_Unit
     (Bld : DI_Builder_T; File : Metadata_T) return Metadata_T;
   pragma Import (C, Create_Debug_Compile_Unit, "Create_Debug_Compile_Unit");

   function Create_Debug_Subprogram
     (Bld            : DI_Builder_T;
      Func           : Value_T;
      File           : Metadata_T;
      Name, Ext_Name : String;
      Lineno         : Integer) return Metadata_T;

   function Create_Debug_Lexical_Block
     (Bld : DI_Builder_T; Scope, File : Metadata_T; Line, Column : Integer)
     return Metadata_T;
   pragma Import (C, Create_Debug_Lexical_Block, "Create_Debug_Lexical_Block");

   procedure Finalize_Debug_Info (Bld : DI_Builder_T);
   pragma Import (C, Finalize_Debug_Info, "Finalize_Debug_Info");

   procedure Set_Debug_Loc
     (Bld : Builder_T; Subp : Metadata_T; Line, Column : Integer);
   pragma Import (C, Set_Debug_Loc, "Set_Debug_Loc");
end GNATLLVM.Wrapper;
