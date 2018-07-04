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

with Interfaces.C; use Interfaces.C;

package body GNATLLVM.Wrapper is

   ----------------------------------
   -- Create_TBAA_Scalar_Type_Node --
   ----------------------------------

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

   ------------------------
   -- Build_Extract_Value --
   ------------------------

   function Build_Extract_Value
     (Bld      : Builder_T;
      Aggr     : Value_T;
      Idx_List : System.Address;
      Num_Idx  : unsigned;
      Name     : String) return Value_T
   is
      function Build_Extract_Value_C
        (Bld      : Builder_T;
         Aggr     : Value_T;
         Idx_List : System.Address;
         Num_Idx  : unsigned;
         Name     : String) return Value_T;
      pragma Import (C, Build_Extract_Value_C, "Build_Extract_Value_C");

   begin
      return Build_Extract_Value_C (Bld, Aggr, Idx_List, Num_Idx,
                                    Name & ASCII.NUL);
   end Build_Extract_Value;

   ------------------------
   -- Build_Insert_Value --
   ------------------------

   function Build_Insert_Value
     (Bld      : Builder_T;
      Aggr     : Value_T;
      Elt      : Value_T;
      Idx_List : System.Address;
      Num_Idx  : unsigned;
      Name     : String) return Value_T
   is
      function Build_Insert_Value_C
        (Bld      : Builder_T;
         Aggr     : Value_T;
         Elt      : Value_T;
         Idx_List : System.Address;
         Num_Idx  : unsigned;
         Name     : String) return Value_T;
      pragma Import (C, Build_Insert_Value_C, "Build_Insert_Value_C");

   begin
      return Build_Insert_Value_C (Bld, Aggr, Elt, Idx_List, Num_Idx,
                                   Name & ASCII.NUL);
   end Build_Insert_Value;

   ----------------------
   -- LLVM_Init_Module --
   ----------------------

   procedure LLVM_Init_Module
     (Module         : Module_T;
      Filename       : String;
      Target_Machine : Target_Machine_T)
   is
      procedure LLVM_Init_Module_C
        (Module         : Module_T;
         Filename       : String;
         Target_Machine : Target_Machine_T);
      pragma Import (C, LLVM_Init_Module_C, "LLVM_Init_Module");

   begin
      LLVM_Init_Module_C (Module, Filename & ASCII.NUL, Target_Machine);
   end LLVM_Init_Module;

   -----------------------
   -- Create_Debug_File --
   -----------------------

   function Create_Debug_File
     (Bld : DI_Builder_T; Name, Dir : String) return Metadata_T
   is
      function Create_Debug_File_C
        (Bld : DI_Builder_T; Name, Dir : String) return Metadata_T;
      pragma Import (C, Create_Debug_File_C, "Create_Debug_File_C");
   begin
      return Create_Debug_File_C (Bld, Name & ASCII.NUL, Dir & ASCII.NUL);
   end Create_Debug_File;

   -----------------------------
   -- Create_Debug_Subprogram --
   -----------------------------

   function Create_Debug_Subprogram
     (Bld            : DI_Builder_T;
      Func           : Value_T;
      File           : Metadata_T;
      Name, Ext_Name : String;
      Lineno         : Logical_Line_Number) return Metadata_T
   is
      function Create_Debug_Subprogram_C
        (Bld            : DI_Builder_T;
         Func           : Value_T;
         File           : Metadata_T;
         Name, Ext_Name : String;
         Lineno         : Logical_Line_Number) return Metadata_T;
      pragma Import (C, Create_Debug_Subprogram_C,
                     "Create_Debug_Subprogram_C");
   begin
      return Create_Debug_Subprogram_C (Bld, Func, File, Name & ASCII.NUL,
                                        Ext_Name & ASCII.NUL, Lineno);
   end Create_Debug_Subprogram;

   -------------------------
   -- Is_Layout_Identical --
   -------------------------

   function Is_Layout_Identical (T1, T2 : Type_T) return Boolean is
      function Is_Layout_Identical_C (T1, T2 : Type_T) return Bool_T;
      pragma Import (C, Is_Layout_Identical_C, "Is_Layout_Identical");
   begin
      return Is_Layout_Identical_C (T1, T2) /= 0;
   end Is_Layout_Identical;

end GNATLLVM.Wrapper;
