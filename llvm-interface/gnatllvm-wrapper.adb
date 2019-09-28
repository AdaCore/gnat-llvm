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

with Interfaces.C; use Interfaces.C;

package body GNATLLVM.Wrapper is

   ----------------------------------
   -- Create_TBAA_Scalar_Type_Node --
   ----------------------------------

   function Create_TBAA_Scalar_Type_Node
     (MDBld  : MD_Builder_T;
      Name   : String;
      Parent : Metadata_T)
     return Metadata_T
   is
      function Create_TBAA_Scalar_Type_Node_C
        (MDBld  : MD_Builder_T;
         Name   : String;
         Parent : Metadata_T)
        return Metadata_T
        with Import, Convention => C,
             External_Name => "Create_TBAA_Scalar_Type_Node_C";

   begin
      return Create_TBAA_Scalar_Type_Node_C (MDBld, Name & ASCII.NUL, Parent);
   end Create_TBAA_Scalar_Type_Node;

   -----------------------
   -- Create_Enumerator --
   -----------------------

   function Create_Enumerator
     (Builder     : DI_Builder_T;
      Name        : String;
      Value       : unsigned_long_long;
      Is_Unsigned : Boolean) return Metadata_T
   is
      function Create_Enumerator_C
        (Builder     : DI_Builder_T;
         Name        : String;
         Value       : unsigned_long_long;
         Is_Unsigned : Bool_T) return Metadata_T
        with Import, Convention => C, External_Name => "Create_Enumerator";
   begin
      return Create_Enumerator_C (Builder, Name & ASCII.NUL, Value,
                                  Boolean'Pos (Is_Unsigned));
   end Create_Enumerator;

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
         Name     : String) return Value_T
        with Import, Convention => C,
             External_Name =>  "Build_Extract_Value_C";

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
         Name     : String) return Value_T
        with Import, Convention => C, External_Name => "Build_Insert_Value_C";

   begin
      return Build_Insert_Value_C (Bld, Aggr, Elt, Idx_List, Num_Idx,
                                   Name & ASCII.NUL);
   end Build_Insert_Value;

   ---------------------------
   --  LLVM_Optimize_Module --
   ---------------------------

   procedure LLVM_Optimize_Module
     (Module                : Module_T;
      Target_Machine        : Target_Machine_T;
      Code_Opt_Level        : Nat;
      Size_Opt_Level        : Nat;
      No_Inlining           : Boolean;
      No_Unit_At_A_Time     : Boolean;
      No_Unroll_Loops       : Boolean;
      No_Loop_Vectorization : Boolean;
      No_SLP_Vectorization  : Boolean)
   is
      procedure LLVM_Optimize_Module_C
        (Module                : Module_T;
         Target_Machine        : Target_Machine_T;
         Code_Opt_Level        : Nat;
         Size_Opt_Level        : Nat;
         No_Inlining           : Bool_T;
         No_Unit_At_A_Time     : Bool_T;
         No_Unroll_Loops       : Bool_T;
         No_Loop_Vectorization : Bool_T;
         No_SLP_Vectorization  : Bool_T)
        with Import, Convention => C, External_Name => "LLVM_Optimize_Module";
      No_Inlining_B  : constant Bool_T := Boolean'Pos (No_Inlining);
      No_Unit_B      : constant Bool_T := Boolean'Pos (No_Unit_At_A_Time);
      No_Unroll_B    : constant Bool_T := Boolean'Pos (No_Unroll_Loops);
      No_Loop_Vect_B : constant Bool_T := Boolean'Pos (No_Loop_Vectorization);
      No_SLP_Vect_B  : constant Bool_T := Boolean'Pos (No_SLP_Vectorization);
   begin
      LLVM_Optimize_Module_C (Module, Target_Machine,
                              Code_Opt_Level, Size_Opt_Level,
                              No_Inlining_B, No_Unit_B, No_Unroll_B,
                              No_Loop_Vect_B, No_SLP_Vect_B);
   end LLVM_Optimize_Module;

   function Get_GEP_Constant_Offset
     (GEP : Value_T; Layout : Target_Data_T; Offset : out ULL) return Boolean
   is
      function Get_GEP_Constant_Offset_C
        (GEP : Value_T;
         Layout : Target_Data_T;
         Offset : access ULL) return Bool_T
        with Import, Convention => C,
             External_Name => "Get_GEP_Constant_Offset";

      Result_Offset : aliased ULL;
      Result        : constant Bool_T :=
        Get_GEP_Constant_Offset_C (GEP, Layout, Result_Offset'Access);

   begin
      Offset := Result_Offset;
      return (if Result = 0 then False else True);
   end Get_GEP_Constant_Offset;

end GNATLLVM.Wrapper;
