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

with System;

with stdint_h;

package GNATLLVM.Wrapper is

   function Create_MDBuilder_In_Context (Ctx : Context_T) return MD_Builder_T
     with Import, Convention => C,
          External_Name => "Create_MDBuilder_In_Context";

   function Create_TBAA_Root (MDBld : MD_Builder_T) return Metadata_T
     with Import, Convention => C, External_Name => "Create_TBAA_Root";
   --  Create the root of the TBAA metadata tree

   function Create_TBAA_Scalar_Type_Node
     (MDBld : MD_Builder_T;
      Name  : String;
      Parent  : Metadata_T) return Metadata_T;
   --  Create a TBAA metadata node for a scalar type

   function Create_TBAA_Access_Tag
     (MDBld                  : MD_Builder_T;
      Base_Type, Access_Type : Metadata_T;
      Offset                 : ULL) return Metadata_T
     with Import, Convention => C, External_Name => "Create_TBAA_Access_Tag";

   procedure Add_TBAA_Access (Value : Value_T; TBAA : Metadata_T)
     with Import, Convention => C, External_Name => "Add_TBAA_Access";

   procedure Add_Cold_Attribute (Func : Value_T)
     with Import, Convention => C, External_Name => "Add_Cold_Attribute";

   procedure Add_Dereferenceable_Attribute
     (Func : Value_T; Idx : unsigned; Bytes : ULL)
     with Import, Convention => C,
          External_Name => "Add_Dereferenceable_Attribute";

   procedure Add_Dereferenceable_Or_Null_Attribute
     (Func : Value_T; Idx : unsigned; Bytes : ULL)
     with Import, Convention => C,
          External_Name => "Add_Dereferenceable_Or_Null_Attribute";

   procedure Add_Inline_Always_Attribute (Func : Value_T)
     with Import, Convention => C,
          External_Name => "Add_Inline_Always_Attribute";

   procedure Add_Inline_Hint_Attribute (Func : Value_T)
     with Import, Convention => C,
          External_Name => "Add_Inline_Hint_Attribute";

   procedure Add_Inline_No_Attribute (Func : Value_T)
     with Import, Convention => C, External_Name => "Add_Inline_No_Attribute";

   procedure Add_Nest_Attribute (Func : Value_T; Idx : unsigned)
     with Import, Convention => C, External_Name =>  "Add_Nest_Attribute";

   procedure Add_Noalias_Attribute (Func : Value_T; Idx : unsigned)
     with Import, Convention => C, External_Name => "Add_Noalias_Attribute";

   procedure Add_Nocapture_Attribute (Func : Value_T; Idx : unsigned)
     with Import, Convention => C, External_Name => "Add_Nocapture_Attribute";

   procedure Add_Non_Null_Attribute (Func : Value_T; Idx : unsigned)
     with Import, Convention => C, External_Name => "Add_Non_Null_Attribute";

   procedure Add_Readonly_Attribute (Func : Value_T; Idx : unsigned)
     with Import, Convention => C, External_Name => "Add_Readonly_Attribute";

   procedure Add_Writeonly_Attribute (Func : Value_T; Idx : unsigned)
     with Import, Convention => C, External_Name => "Add_Writeonly_Attribute";

   function Get_Stack_Alignment (Layout : Target_Data_T) return unsigned
     with Import, Convention => C, External_Name => "Get_Stack_Alignment";

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

   function Does_Not_Throw (Fn : Value_T) return Boolean
     with Import, Convention => C, External_Name => "Does_Not_Throw",
          Warnings => Off;

   procedure Set_Does_Not_Throw (Fn : Value_T)
     with Import, Convention => C, External_Name => "Set_Does_Not_Throw";

   procedure Set_Does_Not_Return (Fn : Value_T)
     with Import, Convention => C, External_Name => "Set_Does_Not_Return";

   procedure Initialize_LLVM
     with Import, Convention => C, External_Name => "Initialize_LLVM";
   --  Initializes various parts of the LLVM infrastructure.

   procedure Set_NUW (V : Value_T)
     with Import, Convention => C, External_Name => "Set_NUW";

   procedure Set_NSW (V : Value_T)
     with Import, Convention => C, External_Name => "Set_NSW";

   procedure LLVM_Optimize_Module
     (Module                : Module_T;
      Target_Machine        : Target_Machine_T;
      Code_Opt_Level        : Nat;
      Size_Opt_Level        : Nat;
      No_Inlining           : Boolean;
      No_Unit_At_A_Time     : Boolean;
      No_Unroll_Loops       : Boolean;
      No_Loop_Vectorization : Boolean;
      No_SLP_Vectorization  : Boolean);
   --  Perform optimizations on the module

   procedure Add_Debug_Flags (Module : Module_T)
     with Import, Convention => C, External_Name => "Add_Debug_Flags";
   --  Create a DIBuilder and return it

   function Create_Debug_Compile_Unit
     (Bld : DI_Builder_T; File : Metadata_T) return Metadata_T
     with Import, Convention => C,
          External_Name => "Create_Debug_Compile_Unit";

   function Create_Debug_Subprogram
     (Bld            : DI_Builder_T;
      Func           : Value_T;
      File           : Metadata_T;
      Name, Ext_Name : String;
      Lineno         : Logical_Line_Number) return Metadata_T;

   function Create_Debug_Lexical_Block
     (Bld         : DI_Builder_T;
      Scope, File : Metadata_T;
      Line        : Logical_Line_Number;
      Column      : Column_Number) return Metadata_T
     with Import, Convention => C,
          External_Name => "Create_Debug_Lexical_Block";

   procedure Set_Debug_Loc
     (Bld    : Builder_T;
      Subp   : Metadata_T;
      Line   : Logical_Line_Number;
      Column : Column_Number)
     with Import, Convention => C, External_Name => "Set_Debug_Loc";

   function Get_Float_From_Words_And_Exp
     (Context   : Context_T;
      Typ       : Type_T;
      Exp       : Int;
      Num_Words : unsigned;
      Words     : access stdint_h.uint64_t) return Value_T
     with Import, Convention => C,
          External_Name => "Get_Float_From_Words_And_Exp";

   function Pred_FP
     (Context : Context_T; T : Type_T; V : Value_T) return Value_T
     with Import, Convention => C, External_Name => "Pred_FP";

end GNATLLVM.Wrapper;
