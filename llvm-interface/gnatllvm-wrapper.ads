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
     (Ctx    : Context_T;
      MDBld  : MD_Builder_T;
      Name   : String;
      Size   : Value_T;
      Parent : Metadata_T) return Metadata_T
     with Inline;
   --  Create a TBAA metadata node for a scalar type

   function Create_TBAA_Access_Tag
     (MDBld                  : MD_Builder_T;
      Base_Type, Access_Type : Metadata_T;
      Offset, Size           : ULL) return Metadata_T
     with Import, Convention => C, External_Name => "Create_TBAA_Access_Tag";

   procedure Add_TBAA_Access (Value : Value_T; TBAA : Metadata_T)
     with Import, Convention => C, External_Name => "Add_TBAA_Access";

   procedure Add_Cold_Attribute (Func : Value_T)
     with Import, Convention => C, External_Name => "Add_Cold_Attribute";

   procedure Add_Dereferenceable_Attribute
     (Func : Value_T; Idx : unsigned; Bytes : ULL)
     with Import, Convention => C,
          External_Name => "Add_Dereferenceable_Attribute";

   procedure Add_Dereferenceable_Attribute (Func : Value_T; Bytes : ULL)
     with Import, Convention => C,
          External_Name => "Add_Ret_Dereferenceable_Attribute";

   procedure Add_Dereferenceable_Or_Null_Attribute
     (Func : Value_T; Idx : unsigned; Bytes : ULL)
     with Import, Convention => C,
          External_Name => "Add_Dereferenceable_Or_Null_Attribute";

   procedure Add_Dereferenceable_Or_Null_Attribute
     (Func : Value_T; Bytes : ULL)
     with Import, Convention => C,
          External_Name => "Add_Ret_Dereferenceable_Or_Null_Attribute";

   procedure Add_Inline_Always_Attribute (Func : Value_T)
     with Import, Convention => C,
          External_Name => "Add_Inline_Always_Attribute";

   procedure Add_Inline_Hint_Attribute (Func : Value_T)
     with Import, Convention => C,
          External_Name => "Add_Inline_Hint_Attribute";

   procedure Add_Inline_No_Attribute (Func : Value_T)
     with Import, Convention => C, External_Name => "Add_Inline_No_Attribute";

   procedure Add_Named_Attribute
     (Func : Value_T; Name, Value : String; Ctx : Context_T)
     with Inline;

   procedure Add_Nest_Attribute (Func : Value_T; Idx : unsigned)
     with Import, Convention => C, External_Name =>  "Add_Nest_Attribute";

   procedure Add_Noalias_Attribute (Func : Value_T; Idx : unsigned)
     with Import, Convention => C, External_Name => "Add_Noalias_Attribute";

   procedure Add_Noalias_Attribute (Func : Value_T)
     with Import, Convention => C,
          External_Name => "Add_Ret_Noalias_Attribute";

   procedure Add_Nocapture_Attribute (Func : Value_T; Idx : unsigned)
     with Import, Convention => C, External_Name => "Add_Nocapture_Attribute";

   procedure Add_Non_Null_Attribute (Func : Value_T; Idx : unsigned)
     with Import, Convention => C, External_Name => "Add_Non_Null_Attribute";

   procedure Add_Non_Null_Attribute (Func : Value_T)
     with Import, Convention => C,
          External_Name => "Add_Ret_Non_Null_Attribute";

   procedure Add_Readonly_Attribute (Func : Value_T; Idx : unsigned)
     with Import, Convention => C, External_Name => "Add_Readonly_Attribute";

   procedure Add_Writeonly_Attribute (Func : Value_T; Idx : unsigned)
     with Import, Convention => C, External_Name => "Add_Writeonly_Attribute";

   function Get_Stack_Alignment (Layout : Target_Data_T) return unsigned
     with Import, Convention => C, External_Name => "Get_Stack_Alignment";

   procedure Set_DSO_Local (V : Value_T)
     with Import, Convention => C, External_Name => "Set_DSO_Local";

   function Is_Constant_Data (V : Value_T) return Value_T
     with Import, Convention => C, External_Name => "Is_Constant_Data";

   function Build_Extract_Value
     (Bld      : Builder_T;
      Aggr     : Value_T;
      Idx_List : System.Address;
      Num_Idx  : unsigned;
      Name     : String) return Value_T
     with Inline;

   function Build_Insert_Value
     (Bld      : Builder_T;
      Aggr     : Value_T;
      Elt      : Value_T;
      Idx_List : System.Address;
      Num_Idx  : unsigned;
      Name     : String) return Value_T
     with Inline;

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

   procedure Set_Volatile_For_Atomic (V : Value_T)
     with Import, Convention => C, External_Name => "Set_Volatile_For_Atomic";

   procedure Add_Function_To_Module (Fn : Value_T; Module : Module_T)
     with Import, Convention => C, External_Name => "Add_Function_To_Module";

   procedure Dump_Metadata (MD : Metadata_T)
     with Import, Convention => C, External_Name => "Dump_Metadata";

   function Get_Metadata_Num_Operands (MD : Metadata_T) return Nat
     with Import, Convention => C,
          External_Name => "Get_Metadata_Num_Operands";

   function Get_Metadata_Operand (MD : Metadata_T; Idx : Nat) return Metadata_T
     with Import, Convention => C, External_Name => "Get_Metadata_Operand";

   function Get_Metadata_Operand_Constant_Value
     (MD : Metadata_T; Idx : Nat) return ULL
     with Pre => Present (MD), Inline;

   procedure LLVM_Optimize_Module
     (Module                : Module_T;
      Target_Machine        : Target_Machine_T;
      Code_Opt_Level        : Nat;
      Size_Opt_Level        : Nat;
      No_Inlining           : Boolean;
      No_Unroll_Loops       : Boolean;
      No_Loop_Vectorization : Boolean;
      No_SLP_Vectorization  : Boolean;
      Merge_Functions       : Boolean;
      PrepareForThinLTO     : Boolean;
      PrepareForLTO         : Boolean;
      RerollLoops           : Boolean)
     with Inline;
   --  Perform optimizations on the module

   procedure Add_Debug_Flags (Module : Module_T)
     with Import, Convention => C, External_Name => "Add_Debug_Flags";

   procedure Inst_Add_Combine_Function
     (PM : Pass_Manager_T; Target_Machine : Target_Machine_T)
     with Import, Convention => C,
          External_Name => "Inst_Add_Combine_Function";

   function Create_Enumerator
     (Builder     : DI_Builder_T;
      Name        : String;
      Value       : unsigned_long_long;
      Is_Unsigned : Boolean) return Metadata_T
     with Inline;

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

   function Get_GEP_Constant_Offset
     (GEP : Value_T; Layout : Target_Data_T; Offset : out ULL) return Boolean
     with Inline;

end GNATLLVM.Wrapper;
