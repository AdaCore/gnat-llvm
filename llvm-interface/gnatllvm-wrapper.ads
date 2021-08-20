------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2021, AdaCore                     --
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

with LLVM.Core; use LLVM.Core;

package GNATLLVM.Wrapper is

   function Get_Latest_Instruction (Bld : Builder_T) return Value_T
     with Import, Convention => C, External_Name => "Get_Latest_Instruction";

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
      Size   : ULL;
      Parent : Metadata_T) return Metadata_T
     with Inline;
   --  Create a TBAA metadata node for a scalar type

   function Create_TBAA_Struct_Type_Node
     (Ctx        : Context_T;
      MDBld      : MD_Builder_T;
      Name       : String;
      Size       : ULL;
      Num_Fields : Nat;
      Parent     : Metadata_T;
      Fields     : System.Address;
      Offsets    : System.Address;
      Sizes      : System.Address) return Metadata_T
     with Inline;
   --  Create a TBAA metadata node for an aggregate type

   function Create_TBAA_Struct_Node
     (Ctx        : Context_T;
      MDBld      : MD_Builder_T;
      Num_Fields : Nat;
      TBAAs      : System.Address;
      Offsets    : System.Address;
      Sizes      : System.Address) return Metadata_T
     with Import, Convention => C, External_Name => "Create_TBAA_Struct_Node";

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

   procedure Add_Readonly_Attribute (Func : Value_T)
     with Import, Convention => C,
          External_Name => "Add_Fn_Readonly_Attribute";

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

   function Build_MemCpy
     (Bld         : Builder_T;
      Dst         : Value_T;
      Dst_Align   : unsigned;
      Src         : Value_T;
      Src_Align   : unsigned;
      Size        : Value_T;
      Is_Volatile : Boolean;
      TBAA        : Metadata_T;
      TBAA_Struct : Metadata_T;
      Scope       : Metadata_T;
      NoAlias     : Metadata_T) return Value_T
     with Inline;

   function Build_MemMove
     (Bld         : Builder_T;
      Dst         : Value_T;
      Dst_Align   : unsigned;
      Src         : Value_T;
      Src_Align   : unsigned;
      Size        : Value_T;
      Is_Volatile : Boolean;
      TBAA        : Metadata_T;
      Scope       : Metadata_T;
      NoAlias     : Metadata_T) return Value_T
     with Inline;

   function Build_MemSet
     (Bld         : Builder_T;
      Ptr         : Value_T;
      Val         : Value_T;
      Size        : Value_T;
      Align       : unsigned;
      Is_Volatile : Boolean;
      TBAA        : Metadata_T;
      Scope       : Metadata_T;
      NoAlias     : Metadata_T) return Value_T
     with Inline;

   function Create_Lifetime_Start
     (Bld  : Builder_T;
      Ptr  : Value_T;
      Size : Value_T) return Value_T
     with Import, Convention => C, External_Name => "Create_Lifetime_Start";

   function Create_Lifetime_End
     (Bld  : Builder_T;
      Ptr  : Value_T;
      Size : Value_T) return Value_T
     with Import, Convention => C, External_Name => "Create_Lifetime_End";

   function Create_Invariant_Start
     (Bld  : Builder_T;
      Ptr  : Value_T;
      Size : Value_T) return Value_T
     with Import, Convention => C, External_Name => "Create_Invariant_Start";

   function Does_Not_Throw (Fn : Value_T) return Boolean
     with Import, Convention => C, External_Name => "Does_Not_Throw",
          Warnings => Off;

   function Does_Not_Return (Fn : Value_T) return Boolean
     with Import, Convention => C, External_Name => "Does_Not_Return",
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

   procedure Set_Weak_For_Atomic_Xchg (V : Value_T)
     with Import, Convention => C, External_Name => "Set_Weak_For_Atomic_Xchg";

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

   function Get_Infinity (T : Type_T) return Value_T
     with Import, Convention => C, External_Name => "Get_Infinity";

   function Get_GEP_Constant_Offset
     (GEP : Value_T; Layout : Target_Data_T; Offset : out ULL) return Boolean
     with Inline;

   function Is_C_String (V : Value_T) return Boolean with Inline;

   function Get_Element_Offset (T : Type_T; Idx : unsigned) return ULL
     with Inline;

   function Equals_Int (V : Value_T; Val : ULL) return Boolean
     with Inline;

   function Equal_Constants (V1, V2 : Value_T) return Boolean
     with Inline;

   function Struct_Has_Name (T : Type_T) return Boolean
     with Pre => Get_Type_Kind (T) = Struct_Type_Kind;

   function Value_Has_Name (V : Value_T) return Boolean
     with Pre => Present (V);

   function Get_Unique_Predecessor (BB : Basic_Block_T) return Basic_Block_T
     with Import, Convention => C, External_Name => "Get_Unique_Predecessor";

   procedure Invert_Predicate (V : Value_T)
     with Import, Convention => C, External_Name => "Invert_Predicate";

   procedure Swap_Successors (V : Value_T)
     with Import, Convention => C, External_Name => "Swap_Successors";

   procedure Replace_Inst_With_Inst (From, To : Value_T)
     with Import, Convention => C, External_Name => "Replace_Inst_With_Inst";

   function Create_And (Op1, Op2 : Value_T) return Value_T
     with Import, Convention => C, External_Name => "Create_And";

   function Create_Or  (Op1, Op2 : Value_T) return Value_T
     with Import, Convention => C, External_Name => "Create_Or";

   function Create_Call_2 (Fn, Op1, Op2 : Value_T) return Value_T
     with Import, Convention => C, External_Name => "Create_Call_2";

   procedure Insert_At_Block_End (V : Value_T; BB : Basic_Block_T)
     with Import, Convention => C, External_Name => "Insert_At_Block_End";

   function Get_First_Non_Phi_Or_Dbg (BB : Basic_Block_T) return Value_T
     with Import, Convention => C, External_Name => "Get_First_Non_Phi_Or_Dbg";

   function Get_Num_CDA_Elements (V : Value_T) return unsigned
     with Import, Convention => C, External_Name => "Get_Num_CDA_Elements";

   function Convert_FP_To_String
     (V : Value_T; Buffer : out String) return Integer
     with Import, Convention => C, External_Name => "Convert_FP_To_String";

   type N_O_D_Fn is access procedure (V : Value_T) with Convention => C;
   procedure Notify_On_Value_Delete (V : Value_T; Fn : N_O_D_Fn)
     with Import, Convention => C, External_Name => "Notify_On_Value_Delete";
end GNATLLVM.Wrapper;
