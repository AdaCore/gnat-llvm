------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2023, AdaCore                     --
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
     with Pre => Present (Is_A_Function (Func)), Inline;

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

   procedure Add_Opt_For_Fuzzing_Attribute (Func : Value_T)
     with Import, Convention => C,
          External_Name => "Add_Opt_For_Fuzzing_Attribute";

   procedure Add_Sanitize_Address_Attribute (Func : Value_T)
     with Import, Convention => C,
          External_Name => "Add_Sanitize_Address_Attribute";

   procedure Add_No_Implicit_Float_Attribute (Func : Value_T)
     with Import, Convention => C,
          External_Name => "Add_No_Implicit_Float_Attribute";

   function Has_Inline_Attribute (Func : Value_T) return Boolean
     with Pre => Present (Is_A_Function (Func));

   function Has_Inline_Always_Attribute (Func : Value_T) return Boolean
     with Pre => Present (Is_A_Function (Func));

   function Has_Nest_Attribute (Func : Value_T; Idx : unsigned) return Boolean
     with Pre => Present (Is_A_Function (Func));

   function Call_Param_Has_Nest (V : Value_T; Idx : unsigned) return Boolean
     with Pre => Present (Is_A_Call_Inst (V));

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
     with Pre => Present (Is_A_Function (Fn)), Inline;

   function Does_Not_Return (Fn : Value_T) return Boolean
     with Pre => Present (Is_A_Function (Fn)), Inline;

   procedure Set_Does_Not_Throw (Fn : Value_T)
     with Import, Convention => C, External_Name => "Set_Does_Not_Throw";

   procedure Set_Does_Not_Return (Fn : Value_T)
     with Import, Convention => C, External_Name => "Set_Does_Not_Return";

   procedure Initialize_LLVM
     with Import, Convention => C, External_Name => "Initialize_LLVM";
   --  Initializes various parts of the LLVM infrastructure.

   procedure Get_Target_C_Types
     (Triple   : String;
      CPU      : String;
      ABI      : String;
      Features : String;
      Info     : out Target_C_Type_Info;
      Success  : out Boolean);
   --  Retrieve information about the C types for the target.

   procedure Set_NUW (V : Value_T)
     with Import, Convention => C, External_Name => "Set_NUW";

   procedure Set_NSW (V : Value_T)
     with Import, Convention => C, External_Name => "Set_NSW";

   function Has_NSW (V : Value_T) return Boolean
     with Pre => Present (Is_A_Instruction (V));

   procedure Set_Volatile_For_Atomic (V : Value_T)
     with Import, Convention => C, External_Name => "Set_Volatile_For_Atomic";

   procedure Set_Weak_For_Atomic_Xchg (V : Value_T)
     with Import, Convention => C, External_Name => "Set_Weak_For_Atomic_Xchg";

   procedure Add_Function_To_Module
     (Fn : Value_T; Module : Module_T; Allow_Deduplication : Boolean);

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

   function LLVM_Optimize_Module
     (Module                   : Module_T;
      Target_Machine           : Target_Machine_T;
      Code_Opt_Level           : Nat;
      Size_Opt_Level           : Nat;
      Need_Loop_Info           : Boolean;
      No_Unroll_Loops          : Boolean;
      No_Loop_Vectorization    : Boolean;
      No_SLP_Vectorization     : Boolean;
      Merge_Functions          : Boolean;
      Prepare_For_Thin_LTO     : Boolean;
      Prepare_For_LTO          : Boolean;
      Reroll_Loops             : Boolean;
      Enable_Fuzzer            : Boolean;
      Enable_Address_Sanitizer : Boolean;
      San_Cov_Allow_List       : String_Access;
      San_Cov_Ignore_List      : String_Access;
      Pass_Plugin_Name         : String_Access;
      Error_Message            : System.Address) return Boolean;
   --  Perform optimizations on the module. The function's interface mimics our
   --  LLVM bindings (e.g., LLVM.Core) by taking the address of a value of type
   --  Ptr_Err_Msg_Type for the optionally returned error message, and
   --  returning a Boolean which is true if an error occurred.

   procedure Add_Debug_Flags (Module : Module_T)
     with Import, Convention => C, External_Name => "Add_Debug_Flags";

   function Get_Float_From_Words_And_Exp
     (Context   : Context_T;
      Typ       : Type_T;
      Exp       : Int;
      Num_Words : unsigned;
      Words     : access uint64_t) return Value_T
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

   function Get_Load_Store_Type (V : Value_T) return Type_T
     with Import, Convention => C, External_Name => "Get_Load_Store_Type";

   function Get_Function_Type (V : Value_T) return Type_T
     with Import, Convention => C, External_Name => "Get_Function_Type";

   function Get_Source_Element_Type (V : Value_T) return Type_T
     with Import, Convention => C, External_Name => "Get_Source_Element_Type";

   function Is_C_String (V : Value_T) return Boolean with Inline;

   function Get_Element_Offset (T : Type_T; Idx : Int) return ULL
     with Inline;

   function Equals_Int (V : Value_T; Val : ULL) return Boolean
     with Inline;

   function Equal_Constants (V1, V2 : Value_T) return Boolean
     with Inline;

   function Struct_Has_Name (T : Type_T) return Boolean
     with Pre => Get_Type_Kind (T) = Struct_Type_Kind;

   function Value_Has_Name (V : Value_T) return Boolean
     with Pre => Present (V);

   function Is_Lifetime_Intrinsic (V : Value_T) return Boolean
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

   function Create_Return
     (Context : Context_T; Retval : Value_T) return Value_T
     with Import, Convention => C, External_Name => "Create_Return";

   function Create_Br (Dest : Basic_Block_T) return Value_T
     with Import, Convention => C, External_Name => "Create_Br";

   procedure Insert_At_Block_End
     (V : Value_T; BB : Basic_Block_T; From : Value_T)
     with Import, Convention => C, External_Name => "Insert_At_Block_End";

   function Insert_Alloca_Before (Ty : Type_T; Before : Value_T) return Value_T
     with Import, Convention => C, External_Name => "Insert_Alloca_Before";

   function Insert_Load_Before
     (Ty : Type_T; Ptr, Before : Value_T) return Value_T
     with Import, Convention => C, External_Name => "Insert_Load_Before";

   procedure Insert_Store_Before (Val, Ptr : Value_T; Before : Value_T)
     with Import, Convention => C, External_Name => "Insert_Store_Before";

   function All_Preds_Are_Unc_Branches (BB : Basic_Block_T) return Boolean
     with Pre => Present (BB), Inline;

   function Is_Dead_Basic_Block (BB : Basic_Block_T) return Boolean
     with Pre => Present (BB), Inline;

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

   procedure Set_Module_PIC_PIE
     (Module : Module_T; PIC : PIC_PIE_Level; PIE : PIC_PIE_Level)
     with Import, Convention => C, External_Name => "Set_Module_PIC_PIE";

   function Has_Default_PIE (Triple : String) return Boolean;

   function Has_SEH (Triple : String) return Boolean;

   function Get_Personality_Function_Name (Triple : String) return String;

   function Get_Features (Triple, Arch, CPU : String) return String;

   function Get_Default_Address_Space
     (Layout : Target_Data_T) return unsigned
     with Import, Convention => C,
          External_Name => "Get_Default_Address_Space";

   procedure Set_Absolute_Address (V : Value_T; Addr : Value_T);

   function Need_Enable_Execute_Stack (Triple : String) return Boolean;

   procedure Print_Targets
     with Import, Convention => C, External_Name => "Print_Targets";

end GNATLLVM.Wrapper;
