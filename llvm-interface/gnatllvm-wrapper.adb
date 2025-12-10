------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2025, AdaCore                     --
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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Uintp.LLVM;           use Uintp.LLVM;

package body GNATLLVM.Wrapper is

   ----------------------------------
   -- Create_TBAA_Scalar_Type_Node --
   ----------------------------------

   function Create_TBAA_Scalar_Type_Node
     (Ctx    : Context_T;
      MDBld  : MD_Builder_T;
      Name   : String;
      Size   : ULL;
      Parent : Metadata_T) return Metadata_T
   is
      function Create_TBAA_Scalar_Type_Node_C
        (Ctx    : Context_T;
         MDBld  : MD_Builder_T;
         Name   : String;
         Size   : ULL;
         Parent : Metadata_T) return Metadata_T
        with Import, Convention => C,
             External_Name => "Create_TBAA_Scalar_Type_Node";

   begin
      return Create_TBAA_Scalar_Type_Node_C (Ctx, MDBld, Name & ASCII.NUL,
                                             Size, Parent);
   end Create_TBAA_Scalar_Type_Node;

   ----------------------------------
   -- Create_TBAA_Struct_Type_Node --
   ----------------------------------

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
   is
      function Create_TBAA_Struct_Type_Node_C
        (Ctx        : Context_T;
         MDBld      : MD_Builder_T;
         Name       : String;
         Size       : ULL;
         Num_Fields : Nat;
         Parent     : Metadata_T;
         Fields     : System.Address;
         Offsets    : System.Address;
         Sizes      : System.Address) return Metadata_T
        with Import, Convention => C,
             External_Name => "Create_TBAA_Struct_Type_Node";

   begin
      return Create_TBAA_Struct_Type_Node_C (Ctx, MDBld, Name & ASCII.NUL,
                                             Size, Num_Fields, Parent, Fields,
                                             Offsets, Sizes);
   end Create_TBAA_Struct_Type_Node;

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

   ------------------
   -- Build_MemCpy --
   ------------------

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
   is
      function Build_MemCpy_C
        (Bld         : Builder_T;
         Dst         : Value_T;
         Dst_Align   : unsigned;
         Src         : Value_T;
         Src_Align   : unsigned;
         Size        : Value_T;
         Is_Volatile : LLVM_Bool;
         TBAA        : Metadata_T;
         TBAA_Struct : Metadata_T;
         Scope       : Metadata_T;
         NoAlias     : Metadata_T) return Value_T
        with Import, Convention => C, External_Name => "Build_MemCpy";
   begin
      return Build_MemCpy_C (Bld, Dst, Dst_Align, Src, Src_Align, Size,
                             Boolean'Pos (Is_Volatile), TBAA, TBAA_Struct,
                             Scope, NoAlias);
   end Build_MemCpy;

   -------------------
   -- Build_MemMove --
   -------------------

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
   is
      function Build_MemMove_C
        (Bld         : Builder_T;
         Dst         : Value_T;
         Dst_Align   : unsigned;
         Src         : Value_T;
         Src_Align   : unsigned;
         Size        : Value_T;
         Is_Volatile : LLVM_Bool;
         TBAA        : Metadata_T;
         Scope       : Metadata_T;
         NoAlias     : Metadata_T) return Value_T
        with Import, Convention => C, External_Name => "Build_MemMove";

   begin
      return Build_MemMove_C (Bld, Dst, Dst_Align, Src, Src_Align, Size,
                              Boolean'Pos (Is_Volatile), TBAA, Scope, NoAlias);
   end Build_MemMove;

   ------------------
   -- Build_MemSet --
   ------------------

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
   is
      function Build_MemSet_C
        (Bld         : Builder_T;
         Ptr         : Value_T;
         Val         : Value_T;
         Size        : Value_T;
         Align       : unsigned;
         Is_Volatile : LLVM_Bool;
         TBAA        : Metadata_T;
         Scope       : Metadata_T;
         NoAlias     : Metadata_T) return Value_T
        with Import, Convention => C, External_Name => "Build_MemSet";

   begin
      return Build_MemSet_C (Bld, Ptr, Val, Size, Align,
                             Boolean'Pos (Is_Volatile), TBAA, Scope, NoAlias);
   end Build_MemSet;

   -------------------------
   -- Add_Named_Attribute --
   -------------------------

   procedure Add_Named_Attribute
     (Func : Value_T; Name, Value : String; Ctx : Context_T)
   is
      procedure Add_Named_Attribute_C
        (Func : Value_T; Name, Value : String; Ctx : Context_T)
        with Import, Convention => C, External_Name => "Add_Named_Attribute";

   begin
      Add_Named_Attribute_C (Func, Name & ASCII.NUL, Value & ASCII.NUL, Ctx);
   end Add_Named_Attribute;

   --------------------------
   -- Has_Inline_Attribute --
   --------------------------

   function Has_Inline_Attribute (Func : Value_T) return Boolean is
      function Has_Inline_Attribute_C (Func : Value_T) return LLVM_Bool
        with Import, Convention => C, External_Name => "Has_Inline_Attribute";
   begin
      return Has_Inline_Attribute_C (Func) /= 0;
   end Has_Inline_Attribute;

   ---------------------------------
   -- Has_Inline_Always_Attribute --
   ---------------------------------

   function Has_Inline_Always_Attribute (Func : Value_T) return Boolean is
      function Has_Inline_Always_Attribute_C (Func : Value_T) return LLVM_Bool
        with Import, Convention => C,
        External_Name => "Has_Inline_Always_Attribute";
   begin
      return Has_Inline_Always_Attribute_C (Func) /= 0;
   end Has_Inline_Always_Attribute;

   ------------------------
   -- Has_Nest_Attribute --
   ------------------------

   function Has_Nest_Attribute (Func : Value_T; Idx : unsigned) return Boolean
   is
      function Has_Nest_Attribute
        (Func : Value_T; Idx : unsigned) return LLVM_Bool
        with Import, Convention => C, External_Name => "Has_Nest_Attribute";
   begin
      return Has_Nest_Attribute (Func, Idx) /= 0;
   end Has_Nest_Attribute;

   -------------------------
   -- Call_Param_Has_Nest --
   -------------------------

   function Call_Param_Has_Nest (V : Value_T; Idx : unsigned) return Boolean
   is
      function Call_Param_Has_Nest
        (V : Value_T; Idx : unsigned) return LLVM_Bool
        with Import, Convention => C, External_Name => "Call_Param_Has_Nest";
   begin
      return Call_Param_Has_Nest (V, Idx) /= 0;
   end Call_Param_Has_Nest;

   -------------
   -- Has_NSW --
   -------------

   function Has_NSW (V : Value_T) return Boolean is
      function Has_NSW (V : Value_T) return LLVM_Bool
        with Import, Convention => C, External_Name => "Has_NSW";
   begin
      return Has_NSW (V) /= 0;
   end Has_NSW;

   ----------------------------
   -- Add_Function_To_Module --
   ----------------------------

   procedure Add_Function_To_Module
     (Fn : Value_T; Module : Module_T; Allow_Deduplication : Boolean)
   is
      procedure Add_Function_To_Module_C
        (Fn                  : Value_T;
         Module              : Module_T;
         Allow_Deduplication : LLVM_Bool)
        with Import, Convention => C,
             External_Name => "Add_Function_To_Module";
   begin
      Add_Function_To_Module_C
        (Fn, Module, Boolean'Pos (Allow_Deduplication));
   end Add_Function_To_Module;

   -----------------------------------------
   -- Get_Metadata_Operand_Constant_Value --
   -----------------------------------------
   function Get_Metadata_Operand_Constant_Value
     (MD : Metadata_T; Idx : Nat) return ULL
   is
      function Get_Metadata_Operand_Constant_Value_C
        (MD : Metadata_T; Idx : unsigned) return uint64_t
        with Import, Convention => C,
             External_Name => "Get_Metadata_Operand_Constant_Value";
   begin
      return ULL (Get_Metadata_Operand_Constant_Value_C (MD, unsigned (Idx)));
   end Get_Metadata_Operand_Constant_Value;

   ---------------------------
   --  LLVM_Optimize_Module --
   ---------------------------

   function LLVM_Optimize_Module
     (Module                   : Module_T;
      Target_Machine           : Target_Machine_T;
      Code_Opt_Level           : Nat;
      Size_Opt_Level           : Nat;
      Need_Loop_Info           : Boolean;
      Unroll_Loops             : Boolean;
      Loop_Vectorization       : Boolean;
      SLP_Vectorization        : Boolean;
      Merge_Functions          : Boolean;
      Prepare_For_Thin_LTO     : Boolean;
      Prepare_For_LTO          : Boolean;
      Reroll_Loops             : Boolean;
      Enable_Fuzzer            : Boolean;
      Enable_Address_Sanitizer : Boolean;
      San_Cov_Allow_List       : String_Access;
      San_Cov_Ignore_List      : String_Access;
      Pass_Plugin_Name         : String_Access;
      Error_Message            : System.Address) return Boolean
   is
      function Maybe_To_C (S : String_Access) return chars_ptr
      is (if S = null then Null_Ptr else New_String (S.all));

      function LLVM_Optimize_Module_C
        (Module                   : Module_T;
         Target_Machine           : Target_Machine_T;
         Code_Opt_Level           : Nat;
         Size_Opt_Level           : Nat;
         Need_Loop_Info           : LLVM_Bool;
         Unroll_Loops             : LLVM_Bool;
         Loop_Vectorization       : LLVM_Bool;
         SLP_Vectorization        : LLVM_Bool;
         Merge_Functions          : LLVM_Bool;
         Prepare_For_Thin_LTO     : LLVM_Bool;
         PrepareFor_LTO           : LLVM_Bool;
         Reroll_Loops             : LLVM_Bool;
         Enable_Fuzzer            : LLVM_Bool;
         Enable_Address_Sanitizer : LLVM_Bool;
         San_Cov_Allow_List       : chars_ptr;
         San_Cov_Ignore_List      : chars_ptr;
         Pass_Plugin_Name         : chars_ptr;
         Error_Message            : System.Address) return LLVM_Bool
        with Import, Convention => C, External_Name => "LLVM_Optimize_Module";
      Need_Loop_Info_B : constant LLVM_Bool := Boolean'Pos (Need_Loop_Info);
      Unroll_B         : constant LLVM_Bool := Boolean'Pos (Unroll_Loops);
      Loop_Vect_B      : constant LLVM_Bool :=
        Boolean'Pos (Loop_Vectorization);
      SLP_Vect_B       : constant LLVM_Bool := Boolean'Pos (SLP_Vectorization);
      Merge_B          : constant LLVM_Bool := Boolean'Pos (Merge_Functions);
      Thin_LTO_B       : constant LLVM_Bool :=
        Boolean'Pos (Prepare_For_Thin_LTO);
      LTO_B            : constant LLVM_Bool := Boolean'Pos (Prepare_For_LTO);
      Reroll_B         : constant LLVM_Bool := Boolean'Pos (Reroll_Loops);
      Fuzzer_B         : constant LLVM_Bool := Boolean'Pos (Enable_Fuzzer);
      ASan_B           : constant LLVM_Bool :=
        Boolean'Pos (Enable_Address_Sanitizer);
      Pass_PN_Ptr      : chars_ptr          := Maybe_To_C (Pass_Plugin_Name);
      Allow_List_Ptr   : chars_ptr          :=
        Maybe_To_C (San_Cov_Allow_List);
      Ignore_List_Ptr  : chars_ptr          :=
        Maybe_To_C (San_Cov_Ignore_List);
      Result           : LLVM_Bool;

   begin
      Result :=
        LLVM_Optimize_Module_C
          (Module, Target_Machine, Code_Opt_Level, Size_Opt_Level,
           Need_Loop_Info_B, Unroll_B, Loop_Vect_B, SLP_Vect_B,
           Merge_B, Thin_LTO_B, LTO_B, Reroll_B, Fuzzer_B, ASan_B,
           Allow_List_Ptr, Ignore_List_Ptr, Pass_PN_Ptr, Error_Message);
      Free (Allow_List_Ptr);
      Free (Ignore_List_Ptr);
      Free (Pass_PN_Ptr);
      return Result /= 0;
   end LLVM_Optimize_Module;

   -----------------------------
   -- Get_GEP_Constant_Offset --
   -----------------------------

   function Get_GEP_Constant_Offset
     (GEP : Value_T; Layout : Target_Data_T; Offset : out ULL) return Boolean
   is
      function Get_GEP_Constant_Offset_C
        (GEP    : Value_T;
         Layout : Target_Data_T;
         Offset : access ULL) return LLVM_Bool
        with Import, Convention => C,
             External_Name => "Get_GEP_Constant_Offset";

      Result_Offset : aliased ULL;
      Result        : constant LLVM_Bool :=
        Get_GEP_Constant_Offset_C (GEP, Layout, Result_Offset'Access);

   begin
      Offset := Result_Offset;
      return Result /= 0;
   end Get_GEP_Constant_Offset;

   --------------------
   -- Does_Not_Throw --
   --------------------

   function Does_Not_Throw (Fn : Value_T) return Boolean is
      function Does_Not_Throw_C (Fn : Value_T) return LLVM_Bool
        with Import, Convention => C, External_Name => "Does_Not_Throw";
   begin
      return Does_Not_Throw_C (Fn) /= 0;
   end Does_Not_Throw;

   ---------------------
   -- Does_Not_Return --
   ---------------------

   function Does_Not_Return (Fn : Value_T) return Boolean is
      function Does_Not_Return_C (Fn : Value_T) return LLVM_Bool
        with Import, Convention => C, External_Name => "Does_Not_Return";
   begin
      return Does_Not_Return_C (Fn) /= 0;
   end Does_Not_Return;

   ------------------------
   -- Get_Target_C_Types --
   ------------------------

   procedure Get_Target_C_Types
     (Triple   : String;
      CPU      : String;
      ABI      : String;
      Features : String;
      Info     : out Target_C_Type_Info;
      Emit_C   : Boolean;
      Success  : out Boolean)
   is
      use Interfaces.C;

      procedure Get_Target_C_Types_C
        (Triple   : char_array;
         CPU      : char_array;
         ABI      : char_array;
         Features : char_array;
         Info     : out Target_C_Type_Info;
         Emit_C   : LLVM_Bool;
         Success  : out unsigned_char)
        with Import, Convention => C, External_Name => "Get_Target_C_Types";

      Success_C    : unsigned_char;

   begin
      Get_Target_C_Types_C
        (To_C (Triple), To_C (CPU), To_C (ABI), To_C (Features), Info,
         Boolean'Pos (Emit_C), Success_C);
      Success := Success_C /= 0;
   end Get_Target_C_Types;

   -----------------
   -- Is_C_String --
   -----------------

   function Is_C_String (V : Value_T) return Boolean is
      function Is_C_String_C (V : Value_T) return LLVM_Bool
        with Import, Convention => C, External_Name => "Is_C_String";
   begin
      return Is_C_String_C (V) /= 0;
   end Is_C_String;

   ------------------------
   -- Get_Element_Offset --
   ------------------------

   function Get_Element_Offset (T : Type_T; Idx : Int) return ULL is
      function Get_Element_Offset_C
        (Layout : Target_Data_T; T : Type_T; Idx : unsigned) return int64_t
        with Import, Convention => C, External_Name => "Get_Element_Offset";
   begin
      return ULL (Get_Element_Offset_C (Module_Data_Layout, T,
                                        unsigned (Idx)));
   end Get_Element_Offset;

   ----------------
   -- Equals_Int --
   ----------------

   function Equals_Int (V : Value_T; Val : ULL) return Boolean is
      function Equals_Int_C (V : Value_T; Val : ULL) return LLVM_Bool
        with Import, Convention => C, External_Name => "Equals_Int";
   begin
      return Equals_Int_C (V, Val) /= 0;
   end Equals_Int;

   ----------------------
   -- Equals_Constants --
   ----------------------

   function Equal_Constants (V1, V2 : Value_T) return Boolean is
      function Equal_Constants_C (V1, V2 : Value_T) return LLVM_Bool
        with Import, Convention => C, External_Name => "Equal_Constants";
   begin
      return Equal_Constants_C (V1, V2) /= 0;
   end Equal_Constants;

   ---------------------
   -- Struct_Has_Name --
   ---------------------

   function Struct_Has_Name (T : Type_T) return Boolean is
      function Struct_Has_Name (T : Type_T) return LLVM_Bool
        with Import, Convention => C, External_Name => "Struct_Has_Name";
   begin
      return Struct_Has_Name (T) /= 0;
   end Struct_Has_Name;

   --------------------
   -- Value_Has_Name --
   --------------------

   function Value_Has_Name (V : Value_T) return Boolean is
      function Value_Has_Name (V : Value_T) return LLVM_Bool
        with Import, Convention => C, External_Name => "Value_Has_Name";
   begin
      return Value_Has_Name (V) /= 0;
   end Value_Has_Name;

   ---------------------------
   -- Is_Lifetime_Intrinsic --
   ---------------------------

   function Is_Lifetime_Intrinsic (V : Value_T) return Boolean is
      function Is_Lifetime_Intrinsic (V : Value_T) return LLVM_Bool
        with Import, Convention => C, External_Name => "Is_Lifetime_Intrinsic";
   begin
      return Is_Lifetime_Intrinsic (V) /= 0;
   end Is_Lifetime_Intrinsic;

   --------------------------------
   -- All_Preds_Are_Unc_Branches --
   --------------------------------

   function All_Preds_Are_Unc_Branches (BB : Basic_Block_T) return Boolean is
      function All_Preds_Are_Unc_Branches (BB : Basic_Block_T) return LLVM_Bool
        with Import, Convention => C,
             External_Name => "All_Preds_Are_Unc_Branches";

   begin
      return All_Preds_Are_Unc_Branches (BB) /= 0;
   end All_Preds_Are_Unc_Branches;

   -------------------------
   -- Is_Dead_Basic_Block --
   -------------------------

   function Is_Dead_Basic_Block (BB : Basic_Block_T) return Boolean is
      function Is_Dead_Basic_Block (BB : Basic_Block_T) return LLVM_Bool
        with Import, Convention => C,
             External_Name => "Is_Dead_Basic_Block";

   begin
      return Is_Dead_Basic_Block (BB) /= 0;
   end Is_Dead_Basic_Block;

   --------------------------
   -- Has_Default_PIE --
   --------------------------

   function Has_Default_PIE (Triple : String) return Boolean is
      function Has_Default_PIE_C (Triple : String) return LLVM_Bool
        with Import, Convention => C, External_Name => "Has_Default_PIE";
   begin
      return Has_Default_PIE_C (Triple & ASCII.NUL) /= 0;
   end Has_Default_PIE;

   -------------
   -- Has_SEH --
   -------------

   function Has_SEH (Triple : String) return Boolean is
      function Has_SEH_C (Triple : String) return LLVM_Bool
        with Import, Convention => C, External_Name => "Has_SEH";
   begin
      return Has_SEH_C (Triple & ASCII.NUL) /= 0;
   end Has_SEH;

   ------------
   -- Is_x86 --
   ------------

   function Is_x86 (Triple : String) return Boolean is
      function Is_x86 (Triple : String) return LLVM_Bool
        with Import, Convention => C, External_Name => "Is_x86";
   begin
      return Is_x86 (Triple & ASCII.NUL) /= 0;
   end Is_x86;

   -----------------------------------
   -- Get_Personality_Function_Name --
   -----------------------------------

   function Get_Personality_Function_Name (Triple : String) return String is
      function Get_Personality_Function_Name_C
        (Triple : String) return chars_ptr with
        Import, Convention => C,
        External_Name      => "Get_Personality_Function_Name";
   begin
      return Value (Get_Personality_Function_Name_C (Triple & ASCII.NUL));
   end Get_Personality_Function_Name;

   ------------------
   -- Get_Features --
   ------------------

   function Get_Features (Triple, Arch, CPU : String) return String is
      use Interfaces.C;

      function Get_Features_C
        (Triple, Arch, CPU : char_array) return chars_ptr with
        Import, Convention => C, External_Name => "Get_Features";

      Triple_C : constant char_array := To_C (Triple);
      Arch_C   : constant char_array := To_C (Arch);
      CPU_C    : constant char_array := To_C (CPU);

      Result_C : chars_ptr := Get_Features_C (Triple_C, Arch_C, CPU_C);
      Result : constant String :=
        (if Result_C = Null_Ptr then "" else Value (Result_C));

   begin
      Free (Result_C);
      return Result;
   end Get_Features;

   ----------------------------
   -- Get_Target_Default_CPU --
   ----------------------------

   function Get_Target_Default_CPU (Triple : String) return String is
      use Interfaces.C;

      function Get_Target_Default_CPU
        (Triple : char_array) return chars_ptr with
        Import, Convention => C, External_Name => "Get_Target_Default_CPU";

      Triple_C : constant char_array := To_C (Triple);
      Result_C : constant chars_ptr  := Get_Target_Default_CPU (Triple_C);

   begin
      return Value (Result_C);
   end Get_Target_Default_CPU;

   --------------------------
   -- Set_Absolute_Address --
   --------------------------

   procedure Set_Absolute_Address (V : Value_T; Addr : Value_T) is
      procedure Set_Absolute_Address_C
        (Ctx : Context_T; V : Value_T; Addr : Value_T) with
        Import, Convention => C, External_Name => "Set_Absolute_Address";
   begin
      Set_Absolute_Address_C (Get_Global_Context, V, Addr);
   end Set_Absolute_Address;

   -------------------------------
   -- Need_Enable_Execute_Stack --
   -------------------------------

   function Need_Enable_Execute_Stack (Triple : String) return Boolean is
      function Need_Enable_Execute_Stack_C (Triple : String) return LLVM_Bool
        with Import, Convention => C,
             External_Name => "Need_Enable_Execute_Stack";
   begin
      return Need_Enable_Execute_Stack_C (Triple & ASCII.NUL) /= 0;
   end Need_Enable_Execute_Stack;

   -----------------------------
   -- Create_And_Insert_Label --
   -----------------------------

   procedure Create_And_Insert_Label
     (Builder         : DI_Builder_T;
      Scope           : Metadata_T;
      Name            : String;
      File            : Metadata_T;
      Line_No         : Physical_Line_Number;
      Debug_Loc       : Metadata_T;
      Block           : Basic_Block_T)
   is
      procedure Create_And_Insert_Label_C
        (Builder         : DI_Builder_T;
         Scope           : Metadata_T;
         Name            : String;
         File            : Metadata_T;
         Line_No         : Physical_Line_Number;
         Debug_Loc       : Metadata_T;
         Block           : Basic_Block_T)
      with Import => True,
         Convention => C,
         External_Name => "Create_And_Insert_Label";
   begin
      Create_And_Insert_Label_C (Builder, Scope, Name & ASCII.NUL,
                                 File, Line_No, Debug_Loc, Block);
   end Create_And_Insert_Label;

   --------------------------
   -- Create_Subrange_Type --
   --------------------------

   function Create_Subrange_Type
     (Builder        : DI_Builder_T;
      Scope          : Metadata_T;
      Name           : String;
      File           : Metadata_T;
      Line_Number    : Logical_Line_Number;
      Size           : ULL;
      Align_In_Bits  : Nat;
      Flags          : DI_Flags_T;
      IsUnsigned     : Boolean;
      BaseType       : Metadata_T;
      Lower_Bound    : Metadata_T;
      Upper_Bound    : Metadata_T;
      Stride         : Metadata_T;
      Bias           : Metadata_T) return Metadata_T
   is
      function Create_Subrange_Type_C
         (Builder        : DI_Builder_T;
          Scope          : Metadata_T;
          Name           : String;
          File           : Metadata_T;
          Line_Number    : unsigned;
          Size           : uint64_t;
          Align_In_Bits  : uint32_t;
          Flags          : DI_Flags_T;
          IsUnsigned     : LLVM_Bool;
          BaseType       : Metadata_T;
          Lower_Bound    : Metadata_T;
          Upper_Bound    : Metadata_T;
          Stride         : Metadata_T;
          Bias           : Metadata_T) return Metadata_T
      with Import => True,
         Convention => C,
         External_Name => "Create_Subrange_Type";
   begin
      return Create_Subrange_Type_C
        (Builder, Scope, Name & ASCII.NUL, File, unsigned (Line_Number),
         uint64_t (Size), uint32_t (Align_In_Bits), Flags,
         Boolean'Pos (IsUnsigned), BaseType,
         Lower_Bound, Upper_Bound, Stride, Bias);
   end Create_Subrange_Type;

   ---------------------------------
   -- Create_Array_Type_With_Name --
   ---------------------------------

   function Create_Array_Type_With_Name
     (Builder        : DI_Builder_T;
      Scope          : Metadata_T;
      Name           : String;
      File           : Metadata_T;
      Line_Number    : Logical_Line_Number;
      Size           : ULL;
      Align_In_Bits  : Nat;
      Ty             : Metadata_T;
      Stride         : Metadata_T;
      Subscripts     : Metadata_Array) return Metadata_T
   is
      function Create_Array_Type_With_Name_C
        (Builder        : DI_Builder_T;
         Scope          : Metadata_T;
         Name           : String;
         File           : Metadata_T;
         Line_Number    : unsigned;
         Size           : uint64_t;
         Align_In_Bits  : uint32_t;
         Ty             : Metadata_T;
         Stride         : Metadata_T;
         Subscripts     : System.Address;
         Num_Subscripts : unsigned) return Metadata_T
      with Import => True,
         Convention => C,
         External_Name => "Create_Array_Type_With_Name";
   begin
      return Create_Array_Type_With_Name_C
        (Builder, Scope, Name & ASCII.NUL, File, unsigned (Line_Number),
         uint64_t (Size), uint32_t (Align_In_Bits), Ty, Stride,
         Subscripts'Address, unsigned (Subscripts'Length));
   end Create_Array_Type_With_Name;

   function Create_Binary_Fixed_Point_Type
     (Builder        : DI_Builder_T;
      Name           : String;
      Size           : ULL;
      Align_In_Bits  : Nat;
      Is_Unsigned    : Boolean;
      Factor         : Int) return Metadata_T
   is
      function Create_Binary_Fixed_Point_Type_C
        (Builder        : DI_Builder_T;
         Name           : String;
         Size           : uint64_t;
         Align_In_Bits  : uint32_t;
         Is_Unsigned    : LLVM_Bool;
         Factor         : Int) return Metadata_T
      with Import => True,
         Convention => C,
         External_Name => "Create_Binary_Fixed_Point_Type";
   begin
      return Create_Binary_Fixed_Point_Type_C
         (Builder, Name & ASCII.NUL, uint64_t (Size),
          uint32_t (Align_In_Bits), Boolean'Pos (Is_Unsigned), Factor);
   end Create_Binary_Fixed_Point_Type;

   function Create_Decimal_Fixed_Point_Type
     (Builder        : DI_Builder_T;
      Name           : String;
      Size           : ULL;
      Align_In_Bits  : Nat;
      Is_Unsigned    : Boolean;
      Factor         : Int) return Metadata_T
   is
      function Create_Decimal_Fixed_Point_Type_C
        (Builder        : DI_Builder_T;
         Name           : String;
         Size           : uint64_t;
         Align_In_Bits  : uint32_t;
         Is_Unsigned    : LLVM_Bool;
         Factor         : Int) return Metadata_T
      with Import => True,
         Convention => C,
         External_Name => "Create_Decimal_Fixed_Point_Type";
   begin
      return Create_Decimal_Fixed_Point_Type_C
         (Builder, Name & ASCII.NUL, uint64_t (Size),
          uint32_t (Align_In_Bits), Boolean'Pos (Is_Unsigned), Factor);
   end Create_Decimal_Fixed_Point_Type;

   function Create_Rational_Fixed_Point_Type
     (Builder        : DI_Builder_T;
      Name           : String;
      Size           : ULL;
      Align_In_Bits  : Nat;
      Is_Unsigned    : Boolean;
      Numerator      : Metadata_T;
      Denominator    : Metadata_T) return Metadata_T
   is
      function Create_Rational_Fixed_Point_Type_C
        (Builder        : DI_Builder_T;
         Name           : String;
         Size           : uint64_t;
         Align_In_Bits  : uint32_t;
         Is_Unsigned    : LLVM_Bool;
         Numerator      : Metadata_T;
         Denominator    : Metadata_T) return Metadata_T
      with Import => True,
         Convention => C,
         External_Name => "Create_Rational_Fixed_Point_Type";
   begin
      return Create_Rational_Fixed_Point_Type_C
         (Builder, Name & ASCII.NUL, uint64_t (Size),
          uint32_t (Align_In_Bits), Boolean'Pos (Is_Unsigned),
          Numerator, Denominator);
   end Create_Rational_Fixed_Point_Type;

   function Constant_As_Metadata (U : Uint) return Metadata_T
   is
      function Constant_As_Metadata
         (Ctx       : Context_T;
          Builder    : MD_Builder_T;
          Num_Words : unsigned;
          Words     : access uint64_t) return Metadata_T
      with Import => True,
         Convention => C,
         External_Name => "Constant_As_Metadata";

      Words : Word_Array := UI_To_Words (U);
   begin
      return Constant_As_Metadata (Get_Global_Context, MD_Builder,
                                   Words'Length, Words (Words'First)'Access);
   end Constant_As_Metadata;

   procedure Create_Import_Declarations (Builder : DI_Builder_T;
                                          Name : String;
                                          Comp_Unit : Metadata_T;
                                          File : Metadata_T;
                                          Line_Number : Logical_Line_Number)
   is
      procedure Create_Import_Declarations (Builder : DI_Builder_T;
                                             Name : String;
                                             Comp_Unit : Metadata_T;
                                             File : Metadata_T;
                                             Line_Number : unsigned)
        with Import => True, Convention => C,
             External_Name => "Create_Import_Declarations";
   begin
      Create_Import_Declarations (Builder, Name & ASCII.NUL, Comp_Unit,
                                   File, unsigned (Line_Number));
   end Create_Import_Declarations;

   function Types_Can_Have_Function_Scope return Boolean
   is
      function Types_Can_Have_Function_Scope_C return LLVM_Bool
        with Import => True, Convention => C,
             External_Name => "Types_Can_Have_Function_Scope";
   begin
      return Types_Can_Have_Function_Scope_C /= 0;
   end Types_Can_Have_Function_Scope;

   function Types_Can_Have_Dynamic_Offsets return Boolean
   is
      function Types_Can_Have_Dynamic_Offsets_C return LLVM_Bool
        with Import => True, Convention => C,
             External_Name => "Types_Can_Have_Dynamic_Offsets";
   begin
      return Types_Can_Have_Dynamic_Offsets_C /= 0;
   end Types_Can_Have_Dynamic_Offsets;

   ---------------------------------------------
   -- Types_Can_Have_Multiple_Variant_Members --
   ---------------------------------------------

   function Types_Can_Have_Multiple_Variant_Members return Boolean
   is
      function Types_Can_Have_Multiple_Variant_Members_C return LLVM_Bool
        with Import => True, Convention => C,
             External_Name => "Types_Can_Have_Multiple_Variant_Members";
   begin
      return Types_Can_Have_Multiple_Variant_Members_C /= 0;
   end Types_Can_Have_Multiple_Variant_Members;

   ------------------------------
   -- DI_Expression_Extensions --
   ------------------------------

   function DI_Expression_Extensions return Boolean
   is
      function DI_Expression_Extensions_C return LLVM_Bool
        with Import => True, Convention => C,
             External_Name => "DI_Expression_Extensions";
   begin
      return DI_Expression_Extensions_C /= 0;
   end DI_Expression_Extensions;

   -------------------------------
   -- DI_Subrange_Allows_Member --
   -------------------------------

   function DI_Subrange_Allows_Member return Boolean
   is
      function DI_Subrange_Allows_Member_C return LLVM_Bool
        with Import => True, Convention => C,
             External_Name => "DI_Subrange_Allows_Member";
   begin
      return DI_Subrange_Allows_Member_C /= 0;
   end DI_Subrange_Allows_Member;

   function Create_Global_Variable_Declaration
     (Builder : DI_Builder_T;
      Scope : Metadata_T;
      Name : String;
      Linkage_Name : String;
      File : Metadata_T;
      Line_Number : Physical_Line_Number;
      Ty : Metadata_T;
      Local_To_Unit : Boolean;
      Expr : Metadata_T;
      Decl : Metadata_T;
      Align_In_Bits : Nat) return Metadata_T
   is
      function Create_Global_Variable_Declaration_C
        (Builder : DI_Builder_T;
         Scope : Metadata_T;
         Name : String;
         Linkage_Name : String;
         File : Metadata_T;
         Line_Number : unsigned;
         Ty : Metadata_T;
         Local_To_Unit : LLVM_Bool;
         Expr : Metadata_T;
         Decl : Metadata_T;
         Align_In_Bits : uint32_t)
        return Metadata_T
        with Import => True, Convention => C,
             External_Name => "Create_Global_Variable_Declaration";
   begin
      return Create_Global_Variable_Declaration_C (Builder, Scope,
                                                   Name & ASCII.NUL,
                                                   Linkage_Name & ASCII.NUL,
                                                   File,
                                                   unsigned (Line_Number), Ty,
                                                   Boolean'Pos (Local_To_Unit),
                                                   Expr, Decl,
                                                   uint32_t (Align_In_Bits));
   end Create_Global_Variable_Declaration;

   function Replace_Composite_Elements
     (Builder   : DI_Builder_T;
      Composite : Metadata_T;
      Elements  : Metadata_Array) return Metadata_T
   is
      function Replace_Composite_Elements_C
        (Builder   : DI_Builder_T;
         Composite : Metadata_T;
         Elements  : System.Address;
         Num_Elements : unsigned) return Metadata_T
        with Import => True, Convention => C,
             External_Name => "Replace_Composite_Elements";
   begin
      return Replace_Composite_Elements_C (Builder, Composite,
                                           Elements'Address,
                                           unsigned (Elements'Length));
   end Replace_Composite_Elements;

   -------------------------
   -- Create_Variant_Part --
   -------------------------

   function Create_Variant_Part
     (Builder : DI_Builder_T;
      Discriminator : Metadata_T;
      Elements : Metadata_Array) return Metadata_T
   is
      function Create_Variant_Part_C
        (Builder : DI_Builder_T;
         Discriminator : Metadata_T;
         Elements : System.Address;
         Num_Elements : unsigned) return Metadata_T
        with Import => True, Convention => C,
             External_Name => "Create_Variant_Part";
   begin
      return Create_Variant_Part_C (Builder, Discriminator,
                                    Elements'Address,
                                    unsigned (Elements'Length));
   end Create_Variant_Part;

   ---------------------------
   -- Create_Variant_Member --
   ---------------------------

   function Create_Variant_Member
     (Builder : DI_Builder_T;
      Elements : Metadata_Array;
      Discriminants : Word_Array) return Metadata_T
   is
      function Create_Variant_Member_C
        (Ctx : Context_T;
         Builder : DI_Builder_T;
         Elements : System.Address;
         Num_Elements : unsigned;
         Discriminants : System.Address;
         Num_Discriminants : unsigned) return Metadata_T
        with Import => True, Convention => C,
             External_Name => "Create_Variant_Member";
   begin
      return Create_Variant_Member_C
        (Get_Global_Context,
         Builder,
         Elements'Address,
         unsigned (Elements'Length),
         Discriminants'Address,
         unsigned (Discriminants'Length));
   end Create_Variant_Member;

   -------------------------
   -- Create_Pointer_Type --
   -------------------------

   function Create_Pointer_Type
     (Pointee_Ty    : Metadata_T;
      Size_In_Bits  : ULL;
      Align_In_Bits : Nat) return Metadata_T
   is
      function Create_Pointer_Type_C
        (Builder       : DI_Builder_T;
         Pointee_Ty    : Metadata_T;
         Size_In_Bits  : uint64_t;
         Align_In_Bits : uint32_t) return Metadata_T
        with Import => True, Convention => C,
             External_Name => "Create_Pointer_Type";
   begin
      return Create_Pointer_Type_C (DI_Builder, Pointee_Ty,
                                    uint64_t (Size_In_Bits),
                                    uint32_t (Align_In_Bits));
   end Create_Pointer_Type;

   -------------------
   -- Create_Member --
   -------------------

   function Create_Member (Builder : DI_Builder_T;
                           Scope : Metadata_T;
                           Name : String;
                           File : Metadata_T;
                           Line_Number : Physical_Line_Number;
                           Size_In_Bits : Metadata_T;
                           Offset_In_Bits : Metadata_T;
                           Ty : Metadata_T;
                           Flags : DI_Flags_T;
                           Is_Bit_Field : Boolean) return Metadata_T
   is
      function Create_Member_C (Builder : DI_Builder_T;
                                Scope : Metadata_T;
                                Name : String;
                                File : Metadata_T;
                                Line_Number : unsigned;
                                Size_In_Bits : Metadata_T;
                                Offset_In_Bits : Metadata_T;
                                Ty : Metadata_T;
                                Flags : DI_Flags_T;
                                Is_Bit_Field : LLVM_Bool) return Metadata_T
        with Import => True, Convention => C,
             External_Name => "Create_Member";
   begin
      return Create_Member_C (Builder, Scope, Name & ASCII.NUL, File,
                              unsigned (Line_Number), Size_In_Bits,
                              Offset_In_Bits, Ty, Flags,
                              Boolean'Pos (Is_Bit_Field));
   end Create_Member;

   ---------------------------
   -- DI_Create_Struct_Type --
   ---------------------------

   function DI_Create_Struct_Type
     (Scope          : Metadata_T;
      Name           : String;
      File           : Metadata_T;
      Line_Number    : Physical_Line_Number;
      Size_In_Bits   : Metadata_T;
      Align_In_Bits  : Nat;
      Flags          : DI_Flags_T;
      Derived_From   : Metadata_T;
      Elements       : Metadata_Array;
      Run_Time_Lang  : Nat;
      V_Table_Holder : Metadata_T;
      Unique_Id      : String) return Metadata_T
   is
      function DI_Create_Struct_Type_C
        (Builder        : DI_Builder_T;
         Scope          : Metadata_T;
         Name           : String;
         File           : Metadata_T;
         Line_Number    : unsigned;
         Size_In_Bits   : Metadata_T;
         Align_In_Bits  : uint32_t;
         Flags          : DI_Flags_T;
         Derived_From   : Metadata_T;
         Elements       : System.Address;
         Num_Elements   : unsigned;
         Run_Time_Lang  : unsigned;
         V_Table_Holder : Metadata_T;
         Unique_Id      : String) return Metadata_T
        with Import => True, Convention => C,
             External_Name => "Create_Struct_Type_Non_Constant_Size";
   begin
      return DI_Create_Struct_Type_C (DI_Builder, Scope, Name & ASCII.NUL,
                                      File, unsigned (Line_Number),
                                      Size_In_Bits, uint32_t (Align_In_Bits),
                                      Flags, Derived_From, Elements'Address,
                                      unsigned (Elements'Length),
                                      unsigned (Run_Time_Lang), V_Table_Holder,
                                      Unique_Id & ASCII.NUL);
   end DI_Create_Struct_Type;

end GNATLLVM.Wrapper;
