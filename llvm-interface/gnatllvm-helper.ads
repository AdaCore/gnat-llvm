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

with LLVM.Debug_Info; use LLVM.Debug_Info;

with GNATLLVM.GLValue; use GNATLLVM.GLValue;
with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

package GNATLLVM.Helper is

   --  This package contains definitions for "helper" subprograms that do
   --  some small amounts of maniplations and then call the lower-level
   --  LLVM function. If the major purpose of the LLVM function is to
   --  generate code, the helper subprogram should be in
   --  GNATLLVM.Instructions and if it's to perform operations on a
   --  GL_Value that LLVM supports on a Value_T, it should be in
   --  GNATLLVM.GLValue.

   function Create_MDBuilder return MD_Builder_T is
     (Create_MDBuilder_In_Context (Get_Global_Context));

   function Create_TBAA_Scalar_Type_Node
     (Name : String; Size : ULL; Parent : Metadata_T) return Metadata_T
   is
     (Create_TBAA_Scalar_Type_Node (Get_Global_Context, MD_Builder, Name,
                                    Size, Parent))
     with Pre  => Present (Parent),
          Post => Present (Create_TBAA_Scalar_Type_Node'Result);

   function Create_TBAA_Struct_Type_Node
     (Name    : String;
      Size    : ULL;
      Parent  : Metadata_T;
      Offsets : ULL_Array;
      Sizes   : ULL_Array;
      TBAAs   : Metadata_Array) return Metadata_T
   is
      (Create_TBAA_Struct_Type_Node
         (Get_Global_Context, MD_Builder, Name, Size, Offsets'Length,
          Parent, TBAAs'Address, Offsets'Address, Sizes'Address))
     with Pre  => Present (Parent) and then Sizes'First = Offsets'First
                  and then Sizes'Last  = Offsets'Last
                  and then TBAAs'First = Offsets'First
                  and then TBAAs'Last  = Offsets'Last
                  and then (for all T of TBAAs   => Present (T)),
          Post => Present (Create_TBAA_Struct_Type_Node'Result);

   function Create_TBAA_Struct_Node
     (TBAAs   : Metadata_Array;
      Offsets : ULL_Array;
      Sizes   : ULL_Array) return Metadata_T
   is
      (Create_TBAA_Struct_Node
         (Get_Global_Context, MD_Builder, TBAAs'Length, TBAAs'Address,
          Offsets'Address, Sizes'Address))
     with Pre  => TBAAs'First = Offsets'First
                  and then TBAAs'Last  = Offsets'Last
                  and then TBAAs'First = Sizes'First
                  and then TBAAs'Last  = Sizes'Last
                  and then (for all T of TBAAs   => Present (T)),
          Post => Present (Create_TBAA_Struct_Node'Result);

   function Create_TBAA_Access_Tag
     (Base_Type, Access_Type : Metadata_T;
      Offset, Size           : ULL) return Metadata_T
   is
     (Create_TBAA_Access_Tag (MD_Builder, Base_Type, Access_Type,
                              Offset, Size))
     with Pre  => Present (Base_Type) and then Present (Access_Type),
          Post => Present (Create_TBAA_Access_Tag'Result);

   function Create_TBAA_Root return Metadata_T is
     (Create_TBAA_Root (MD_Builder))
     with Post => Present (Create_TBAA_Root'Result);

   function Get_Subprogram_Debug_Metadata (V : GL_Value) return Metadata_T is
     (Get_Subprogram (+V))
     with Pre => Is_A_Function (V), Inline;

   procedure Set_Subprogram_Debug_Metadata (V : GL_Value; M : Metadata_T)
     with Pre => Is_A_Function (V) and then Present (M), Inline;

   function Value_As_Metadata (V : GL_Value) return Metadata_T is
     (Value_As_Metadata (+V))
     with Pre => Present (V), Post => Present (Value_As_Metadata'Result);

   function Metadata_As_Value (M : Metadata_T) return Value_T is
     (Metadata_As_Value (Get_Global_Context, M))
     with Pre => Present (M), Post => Present (Metadata_As_Value'Result);

   function Const_32_As_Metadata (U : Uint) return Metadata_T is
     (Value_As_Metadata (+Const_Int_32 (U)))
     with Pre => Present (U), Post => Present (Const_32_As_Metadata'Result);

   function MD_String (S : String) return Metadata_T is
     (MD_String_In_Context_2 (Get_Global_Context, S, S'Length))
     with Post => Present (MD_String'Result);

   function MD_Node (MDs : Metadata_Array) return Metadata_T is
     (MD_Node_In_Context_2 (Get_Global_Context, MDs'Address, MDs'Length))
     with Pre  => (for all M of MDs => Present (M)),
          Post => Present (MD_Node'Result);

   procedure Add_Named_Metadata_Operand (Name : String; V : Value_T)
     with Pre => Present (V), Inline;

   procedure Add_Named_Metadata_Operand (Name : String; M : Metadata_T)
     with Pre => Present (M), Inline;

   procedure Set_Current_Debug_Location (MD : Metadata_T)
     with Pre => Present (MD), Inline;

   function DI_Create_Compile_Unit
     (Lang                     : DWARF_Source_Language_T;
      File_Ref                 : Metadata_T;
      Producer                 : String;
      Is_Optimized             : Boolean;
      Flags                    : String;
      Runtime_Ver              : Int;
      Split_Name               : String;
      Kind                     : DWARF_Emission_Kind_T;
      DWO_Id                   : Int;
      Split_Debug_Inlining     : Boolean;
      Debug_Info_For_Profiling : Boolean;
      Sys_Root                 : String;
      SDK                      : String) return Metadata_T
   is
     (DI_Create_Compile_Unit (DI_Builder, Lang, File_Ref, Producer,
                              Producer'Length, Is_Optimized,
                              Flags, Flags'Length,
                              unsigned (Runtime_Ver),
                              Split_Name, Split_Name'Length, Kind,
                              unsigned (DWO_Id), Split_Debug_Inlining,
                              Debug_Info_For_Profiling,
                              Sys_Root, Sys_Root'Length, SDK, SDK'Length))
     with Pre  => Present (File_Ref),
          Post => Present (DI_Create_Compile_Unit'Result);

   function DI_Create_File
     (Filename : String; Directory : String) return Metadata_T
   is
     (DI_Create_File (DI_Builder, Filename, Filename'Length,
                      Directory, Directory'Length))
     with Post => Present (DI_Create_File'Result);

   function DI_Builder_Create_Subroutine_Type
     (File            : Metadata_T;
      Parameter_Types : Metadata_Array;
      Flags           : DI_Flags_T) return Metadata_T
   is
     (DI_Builder_Create_Subroutine_Type
        (DI_Builder, File,
         Parameter_Types'Address, unsigned (Parameter_Types'Length), Flags))
     with Pre  => Present (File),
          Post => Present (DI_Builder_Create_Subroutine_Type'Result);

   function DI_Create_Function
     (Scope            : Metadata_T;
      Name             : String;
      Linkage_Name     : String;
      File             : Metadata_T;
      Line_No          : Physical_Line_Number;
      Ty               : Metadata_T;
      Is_Local_To_Unit : Boolean;
      Is_Definition    : Boolean;
      Scope_Line       : Physical_Line_Number;
      Flags            : DI_Flags_T;
      Is_Optimized     : Boolean) return Metadata_T
   is
     (DI_Create_Function (DI_Builder, Scope, Name, Name'Length,
                          Linkage_Name, Linkage_Name'Length, File,
                          unsigned (Line_No), Ty, Is_Local_To_Unit,
                          Is_Definition, unsigned (Scope_Line), Flags,
                          Is_Optimized))
     with Pre  => Present (Scope) and then Present (File),
          Post => Present (DI_Create_Function'Result);

   function DI_Builder_Create_Lexical_Block
     (Scope  : Metadata_T;
      File   : Metadata_T;
      Line   : Physical_Line_Number;
      Column : Column_Number) return Metadata_T
   is
     (DI_Builder_Create_Lexical_Block
        (DI_Builder, Scope, File, unsigned (Line), unsigned (Column)))
     with Pre  => Present (Scope) and then Present (File),
          Post => Present (DI_Builder_Create_Lexical_Block'Result);

   function DI_Builder_Create_Debug_Location
     (Line       : Physical_Line_Number;
      Column     : Column_Number;
      Scope      : Metadata_T;
      Inlined_At : Metadata_T) return Metadata_T
   is
     (DI_Builder_Create_Debug_Location
        (Get_Global_Context, unsigned (Line), unsigned (Column), Scope,
         Inlined_At))
     with Pre  => Present (Scope),
          Post => Present (DI_Builder_Create_Debug_Location'Result);

   function DI_Create_Unspecified_Type (Name : String) return Metadata_T is
     (DI_Create_Unspecified_Type (DI_Builder, Name, Name'Length))
     with Post => Present (DI_Create_Unspecified_Type'Result);

   function DI_Create_Basic_Type
     (Name         : String;
      Size_In_Bits : ULL;
      Encoding     : DWARF_Type_Encoding_T;
      Flags        : DI_Flags_T) return Metadata_T
   is
     (DI_Create_Basic_Type
        (DI_Builder, Name, Name'Length, uint64_t (Size_In_Bits), Encoding,
         Flags))
     with Post => Present (DI_Create_Basic_Type'Result);

   function DI_Create_Pointer_Type
     (Pointee_Ty    : Metadata_T;
      Size_In_Bits  : ULL;
      Align_In_Bits : Nat;
      Address_Space : unsigned;
      Name          : String) return Metadata_T
   is
     (DI_Create_Pointer_Type
        (DI_Builder, Pointee_Ty, uint64_t (Size_In_Bits),
         uint32_t (Align_In_Bits), Address_Space, Name, Name'Length))
     with Pre  => Present (Pointee_Ty),
          Post => Present (DI_Create_Pointer_Type'Result);

   function DI_Builder_Get_Or_Create_Subrange
     (Lower_Bound, Count : LLI) return Metadata_T
   is
     (DI_Builder_Get_Or_Create_Subrange
        (DI_Builder, int64_t (Lower_Bound), int64_t (Count)))
     with Post => Present (DI_Builder_Get_Or_Create_Subrange'Result);

   function DI_Builder_Create_Array_Type
     (Size          : ULL;
      Align_In_Bits : Nat;
      Ty            : Metadata_T;
      Subscripts    : Metadata_Array) return Metadata_T
   is
     (DI_Builder_Create_Array_Type
        (DI_Builder, uint64_t (Size), uint32_t (Align_In_Bits), Ty,
         Subscripts'Address, unsigned (Subscripts'Length)))
     with Pre  => Present (Ty),
          Post => Present (DI_Builder_Create_Array_Type'Result);

   function DI_Create_Struct_Type
     (Scope          : Metadata_T;
      Name           : String;
      File           : Metadata_T;
      Line_Number    : Physical_Line_Number;
      Size_In_Bits   : ULL;
      Align_In_Bits  : Nat;
      Flags          : DI_Flags_T;
      Derived_From   : Metadata_T;
      Elements       : Metadata_Array;
      Run_Time_Lang  : Nat;
      V_Table_Holder : Metadata_T;
      Unique_Id      : String) return Metadata_T
   is
     (DI_Create_Struct_Type (DI_Builder, Scope, Name, Name'Length, File,
                             unsigned (Line_Number), uint64_t (Size_In_Bits),
                             uint32_t (Align_In_Bits), Flags, Derived_From,
                             Elements'Address, unsigned (Elements'Length),
                             unsigned (Run_Time_Lang), V_Table_Holder,
                             Unique_Id, Unique_Id'Length))
     with Pre =>  (for all MD of Elements => Present (MD)),
          Post => Present (DI_Create_Struct_Type'Result);

   function DI_Create_Enumeration_Type
     (Scope         : Metadata_T;
      Name          : String;
      File          : Metadata_T;
      Line_Number   : Physical_Line_Number;
      Size_In_Bits  : ULL;
      Align_In_Bits : Nat;
      Elements      : Metadata_Array;
      Class_Ty      : Metadata_T) return Metadata_T
   is
     (DI_Create_Enumeration_Type
        (DI_Builder, Scope, Name, Name'Length, File, unsigned (Line_Number),
         uint64_t (Size_In_Bits), uint32_t (Align_In_Bits),
         Elements'Address, Elements'Length, Class_Ty))
     with Pre  => Present (File)
                  and then (for all MD of Elements => Present (MD)),
          Post => Present (DI_Create_Enumeration_Type'Result);

   function DI_Create_Member_Type
     (Scope          : Metadata_T;
      Name           : String;
      File           : Metadata_T;
      Line_No        : Physical_Line_Number;
      Size_In_Bits   : ULL;
      Align_In_Bits  : Nat;
      Offset_In_Bits : ULL;
      Ty             : Metadata_T;
      Flags          : DI_Flags_T := DI_Flag_Zero) return Metadata_T
   is
     (DI_Create_Member_Type
        (DI_Builder, Scope, Name, Name'Length, File, unsigned (Line_No),
         uint64_t (Size_In_Bits), uint32_t (Align_In_Bits),
         uint64_t (Offset_In_Bits), Flags, Ty))
     with Pre  => Present (File) and then Present (Ty),
          Post => Present (DI_Create_Member_Type'Result);

   function DI_Create_Bit_Field_Member_Type
     (Scope                  : Metadata_T;
      Name                   : String;
      File                   : Metadata_T;
      Line_Number            : Physical_Line_Number;
      Size_In_Bits           : ULL;
      Offset_In_Bits         : ULL;
      Storage_Offset_In_Bits : ULL;
      C_Type                 : Metadata_T;
      Flags                  : DI_Flags_T := DI_Flag_Bit_Field)
     return LLVM.Types.Metadata_T
   is
    (DI_Create_Bit_Field_Member_Type
       (DI_Builder, Scope, Name, Name'Length, File, unsigned (Line_Number),
        uint64_t (Size_In_Bits), uint64_t (Offset_In_Bits),
        uint64_t (Storage_Offset_In_Bits), Flags, C_Type))
    with Pre  => Present (File) and then Present (C_Type),
         Post => Present (DI_Create_Bit_Field_Member_Type'Result);

   function DI_Create_Enumerator
     (Name : String; Value : LLI; Is_Unsigned : Boolean) return Metadata_T
   is
     (DI_Create_Enumerator (DI_Builder, Name, Name'Length, int64_t (Value),
                            Is_Unsigned))
     with Post => Present (DI_Create_Enumerator'Result);

   function DI_Create_Global_Variable_Expression
     (Scope         : Metadata_T;
      Name          : String;
      Linkage       : String;
      File          : Metadata_T;
      Line_No       : Physical_Line_Number;
      Ty            : Metadata_T;
      Local_To_Unit : Boolean;
      Expr          : Metadata_T;
      Decl          : Metadata_T;
      Align_In_Bits : Nat) return Metadata_T
   is
     (DI_Create_Global_Variable_Expression
        (DI_Builder, Scope, Name, Name'Length, Linkage, Linkage'Length, File,
         unsigned (Line_No), Ty, Local_To_Unit, Expr, Decl,
         uint32_t (Align_In_Bits)))
     with Pre  => Present (Scope) and then Present (File)
                  and then Present (Expr),
          Post => Present (DI_Create_Global_Variable_Expression'Result);

   function DI_Create_Auto_Variable
     (Scope           : Metadata_T;
      Name            : String;
      File            : Metadata_T;
      Line_No         : Physical_Line_Number;
      Ty              : Metadata_T;
      Always_Preserve : Boolean;
      Flags           : DI_Flags_T;
      Align_In_Bits   : Nat) return Metadata_T
   is
     (DI_Create_Auto_Variable
        (DI_Builder, Scope, Name, Name'Length, File, unsigned (Line_No),
         Ty, Always_Preserve, Flags, uint32_t (Align_In_Bits)))
     with Pre  => Present (Scope) and then Present (File)
                  and then Present (Ty),
          Post => Present (DI_Create_Auto_Variable'Result);

   function DI_Create_Parameter_Variable
     (Scope           : Metadata_T;
      Name            : String;
      Arg_No          : Nat;
      File            : Metadata_T;
      Line_No         : Physical_Line_Number;
      Ty              : Metadata_T;
      Always_Preserve : Boolean;
      Flags           : DI_Flags_T) return Metadata_T
   is
     (DI_Create_Parameter_Variable
        (DI_Builder, Scope, Name, Name'Length, unsigned (Arg_No), File,
         unsigned (Line_No), Ty, Always_Preserve, Flags))
     with Pre  => Present (Scope) and then Present (File)
                  and then Present (Ty),
          Post => Present (DI_Create_Parameter_Variable'Result);

   function DI_Builder_Insert_Dbg_Value_At_End
     (V         : GL_Value;
      Var_Info  : Metadata_T;
      Expr      : Metadata_T;
      Debug_Loc : Metadata_T;
      Block     : Basic_Block_T) return Value_T
   is
     (DI_Builder_Insert_Dbg_Value_At_End
        (DI_Builder, +V, Var_Info, Expr, Debug_Loc, Block))
     with Pre  => Present (V) and then Present (Var_Info)
                  and then Present (Expr) and then Present (Debug_Loc)
                  and then Present (Block),
          Post => Present (DI_Builder_Insert_Dbg_Value_At_End'Result);

   function DI_Builder_Insert_Declare_At_End
     (V         : GL_Value;
      Var_Info  : Metadata_T;
      Expr      : Metadata_T;
      Debug_Loc : Metadata_T;
      Block     : Basic_Block_T) return Value_T
   is
     (DI_Builder_Insert_Declare_At_End
        (DI_Builder, +V, Var_Info, Expr, Debug_Loc, Block))
     with Pre  => Present (V) and then Present (Var_Info)
                  and then Present (Expr) and then Present (Debug_Loc)
                  and then Present (Block),
          Post => Present (DI_Builder_Insert_Declare_At_End'Result);

end GNATLLVM.Helper;
