------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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

with Ada.Unchecked_Conversion;

with Interfaces.C;

with LLVM.Core;  use LLVM.Core;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

package CCG.Helper is

   subtype Double   is Interfaces.C.double;
   subtype unsigned is Interfaces.C.unsigned;

   --  This package contains helper subprograms of the same name as LLVM
   --  API subprograms, but with different parameter or return types or
   --  other minor changes.

   function Const_Real_Get_Double
     (V : Value_T; Loses_Info : out Boolean) return Double
     with Pre => Present (V);

   function Count_Params (V : Value_T) return Nat is
     (Nat (unsigned'(Count_Params (V))))
     with Pre => Present (V);

   function Count_Param_Types (T : Type_T) return Nat is
     (Nat (unsigned'(Count_Param_Types (T))))
     with Pre => Present (T);

   function Get_Param (V : Value_T; P_Num : Nat) return Value_T is
     (Get_Param (V, unsigned (P_Num)))
     with Pre => Present (V), Post => Present (Get_Param'Result);

   function Get_Num_Successors (V : Value_T) return Nat is
     (Nat (unsigned'(Get_Num_Successors (V))))
     with Pre => Is_A_Terminator_Inst (V);

   function Get_Successor (V : Value_T; J : Nat) return Basic_Block_T is
     (Get_Successor (V, unsigned (J)))
     with Pre  => Is_A_Terminator_Inst (V) and then J < Get_Num_Successors (V),
          Post => Present (Get_Successor'Result);

   function Get_Num_Operands (V : Value_T) return Int is
     (Int (Interfaces.C.int'(Get_Num_Operands (V))))
     with Pre => Present (V);

   function Get_Operand (V : Value_T; Idx : Nat) return Value_T is
     (Get_Operand (V, unsigned (Idx)))
     with Pre  => Present (V) and then Idx < Get_Num_Operands (V),
          Post => Present (Get_Operand'Result);

   function Get_Operand0 (V : Value_T) return Value_T is
     (Get_Operand (V, unsigned (0)))
     with Pre  => Present (V) and then Get_Num_Operands (V) > Nat (0),
          Post => Present (Get_Operand0'Result);

   function Get_Operand1 (V : Value_T) return Value_T is
     (Get_Operand (V, unsigned (1)))
     with Pre  => Present (V) and then Get_Num_Operands (V) > Nat (1),
          Post => Present (Get_Operand1'Result);

   function Get_Operand2 (V : Value_T) return Value_T is
     (Get_Operand (V, unsigned (2)))
     with Pre  => Present (V) and then Get_Num_Operands (V) > Nat (2),
          Post => Present (Get_Operand2'Result);

   function Get_Dest_BB (V : Value_T) return Basic_Block_T is
     (Value_As_Basic_Block (Get_Operand0 (V)))
     with Pre  => Is_A_Branch_Inst (V) and then not Is_Conditional (V),
          Post => Present (Get_Dest_BB'Result);

   function Get_True_BB (V : Value_T) return Basic_Block_T is
     (Value_As_Basic_Block (Get_Operand2 (V)))
     with Pre  => Is_A_Branch_Inst (V) and then Is_Conditional (V),
          Post => Present (Get_True_BB'Result);

   function Get_False_BB (V : Value_T) return Basic_Block_T is
     (Value_As_Basic_Block (Get_Operand1 (V)))
     with Pre  => Is_A_Branch_Inst (V) and then Is_Conditional (V),
          Post => Present (Get_False_BB'Result);

   procedure Set_Successor (V : Value_T; J : Nat; BB : Basic_Block_T)
       with Pre => Present (V) and then Present (BB), Inline;

   function Has_Name (T : Type_T) return Boolean is
     (Struct_Has_Name (T))
     with Pre => Present (T);

   function Has_Name (V : Value_T) return Boolean is
     (Value_Has_Name (V))
     with Pre => Present (V);

   function Has_Name (BB : Basic_Block_T) return Boolean is
     (Value_Has_Name (Basic_Block_As_Value (BB)))
     with Pre => Present (BB);

   function Get_Value_Name (BB : Basic_Block_T) return String is
     (Get_Value_Name (Basic_Block_As_Value (BB)))
     with Pre => Present (BB);

   function Count_Struct_Element_Types (T : Type_T) return Nat is
      (Nat (unsigned'(Count_Struct_Element_Types (T))))
      with Pre => Get_Type_Kind (T) = Struct_Type_Kind;

   function Struct_Get_Type_At_Index (T : Type_T; Idx : Nat) return Type_T is
      (Struct_Get_Type_At_Index (T, unsigned (Idx)))
     with Pre => Get_Type_Kind (T) = Struct_Type_Kind;

   function Get_Array_Length (T : Type_T) return Nat is
      (Nat (unsigned'(Get_Array_Length (T))))
      with Pre => Get_Type_Kind (T) = Array_Type_Kind;

   function Is_A_Constant (V : Value_T) return Boolean is
     (Present (Is_A_Constant (V)))
     with Pre => Present (V);

   function Is_A_Constant_Int (V : Value_T) return Boolean is
     (Present (Is_A_Constant_Int (V)))
     with Pre => Present (V);

   function Is_A_Constant_FP (V : Value_T) return Boolean is
     (Present (Is_A_Constant_FP (V)))
     with Pre => Present (V);

   function Is_A_Constant_Array (V : Value_T) return Boolean is
     (Present (Is_A_Constant_Array (V)))
     with Pre => Present (V);

   function Is_A_Constant_Data_Array (V : Value_T) return Boolean is
     (Present (Is_A_Constant_Data_Array (V)))
     with Pre => Present (V);

   function Is_A_Constant_Struct (V : Value_T) return Boolean is
     (Present (Is_A_Constant_Struct (V)))
     with Pre => Present (V);

   function Is_A_Constant_Aggregate_Zero (V : Value_T) return Boolean is
     (Present (Is_A_Constant_Aggregate_Zero (V)))
     with Pre => Present (V);

   function Is_A_Constant_Pointer_Null (V : Value_T) return Boolean is
     (Present (Is_A_Constant_Pointer_Null (V)))
     with Pre => Present (V);

   function Is_A_Constant_Expr (V : Value_T) return Boolean is
     (Present (Is_A_Constant_Expr (V)))
     with Pre => Present (V);

   function Is_A_Function (V : Value_T) return Boolean is
     (Present (Is_A_Function (V)))
     with Pre => Present (V);

   function Is_A_Argument (V : Value_T) return Boolean is
     (Present (Is_A_Argument (V)))
     with Pre => Present (V);

   function Is_A_Instruction (V : Value_T) return Boolean is
     (Present (Is_A_Instruction (V)))
     with Pre => Present (V);

   function Is_A_Terminator_Inst (V : Value_T) return Boolean is
     (Present (Is_A_Terminator_Inst (V)))
     with Pre => Present (V);

   function Is_A_Call_Inst (V : Value_T) return Boolean is
     (Present (Is_A_Call_Inst (V)))
     with Pre => Present (V);

   function Is_A_Dbg_Info_Intrinsic (V : Value_T) return Boolean is
     (Present (Is_A_Dbg_Info_Intrinsic (V)))
     with Pre => Present (V);

   function Is_A_Return_Inst (V : Value_T) return Boolean is
     (Present (Is_A_Return_Inst (V)))
     with Pre => Present (V);

   function Is_A_Alloca_Inst (V : Value_T) return Boolean is
     (Present (Is_A_Alloca_Inst (V)))
     with Pre => Present (V);

   function Is_A_Load_Inst (V : Value_T) return Boolean is
     (Present (Is_A_Load_Inst (V)))
     with Pre => Present (V);

   function Is_A_Store_Inst (V : Value_T) return Boolean is
     (Present (Is_A_Store_Inst (V)))
     with Pre => Present (V);

   function Is_A_Switch_Inst (V : Value_T) return Boolean is
     (Present (Is_A_Switch_Inst (V)))
     with Pre => Present (V);

   function Is_A_Branch_Inst (V : Value_T) return Boolean is
     (Present (Is_A_Branch_Inst (V)))
     with Pre => Present (V);

   function Is_A_Bit_Cast_Inst (V : Value_T) return Boolean is
     (Present (Is_A_Bit_Cast_Inst (V)))
     with Pre => Present (V);

   function Is_A_Extract_Value_Inst (V : Value_T) return Boolean is
     (Present (Is_A_Extract_Value_Inst (V)))
     with Pre => Present (V);

   function Is_A_Insert_Value_Inst (V : Value_T) return Boolean is
     (Present (Is_A_Insert_Value_Inst (V)))
     with Pre => Present (V);

   function Is_APHI_Node (V : Value_T) return Boolean is
     (Present (Is_APHI_Node (V)))
     with Pre => Present (V);

   function Is_A_Basic_Block (V : Value_T) return Boolean is
     (Present (Is_A_Basic_Block (V)))
     with Pre => Present (V);

   function Is_A_Global_Variable (V : Value_T) return Boolean is
     (Present (Is_A_Global_Variable (V)))
     with Pre => Present (V);

   function Is_A_Get_Element_Ptr_Inst (V : Value_T) return Boolean is
     (Present (Is_A_Get_Element_Ptr_Inst (V)))
     with Pre => Present (V);

   function Acts_As_Instruction (V : Value_T) return Boolean is
     (Is_A_Instruction (V) or else Is_A_Constant_Expr (V))
     with Pre => Present (V);

   function Get_Type_Kind (V : Value_T) return Type_Kind_T is
     (Get_Type_Kind (Type_Of (V)))
     with Pre => Present (V);

   function Get_Element_Type (V : Value_T) return Type_T is
     (Get_Element_Type (Type_Of (V)))
     with Pre => Present (V), Post => Present (Get_Element_Type'Result);

   function Get_Int_Type_Width (V : Value_T) return Int is
     (Int (Get_Int_Type_Width (Type_Of (V))))
     with Pre => Present (V);

   --  extractvalue and insertvalue instructions have a list of indices.
   --  The C API returns a pointer to the first of a list of unsigned
   --  values representing the list. We have to do some kludging to actually
   --  access a value in a clean way, so we bury that here.

   type A_unsigned    is access all unsigned;
   type A_Index_Array is access all Index_Array (0 .. 100);
   function Cvt_A is new Ada.Unchecked_Conversion (A_unsigned, A_Index_Array);

   function Get_Num_Indices (V : Value_T) return Nat is
     (Nat (unsigned'(Get_Num_Indices (V))))
     with Pre => Is_A_Extract_Value_Inst (V)
                 or else Is_A_Insert_Value_Inst (V);

   function Get_Index (V : Value_T; Idx : Nat) return Nat is
     (Nat (Cvt_A (Get_Indices (V)) (Idx)))
     with Pre => (Is_A_Extract_Value_Inst (V)
                    or else Is_A_Insert_Value_Inst (V))
                 and then Idx < Get_Num_Indices (V);

   function Get_Opcode (V : Value_T) return Opcode_T is
     (if   Is_A_Instruction (V) then Get_Instruction_Opcode (V)
      else Get_Const_Opcode (V))
     with Pre => Acts_As_Instruction (V);

   function Get_Opcode_Name (Opc : Opcode_T) return String with Inline;

   function Get_Opcode_Name (V : Value_T) return String is
     (Get_Opcode_Name (Get_Opcode (V)))
     with Pre => Is_A_Instruction (V) or else Is_A_Constant_Expr (V);

   function Get_Num_CDA_Elements (V : Value_T) return Nat is
     (Nat (unsigned'(Get_Num_CDA_Elements (V))))
     with Pre => Is_A_Constant_Data_Array (V);

   function Get_Element_As_Constant (V : Value_T; Idx : Nat) return Value_T is
     (Get_Element_As_Constant (V, unsigned (Idx)))
      with Pre  => Is_A_Constant_Data_Array (V)
                   and then Idx < Get_Num_CDA_Elements (V),
           Post => Present (Get_Element_As_Constant'Result);

   function Get_As_String (V : Value_T) return String
     with Pre => Is_A_Constant_Data_Array (V) and then Is_Constant_String (V);

   function Get_Debug_Loc_Filename (V : Value_T) return String
     with Pre => Is_A_Instruction (V) or else Is_A_Function (V)
                 or else Is_A_Global_Variable (V);
   function Get_Debug_Loc_Directory (V : Value_T) return String
     with Pre => Is_A_Instruction (V) or else Is_A_Function (V)
                 or else Is_A_Global_Variable (V);
   function Get_Debug_Loc_Line (V : Value_T) return Physical_Line_Number
     with Pre => Is_A_Instruction (V) or else Is_A_Function (V)
                 or else Is_A_Global_Variable (V);
   function Create_Return (V : Value_T) return Value_T is
     (Create_Return (Get_Global_Context, V))
     with Pre => Present (V), Post => Is_A_Return_Inst (Create_Return'Result);

end CCG.Helper;
