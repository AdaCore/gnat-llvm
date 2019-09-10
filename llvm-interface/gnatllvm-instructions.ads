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

with LLVM.Core;       use LLVM.Core;

with GNATLLVM.GLValue; use GNATLLVM.GLValue;
with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

package GNATLLVM.Instructions is

   function Alloca
     (GT        : GL_Type;
      Def_Ident : Entity_Id := Empty;
      Name      : String    := "") return GL_Value
     with Pre  => Present (GT), Post => Is_Reference (Alloca'Result),
          Inline;

   function Array_Alloca
     (GT        : GL_Type;
      Num_Elts  : GL_Value;
      Def_Ident : Entity_Id := Empty;
      Name      : String    := "") return GL_Value
     with Pre  => Present (GT) and then Present (Num_Elts),
          Post => Is_Reference (Array_Alloca'Result), Inline;

   function Int_To_Ptr
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Pointer (Int_To_Ptr'Result), Inline;

   function Ptr_To_Int
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Pointer (V) and then Present (GT),
          Post => Is_Discrete_Or_Fixed_Point_Type (Ptr_To_Int'Result), Inline;

   function Ptr_To_Size_Type
     (V : GL_Value; Name : String := "") return GL_Value
   is
     (Ptr_To_Int (V, Size_GL_Type, Name))
     with Pre  => Is_Pointer (V),
          Post => Is_Discrete_Or_Fixed_Point_Type (Ptr_To_Size_Type'Result);

   function Bit_Cast
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Present (V) and then not Is_Pointer (V)
                  and then Present (GT),
          Post => Present (Bit_Cast'Result), Inline;

   function Bit_Cast (V, T : GL_Value; Name : String := "") return GL_Value is
     (G_From (Bit_Cast (IR_Builder, LLVM_Value (V), Type_Of (T), Name), T))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Bit_Cast'Result);

   function Bit_Cast
     (V : GL_Value; T : Type_T; Name : String := "") return GL_Value
   is
     (G (Bit_Cast (IR_Builder, LLVM_Value (V), T, Name), Related_Type (V),
         Unknown))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Bit_Cast'Result);

   function Bit_Cast_To_Relationship
     (V    : GL_Value;
      T    : Type_T;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
     (G (Bit_Cast (IR_Builder, LLVM_Value (V), T, Name), Related_Type (V), R))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Bit_Cast_To_Relationship'Result);

   function Pointer_Cast
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Pointer (V) and then Present (GT),
          Post => Is_Pointer (Pointer_Cast'Result), Inline;

   function Pointer_Cast
     (V, T : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Pointer_Cast (IR_Builder, LLVM_Value (V), Type_Of (T), Name), T))
     with Pre  => Is_Pointer (V) and then Is_Pointer (T),
          Post => Is_Pointer (Pointer_Cast'Result);

   function Pointer_Cast_To_Relationship
     (V    : GL_Value;
      T    : Type_T;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
     (G (Pointer_Cast (IR_Builder, LLVM_Value (V), T, Name),
         Related_Type (V), R))
     with Pre  => Is_Pointer (V) and then Present (T),
          Post => Is_Pointer (Pointer_Cast_To_Relationship'Result);

   function Ptr_To_Ref
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Pointer (V) and then Present (GT),
          Post => Is_Pointer (Ptr_To_Ref'Result), Inline;

   function Ptr_To_Ref (V, T : GL_Value; Name : String := "") return GL_Value
     with Pre  => Is_Pointer (V) and then Is_Pointer (T),
          Post => Is_Pointer (Ptr_To_Ref'Result), Inline;

   function Ptr_To_Relationship
     (V    : GL_Value;
      GT   : GL_Type;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
     with Pre  => Is_Pointer (V) and then Present (GT),
          Post => Is_Pointer (Ptr_To_Relationship'Result), Inline;

   function Ptr_To_Relationship
     (V    : GL_Value;
      T    : Type_T;
      R    : GL_Relationship;
      Name : String := "") return GL_Value is
     (GM (Pointer_Cast (IR_Builder, LLVM_Value (V), T, Name),
          Related_Type (V), R, V))
     with Pre  => Is_Pointer (V) and then Present (T),
          Post => Is_Pointer (Ptr_To_Relationship'Result), Inline;

   function Ptr_To_Relationship
     (V, T : GL_Value;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
     with Pre  => Is_Pointer (V) and then Present (T),
          Post => Is_Pointer (Ptr_To_Relationship'Result), Inline;

   function Trunc
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Discrete_Or_Fixed_Point_Type (Trunc'Result);

   function Trunc_To_Relationship
     (V    : GL_Value;
      T    : Type_T;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
     with Pre  => Present (V) and then Present (T),
          Post => Present (Trunc_To_Relationship'Result), Inline;

   function S_Ext
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Discrete_Or_Fixed_Point_Type (S_Ext'Result), Inline;

   function Z_Ext
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Discrete_Or_Fixed_Point_Type (Z_Ext'Result), Inline;

   function Z_Ext_To_Relationship
     (V    : GL_Value;
      T    : Type_T;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
     (G (Z_Ext (IR_Builder, LLVM_Value (V), T, Name), Related_Type (V), R))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Z_Ext_To_Relationship'Result);

   function FP_Trunc
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Floating_Point_Type (V) and then Present (GT),
          Post => Is_Floating_Point_Type (FP_Trunc'Result), Inline;

   function FP_Ext
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Floating_Point_Type (V) and then Present (GT),
          Post => Is_Floating_Point_Type (FP_Ext'Result), Inline;

   function FP_To_SI
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Floating_Point_Type (V) and then Present (GT),
          Post => Is_Discrete_Or_Fixed_Point_Type (FP_To_SI'Result), Inline;

   function FP_To_UI
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Floating_Point_Type (V) and then Present (GT),
          Post => Is_Discrete_Or_Fixed_Point_Type (FP_To_UI'Result), Inline;

   function UI_To_FP
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Floating_Point_Type (UI_To_FP'Result), Inline;

   function SI_To_FP
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Floating_Point_Type (SI_To_FP'Result), Inline;

   function Int_To_Ptr
     (V, T : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Int_To_Ptr (IR_Builder, LLVM_Value (V), Type_Of (T), Name), T))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V)
                  and then Is_Pointer (T),
          Post => Is_Pointer (Int_To_Ptr'Result);

   function Ptr_To_Int
     (V, T : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Ptr_To_Int (IR_Builder, LLVM_Value (V), Type_Of (T), Name), T))
     with Pre  => Is_Pointer (V)
                  and then Is_Discrete_Or_Fixed_Point_Type (T),
          Post => Is_Discrete_Or_Fixed_Point_Type (Ptr_To_Int'Result);

   function Trunc (V, T : GL_Value; Name : String := "") return GL_Value is
     (Trunc (V, Related_Type (T), Name))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V)
                  and then Is_Discrete_Or_Fixed_Point_Type (T),
          Post => Is_Discrete_Or_Fixed_Point_Type (Trunc'Result);

   function S_Ext (V, T : GL_Value; Name : String := "") return GL_Value is
     (S_Ext (V, Related_Type (T), Name))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V)
                  and then Is_Discrete_Or_Fixed_Point_Type (T),
          Post => Is_Discrete_Or_Fixed_Point_Type (S_Ext'Result);

   function Z_Ext (V, T : GL_Value; Name : String := "") return GL_Value is
     (Z_Ext (V, Related_Type (T), Name))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V)
                  and then Is_Discrete_Or_Fixed_Point_Type (T),
          Post => Is_Discrete_Or_Fixed_Point_Type (Z_Ext'Result);

   function FP_Trunc (V, T : GL_Value; Name : String := "") return GL_Value is
     (FP_Trunc (V, Related_Type (T), Name))
     with Pre  => Is_Floating_Point_Type (V)
                  and then Is_Floating_Point_Type (T),
          Post => Is_Floating_Point_Type (FP_Trunc'Result);

   function FP_Ext (V, T : GL_Value; Name : String := "") return GL_Value is
     (FP_Ext (V, Related_Type (T), Name))
     with Pre  => Is_Floating_Point_Type (V)
                  and then Is_Floating_Point_Type (T),
          Post => Is_Floating_Point_Type (FP_Ext'Result);

   function FP_To_SI (V, T : GL_Value; Name : String := "") return GL_Value is
     (FP_To_SI (V, Related_Type (T), Name))
     with Pre  => Is_Floating_Point_Type (V)
                  and then Is_Discrete_Or_Fixed_Point_Type (T),
          Post => Is_Discrete_Or_Fixed_Point_Type (FP_To_SI'Result);

   function FP_To_UI (V, T : GL_Value; Name : String := "") return GL_Value is
     (FP_To_UI (V, Related_Type (T), Name))
     with Pre  => Is_Floating_Point_Type (V)
                  and then Is_Discrete_Or_Fixed_Point_Type (T),
          Post => Is_Discrete_Or_Fixed_Point_Type (FP_To_UI'Result);

   function UI_To_FP (V, T : GL_Value; Name : String := "") return GL_Value is
     (UI_To_FP (V, Related_Type (T), Name))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V)
                  and then Is_Floating_Point_Type (T),
          Post => Is_Floating_Point_Type (UI_To_FP'Result);

   function SI_To_FP (V, T : GL_Value; Name : String := "") return GL_Value is
     (SI_To_FP (V, Related_Type (T), Name))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V)
                  and then Is_Floating_Point_Type (T),
          Post => Is_Floating_Point_Type (SI_To_FP'Result);

   procedure Store (Expr : GL_Value; Ptr : GL_Value)
     with Pre => Present (Expr)
                 and then Present (Ptr) and then Is_Reference (Ptr),
          Inline;

   function Load (Ptr : GL_Value; Name : String := "") return GL_Value
     with Pre  => Is_Reference (Ptr), Post => Present (Load'Result), Inline;

   function I_Cmp
     (Op       : Int_Predicate_T;
      LHS, RHS : GL_Value;
      Name     : String := "") return GL_Value
   is
     (G (I_Cmp (IR_Builder, Op, LLVM_Value (LHS), LLVM_Value (RHS), Name),
         Boolean_GL_Type, Boolean_Data))
     with Pre  => Present (LHS) and then Present (RHS),
          Post => Present (I_Cmp'Result);

   function F_Cmp
     (Op       : Real_Predicate_T;
      LHS, RHS : GL_Value;
      Name     : String := "") return GL_Value
   is
     (G (F_Cmp (IR_Builder, Op, LLVM_Value (LHS), LLVM_Value (RHS), Name),
         Boolean_GL_Type, Boolean_Data))
     with Pre  => Is_Floating_Point_Type (LHS)
                  and then Is_Floating_Point_Type (RHS),
          Post => Present (F_Cmp'Result);

   function Add_Sub
     (LHS, RHS : GL_Value; Is_Add : Boolean; Name : String) return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                  and then Is_Discrete_Or_Fixed_Point_Type (RHS),
          Post => Is_Discrete_Or_Fixed_Point_Type (Add_Sub'Result);

   function Add
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Add_Sub (LHS, RHS, True, Name))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                  and then Is_Discrete_Or_Fixed_Point_Type (RHS),
          Post => Is_Discrete_Or_Fixed_Point_Type (Add'Result);

   function Sub
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Add_Sub (LHS, RHS, False, Name))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                  and then Is_Discrete_Or_Fixed_Point_Type (RHS),
          Post => Is_Discrete_Or_Fixed_Point_Type (Sub'Result);

   function Mul
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                  and then Is_Discrete_Or_Fixed_Point_Type (RHS),
          Post => Is_Discrete_Or_Fixed_Point_Type (Mul'Result);

   function S_Div
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (S_Div (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
                 LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (S_Div'Result);

   function U_Div
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (U_Div (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
                 LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (U_Div'Result);

   function S_Rem
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (S_Rem (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
                 LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (S_Rem'Result);

   function U_Rem
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (U_Rem (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
                 LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (U_Rem'Result);

   function Build_And
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (Build_And (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS),
                            Name),
                 LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (Build_And'Result);

   function Build_Or
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (Build_Or (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS),
                           Name),
                 LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (Build_Or'Result);

   function Build_Xor
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (Build_Xor (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS),
                           Name),
                 LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (Build_Xor'Result);

   function F_Add
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (F_Add (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
                 LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Is_Floating_Point_Type (LHS)
                   and then Is_Floating_Point_Type (RHS),
           Post => Is_Floating_Point_Type (F_Add'Result);

   function F_Sub
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (F_Sub (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
                 LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Is_Floating_Point_Type (LHS)
                   and then Is_Floating_Point_Type (RHS),
           Post => Is_Floating_Point_Type (F_Sub'Result);

   function F_Mul
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (F_Mul (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
                 LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Is_Floating_Point_Type (LHS)
                   and then Is_Floating_Point_Type (RHS),
           Post => Is_Floating_Point_Type (F_Mul'Result);

   function F_Div
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (F_Div (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
                 LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Is_Floating_Point_Type (LHS)
                   and then Is_Floating_Point_Type (RHS),
           Post => Is_Floating_Point_Type (F_Div'Result);

   function Shl
     (V              : GL_Value;
      Count          : GL_Value;
      Name           : String  := "";
      Allow_Overflow : Boolean := False) return GL_Value
   is
      (G_From ((if   Allow_Overflow
                then Shl (IR_Builder, LLVM_Value (V), LLVM_Value (Count), Name)
                else Set_Arith_Attrs
                  (Shl (IR_Builder, LLVM_Value (V), LLVM_Value (Count), Name),
                   V)),
               V))
      with Pre  => Present (V) and then Present (Count),
           Post => Present (Shl'Result);

   function L_Shr
     (V, Count : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (L_Shr (IR_Builder, LLVM_Value (V), LLVM_Value (Count), Name), V))
      with Pre  => Present (V) and then Present (Count),
           Post => Present (L_Shr'Result);

   function A_Shr
     (V, Count : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (A_Shr (IR_Builder, LLVM_Value (V), LLVM_Value (Count), Name), V))
      with Pre  => Present (V) and then Present (Count),
           Post => Present (A_Shr'Result);

   function Build_Not
     (V : GL_Value; Name : String := "") return GL_Value
   is
      (G_From (Build_Not (IR_Builder, LLVM_Value (V), Name), V))
      with Pre  => Present (V),
           Post => Present (Build_Not'Result);

   function Neg
     (V : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Set_Arith_Attrs (Neg (IR_Builder, LLVM_Value (V), Name), V), V))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (V),
           Post => Is_Discrete_Or_Fixed_Point_Type (Neg'Result);

   function F_Neg
     (V : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (F_Neg (IR_Builder, LLVM_Value (V), Name), V))
     with Pre  => Is_Floating_Point_Type (V),
          Post => Is_Floating_Point_Type (F_Neg'Result);

   function "+" (LHS, RHS : GL_Value)   return GL_Value is
     (Add (LHS, RHS));
   function "-" (LHS, RHS : GL_Value)   return GL_Value is
     (Sub (LHS, RHS));
   function "-" (V : GL_Value)          return GL_Value is
     (Neg (V));
   function "*" (LHS, RHS : GL_Value)   return GL_Value is
     (Mul (LHS, RHS));
   function "/" (LHS, RHS : GL_Value)   return GL_Value is
     (U_Div (LHS, RHS));
   function "mod" (LHS, RHS : GL_Value) return GL_Value is
     (U_Rem (LHS, RHS));

   function "+" (LHS : Uint; RHS : GL_Value) return Uint is
     (LHS + UI_From_GL_Value (RHS))
     with Pre  => Present (LHS) and then Is_A_Const_Int (RHS);
   function "+" (LHS : GL_Value; RHS : Uint) return Uint is
     (RHS + UI_From_GL_Value (LHS))
     with Pre  => Present (RHS) and then Is_A_Const_Int (LHS);

   function "-" (LHS : Uint; RHS : GL_Value) return Uint is
     (LHS - UI_From_GL_Value (RHS))
     with Pre  => Present (LHS) and then Is_A_Const_Int (RHS);

   function "<" (LHS, RHS : GL_Value) return Boolean is
     (I_Cmp ((if Is_Unsigned_Type (LHS) then Int_ULT else Int_SLT),
             LHS, RHS) = Const_True);
   function "<=" (LHS, RHS : GL_Value) return Boolean is
     (I_Cmp ((if Is_Unsigned_Type (LHS) then Int_ULE else Int_SLE),
             LHS, RHS) = Const_True);
   function ">" (LHS, RHS : GL_Value) return Boolean is
     (I_Cmp ((if Is_Unsigned_Type (LHS) then Int_UGT else Int_SGT),
             LHS, RHS) = Const_True);
   function ">=" (LHS, RHS : GL_Value) return Boolean is
     (I_Cmp ((if Is_Unsigned_Type (LHS) then Int_UGE else Int_SGE),
             LHS, RHS) = Const_True);

   function "+" (LHS : GL_Value; RHS : Int)   return GL_Value is
     (LHS + Const_Int (LHS, UI_From_Int (RHS)));
   function "-" (LHS : GL_Value; RHS : Int)   return GL_Value is
     (LHS - Const_Int (LHS, UI_From_Int (RHS)));
   function "*" (LHS : GL_Value; RHS : Int)   return GL_Value is
     (LHS * Const_Int (LHS, UI_From_Int (RHS)));
   function "/" (LHS : GL_Value; RHS : Int)   return GL_Value is
     (LHS / Const_Int (LHS, UI_From_Int (RHS)));
   function "mod" (LHS : GL_Value; RHS : Int) return GL_Value is
     (LHS mod Const_Int (LHS, UI_From_Int (RHS)));

   function To_Bytes (V : GL_Value) return GL_Value is
     ((V + (BPU - 1)) / BPU)
     with Pre => Present (V), Post => Present (To_Bytes'Result);

   function Build_Select
     (C_If, C_Then, C_Else : GL_Value; Name : String := "")
     return GL_Value
   is
     ((if   C_If = Const_True then C_Then elsif C_If = Const_False then C_Else
       else G_From (Build_Select (IR_Builder, C_If => LLVM_Value (C_If),
                                  C_Then => LLVM_Value (C_Then),
                                  C_Else => LLVM_Value (C_Else), Name => Name),
                    C_Then)))
     with Pre  => Ekind (Full_Etype (C_If)) in Enumeration_Kind
                  and then Is_Elementary_Type (C_Then)
                  and then Is_Elementary_Type (C_Else),
          Post => Is_Elementary_Type (Build_Select'Result);

   procedure Build_Cond_Br
     (C_If : GL_Value; C_Then, C_Else : Basic_Block_T)
     with Pre => Ekind (Full_Etype (C_If)) in Enumeration_Kind
                 and then Present (C_Then) and then Present (C_Else),
          Inline;

   procedure Build_Ret (V : GL_Value)
     with Pre => Present (V), Inline;

   procedure Build_Ret_Void
     with Inline;

   procedure Build_Unreachable
     with Inline;

   function Build_Phi
     (GL_Values : GL_Value_Array;
      BBs       : Basic_Block_Array;
      Name      : String := "") return GL_Value
     with Pre  => GL_Values'First = BBs'First
                  and then GL_Values'Last = BBs'Last,
          Post => Present (Build_Phi'Result);

   function Int_To_Ref
     (V : GL_Value; GT : GL_Type; Name : String := "")
     return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Pointer (Int_To_Ref'Result), Inline;
   --  Similar to Int_To_Ptr, but GT is the Designed_Type, not the
   --  access type.

   function Int_To_Relationship
     (V    : GL_Value;
      GT   : GL_Type;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Pointer (Int_To_Relationship'Result), Inline;
   --  Similar to Int_To_Ptr, but specify the relationship to GT

   function Atomic_RMW
     (Op            : Atomic_RMW_Bin_Op_T;
      Ptr           : GL_Value;
      V             : GL_Value;
      Order         : Atomic_Ordering_T :=
        Atomic_Ordering_Sequentially_Consistent;
      Single_Thread : Boolean := False) return GL_Value
   is
      (G_From (Atomic_RMW (IR_Builder, Op, LLVM_Value (Ptr), LLVM_Value (V),
                           Order, Single_Thread),
               V))
      with Pre  => Is_Pointer (Ptr) and then Present (V),
           Post => Present (Atomic_RMW'Result);

   function Atomic_Cmp_Xchg
     (Ptr              : GL_Value;
      Cmp              : GL_Value;
      C_New            : GL_Value;
      Success_Ordering : Atomic_Ordering_T :=
        Atomic_Ordering_Sequentially_Consistent;
      Failure_Ordering : Atomic_Ordering_T :=
        Atomic_Ordering_Sequentially_Consistent;
      Single_Thread    : Boolean           := False) return GL_Value
   is
     (G (Atomic_Cmp_Xchg (IR_Builder, LLVM_Value (Ptr), LLVM_Value (Cmp),
                          LLVM_Value (C_New), Success_Ordering,
                          Failure_Ordering, Single_Thread),
         Related_Type (Cmp), Boolean_And_Data));

   function Extract_Value
     (GT    : GL_Type;
      Arg   : GL_Value;
      Index : unsigned;
      Name  : String := "") return GL_Value
   is
     (G (Extract_Value (IR_Builder, LLVM_Value (Arg), Index, Name), GT))
     with  Pre  => Present (Arg) and then Present (GT),
           Post => Present (Extract_Value'Result);

   function Extract_Value_To_Ref
     (GT    : GL_Type;
      Arg   : GL_Value;
      Index : unsigned;
      Name  : String := "") return GL_Value
   is
     (G_Ref (Extract_Value (IR_Builder, LLVM_Value (Arg), Index, Name), GT))
     with  Pre  => Present (Arg) and then Present (GT),
           Post => Is_Pointer (Extract_Value_To_Ref'Result);

   function Extract_Value_To_Relationship
     (GT    : GL_Type;
      Arg   : GL_Value;
      Index : unsigned;
      R     : GL_Relationship;
      Name  : String := "") return GL_Value
   is
     (G (Extract_Value (IR_Builder, LLVM_Value (Arg), Index, Name),
         GT, R))
     with  Pre  => Present (Arg) and then Present (GT),
           Post => Present (Extract_Value_To_Relationship'Result);

   function Insert_Value
     (Arg, Elt : GL_Value;
      Index    : unsigned;
      Name     : String := "") return GL_Value
   is
     (G_From (Insert_Value (IR_Builder, LLVM_Value (Arg), LLVM_Value (Elt),
                            Index, Name),
              Arg))
     with  Pre  => Present (Arg) and then Present (Elt),
           Post => Present (Insert_Value'Result);

   function Extract_Value
     (GT      : GL_Type;
      Arg     : GL_Value;
      Idx_Arr : Index_Array;
      Name    : String := "") return GL_Value
   is
     (G (Build_Extract_Value (IR_Builder, LLVM_Value (Arg),
                              Idx_Arr'Address, Idx_Arr'Length, Name),
         GT))
     with  Pre  => Present (GT) and then Present (Arg),
           Post => Present (Extract_Value'Result);

   function Extract_Value_To_Ref
     (GT      : GL_Type;
      Arg     : GL_Value;
      Idx_Arr : Index_Array;
      Name    : String := "") return GL_Value
   is
     (G_Ref (Build_Extract_Value (IR_Builder, LLVM_Value (Arg),
                                  Idx_Arr'Address, Idx_Arr'Length, Name), GT))
     with  Pre  => Present (GT) and then Present (Arg),
           Post => Present (Extract_Value_To_Ref'Result);

   function Extract_Value_To_Relationship
     (GT      : GL_Type;
      Arg     : GL_Value;
      Idx_Arr : Index_Array;
      R       : GL_Relationship;
      Name    : String := "") return GL_Value
   is
     (G (Build_Extract_Value (IR_Builder, LLVM_Value (Arg),
                              Idx_Arr'Address, Idx_Arr'Length, Name),
         GT, R))
     with  Pre  => Present (GT) and then Present (Arg),
           Post => Present (Extract_Value_To_Relationship'Result);

   function Insert_Value
     (Arg, Elt : GL_Value;
      Idx_Arr  : Index_Array;
      Name     : String := "") return GL_Value
   is
     (G_From (Build_Insert_Value (IR_Builder, LLVM_Value (Arg),
                                  LLVM_Value (Elt),
                                  Idx_Arr'Address, Idx_Arr'Length, Name),
              Arg))
     with  Pre  => Present (Arg) and then Present (Elt),
           Post => Present (Insert_Value'Result);

   function GEP_To_Relationship
     (GT      : GL_Type;
      R       : GL_Relationship;
      Ptr     : GL_Value;
      Indices : GL_Value_Array;
      Name    : String := "") return GL_Value
     with Pre  => Is_Pointer (Ptr) and then Present (GT),
          Post => Is_Pointer (GEP_To_Relationship'Result);

   function GEP_Idx_To_Relationship
     (GT      : GL_Type;
      R       : GL_Relationship;
      Ptr     : GL_Value;
      Indices : Index_Array;
      Name    : String := "") return GL_Value
     with Pre  => Is_Pointer (Ptr) and then Present (GT),
          Post => Is_Pointer (GEP_Idx_To_Relationship'Result), Inline;

   function GEP
     (GT      : GL_Type;
      Ptr     : GL_Value;
      Indices : GL_Value_Array;
      Name    : String := "") return GL_Value is
     (GEP_To_Relationship (GT, Reference, Ptr, Indices, Name))
     with Pre  => Is_Pointer (Ptr) and then Present (GT),
          Post => Is_Pointer (GEP'Result), Inline;

   function GEP_Idx
     (GT      : GL_Type;
      Ptr     : GL_Value;
      Indices : Index_Array;
      Name    : String := "") return GL_Value is
     (GEP_Idx_To_Relationship (GT, Reference, Ptr, Indices, Name))
     with Pre  => Is_Pointer (Ptr) and then Present (GT),
          Post => Is_Pointer (GEP_Idx'Result), Inline;

   function Call
     (Func : GL_Value;
      GT   : GL_Type;
      Args : GL_Value_Array;
      Name : String := "") return GL_Value
     with Pre  => Present (Func) and then Present (GT),
          Post => Present (Call'Result), Inline;

   function Call_Ref
     (Func : GL_Value;
      GT   : GL_Type;
      Args : GL_Value_Array;
      Name : String := "") return GL_Value
     with Pre  => Present (Func) and then Present (GT),
          Post => Is_Reference (Call_Ref'Result), Inline;

   function Call_Relationship
     (Func : GL_Value;
      GT   : GL_Type;
      Args : GL_Value_Array;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
     with Pre  => Present (Func) and then Present (GT),
          Post => Present (Call_Relationship'Result), Inline;
     --  Used when an LLVM function is returning something of a specified
     --  relationship.

   procedure Call
     (Func : GL_Value; Args : GL_Value_Array; Name : String := "")
     with Pre => Present (Func), Inline;

   procedure Call_With_Align
     (Func : GL_Value; Args : GL_Value_Array; Align : Nat; Name : String := "")
     with Pre => Present (Func), Inline;

   procedure Call_With_Align_2
     (Func             : GL_Value;
      Args             : GL_Value_Array;
      Align_1, Align_2 : Nat;
      Name             : String := "")
     with Pre => Present (Func), Inline;

   function Landing_Pad
     (T                : Type_T;
      Personality_Func : GL_Value;
      Num_Clauses      : Nat    := 5;
      Name             : String := "") return GL_Value
   is
      (G (Landing_Pad (IR_Builder, T, LLVM_Value (Personality_Func),
                       unsigned (Num_Clauses), Name),
         A_Char_GL_Type, Unknown))
     with Pre  => Present (T) and then Present (Personality_Func),
          Post => Present (Landing_Pad'Result);

   procedure Build_Resume (V : GL_Value)
     with Pre => Present (V), Inline;

   function Inline_Asm
     (Args           : GL_Value_Array;
      Output_Value   : Entity_Id;
      Template       : String;
      Constraints    : String;
      Is_Volatile    : Boolean := False;
      Is_Stack_Align : Boolean := False) return GL_Value;

   function Build_Switch
     (V : GL_Value; Default : Basic_Block_T; Blocks : Nat := 15) return Value_T
   is
     (Build_Switch (IR_Builder, LLVM_Value (V), Default, unsigned (Blocks)))
     with Pre  => Present (V) and then Present (Default),
          Post => Present (Build_Switch'Result);

end GNATLLVM.Instructions;
