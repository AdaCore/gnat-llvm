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

with GNATLLVM.Aliasing; use GNATLLVM.Aliasing;
with GNATLLVM.GLValue;  use GNATLLVM.GLValue;
with GNATLLVM.Wrapper;  use GNATLLVM.Wrapper;

package GNATLLVM.Instructions is

   --  A type used to save a position at some earlier point in emitting
   --  code so that we can go back to it to emit more code.  Instr can
   --  be empty, in which case we mean to insert at the beginning of
   --  the basic block.

   type Position_T is record
      BB    : Basic_Block_T;
      Instr : Value_T;
   end record;

   No_Position_T : Position_T := (No_BB_T, No_Value_T);

   function No      (P : Position_T) return Boolean is (P =  No_Position_T);
   function Present (P : Position_T) return Boolean is (P /= No_Position_T);

   function  Get_Current_Position return Position_T;
   procedure Set_Current_Position (P : Position_T);
   --  Save and restore the current position within a basic block. Note that
   --  this is meant to only insert small sequences of instructions, which
   --  cannot include a terminator.

   function Get_Next_Instruction_After (P : Position_T) return Value_T is
     ((if   Present (P.Instr) then Get_Next_Instruction (P.Instr)
       else Get_Last_Instruction (P.BB)));
   --  Get the next instruction following P

   function Is_Equivalent_Position (P1, P2 : Position_T) return Boolean;
   --  Return if P1 and P2 are equivalent positions, meaning they're either
   --  the same position or one is an unconditional branch instruction to
   --  the other.

   type Pred_Mapping is record
      Signed   : Int_Predicate_T;
      Unsigned : Int_Predicate_T;
      Real     : Real_Predicate_T;
   end record;

   function Get_Preds (Kind : Node_Kind) return Pred_Mapping is
     (case Kind is
        when N_Op_Eq => (Int_EQ, Int_EQ, Real_OEQ),
        when N_Op_Ne => (Int_NE, Int_NE, Real_UNE),
        when N_Op_Lt => (Int_SLT, Int_ULT, Real_OLT),
        when N_Op_Le => (Int_SLE, Int_ULE, Real_OLE),
        when N_Op_Gt => (Int_SGT, Int_UGT, Real_OGT),
        when N_Op_Ge => (Int_SGE, Int_UGE, Real_OGE),
        when others => (others => <>));

   function Are_In_Dead_Code return Boolean
     with Inline;
   --  True if we're in dead code (the last instruction is a terminator)

   procedure Position_Builder_At_End (BB : Basic_Block_T)
     with Pre => Present (BB), Inline;

   function Alloca
     (GT    : GL_Type;
      E     : Entity_Id := Empty;
      Align : Nat       := 0;
      Name  : String    := "") return GL_Value
     with Pre  => Present (GT), Post => Is_Reference (Alloca'Result),
          Inline;

   function Array_Alloca
     (GT       : GL_Type;
      Num_Elts : GL_Value;
      E        : Opt_Object_Kind_Id := Empty;
      Align    : Nat                := 0;
      Name     : String             := "") return GL_Value
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

   function Ptr_To_Address_Type
     (V : GL_Value; Name : String := "") return GL_Value
   is
     (Ptr_To_Int (V, Address_GL_Type, Name))
     with Pre  => Is_Pointer (V),
          Post => Is_Discrete_Or_Fixed_Point_Type (Ptr_To_Address_Type'Result);

   function Bit_Cast
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Present (V) and then not Is_Pointer (V)
                  and then Present (GT),
          Post => Present (Bit_Cast'Result), Inline;

   function Bit_Cast (V, T : GL_Value; Name : String := "") return GL_Value is
     (G_From (Bit_Cast (IR_Builder, +V, Type_Of (T), Name), T))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Bit_Cast'Result);

   function Bit_Cast
     (V : GL_Value; T : Type_T; Name : String := "") return GL_Value
   is
     (G (Bit_Cast (IR_Builder, +V, T, Name), Related_Type (V),
         Unknown, Alignment => Alignment (V)))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Bit_Cast'Result);

   function Bit_Cast_To_Relationship
     (V    : GL_Value;
      T    : Type_T;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
     (Initialize_TBAA
        (G (Bit_Cast (IR_Builder, +V, T, Name), Related_Type (V),
            R, Alignment => Alignment (V))))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Bit_Cast_To_Relationship'Result);

   function Pointer_Cast
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Pointer (V) and then Present (GT),
          Post => Is_Pointer (Pointer_Cast'Result), Inline;

   function Pointer_Cast
     (V, T : GL_Value; Name : String := "") return GL_Value
   is
     (Initialize_TBAA
        (G_From (Pointer_Cast (IR_Builder, +V, Type_Of (T), Name), T)))
     with Pre  => Is_Pointer (V) and then Is_Pointer (T),
          Post => Is_Pointer (Pointer_Cast'Result);

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
      Name : String := "") return GL_Value
   is
     (Initialize_TBAA
        (GM (Pointer_Cast (IR_Builder, +V, T, Name), Related_Type (V), R, V)))
     with Pre  => Is_Pointer (V) and then Present (T),
          Post => Is_Pointer (Ptr_To_Relationship'Result), Inline;

   function Ptr_To_Relationship
     (V    : GL_Value;
      T    : Type_T;
      GT   : GL_Type;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
     (Initialize_TBAA
        (GM (Pointer_Cast (IR_Builder, +V, T, Name), GT, R, V)))
     with Pre  => Is_Pointer (V) and then Present (T) and then Present (GT),
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
          Post => Is_Discrete_Or_Fixed_Point_Type (Trunc'Result), Inline;

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
     (G (Z_Ext (IR_Builder, +V, T, Name), Related_Type (V), R,
         Alignment => Alignment (V)))
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
     (Initialize_TBAA
        (G_From (Int_To_Ptr (IR_Builder, +V, Type_Of (T), Name), T)))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V)
                  and then Is_Pointer (T),
          Post => Is_Pointer (Int_To_Ptr'Result);

   function Ptr_To_Int
     (V, T : GL_Value; Name : String := "") return GL_Value
   is
     (Initialize_TBAA
        (G_From (Ptr_To_Int (IR_Builder, +V, Type_Of (T), Name), T)))
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

   procedure Store (Expr, Ptr : GL_Value)
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
     (G (I_Cmp (IR_Builder, Op, +LHS, +RHS, Name), Boolean_GL_Type,
         Boolean_Data))
     with Pre  => Present (LHS) and then Present (RHS),
          Post => Present (I_Cmp'Result);

   function F_Cmp
     (Op       : Real_Predicate_T;
      LHS, RHS : GL_Value;
      Name     : String := "") return GL_Value
   is
     (G (F_Cmp (IR_Builder, Op, +LHS, +RHS, Name), Boolean_GL_Type,
         Boolean_Data))
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

   function Div
     (LHS, RHS : GL_Value;
      Signed   : Boolean;
      Name     : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                  and then Is_Discrete_Or_Fixed_Point_Type (RHS),
          Post => Is_Discrete_Or_Fixed_Point_Type (Div'Result);

   function S_Div
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Div (LHS, RHS, True, Name))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (S_Div'Result);

   function U_Div
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Div (LHS, RHS, False, Name))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (U_Div'Result);

   function S_Rem
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Set_TBAA_Type
        (Set_Alignment
           (Mark_Overflowed
              (G_From (S_Rem (IR_Builder, +LHS, +RHS, Name), LHS),
               Overflowed (LHS) or else Overflowed (RHS)),
            Nat'Min (Alignment (LHS), Alignment (RHS))),
         No_Metadata_T))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (S_Rem'Result);

   function U_Rem
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Set_TBAA_Type
        (Set_Alignment
           (Mark_Overflowed
              (G_From (U_Rem (IR_Builder, +LHS, +RHS, Name), LHS),
               Overflowed (LHS) or else Overflowed (RHS)),
            Nat'Min (Alignment (LHS), Alignment (RHS))),
         No_Metadata_T))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (U_Rem'Result);

   function Build_And
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Set_Alignment
        (Mark_Overflowed
           (G_From (Build_And (IR_Builder, +LHS, +RHS, Name), LHS),
            Overflowed (LHS) or else Overflowed (RHS)),
         Nat'Max (Alignment (LHS), Alignment (RHS))))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (Build_And'Result);

   function Build_Or
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     ((if   Is_Const_Int_Value (RHS, 0) then LHS
       else Set_TBAA_Type
             (Set_Alignment
                (Mark_Overflowed
                   (G_From (Build_Or (IR_Builder, +LHS, +RHS, Name), LHS),
                    Overflowed (LHS) or else Overflowed (RHS)),
                 Nat'Min (Alignment (LHS), Alignment (RHS))),
              No_Metadata_T)))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (Build_Or'Result);

   function Build_Xor
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     ((if   Is_Const_Int_Value (RHS, 0) then LHS
       else Set_TBAA_Type
              (Set_Alignment
                 (Mark_Overflowed
                    (G_From (Build_Xor (IR_Builder, +LHS, +RHS, Name), LHS),
                     Overflowed (LHS) or else Overflowed (RHS)),
                  Nat'Min (Alignment (LHS), Alignment (RHS))),
               No_Metadata_T)))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (Build_Xor'Result);

   function F_Add
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (F_Add (IR_Builder, +LHS, +RHS, Name), LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Is_Floating_Point_Type (LHS)
                   and then Is_Floating_Point_Type (RHS),
           Post => Is_Floating_Point_Type (F_Add'Result);

   function F_Sub
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (F_Sub (IR_Builder, +LHS, +RHS, Name), LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Is_Floating_Point_Type (LHS)
                   and then Is_Floating_Point_Type (RHS),
           Post => Is_Floating_Point_Type (F_Sub'Result);

   function F_Mul
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (F_Mul (IR_Builder, +LHS, +RHS, Name), LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Is_Floating_Point_Type (LHS)
                   and then Is_Floating_Point_Type (RHS),
           Post => Is_Floating_Point_Type (F_Mul'Result);

   function F_Div
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (Mark_Overflowed
        (G_From (F_Div (IR_Builder, +LHS, +RHS, Name), LHS),
         Overflowed (LHS) or else Overflowed (RHS)))
      with Pre  => Is_Floating_Point_Type (LHS)
                   and then Is_Floating_Point_Type (RHS),
           Post => Is_Floating_Point_Type (F_Div'Result);

   function Shift
     (V              : GL_Value;
      Count          : GL_Value;
      Left           : Boolean;
      Arithmetic     : Boolean;
      Allow_Overflow : Boolean;
      Name           : String := "") return GL_Value
     with Pre  => Present (V) and then Present (Count),
          Post => Present (Shift'Result);

   function Shl
     (V              : GL_Value;
      Count          : GL_Value;
      Name           : String  := "";
      Allow_Overflow : Boolean := False) return GL_Value
   is
     (Shift (V, Count, Left => True, Arithmetic => False,
             Allow_Overflow => Allow_Overflow, Name => Name))
      with Pre  => Present (V) and then Present (Count),
           Post => Present (Shl'Result);

   function L_Shr
     (V, Count : GL_Value; Name : String := "") return GL_Value
   is
     (Shift (V, Count, Left => False, Arithmetic => False,
             Allow_Overflow => False, Name => Name))
      with Pre  => Present (V) and then Present (Count),
           Post => Present (L_Shr'Result);

   function A_Shr
     (V, Count : GL_Value; Name : String := "") return GL_Value
   is
     (Shift (V, Count, Left => False, Arithmetic => True,
             Allow_Overflow => False, Name => Name))
      with Pre  => Present (V) and then Present (Count),
           Post => Present (A_Shr'Result);

   function Build_Not
     (V : GL_Value; Name : String := "") return GL_Value
   is
     (Clear_Alignment
        (G_From (Build_Not (IR_Builder, +V, Name), V)))
      with Pre  => Present (V),
           Post => Present (Build_Not'Result);

   function Neg
     (V : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Set_Arith_Attrs (Neg (IR_Builder, +V, Name), V), V))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (V),
           Post => Is_Discrete_Or_Fixed_Point_Type (Neg'Result);

   function F_Neg
     (V : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (F_Neg (IR_Builder, +V, Name), V))
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
     with Pre  => Present (LHS) and then Is_A_Constant_Int (RHS);
   function "+" (LHS : GL_Value; RHS : Uint) return Uint is
     (RHS + UI_From_GL_Value (LHS))
     with Pre  => Present (RHS) and then Is_A_Constant_Int (LHS);

   function "-" (LHS : Uint; RHS : GL_Value) return Uint is
     (LHS - UI_From_GL_Value (RHS))
     with Pre  => Present (LHS) and then Is_A_Constant_Int (RHS);

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
     (LHS + Const_Int (LHS, +RHS));
   function "-" (LHS : GL_Value; RHS : Int)   return GL_Value is
     (LHS - Const_Int (LHS, +RHS));
   function "*" (LHS : GL_Value; RHS : Int)   return GL_Value is
     (LHS * Const_Int (LHS, +RHS));
   function "/" (LHS : GL_Value; RHS : Int)   return GL_Value is
     (LHS / Const_Int (LHS, +RHS));
   function "mod" (LHS : GL_Value; RHS : Int) return GL_Value is
     (LHS mod Const_Int (LHS, +RHS));

   function To_Bytes (V : GL_Value) return GL_Value is
     (Set_Alignment ((V + (BPU - 1)) / BPU, Alignment (V) / BPU))
     with Pre => Present (V), Post => Present (To_Bytes'Result);

   function Build_Select
     (C_If, C_Then, C_Else : GL_Value; Name : String := "") return GL_Value
     with Pre  => Ekind (Full_Etype (C_If)) in Enumeration_Kind
                  and then Is_Elementary_Type (C_Then)
                  and then Is_Elementary_Type (C_Else),
          Post => Is_Elementary_Type (Build_Select'Result);

   procedure Add_Clause (V, Exc : GL_Value)
     with Pre => Present (V) and then Present (Exc), Inline;

   procedure Set_Cleanup (V : GL_Value)
     with Pre => Present (V), Inline;

   procedure Build_Cond_Br
     (C_If           : GL_Value;
      C_Then, C_Else : Basic_Block_T;
      Optimize       : Boolean := True)
     with Pre => Ekind (Full_Etype (C_If)) in Enumeration_Kind
                 and then Present (C_Then) and then Present (C_Else),
          Inline;

   procedure Build_Br (BB : Basic_Block_T)
     with Pre => Present (BB), Inline;

   procedure Maybe_Build_Br (BB : Basic_Block_T)
     with Inline;
   --  Like Build_Br, but do nothing if No (BB) or if we're in dead code

   procedure Move_To_BB (BB : Basic_Block_T)
     with Inline;
   --  If BB is Present, generate a branch to it and position there

   procedure Build_Ret (V : GL_Value)
     with Pre => Present (V), Inline;

   procedure Build_Ret_Void
     with Inline;

   procedure Build_Unreachable
     with Inline;

   procedure Maybe_Build_Unreachable
     with Inline;

   function Build_Phi
     (GL_Values : GL_Value_Array;
      BBs       : Basic_Block_Array;
      Name      : String := "") return GL_Value
     with Pre  => GL_Values'First = BBs'First
                  and then GL_Values'Last = BBs'Last
                  and then (for all V of GL_Values => Present (V)),
          Post => Present (Build_Phi'Result);

   function Int_To_Ref
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
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
      with Pre  => Is_Pointer (Ptr) and then Present (V),
           Post => Present (Atomic_RMW'Result);

   procedure Fence
     (Order         : Atomic_Ordering_T :=
       Atomic_Ordering_Sequentially_Consistent;
      Single_Thread : Boolean           := False;
      Name          : String            := "");

   function Atomic_Cmp_Xchg
     (Ptr              : GL_Value;
      Cmp              : GL_Value;
      C_New            : GL_Value;
      Success_Ordering : Atomic_Ordering_T :=
        Atomic_Ordering_Sequentially_Consistent;
      Failure_Ordering : Atomic_Ordering_T :=
        Atomic_Ordering_Sequentially_Consistent;
      Single_Thread    : Boolean           := False;
      Weak             : Boolean           := False) return GL_Value
     with Pre  => Is_Pointer (Ptr) and then Present (Cmp)
                  and then Present (C_New),
           Post => Present (Atomic_Cmp_Xchg'Result);

   function Extract_Value
     (GT    : GL_Type;
      Arg   : GL_Value;
      Index : unsigned;
      Name  : String := "") return GL_Value
   is
      (Initialize_Alignment
         (G (Extract_Value (IR_Builder, +Arg, Index, Name), GT,
             SM_Object => SM_Object (Arg))))
     with  Pre  => Present (Arg) and then Present (GT),
           Post => Present (Extract_Value'Result);

   function Extract_Value_To_Ref
     (GT    : GL_Type;
      Arg   : GL_Value;
      Index : unsigned;
      Name  : String := "") return GL_Value
   is
     (Initialize_Alignment
        (G_Ref (Extract_Value (IR_Builder, +Arg, Index, Name), GT,
                SM_Object => SM_Object (Arg))))
     with  Pre  => Present (Arg) and then Present (GT),
           Post => Is_Pointer (Extract_Value_To_Ref'Result);

   function Extract_Value_To_Relationship
     (GT    : GL_Type;
      Arg   : GL_Value;
      Index : unsigned;
      R     : GL_Relationship;
      Name  : String := "") return GL_Value
   is
     (Initialize_TBAA
        (Initialize_Alignment
           (G (Extract_Value (IR_Builder, +Arg, Index, Name), GT, R,
               SM_Object => SM_Object (Arg)))))
     with  Pre  => Present (Arg) and then Present (GT),
           Post => Present (Extract_Value_To_Relationship'Result);

   function Insert_Value
     (Arg, Elt : GL_Value;
      Index    : unsigned;
      Name     : String := "") return GL_Value
   is
     (Clear_Alignment
        (G_From (Insert_Value (IR_Builder, +Arg, +Elt, Index, Name), Arg)))
     with  Pre  => Present (Arg) and then Present (Elt),
           Post => Present (Insert_Value'Result);

   function Extract_Value
     (GT      : GL_Type;
      Arg     : GL_Value;
      Idx_Arr : Index_Array;
      Name    : String := "") return GL_Value
   is
     (Initialize_Alignment
        (G (Build_Extract_Value (IR_Builder, +Arg, Idx_Arr'Address,
                                 Idx_Arr'Length, Name),
            GT)))
     with  Pre  => Present (GT) and then Present (Arg),
           Post => Present (Extract_Value'Result);

   function Extract_Value_To_Ref
     (GT      : GL_Type;
      Arg     : GL_Value;
      Idx_Arr : Index_Array;
      Name    : String := "") return GL_Value
   is
     (Initialize_Alignment
        (G_Ref (Build_Extract_Value (IR_Builder, +Arg, Idx_Arr'Address,
                                     Idx_Arr'Length, Name),
                GT, SM_Object => SM_Object (Arg))))
     with  Pre  => Present (GT) and then Present (Arg),
           Post => Present (Extract_Value_To_Ref'Result);

   function Extract_Value_To_Relationship
     (GT      : GL_Type;
      Arg     : GL_Value;
      Idx_Arr : Index_Array;
      R       : GL_Relationship;
      Name    : String := "") return GL_Value
   is
     (Initialize_TBAA
        (Initialize_Alignment
           (G (Build_Extract_Value (IR_Builder, +Arg, Idx_Arr'Address,
                                    Idx_Arr'Length, Name),
               GT, R, SM_Object => SM_Object (Arg)))))
     with  Pre  => Present (GT) and then Present (Arg),
           Post => Present (Extract_Value_To_Relationship'Result);

   function Insert_Value
     (Arg, Elt : GL_Value;
      Idx_Arr  : Index_Array;
      Name     : String := "") return GL_Value
   is
     (G_From (Build_Insert_Value (IR_Builder, +Arg, +Elt, Idx_Arr'Address,
                                  Idx_Arr'Length, Name),
              Arg))
     with  Pre  => Present (Arg) and then Present (Elt),
           Post => Present (Insert_Value'Result);

   procedure Build_MemCpy
     (Dst         : GL_Value;
      Dst_Align   : ULL;
      Src         : GL_Value;
      Src_Align   : ULL;
      Size        : GL_Value;
      Is_Volatile : Boolean;
      TBAA        : Metadata_T := No_Metadata_T;
      TBAA_Struct : Metadata_T := No_Metadata_T;
      Scope       : Metadata_T := No_Metadata_T;
      NoAlias     : Metadata_T := No_Metadata_T)
     with Pre => Present (Dst) and then Present (Src) and then Present (Size);

   procedure Build_MemMove
     (Dst         : GL_Value;
      Dst_Align   : ULL;
      Src         : GL_Value;
      Src_Align   : ULL;
      Size        : GL_Value;
      Is_Volatile : Boolean;
      TBAA        : Metadata_T := No_Metadata_T;
      Scope       : Metadata_T := No_Metadata_T;
      NoAlias     : Metadata_T := No_Metadata_T)
     with Pre => Present (Dst) and then Present (Src) and then Present (Size);

   procedure Build_MemSet
     (Ptr         : GL_Value;
      Val         : GL_Value;
      Size        : GL_Value;
      Align       : ULL;
      Is_Volatile : Boolean;
      TBAA        : Metadata_T := No_Metadata_T;
      Scope       : Metadata_T := No_Metadata_T;
      NoAlias     : Metadata_T := No_Metadata_T)
     with Pre => Present (Ptr) and then Present (Val) and then Present (Size);

   procedure Create_Lifetime_Start (Ptr, Size : GL_Value)
     with Pre => Present (Ptr) and then Present (Size);
   procedure Create_Lifetime_End (Ptr, Size : GL_Value)
     with Pre => Present (Ptr) and then Present (Size);
   procedure Create_Invariant_Start
     (Ptr : GL_Value; Size : GL_Value := No_GL_Value)
     with Pre => Present (Ptr);

   function GEP
     (Bld     : Builder_T;
      T       : Type_T;
      Ptr     : Value_T;
      Indices : Value_Array;
      Name    : String := "") return Value_T
     with Pre  => Present (Bld) and then Present (Ptr)
                  and then (for all V of Indices => Present (V)),
          Post => Present (GEP'Result);

   function GEP_To_Relationship
     (GT      : GL_Type;
      R       : GL_Relationship;
      Ptr     : GL_Value;
      Indices : GL_Value_Array;
      Name    : String := "") return GL_Value
     with Pre  => Is_Pointer (Ptr) and then Present (GT)
                  and then (for all V of Indices => Present (V)),
          Post => Is_Pointer (GEP_To_Relationship'Result);

   function GEP_To_Relationship
     (GT      : GL_Type;
      R       : GL_Relationship;
      Ptr     : GL_Value;
      Ptr_T   : Type_T;
      Indices : GL_Value_Array;
      Name    : String := "") return GL_Value
     with Pre  => Is_Pointer (Ptr) and then Present (GT)
                  and then Present (Ptr_T)
                  and then (for all V of Indices => Present (V)),
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
      Name    : String := "") return GL_Value
   is
     (GEP_To_Relationship (GT, Reference, Ptr, Indices, Name))
     with Pre  => Is_Pointer (Ptr) and then Present (GT)
                  and then (for all V of Indices => Present (V)),
          Post => Is_Pointer (GEP'Result), Inline;

   function GEP_Idx
     (GT      : GL_Type;
      Ptr     : GL_Value;
      Indices : Index_Array;
      Name    : String := "") return GL_Value
   is
     (GEP_Idx_To_Relationship (GT, Reference, Ptr, Indices, Name))
     with Pre  => Is_Pointer (Ptr) and then Present (GT),
          Post => Is_Pointer (GEP_Idx'Result), Inline;

   function Null_Derived_Ptr
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (GEP (IR_Builder, Byte_T, Const_Null (Void_Ptr_T), (1 => +V), Name),
         GT))
   with Pre  => Is_Discrete_Type (V),
        Post => Present (Null_Derived_Ptr'Result);

   function Call
     (Func : GL_Value;
      Args : GL_Value_Array;
      Name : String := "") return GL_Value
     with Pre  => Is_A_Function (Func)
                  and then (for all V of Args => Present (V)),
          Post => Present (Call'Result), Inline;

   function Call
     (Func : GL_Value;
      Fn_T : Type_T;
      Args : GL_Value_Array;
      Name : String := "") return GL_Value
     with Pre  => Present (Func)
                  and then (for all V of Args => Present (V)),
          Post => Present (Call'Result), Inline;

   function Call
     (Func : GL_Value;
      Fn_T : Type_T;
      Args : GL_Value_Array;
      GT   : GL_Type;
      Name : String := "") return GL_Value
     with Pre  => Present (Func) and then Present (GT)
                  and then (for all V of Args => Present (V)),
          Post => Present (Call'Result), Inline;

   function Call_Ref
     (Func : GL_Value;
      Fn_T : Type_T;
      Args : GL_Value_Array;
      Name : String := "") return GL_Value
     with Pre  => Present (Func)
                  and then (for all V of Args => Present (V)),
          Post => Is_Reference (Call_Ref'Result), Inline;

   function Call_Relationship
     (Func : GL_Value;
      Args : GL_Value_Array;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
     with Pre  => Is_A_Function (Func)
                  and then (for all V of Args => Present (V)),
          Post => Present (Call_Relationship'Result), Inline;
   function Call_Relationship
     (Func : GL_Value;
      Fn_T : Type_T;
      Args : GL_Value_Array;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
     with Pre  => Present (Func)
                  and then (for all V of Args => Present (V)),
          Post => Present (Call_Relationship'Result), Inline;
     --  Used when an LLVM function is returning something of a specified
     --  relationship.

   procedure Call
     (Func : GL_Value; Args : GL_Value_Array; Name : String := "")
     with Pre => Is_A_Function (Func)
                  and then (for all V of Args => Present (V)),
          Inline;

   procedure Call
     (Func : GL_Value;
      Fn_T : Type_T;
      Args : GL_Value_Array;
      Name : String := "")
     with Pre => Present (Func)
                  and then (for all V of Args => Present (V)),
          Inline;

   function Get_Pointer_Address (Ptr : GL_Value) return GL_Value
     with Pre  => Is_Address (Ptr) or else Is_Pointer (Ptr),
          Post => Present (Get_Pointer_Address'Result);

   function Set_Pointer_Address (Ptr, Addr : GL_Value) return GL_Value
     with Pre  => (Is_Address (Ptr) or else Is_Pointer (Ptr))
                  and then Is_Integer_Type (Addr),
          Post => Present (Set_Pointer_Address'Result);

   function Address_Add (Ptr, Offset : GL_Value) return GL_Value
   is (if   Tagged_Pointers
       then Set_Pointer_Address (Ptr, Get_Pointer_Address (Ptr) + Offset)
       else Ptr + Offset)
     with Pre  => Is_Address (Ptr) and then Is_Integer_Type (Offset),
          Post => Is_Address (Address_Add'Result);

   function Address_Sub (Ptr, Offset : GL_Value) return GL_Value is
     (Address_Add (Ptr, -Offset));

   function Landing_Pad
     (T                : Type_T;
      Personality_Func : GL_Value;
      Num_Clauses      : Nat    := 5;
      Name             : String := "") return GL_Value
   is
      (G (Landing_Pad (IR_Builder, T, +Personality_Func,
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
      Fn_T           : out Type_T;
      Is_Volatile    : Boolean := False;
      Is_Stack_Align : Boolean := False) return GL_Value
     with Pre  => (for all V of Args => Present (V)),
          Post => Present (Inline_Asm'Result)
                  and then Get_Type_Kind (Fn_T) = Function_Type_Kind;

   function Build_Switch
     (V : GL_Value; Default : Basic_Block_T; Blocks : Nat := 15) return Value_T
   is
     (Build_Switch (IR_Builder, +V, Default, unsigned (Blocks)))
     with Pre  => Present (V) and then Present (Default),
          Post => Present (Build_Switch'Result);

   function Build_Indirect_Br
     (V : GL_Value; Dests : Nat := 2) return Value_T
   is
     (Build_Indirect_Br (IR_Builder, +V, unsigned (Dests)))
     with Pre => Present (V), Post => Present (Build_Indirect_Br'Result);

end GNATLLVM.Instructions;
