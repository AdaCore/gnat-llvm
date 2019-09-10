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

with Ada.Unchecked_Conversion;

with GNATLLVM.Blocks;      use GNATLLVM.Blocks;
with GNATLLVM.GLType;      use GNATLLVM.GLType;
with GNATLLVM.Subprograms; use GNATLLVM.Subprograms;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Utils;       use GNATLLVM.Utils;
with GNATLLVM.Variables;   use GNATLLVM.Variables;

package body GNATLLVM.Instructions is

   function Call_Internal
     (Func        : GL_Value;
      Args        : GL_Value_Array;
      Name        : String := "") return Value_T;
   --  Internal version of Call and Call_Ref

   function Trunc_Overflowed (V, Result : GL_Value) return Boolean
     with Pre => Present (V) and then Present (Result);
   --  Return True if the truncation operation from V to Result overflowed

   --------------------------
   -- Get_Current_Position --
   --------------------------

   function Get_Current_Position return Position_T is
      BB : constant Basic_Block_T := Get_Insert_Block;

   begin
      return (BB, Get_Last_Instruction (BB));
   end Get_Current_Position;

   --------------------------
   -- Set_Current_Position --
   --------------------------

   procedure Set_Current_Position (P : Position_T) is
      BB   : constant Basic_Block_T := P.BB;
      Next : Value_T;

   begin
      --  There are two tricky parts here.  First is that if there's no
      --  instruction, LLVM will treat this as a request to insert at the
      --  end of a basic block, but we mean the beginning.  So we need to
      --  get the first instruction in the block and set the insertion
      --  point in front of it.  Secondly, if we have an instruction, the
      --  builder operation inserts in front of it, but we want to insert
      --  after.  The way to do that is to get the next instruction and
      --  insert before that, but there may not be another instruction.
      --  If so, then insert at the end of the block.

      if Present (P.Instr) then
         Next := Get_Next_Instruction (P.Instr);
         if Present (Next) then
            Position_Builder (IR_Builder, BB, Next);
         else
            Position_Builder_At_End (IR_Builder, BB);
         end if;
      else
         Position_Builder (IR_Builder, BB, Get_First_Instruction (BB));
      end if;
   end Set_Current_Position;

   ----------------------
   -- Are_In_Dead_Code --
   ----------------------

   function Are_In_Dead_Code return Boolean is
      Last_Inst : constant Value_T := Get_Last_Instruction (Get_Insert_Block);
   begin
      --  We're in dead code if there is an instruction in this basic block
      --  and the last is a terminator.

      return Present (Last_Inst)
        and then Present (Is_A_Terminator_Inst (Last_Inst));
   end Are_In_Dead_Code;

   -----------------------------
   -- Position_Builder_At_End --
   -----------------------------

   procedure Position_Builder_At_End (BB : Basic_Block_T) is
   begin
      Position_Builder_At_End (IR_Builder, BB);
   end Position_Builder_At_End;

   ------------
   -- Alloca --
   ------------

   function Alloca
     (GT        : GL_Type;
      Def_Ident : Entity_Id := Empty;
      Name      : String    := "") return GL_Value
   is
      R       : constant GL_Relationship := Relationship_For_Alloc (GT);
      PT      : constant Type_T          := Type_For_Relationship (GT, R);
      T       : constant Type_T          := Get_Element_Type (PT);
      Promote : constant Basic_Block_T   := Maybe_Promote_Alloca (T);
      Inst    : constant Value_T         :=
        Alloca (IR_Builder, T, Get_Alloca_Name (Def_Ident, Name));

   begin
      Set_Object_Align (Inst, GT, Def_Ident);
      Done_Promoting_Alloca (Inst, Promote, T);
      return G (Inst, GT, R, Is_Pristine => True);
   end Alloca;

   ------------------
   -- Array_Alloca --
   ------------------

   function Array_Alloca
     (GT        : GL_Type;
      Num_Elts  : GL_Value;
      Def_Ident : Entity_Id := Empty;
      Name      : String    := "") return GL_Value
   is
      T       : constant Type_T        := Type_Of (GT);
      Promote : constant Basic_Block_T := Maybe_Promote_Alloca (T, Num_Elts);
      Inst    : constant Value_T       :=
        Array_Alloca (IR_Builder, Type_Of (GT), LLVM_Value (Num_Elts),
                      Get_Alloca_Name (Def_Ident, Name));

   begin
      Set_Object_Align (Inst, GT, Def_Ident);
      Done_Promoting_Alloca (Inst, Promote, T, Num_Elts);
      return G_Ref (Inst, GT, Is_Pristine => True);
   end Array_Alloca;

   ----------------
   -- Int_To_Ptr --
   ----------------

   function Int_To_Ptr
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
  is
     (GM (Int_To_Ptr (IR_Builder, LLVM_Value (V), Type_Of (GT), Name), GT,
          GV => V));

   ----------------
   -- Ptr_To_Int --
   ----------------

   function Ptr_To_Int
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (GM (Ptr_To_Int (IR_Builder, LLVM_Value (V), Type_Of (GT), Name), GT,
          GV => V));

   ----------------
   -- Int_To_Ref --
   ----------------

   function Int_To_Ref
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
      (GM_Ref (Int_To_Ptr (IR_Builder, LLVM_Value (V),
                           Pointer_Type (Type_Of (GT), 0), Name),
               GT, V));

   -------------------------
   -- Int_To_Relationship --
   -------------------------

   function Int_To_Relationship
     (V    : GL_Value;
      GT   : GL_Type;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
     (GM (Int_To_Ptr (IR_Builder, LLVM_Value (V),
                      Type_For_Relationship (GT, R), Name),
         GT, R, GV => V));

   --------------
   -- Bit_Cast --
   --------------

   function Bit_Cast
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (GM (Bit_Cast (IR_Builder, LLVM_Value (V), Type_Of (GT), Name), GT,
          GV => V));

   ------------------
   -- Pointer_Cast --
   ------------------

   function Pointer_Cast
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (GM (Pointer_Cast (IR_Builder, LLVM_Value (V), Type_Of (GT), Name), GT,
          GV => V));

   ----------------
   -- Ptr_To_Ref --
   ----------------

   function Ptr_To_Ref
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (GM_Ref (Pointer_Cast (IR_Builder, LLVM_Value (V),
                            Pointer_Type (Type_Of (GT), 0), Name),
              GT, V));

   ----------------
   -- Ptr_To_Ref --
   ----------------

   function Ptr_To_Ref (V, T : GL_Value; Name : String := "") return GL_Value
   is
     (GM_Ref (Pointer_Cast (IR_Builder, LLVM_Value (V),
                            Pointer_Type (Type_Of (T), 0), Name),
              Full_Designated_GL_Type (T), V));

   -------------------------
   -- Ptr_To_Relationship --
   -------------------------

   function Ptr_To_Relationship
     (V    : GL_Value;
      GT   : GL_Type;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
     (GM (Pointer_Cast (IR_Builder, LLVM_Value (V),
                        Type_For_Relationship (GT, R), Name),
          GT, R, V));

   -------------------------
   -- Ptr_To_Relationship --
   -------------------------

   function Ptr_To_Relationship
     (V, T : GL_Value;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
      (GM (Pointer_Cast (IR_Builder, LLVM_Value (V),
                         Type_For_Relationship (Related_Type (T), R), Name),
           Related_Type (T), R, V));

   -------------
   -- Add_Sub --
   -------------

   function Add_Sub
     (LHS, RHS : GL_Value; Is_Add : Boolean; Name : String) return GL_Value
   is
      V      : Value_T;
      Result : GL_Value;

   begin
      --  First, do some simple constant folding

      if Is_Const_Int_Value (RHS, 0) then
         return Mark_Overflowed (LHS, Overflowed (RHS));
      elsif Is_Const_Int_Value (LHS, 0) then
         return Mark_Overflowed ((if Is_Add then RHS else Neg (RHS, Name)),
                                 Overflowed (LHS));
      end if;

      --  Otherwise, perform the operation and respect any overflow flags

      V := (if   Is_Add
            then Add (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name)
            else Sub (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name));
      Result := Mark_Overflowed (G_From (Set_Arith_Attrs (V, LHS), LHS),
                                 Overflowed (LHS) or else Overflowed (RHS));

      --  If either operand or the result isn't a constant integer, if this
      --  is a modular integer type, or if we already had an overflow, we
      --  don't have to test for overflow.

      if not Is_A_Const_Int (LHS) or else not Is_A_Const_Int (RHS)
        or else not Is_A_Const_Int (Result) or else Overflowed (Result)
        or else Is_Modular_Integer_Type (Result)
      then
         return Result;

      --  Otherwise, test for overflow.  Note that, unlike in C, LLVM
      --  defines the result if an overflow occurs (mod 2**N), so we can
      --  safely do post-operation testing that we couldn't do if this were
      --  the C addition operation.  The unsigned case is simple

      elsif Is_Unsigned_Type (Result) then
         declare
            LHS_I : constant ULL := Get_Const_Int_Value_ULL (LHS);
            RHS_I : constant ULL := Get_Const_Int_Value_ULL (RHS);
            Res_I : constant ULL := Get_Const_Int_Value_ULL (Result);

         begin
            return Mark_Overflowed
              (Result, (if Is_Add then Res_I < LHS_I else LHS_I < RHS_I));
         end;

      else
         --  For signed, we overflow if the two operands have the same (for
         --  addition) or different (for subtraction) sign and the sign of
         --  the result differs.

         declare
            LHS_Neg    : constant Boolean  := Get_Const_Int_Value (LHS) < 0;
            RHS_Neg    : constant Boolean  := Get_Const_Int_Value (RHS) < 0;
            Res_Neg    : constant Boolean  := Get_Const_Int_Value (Result) < 0;
            Maybe_Ovfl : constant Boolean  :=
              (if Is_Add then LHS_Neg = RHS_Neg else LHS_Neg = not RHS_Neg);

         begin
            return Mark_Overflowed (Result,
                                    Maybe_Ovfl and then LHS_Neg = not Res_Neg);

         end;
      end if;

   end Add_Sub;

   ---------
   -- Mul --
   ---------

   function Mul (LHS, RHS : GL_Value; Name : String := "") return GL_Value is
      Result : GL_Value;

   begin
      --  First, do some simple constant folding. Note that 0 * overflow is
      --  still zero.

      if Is_Const_Int_Value (RHS, 0) then
         return RHS;
      elsif Is_Const_Int_Value (LHS, 0) then
         return LHS;
      elsif Is_Const_Int_Value (RHS, 1) then
         return Mark_Overflowed (LHS, Overflowed (RHS));
      elsif Is_Const_Int_Value (LHS, 1) then
         return Mark_Overflowed (RHS, Overflowed (LHS));
      end if;

      --  Otherwise, perform the operation and respect any overflow flags

      Result := Mark_Overflowed
        (G_From
           (Set_Arith_Attrs (Mul (IR_Builder, LLVM_Value (LHS),
                                  LLVM_Value (RHS), Name),
                             LHS),
            LHS),
         Overflowed (LHS) or else Overflowed (RHS));

      --  If either operand or the result isn't a constant integer, if this
      --  is a modular integer type, or if we already had an overflow, we
      --  don't have to test for overflow.

      if not Is_A_Const_Int (LHS) or else not Is_A_Const_Int (RHS)
        or else not Is_A_Const_Int (Result) or else Overflowed (Result)
        or else Is_Modular_Integer_Type (Result)
      then
         return Result;

      else
         declare
            LHS_I : constant LLI := Get_Const_Int_Value (LHS);
            RHS_I : constant LLI := Get_Const_Int_Value (RHS);
            Res_I : constant LLI := Get_Const_Int_Value (Result);

         begin
            return Mark_Overflowed (Result, Res_I / LHS_I /= RHS_I);
         end;
      end if;

   end Mul;

   ---------------------
   -- Trunc_Oveflowed --
   ---------------------

   function Trunc_Overflowed (V, Result : GL_Value) return Boolean is
      Bitsize : constant Integer :=
        Integer (Get_Scalar_Bit_Size (Type_Of (Result)));

   begin
      --  If this is a modular type or the input or output isn't an integer,
      --  we can't have an overlow (the output should be an integer if the
      --  input is, but let's check anyway).

      if Is_Modular_Integer_Type (Result) or else not Is_A_Const_Int (V)
        or else not Is_A_Const_Int (Result)

        --  If the values of the old and new constants are the same, there's
        --  no overflow.

        or else Get_Const_Int_Value (V) = Get_Const_Int_Value (Result)
      then
         return False;

      --  At this point, we have a non-modular type and the values differ.
      --  That's an overflow if we have a signed type.

      elsif not Is_Unsigned_Type (Result) then
         return True;

      --  Otherwise, we could have an issue because LLVM views all constants
      --  as sign-extended, so if the high-order bit is set, it'll extend
      --  the sign all the way out.  We check for this by masking the
      --  value to its width.

      else
         declare
            Mask          : constant ULL := (ULL (2) ** Bitsize) - 1;
            Orig          : constant ULL := Get_Const_Int_Value_ULL (V);
            Masked_Result : constant ULL :=
              Get_Const_Int_Value_ULL (V) and Mask;

         begin
            return Orig /= Masked_Result;
         end;
      end if;
   end Trunc_Overflowed;

   -----------
   -- Trunc --
   -----------

   function Trunc
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
      T      : constant Type_T   := Type_Of (GT);
      Result : constant GL_Value :=
        G (Trunc (IR_Builder, LLVM_Value (V), T, Name), GT);

   begin
      return Mark_Overflowed
        (Result, Overflowed (V) or else Trunc_Overflowed (V, Result));

   end Trunc;

   ---------------------------
   -- Trunc_To_Relationship --
   ---------------------------

   function Trunc_To_Relationship
     (V    : GL_Value;
      T    : Type_T;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
      Result : constant GL_Value :=
        G (Trunc (IR_Builder, LLVM_Value (V), T, Name), Related_Type (V), R);

   begin
      return Mark_Overflowed
        (Result, Overflowed (V) or else Trunc_Overflowed (V, Result));

   end Trunc_To_Relationship;

   -----------
   -- S_Ext --
   -----------

   function S_Ext
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (S_Ext (IR_Builder, LLVM_Value (V), Type_Of (GT), Name), GT));

   -----------
   -- Z_Ext --
   -----------

   function Z_Ext
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (Z_Ext (IR_Builder, LLVM_Value (V), Type_Of (GT), Name), GT,
         Overflowed => Overflowed (V)));

   --------------
   -- FP_Trunc --
   --------------

   function FP_Trunc
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (FP_Trunc (IR_Builder, LLVM_Value (V), Type_Of (GT), Name), GT,
         Overflowed => Overflowed (V)));

   ------------
   -- FP_Ext --
   ------------

   function FP_Ext
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (FP_Ext (IR_Builder, LLVM_Value (V), Type_Of (GT), Name), GT,
         Overflowed => Overflowed (V)));

   --------------
   -- FP_To_SI --
   --------------

   function FP_To_SI
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (FP_To_SI (IR_Builder, LLVM_Value (V), Type_Of (GT), Name), GT,
         Overflowed => Overflowed (V)));

   --------------
   -- FP_To_UI --
   --------------

   function FP_To_UI
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (FP_To_UI (IR_Builder, LLVM_Value (V), Type_Of (GT), Name), GT));

   --------------
   -- UI_To_FP --
   --------------

   function UI_To_FP
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (UI_To_FP (IR_Builder, LLVM_Value (V), Type_Of (GT), Name), GT,
        Overflowed => Overflowed (V)));

   --------------
   -- SI_To_FP --
   --------------

   function SI_To_FP
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (SI_To_FP (IR_Builder, LLVM_Value (V), Type_Of (GT), Name), GT,
         Overflowed => Overflowed (V)));

   -------------------
   -- Build_Cond_Br --
   -------------------

   procedure Build_Cond_Br (C_If : GL_Value; C_Then, C_Else : Basic_Block_T) is
   begin
      Discard (Build_Cond_Br (IR_Builder, LLVM_Value (C_If), C_Then, C_Else));
   end Build_Cond_Br;

   --------------
   -- Build_Br --
   --------------

   procedure Build_Br (BB : Basic_Block_T) is
   begin
      if not Are_In_Dead_Code then
         Discard (Build_Br (IR_Builder, BB));
      end if;
   end Build_Br;

   --------------------
   -- Maybe_Build_Br --
   --------------------

   procedure Maybe_Build_Br (BB : Basic_Block_T) is
   begin
      if not Are_In_Dead_Code and then Present (BB) then
         Build_Br (BB);
      end if;
   end Maybe_Build_Br;

   ----------------
   -- Move_To_BB --
   ----------------

   procedure Move_To_BB (BB : Basic_Block_T) is
   begin
      if Present (BB) then
         Build_Br (BB);
         Position_Builder_At_End (BB);
      end if;
   end Move_To_BB;

   ---------------
   -- Build_Ret --
   ---------------

   procedure Build_Ret (V : GL_Value) is
   begin
      Discard (Build_Ret (IR_Builder, LLVM_Value (V)));
   end Build_Ret;

   --------------------
   -- Build_Ret_Void --
   --------------------

   procedure Build_Ret_Void is
   begin
      Discard (Build_Ret_Void (IR_Builder));
   end Build_Ret_Void;

   -----------------------
   -- Build_Unreachable --
   -----------------------

   procedure Build_Unreachable is
   begin
      Discard (Build_Unreachable (IR_Builder));
   end Build_Unreachable;

   ---------------
   -- Build_Phi --
   ---------------

   function Build_Phi
     (GL_Values : GL_Value_Array;
      BBs       : Basic_Block_Array;
      Name      : String := "") return GL_Value
   is
      Values  : aliased Value_Array (GL_Values'Range);
      Our_Phi : Value_T;

   begin
      for J in Values'Range loop
         Values (J) := LLVM_Value (GL_Values (J));
      end loop;

      Our_Phi := Phi (IR_Builder, Type_Of (GL_Values (GL_Values'First)), Name);
      Add_Incoming (Our_Phi, Values'Address, BBs'Address, Values'Length);
      return G_From (Our_Phi, GL_Values (GL_Values'First));
   end Build_Phi;

   ---------
   -- GEP --
   ---------

   function GEP
     (Bld     : Builder_T;
      Ptr     : Value_T;
      Indices : Value_Array;
      Name    : String := "") return Value_T
   is
     (GEP (Bld, Ptr, Indices'Address, Indices'Length, Name));

   -------------------------
   -- GEP_To_Relationship --
   -------------------------

   function GEP_To_Relationship
     (GT      : GL_Type;
      R       : GL_Relationship;
      Ptr     : GL_Value;
      Indices : GL_Value_Array;
      Name    : String := "") return GL_Value
   is
      Val_Idxs : aliased Value_Array (Indices'Range);
      Result   : Value_T;

   begin
      for J in Indices'Range loop
         Val_Idxs (J) := LLVM_Value (Indices (J));
      end loop;

      Result := In_Bounds_GEP (IR_Builder, LLVM_Value (Ptr), Val_Idxs'Address,
                               Val_Idxs'Length, Name);
      return GM (Result, GT, R, Ptr);
   end GEP_To_Relationship;

   -----------------------------
   -- GEP_Idx_To_Relationship --
   -----------------------------

   function GEP_Idx_To_Relationship
     (GT      : GL_Type;
      R       : GL_Relationship;
      Ptr     : GL_Value;
      Indices : Index_Array;
      Name    : String := "") return GL_Value
   is
      Val_Idxs : aliased Value_Array (Indices'Range);
      Result   : Value_T;

   begin
      for J in Indices'Range loop
         Val_Idxs (J) :=
           Const_Int (Int_Ty (Nat (32)), ULL (Indices (J)), False);
      end loop;

      Result := In_Bounds_GEP (IR_Builder, LLVM_Value (Ptr), Val_Idxs'Address,
                               Val_Idxs'Length, Name);
      return GM (Result, GT, R, Ptr);
   end GEP_Idx_To_Relationship;

   ----------
   -- Load --
   ----------

   function Load (Ptr : GL_Value; Name : String := "") return GL_Value is
      New_R          : constant GL_Relationship :=
        Relation_Props (Relationship (Ptr)).Deref;
      --  Get the resulting relation after the load

      Load_GT        : constant GL_Type         :=
        (if   Is_Data (Ptr) then Full_Designated_GL_Type (Ptr)
         else Related_Type (Ptr));
      --  If our input is data, the resulting type is the designated type
      --  of an access type. Otherwise, it's the same type.

      T              : constant Type_T          := Type_Of (Load_GT);
      --  The LLVM type that will be loaded by this instruction

      Result_Bits    : constant Nat             :=
        (if Is_Data (New_R) then Nat (Get_Scalar_Bit_Size (T)) else 0);
      --  Size in bits that will be loaded by this instruction

      Special_Atomic : constant Boolean         :=
        Is_Data (New_R) and then Is_Atomic (Ptr)
          and then not Atomic_Kind (T)
          and then Nat'(Get_Type_Alignment (Load_GT)) >= Result_Bits;
      --  True if this is an atomic reference that LLVM can't handle
      --  directly.

      Equiv_T        : constant Type_T         :=
        (if   Special_Atomic then Pointer_Type (Int_Ty (Result_Bits), 0)
         else No_Type_T);
      --  Integer type with size matching that of the type to be loaded

      Ptr_Val        : constant Value_T        :=
        (if   Special_Atomic
         then Pointer_Cast (IR_Builder, LLVM_Value (Ptr), Equiv_T, "")
         else LLVM_Value (Ptr));
      --  Address of item to load

      Load_Inst : Value_T                      :=
        Load (IR_Builder, Ptr_Val, Name);
      --  The actual load instruction

      Memory    : GL_Value;
      --  Memory to use as temporary

   begin
      --  If this is going to actually be pointing to data of the related
      --  type, indicate that we're loading an object of that type.
      --  ??? At some point, we need to deal with TBAA or similar when
      --  this isn't true.

      if Is_Data (New_R) then
         Add_Type_Data_To_Instruction (Load_Inst, Ptr, Special_Atomic);
      end if;

      --  If this is the special atomic case, we need to allocate memory,
      --  store what we loaded into it, load it back again as the proper
      --  type, and return that value.

      if Special_Atomic then
         Memory := Allocate_For_Type (Load_GT, Load_GT, Empty);
         Discard (Build_Store (IR_Builder, Load_Inst,
                               Pointer_Cast (IR_Builder, LLVM_Value (Memory),
                                             Equiv_T, "")));
         Load_Inst := Load (IR_Builder, LLVM_Value (Memory), "");
      end if;

      --  Now build the result, with the proper GT and relationship

      return G (Load_Inst, Load_GT, New_R);
   end Load;

   -----------
   -- Store --
   -----------

   procedure Store (Expr : GL_Value; Ptr : GL_Value) is
      GT             : constant GL_Type := Related_Type (Expr);
      T              : constant Type_T  := Type_Of (Expr);
      Result_Bits    : constant Nat     :=
        (if Is_Data (Expr) then Nat (Get_Scalar_Bit_Size (T)) else 0);
      Special_Atomic : constant Boolean :=
        Is_Data (Expr) and then Is_Atomic (Ptr) and then not Atomic_Kind (T)
          and then Nat'(Get_Type_Alignment (GT)) >= Result_Bits;
      Equiv_T        : constant Type_T  :=
        (if   Special_Atomic then Pointer_Type (Int_Ty (Result_Bits), 0)
         else No_Type_T);
      Ptr_Val        : constant Value_T :=
        (if   Special_Atomic
         then Pointer_Cast (IR_Builder, LLVM_Value (Ptr), Equiv_T, "")
         else LLVM_Value (Ptr));
      Val_To_Store   : Value_T          := LLVM_Value (Expr);
      Store_Inst     : Value_T;
      Memory         : GL_Value;

   begin
      --  If this is a special atomic store, allocate a temporary, store
      --  the data into it, then load that as the equivalent type and store
      --  into into the pointer-punned result.

      if Special_Atomic then
         Memory := Allocate_For_Type (GT, GT, Empty);
         Discard (Build_Store (IR_Builder, Val_To_Store, LLVM_Value (Memory)));
         Val_To_Store := Load (IR_Builder,
                               Pointer_Cast (IR_Builder, LLVM_Value (Memory),
                                             Equiv_T, ""), "");
      end if;

      --  Now do the actual store and set the attributes

      Store_Inst := Build_Store (IR_Builder, Val_To_Store, Ptr_Val);

      if Is_Data (Expr) then
         Add_Type_Data_To_Instruction (Store_Inst, Ptr, Special_Atomic);
      end if;

   end Store;

   -------------------
   -- Call_Internal --
   ------------------

   function Call_Internal
     (Func        : GL_Value;
      Args        : GL_Value_Array;
      Name        : String := "") return Value_T
   is
      LLVM_Func   : constant Value_T       := LLVM_Value (Func);
      No_Raise    : constant Boolean       :=
        Is_A_Function (Func) and then Does_Not_Throw (Func);
      Lpad        : constant Basic_Block_T :=
        (if No_Raise then No_BB_T else Get_Landing_Pad);
      Act_Param   : Int                    := -1;
      Arg_Values  : aliased Value_Array (Args'Range);
      Next_BB     : Basic_Block_T;
      Call_Inst   : Value_T;

   begin
      for J in Args'Range loop
         Arg_Values (J) := LLVM_Value (Args (J));
         if Relationship (Args (J)) = Reference_To_Activation_Record then
            Act_Param := J - Args'First;
         end if;
      end loop;

      --  If we have a landing pad, use an invoke instruction, first creating
      --  the basic block to branch to in the normal case.

      if Present (Lpad) then
         Next_BB := Create_Basic_Block;
         Call_Inst := Invoke (IR_Builder, LLVM_Func,
                              Arg_Values'Address, Arg_Values'Length,
                              Next_BB, Lpad, Name);
         Position_Builder_At_End (Next_BB);
      else
         Call_Inst := Call (IR_Builder, LLVM_Func,
                            Arg_Values'Address, Arg_Values'Length, Name);
      end if;

      --  If we found a parameter that was an activation record, mark it

      if Act_Param >= 0 then
         Add_Nest_Attribute (Call_Inst, unsigned (Act_Param));
      end if;

      return Call_Inst;
   end Call_Internal;

   ----------
   -- Call --
   ----------

   function Call
     (Func : GL_Value;
      GT   : GL_Type;
      Args : GL_Value_Array;
      Name : String := "") return GL_Value is
     (G (Call_Internal (Func, Args, Name), GT));

   --------------
   -- Call_Ref --
   --------------

   function Call_Ref
     (Func : GL_Value;
      GT   : GL_Type;
      Args : GL_Value_Array;
      Name : String := "") return GL_Value is
     (G_Ref (Call_Internal (Func, Args, Name), GT));

   -----------------------
   -- Call_Relationship --
   -----------------------

   function Call_Relationship
     (Func : GL_Value;
      GT   : GL_Type;
      Args : GL_Value_Array;
      R    : GL_Relationship;
      Name : String := "") return GL_Value is
     (G (Call_Internal (Func, Args, Name), GT, R));

   ----------
   -- Call --
   ----------

   procedure Call
     (Func : GL_Value; Args : GL_Value_Array; Name : String := "") is
   begin
      Discard (Call_Internal (Func, Args, Name));
   end Call;

   ----------------
   -- Call_Align --
   ----------------

   procedure Call_With_Align
     (Func : GL_Value; Args : GL_Value_Array; Align : Nat; Name : String := "")
   is
      CI : constant Value_T := Call_Internal (Func, Args, Name);

   begin
      --  ??? See comment in Add_Type_Data_To_Instruction.

      Set_Instr_Param_Alignment (CI, 1, unsigned (Nat'Min (Align, 64)));
   end Call_With_Align;

   ------------------------
   --  Call_With_Align_2 --
   ------------------------

   procedure Call_With_Align_2
     (Func             : GL_Value;
      Args             : GL_Value_Array;
      Align_1, Align_2 : Nat;
      Name             : String := "")
   is
      CI : constant Value_T := Call_Internal (Func, Args, Name);

   begin
      --  ??? See comment in Add_Type_Data_To_Instruction.

      Set_Instr_Param_Alignment (CI, 1, unsigned (Nat'Min (Align_1, 64)));
      Set_Instr_Param_Alignment (CI, 2, unsigned (Nat'Min (Align_2, 64)));
   end Call_With_Align_2;

   ------------------
   -- Build_Resume --
   ------------------

   procedure Build_Resume (V : GL_Value) is
   begin
      Discard (Build_Resume (IR_Builder, LLVM_Value (V)));
   end Build_Resume;

   ----------------
   -- Inline_Asm --
   ----------------

   function Inline_Asm
     (Args           : GL_Value_Array;
      Output_Value   : Entity_Id;
      Template       : String;
      Constraints    : String;
      Is_Volatile    : Boolean := False;
      Is_Stack_Align : Boolean := False) return GL_Value
   is
      GT        : constant GL_Type :=
        (if   Present (Output_Value)
         then Primitive_GL_Type (Full_Etype (Output_Value))
         else Void_GL_Type);
      T         : constant Type_T  :=
        (if Present (Output_Value) then Type_Of (GT) else Void_Type);
      Arg_Types : Type_Array (Args'Range);

   begin
      for J in Args'Range loop
         Arg_Types (J) := Type_Of (Args (J));
      end loop;

      return G (Const_Inline_Asm (Fn_Ty (Arg_Types, T), Template,
                                  Constraints, Is_Volatile, Is_Stack_Align),
                GT, Reference_To_Subprogram);
   end Inline_Asm;

end GNATLLVM.Instructions;
