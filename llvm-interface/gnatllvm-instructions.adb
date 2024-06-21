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

with Errout;   use Errout;
with Restrict; use Restrict;
with Rident;   use Rident;
with Targparm; use Targparm;

with GNATLLVM.Blocks;      use GNATLLVM.Blocks;
with GNATLLVM.Builtins;    use GNATLLVM.Builtins;
with GNATLLVM.Codegen;     use GNATLLVM.Codegen;
with GNATLLVM.Conversions; use GNATLLVM.Conversions;
with GNATLLVM.GLType;      use GNATLLVM.GLType;
with GNATLLVM.Subprograms; use GNATLLVM.Subprograms;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Variables;   use GNATLLVM.Variables;

package body GNATLLVM.Instructions is

   function Call_Internal
     (Func : GL_Value;
      Fn_T : Type_T;
      Args : GL_Value_Array;
      Name : String := "") return Value_T;
   --  Internal version of Call and Call_Ref

   function GEP_To_Relationship_Internal
     (Ptr      : GL_Value;
      Ptr_T    : Type_T;
      GT       : GL_Type;
      R        : GL_Relationship;
      Val_Idxs : Value_Array;
      Name     : String) return GL_Value
     with Pre => Present (Ptr) and then Present (Ptr_T) and then Present (GT);
   --  Internal version of GEP_To_Relationship variants

   function Trunc_Overflowed (V, Result : GL_Value) return Boolean
     with Pre => Present (V) and then Present (Result);
   --  Return True if the truncation operation from V to Result overflowed

   procedure Update_Offset_For_GEP (Result : in out GL_Value; Ptr : GL_Value)
     with Pre => Present (Result) and then Present (Ptr);
   --  Result is the result of a GEP whose pointer is Ptr. Update the
   --  TBAA offset of Result or clear the TBAA data if we can't compute it.

   function Return_GL_Type (Func : GL_Value) return GL_Type is
     (if   Ekind (Func) = E_Subprogram_Type
      then Default_GL_Type (Full_Etype (Full_Etype (Func)))
      else Related_Type (Func));

   --------------------------
   -- Get_Current_Position --
   --------------------------

   function Get_Current_Position return Position_T is
      BB          : constant Basic_Block_T := Get_Insert_Block;
      Last_Inst   : constant Value_T       := Get_Last_Instruction (BB);
      Latest_Inst : constant Value_T       :=
        (if   Present (Last_Inst) then Get_Latest_Instruction (IR_Builder)
         else No_Value_T);

   begin
      return (BB, Latest_Inst);
   end Get_Current_Position;

   --------------------------
   -- Set_Current_Position --
   --------------------------

   procedure Set_Current_Position (P : Position_T) is
      BB   : constant Basic_Block_T := P.BB;
      Next : Value_T;

   begin
      --  There are two tricky parts here. First is that if there's no
      --  instruction, LLVM will treat this as a request to insert at the
      --  end of a basic block, but we mean the beginning. So we need to
      --  get the first instruction in the block and set the insertion
      --  point in front of it. Secondly, if we have an instruction, the
      --  builder operation inserts in front of it, but we want to insert
      --  after. The way to do that is to get the next instruction and
      --  insert before that, but there may not be another instruction. If
      --  so, then insert at the end of the block.

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

   ----------------------------
   -- Is_Equivalent_Position --
   ----------------------------

   function Is_Equivalent_Position (P1, P2 : Position_T) return Boolean is
      function Is_Branch_To
        (Inst : Value_T; BB : Basic_Block_T) return Boolean
      is
        (Present (Inst) and then Present (Is_A_Branch_Inst (Inst))
         and then not Is_Conditional (Inst)
         and then Value_As_Basic_Block (Get_Operand (Inst, 0)) = BB);

   begin
      return P1 = P2
        or else Is_Branch_To (Get_Next_Instruction_After (P1), P2.BB)
        or else Is_Branch_To (Get_Next_Instruction_After (P2), P1.BB);
   end Is_Equivalent_Position;

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
     (GT    : GL_Type;
      E     : Entity_Id := Empty;
      Align : Nat       := 0;
      Name  : String    := "") return GL_Value
   is
      R         : constant GL_Relationship := Relationship_For_Alloc (GT);
      T         : constant Type_T          :=
        Type_For_Relationship (GT, Deref (R));
      Promote   : constant Basic_Block_T   := Maybe_Promote_Alloca (T);
      Inst      : constant Value_T         :=
        Alloca (IR_Builder, T, Get_Alloca_Name (E, Name));
      Our_Align : constant Nat             :=
        Set_Object_Align (Inst, GT, E, Align);
      Result    : GL_Value                 :=
        G (Inst, GT, R, Is_Pristine => True, Alignment => Our_Align);

   begin
      Done_Promoting_Alloca (Result, Promote, T);
      Initialize_TBAA (Result, Kind_From_Decl (E));
      return Result;
   end Alloca;

   ------------------
   -- Array_Alloca --
   ------------------

   function Array_Alloca
     (GT       : GL_Type;
      Num_Elts : GL_Value;
      E        : Opt_Object_Kind_Id := Empty;
      Align    : Nat                := 0;
      Name     : String             := "") return GL_Value
   is
      T         : constant Type_T        := Type_Of (GT);
      Promote   : constant Basic_Block_T := Maybe_Promote_Alloca (T, Num_Elts);
      Inst      : constant Value_T       :=
        Array_Alloca (IR_Builder, Type_Of (GT), +Num_Elts,
                      Get_Alloca_Name (E, Name));
      Our_Align : constant Nat           :=
        Set_Object_Align (Inst, GT, E, Align);
      Result    : GL_Value               :=
        G_Ref (Inst, GT, Is_Pristine => True, Alignment => Our_Align);

   begin
      Done_Promoting_Alloca (Result, Promote, T, Num_Elts);
      Initialize_TBAA (Result, Kind_From_Decl (E));
      return Result;
   end Array_Alloca;

   ----------------
   -- Int_To_Ptr --
   ----------------

   function Int_To_Ptr
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
      Result : GL_Value :=
        GM (Int_To_Ptr (IR_Builder, +V, Type_Of (GT), Name), GT, GV => V);

   begin
      Initialize_TBAA_If_Changed (Result, V);
      return Result;
   end Int_To_Ptr;

   ----------------
   -- Ptr_To_Int --
   ----------------

   function Ptr_To_Int
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (GM
        ((if   Tagged_Pointers
          then Bit_Cast (IR_Builder, +V, Void_Ptr_T, Name)
          else Ptr_To_Int (IR_Builder, +V, Type_Of (GT), Name)),
         GT, GV => V));

   ----------------
   -- Int_To_Ref --
   ----------------

   function Int_To_Ref
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
      Typ    : constant Type_T  :=
        Pointer_Type (Type_Of (GT), Address_Space);
      Ptr    : constant Value_T :=
        (if   Tagged_Pointers
         then Pointer_Cast (IR_Builder, +V, Typ, Name)
         else Int_To_Ptr (IR_Builder, +V, Typ, Name));
      Result : GL_Value         := GM_Ref (Ptr, GT, V);

   begin
      Initialize_TBAA_If_Changed (Result, V);
      return Result;
   end Int_To_Ref;

   -------------------------
   -- Int_To_Relationship --
   -------------------------

   function Int_To_Relationship
     (V    : GL_Value;
      GT   : GL_Type;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
      Typ    : constant Type_T  := Type_For_Relationship (GT, R);
      Ptr    : constant Value_T :=
        (if   Tagged_Pointers
         then Pointer_Cast (IR_Builder, +V, Typ, Name)
         else Int_To_Ptr (IR_Builder, +V, Typ, Name));
      Result : GL_Value         := GM (Ptr, GT, R, GV => V);

   begin
      Initialize_TBAA_If_Changed (Result, V);
      return Result;
   end Int_To_Relationship;

   --------------
   -- Bit_Cast --
   --------------

   function Bit_Cast
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (GM (Bit_Cast (IR_Builder, +V, Type_Of (GT), Name), GT, GV => V));

   ------------------
   -- Pointer_Cast --
   ------------------

   function Pointer_Cast
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
      Result : GL_Value :=
        GM (Pointer_Cast (IR_Builder, +V, Type_Of (GT), Name), GT, GV => V);
   begin
      Initialize_TBAA_If_Changed (Result, V);
      return Result;
   end Pointer_Cast;

   ----------------
   -- Ptr_To_Ref --
   ----------------

   function Ptr_To_Ref
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
      Result : GL_Value :=
        GM_Ref
          (Pointer_Cast
             (IR_Builder, +V, Pointer_Type (Type_Of (GT), Address_Space),
              Name),
           GT, V);

   begin
      Initialize_TBAA_If_Changed (Result, V);
      return Result;
   end Ptr_To_Ref;

   ----------------
   -- Ptr_To_Ref --
   ----------------

   function Ptr_To_Ref (V, T : GL_Value; Name : String := "") return GL_Value
   is
      Result : GL_Value :=
        GM_Ref (Pointer_Cast (IR_Builder, +V,
                              Pointer_Type (Type_Of (T), Address_Space), Name),
                Full_Designated_GL_Type (T), V);

   begin
      Initialize_TBAA_If_Changed (Result, V);
      return Result;
   end Ptr_To_Ref;

   -------------------------
   -- Ptr_To_Relationship --
   -------------------------

   function Ptr_To_Relationship
     (V    : GL_Value;
      GT   : GL_Type;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
      Result : GL_Value :=
        GM (Pointer_Cast (IR_Builder, +V, Type_For_Relationship (GT, R), Name),
            GT, R, V);

   begin
      Initialize_TBAA_If_Changed (Result, V);
      return Result;
   end Ptr_To_Relationship;

   -------------------------
   -- Ptr_To_Relationship --
   -------------------------

   function Ptr_To_Relationship
     (V, T : GL_Value;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
      Result : GL_Value :=
        GM (Pointer_Cast (IR_Builder, +V,
                          Type_For_Relationship (Related_Type (T), R), Name),
            Related_Type (T), R, V);

   begin
      Initialize_TBAA_If_Changed (Result, V);
      return Result;
   end Ptr_To_Relationship;

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

      --  Otherwise, perform the operation, respect any overflow flags,
      --  and indicate what it does to the alignment

      V := (if   Is_Add
            then Add (IR_Builder, +LHS, +RHS, Name)
            else Sub (IR_Builder, +LHS, +RHS, Name));
      Result := G_From (Set_Arith_Attrs (V, LHS), LHS);
      Mark_Overflowed (Result, Overflowed (LHS) or else Overflowed (RHS));
      Set_Alignment (Result, Nat'Min (Alignment (LHS), Alignment (RHS)));

      --  If we have a TBAA type on the LHS (and hence on the result), we
      --  can only leave it that way (and update the offset) if there's no
      --  TBAA type on the RHS and the RHS is a constant. It must also be
      --  true that the new offset is not negative.

      if Present (TBAA_Type (Result)) then
         if Present (TBAA_Type (RHS)) or else not Is_A_Constant_Int (RHS)
           or else Overflowed (RHS)
         then
            Set_TBAA_Type (Result, No_Metadata_T);
         else
            declare
               Old_Offset      : constant LLI := LLI (TBAA_Offset (Result));
               RHS_Value       : constant LLI := +RHS;
               RHS_Effective_V : constant LLI :=
                 (if Is_Add then RHS_Value else -RHS_Value);
               New_Offset      : constant LLI := Old_Offset + RHS_Effective_V;

            begin
               if New_Offset < 0 then
                  Set_TBAA_Type   (Result, No_Metadata_T);
               else
                  Set_TBAA_Offset (Result, ULL (New_Offset));
               end if;
            end;
         end if;
      end if;

      --  If either operand or the result isn't a constant integer, if this
      --  is a modular integer type, or if we already had an overflow, we
      --  don't have to test for overflow.
      --  ???  We can't currently test for overflow if wider than ULL.

      if not Is_A_Constant_Int (LHS) or else not Is_A_Constant_Int (RHS)
        or else not Is_A_Constant_Int (Result) or else Overflowed (Result)
        or else Is_Modular_Integer_Type (Result)
        or else Esize (Result) > ULL'Size
      then
         return Result;

      --  Otherwise, test for overflow. Note that, unlike in C, LLVM
      --  defines the result if an overflow occurs (mod 2**N), so we can
      --  safely do post-operation testing that we couldn't do if this were
      --  the C addition operation. The unsigned case is simple

      elsif Is_Unsigned_Type (Result) then
         declare
            LHS_I : constant ULL := +LHS;
            RHS_I : constant ULL := +RHS;
            Res_I : constant ULL := +Result;

         begin
            Mark_Overflowed
              (Result, (if Is_Add then Res_I < LHS_I else LHS_I < RHS_I));
            return Result;
         end;
      else
         --  For signed, we overflow if the two operands have the same (for
         --  addition) or different (for subtraction) sign and the sign of
         --  the result differs.

         declare
            LHS_Neg    : constant Boolean  := +LHS    < LLI (0);
            RHS_Neg    : constant Boolean  := +RHS    < LLI (0);
            Res_Neg    : constant Boolean  := +Result < LLI (0);
            Maybe_Ovfl : constant Boolean  :=
              (if Is_Add then LHS_Neg = RHS_Neg else LHS_Neg = not RHS_Neg);

         begin
            Mark_Overflowed (Result,
                             Maybe_Ovfl and then LHS_Neg = not Res_Neg);
            return Result;
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

      --  Otherwise, perform the operation, respect any overflow flags

      Result := G_From (Set_Arith_Attrs (Mul (IR_Builder, +LHS, +RHS, Name),
                                         LHS),
                        LHS);
      Mark_Overflowed (Result, Overflowed (LHS) or else Overflowed (RHS));
      Set_TBAA_Type   (Result, No_Metadata_T);

      --  Set the resulting alignment. Be careful about overflow, but be
      --  conservative on overflow with regard to BPU.

      Set_Alignment
        (Result,
         (if   Max_Valid_Align / Alignment (LHS) > Alignment (RHS)
          then Alignment (LHS) * Alignment (RHS) / BPU
          else Max_Valid_Align / BPU));

      --  If either operand or the result isn't a constant integer, if this
      --  is a modular integer type, or if we already had an overflow, we
      --  don't have to test for overflow.
      --  ???  We can't currently test for overflow if wider than ULL.

      if not Is_A_Constant_Int (LHS) or else not Is_A_Constant_Int (RHS)
        or else not Is_A_Constant_Int (Result) or else Overflowed (Result)
        or else Is_Modular_Integer_Type (Result)
        or else Esize (Result) > ULL'Size
      then
         return Result;

      else
         declare
            LHS_I : constant LLI := +LHS;
            RHS_I : constant LLI := +RHS;
            Res_I : constant LLI := +Result;

         begin
            Mark_Overflowed (Result, Res_I / LHS_I /= RHS_I);
            return Result;
         end;
      end if;
   end Mul;

   ---------
   -- Div --
   ---------

   function Div
     (LHS, RHS : GL_Value;
      Signed   : Boolean;
      Name     : String := "") return GL_Value
   is
      Result : GL_Value;

   begin
      --  Constant fold dividing by one

      if Is_Const_Int_Value (RHS, 1) then
         return LHS;
      end if;

      --  Otherwise, compute the value and check for overflow

      Result := G_From ((if Signed
                         then S_Div (IR_Builder, +LHS, +RHS, Name)
                         else U_Div (IR_Builder, +LHS, +RHS, Name)),
                        LHS);
      Mark_Overflowed (Result,
                       Overflowed (LHS) or else Overflowed (RHS)
                       or else (Signed and Is_Const_Int_Value (RHS, -1)));
      Set_TBAA_Type   (Result, No_Metadata_T);

      --  If RHS is a constant power of two, we can compute the alignment
      --  of the result from the input, but only check for small powers of
      --  two. Otherwise we don't know anything about the alignment. To
      --  avoid issues relating to negative-looking values, we handle each
      --  case specifically as a constant.

      if Is_Const_Int_Value (RHS, 2) then
         Set_Alignment (Result, Alignment (LHS) / 2);
      elsif Is_Const_Int_Value (RHS, 4) then
         Set_Alignment (Result, Alignment (LHS) / 4);
      elsif Is_Const_Int_Value (RHS, 8) then
         Set_Alignment (Result, Alignment (LHS) / 8);
      else
         Clear_Alignment (Result);
      end if;

      return Result;
   end Div;

   -----------
   -- Shift --
   -----------

   function Shift
     (V              : GL_Value;
      Count          : GL_Value;
      Left           : Boolean;
      Arithmetic     : Boolean;
      Allow_Overflow : Boolean;
      Name           : String := "") return GL_Value
   is
      VV     : constant Value_T := +V;
      CV     : constant Value_T := +Count;
      RV     : Value_T;
      Result : GL_Value;

   begin
      --  Constant fold shifting by zero

      if Is_Const_Int_Value (Count, 0) then
         return V;
      end if;

      --  Perform the shift desired shift operation

      if Left then
         RV := Shl   (IR_Builder, VV, CV, Name);
      elsif Arithmetic then
         RV := A_Shr (IR_Builder, VV, CV, Name);
      else
         RV := L_Shr (IR_Builder, VV, CV, Name);
      end if;

      --  For left shift, set NUW/NSW unless we're told not to

      if Left and then not Allow_Overflow then
         RV := Set_Arith_Attrs (RV, V);
      end if;

      --  Build the result and propagate the overflow indication. We're not
      --  going to check for a possible constant overflow in the shift
      --  itself.

      Result := G_From (RV, V);
      Mark_Overflowed (Result, Overflowed (V) or else Overflowed (Count));
      Set_TBAA_Type   (Result, No_Metadata_T);

      --  If count is a positive constant, we can compute the alignment of
      --  the result from the input. Otherwise we don't know anything about
      --  the alignment.

      if Is_A_Constant_Int (Count) and then +Count > ULL (0) then
         if +Count >= ULL (20) then
            Set_Alignment (Result, (if Left then Max_Valid_Align else BPU));
         else
            declare
               L : constant Nat := Alignment (V);
               R : constant Nat := 2 ** Integer (Get_Const_Int_Value (Count));

            begin
               --  For left shifts, ensure we aren't going to overflow and
               --  for right shifts, never set an alignment less than BPU.

               Set_Alignment
                 (Result,
                  (if   Left
                   then (if   Max_Valid_Align / R > L then L * R
                         else Max_Valid_Align)
                   else Nat'Max (BPU, L / R)));
            end;
         end if;
      else
         Clear_Alignment (Result);
      end if;

      return Result;
   end Shift;

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

      if Is_Modular_Integer_Type (Result) or else not Is_A_Constant_Int (V)
        or else not Is_A_Constant_Int (Result)

        --  If the values of the old and new constants are the same, there's
        --  no overflow.

        or else Is_Const_Int_Values_Equal (V, Result)
      then
         return False;

      --  At this point, we have a non-modular type and the values differ.
      --  That's an overflow if we have a signed type.

      elsif not Is_Unsigned_Type (Result) then
         return True;

      --  Otherwise, we could have an issue because LLVM views all constants
      --  as sign-extended, so if the high-order bit is set, it'll extend
      --  the sign all the way out. We check for this by masking the
      --  value to its width. But don't do this unless we have data (not
      --  "unknown"

      elsif Is_Data (V) then
         declare
            Mask          : constant GL_Value :=
              Const_Int (V, (ULL (2) ** Bitsize) - 1);
            Masked_Result : constant GL_Value := Build_And (Mask, V);

         begin
            return not Is_Const_Int_Values_Equal (V, Masked_Result);
         end;

      --  Otherwise, no overflow could have occurred

      else
         return False;
      end if;
   end Trunc_Overflowed;

   -----------
   -- Trunc --
   -----------

   function Trunc
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
      T      : constant Type_T := Type_Of (GT);
      Result :  GL_Value       :=
        GM (Trunc (IR_Builder, +V, T, Name), GT, Data, V);

   begin
      Mark_Overflowed (Result,
                       Overflowed (V) or else Trunc_Overflowed (V, Result));
      return Result;
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
      Result : GL_Value :=
        GM (Trunc (IR_Builder, +V, T, Name), Related_Type (V), R, V);

   begin
      Mark_Overflowed (Result,
                       Overflowed (V) or else Trunc_Overflowed (V, Result));
      return Result;
   end Trunc_To_Relationship;

   -----------
   -- S_Ext --
   -----------

   function S_Ext
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (S_Ext (IR_Builder, +V, Type_Of (GT), Name), GT));

   -----------
   -- Z_Ext --
   -----------

   function Z_Ext
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (Z_Ext (IR_Builder, +V, Type_Of (GT), Name), GT,
         Alignment  => Alignment  (V),
         Overflowed => Overflowed (V)));

   --------------
   -- FP_Trunc --
   --------------

   function FP_Trunc
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (FP_Trunc (IR_Builder, +V, Type_Of (GT), Name), GT,
         Overflowed => Overflowed (V)));

   ------------
   -- FP_Ext --
   ------------

   function FP_Ext
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (FP_Ext (IR_Builder, +V, Type_Of (GT), Name), GT,
         Overflowed => Overflowed (V)));

   --------------
   -- FP_To_SI --
   --------------

   function FP_To_SI
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (FP_To_SI (IR_Builder, +V, Type_Of (GT), Name), GT,
         Overflowed => Overflowed (V)));

   --------------
   -- FP_To_UI --
   --------------

   function FP_To_UI
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (FP_To_UI (IR_Builder, +V, Type_Of (GT), Name), GT));

   --------------
   -- UI_To_FP --
   --------------

   function UI_To_FP
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (UI_To_FP (IR_Builder, +V, Type_Of (GT), Name), GT,
        Overflowed => Overflowed (V)));

   --------------
   -- SI_To_FP --
   --------------

   function SI_To_FP
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
   is
     (G (SI_To_FP (IR_Builder, +V, Type_Of (GT), Name), GT,
         Overflowed => Overflowed (V)));

   ------------------
   -- Build_Select --
   ------------------

   function Build_Select
     (C_If, C_Then, C_Else : GL_Value; Name : String := "") return GL_Value
   is
      Result : GL_Value;

   begin
      --  If we know the direction this will go, we can constant fold

      if C_If = Const_True then
         return C_Then;
      elsif C_If = Const_False then
         return C_Else;

      --  Otherwise, form the result. The alignment can be the most
      --  conservative of the two, but we can only keep the TBAA
      --  information if it's the same on both arms.

      else
         Result := G_From (Build_Select (IR_Builder,
                                         C_If   => +C_If,
                                         C_Then => +C_Then,
                                         C_Else => +C_Else,
                                         Name   => Name),
                           C_Then);
         Set_Alignment (Result,
                        Nat'Min (Alignment (C_Then), Alignment (C_Else)));

         if TBAA_Type (C_Then) /= TBAA_Type (C_Else)
           or else TBAA_Offset (C_Then) /= TBAA_Offset (C_Else)
         then
            Set_TBAA_Type (Result, No_Metadata_T);
         end if;

         return Result;
      end if;
   end Build_Select;

   -------------------
   -- Build_Cond_Br --
   -------------------

   procedure Build_Cond_Br
     (C_If           : GL_Value;
      C_Then, C_Else : Basic_Block_T;
      Optimize       : Boolean := True) is
   begin
      if Optimize and then C_If = Const_True then
         Build_Br (C_Then);
      elsif Optimize and then C_If = Const_False then
         Build_Br (C_Else);
      else
         Discard (Build_Cond_Br (IR_Builder, +C_If, C_Then, C_Else));
      end if;
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

   ----------------
   -- Add_Clause --
   ----------------

   procedure Add_Clause (V, Exc : GL_Value) is
   begin
      Add_Clause (+V, +Exc);
   end Add_Clause;

   -----------------
   -- Set_Cleanup --
   -----------------

   procedure Set_Cleanup (V : GL_Value) is
   begin
      Set_Cleanup (+V, True);
   end Set_Cleanup;

   ---------------
   -- Build_Ret --
   ---------------

   procedure Build_Ret (V : GL_Value) is
   begin
      Discard (Build_Ret (IR_Builder, +V));
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

   -----------------------------
   -- Maybe_Build_Unreachable --
   -----------------------------

   procedure Maybe_Build_Unreachable is
   begin
      if not Are_In_Dead_Code then
         Build_Unreachable;
      end if;
   end Maybe_Build_Unreachable;

   ---------------
   -- Build_Phi --
   ---------------

   function Build_Phi
     (GL_Values : GL_Value_Array;
      BBs       : Basic_Block_Array;
      Name      : String := "") return GL_Value
   is
      First   : constant GL_Value := GL_Values (GL_Values'First);
      Offset  : constant ULL      := TBAA_Offset (First);
      TBAA    : Metadata_T        := TBAA_Type (First);
      Align   : Nat               := BPU;
      Our_Phi : Value_T;
      Result  : GL_Value;
      Values  : aliased Value_Array (GL_Values'Range);

   begin
      for J in Values'Range loop
         Values (J) := +GL_Values (J);
         Align := Nat'Min (Align, Alignment (GL_Values (J)));
      end loop;

      Our_Phi := Phi (IR_Builder, Type_Of (First), Name);
      Add_Incoming (Our_Phi, Values'Address, BBs'Address, Values'Length);
      Result := G_From (Our_Phi, First);
      Set_Alignment (Result, Align);

      --  If there's a TBAA type for the first operand and all the other
      --  operands have the TBAA types with a common parent (and are the same)
      --  and the offsets are the same, we can set the result to that type.
      --  Otherwise, we have to reinitialize.

      for V of GL_Values loop
         TBAA := (if   TBAA_Offset (V) = Offset
                  then Common_TBAA (TBAA, TBAA_Type (V))
                  else No_Metadata_T);
      end loop;

      if Present (TBAA) then
         Set_TBAA_Type (Result, TBAA);
      else
         Initialize_TBAA (Result);
      end if;

      return Result;
   end Build_Phi;

   ------------------
   -- Build_MemCpy --
   ------------------

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
      NoAlias     : Metadata_T := No_Metadata_T) is

   begin
      Discard (Build_MemCpy (IR_Builder, +Dst, unsigned (Dst_Align), +Src,
                             unsigned (Src_Align), +Size, Is_Volatile, TBAA,
                             TBAA_Struct, Scope, NoAlias));

   end Build_MemCpy;

   -------------------
   -- Build_MemMove --
   -------------------

   procedure Build_MemMove
     (Dst         : GL_Value;
      Dst_Align   : ULL;
      Src         : GL_Value;
      Src_Align   : ULL;
      Size        : GL_Value;
      Is_Volatile : Boolean;
      TBAA        : Metadata_T := No_Metadata_T;
      Scope       : Metadata_T := No_Metadata_T;
      NoAlias     : Metadata_T := No_Metadata_T) is
   begin
      Discard (Build_MemMove (IR_Builder, +Dst, unsigned (Dst_Align), +Src,
                              unsigned (Src_Align), +Size, Is_Volatile, TBAA,
                              Scope, NoAlias));
   end Build_MemMove;

   ------------------
   -- Build_MemSet --
   ------------------

   procedure Build_MemSet
     (Ptr         : GL_Value;
      Val         : GL_Value;
      Size        : GL_Value;
      Align       : ULL;
      Is_Volatile : Boolean;
      TBAA        : Metadata_T := No_Metadata_T;
      Scope       : Metadata_T := No_Metadata_T;
      NoAlias     : Metadata_T := No_Metadata_T) is
   begin
      Discard (Build_MemSet (IR_Builder, +Ptr, +Val, +Size, unsigned (Align),
                             Is_Volatile, TBAA, Scope, NoAlias));
   end Build_MemSet;

   ---------------------------
   -- Create_Lifetime_Start --
   ---------------------------

   procedure Create_Lifetime_Start (Ptr, Size : GL_Value) is
      Byte_Size : constant GL_Value :=
        Convert (To_Bytes (Size), Int_64_GL_Type);

   begin
      Discard (Create_Lifetime_Start (IR_Builder, +Ptr, +Byte_Size));
   end Create_Lifetime_Start;

   -------------------------
   -- Create_Lifetime_End --
   -------------------------

   procedure Create_Lifetime_End (Ptr, Size : GL_Value) is
      Byte_Size : constant GL_Value :=
        Convert (To_Bytes (Size), Int_64_GL_Type);

   begin
      Discard (Create_Lifetime_End (IR_Builder, +Ptr, +Byte_Size));
   end Create_Lifetime_End;

   ----------------------------
   -- Create_Invariant_Start --
   ----------------------------

   procedure Create_Invariant_Start
     (Ptr : GL_Value; Size : GL_Value := No_GL_Value)
   is
      Byte_Size : Value_T;

   begin
      --  If we don't have a 64-bit type, we can't make invariant lifetime
      --  calls, so do nothing in that case. Likewise if we're generating C.

      if No (Int_64_GL_Type) or else Emit_C then
         return;
      end if;

      Byte_Size := (if   Present (Size)
                    then +Convert (To_Bytes (Size), Int_64_GL_Type)
                    else No_Value_T);

      Discard (Create_Invariant_Start (IR_Builder, +Ptr, Byte_Size));
   end Create_Invariant_Start;

   ---------------------------
   -- Update_Offset_For_GEP --
   ---------------------------

   procedure Update_Offset_For_GEP
     (Result : in out GL_Value; Ptr : GL_Value)
   is
      Orig_Offset : ULL;
      New_Offset  : ULL;

   begin
      --  First see if there's no TBAA data for Result, if we can't
      --  compute an offset for it, or if the input's type is SSI_GL_Type,
      --  in which case we know nothing about this.

      if No (TBAA_Type (Result)) then
         return;
      elsif not Get_GEP_Constant_Offset (Result, New_Offset)
        or else Related_Type (Ptr) = SSI_GL_Type
      then
         Set_TBAA_Type (Result, No_Metadata_T);
         return;

      --  Otherwise, adjust Result's offset. We know that Result is a GEP,
      --  but it's possible that Ptr was also a GEP with the same input as
      --  Result and that Result is folded into a GEP with an additional
      --  operand. In that case, incrementing the TBAA offset of Ptr by the
      --  offset of Result is wrong since that would be counting the offset
      --  of Ptr's GEP twice.

      else
         Set_TBAA_Offset (Result, TBAA_Offset (Result) + New_Offset);

         if Get_Value_Kind (Ptr)
             in Constant_Expr_Value_Kind | Instruction_Value_Kind
           and then Get_Num_Operands (Ptr) >= 1
           and then Get_Operand (Result, 0) = Get_Operand (Ptr, 0)
         then
            if not Get_GEP_Constant_Offset (Ptr, Orig_Offset) then
               Set_TBAA_Type (Result, No_Metadata_T);
            else
               Set_TBAA_Offset (Result, TBAA_Offset (Result) - Orig_Offset);
            end if;
         end if;

         --  If the above set the offset to beyond the size of the object
         --  being referenced, we have an erroneous access and we must
         --  clear the TBAA information.

         if Present (GT_Size (Related_Type (Ptr)))
           and then TBAA_Offset (Result) * UBPU >=
                    +GT_Size (Related_Type (Ptr))
         then
            Set_TBAA_Type (Result, No_Metadata_T);
         end if;
      end if;
   end Update_Offset_For_GEP;

   ---------
   -- GEP --
   ---------

   function GEP
     (Bld     : Builder_T;
      T       : Type_T;
      Ptr     : Value_T;
      Indices : Value_Array;
      Name    : String := "") return Value_T
   is
     (GEP2 (Bld, T, Ptr, Indices'Address, Indices'Length, Name));

   ----------------------------------
   -- GEP_To_Relationship_Internal --
   ----------------------------------

   function GEP_To_Relationship_Internal
     (Ptr      : GL_Value;
      Ptr_T    : Type_T;
      GT       : GL_Type;
      R        : GL_Relationship;
      Val_Idxs : Value_Array;
      Name     : String) return GL_Value
   is
      Result : GL_Value := GM (In_Bounds_GEP2 (IR_Builder, Ptr_T,
                                               +Ptr, Val_Idxs'Address,
                                               Val_Idxs'Length, Name),
                               GT, R, Ptr);
   begin
      --  This may have been optimized so it's not a GEP any longer. If so,
      --  we don't need to adjust it.

      if Is_A_GEP (Result) then
         Set_Alignment (Result, Nat'Min (Alignment (Ptr),
                                         Get_GEP_Offset_Alignment (Result)));
         Update_Offset_For_GEP (Result, Ptr);
      end if;

      return Result;
   end GEP_To_Relationship_Internal;

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

   begin
      for J in Indices'Range loop
         Val_Idxs (J) := +Indices (J);
      end loop;

      return GEP_To_Relationship_Internal
        (Ptr, Element_Type_Of (Ptr), GT, R, Val_Idxs, Name);
   end GEP_To_Relationship;

   -------------------------
   -- GEP_To_Relationship --
   -------------------------

   function GEP_To_Relationship
     (GT      : GL_Type;
      R       : GL_Relationship;
      Ptr     : GL_Value;
      Ptr_T   : Type_T;
      Indices : GL_Value_Array;
      Name    : String := "") return GL_Value
   is
      Val_Idxs : aliased Value_Array (Indices'Range);

   begin
      for J in Indices'Range loop
         Val_Idxs (J) := +Indices (J);
      end loop;

      return GEP_To_Relationship_Internal (Ptr, Ptr_T, GT, R, Val_Idxs, Name);
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

   begin
      for J in Indices'Range loop
         Val_Idxs (J) := Const_Int (Int_32_T, ULL (Indices (J)), False);
      end loop;

      return GEP_To_Relationship_Internal
        (Ptr, Element_Type_Of (Ptr), GT, R, Val_Idxs, Name);
   end GEP_Idx_To_Relationship;

   ----------
   -- Load --
   ----------

   function Load (Ptr : GL_Value; Name : String := "") return GL_Value is
      New_R          : constant GL_Relationship := Deref (Relationship (Ptr));
      --  Get the resulting relation after the load

      Load_GT        : constant GL_Type         :=
        (if   Is_Data (Ptr) then Full_Designated_GL_Type (Ptr)
         else Related_Type (Ptr));
      --  If our input is data, the resulting type is the designated type
      --  of an access type. Otherwise, it's the same type.

      T              : constant Type_T          := Element_Type_Of (Ptr);
      --  The LLVM type that will be loaded by this instruction

      Result_Bits    : constant Nat             :=
        (if Is_Data (New_R) then Nat (Get_Scalar_Bit_Size (T)) else 0);
      --  Size in bits that will be loaded by this instruction

      Special_Atomic : constant Boolean         :=
        Is_Data (New_R) and then Is_Atomic (Ptr)
          and then not Atomic_Kind (T)
          and then Result_Bits /= 0
          and then Nat'(Get_Type_Alignment (Load_GT)) >= Result_Bits;
      --  True if this is an atomic reference that LLVM can't handle
      --  directly.

      Ptr_T          : constant Type_T         :=
        (if Special_Atomic then Int_Ty (Result_Bits) else T);
      --  Type that Ptr_Val will have

      Equiv_T : constant Type_T :=
        (if Special_Atomic then Pointer_Type (Ptr_T, Address_Space)
         else No_Type_T);
      --  Pointer to integer type with size matching that of the type
      --  to be loaded

      Ptr_Val        : Value_T                 :=
        (if   Special_Atomic then Pointer_Cast (IR_Builder, +Ptr, Equiv_T, "")
         else +Ptr);
      --  Address of item to load

      Load_Inst : Value_T;
      --  The actual load instruction

      Result    : GL_Value;
      --  Data to return

   begin
      --  If we're emitting C and this is a zero-sized load, the result is
      --  an undef. Likewise if the pointer is an undef (meaning it was
      --  zero-sized).

      if Emit_C
        and then ((Is_Zero_Size (Load_GT) and then New_R /= Bounds_And_Data)
                  or else Is_Undef (Ptr))
      then
         return Get_Undef_Relationship (Load_GT, New_R);

      --  If this needs a copy-in, make a temporary for it, copy the
      --  value into the temporary, and load the temporary.

      elsif Has_SM_Copy_From (Ptr) then
         declare
            T : constant Type_T := Element_Type_Of (Ptr);

         begin
            Result := G_From (Alloca (IR_Builder, T, ""), Ptr);
            Call_SM_Copy_From (Result, Ptr, To_Bytes (Get_Type_Size (T)));
            Ptr_Val := +Result;
         end;
      end if;

      --  Generate the load instruction and set up any flags. Don't set flags
      --  if the pointer is undefined (which includes poison).

      Load_Inst := Load_2 (IR_Builder, Ptr_T, Ptr_Val, Name);
      if not Is_Undef (Ptr) then
         Add_Flags_To_Instruction (Load_Inst, Ptr, Special_Atomic);
      end if;

      --  If this is the special atomic case, we need to allocate memory,
      --  store what we loaded into it, load it back again as the proper
      --  type, and return that value.

      if Special_Atomic then
         declare
            Memory     : constant GL_Value := Allocate_For_Type (Load_GT);
            Store_Inst : constant Value_T  :=
              Build_Store (IR_Builder, Load_Inst,
                           Pointer_Cast (IR_Builder, +Memory, Equiv_T, ""));
            Align      : constant unsigned :=
              unsigned (To_Bytes (Alignment (Ptr)));

         begin
            Load_Inst := Load_2 (IR_Builder, Type_Of (Load_GT), +Memory, "");
            Set_Alignment (Store_Inst, Align);
            Set_Alignment (Load_Inst, Align);
         end;
      end if;

      --  Build the result, with the proper GT and relationship

      Result := G (Load_Inst, Load_GT, New_R);
      Initialize_Alignment (Result);
      Initialize_TBAA      (Result);
      return Result;
   end Load;

   -----------
   -- Store --
   -----------

   procedure Store (Expr, Ptr : GL_Value) is
      GT             : constant GL_Type := Related_Type (Expr);
      T              : constant Type_T  := Type_Of (Expr);
      Result_Bits    : constant Nat     :=
        (if Is_Data (Expr) then Nat (Get_Scalar_Bit_Size (T)) else 0);
      Special_Atomic : constant Boolean :=
        Is_Data (Expr) and then Is_Atomic (Ptr) and then not Atomic_Kind (T)
          and then Nat'(Get_Type_Alignment (GT)) >= Result_Bits;
      Equiv_T        : constant Type_T  :=
        (if   Special_Atomic then Int_Ty (Result_Bits) else No_Type_T);
      Ptr_T          : constant Type_T  :=
        (if   Special_Atomic then Pointer_Type (Equiv_T, Address_Space)
         else No_Type_T);
      Ptr_Val        : constant Value_T :=
        (if   Special_Atomic then Pointer_Cast (IR_Builder, +Ptr, Ptr_T, "")
         else +Ptr);
      Val_To_Store   : Value_T          := +Expr;
      Store_Inst     : Value_T;
      Memory         : GL_Value;

   begin
      --  If Ptr has a non-default storage model, we have to generate
      --  a copy procedure.

      if Has_SM_Copy_To (Ptr) then
         Call_SM_Copy_To
           (Ptr, Expr,
            To_Bytes (Get_Type_Size (Element_Type_Of (Ptr))));
         return;

      --  If this is a special atomic store, allocate a temporary, store
      --  the data into it, then load that as the equivalent type and store
      --  into into the pointer-punned result.

      elsif Special_Atomic then
         Memory := Allocate_For_Type (GT);
         Discard (Build_Store (IR_Builder, Val_To_Store, +Memory));
         Val_To_Store := Load_2 (IR_Builder, Equiv_T,
                                 Pointer_Cast (IR_Builder, +Memory,
                                               Ptr_T, ""), "");
      end if;

      --  If we're emitting C and this is a zero-sized store do nothing.
      --  Likewise if the address is an object that was zero-sized and is
      --  now an undef.

      if Emit_C and then (Is_Zero_Size (Expr) or else Is_Undef (Ptr)) then
         return;

      --  Otherwise, do the actual store and set the attributes

      else
         Store_Inst := Build_Store (IR_Builder, Val_To_Store, Ptr_Val);
         Add_Flags_To_Instruction (Store_Inst, Ptr, Special_Atomic);
      end if;
   end Store;

   ----------------
   -- Atomic_RMW --
   ----------------

   function Atomic_RMW
     (Op            : Atomic_RMW_Bin_Op_T;
      Ptr           : GL_Value;
      V             : GL_Value;
      Order         : Atomic_Ordering_T :=
        Atomic_Ordering_Sequentially_Consistent;
      Single_Thread : Boolean := False) return GL_Value
   is
      Inst : constant Value_T :=
        Atomic_RMW (IR_Builder, Op, +Ptr, +V, Order, Single_Thread);

   begin
      Add_Aliasing_To_Instruction (Inst, Ptr);
      return G_From (Inst, V);
   end Atomic_RMW;

   -----------
   -- Fence --
   -----------

   procedure Fence
     (Order         : Atomic_Ordering_T :=
       Atomic_Ordering_Sequentially_Consistent;
      Single_Thread : Boolean           := False;
      Name          : String            := "")
   is
   begin
      Discard (Fence (IR_Builder, Order, Single_Thread, Name));
   end Fence;

   ---------------------
   -- Atomic_Cmp_Xchg --
   ---------------------

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
   is
      Inst : constant Value_T :=
        Atomic_Cmp_Xchg (IR_Builder, +Ptr, +Cmp, +C_New, Success_Ordering,
                         Failure_Ordering, Single_Thread);
   begin
      Add_Aliasing_To_Instruction (Inst, Ptr);
      if Weak then
         Set_Weak_For_Atomic_Xchg (Inst);
      end if;

      return G (Inst, Related_Type (Cmp), Boolean_And_Data);
   end Atomic_Cmp_Xchg;

   -------------------
   -- Call_Internal --
   ------------------

   function Call_Internal
     (Func : GL_Value;
      Fn_T : Type_T;
      Args : GL_Value_Array;
      Name : String := "") return Value_T
   is
      function To_Param_Num (Idx : Nat) return unsigned;
      --  Map index in Args array to LLVM parameter number

      LLVM_Func   : constant Value_T       := +Func;
      No_Raise    : constant Boolean       :=
        No_Exception_Propagation_Active
          or else (Is_A_Function (Func) and then Does_Not_Throw (Func));
      Lpad        : constant Basic_Block_T :=
        (if No_Raise then No_BB_T else Get_Landing_Pad);
      Arg_Values  : aliased Value_Array (Args'Range);
      Next_BB     : Basic_Block_T;
      Call_Inst   : Value_T;

      ------------------
      -- To_Param_Num --
      ------------------

      function To_Param_Num (Idx : Nat) return unsigned is
        (unsigned (Idx - Args'First));

   begin
      for J in Args'Range loop
         Arg_Values (J) := +Args (J);
      end loop;

      --  If we have a landing pad, use an invoke instruction, first creating
      --  the basic block to branch to in the normal case.

      if Present (Lpad) then
         Next_BB := Create_Basic_Block;
         Call_Inst := Invoke_2 (IR_Builder, Fn_T, LLVM_Func,
                                Arg_Values'Address, Arg_Values'Length,
                                Next_BB, Lpad, Name);
         Position_Builder_At_End (Next_BB);
      else
         Call_Inst := Call_2 (IR_Builder, Fn_T, LLVM_Func,
                              Arg_Values'Address, Arg_Values'Length, Name);
      end if;

      --   Set some parameter attributes based on the called function.
      --   It is peculiar that the two LLVM calls to set attributes of
      --   the call instruction have a different value for the first
      --   parameter.

      for J in Args'Range loop

         --  For each parameter that's a pointer, set the alignment.

         if Get_Type_Kind (Type_Of (Args (J))) = Pointer_Type_Kind then
            Set_Instr_Param_Alignment (Call_Inst, To_Param_Num (J) + 1,
                                       unsigned (To_Bytes (Alignment
                                                             (Args (J)))));
         end if;

         --  If this is a direct call and we have a parameter of the function
         --  that's an activation record, mark it in the call too.

         if (Is_A_Function (Func)
               and then not Restrictions_On_Target.Set
                              (No_Implicit_Dynamic_Code)
               and then Has_Nest_Attribute (LLVM_Func, To_Param_Num (J)))
           or else Relationship (Args (J)) = Reference_To_Activation_Record
         then
            Add_Nest_Attribute (Call_Inst, To_Param_Num (J));
         end if;
      end loop;

      return Call_Inst;
   end Call_Internal;

   ----------
   -- Call --
   ----------

   function Call
     (Func : GL_Value;
      Fn_T : Type_T;
      Args : GL_Value_Array;
      Name : String := "") return GL_Value
   is
     (Initialize_TBAA
        (Initialize_Alignment (G (Call_Internal (Func, Fn_T, Args, Name),
                                  Return_GL_Type (Func))),
         For_Aliased));

   ----------
   -- Call --
   ----------

   function Call
     (Func : GL_Value;
      Fn_T : Type_T;
      Args : GL_Value_Array;
      GT   : GL_Type;
      Name : String := "") return GL_Value
   is
     (Initialize_TBAA
        (Initialize_Alignment (G (Call_Internal (Func, Fn_T, Args, Name), GT)),
         For_Aliased));

   ----------
   -- Call --
   ----------

   function Call
     (Func : GL_Value;
      Args : GL_Value_Array;
      Name : String := "") return GL_Value
   is
     (Initialize_TBAA
        (Initialize_Alignment (G (Call_Internal
                                    (Func, Get_Function_Type (Func), Args,
                                     Name),
                                  Return_GL_Type (Func))),
         For_Aliased));

   --------------
   -- Call_Ref --
   --------------

   function Call_Ref
     (Func : GL_Value;
      Fn_T : Type_T;
      Args : GL_Value_Array;
      Name : String := "") return GL_Value
   is
     (Initialize_TBAA
        (Initialize_Alignment (G_Ref (Call_Internal (Func, Fn_T, Args, Name),
                                      Return_GL_Type (Func))),
         For_Aliased));

   -----------------------
   -- Call_Relationship --
   -----------------------

   function Call_Relationship
     (Func : GL_Value;
      Fn_T : Type_T;
      Args : GL_Value_Array;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
     (Initialize_TBAA
        (Initialize_Alignment (G (Call_Internal (Func, Fn_T, Args, Name),
                                  Return_GL_Type (Func), R)),
         For_Aliased));

   -----------------------
   -- Call_Relationship --
   -----------------------

   function Call_Relationship
     (Func : GL_Value;
      Args : GL_Value_Array;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
     (Initialize_TBAA
        (Initialize_Alignment (G (Call_Internal
                                    (Func, Get_Function_Type (Func),
                                     Args, Name),
                                  Return_GL_Type (Func), R)),
         For_Aliased));

   ----------
   -- Call --
   ----------

   procedure Call
     (Func : GL_Value; Args : GL_Value_Array; Name : String := "") is
   begin
      Discard (Call_Internal (Func, Get_Function_Type (Func), Args, Name));
   end Call;

   ----------
   -- Call --
   ----------

   procedure Call
     (Func : GL_Value;
      Fn_T : Type_T;
      Args : GL_Value_Array;
      Name : String := "") is
   begin
      Discard (Call_Internal (Func, Fn_T, Args, Name));
   end Call;

   ------------------
   -- Build_Resume --
   ------------------

   procedure Build_Resume (V : GL_Value) is
   begin
      Discard (Build_Resume (IR_Builder, +V));
   end Build_Resume;

   ----------------
   -- Inline_Asm --
   ----------------

   function Inline_Asm
     (Args           : GL_Value_Array;
      Output_Value   : Entity_Id;
      Template       : String;
      Constraints    : String;
      Fn_T           : out Type_T;
      Is_Volatile    : Boolean := False;
      Is_Stack_Align : Boolean := False) return GL_Value
   is
      GT        : GL_Type :=
        (if   Present (Output_Value)
         then Primitive_GL_Type (Full_Etype (Output_Value))
         else Void_GL_Type);
      T         : Type_T  :=
        (if Present (Output_Value) then Type_Of (GT) else Void_Type);
      Arg_Types : Type_Array (Args'Range);

   begin
      if Is_Record_Type (GT) then
         --  LLVM doesn't allow struct return types for inline assembly.
         --  Therefore, if GT is a record, we need to use the equivalent
         --  integer type for the return value, and then pointer-cast when
         --  storing the result.

         declare
            GT_Bits : constant ULL := Get_Scalar_Bit_Size (GT);
         begin
            if GT_Bits not in 32 | 64 then
               Error_Msg_N ("unsupported Asm output", Output_Value);
            end if;

            T  := Int_Ty (GT_Bits);
            GT :=
              (if GT_Bits = 32 then Int_32_GL_Type else Int_64_GL_Type);
         end;
      end if;

      for J in Args'Range loop
         Arg_Types (J) := Type_Of (Args (J));
      end loop;

      --  ??? Is the Relationship really right?
      Fn_T := Fn_Ty (Arg_Types, T);
      return G (Const_Inline_Asm (Fn_T, Template, Constraints, Is_Volatile,
                                  Is_Stack_Align),
                GT, Reference_To_Subprogram);
   end Inline_Asm;

   -------------------------
   -- Get_Pointer_Address --
   -------------------------

   function Get_Pointer_Address (Ptr : GL_Value) return GL_Value
   is (G (+Call (Get_Get_Address_Fn, (1 => Bit_Cast (Ptr, Void_Ptr_T))),
          Size_GL_Type));

   -------------------------
   -- Set_Pointer_Address --
   -------------------------

   function Set_Pointer_Address (Ptr, Addr : GL_Value) return GL_Value
   is (Bit_Cast
         (Call
            (Get_Set_Address_Fn,
             (1 => Bit_Cast (Ptr, Void_Ptr_T), 2 => Addr)),
          Ptr));

end GNATLLVM.Instructions;
