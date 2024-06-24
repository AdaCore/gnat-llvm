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

with Interfaces.C;            use Interfaces.C;

with Ada.Containers.Hashed_Maps;

--  This clause is only needed with old versions of GNAT
pragma Warnings (Off);
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
pragma Warnings (On);

with LLVM.Core; use LLVM.Core;

with Nlists;   use Nlists;
with Sem_Util; use Sem_Util;
with Set_Targ; use Set_Targ;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Table;

with GNATLLVM.Types;   use GNATLLVM.Types;
with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with CCG.Aggregates;  use CCG.Aggregates;
with CCG.Flow;        use CCG.Flow;
with CCG.Output;      use CCG.Output;
with CCG.Subprograms; use CCG.Subprograms;
with CCG.Target;      use CCG.Target;
with CCG.Utils;       use CCG.Utils;
with CCG.Write;       use CCG.Write;

package body CCG.Instructions is

   function Shift_Left (Value : ULL; Amount : Natural) return ULL
     with Import, Convention => Intrinsic;

   function Next_Pow2 (W : Nat) return Nat is
     ((case W is when 1  .. 8  => 8,  when  9 .. 16 => 16,
                 when 17 .. 32 => 32, when 33 .. 64 => 64,
                 when others   => (if   W < 128 then 128
                                      else Nat'Max (W, 256))));
   --  Return the lowest power of 2 higher or equal to W. For large values
   --  of W, it's safe for this to return an arbitrary value of at least W.

   function Get_Extra_Bits (J : Nat) return Nat;
   --  Return the number of bits needed to go from J or the bit width of T to
   --  the next power-of-two size in bits that's at least a byte wide.

   function Is_Comparison (V : Value_T) return Boolean
     with Pre => Present (V);
   --  Return True if V is known to be the result of a comparison or a
   --  logical operation on comparisons.

   procedure Alloca_Instruction (V, Op : Value_T)
     with Pre  => Is_A_Alloca_Inst (V) and then Present (Op);
   --  Return the value corresponding to a cast instruction

   function Deref_For_Load_Store (Op, V : Value_T) return Str
     with Pre  => (Is_A_Load_Inst (V) or else Is_A_Store_Inst (V))
                  and then Present (V),
          Post => Present (Deref_For_Load_Store'Result);
   --  Generate a dereference of Op in V, a load or store instruction,
   --  including a cast to a volatile pointer if necessary

   procedure Load_Instruction (V, Op : Value_T)
     with Pre  => Is_A_Load_Inst (V) and then Present (Op);
   --  Process a load instruction

   procedure Store_Instruction (V, Op1, Op2 : Value_T)
     with Pre  => Is_A_Store_Inst (V) and then Present (Op1)
                  and then Present (Op2);
   --  Process a store instruction

   function Select_Instruction (Op1, Op2, Op3 : Value_T) return Str
     with Pre  => Present (Op1) and then Present (Op2) and then Present (Op3),
          Post => Present (Select_Instruction'Result);
   --  Return the value corresponding to a "select" instruction

   function Binary_Instruction (V, Op1, Op2 : Value_T) return Str
     with Pre  => Acts_As_Instruction (V) and then Present (Op1)
                  and then Present (Op2),
          Post => Present (Binary_Instruction'Result);
   --  Return the value corresponding to a binary instruction

   function Cast_Instruction (V, Op : Value_T) return Str
     with Pre  => Acts_As_Instruction (V) and then Present (Op),
          Post => Present (Cast_Instruction'Result);
   --  Return the value corresponding to a cast instruction

   function Cmp_Instruction (V, Op1, Op2 : Value_T) return Str
     with Pre  => Get_Opcode (V) in Op_I_Cmp | Op_F_Cmp
                  and then Present (Op1) and then Present (Op2),
          Post => Present (Cmp_Instruction'Result);
   --  Return the value corresponding to a comparison instruction

   --  We need to record those values where we've made them equivalent to
   --  a C value but haven't written them yet because if we encounter a
   --  store or procedure call, we need to write them out since a variable
   --  may be changed by that store or procedure call. Here we store each
   --  such value when we make the assignment, but don't delete it when
   --  we've written it; we assume that the caller will check if it's been
   --  written. We record whether this is from a chain containing a load
   --  since we have cases where we only need to flush stores.

   type Pending_Value_Entry is record
      Value    : Value_T;
      Has_Load : Boolean;
   end record;

   package Pending_Values is new Table.Table
     (Table_Component_Type => Pending_Value_Entry,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 50,
      Table_Name           => "Pending_Values");

   package Pending_Values_Map_P is new Ada.Containers.Hashed_Maps
     (Key_Type        => Value_T,
      Element_Type    => Nat,
      Hash            => Hash_Value,
      Equivalent_Keys => "=");
   use Pending_Values_Map_P;

   Pending_Values_Map : Pending_Values_Map_P.Map;
   --  The table of pending values and a map that links a value to its
   --  entry in the table.

   procedure Add_Pending_Value (V : Value_T)
      with Pre => Present (V);
   --  Add a value to the pending value tables

   function Depends_On_Pending (V : Value_T) return Boolean
     with Pre => Present (V);
   --  True if V depends on a value in the pending value table

   procedure Remove_Pending_Value (V : Value_T)
     with Pre => Present (V);
   --  Remove V, which is known to be in the pending value table, and
   --  all uses which are.

   --  For annotations (pragma Annotate and Comment) that aren't at top
   --  level, we need to generate a builtin call (to llvm.ccg.annotate)
   --  that points to the string. We could pass the Str as an integer, but
   --  that involves potentially nonportable code, so it's simplest to make
   --  a table of these and pass the index in the table.

   package Annotations is new Table.Table
     (Table_Component_Type => Str,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 5,
      Table_Name           => "Annotations");

   --------------------
   -- Get_Extra_Bits --
   --------------------

   function Get_Extra_Bits (J : Nat) return Nat is
      type M is mod 2**16;
      Pow2_M_1 : M := M (J) - 1;

   begin
      --  We do this with bit-twiddling that turns on all bits that are
      --  one within Width - 1, use at least a value of BPU-1 so that we go
      --  to at least a byte, and then add the one back.

      Pow2_M_1 := Pow2_M_1 or Pow2_M_1 / 2;
      Pow2_M_1 := Pow2_M_1 or Pow2_M_1 / 4;
      Pow2_M_1 := M'Max (Pow2_M_1 or Pow2_M_1 / 16, M (BPU) - 1);

      return Nat (Pow2_M_1 - M (J) + 1);
   end Get_Extra_Bits;

   ---------------------
   -- Process_Operand --
   ---------------------

   function Process_Operand
     (V : Value_T; POO : Process_Operand_Option; P : Precedence) return Str
   is
      T      : constant Type_T := Type_Of (V);
      Size   : constant Nat    :=
        (if Is_Integral_Type (T) then Get_Scalar_Bit_Size (T) else BPU);
      Extras : constant Nat    := Get_Extra_Bits (Size);
      Result : Str             :=
        (case POO is when X            => V + P,
                     when POO_Signed   => V + Need_Signed + P,
                     when POO_Unsigned => V + Need_Unsigned + P);
      Use_Signed : constant Boolean :=
        (case POO is when X            => Is_Unsigned (V),
                     when POO_Signed   => True,
                     when POO_Unsigned => False);

   begin
      --  If all we have to do is deal with signedness, we're done. We also
      --  don't need to do anything if we have a constant.

      if Extras = 0 or else Is_A_Constant_Int (V) then
         return Result;

      --  A pair of shifts will always work and a decent optimizer will
      --  turn the pair of shifts into a AND in the unsigned case, but
      --  an AND operation is much easier to read, so we generate that.
      --  The size test should never be false, but we put it there just
      --  to be safe.

      elsif not Use_Signed and then Extras < ULL'Size - 1 then
         declare
            P2_Size : constant Nat     := Next_Pow2 (Get_Scalar_Bit_Size (T));
            Mask_T  : constant Type_T  := Int_Ty (P2_Size);
            Mask_W  : constant Natural := Integer (P2_Size - Extras);
            Mask    : constant ULL     := Shift_Left (1, Mask_W) - 1;
            Mask_V  : constant Value_T := Const_Int (Mask_T, Mask, False);

         begin
            return (Result + Bit & " & " & Mask_V) + P;
         end;

      --  Otherwise, we have to do a pair of shifts. Because of the C's
      --  integer promotion rules, we have to cast back to our type after
      --  each shift. We use the unsigned or signed version of the type of
      --  V depending on which we want or which we think it is if we don't
      --  care.

      else
         declare
            Cast : constant Str := "(" & (T + not Use_Signed) & ")";

         begin
            Result := Cast & "(" & (Result + Shift) & " << " & Extras & ")";
            return (Cast & "(" & Result + Shift & " >> " & Extras & ")") + P;
         end;
      end if;
   end Process_Operand;

   -------------------
   -- Is_Comparison --
   -------------------

   function Is_Comparison (V : Value_T) return Boolean is
   begin
      --  If this isn't a bit type, we know it isn't a comparison. If it's
      --  a simple constant, we know that it is.  If this isn't an
      --  instruction, we don't know that it's a comparison.

      if Type_Of (V) /= Bit_T then
         return False;
      elsif Is_Simple_Constant (V) then
         return True;
      elsif not Is_A_Instruction (V) then
         return False;
      end if;

      --  Otherwise our test is opcode-specific

      case Get_Opcode (V) is
         when Op_I_Cmp | Op_F_Cmp =>
            return True;

         when Op_And | Op_Or =>
            return Is_Comparison (Get_Operand0 (V))
              and then Is_Comparison (Get_Operand1 (V));

         when Op_Xor =>
            return Is_Comparison (Get_Operand0 (V))
              and then Is_A_Constant_Int (Get_Operand1 (V))
              and then Equals_Int (Get_Operand1 (V), 1);

         when Op_Select =>
            return Is_Comparison (Get_Operand1 (V))
              and then Is_Comparison (Get_Operand2 (V));

         when others =>
            return False;
      end case;
   end Is_Comparison;

   -----------------------
   -- Add_Pending_Value --
   -----------------------

   procedure Add_Pending_Value (V : Value_T) is
      Has_Load : Boolean := Is_A_Load_Inst (V);

   begin
      --  If this has operands, go through the operands to see if any
      --  are pending loads.

      if Has_Operands (V) then
         for J in Nat (0) .. Get_Num_Operands (V) - 1 loop
            declare
               Op : constant Value_T := Get_Operand (V, J);

            begin
               if Contains (Pending_Values_Map, Op)
                 and then Pending_Values.Table
                            (Element (Pending_Values_Map, Op)).Has_Load
               then
                  Has_Load := True;
               end if;
            end;
         end loop;
      end if;

      --  Finally add to both table and map

      Pending_Values.Append ((V, Has_Load));
      Insert (Pending_Values_Map, V, Pending_Values.Last);
   end Add_Pending_Value;

   ------------------------
   -- Depends_On_Pending --
   ------------------------

   function Depends_On_Pending (V : Value_T) return Boolean is
   begin
      if Has_Operands (V) then
         for J in Nat (0) .. Get_Num_Operands (V) - 1 loop
            if Contains (Pending_Values_Map, Get_Operand (V, J)) then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Depends_On_Pending;

   --------------------------
   -- Remove_Pending_Value --
   --------------------------

   procedure Remove_Pending_Value (V : Value_T) is
   begin
      Delete (Pending_Values_Map, V);

      if Has_Operands (V) then
         for J in Nat (0) .. Get_Num_Operands (V) - 1 loop
            if Contains (Pending_Values_Map, Get_Operand (V, J)) then
               Remove_Pending_Value (Get_Operand (V, J));
            end if;
         end loop;
      end if;
   end Remove_Pending_Value;

   ----------------------------
   -- Process_Pending_Values --
   ----------------------------

   procedure Process_Pending_Values (Calls_Only : Boolean := False) is
   begin
      --  We have a list of pending values, which represent LLVM
      --  instructions that are being stored as C expressions and not
      --  copied into declared variables. We want to do the stores for any
      --  "final" values. The only values that have been saved to this list
      --  are those that are used exactly once. That usage is either by a
      --  later entry in the list or by an instruction we haven't
      --  encountered yet. In the former case, we want to use it in the
      --  elaboration of that later list entry.
      --
      --  We work from the end of the list towards the front since we don't
      --  need to produce variables for expressions only used later. But
      --  because the only entries in the list are used exactly once, we
      --  know that we don't see the reference to the value as a variable
      --  in elaborating any other list entry. So we know that the values
      --  written out here are independent and thus the fact that we're
      --  writing them out "backwards" is fine.

      --  ??? This is quadratic in the number of pending values and they
      --  can accumulate between blocks. We need to find a better
      --  implementation.

      for J in reverse 1 .. Pending_Values.Last loop
         declare
            V : constant Value_T := Pending_Values.Table (J).Value;

         begin
            if not Get_Is_Used (V)
              and then (not Calls_Only
                        or else not Pending_Values.Table (J).Has_Load)
            then
               Remove_Pending_Value (V);
               Force_To_Variable (V);
            end if;
         end;
      end loop;

      --  If we're processing all of the pending values, clear them

      if not Calls_Only then
         Clear_Pending_Values;
      end if;

   end Process_Pending_Values;

   --------------------------
   -- Clear_Pending_Values --
   --------------------------

   procedure Clear_Pending_Values is
   begin
      Pending_Values.Set_Last (0);
      Clear (Pending_Values_Map);
   end Clear_Pending_Values;

   ------------------------
   -- Alloca_Instruction --
   ------------------------

   procedure Alloca_Instruction (V, Op : Value_T) is
   begin
      --  If this is in the entry block and we're allocating one of an
      --  object, this is a simple variable.

      if Is_Entry_Block (V) and then Is_A_Constant_Int (Op)
        and then Equals_Int (Op, 1)
      then
         Set_Is_LHS (V);
         Maybe_Decl (V);

      --  Otherwise, it's of variable size and we have to call alloca and
      --  set V to our result.

      else
         declare
            Size : constant Str :=
              "sizeof (" & Get_Allocated_Type (V) & ") * " & (Op + Mult);
            Call : constant Str :=
              "alloca (" & Size & ")" + Unknown + Component;

         begin
            Assignment (V, TP ("(#T1) ", V) & Call + Unary);
            Needs_Malloc_H := True;

            --  We can't support alloca in very early versions of C.

            if C_Version <= 1990 then
               Error_Msg
                 ("dynamic stack allocation not supported in C89/C90", V);
               Error_Msg
                 ("\\specify -c-target-version=C99 or later to support this",
                  V);
            end if;
         end;
      end if;
   end Alloca_Instruction;

   --------------------------
   -- Deref_For_Load_Store --
   --------------------------

   function Deref_For_Load_Store (Op, V : Value_T) return Str is
      T             : constant Type_T  := Get_Load_Store_Type (V);
      Need_Volatile : constant Boolean :=
        Get_Volatile (V) and then not Is_Ref_To_Volatile (Op);

   begin
      --  If this is a load or store of a partial integer, we need to
      --  cast to a pointer to a struct consisting of an int of that bitsize
      --  and reference the integer field. However, if the C compiler we're
      --  using doesn't support packing, this won't help, so don't worry
      --  about the out-of-bounds access. "Partial" here means that the LLVM
      --  IR is asking to load fewer bits than a normal hardware operation
      --  (which corresponds to a C type). So the first issue is when the
      --  IR is referencing a three-byte object (17 to 24 bits) and the
      --  next is above 32 but below 64-8.

      if not Pack_Not_Supported
        and then Is_Integral_Type (T)
        and then Get_Scalar_Bit_Size (T) in 17 .. 24 | 33 .. 56 | 65 .. 120
      then
         declare
            Bits   : constant Nat := Get_Scalar_Bit_Size (T);
            Result : Str :=
              "((struct ccg_i" & Bits & " *" &
              (if Need_Volatile then "volatile" else "") & ") " & Op & ")->f";

         begin
            Need_IXX_Struct (Bits);

            --  If this is larger than an int size record, some C compilers,
            --  such as GCC, will treat a subsequent operation, such as a
            --  shift, as being done in Bits, so cast to long long to
            --  prevent that odd behavior.

            if Is_A_Load_Inst (V) and then Bits > Int_Size then
               Result := "((long long) " & Result & ")";
            end if;

            return Result;
         end;

      --  If this isn't volatile, it's a normal dereference. Likewise if
      --  it's already known to be volatile. But if this is a null, we need
      --  to cast it to a volatile form of its type to avoid an error from
      --  most C compilers.

      elsif not Need_Volatile and then not Is_A_Constant_Pointer_Null (Op)
        and then not Is_Undef (Op)
      then
         return Deref (Op);

      --  Otherwise, cast to a volatile form of the type and dereference that

      else
         return Deref (TP ("(volatile #T1) #1", Op));
      end if;

   end Deref_For_Load_Store;

   ----------------------
   -- Load_Instruction --
   ----------------------

   procedure Load_Instruction (V, Op : Value_T) is
   begin
      --  ??? Need to deal with both unaligned load and unaligned store

      Process_Pending_Values (Calls_Only => True);
      Assignment (V, Deref_For_Load_Store (Op, V));
   end Load_Instruction;

   -----------------------
   -- Store_Instruction --
   -----------------------

   procedure Store_Instruction (V, Op1, Op2 : Value_T) is
      LHS : constant Str := Deref_For_Load_Store (Op2, V);
      RHS : constant Str := +Op1;

   begin
      Error_If_Cannot_Pack (Type_Of (Op1));
      Process_Pending_Values;
      Output_Copy (LHS, RHS, Type_Of (Op1), V => V);
   end Store_Instruction;

   ------------------------
   -- Select_Instruction --
   ------------------------

   function Select_Instruction (Op1, Op2, Op3 : Value_T) return Str is
   begin
      --  If either operand has side effects, force it to a variable so that
      --  we're sure that both are evaluated.

      if Has_Side_Effects (Op2) then
         Force_To_Variable (Op2);
      end if;

      if Has_Side_Effects (Op3) then
         Force_To_Variable (Op3);
      end if;

      --  If we have a pointer type, it's possible that the two pointers
      --  point to something that differs in signedness, so we need to cast
      --  both to the same signedness.
      --  ??? When we support opaque pointer and track what something
      --  points to, we'll be able to do better here.

      if Is_Pointer_Type (Op2) then
         return TP ("#1 ? (#T2) #2 : (#T2) #3", Op1, Op2, Op3) + Conditional;
      else
         return TP ("#1 ? #2 : #3", Op1, Op2, Op3) + Conditional;
      end if;
   end Select_Instruction;

   ------------------------
   -- Binary_Instruction --
   ------------------------

   function Binary_Instruction (V, Op1, Op2 : Value_T) return Str is
      Opc      : constant Opcode_T               := Get_Opcode (V);
      T        : constant Type_T                 := Type_Of (V);
      Can_Ovfl : constant Boolean                :=
        Is_A_Instruction (V) and then Opc in Op_Add | Op_Sub | Op_Mul
        and then not Has_NSW (V);
      POO      : constant Process_Operand_Option :=
        (case Opc is when Op_U_Div | Op_U_Rem | Op_L_Shr => POO_Unsigned,
                     when Op_S_Div | Op_S_Rem | Op_A_Shr => POO_Signed,
                     when others => (if Can_Ovfl then POO_Unsigned else X));
      --  If this operation is allowed to overflow, we must ensure that
      --  it's performed as unsigned, since unsigned overflow is defined.
      --  If we don't do this, some C compilers can make incorrect
      --  assumptions. After we do this, we have to convert back to the
      --  signed type and that conversion may produce an unrepresentable
      --  value, which is implementation-specified, but all compilers do
      --  the right thing as far as we know and there doesn't seem to be a
      --  better approach.

      Result : Str;

   begin
      case Opc is
         when Op_Add =>
            --  ??? J + -6 should be J - 6 and likewise for Op_Sub
            Result := Process_Operand (Op1, POO, Add) & " + " &
              Process_Operand (Op2, POO, Add) + Add;

         when Op_Sub =>
            Result := Process_Operand (Op1, POO, Add) & " - " &
              Process_Operand (Op2, POO, Add) + Add;

         when Op_Mul =>
            Result := Process_Operand (Op1, POO, Mult) & " * " &
              Process_Operand (Op2, POO, Mult) + Mult;

         when Op_S_Div | Op_U_Div =>
            return (Process_Operand (Op1, POO, Mult) & " / " &
                    Process_Operand (Op2, POO, Mult)) + Mult;

         when Op_S_Rem | Op_U_Rem =>
            return (Process_Operand (Op1, POO, Mult) & " % " &
                    Process_Operand (Op2, POO, Mult)) + Mult;

         when Op_Shl =>
            return TP ("#1 << #2", Op1, Op2) + Shift;

         when Op_L_Shr | Op_A_Shr =>
            return Process_Operand (Op1, POO, Shift) & " >> " & (Op2 + Shift);

         when Op_F_Add =>
            return TP ("#1 + #2", Op1, Op2) + Add;

         when Op_F_Sub =>
            return TP ("#1 - #2", Op1, Op2) + Add;

         when Op_F_Mul =>
            return TP ("#1 * #2", Op1, Op2) + Mult;

         when Op_F_Div =>
            return TP ("#1 / #2", Op1, Op2) + Mult;

         when Op_And =>

            --  If this is a bit operation and neither has side-effects, use
            --  && because this is clearer and more efficient in C.

            if T = Bit_T and then not Has_Side_Effects (Op1)
              and then not Has_Side_Effects (Op2)
            then
               return TP ("#1 && #2", Op1, Op2) + Logical_AND;
            else
               return TP ("#1 & #2", Op1, Op2) + Bit;
            end if;

         when Op_Or =>

            if T = Bit_T and then not Has_Side_Effects (Op1)
              and then not Has_Side_Effects (Op2)
            then
               return TP ("#1 || #2", Op1, Op2) + Logical_OR;
            else
               return TP ("#1 | #2", Op1, Op2) + Bit;
            end if;

         when Op_Xor =>

            if T = Bit_T and then Is_A_Constant_Int (Op2)
              and then Equals_Int (Op2, 1)
            then
               return TP ("! #1", Op1) + Unary;
            else
               return TP ("#1 ^ #2", Op1, Op2) + Bit;
            end if;

         when others =>
            return raise Program_Error;
      end case;

      --  If we fall through to here and have an instruction that can
      --  overflow but which wasn't supposed to be unsigned, cast the
      --  result to signed.

      if Can_Ovfl and then not Is_Unsigned (V) then
         Result := "(" & T & ") (" & Result & ")" + Unary;
      end if;

      return Result;

   end Binary_Instruction;

   ----------------------
   -- Cast_Instruction --
   ----------------------

   function Cast_Instruction (V, Op : Value_T) return Str is
      Opc       : constant Opcode_T := Get_Opcode (V);
      Src_T     : constant Type_T   := Type_Of (Op);
      Dest_T    : constant Type_T   := Type_Of (V);
      Our_Op    : constant Str      :=
        Process_Operand
        (Op, (case Opc is when Op_UI_To_FP | Op_Z_Ext => POO_Unsigned,
                          when Op_SI_To_FP | Op_S_Ext => POO_Signed,
                          when others                 => X),
         Unary);

   begin
      --  If we're doing a bitcast and the input and output types aren't
      --  both pointers, we need to do this by pointer-punning.

      if Opc = Op_Bit_Cast
        and then (not Is_Pointer_Type (Src_T)
                    or else not Is_Pointer_Type (Dest_T))
      then
         --  If our operand is an expression, we probably can't validly take
         --  its address, so be sure that we make an actual variable that
         --  we can take the address of.

         Force_To_Variable (Op);
         return TP ("*((#T2 *) #A1)", Op, V) + Unary;

         --  If we have a bitcast where both are pointers, we have a few cases
         --  to look at.

      elsif Opc = Op_Bit_Cast and then Is_Pointer_Type (Src_T)
        and then Is_Pointer_Type (Dest_T)
      then
         declare
            Safe_User : constant Value_T := Safe_Single_User (V);

         begin
            --  If our only user is another bitcast to a pointer type, this
            --  is a nop because multiple conversions between pointer types
            --  are equivalent to just the outer one.

            if Present (Safe_User) and then Is_A_Bit_Cast_Inst (Safe_User)
              and then Is_Pointer_Type (Safe_User)
              and then not Get_Is_LHS (Op)
            then
               return Our_Op;
            end if;
         end;

      --  If we're zero-extending a value that's known to be a comparison
      --  result, we do nothing since we know that the value is already
      --  either a zero or one.

      elsif Opc = Op_Z_Ext and then Is_Comparison (Op) then
         return +Op;

      --  Likewise if this is a truncation to a small integral type that's
      --  the same C type.

      elsif Opc = Op_Trunc
        and then Next_Pow2 (Get_Scalar_Bit_Size (Src_T)) =
                 Next_Pow2 (Get_Scalar_Bit_Size (Dest_T))
      then
         return +Op;
      end if;

      --  Otherwise, just do a cast. If we're considered volatile, make
      --  sure that's reflected in the cast we write.

      return ("(" & (V + Write_Type) &
              (if Is_Volatile (V) then " volatile) " else ") ") & Our_Op) +
              Unary;

   end Cast_Instruction;

   ---------------------
   -- Cmp_Instruction --
   ---------------------

   function Cmp_Instruction (V, Op1, Op2 : Value_T) return Str is
      Result : Str;

   begin
      --  This is either an integer or an FP comparison

      if Get_Opcode (V) = Op_I_Cmp then
         declare
            type I_Info is record
               Is_Unsigned : Boolean;
               Length      : Integer;
               Op          : String (1 .. 2);
            end record;
            type I_Info_Array is array (Int_Predicate_T range <>) of I_Info;
            Pred        : constant Int_Predicate_T        :=
              Get_I_Cmp_Predicate (V);
            Int_Info    : constant I_Info_Array           :=
              (Int_EQ  => (False, 2, "=="),
               Int_NE  => (False, 2, "!="),
               Int_UGT => (True,  1, "> "),
               Int_UGE => (True,  2, ">="),
               Int_ULT => (True,  1, "< "),
               Int_ULE => (True,  2, "<="),
               Int_SGT => (False, 1, "> "),
               Int_SGE => (False, 2, ">="),
               Int_SLT => (False, 1, "< "),
               Int_SLE => (False, 2, "<="));
            Info        : constant I_Info                 := Int_Info (Pred);
            Maybe_Uns   : constant Boolean                :=
              Is_Unsigned (Op1) or else Is_Unsigned (Op2);
            Do_Unsigned : constant Boolean                :=
              (if   Pred in Int_EQ | Int_NE then Maybe_Uns
               else Info.Is_Unsigned);
            X_Signed    : constant Boolean                :=
               Pred in Int_EQ | Int_NE
               and then Get_Scalar_Bit_Size (Op1) = Int_Size;
            POO         : constant Process_Operand_Option :=
              (if    X_Signed then X
               elsif Do_Unsigned then POO_Unsigned else POO_Signed);
            LHS         : constant Str                    :=
               Process_Operand (Op1, POO, Relation);
            RHS         : constant Str                    :=
               Process_Operand (Op2, POO, Relation);

         begin
            return (LHS & " " & Info.Op (1 .. Info.Length) & " " & RHS) +
                    Relation;
         end;

      --  If not integer comparison, it must be FP

      else
         case Get_F_Cmp_Predicate (V) is
            when Real_Predicate_True =>
               return +"1";
            when Real_Predicate_False =>
               return +"0";
            when Real_OEQ | Real_UEQ =>
               return TP ("#1 == #2", Op1, Op2) + Relation;
            when Real_OGT | Real_UGT =>
               return TP ("#1 > #2", Op1, Op2) + Relation;
            when Real_OGE | Real_UGE =>
               return TP ("#1 >= #2", Op1, Op2) + Relation;
            when Real_OLT | Real_ULT =>
               return TP ("#1 < #2", Op1, Op2) + Relation;
            when Real_OLE | Real_ULE =>
               return TP ("#1 <= #2", Op1, Op2) + Relation;
            when Real_ONE | Real_UNE =>
               return TP ("#1 != #2", Op1, Op2) + Relation;

            when Real_ORD =>

               --  This tests that neither input is a Nan, which means that
               --  both inputs are equal to themselves in C. We check if
               --  Op2 is a constant since it often is.

               Result := TP ("#1 == #1", Op1) + Relation;

               if not Is_A_Constant (Op2) then
                  Result :=
                    (Result & " && " & (TP ("#1 == #1", Op2) + Relation))
                    + Logical_AND;
               end if;

               return Result;

            when Real_UNO =>

               --  This is the opposite of ORD

               Result := TP ("#1 != #1", Op1) + Relation;

               if not Is_A_Constant (Op2) then
                  Result :=
                    (Result & " || " & (TP ("#1 != #1", Op2) + Relation))
                    + Logical_OR;
               end if;

               return Result;
         end case;
      end if;
   end Cmp_Instruction;

   -----------------
   -- Output_Copy --
   -----------------

   procedure Output_Copy (LHS : Value_T; RHS : Str; T : Type_T) is
   begin
      Output_Copy (+LHS, RHS, T, V => LHS);
   end Output_Copy;

   -----------------
   -- Output_Copy --
   -----------------

   procedure Output_Copy (LHS : Str; RHS : Value_T; T : Type_T) is
   begin
      Output_Copy (LHS, +RHS, T, V => RHS);
   end Output_Copy;

   -----------------
   -- Output_Copy --
   -----------------

   procedure Output_Copy (LHS, RHS : Value_T; T : Type_T) is
   begin
      Output_Copy (+LHS, +RHS, T, V => LHS);
   end Output_Copy;

   -----------------
   -- Output_Copy --
   -----------------

   procedure Output_Copy
     (LHS, RHS : Str; T : Type_T; V : Value_T := No_Value_T)
   is
   begin
      --  If this isn't an array type, write a normal assignment. Otherwise,
      --  use memmove.
      --  ??? We can usually use memcpy, but it's not clear what test to
      --  do here at the moment.

      if not Is_Array_Type (T) then
         Add_Line (LHS & " = " & RHS + Assign, V);

      --  If T is a zero-sized array, it means that we're not to move
      --  anything, but we make a one-element array for zero-length arrays,
      --  so taking sizeof the type is wrong. Also note that we represent
      --  the "value" of an array type by its address, so we don't have
      --  to explicitly take the address here and, in fact, doing that
      --  is wrong in some cases.

      elsif Get_Array_Length (T) /= Nat (0) then
         Add_Line ("memmove ((void *) " & (LHS + Unary) &
                     ", (void *) " & (RHS + Unary) &
                     ", sizeof (" & T & "))", V);
      end if;
   end Output_Copy;

   -----------------------
   -- Force_To_Variable --
   -----------------------

   procedure Force_To_Variable (V : Value_T) is
      C_Val : Str := Get_C_Value (V);

   begin
      if Present (C_Val) then

         --  We have to undo what was done to show that we don't need a
         --  variable for Op. Specifically, we have to clear its value,
         --  declare it, and copy the value to it.

         Set_C_Value (V, No_Str);

         --  If V is a LHS, it means that we're presenting the value as if it
         --  was the address. So take the address and clear the flag.

         if Get_Is_LHS (V) then
            C_Val := Addr_Of (C_Val);
            Set_Is_LHS (V, False);
         end if;

         --  Declare the variable and write the copy into it

         Maybe_Decl  (V);
         Output_Copy  (V, C_Val, Type_Of (V));
      end if;
   end Force_To_Variable;

   ----------------
   -- Assignment --
   ----------------

   procedure Assignment
     (LHS : Value_T; RHS : Str; Is_Opencode_Builtin : Boolean := False)
   is
   begin
      --  If LHS has no uses, see if it has side-effects. If so, force it
      --  out so that we evaluate the side-effects. If not, do nothing.

      if Num_Uses (LHS) = 0 then
         if Has_Side_Effects (LHS) then
            Maybe_Decl (LHS);
            Output_Copy (LHS, RHS, Type_Of (LHS));
         else
            return;
         end if;

      --  If LHS is a LHS, has more than one use in the IR, if we've
      --  already emitted a decl for it (e.g., it was defined in a block we
      --  haven't processed yet), if it's a source-level variable, or if
      --  it's a function call that returns an aggregate type (because we
      --  may try to take the address of it), generate an assignment
      --  statement into LHS. Otherwise, mark LHS as having value RHS. If
      --  LHS is a constant expression or of array types, never generate an
      --  assignment statement, the former because we may be at top level
      --  and the latter because C doesn't allow assignments of objects of
      --  aggregate type.
      --
      --  ??? We'd like to avoid the copy if we have a load of a variable
      --  that's used more than once, but that causes problems if it gets
      --  deferred until after a store into that variable. The handling of
      --  pending values can't help us here because it assumes  that all
      --  deferred values are used only once. It may be worth trying to do
      --  something better here because it generates a lot of extra variables
      --  in optimized code.

      elsif (Get_Is_LHS (LHS) or else Num_Uses (LHS) > 1
            or else Is_Variable (LHS) or else Get_Is_Decl_Output (LHS)
            or else (Is_A_Call_Inst (LHS) and then Is_Aggregate_Type (LHS)))
        and then not Is_A_Constant_Expr (LHS)
        and then not (Get_Is_LHS (LHS) and then Is_Array_Type (LHS))
      then
         Maybe_Decl (LHS);
         Output_Copy (LHS, RHS, Type_Of (LHS));
      else
         --  Make a note of the value of V. If V is an instruction, that
         --  has a potential side effect-such as call or load, make a
         --  note of this pending assignment in case we get a store or
         --  call. Also do this if it depends on a pending value.

         Set_C_Value (LHS, RHS);

         if (Is_A_Call_Inst (LHS) and then not Is_Opencode_Builtin)
           or else Is_A_Load_Inst (LHS)
           or else Depends_On_Pending (LHS)
         then
            Add_Pending_Value (LHS);
         end if;
      end if;
   end Assignment;

   ------------------
   --  Instruction --
   ------------------

   procedure Instruction (V : Value_T; Ops : Value_Array) is
      Op1 : constant Value_T  :=
        (if Ops'Length >= 1 then Ops (Ops'First) else No_Value_T);
      Op2 : constant Value_T  :=
        (if Ops'Length >= 2 then Ops (Ops'First + 1) else No_Value_T);
      Op3 : constant Value_T  :=
        (if Ops'Length >= 3 then Ops (Ops'First + 2) else No_Value_T);
      Opc : constant Opcode_T := Get_Opcode (V);

   begin
      --  First, make sure we've declared all operands

      for Op of Ops loop
         Maybe_Decl (Op);
      end loop;

      --  Handle the instruction according to its opcode

      case Opc is
         when Op_Call =>
            Call_Instruction (V, Ops);

         when Op_Alloca =>
            Alloca_Instruction (V, Op1);

         when Op_Load =>
            Load_Instruction (V, Op1);

         when Op_Store =>
            Store_Instruction (V, Op1, Op2);

         when Op_I_Cmp | Op_F_Cmp =>
            Assignment (V, Cmp_Instruction (V, Op1, Op2));

         when Op_Select =>
            Assignment (V, Select_Instruction (Op1, Op2, Op3));

         when Op_Add | Op_Sub | Op_Mul | Op_S_Div | Op_U_Div | Op_S_Rem
            | Op_U_Rem | Op_Shl | Op_L_Shr | Op_A_Shr | Op_F_Add | Op_F_Sub
            | Op_F_Mul | Op_F_Div | Op_And | Op_Or | Op_Xor =>
            Assignment (V, Binary_Instruction (V, Op1, Op2));

         when Op_F_Neg =>
            Assignment (V, TP (" -#1", Op1) + Unary);

         when Op_Trunc | Op_SI_To_FP | Op_FP_Trunc | Op_FP_Ext | Op_S_Ext
            | Op_UI_To_FP | Op_FP_To_SI | Op_FP_To_UI | Op_Z_Ext | Op_Bit_Cast
            | Op_Ptr_To_Int | Op_Int_To_Ptr =>
            Assignment (V, Cast_Instruction (V, Op1));

         when Op_Extract_Value =>
            Assignment (V, Extract_Value_Instruction (V, Op1));

         when Op_Insert_Value =>
            Insert_Value_Instruction (V, Op1, Op2);

         when Op_Get_Element_Ptr =>
            GEP_Instruction (V, Ops);

         when Op_Freeze =>
            Assignment (V, +Op1);

         when Op_Unreachable =>
            null;

         when others =>
            raise Program_Error;
      end case;
   end Instruction;

   -------------------------
   -- Process_Instruction --
   -------------------------

   procedure Process_Instruction (V : Value_T) is
      N_Ops : constant Nat := Get_Num_Operands (V);
      Ops   : Value_Array (1 .. N_Ops);

   begin
      for J in Ops'Range loop
         Ops (J) := Get_Operand (V, J - Ops'First);
      end loop;

      Instruction (V, Ops);
   end Process_Instruction;

   -----------------------
   -- Create_Annotation --
   ----------------------

   function Create_Annotation (N : N_Pragma_Id) return Nat is
      PAAs : constant List_Id := Pragma_Argument_Associations (N);
      S    : Str              := No_Str;

   begin
      if Get_Pragma_Id (N) in Pragma_Annotate | Pragma_GNAT_Annotate then
         pragma Assert (List_Length (PAAs) = 3);
         declare
            Arg1  : constant N_Pragma_Argument_Association_Id := First (PAAs);
            Arg2  : constant N_Pragma_Argument_Association_Id := Next (Arg1);
            Arg3  : constant N_Pragma_Argument_Association_Id := Next (Arg2);
            Expr1 : constant N_Subexpr_Id                     :=
              Expression (Arg1);
            Expr2 : constant N_Subexpr_Id                     :=
              Expression (Arg2);
            Expr3 : constant N_Subexpr_Id                     :=
              Expression (Arg3);

         begin
            --  The first operand must be an identifier named
            --  "ccg", the second must be an identifier, and the
            --  third must be a string literal.

            if Nkind (Expr1) = N_Identifier
              and then Get_Name_String (Chars (Expr1)) = "ccg"
              and then Nkind (Expr2) = N_Identifier
              and then Nkind (Expr3) = N_String_Literal
            then
               --  We support "c_pragma" and "verbatim"

               String_To_Name_Buffer (Strval (Expr3));

               if Get_Name_String (Chars (Expr2)) = "c_pragma" then
                  S := +"#pragma " & Name_Buffer (1 .. Name_Len);
               elsif Get_Name_String (Chars (Expr2)) = "verbatim" then
                  S := +Name_Buffer (1 .. Name_Len);
               end if;
            end if;
         end;
      else
         pragma Assert (Get_Pragma_Id (N) = Pragma_Comment
                        and then Is_Non_Empty_List (PAAs)
                        and then Nkind (Expression (First (PAAs))) =
                                 N_String_Literal);

         String_To_Name_Buffer (Strval (Expression (First (PAAs))));
         S := +"/* " & Name_Buffer (1 .. Name_Len) & " */";
      end if;

      --  Other than the normal hashing of strings, we make no attempt
      --  to try to detect duplicate annotations. Though it's possible
      --  there may be some, there aren't enough to justify the effort
      --  and the space utilization is very small.

      if Present (S) then
         Annotations.Append (S);
         return Annotations.Last;
      else
         return 0;
      end if;

   end Create_Annotation;

   -----------------------
   -- Output_Annotation --
   -----------------------

   procedure Output_Annotation (J : Nat; V : Value_T; Is_Global : Boolean) is
   begin
      pragma Assert (J > 0 and then J <= Annotations.Last);

      if Is_Global then
         Write_Str (Annotations.Table (J), Eol => True);
      else
         Add_Line (Annotations.Table (J), V,
                   Force_Left => Is_First_Char (Annotations.Table (J), '#'),
                   Semicolon  => False);
      end if;
   end Output_Annotation;

end CCG.Instructions;
