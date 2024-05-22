-----------------------------------------------------------------------------
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

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Einfo.Utils; use Einfo.Utils;
with Errout;      use Errout;
with Sem_Eval;    use Sem_Eval;
with Sem_Util;    use Sem_Util;
with Stand;       use Stand;

with GNATLLVM.Codegen;      use GNATLLVM.Codegen;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Conditionals; use GNATLLVM.Conditionals;
with GNATLLVM.Conversions;  use GNATLLVM.Conversions;
with GNATLLVM.Exprs;        use GNATLLVM.Exprs;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Subprograms;  use GNATLLVM.Subprograms;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Variables;    use GNATLLVM.Variables;

package body GNATLLVM.Builtins is

   function Name_To_RMW_Op
     (S           : String;
      Index       : Integer;
      End_Index   : out Integer;
      Op          : out Atomic_RMW_Bin_Op_T) return Boolean;
   --  See if the string S starting at position Index is the name of
   --  a supported LLVM atomicrmw instruction. If so, set End_Index
   --  to after the name and Op to the code for the operation and return True.

   function Last_Non_Suffix (S : String) return Integer;
   --  S is the name of some intrinsic function. Return the index of
   --  the last character of that name that doesn't include a "_N" suffix
   --  where N is a string of digits denoting a size and the '_' is optional.

   function Type_Size_Matches_Name
     (S        : String;
      In_Bytes : Boolean;
      GT       : GL_Type) return Boolean
     with Pre => Present (GT);
   --  Return True if the characteers at the end of string S
   --  represents an integer corresponding to the size of GT measured
   --  in bytes or bits, depending on In_Bytes. If it's in bits, a
   --  leading underscore isn't required.

   function Emit_Ptr (N : N_Subexpr_Id; GT : GL_Type) return GL_Value
     with Pre => Present (GT);
   --  If N is an Address or an access type pointing to GT, return a
   --  GL_Value that's a Reference to GT. If not, or if GT is not an
   --  elementary type, return No_GL_Value.

   function Emit_And_Convert (N : N_Subexpr_Id; GT : GL_Type) return GL_Value
     with Pre => Present (GT);
   --  If N's type is GT, return the evaluated expression. If not, return
   --  No_GL_Value.

   function Emit_And_Deref (N : N_Subexpr_Id; GT : GL_Type) return GL_Value
     with Pre => Present (GT);
   --  If N's type is an address or if it's an access type to GT, return
   --  the evaluated expression. If not, return No_GL_Value.

   function Memory_Order
     (N          : N_Subexpr_Id;
      No_Acquire : Boolean     := False;
      No_Release : Boolean := False) return Atomic_Ordering_T;
   --  N is an expression being passed as an operand for a memory order.
   --  Return the corresponding memory ordering for an instruction.

   function Emit_Fetch_And_Op
     (N       : N_Subexpr_Id;
      Val     : GL_Value;
      Op      : Atomic_RMW_Bin_Op_T;
      Op_Back : Boolean;
      Order   : Atomic_Ordering_T;
      S       : String;
      GT      : GL_Type) return GL_Value
     with Pre => Present (Val) and then Present (GT);
   --  Perform the Fetch_And_Op (Atomic_RMW) given by Op at the location
   --  N. If Op_Back is True, we want the new value, so we have to
   --  perform the operation on the result. Order is the memory ordering
   --  needed. S and GT are as for Type_Size_Matches_Name. If we can
   --  peform the operation, return the result. Otherwise, return
   --  No_GL_Value.

   function Emit_Atomic_Load
     (Ptr : GL_Value; Order : Atomic_Ordering_T; GT : GL_Type) return GL_Value
     with Pre  => Present (Ptr) and then Present (GT),
          Post => Present (Emit_Atomic_Load'Result);
   --  Emit an atomic load from Ptr with the specified memory order and
   --  result type.

   procedure Emit_Atomic_Store
     (Ptr, Val : GL_Value; Order : Atomic_Ordering_T; GT : GL_Type)
     with Pre => Present (Ptr) and then Present (Val) and then Present (GT);
   --  Emit an atomic Store of Val from to Ptr_Val with the specified
   --  memory order and data type.

   function Emit_Bswap_Call
     (N : N_Subprogram_Call_Id; S : String) return GL_Value;
   --  If N is a valid call to builtin_bswap, generate it

   function Emit_Frame_Address_Call
     (N : N_Subprogram_Call_Id) return GL_Value;
   --  If N is a valid call to __builtin_frame_address, generate it

   function Emit_FP_Builtin_Call
     (N : N_Subprogram_Call_Id; S : String) return GL_Value;
   --  If N is a valid call to a floating point builtin, generate it

   function Emit_FP_Isinf_Call
     (N : N_Subprogram_Call_Id; S : String) return GL_Value;
   --  If N is a valid call to a test for infinity builtin, generate it

   function Emit_Sync_Call
     (N : N_Subprogram_Call_Id; S : String) return GL_Value;
   --  If S is a valid __sync name for an instrinsic subprogram and
   --  the operands are value, emit it. Otherwise, return
   --  No_GL_Value.

   function Emit_Cheri_Call
     (N : N_Subprogram_Call_Id; S : String) return GL_Value;
   --  If S is a valid __builtin_cheri name for an instrinsic subprogram
   --  and the operands are valid, emit it. Otherwise, return No_GL_Value.

   function Emit_Branch_Prediction_Call
     (N : N_Subprogram_Call_Id; S : String) return GL_Value;
   --  Generate a call to the branch prediction function if the operands
   --  are the right type.

   function Emit_Atomic_Call
     (N : N_Subprogram_Call_Id; S : String) return GL_Value;
   --  Generate a call to the an __atomic builtin if valid

   function Get_Default_Alloc_Fn_Name return String is
     ((if   Emit_C and then not Use_GNAT_Allocs
       then "malloc" else "__gnat_malloc"));

   function Get_Default_Free_Fn_Name return String is
     ((if   Emit_C and then not Use_GNAT_Allocs
       then "free" else "__gnat_free"));

   Default_Alloc_Fn   : GL_Value := No_GL_Value;
   --  Default memory allocation function

   Default_Free_Fn    : GL_Value := No_GL_Value;
   --  Default memory deallocation function

   Memory_Compare_Fn  : GL_Value := No_GL_Value;
   --  Function to compare memory

   Stack_Save_Fn      : GL_Value := No_GL_Value;
   Stack_Restore_Fn   : GL_Value := No_GL_Value;
   --  Functions to save and restore the stack pointer

   Tramp_Init_Fn      : GL_Value := No_GL_Value;
   Tramp_Adjust_Fn    : GL_Value := No_GL_Value;
   --  Functions to initialize and adjust a trampoline

   Enable_Execute_Stack_Fn : GL_Value := No_GL_Value;
   --  Function to make a portion of the stack executable

   Expect_Fn          : GL_Value := No_GL_Value;
   --  Function to provide branch prediction information

   Frame_Address_Fn   : GL_Value := No_GL_Value;
   --  Function to provide the address of a stack frame

   Get_Address_Fn     : GL_Value := No_GL_Value;
   Set_Address_Fn     : GL_Value := No_GL_Value;
   --  Functions to get and set the address of a pointer (for tagged
   --  pointers).

   ---------------------
   -- Build_Intrinsic --
   ---------------------

   function Build_Intrinsic
     (Name             : String;
      Return_GT        : GL_Type;
      Overloaded_Types : Type_Array := (1 .. 0 => <>)) return GL_Value
   is
      Intrinsic_ID : constant unsigned :=
        Lookup_Intrinsic_ID (Name, Name'Length);
   begin
      if Intrinsic_ID = 0 then
         return No_GL_Value;
      end if;

      return
        G (Get_Intrinsic_Declaration
             (Module, Intrinsic_ID, Overloaded_Types'Address,
              Overloaded_Types'Length),
           Return_GT, Reference_To_Subprogram);
   end Build_Intrinsic;

   --------------------
   -- Name_To_RMW_Op --
   --------------------

   function Name_To_RMW_Op
     (S           : String;
      Index       : Integer;
      End_Index   : out Integer;
      Op          : out Atomic_RMW_Bin_Op_T) return Boolean
   is
      type RMW_Op is record
         Length : Integer;
         Name   : String (1 .. 4);
         Op     : Atomic_RMW_Bin_Op_T;
      end record;

      type RMW_Op_Array is array (Integer range <>) of RMW_Op;

      Len : Integer;
      Ops : constant RMW_Op_Array :=
        ((4, "xchg", Atomic_RMW_Bin_Op_Xchg),
         (3, "add ", Atomic_RMW_Bin_Op_Add),
         (3, "sub ", Atomic_RMW_Bin_Op_Sub),
         (3, "and ", Atomic_RMW_Bin_Op_And),
         (4, "nand", Atomic_RMW_Bin_Op_Nand),
         (2, "or  ", Atomic_RMW_Bin_Op_Or),
         (3, "xor ", Atomic_RMW_Bin_Op_Xor),
         (3, "max ", Atomic_RMW_Bin_Op_Max),
         (3, "min ", Atomic_RMW_Bin_Op_Min),
         (4, "umax", Atomic_RMW_Bin_Op_U_Max),
         (4, "umin", Atomic_RMW_Bin_Op_U_Min));

   begin
      for RMW of Ops loop
         Len := RMW.Length;

         if S'Last >= Index + Len - 1
           and then S (Index .. Index + Len - 1) = RMW.Name (1 .. Len)
         then
            End_Index := Index + Len;
            Op        := RMW.Op;
            return True;
         end if;
      end loop;

      End_Index := Integer'First;
      Op := Atomic_RMW_Bin_Op_T'First;
      return False;
   end Name_To_RMW_Op;

   ---------------------
   -- Last_Non_Suffix --
   ---------------------

   function Last_Non_Suffix (S : String) return Integer is
   begin
      --  If there's no digit at the end, return the highest index

      if not Is_Digit (S (S'Last)) then
         return S'Last;

      --  Otherwise, search for the last non-digit. We know that the
      --  last character is a digit.

      else
         return Idx : Integer := S'Last - 1 do
            while Idx >= S'First and then Is_Digit (S (Idx)) loop
               Idx := Idx - 1;
            end loop;

            --  If we have characters left, and we're preceeded by a '_',
            --  return the index of the character before that.
            --  the string doesn't look like "xxx_N", so return 'Last.

            if Idx >= S'First + 1 and then S (Idx) = '_' then
               Idx := Idx - 1;
            end if;
         end return;
      end if;
   end Last_Non_Suffix;

   ----------------------------
   -- Type_Size_Matches_Name --
   ----------------------------

   function Type_Size_Matches_Name
     (S        : String;
      In_Bytes : Boolean;
      GT       : GL_Type) return Boolean
   is
      GT_Size : constant Nat     := Nat (ULL'(Get_Type_Size (Type_Of (GT))));
      Size    : constant Integer :=
        Integer (GT_Size / (if In_Bytes then BPU else 1));
      Digs    : constant Integer :=
        (if Size >= 100 then 3 elsif Size >= 10 then 2 else 1);

   begin
      --  If the last characeter isn't a digit, then we always match since
      --  this is the generic form.

      if not Is_Digit (S (S'Last)) then
         return True;

      --  We don't match if the rest of the string isn't the right length
      --  and we also don't deal with three-digit sizes.

      elsif S'Length < Digs + 1 or else Digs > 2 then
         return False;

      --  If there's only one digit, see if it's the right one

      elsif Digs = 1 then
         return S (S'Last) = Character'Val (Character'Pos ('0') + Size)
           and then (not In_Bytes or else S (S'Last - 1) = '_');

      --  If there are two digits (the only remaining case), check both

      else
         return S (S'Last - 1) =
                  Character'Val (Character'Pos ('0') + Size / 10)
           and then S (S'Last) = Character'Val
                                      (Character'Pos ('0') + Size mod 10)
           and then (not In_Bytes or else S (S'Last - 2) = '_');
      end if;
   end Type_Size_Matches_Name;

   ------------------
   -- Memory_Order --
   ------------------

   function Memory_Order
     (N          : N_Subexpr_Id;
      No_Acquire : Boolean := False;
      No_Release : Boolean := False) return Atomic_Ordering_T
   is
      Val   : Uint;
      Order : Atomic_Ordering_T;

   begin
      if not Compile_Time_Known_Value (N) then
         return Atomic_Ordering_Sequentially_Consistent;
      end if;

      Val := Expr_Value (N);

      if Val = Uint_0 then
         return Atomic_Ordering_Unordered;
      elsif Val =  Uint_1 or else Val = Uint_2 then
         Order := Atomic_Ordering_Acquire;
      elsif Val = Uint_3 then
         Order := Atomic_Ordering_Release;
      elsif Val = Uint_4 then
         Order := Atomic_Ordering_Acquire_Release;
      else
         return Atomic_Ordering_Sequentially_Consistent;
      end if;

      --  Test to see if we have an ordering that's not supported by the
      --  operation.

      if (Order = Atomic_Ordering_Acquire_Release
            and then (No_Acquire or else No_Release))
        or else (Order = Atomic_Ordering_Acquire and then No_Acquire)
        or else (Order = Atomic_Ordering_Release and then No_Release)
      then
         Error_Msg_N ("invalid memory ordering for operation", N);
         Order := Atomic_Ordering_Sequentially_Consistent;
      end if;

      return Order;
   end Memory_Order;

   --------------
   -- Emit_Ptr --
   --------------

   function Emit_Ptr (N : N_Subexpr_Id; GT : GL_Type) return GL_Value is
      Val : GL_Value := Emit_Expression (N);

   begin
      if not Is_Elementary_Type (GT) then
         return No_GL_Value;

      --  If the pointer is derived from System.Address convert to GT

      elsif Is_Descendant_Of_Address (Val) then
         return Int_To_Ref (Val, GT);
      elsif Is_Access_Type (Val) then
         Val := From_Access (Val);
         return (if Related_Type (Val) = GT then Val else No_GL_Value);
      else
         return No_GL_Value;
      end if;
   end Emit_Ptr;

   ----------------------
   -- Emit_And_Convert --
   ----------------------

   function Emit_And_Convert
     (N : N_Subexpr_Id; GT : GL_Type) return GL_Value
   is
      Result : constant GL_Value := Emit_Expression (N);

   begin
      return
        (if   Full_Etype (Result) = Full_Etype (GT)
         then Convert_GT (Result, GT)
         else No_GL_Value);
   end Emit_And_Convert;

   --------------------
   -- Emit_And_Deref --
   --------------------

   function Emit_And_Deref
     (N : N_Subexpr_Id; GT : GL_Type) return GL_Value
   is
      Result : GL_Value := Emit_Expression (N);

   begin
      if Is_Address (Result) then
         return Get (Int_To_Ref (Result, GT), Data);
      elsif Is_Access_Type (Result) then
         Result := Get (From_Access (Result), Data);
         return (if Related_Type (Result) = GT then Result else No_GL_Value);
      else
         return No_GL_Value;
      end if;
   end Emit_And_Deref;

   -----------------------
   -- Emit_Fetch_And_Op --
   -----------------------

   function Emit_Fetch_And_Op
     (N       : N_Subexpr_Id;
      Val     : GL_Value;
      Op      : Atomic_RMW_Bin_Op_T;
      Op_Back : Boolean;
      Order   : Atomic_Ordering_T;
      S       : String;
      GT      : GL_Type) return GL_Value
   is
      Ptr_Val : GL_Value;
      Result  : GL_Value;

   begin
      --  Emit all operands and validate all our conditions

      Ptr_Val := Emit_Ptr (N, GT);

      if No (Ptr_Val) or else Type_Of (Val) /= Type_Of (GT)
        or else not Type_Size_Matches_Name (S, True, GT)
      then
         return No_GL_Value;
      end if;

      --  Now we can emit the operation. But we can't have unordered
      --  atomic RMW operations, so silently avoid that.

      Result := Atomic_RMW (Op, Ptr_Val, Val,
                            (if   Order = Atomic_Ordering_Unordered
                             then Atomic_Ordering_Sequentially_Consistent
                             else Order));
      Set_Volatile_For_Atomic (Result);

      --  If we want the value before the operation, we're done. Otherwise,
      --  we have to do the operation.

      if not Op_Back then
         return Result;
      end if;

      case Op is
         when Atomic_RMW_Bin_Op_Xchg =>
            return Result;

         when Atomic_RMW_Bin_Op_Add =>
            return Result + Val;

         when Atomic_RMW_Bin_Op_F_Add =>
            return F_Add (Result, Val);

         when Atomic_RMW_Bin_Op_F_Sub =>
            return F_Sub (Result, Val);

         when Atomic_RMW_Bin_Op_Sub =>
            return Result - Val;

         when Atomic_RMW_Bin_Op_And =>
            return Build_And (Result, Val);

         when Atomic_RMW_Bin_Op_Nand =>
            return Build_Not (Build_And (Result, Val));

         when Atomic_RMW_Bin_Op_Or =>
            return Build_Or (Result, Val);

         when Atomic_RMW_Bin_Op_Xor =>
            return Build_Xor (Result, Val);

         when Atomic_RMW_Bin_Op_Max
            | Atomic_RMW_Bin_Op_U_Max
            | Atomic_RMW_Bin_Op_F_Max
            =>
            return Build_Max (Result, Val);

         when Atomic_RMW_Bin_Op_Min
            | Atomic_RMW_Bin_Op_U_Min
            | Atomic_RMW_Bin_Op_F_Min
            =>
            return Build_Min (Result, Val);
      end case;
   end Emit_Fetch_And_Op;

   ----------------------
   -- Emit_Atomic_Load --
   ----------------------

   function Emit_Atomic_Load
     (Ptr : GL_Value; Order : Atomic_Ordering_T; GT : GL_Type) return GL_Value
   is
      Inst : constant Value_T :=
        Load_2 (IR_Builder, Element_Type_Of (Ptr), +Ptr, "");

   begin
      Set_Ordering  (Inst, Order);
      Set_Volatile  (Inst, True);
      Set_Alignment (Inst,
                     unsigned (Nat'(To_Bytes (Get_Type_Alignment (GT)))));

      return G (Inst, GT);
   end Emit_Atomic_Load;

   -----------------------
   -- Emit_Atomic_Store --
   -----------------------

   procedure Emit_Atomic_Store
     (Ptr, Val : GL_Value; Order : Atomic_Ordering_T; GT : GL_Type)
   is
      Inst : constant Value_T := Build_Store (IR_Builder, +Val, +Ptr);

   begin
      Set_Ordering (Inst, Order);
      Set_Volatile  (Inst, True);
      Set_Alignment
        (Inst, unsigned (Nat'(To_Bytes (Get_Type_Alignment (GT)))));
   end Emit_Atomic_Store;

   --------------------
   -- Emit_Sync_Call --
   --------------------

   function Emit_Sync_Call
     (N : N_Subprogram_Call_Id; S : String) return GL_Value
   is
      Ptr       : constant Opt_N_Subexpr_Id := First_Actual (N);
      N_Args    : constant Nat              := Num_Actuals (N);
      Val       : constant Opt_N_Subexpr_Id :=
        (if N_Args >= 2 then Next_Actual (Ptr) else Empty);
      GT        : constant GL_Type          :=
        (if    Present (Val) then Full_GL_Type (Val)
         elsif Present (Ptr) and then Is_Access_Type (Full_Etype (Ptr))
         then  Full_Designated_GL_Type (Full_Etype (Ptr)) else No_GL_Type);
      Ptr_Val   : constant GL_Value         :=
        (if Present (Ptr) then Emit_Ptr (Ptr, GT) else No_GL_Value);
      Order     : Atomic_Ordering_T         :=
        Atomic_Ordering_Sequentially_Consistent;
      Op_Name   : constant String           :=
        S (S'First .. Last_Non_Suffix (S));
      Op        : Atomic_RMW_Bin_Op_T;
      Op_Back   : Boolean;
      Index     : Integer;
      Result    : GL_Value;

   begin
      --  This may be a __sync builtin. Parse it to see what it tells us
      --  to do. If anything is wrong with the builtin or its operands,
      --  just return No_GL_Value and a normal call will result, which will
      --  produce a link error.
      --
      --  First handle the compare_and_swap variants.

      if Op_Name in "bool_compare_and_swap" | "val_compare_and_swap"
        and then N_Args = 3 and then Nkind (N) = N_Function_Call
      then
         declare
            New_Val : constant GL_Value := Emit_Expression (Next_Actual (Val));
            For_Val : constant Boolean  := Op_Name (Op_Name'First) = 'v';

         begin
            if Present (Ptr_Val) and then Type_Of (New_Val) = Type_Of (GT)
              and then Type_Size_Matches_Name (S, True, GT)
            then
               Result :=
                 Atomic_Cmp_Xchg (Ptr_Val, Emit_Expression (Val), New_Val);
               Set_Volatile_For_Atomic (Result);

               return Get (Result, (if For_Val then Data else Boolean_Data));
            else
               return No_GL_Value;
            end if;
         end;

      --  Now handle fence

      elsif Op_Name = "synchronize" and then N_Args = 0
        and then Nkind (N) = N_Procedure_Call_Statement
      then
         Fence;
         return Const_True;

      --  Handle __sync_release, which is an atomic write of zero

      elsif Op_Name = "lock_release" and then N_Args = 1
        and then Nkind (N) = N_Procedure_Call_Statement
        and then Present (GT)
        and then Type_Size_Matches_Name (S, True, GT)
      then
         Emit_Atomic_Store
           (Ptr_Val, Const_Null (GT), Atomic_Ordering_Release, GT);

         return Const_True;

      --  The remaining possibility is to have "Op_and_fetch",
      --  "fetch_and_Op", or "lock_test_and_set", all of which are
      --  fetch-and-ops.

      elsif Name_To_RMW_Op (Op_Name, Op_Name'First, Index, Op)
        and then Op_Name'Last = Index + 9
        and then Op_Name (Index .. Op_Name'Last) = "_and_fetch"
        and then N_Args = 2
      then
         Op_Back := True;
      elsif Op_Name'Last > Op_Name'First + 9
        and then Op_Name (Op_Name'First .. Op_Name'First + 9) = "fetch_and_"
        and then Name_To_RMW_Op (Op_Name, Op_Name'First + 10, Index, Op)
        and then Index - 1 = Op_Name'Last
        and then N_Args = 2
      then
         Op_Back := False;
      elsif Op_Name = "lock_test_and_set"
        and then Nkind (N) = N_Function_Call
        and then N_Args = 2
      then
         Op      := Atomic_RMW_Bin_Op_Xchg;
         Op_Back := False;
         Order   := Atomic_Ordering_Acquire;
      else
         return No_GL_Value;
      end if;

      return Emit_Fetch_And_Op (Ptr, Emit_Expression (Val), Op, Op_Back,
                                Order, S, GT);
   end Emit_Sync_Call;

   ---------------------
   -- Emit_Cheri_Call --
   ---------------------

   function Emit_Cheri_Call
     (N : N_Subprogram_Call_Id; S : String) return GL_Value
   is
      N_Args    : constant Nat              := Num_Actuals (N);
      Val       : constant Opt_N_Subexpr_Id := First_Actual (N);
      V         : GL_Value;

   begin
      if S = "address_get" and then N_Args = 1 then
         return Call (Get_Get_Address_Fn, (1 => Emit_Expression (Val)));

      elsif S = "address_set" and then N_Args = 2 then
         V := Emit_Expression (Val);
         return
           Call
             (Get_Set_Address_Fn,
              (1 => V, 2 => Emit_Expression (Next_Actual (Val))));

      elsif S = "base_get" and then N_Args = 1 then
         return
           Call
             (Build_Intrinsic
                ("llvm.cheri.cap.base.get", Size_GL_Type,
                 (1 => Size_T)),
              (1 => Emit_Expression (Val)));

      elsif S = "bounds_set" and then N_Args = 2 then
         V := Emit_Expression (Val);
         return
           Call
             (Build_Intrinsic
                ("llvm.cheri.cap.bounds.set", A_Char_GL_Type,
                 (1 => Size_T)),
              (1 => V, 2 => Emit_Expression (Next_Actual (Val))));

      elsif S = "bounds_set_exact" and then N_Args = 2 then
         V := Emit_Expression (Val);
         return
           Call
             (Build_Intrinsic
                ("llvm.cheri.cap.bounds.set.exact", A_Char_GL_Type,
                 (1 => Size_T)),
              (1 => V, 2 => Emit_Expression (Next_Actual (Val))));

      elsif S = "global_data_get" and then N_Args = 0 then
         return
           Call
             (Build_Intrinsic ("llvm.cheri.ddc.get", A_Char_GL_Type),
              (1 .. 0 => <>));

      elsif S = "perms_and" and then N_Args = 2 then
         V := Emit_Expression (Val);
         return
           Call
             (Build_Intrinsic
                ("llvm.cheri.cap.perms.and", A_Char_GL_Type,
                 (1 => Size_T)),
              (1 => V, 2 => Emit_Expression (Next_Actual (Val))));

      elsif S = "perms_get" and then N_Args = 1 then
         return
           Call
             (Build_Intrinsic
                ("llvm.cheri.cap.perms.get", Size_GL_Type,
                 (1 => Size_T)),
              (1 => Emit_Expression (Val)));

      elsif S = "program_counter_get" and then N_Args = 0 then
         return
           Call
             (Build_Intrinsic ("llvm.cheri.pcc.get", A_Char_GL_Type),
              (1 .. 0 => <>));

      elsif S = "representable_alignment_mask" and then N_Args = 1 then
         return
           Call
             (Build_Intrinsic
                ("llvm.cheri.representable.alignment.mask", Size_GL_Type,
                 (1 => Size_T)),
              (1 => Emit_Expression (Val)));

      elsif S = "round_representable_length" and then N_Args = 1 then
         return
           Call
             (Build_Intrinsic
                ("llvm.cheri.round.representable.length", Size_GL_Type,
                 (1 => Size_T)),
              (1 => Emit_Expression (Val)));

      elsif S = "seal_entry" and then N_Args = 1 then
         return
           Call
             (Build_Intrinsic
                ("llvm.cheri.cap.seal.entry", A_Char_GL_Type),
              (1 => Emit_Expression (Val)));

      elsif S = "sealed_get" and then N_Args = 1 then
         return
           Call_Relationship
             (Build_Intrinsic
                ("llvm.cheri.cap.sealed.get", Boolean_GL_Type),
              (1 => Emit_Expression (Val)), Boolean_Data);

      elsif S = "tag_get" and then N_Args = 1 then
         return
           Call_Relationship
             (Build_Intrinsic
                ("llvm.cheri.cap.tag.get", Boolean_GL_Type),
              (1 => Emit_Expression (Val)), Boolean_Data);

      elsif S = "type_get" and then N_Args = 1 then
         return
           Call
             (Build_Intrinsic
                ("llvm.cheri.cap.type.get", Size_GL_Type,
                 (1 => Size_T)),
              (1 => Emit_Expression (Val)));

      elsif S = "unseal" and then N_Args = 2 then
         V := Emit_Expression (Val);
         return
           Call
             (Build_Intrinsic
                ("llvm.cheri.cap.unseal", A_Char_GL_Type),
              (1 => V, 2 => Emit_Expression (Next_Actual (Val))));
      end if;

      return No_GL_Value;
   end Emit_Cheri_Call;

   ---------------------
   -- Emit_Bswap_Call --
   ---------------------

   function Emit_Bswap_Call
     (N : N_Subprogram_Call_Id; S : String) return GL_Value
   is
      Val       : constant Opt_N_Subexpr_Id := First_Actual (N);
      GT        : GL_Type;

   begin
      --  This is supposedly a __builtin_bswap builtin. Verify that it is.
      --  There must be exactly one actual.

      if No (Val) or else Present (Next_Actual (Val)) then
         return No_GL_Value;
      end if;

      GT := Full_GL_Type (Val);

      if not Is_Elementary_Type (GT)
        or else not Type_Size_Matches_Name (S, False, GT)
      then
         return No_GL_Value;
      end if;

      --  Otherwise, emit the intrinsic

      return Call (Build_Intrinsic ("llvm.bswap", GT, (1 => Type_Of (GT))),
                   (1 => Emit_Expression (Val)));
   end Emit_Bswap_Call;

   function Emit_Frame_Address_Call
     (N : N_Subprogram_Call_Id) return GL_Value
   is
      Val : constant Opt_N_Subexpr_Id := First_Actual (N);

   begin
      --  Verify that the types and number of arguments are correct

      if No (Val) or else Present (Next_Actual (Val))
        or else Nkind (Val) /= N_Integer_Literal
        or else not Is_Descendant_Of_Address (Full_Etype (N))
      then
         return No_GL_Value;
      end if;

      return Call (Get_Frame_Address_Fn, (1 => Emit_Expression (Val)));
   end Emit_Frame_Address_Call;

   ------------------------------
   -- Emit_Branch_Predict_Call --
   ------------------------------

   function Emit_Branch_Prediction_Call
     (N : N_Subprogram_Call_Id; S : String) return GL_Value
   is
      Val      : constant Opt_N_Subexpr_Id := First_Actual (N);
      Two_Arg  : constant Boolean          := S = "__builtin_expect";
      Expected : GL_Value;

   begin
      --  Verify that the types and number of arguments are correct

      if Nkind (N) /= N_Function_Call or else No (Val)
        or else Full_Etype (Val) /= Standard_Boolean
        or else Full_Etype (N) /= Standard_Boolean
        or else (Two_Arg
                   and then (No (Next_Actual (Val))
                               or else Full_Etype (Next_Actual (Val)) /=
                                         Standard_Boolean))
      then
         return No_GL_Value;

      --  If this is a two-arg form, set the expected value from the name,
      --  and evaluate it otherwise.

      elsif Two_Arg then
         Expected := Get (Emit (Next_Actual (Val)), Boolean_Data);
      else
         Expected
           := (if S = "__builtin_likely" then Const_True else Const_False);
      end if;

      --  Now emit the call and return the result

      return Call_Relationship (Get_Expect_Fn,
                                (1 => Get (Emit (Val), Boolean_Data),
                                 2 => Expected),
                                Boolean_Data);
   end Emit_Branch_Prediction_Call;

   ----------------------
   -- Emit_Atomic_Call --
   ----------------------

   function Emit_Atomic_Call
     (N : N_Subprogram_Call_Id; S : String) return GL_Value
   is
      Ptr       : constant Opt_N_Subexpr_Id := First_Actual (N);
      N_Args    : constant Nat              := Num_Actuals (N);
      Is_Proc   : constant Boolean          :=
        Nkind (N) = N_Procedure_Call_Statement;
      Arg2      : constant Opt_N_Subexpr_Id :=
        (if N_Args >= 2 then Next_Actual (Ptr) else Empty);
      Arg3      : constant Opt_N_Subexpr_Id :=
        (if N_Args >= 3 then Next_Actual (Arg2) else Empty);
      Arg4      : constant Opt_N_Subexpr_Id :=
        (if N_Args >= 4 then Next_Actual (Arg3) else Empty);
      Arg5      : constant Opt_N_Subexpr_Id :=
        (if N_Args >= 5 then Next_Actual (Arg4) else Empty);
      Arg6      : constant Opt_N_Subexpr_Id :=
        (if N_Args >= 6 then Next_Actual (Arg5) else Empty);
      GT        : constant GL_Type          :=
        (if    Present (Ptr) and then Is_Access_Type (Full_Etype (Ptr))
         then  Full_Designated_GL_Type (Full_Etype (Ptr))
         elsif N_Args > 2
               and then not Is_Access_Type (Full_Etype (Arg2))
               and then not Is_Descendant_Of_Address (Full_Etype (Arg2))
         then  Full_GL_Type (Arg2)
         elsif N_Args > 3 then Full_GL_Type (Arg3)
         elsif not Is_Proc then Full_GL_Type (N) else No_GL_Type);
      --  The above is a bit of a kludge to get the "operation" GL_Type
      --  given all the possible variants of atomic operations. The last
      --  argument is always an ordering type, so can't determine the
      --  needed type.

      Def_GT    : constant GL_Type          :=
        (if Present (GT) then GT else SSI_GL_Type);
      --  In some cases (test_and_set and clear), we may not have a type at
      --  all. In that case, we mean to use a byte.

      Op_Name   : constant String           :=
        S (S'First .. Last_Non_Suffix (S));
      --  The operation name without its concrete size suffix

      Integral  : constant Boolean          :=
        Last_Non_Suffix (S) /= S'Last
        or else Ends_With (S, "_n")
        or else Ends_With (S, "_capability");
      --  The atomic builtins for integer types (whose names typically end
      --  with "_N" for some size value N, or literal "_n") take a value
      --  parameter, whereas the generic versions (without suffix) take a
      --  pointer.

      Op        : Atomic_RMW_Bin_Op_T;
      Op_Back   : Boolean;
      Index     : Integer;
      Result    : GL_Value;

   begin
      --  First check for the function form of load

      if not Is_Proc and then N_Args = 2
        and then Op_Name in "load" | "load_n"
        and then Type_Size_Matches_Name (S, True, GT)
        and then Base_GL_Type (Full_GL_Type (N)) = Base_GL_Type (GT)
      then
         return Emit_Atomic_Load
           (Emit_Ptr (Ptr, GT),
            Memory_Order (Next_Actual (Ptr), No_Release => True), GT);

      --  Next check for the procedural form of load

      elsif Is_Proc and then N_Args = 3 and then Op_Name = "load"
        and then Is_Access_Type (Full_Etype (Next_Actual (Ptr)))
        and then Full_Designated_GL_Type (Full_Etype (Arg2)) = GT
        and then Type_Size_Matches_Name (S, True, GT)
      then
         Result := Emit_Atomic_Load
           (Emit_Ptr (Ptr, GT), Memory_Order (Arg3, No_Release => True), GT);
         Store (Result, Emit_Ptr (Arg2, GT));
         return Const_True;

      --  Check for store

      elsif Is_Proc and then N_Args = 3
        and then Op_Name in "store" | "store_n"
        and then Type_Size_Matches_Name (S, True, GT)
      then
         Result :=
           (if   Integral
            then Emit_And_Convert (Next_Actual (Ptr), GT)
            else Emit_And_Deref (Next_Actual (Ptr), GT));

         if No (Result) then
            return No_GL_Value;
         else
            Emit_Atomic_Store (Emit_Ptr (Ptr, GT), Result,
                               Memory_Order (Arg3, No_Acquire => True),
                               GT);

            return Const_True;
         end if;

      --  Handle exchange, which is a fetch-and operation

      elsif not Is_Proc and then N_Args = 3
        and then Base_GL_Type (Full_GL_Type (N)) = Base_GL_Type (GT)
        and then Op_Name in "exchange" | "exchange_n"
        and then Type_Size_Matches_Name (S, True, GT)
      then
         return
           Emit_Fetch_And_Op (Ptr, Emit_Expression (Arg2),
                              Atomic_RMW_Bin_Op_Xchg, False,
                              Memory_Order (Arg3), S, GT);
      elsif Is_Proc and then N_Args = 4 and then Op_Name = "exchange"
        and then Base_GL_Type (Full_Designated_GL_Type (Full_Etype (Arg3))) =
                 Base_GL_Type (GT)
      then
         Result := Emit_And_Deref (Arg2, GT);

         if Present (Result) then
            Result := Emit_Fetch_And_Op (Ptr, Result,
                                         Atomic_RMW_Bin_Op_Xchg, False,
                                         Memory_Order (Arg4), S, GT);
         end if;

         if No (Result) then
            return No_GL_Value;
         else
            Store (Result, Emit_Ptr (Arg3, GT));
            return Const_True;
         end if;

      --  Next is compare-exchange

      elsif not Is_Proc and then N_Args = 6
        and then Op_Name in "compare_exchange" |
                            "compare_exchange_n" |
                            "compare_exchange_capability"
        and then Is_Boolean_Type (Full_Etype (N))
        and then Is_Boolean_Type (Full_Etype (Arg4))
        and then Base_GL_Type (Full_GL_Type (Arg3)) = Base_GL_Type (GT)
        and then Type_Size_Matches_Name (S, True, GT)
      then
         declare
            Weak         : constant Boolean           :=
              Compile_Time_Known_Value (Arg4)
              and then Expr_Value (Arg4) = Uint_1;
            Old_Val      : constant GL_Value          :=
              Emit_And_Deref (Arg2, GT);
            New_Val      : constant GL_Value          :=
              (if   Integral
               then Emit_And_Convert (Arg3, GT)
               else Emit_And_Deref (Arg3, GT));
            Old_As_Ptr   : constant GL_Value          := Emit_Ptr (Arg2, GT);
            Orig_S_Order : constant Atomic_Ordering_T := Memory_Order (Arg5);
            Orig_F_Order : constant Atomic_Ordering_T :=
              Memory_Order (Arg6, No_Release => True);
            S_Order      : constant Atomic_Ordering_T :=
              (if   Orig_S_Order = Atomic_Ordering_Unordered
                   or else Orig_F_Order = Atomic_Ordering_Unordered
               then Atomic_Ordering_Sequentially_Consistent else Orig_S_Order);
            F_Order      : Atomic_Ordering_T          :=
             (if   Orig_F_Order = Atomic_Ordering_Unordered
              then Atomic_Ordering_Sequentially_Consistent else Orig_F_Order);

         begin
            if No (Old_Val) or else No (New_Val) then
               return No_GL_Value;
            elsif F_Order > S_Order then
               Error_Msg_N
                 ("failure order cannot be stronger than success order", N);
               F_Order := S_Order;
            elsif S = "compare_exchange_capability"
              and then ABI.all /= "purecap"
            then
               Error_Msg_N
                 ("__atomic_compare_exchange_capability is only valid " &
                  "with the Morello purecap ABI",
                  N);
            end if;

            --  Now do the operation. Return the result as a boolean, but
            --  if "expected" was a pointer (or address), set it to the old
            --  value.

            Result := Atomic_Cmp_Xchg (Emit_Ptr (Ptr, GT), Old_Val, New_Val,
                                       S_Order, F_Order, Weak => Weak);
            if Present (Old_As_Ptr) then
               Store (Get (Result, Data), Old_As_Ptr);
            end if;

            return Get (Result, Boolean_Data);
         end;

      --  Now test-and-set, which is an exchange

      elsif not Is_Proc and then N_Args = 2
        and then Op_Name = "test_and_set"
        and then Is_Boolean_Type (Full_Etype (N))
      then
         Result := Emit_Fetch_And_Op (Ptr, Const_Int (Def_GT, Uint_1),
                                      Atomic_RMW_Bin_Op_Xchg, False,
                                      Memory_Order (Arg2), S, Def_GT);

         if No (Result) then
            return No_GL_Value;
         else
            return I_Cmp (Int_NE, Result, Const_Null (Def_GT));
         end if;

      --  Next is clear, which is a store of zero

      elsif Is_Proc and then N_Args = 2 and then Op_Name = "clear" then
         Emit_Atomic_Store (Emit_Ptr (Ptr, Def_GT), Const_Null (Def_GT),
                            Memory_Order (Arg2, No_Acquire => True),
                            Def_GT);
         return Const_True;

      --  Now we have the fence operations, which we treat the same

      elsif Is_Proc and then N_Args = 1
        and then (S = "thread_fence" or else S = "signal_fence")
      then
         Fence (Order => Memory_Order (Ptr));
         return Const_True;

      --  Handle always_lock_free and is_lock_free

      elsif not Is_Proc and then N_Args in 1 .. 2
        and then Compile_Time_Known_Value (Ptr)
        and then (S = "always_lock_free" or else S = "is_lock_free")
      then
         return (if   Expr_Value (Ptr) = Uint_1
                      or else Expr_Value (Ptr) = Uint_2
                      or else Expr_Value (Ptr) = Uint_4
                      or else Expr_Value (Ptr) = Uint_8
                 then Const_True else Const_False);

      --  The remaining possibilities are "op_fetch" and "fetch_op"

      elsif Name_To_RMW_Op (Op_Name, Op_Name'First, Index, Op)
        and then Op_Name'Last = Index + 5
        and then Op_Name (Index .. Op_Name'Last) = "_fetch"
        and then Nkind (N) = N_Function_Call
        and then N_Args = 3
      then
         Op_Back := True;
      elsif Op_Name'Last > Op_Name'First + 5
        and then Op_Name (Op_Name'First .. Op_Name'First + 5) = "fetch_"
        and then Name_To_RMW_Op (Op_Name, Op_Name'First + 6, Index, Op)
        and then Index - 1 = Op_Name'Last
        and then Nkind (N) = N_Function_Call and then N_Args = 3
      then
         Op_Back := False;
      else
         return No_GL_Value;
      end if;

      return Emit_Fetch_And_Op (Ptr, Emit_Expression (Arg2), Op, Op_Back,
                                Memory_Order (Arg3), S, GT);
   end Emit_Atomic_Call;

   --------------------------
   -- Emit_FP_Builtin_Call --
   --------------------------

   function Emit_FP_Builtin_Call
     (N : N_Subprogram_Call_Id; S : String) return GL_Value
   is
      type FP_Builtin is record
         Length : Integer;
         Name   : String (1 .. 5);
         Kind   : Overloaded_Intrinsic_Kind;
      end record;

      type FP_Builtin_Array is array (Integer range <>) of FP_Builtin;

      FP_Builtins : constant FP_Builtin_Array :=
        ((4, "sqrt ", Unary),
         (3, "sin  ", Unary),
         (3, "cos  ", Unary),
         (3, "pow  ", Binary),
         (3, "exp  ", Unary),
         (4, "exp2 ", Unary),
         (3, "log  ", Unary),
         (5, "log10", Unary),
         (4, "log2 ", Unary),
         (3, "fma  ", Ternary));

      Len     : Integer;
      Actuals : Nat;

   begin
      for FP of FP_Builtins loop
         Len := FP.Length;
         Actuals := (case FP.Kind is
                     when Unary => 1,
                     when Binary | Boolean_And_Data => 2,
                     when Ternary => 3);

         if Num_Actuals (N) = Actuals
           and then S'Length >= Len + 10
           and then S (S'First .. S'First + 9) = "__builtin_"
           and then S (S'First + 10 .. S'First + Len + 9) = FP.Name (1 .. Len)
           and then (S'Length = Len + 10
                       or else (S'Length = Len + 11
                                  and then S (S'First + Len + 10)
                                             in 'f' | 'l'))
         then
            declare
               GT     : constant GL_Type           := Full_GL_Type (N);
               I_Name : constant String            :=
                 "llvm." & FP.Name (1 .. FP.Length);
               Subp   : constant GL_Value          :=
                 Build_Intrinsic (I_Name, GT, (1 => Type_Of (GT)));
               Actual : constant Opt_N_Subexpr_Id  := First_Actual (N);

            begin
               case FP.Kind is
               when Unary =>
                  return Call (Subp, (1 => Emit_Expression (Actual)));
               when Binary | Boolean_And_Data =>
                  return Call (Subp, (1 => Emit_Expression (Actual),
                                      2 => Emit_Expression (Next_Actual
                                                              (Actual))));
               when Ternary =>
                  return Call
                           (Subp,
                            (1 => Emit_Expression (Actual),
                             2 => Emit_Expression (Next_Actual (Actual)),
                             3 => Emit_Expression
                                    (Next_Actual (Next_Actual (Actual)))));
               end case;
            end;
         end if;
      end loop;

      return No_GL_Value;
   end Emit_FP_Builtin_Call;

   ------------------------
   -- Emit_FP_Isinf_Call --
   ------------------------

   function Emit_FP_Isinf_Call
     (N : N_Subprogram_Call_Id; S : String) return GL_Value
   is
   begin
      if Num_Actuals (N) = 1
        and then S'Length >= 15
           and then S (S'First .. S'First + 14) = "__builtin_isinf"
           and then (S'Length = 15
                       or else (S'Length = 16
                                  and then S (S'First + 15) in 'f' | 'l'))
      then
         --  Compute the absolute value of the operand and compare that
         --  against infinity.

         declare
            Op       : constant GL_Value := Emit_Expression (First_Actual (N));
            Zero     : constant GL_Value := Const_Null (Op);
            Nonzero  : constant GL_Value :=
              Build_Elementary_Comparison (N_Op_Ge, Op, Zero);
            Abs_Expr : constant GL_Value :=
              Build_Select (Nonzero, Op, F_Neg (Op));
            Inf      : constant GL_Value := Const_Infinity (Op);

         begin
            return Build_Elementary_Comparison (N_Op_Eq, Abs_Expr, Inf);
         end;

      --  Otherwise, show this isn't a test for infinity

      else
         return No_GL_Value;
      end if;

   end Emit_FP_Isinf_Call;

   -------------------------
   -- Emit_Intrinsic_Call --
   -------------------------

   function Emit_Intrinsic_Call
     (N : N_Subprogram_Call_Id; Subp : Subprogram_Kind_Id) return GL_Value
   is
      S      : constant String  := Get_Ext_Name (Subp);
      First  : constant Integer := S'First;
      Last   : constant Integer := Last_Non_Suffix (S);
      Name   : constant String  := S (First .. Last);
      Result : GL_Value;

   begin
      --  First see if this is a __sync class of subprogram

      if S'Length > 7 and then S (First .. First + 6) = "__sync_" then
         return Emit_Sync_Call (N, S (First + 7 .. S'Last));

      --  Next, check if it's a CHERI builtin

      elsif S'Length > 16
        and then S (First .. First + 15) = "__builtin_cheri_"
      then
         return Emit_Cheri_Call (N, S (First + 16 .. S'Last));

      --  Check for __builtin_bswap, __builtin_expect, and __atomic_load

      elsif Name = "__builtin_bswap" then
         return Emit_Bswap_Call (N, S);
      elsif Name = "__builtin_frame_address" then
         return Emit_Frame_Address_Call (N);
      elsif S in "__builtin_expect" | "__builtin_likely" | "__builtin_unlikely"
      then
         return Emit_Branch_Prediction_Call (N, S);
      elsif S'Length > 9
        and then S (First .. First + 8) = "__atomic_"
      then
         return Emit_Atomic_Call (N, S (First + 9 .. S'Last));

      --  Now see if this is a FP builtin

      elsif Nkind (N) = N_Function_Call then
         Result := Emit_FP_Builtin_Call (N, S);

         if No (Result) then
            Result := Emit_FP_Isinf_Call (N, S);
         end if;

         if Present (Result) then
            return Result;
         end if;

         if S'Length >= 13
           and then S (First .. First + 12) = "__builtin_inf"
           and then
           (S'Length = 13
            or else
            (S'Length = 14 and then S (First + 13) in 'f' | 'l'))
         then
            return Const_Infinity (Full_GL_Type (N));
         end if;
      end if;

      --  That's all we support for now

      return No_GL_Value;
   end Emit_Intrinsic_Call;

   --------------------------
   -- Get_Default_Alloc_Fn --
   --------------------------

   function Get_Default_Alloc_Fn return GL_Value is
   begin
      if No (Default_Alloc_Fn) then
         Default_Alloc_Fn :=
           Add_Global_Function
             (Get_Default_Alloc_Fn_Name,
              Fn_Ty
                ((1 => Size_T),
                 (if Emit_C then Void_Ptr_T else Address_T)),
              (if Emit_C then A_Char_GL_Type else Address_GL_Type));
      end if;

      return Default_Alloc_Fn;
   end Get_Default_Alloc_Fn;

   -------------------------
   -- Get_Default_Free_Fn --
   -------------------------

   function Get_Default_Free_Fn return GL_Value is
   begin
      if No (Default_Free_Fn) then
         Default_Free_Fn :=
           Add_Global_Function
             (Get_Default_Free_Fn_Name,
              Fn_Ty
                ((1 => (if Emit_C then Void_Ptr_T else Address_T)),
                 Void_Type),
              Void_GL_Type);
      end if;

      return Default_Free_Fn;
   end Get_Default_Free_Fn;

   ---------------------------
   -- Get_Memory_Compare_Fn --
   ---------------------------

   function Get_Memory_Compare_Fn return GL_Value is
   begin
      if No (Memory_Compare_Fn) then
         Memory_Compare_Fn := Add_Global_Function
           ("memcmp",
            Fn_Ty ((1 => Void_Ptr_T, 2 => Void_Ptr_T, 3 => Size_T),
                   Type_Of (Integer_GL_Type)),
            Integer_GL_Type);

         if Is_A_Function (Memory_Compare_Fn) then
            Add_Nocapture_Attribute (Memory_Compare_Fn, 0);
            Add_Nocapture_Attribute (Memory_Compare_Fn, 1);
            Add_Readonly_Attribute  (Memory_Compare_Fn, 0);
            Add_Readonly_Attribute  (Memory_Compare_Fn, 1);
            Add_Non_Null_Attribute  (Memory_Compare_Fn, 0);
            Add_Non_Null_Attribute  (Memory_Compare_Fn, 1);
            Set_Does_Not_Throw      (Memory_Compare_Fn);
         end if;
      end if;

      return Memory_Compare_Fn;
   end Get_Memory_Compare_Fn;

   -----------------------
   -- Get_Stack_Save_Fn --
   -----------------------

   function Get_Stack_Save_Fn return GL_Value is
   begin
      if No (Stack_Save_Fn) then
         Stack_Save_Fn :=
           Build_Intrinsic ("llvm.stacksave", A_Char_GL_Type);
      end if;

      return Stack_Save_Fn;
   end Get_Stack_Save_Fn;

   --------------------------
   -- Get_Stack_Restore_Fn --
   --------------------------

   function Get_Stack_Restore_Fn return GL_Value is
   begin
      if No (Stack_Restore_Fn) then
         Stack_Restore_Fn :=
           Build_Intrinsic ("llvm.stackrestore", Void_GL_Type);
      end if;

      return Stack_Restore_Fn;
   end Get_Stack_Restore_Fn;

   -----------------------
   -- Get_Tramp_Init_Fn --
   -----------------------

   function Get_Tramp_Init_Fn return GL_Value is
   begin
      if No (Tramp_Init_Fn) then
         Tramp_Init_Fn :=
           Build_Intrinsic ("llvm.init.trampoline", Void_GL_Type);
      end if;

      return Tramp_Init_Fn;
   end Get_Tramp_Init_Fn;

   -------------------------
   -- Get_Tramp_Adjust_Fn --
   -------------------------

   function Get_Tramp_Adjust_Fn return GL_Value is
   begin
      if No (Tramp_Adjust_Fn) then
         Tramp_Adjust_Fn :=
           Build_Intrinsic ("llvm.adjust.trampoline", A_Char_GL_Type);
      end if;

      return Tramp_Adjust_Fn;
   end Get_Tramp_Adjust_Fn;

   ---------------------------------
   -- Get_Enable_Execute_Stack_Fn --
   ---------------------------------

   function Get_Enable_Execute_Stack_Fn return GL_Value is
   begin
      if No (Enable_Execute_Stack_Fn) then
         --  Defined in libgcc and compiler-rt builtins
         Enable_Execute_Stack_Fn :=
           Add_Global_Function
             ("__enable_execute_stack",
              Fn_Ty ((1 => Void_Ptr_T), Void_Type), Void_GL_Type);
      end if;

      return Enable_Execute_Stack_Fn;
   end Get_Enable_Execute_Stack_Fn;

   -------------------
   -- Get_Expect_Fn --
   -------------------

   function Get_Expect_Fn return GL_Value is
   begin
      if No (Expect_Fn) then
         Expect_Fn :=
           Build_Intrinsic
             ("llvm.expect", Boolean_GL_Type, (1 => Bit_T));
      end if;

      return Expect_Fn;
   end Get_Expect_Fn;

   --------------------------
   -- Get_Frame_Address_Fn --
   --------------------------

   function Get_Frame_Address_Fn return GL_Value is
   begin
      if No (Frame_Address_Fn) then
         Frame_Address_Fn :=
           Build_Intrinsic
             ("llvm.frameaddress.p0", A_Char_GL_Type,
              (1 => Void_Ptr_T));
      end if;

      return Frame_Address_Fn;
   end Get_Frame_Address_Fn;

   ------------------------
   -- Get_Get_Address_Fn --
   ------------------------

   function Get_Get_Address_Fn return GL_Value is
   begin
      if No (Get_Address_Fn) then

         --  This is currently implemented for the Morello purecap ABI
         --  only.

         pragma Assert (ABI.all = "purecap");

         Get_Address_Fn :=
           Build_Intrinsic
             ("llvm.cheri.cap.address.get", Size_GL_Type,
              (1 => Size_T));
      end if;

      return Get_Address_Fn;
   end Get_Get_Address_Fn;

   ------------------------
   -- Get_Set_Address_Fn --
   ------------------------

   function Get_Set_Address_Fn return GL_Value is
   begin
      if No (Set_Address_Fn) then

         --  This is currently implemented for the Morello purecap ABI
         --  only.

         pragma Assert (ABI.all = "purecap");

         Set_Address_Fn :=
           Build_Intrinsic
             ("llvm.cheri.cap.address.set", A_Char_GL_Type,
              (1 => Size_T));
      end if;

      return Set_Address_Fn;
   end Get_Set_Address_Fn;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register_Global_Name (Get_Default_Free_Fn_Name);
      Register_Global_Name (Get_Default_Alloc_Fn_Name);
      Register_Global_Name ("memcmp");
   end Initialize;

end GNATLLVM.Builtins;
