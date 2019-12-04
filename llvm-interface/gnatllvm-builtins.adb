-----------------------------------------------------------------------------
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

with Errout;   use Errout;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Table;    use Table;

with LLVM.Core; use LLVM.Core;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.Strings;            use GNAT.Strings;

with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Exprs;        use GNATLLVM.Exprs;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Subprograms;  use GNATLLVM.Subprograms;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Variables;    use GNATLLVM.Variables;

package body GNATLLVM.Builtins is

   type Intrinsic is record
      Name  : String_Access;
      Width : ULL;
      Func  : GL_Value;
   end record;
   --  A description of an intrinsic function that we've created

   --  Since we aren't going to be creating all that many different
   --  intrinsic functions, a simple list that we search should be
   --  fast enough.

   package Intrinsic_Functions_Table is new Table.Table
     (Table_Component_Type => Intrinsic,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 5,
      Table_Name           => "Intrinsic_Function_Table");

   function Name_To_RMW_Op
     (S           : String;
      Index       : Integer;
      End_Index   : out Integer;
      Op          : out Atomic_RMW_Bin_Op_T) return Boolean;
   --  See if the string S starting at position Index is the name of
   --  a supported LLVM atomicrmw instruction.  If so, set End_Index
   --  to after the name and Op to the code for the operation and return True.

   function Last_Non_Suffix (S : String) return Integer;
   --  S is the name of some intrinsic function.  Return the index of
   --  the last character of that name that doesn't include a "_N" suffix
   --  where N is a string of digits denoting a size.

   function Type_Size_Matches_Name
     (S        : String;
      In_Bytes : Boolean;
      GT       : GL_Type) return Boolean
     with Pre => Present (GT);
   --  Return True if the characteers at the end of string S
   --  represents an integer corresponding to the size of GT measured
   --  in bytes or bits, depending on In_Bytes.

   function Emit_Ptr
     (Ptr : Node_Id; GT : GL_Type) return GL_Value
     with Pre => Present (Ptr) and then Present (GT);
   --  If Ptr is an Address or an access type pointing to GT, return a
   --  GL_Value that's a Reference to GT.  If not, or if GT is not an
   --  elementary type, return No_GL_Value.

   function Memory_Order
     (N          : Node_Id;
      No_Acq     : Boolean     := False;
      No_Release : Boolean := False) return Atomic_Ordering_T
     with Pre => Present (N);
   --  N is an expression being passed as an operand for a memory order.
   --  Return the corresponding memory ordering for an instruction.

   function Emit_Fetch_And_Op
     (Ptr     : Node_Id;
      Val     : Entity_Id;
      Op      : Atomic_RMW_Bin_Op_T;
      Op_Back : Boolean;
      Order   : Atomic_Ordering_T;
      S       : String;
      GT      : GL_Type) return GL_Value
     with Pre => Present (Ptr) and then Present (Val) and then Present (GT);
   --  Perform the Fetch_And_Op (Atomic_RMW) given by Op at the location
   --  Ptr.  If Op_Back is True, we want the new value, so we have to
   --  perform the operation on the result.  Order is the memory ordering
   --  needed.  S and GT are as for Type_Size_Matches_Name.  If we can
   --  peform the operation, return the result.  Otherwise, return
   --  No_GL_Value.

   function Emit_Compare_Xchg
     (Ptr_Val  : GL_Value;
      Old_Val  : GL_Value;
      New_Val  : GL_Value;
      S_Order  : Atomic_Ordering_T;
      F_Order  : Atomic_Ordering_T;
      Weak     : Boolean;
      Want_Val : Boolean) return GL_Value
     with Pre  => Present (Ptr_Val) and then Present (Old_Val)
                  and then Present (New_Val)
                  and then Related_Type (Ptr_Val) = Related_Type (Old_Val)
                  and then Related_Type (Ptr_Val) = Related_Type (New_Val),
          Post => Present (Emit_Compare_Xchg'Result);
   --  Emit an atomic compare-exchange operation at Ptr with the given old
   --  and new values. Weak, S_Order, and F_Order give the desired
   --  atomicity parameters.  If Want_Val is True, we return the old value
   --  at Ptr, but if False, we return a boolean saying whether the
   --  value was written.

   function Emit_Bswap_Call (N : Node_Id; S : String) return GL_Value
     with Pre  => Nkind (N) in N_Subprogram_Call;
   --  If N is a valid call to builtin_bswap, generate it

   function Emit_Sync_Call (N : Node_Id; S : String) return GL_Value
     with Pre  => Nkind (N) in N_Subprogram_Call;
   --  If S is a valid __sync name for an instrinsic subprogram and
   --  the operands are value, emit it. Otherwise, return
   --  No_GL_Value.

   function Emit_Branch_Prediction_Call
     (N : Node_Id; S : String) return GL_Value
     with Pre  => Nkind (N) in N_Subprogram_Call;
   --  Generate a call to the branch prediction function if the operands
   --  are the right type.

   function Emit_Atomic_Call (N : Node_Id; S : String) return GL_Value
     with Pre  => Nkind (N) in N_Subprogram_Call;
   --  Generate a call to the atomic load function if the operands are the
   --  right type.

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

   Lifetime_Start_Fn  : GL_Value := No_GL_Value;
   Lifetime_End_Fn    : GL_Value := No_GL_Value;
   Invariant_Start_Fn : GL_Value := No_GL_Value;
   --  Functions to mark the start and end of the lifetime of a variable or
   --  constant, and, for the latter, when it starts becoming constant.

   Expect_Fn          : GL_Value := No_GL_Value;
   --  Function to provide branch prediction information

   ---------------------
   -- Build_Intrinsic --
   ---------------------

   function Build_Intrinsic
     (Kind : Overloaded_Intrinsic_Kind;
      Name : String;
      GT   : GL_Type) return GL_Value
   is
      T         : constant Type_T := Type_Of (GT);
      Width     : constant ULL    := Get_Scalar_Bit_Size (T);
      W         : constant String := Int'Image (Int (Width));
      Full_Name : constant String := Name & W (W'First + 1 .. W'Last);
      Return_GT : GL_Type         := GT;
      Fun_Ty    : Type_T;
      Result    : GL_Value;

   begin

      for J in 1 .. Intrinsic_Functions_Table.Last loop
         if Intrinsic_Functions_Table.Table (J).Name.all = Name
           and then Intrinsic_Functions_Table.Table (J).Width = Width
         then
            return Intrinsic_Functions_Table.Table (J).Func;
         end if;
      end loop;

      case Kind is
         when Unary =>
            Fun_Ty := Fn_Ty ((1 => T), T);

         when Binary =>
            Fun_Ty := Fn_Ty ((1 => T, 2 => T), T);

         when Boolean_And_Data =>
            Fun_Ty := Fn_Ty ((1 => T, 2 => T),
                             Type_For_Relationship (GT, Boolean_And_Data));

         when Memcpy =>
            Return_GT := Void_GL_Type;
            Fun_Ty    := Fn_Ty
              ((1 => Void_Ptr_Type,  2 => Void_Ptr_Type,
                3 => LLVM_Size_Type, 4 => Bit_T),
               Void_Type);

         when Memset =>
            Return_GT := Void_GL_Type;
            Fun_Ty    := Fn_Ty
              ((1 => Void_Ptr_Type,  2 => Byte_T,
                3 => LLVM_Size_Type, 4 => Bit_T),
               Void_Type);
      end case;

      Result := Add_Function (Full_Name, Fun_Ty, Return_GT,
                              Is_Builtin => True);
      Set_Does_Not_Throw (Result);
      if Kind = Memcpy then
         Add_Nocapture_Attribute (Result, 0);
         Add_Nocapture_Attribute (Result, 1);
         Add_Non_Null_Attribute  (Result, 0);
         Add_Non_Null_Attribute  (Result, 1);
         Add_Writeonly_Attribute (Result, 0);
         Add_Readonly_Attribute  (Result, 1);
      elsif Kind = Memset then
         Add_Nocapture_Attribute (Result, 0);
         Add_Non_Null_Attribute  (Result, 0);
         Add_Writeonly_Attribute (Result, 0);
      end if;

      Intrinsic_Functions_Table.Append ((new String'(Name), Width, Result));
      return Result;
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
         if S'Last > Index + Len - 1
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

      --  Otherwise, search for the last non-digit.  We know that the
      --  last character is a digit.

      else
         return Idx : Integer := S'Last - 1 do
            while Idx >= S'First and then Is_Digit (S (Idx)) loop
               Idx := Idx - 1;
            end loop;

            --  If we have characters left, and we're preceeded by a '_',
            --  return the index of the character before that.  Otherwise,
            --  the string doesn't look like "xxx_N", so return 'Last.

            if Idx >= S'First + 1 and then S (Idx) = '_' then
               Idx := Idx - 1;
            else
               Idx := S'Last;
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
      --  We don't match if the rest of the string isn't the right length
      --  and we also don't deal with three-digit sizes.

      if S'Length < Digs + 1 or else Digs > 2 then
         return False;

      --  If there's only one digit, see if it's the right one

      elsif Digs = 1 then
         return S (S'Last) = Character'Val (Character'Pos ('0') + Size)
           and then S (S'Last - 1) = '_';

      --  If there are two digits (the only remaining case), check both

      else
         return S (S'Last) = Character'Val (Character'Pos ('0') + Size / 10)
           and then S (S'Last - 1) = Character'Val
                                      (Character'Pos ('0') + Size mod 10)
           and then S (S'Last - 2) = '_';
      end if;

   end Type_Size_Matches_Name;

   ------------------
   -- Memory_Order --
   ------------------

   function Memory_Order
     (N          : Node_Id;
      No_Acq     : Boolean     := False;
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
            and then (No_Acq or else No_Release))
        or else (Order = Atomic_Ordering_Acquire and then No_Acq)
        or else (Order = Atomic_Ordering_Release and then No_Release)
      then
         Error_Msg_N ("Invalid memory ordering for operation", N);
         Order := Atomic_Ordering_Sequentially_Consistent;
      end if;

      return Order;

   end Memory_Order;

   --------------
   -- Emit_Ptr --
   --------------

   function Emit_Ptr
     (Ptr : Node_Id; GT : GL_Type) return GL_Value
   is
      Ptr_Val : GL_Value := Emit_Expression (Ptr);

   begin
      if not Is_Elementary_Type (GT) then
         return No_GL_Value;

      --  If the pointer is derived from System.Address convert to GT

      elsif Is_Descendant_Of_Address (Ptr_Val) then
         return Int_To_Ref (Ptr_Val, GT);
      elsif Is_Access_Type (Ptr_Val) then
         Ptr_Val := From_Access (Ptr_Val);
         return (if Related_Type (Ptr_Val) = GT then Ptr_Val else No_GL_Value);
      else
         return No_GL_Value;
      end if;

   end Emit_Ptr;

   -----------------------
   -- Emit_Fetch_And_Op --
   -----------------------

   function Emit_Fetch_And_Op
     (Ptr     : Node_Id;
      Val     : Entity_Id;
      Op      : Atomic_RMW_Bin_Op_T;
      Op_Back : Boolean;
      Order   : Atomic_Ordering_T;
      S       : String;
      GT      : GL_Type) return GL_Value
   is
      Ptr_Val : GL_Value;
      Value   : GL_Value;
      Result  : GL_Value;

   begin
      --  Emit all operands and validate all our conditions

      Ptr_Val := Emit_Ptr (Ptr, GT);
      Value   := Emit_Expression (Val);
      if No (Ptr_Val) or else Related_Type (Value) /= GT
        or else not Type_Size_Matches_Name (S, True, GT)
      then
         return No_GL_Value;
      end if;

      --  Now we can emit the operation

      Result := Atomic_RMW (Op, Ptr_Val, Value, Order);
      Set_Volatile_For_Atomic (Result);

      --  If we want the value before the operation, we're done.  Otherwise,
      --  we have to do the operation.

      if not Op_Back then
         return Result;
      end if;

      case Op is
         when Atomic_RMW_Bin_Op_Xchg =>
            return Result;

         when Atomic_RMW_Bin_Op_Add =>
            return Result + Value;

         when Atomic_RMW_Bin_Op_Sub =>
            return Result - Value;

         when Atomic_RMW_Bin_Op_And =>
            return Build_And (Result, Value);

         when Atomic_RMW_Bin_Op_Nand =>
            return Build_Not (Build_And (Result, Value));

         when Atomic_RMW_Bin_Op_Or =>
            return Build_Or (Result, Value);

         when Atomic_RMW_Bin_Op_Xor =>
            return Build_Xor (Result, Value);

         when Atomic_RMW_Bin_Op_Max | Atomic_RMW_Bin_Op_U_Max =>
            return Build_Max (Result, Value);

         when Atomic_RMW_Bin_Op_Min | Atomic_RMW_Bin_Op_U_Min =>
            return Build_Min (Result, Value);
      end case;

   end Emit_Fetch_And_Op;

   -----------------------
   -- Emit_Compare_Xchg --
   -----------------------

   function Emit_Compare_Xchg
     (Ptr_Val  : GL_Value;
      Old_Val  : GL_Value;
      New_Val  : GL_Value;
      S_Order  : Atomic_Ordering_T;
      F_Order  : Atomic_Ordering_T;
      Weak     : Boolean;
      Want_Val : Boolean) return GL_Value
   is
      Result : constant GL_Value :=
        Atomic_Cmp_Xchg (Ptr_Val, Old_Val, New_Val, S_Order, F_Order,
                         Weak => Weak);

   begin
      Set_Volatile_For_Atomic (Result);

      if Want_Val then
         return Get (Result, Data);
      else
         return Get (G_Is_Relationship (Result, Boolean_GL_Type,
                                        Boolean_And_Data),
                     Boolean_Data);
      end if;

   end Emit_Compare_Xchg;

   --------------------
   -- Emit_Sync_Call --
   --------------------

   function Emit_Sync_Call (N : Node_Id; S : String) return GL_Value is
      Ptr       : constant Node_Id  := First_Actual (N);
      N_Args    : constant Nat      := Num_Actuals (N);
      Val       : constant Node_Id  :=
        (if N_Args < 2 then Empty else Next_Actual (Ptr));
      GT        : constant GL_Type  := Full_GL_Type (Val);
      First     : constant Integer  := S'First + String'("__sync_")'Length;
      Last      : constant Integer  := Last_Non_Suffix (S);
      Name      : constant String   := S (First .. Last);
      Ptr_Val   : constant GL_Value :=
        (if N_Args = 0 then No_GL_Value else Emit_Ptr (Ptr, GT));
      Order     : Atomic_Ordering_T := Atomic_Ordering_Sequentially_Consistent;
      Op        : Atomic_RMW_Bin_Op_T;
      Op_Back   : Boolean;
      Index     : Integer;

   begin
      --  This may be a __sync builtin.  Parse it to see what it tells us
      --  to do.  If anything is wrong with the builtin or its operands,
      --  just return No_GL_Value and a normal call will result, which will
      --  produce a link error.
      --
      --  First handle the compare_and_swap variants.

      if (Name = "bool_compare_and_swap" or else Name = "val_compare_and_swap")
        and then N_Args = 3 and then Nkind (N) = N_Function_Call
      then
         declare
            New_Val : constant GL_Value := Emit_Expression (Next_Actual (Val));

         begin
            if Present (Ptr_Val) and then Related_Type (New_Val) = GT
              and then Type_Size_Matches_Name (S, True, GT)
            then
               return
                 Emit_Compare_Xchg (Ptr_Val, Emit_Expression (Val), New_Val,
                                    Atomic_Ordering_Sequentially_Consistent,
                                    Atomic_Ordering_Sequentially_Consistent,
                                    False, Name (Name'First) = 'v');
            else
               return No_GL_Value;
            end if;
         end;
      end if;

      --  The remaining possibility is to have "Op_and_fetch",
      --  "fetch_and_Op", or "lock_test_and_set", all of which are
      --  fetch-and-ops.

      if Name_To_RMW_Op (S, First, Index, Op) and then Last = Index + 9
        and then S (Index .. Last) = "_and_fetch"
        and then Nkind (N) = N_Function_Call and then N_Args = 2
      then
         Op_Back := True;
      elsif S'Last > First + 9 and then S (First .. First + 9) = "fetch_and_"
        and then Name_To_RMW_Op (S, First + 10, Index, Op)
        and then Index - 1 = Last
        and then Nkind (N) = N_Function_Call and then N_Args = 2
      then
         Op_Back := False;
      elsif Name = "lock_test_and_set" and then Nkind (N) = N_Function_Call
        and then N_Args = 2
      then
         Op      := Atomic_RMW_Bin_Op_Xchg;
         Op_Back := False;
         Order   := Atomic_Ordering_Acquire;
      else
         return No_GL_Value;
      end if;

      return Emit_Fetch_And_Op (Ptr, Val, Op, Op_Back, Order, S, GT);

   end Emit_Sync_Call;

   ---------------------
   -- Emit_Bswap_Call --
   ---------------------

   function Emit_Bswap_Call (N : Node_Id; S : String) return GL_Value is
      Val       : constant Node_Id := First_Actual (N);
      GT        : GL_Type;

   begin
      --  This is supposedly a __builtin_bswap builtin.  Verify that it is.
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

      return Call (Build_Intrinsic (Unary, "llvm.bswap.i", GT), GT,
                   (1 => Emit_Expression (Val)));
   end Emit_Bswap_Call;

   ------------------------------
   -- Emit_Branch_Predict_Call --
   ------------------------------

   function Emit_Branch_Prediction_Call
     (N : Node_Id; S : String) return GL_Value
   is
      Val      : constant Node_Id := First_Actual (N);
      Two_Arg  : constant Boolean := S = "__builtin_expect";
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

      return Call_Relationship (Get_Expect_Fn, Boolean_GL_Type,
                                (1 => Get (Emit (Val), Boolean_Data),
                                 2 => Expected),
                                Boolean_Data);

   end Emit_Branch_Prediction_Call;

   ----------------------
   -- Emit_Atomic_Call --
   ----------------------

   function Emit_Atomic_Call (N : Node_Id; S : String) return GL_Value is
      Ptr        : constant Node_Id := First_Actual (N);
      GT         : constant GL_Type := Full_GL_Type (N);
      Ptr_Val    : GL_Value;
      Order      : Node_Id;
      Inst       : Value_T;

   begin
      --  Verify that the types and number of arguments are correct

      if Nkind (N) /= N_Function_Call or else No (Ptr) then
         return No_GL_Value;
      end if;

      --  Deal with the pointer and validate all our conditions

      Ptr_Val := Emit_Ptr (Ptr, GT);
      if No (Ptr_Val) or else not Type_Size_Matches_Name (S, True, GT) then
         return No_GL_Value;
      end if;

      --  Now validate the Order argument

      Order := Next_Actual (Ptr);
      if No (Order) or else Present (Next_Actual (Order)) then
         return No_GL_Value;
      end if;

      --  Finally emit the desired load

      Inst := Load  (IR_Builder, LLVM_Value (Ptr_Val), "");
      Set_Ordering  (Inst, Memory_Order (Order, No_Release => True));
      Set_Volatile  (Inst, True);
      Set_Alignment (Inst,
                     unsigned (Nat'(To_Bytes (Get_Type_Alignment (GT)))));

      return G (Inst, GT);

   end Emit_Atomic_Call;

   -------------------------
   -- Emit_Intrinsic_Call --
   -------------------------

   function Emit_Intrinsic_Call (N : Node_Id; Subp : Entity_Id) return GL_Value
   is
      Fn_Name : constant String  := Get_Ext_Name (Subp);
      J       : constant Integer := Fn_Name'First;
      N_Args  : constant Nat     := Num_Actuals (N);
      Len     : Integer;

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
         (4, "log2 ", Unary));

   begin
      --  First see if this is a __sync class of subprogram

      if Fn_Name'Length > 7 and then Fn_Name (J .. J + 6) = "__sync_"
        and then N_Args >= 2
      then
         return Emit_Sync_Call (N, Fn_Name);

      --  Check for __builtin_bswap, __builtin_expect, and __atomic_load

      elsif Fn_Name'Length > 16
        and then Fn_Name (J .. J + 14) = "__builtin_bswap"
      then
         return Emit_Bswap_Call (N, Fn_Name);
      elsif Fn_Name = "__builtin_expect" or else Fn_Name = "__builtin_likely"
        or else Fn_Name = "__builtin_unlikely"
      then
         return Emit_Branch_Prediction_Call (N, Fn_Name);
      elsif Fn_Name'Length > 14
        and then Fn_Name (J .. J + 13) = "__atomic_load_"
      then
         return Emit_Atomic_Call (N, Fn_Name);
      end if;

      --  Now see if this is a FP builtin

      for FP of FP_Builtins loop
         Len := FP.Length;
         if Fn_Name'Length >= Len + 10
           and then Fn_Name (J .. J + 9) = "__builtin_"
           and then Fn_Name (J + 10 .. J + Len + 9) = FP.Name (1 .. Len)
           and then (Fn_Name'Length = Len + 10
                       or else (Fn_Name'Length = Len + 11
                                  and then (Fn_Name (J + Len + 10)
                                              in 'f' | 'l')))
         then
            declare
               GT     : constant GL_Type  := Full_GL_Type (N);
               I_Name : constant String   :=
                 "llvm." & FP.Name (1 .. FP.Length) & ".f";
               Subp   : constant GL_Value :=
                 Build_Intrinsic (FP.Kind, I_Name, GT);
               Actual : constant Node_Id  := First_Actual (N);

            begin
               if FP.Kind = Unary then
                  return Call (Subp, GT, (1 => Emit_Expression (Actual)));
               else
                  return Call (Subp, GT, (1 => Emit_Expression (Actual),
                                          2 => Emit_Expression (Next_Actual
                                                                  (Actual))));
               end if;
            end;
         end if;
      end loop;

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
           Add_Global_Function ("__gnat_malloc",
                                Fn_Ty ((1 => LLVM_Size_Type), Void_Ptr_Type),
                                A_Char_GL_Type);

         if Is_A_Function (Default_Alloc_Fn) then
            Add_Noalias_Attribute (Default_Alloc_Fn);
         end if;
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
           Add_Global_Function ("__gnat_free",
                                Fn_Ty ((1 => Void_Ptr_Type), Void_Type),
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
            Fn_Ty ((1 => Void_Ptr_Type, 2 => Void_Ptr_Type,
                    3 => LLVM_Size_Type),
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
         Stack_Save_Fn := Add_Function
           ("llvm.stacksave", Fn_Ty ((1 .. 0 => <>), Void_Ptr_Type),
            A_Char_GL_Type, Is_Builtin => True);
         Set_Does_Not_Throw (Stack_Save_Fn);
      end if;

      return Stack_Save_Fn;
   end Get_Stack_Save_Fn;

   --------------------------
   -- Get_Stack_Restore_Fn --
   --------------------------

   function Get_Stack_Restore_Fn return GL_Value is
   begin
      if No (Stack_Restore_Fn) then
         Stack_Restore_Fn := Add_Function
           ("llvm.stackrestore",
            Fn_Ty ((1 => Void_Ptr_Type), Void_Type), Void_GL_Type,
            Is_Builtin => True);
         Set_Does_Not_Throw (Stack_Restore_Fn);
      end if;

      return Stack_Restore_Fn;
   end Get_Stack_Restore_Fn;

   -----------------------
   -- Get_Tramp_Init_Fn --
   -----------------------

   function Get_Tramp_Init_Fn return GL_Value is
   begin
      if No (Tramp_Init_Fn) then
         Tramp_Init_Fn := Add_Function
           ("llvm.init.trampoline",
            Fn_Ty ((1 => Void_Ptr_Type, 2 => Void_Ptr_Type,
                    3 => Void_Ptr_Type),
                   Void_Type),
            Void_GL_Type, Is_Builtin => True);
         Set_Does_Not_Throw (Tramp_Init_Fn);
      end if;

      return Tramp_Init_Fn;
   end Get_Tramp_Init_Fn;

   -------------------------
   -- Get_Tramp_Adjust_Fn --
   -------------------------

   function Get_Tramp_Adjust_Fn return GL_Value is
   begin
      if No (Tramp_Adjust_Fn) then
         Tramp_Adjust_Fn := Add_Function
           ("llvm.adjust.trampoline",
            Fn_Ty ((1 => Void_Ptr_Type), Void_Ptr_Type), A_Char_GL_Type,
            Is_Builtin => True);
         Set_Does_Not_Throw (Tramp_Adjust_Fn);
      end if;

      return Tramp_Adjust_Fn;
   end Get_Tramp_Adjust_Fn;

   ---------------------------
   -- Get_Lifetime_Start_Fn --
   ---------------------------

   function Get_Lifetime_Start_Fn return GL_Value is
   begin
      if No (Lifetime_Start_Fn) then
         Lifetime_Start_Fn := Add_Function
           ("llvm.lifetime.start.p0i8",
            Fn_Ty ((1 => LLVM_Size_Type, 2 => Void_Ptr_Type), Void_Type),
            Void_GL_Type, Is_Builtin => True);
         Set_Does_Not_Throw      (Lifetime_Start_Fn);
         Add_Nocapture_Attribute (Lifetime_Start_Fn, 1);
         Add_Readonly_Attribute  (Lifetime_Start_Fn, 1);
         Add_Non_Null_Attribute  (Lifetime_Start_Fn, 1);
      end if;

      return Lifetime_Start_Fn;
   end Get_Lifetime_Start_Fn;

   -------------------------
   -- Get_Lifetime_End_Fn --
   -------------------------

   function Get_Lifetime_End_Fn return GL_Value is
   begin
      if No (Lifetime_End_Fn) then
         Lifetime_End_Fn := Add_Function
           ("llvm.lifetime.end.p0i8",
            Fn_Ty ((1 => LLVM_Size_Type, 2 => Void_Ptr_Type), Void_Type),
            Void_GL_Type, Is_Builtin => True);
         Set_Does_Not_Throw      (Lifetime_End_Fn);
         Add_Nocapture_Attribute (Lifetime_End_Fn, 1);
         Add_Readonly_Attribute  (Lifetime_End_Fn, 1);
         Add_Non_Null_Attribute  (Lifetime_End_Fn, 1);
      end if;

      return Lifetime_End_Fn;
   end Get_Lifetime_End_Fn;

   ----------------------------
   -- Get_Invariant_Start_Fn --
   ----------------------------

   function Get_Invariant_Start_Fn return GL_Value is
   begin
      if No (Invariant_Start_Fn) then
         Invariant_Start_Fn := Add_Function
           ("llvm.invariant.start.p0i8",
            Fn_Ty ((1 => LLVM_Size_Type, 2 => Void_Ptr_Type),
                   Pointer_Type
                     (Build_Struct_Type ((1 .. 0 => <>), False), 0)),
            A_Char_GL_Type, Is_Builtin => True);
         Set_Does_Not_Throw      (Invariant_Start_Fn);
         Add_Nocapture_Attribute (Invariant_Start_Fn, 1);
         Add_Readonly_Attribute  (Invariant_Start_Fn, 1);
         Add_Non_Null_Attribute  (Invariant_Start_Fn, 1);
      end if;

      return Invariant_Start_Fn;
   end Get_Invariant_Start_Fn;

   -------------------
   -- Get_Expect_Fn --
   -------------------

   function Get_Expect_Fn return GL_Value is
   begin
      if No (Expect_Fn) then
         Expect_Fn := Add_Function
           ("llvm.expect.i1",
            Fn_Ty ((1 => Bit_T, 2 => Bit_T), Bit_T),
            Void_GL_Type, Is_Builtin => True);
         Set_Does_Not_Throw      (Get_Expect_Fn);
      end if;

      return Expect_Fn;
   end Get_Expect_Fn;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register_Global_Name ("__gnat_free");
      Register_Global_Name ("__gnat_malloc");
      Register_Global_Name ("memcmp");
   end Initialize;

end GNATLLVM.Builtins;
