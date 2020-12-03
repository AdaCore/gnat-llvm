------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020, AdaCore                          --
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
with Ada.Containers; use Ada.Containers;

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with LLVM.Core;   use LLVM.Core;
with LLVM.Target; use LLVM.Target;

with CCG.Helper; use CCG.Helper;
with CCG.Tables; use CCG.Tables;

package CCG.Utils is

   function TP
     (S           : String;
      Op1         : Value_T := No_Value_T;
      Op2         : Value_T := No_Value_T;
      Op3         : Value_T := No_Value_T) return Str
     with Post => Present (TP'Result);
   --  This provides a simple template facility for insertion of operands.
   --  Every character up to '#' in S is placed in Str. '#' is followed
   --  optionally by an 'A', 'B', 'I', 'L', or 'T' and then a number.  The
   --  operand of that number is inserted into Str at that point. If 'B' is
   --  present, the operand is interpreted as a basic block. If 'L' is
   --  present, then we want the operand always written as a LHS. If 'I' is
   --  present, this is for the context of an initializer. If 'A' is
   --  present, we take the address of the operand and deference it if 'D'
   --  is present. If 'T' is present, we output the type of that operand.

   function Num_Uses (V : Value_T) return Nat
     with Pre => Present (V);
   --  Returns the number of uses of V

   function Is_Pointer_Type (T : Type_T) return Boolean is
     (Get_Type_Kind (T) = Pointer_Type_Kind)
     with Pre => Present (T);
   function Is_Pointer_Type (V : Value_T) return Boolean is
     (Get_Type_Kind (V) = Pointer_Type_Kind)
     with Pre => Present (V);
   function Is_Simple_Type (T : Type_T) return Boolean is
     (Get_Type_Kind (T) in Half_Type_Kind .. Integer_Type_Kind
        or else Get_Type_Kind (T) = Pointer_Type_Kind)
     with Pre => Present (T);
   function Is_Simple_Type (V : Value_T) return Boolean is
     (Is_Simple_Type (Type_Of (V)))
     with Pre => Present (V);
   --  True if this is or has a type that's simple (elementary)

   function Is_Simple_Constant (V : Value_T) return Boolean is
     ((Get_Value_Kind (V)
         in Constant_Int_Value_Kind | Constant_Pointer_Null_Value_Kind
            | Constant_FP_Value_Kind | Constant_Expr_Value_Kind)
      or else (Is_Undef (V) and then Is_Simple_Type (Type_Of (V))))
     with Pre => Present (V);
   --  True if this is a simple enough constant that we output it in C
   --  source as a constant.
   --  ??? Strings are also simple constants, but we don't support them just
   --  yet.

   function Might_Be_Unsigned (V : Value_T) return Boolean
     with Pre => Present (V);
   --  True if it's possible that V is unsigned

   --  LLVM uses a zero-length array to indicate a variable-length
   --  array.  C doesn't permit zero-element arrays. It's tempting to
   --  use a pointer to the element type instead of a pointer to the
   --  array. In this LLVM usage, we never have any objects of that
   --  array type. However, Ada can have arrays of zero length and can
   --  have objects of that length and doing the above conversion will cause
   --  confusion there.  So we instead interpret an array of length zero as
   --  an array of length one.

   function Effective_Array_Length (T : Type_T) return Nat is
     (if Get_Array_Length (T) = 0 then 1 else Get_Array_Length (T))
      with Pre => Present (T);

   function UC_V is new Ada.Unchecked_Conversion (Value_T, System.Address);
   function UC_T is new Ada.Unchecked_Conversion (Type_T, System.Address);
   function UC_B is new Ada.Unchecked_Conversion (Basic_Block_T,
                                                  System.Address);

   function Hash (V : Value_T)       return Hash_Type is
     (Hash_Type'Mod (To_Integer (UC_V (V)) / (V'Size / 8)))
     with Pre => Present (V);
   function Hash (T : Type_T)        return Hash_Type is
     (Hash_Type'Mod (To_Integer (UC_T (T)) / (T'Size / 8)))
     with Pre => Present (T);
   function Hash (B : Basic_Block_T) return Hash_Type is
     (Hash_Type'Mod (To_Integer (UC_B (B)) / (B'Size / 8)))
     with Pre => Present (B);
   --  Hash functions for LLVM values, types, and basic blocks

   --  We want to compute a hash code for a Str_Component_Array that will be
   --  the same no matter how we break up a concatentation of strings
   --  that do not involve a Value_T, so we don't want to use Ada.Strings.Hash
   --  but instead accumulate the hash value piece by piece.

   procedure Update_Hash (H : in out Hash_Type; Key : Hash_Type) with Inline;
   --  Update H by including the value of Key

   procedure Update_Hash (H : in out Hash_Type; S : String)      with Inline;
   --  Update H taking into account the characters in S

   procedure Update_Hash (H : in out Hash_Type; B : Boolean)      with Inline;
   --  Update H taking into account the value of B

   procedure Update_Hash (H : in out Hash_Type; V : Value_T)
     with Pre => Present (V), Inline;
   --  Update H taking into account the value V

   procedure Update_Hash (H : in out Hash_Type; T : Type_T)
     with Pre => Present (T), Inline;
   --  Update H taking into account the type T

   procedure Update_Hash (H : in out Hash_Type; B : Basic_Block_T)
     with Pre => Present (B), Inline;
   --  Update H taking into account the type T

   function Get_Scalar_Bit_Size (T : Type_T) return ULL is
     (Size_Of_Type_In_Bits (Module_Data_Layout, T))
     with Pre => Present (T);

   function Int_Ty (Num_Bits : ULL) return Type_T is
     (Int_Type (unsigned (Num_Bits)))
     with Post => Get_Type_Kind (Int_Ty'Result) = Integer_Type_Kind;

end CCG.Utils;
