------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with Interfaces; use Interfaces;
with Interfaces.C.Extensions;
with LLVM.Core; use LLVM.Core;

with stdint_h; use stdint_h;

package body Uintp.LLVM is

   function Big_UI_To_LLVM (T : Type_T; U : Uint) return Value_T;

   --------------------
   -- Big_UI_To_LLVM --
   --------------------

   function Big_UI_To_LLVM (T : Type_T; U : Uint) return Value_T is
      Loc     : constant Nat := Uints.Table (U).Loc;
      Length  : constant Integer := Integer (Uints.Table (U).Length);
      D_Table : Udigits.Table_Ptr renames Udigits.Table;

      N_Bits  : constant Integer := Base_Bits * Length;
      N_Words : constant Integer := (N_Bits + 63) / 64;
      N_Padding_Bits : constant Integer := N_Words * 64 - N_Bits;

      Is_Negative : constant Boolean := Integer (D_Table (Loc)) < 0;
      Words       : array (1 .. N_Words) of aliased uint64_t := (others => 0);

      Cur_Word : Integer := N_Words;
      Cur_Bit  : Integer := 64;
      Result   : Value_T;

      procedure Push_Bits (Bits : uint64_t; Length : Integer);
      --  Push Bits (an integer Length bits arge) into the upper bits of Words
      --  right after the cursor. Update the cursor accordingly.

      ---------------
      -- Push_Bits --
      ---------------

      procedure Push_Bits (Bits : uint64_t; Length : Integer) is
         Buffer        : Unsigned_64 := Unsigned_64 (Bits);
         Buffer_Length : Integer := Length;

      begin
         --  Cur_Bit is how many bits are left inside the current word

         if Length > Cur_Bit then
            --  There are more bits to store than free bits inside the current
            --  word: first store the upper ones.

            declare
               Left_Over : constant Integer := Buffer_Length - Cur_Bit;
               --  Number of bits left in the buffer after storing high-order
               --  bits.

            begin
               --  First finish filling the current word

               Words (Cur_Word) := uint64_t
                 (Unsigned_64 (Words (Cur_Word))
                  or Shift_Right (Buffer, Left_Over));

               --  Then go to the next one, updating both the cursor and the
               --  bits to store.

               Cur_Word := Cur_Word - 1;
               Cur_Bit := 64;
               Buffer := Buffer and Unsigned_64 (Ones (Left_Over));
               Buffer_Length := Left_Over;
            end;
         end if;

         Words (Cur_Word) := uint64_t
           (Unsigned_64 (Words (Cur_Word))
            or Shift_Left (Buffer, Cur_Bit - Buffer_Length));
         Cur_Bit := Cur_Bit - Buffer_Length;
         if Cur_Bit = 0 then
            Cur_Word := Cur_Word - 1;
            Cur_Bit := 64;
         end if;
      end Push_Bits;

   begin

      --  There are a number of tricky things here.  First, we use the "**"
      --  operator and we're a child of Uintp, which defines that operator
      --  for Nat ** Nat.  So we need to be sure that we stay away from
      --  Int/Nat and instead use standard Integer.  However, can safely
      --  index into the Udigits table using Nat and that's actually easier.
      --  Also, LLVM takes the first word passed to it as the low-order
      --  part of the constant, not the high-order, as might be expected.
      --  Finally, the absolute value of the constant is what's stored in
      --  the Uint table and we later negate if it is negative.

      Push_Bits (0, N_Padding_Bits);

      for I in Nat range 1 .. Nat (Length) loop
         declare
            D_Digit : Int renames D_Table (Loc + I - Nat (1));
            Digit   : constant Integer := Integer (D_Digit);
            Bits    : constant uint64_t := uint64_t
              (if Digit < 0 then 2 ** Base_Bits - Digit else Digit);
         begin
            Push_Bits (Bits, Base_Bits);
         end;
      end loop;

      Result := Const_Int_Of_Arbitrary_Precision
        (T, Words'Length, Words (Words'First)'Access);

      return (if Is_Negative then Const_Neg (Result) else Result);
   end Big_UI_To_LLVM;

   ----------------
   -- UI_To_LLVM --
   ----------------

   function UI_To_LLVM (T : Type_T; U : Uint) return Value_T is
   begin
      if UI_Is_In_Int_Range (U) then
         return Const_Int
           (T,
            Interfaces.C.Extensions.unsigned_long_long (UI_To_Int (U)), True);

      else
         return Big_UI_To_LLVM (T, U);
      end if;
   end UI_To_LLVM;

end Uintp.LLVM;
