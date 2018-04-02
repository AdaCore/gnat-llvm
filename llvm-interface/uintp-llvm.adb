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
      Loc     : constant Int := Uints.Table (U).Loc;
      Length  : constant Int := Uints.Table (U).Length;
      D_Table : Udigits.Table_Ptr renames Udigits.Table;

      N_Bits  : constant Nat := Base_Bits * Length;
      N_Words : constant Nat := (N_Bits + 63) / 64;
      N_Padding_Bits : constant Nat := N_Words * 64 - N_Bits;

      Is_Negative : constant Boolean := D_Table (Loc) < Int (0);
      Words       : array (1 .. N_Words) of aliased uint64_t := (others => 0);

      Cur_Word : Nat := 1;
      Cur_Bit  : Nat := 64;

      function Ones (Length : Nat) return uint64_t is
        (uint64_t (Nat (2) ** Length - Nat (1)));
      --  Return a bitfield with the Length least significant bits set to 1

      procedure Push_Bits (Bits : uint64_t; Length : Nat);
      --  Push Bits (an integer Length bits arge) into the upper bits of Words
      --  right after the cursor. Update the cursor accordingly.

      ---------------
      -- Push_Bits --
      ---------------

      procedure Push_Bits (Bits : uint64_t; Length : Nat) is
         Buffer        : Unsigned_64 := Unsigned_64 (Bits);
         Buffer_Length : Nat := Length;

      begin
         --  Cur_Bit is how many bits are left inside the current word

         if Length > Cur_Bit then
            --  There are more bits to store than free bits inside the current
            --  word: first store the upper ones.

            declare
               Left_Over : constant Natural :=
                 Natural (Buffer_Length - Cur_Bit);
               --  Number of bits left in the buffer after storing high-order
               --  bits.

            begin
               --  First finish filling the current word

               Words (Cur_Word) := uint64_t
                 (Unsigned_64 (Words (Cur_Word))
                  or Shift_Right (Buffer, Left_Over));

               --  Then go to the next one, updating both the cursor and the
               --  bits to store.

               Cur_Word := Cur_Word + 1;
               Cur_Bit := 64;
               Buffer := Buffer and Unsigned_64 (Ones (Nat (Left_Over)));
               Buffer_Length := Nat (Left_Over);
            end;
         end if;

         Words (Cur_Word) := uint64_t
           (Unsigned_64 (Words (Cur_Word))
            or Shift_Left (Buffer, Natural (Cur_Bit - Buffer_Length)));
         Cur_Bit := Cur_Bit - Buffer_Length;
         if Cur_Bit = Nat (0) then
            Cur_Word := Cur_Word + 1;
            Cur_Bit := 64;
         end if;
      end Push_Bits;

   begin
      Push_Bits
        (Bits   => (if Is_Negative then Ones (N_Padding_Bits) else 0),
         Length => N_Padding_Bits);

      for I in 1 .. Length loop
         declare
            Digit : Int renames D_Table (Loc + I - 1);
            Bits  : constant uint64_t := uint64_t
              (if Digit < Nat (0) then 2 ** Base_Bits - Digit else Digit);
         begin
            Push_Bits (Bits, Base_Bits);
         end;
      end loop;

      return Const_Int_Of_Arbitrary_Precision
        (T,
         Words'Length,
         Words (Words'First)'Access);
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
