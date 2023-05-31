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

with Interfaces; use Interfaces;

with stdint_h; use stdint_h;

with LLVM.Core; use LLVM.Core;

package body Uintp.LLVM is

   subtype Unsigned_64 is Interfaces.Unsigned_64;

   function Big_UI_To_Words (U : Uint) return Word_Array;
   --  Convert a Uint into an array of words representing the value

   ----------------
   -- UI_To_LLVM --
   ----------------

   function UI_To_LLVM (T : Type_T; U : Uint) return Value_T is
   begin
      if UI_Is_In_Int_Range (U) then
         return Const_Int (T, ULL (UI_To_Int (U)), True);
      else
         declare
            Words  : Word_Array       := Big_UI_To_Words (U);
            Result : constant Value_T := Const_Int_Of_Arbitrary_Precision
              (T, Words'Length, Words (Words'First)'Access);

         begin
            return (if U < Uint_0 then Const_Neg (Result) else Result);
         end;
      end if;
   end UI_To_LLVM;

   ---------------
   -- UI_To_ULL --
   ---------------

   function UI_To_ULL (U : Uint) return ULL is
   begin
      if UI_Is_In_Int_Range (U) then
         return ULL (UI_To_Int (U));
      else
         declare
            Words : constant Word_Array := Big_UI_To_Words (U);
         begin
            --  We assume that ULL is no wider than an element of Word_Array,
            --  but is no narrower than Int.

            pragma Assert (ULL'Size <= Words (Words'First)'Size
                             and then ULL'Size >= Int'Size);

            --  If this takes up more than one word, it's too large.
            --  Otherwise, return the value.

            if Words'Length /= 1 then
               raise Constraint_Error;
            else
               return ULL (Words (Words'First));
            end if;
         end;
      end if;

   end UI_To_ULL;

   ------------------------
   -- UI_Is_In_ULL_Range --
   ------------------------

   function UI_Is_In_ULL_Range (U : Uint) return Boolean is
   begin
      return UI_Is_In_Int_Range (U)
        or else Big_UI_To_Words (U)'Length = 1;
   end UI_Is_In_ULL_Range;

   -----------------
   -- UI_To_Words --
   -----------------

   function UI_To_Words (U : Uint) return Word_Array is
      Words : Word_Array (1 .. 1);
   begin
      --  If this fits in an int, get that value. We can't use
      --  Big_UI_To_Words for many integer values due to the way Uint works.

      if UI_Is_In_Int_Range (U) then
         Words (1) := uint64_t (UI_To_Int (U));
         return Words;
      else
         return Big_UI_To_Words (U);
      end if;
   end UI_To_Words;

   ---------------------
   -- Big_UI_To_Words --
   ---------------------

   function Big_UI_To_Words (U : Uint) return Word_Array is
      D_Table        : Udigits.Table_Ptr renames Udigits.Table;
      Loc            : constant Int     := Uints.Table (U).Loc;
      Length         : constant Pos     := Uints.Table (U).Length;
      N_Bits         : constant Pos     := Base_Bits * Length;
      N_Words        : constant Pos     := (N_Bits + 63) / 64;
      N_Padding_Bits : constant Pos     := N_Words * 64 - N_Bits;
      Words          : Word_Array (1 .. N_Words) := (others => 0);
      Cur_Word       : Nat              := N_Words;
      Cur_Bit        : Nat              := 64;

      function Ones (Length : Nat) return uint64_t is
         (uint64_t (2 ** Integer (Length) - 1));
      --  Return a bitfield with the Length least significant bits set to 1

      procedure Push_Bits (Bits : uint64_t; Length : Nat);
      --  Push Bits (an integer Length bits arge) into the upper bits of Words
      --  right after the cursor. Update the cursor accordingly.

      ---------------
      -- Push_Bits --
      ---------------

      procedure Push_Bits (Bits : uint64_t; Length : Nat) is
         Buffer        : Unsigned_64 := Unsigned_64 (Bits);
         Buffer_Length : Nat         := Length;

      begin
         --  Cur_Bit is how many bits are left inside the current word

         if Length > Cur_Bit then
            --  There are more bits to store than free bits inside the
            --  current word: first store the upper ones.

            declare
               Left_Over : constant Nat := Buffer_Length - Cur_Bit;
               --  Number of bits left in the buffer after storing
               --  high-order bits.

            begin
               --  First finish filling the current word

               Words (Cur_Word) := uint64_t
                 (Unsigned_64 (Words (Cur_Word))
                  or Shift_Right (Buffer, Integer (Left_Over)));

               --  Then go to the next one, updating both the cursor and the
               --  bits to store.

               Cur_Word      := Cur_Word - 1;
               Cur_Bit       := 64;
               Buffer        := Buffer and Unsigned_64 (Ones (Left_Over));
               Buffer_Length := Left_Over;
            end;
         end if;

         Words (Cur_Word) := uint64_t (Unsigned_64 (Words (Cur_Word))
            or Shift_Left (Buffer, Integer (Cur_Bit - Buffer_Length)));
         Cur_Bit := Cur_Bit - Buffer_Length;

         if Cur_Bit = Nat (0) then
            Cur_Word := Cur_Word - 1;
            Cur_Bit  := 64;
         end if;
      end Push_Bits;

   begin
      --  Note that LLVM takes the first word passed to it as the low-order
      --  part of the constant, not the high-order, as might be expected.
      --  Also, the absolute value of the constant is what's stored in the
      --  Uint table and we later negate if it's negative.

      Push_Bits (0, N_Padding_Bits);

      for J in 1 .. Nat (Length) loop
         Push_Bits (uint64_t (abs D_Table (Loc + J - 1)), Base_Bits);
      end loop;

      return Words;
   end Big_UI_To_Words;

end Uintp.LLVM;
