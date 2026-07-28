------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2026, AdaCore                     --
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

with LLVM.Types; use LLVM.Types;

with GNATLLVM; use GNATLLVM;

package Uintp.LLVM is

   function UI_To_Words (U : Uint) return Word_Array;
   --  Convert a Uint into an array of words representing the value

   function UI_To_LLVM (T : Type_T; U : Uint) return Value_T;
   --  Convert a Uint into an LLVM native integer constant

   function UI_To_ULL (U : Uint) return ULL;
   function "+" (U : Uint) return ULL renames UI_To_ULL;
   --  Like UI_To_Int, but for Unsigned_Long_Long

   function UI_Is_In_ULL_Range (U : Uint) return Boolean;
   --  Like UI_Is_In_Int_Range, but for Unsigned_Long_Long;

   function UI_From_ULL (V : ULL) return Uint;
   function "+" (V : ULL) return Uint renames UI_From_ULL;
   --  Like UI_From_LLI, but for ULL. Values in the highest half of ULL
   --  don't fit LLI, so build those in two steps. This is now reachable
   --  because UI_From_Words feeds it each raw 64-bit word of a wider
   --  constant, and any such word can have its top bit set regardless of
   --  the overall magnitude.

   function UI_From_Words
     (Words : Word_Array; Width : Pos; Is_Signed : Boolean) return Uint
     with Pre  => Words'Length = Nat'((Width + 63) / 64),
          Post =>
            (if   Is_Signed
             then UI_From_Words'Result <
                    UI_Expon (Uint_2, Nat'(Width - 1))
               and then UI_From_Words'Result >=
                          -UI_Expon (Uint_2, Nat'(Width - 1))
             else UI_From_Words'Result >= Uint_0
               and then UI_From_Words'Result < UI_Expon (Uint_2, Width));
   --  Reconstruct the value of an integer constant of the given bit Width
   --  from Words, which contains its bits with the low-order word first,
   --  as filled in by Get_Const_Int_Words. Any unused high-order bits of
   --  the last word must be zero. If Is_Signed, the bits are interpreted
   --  as a two's complement signed value, otherwise as an unsigned value.
   --
   --  We need to handle widths above 64 because System.Max_Int is now
   --  2 ** 127 - 1 (Standard_Long_Long_Long_Integer is 128 bits on the
   --  targets we support), so a folded bound or size of a 128-bit type no
   --  longer fits in a single word.

   function UI_S_Div
     (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint
   is
     (UI_Div (Left, Right));

   function UI_U_Div
     (Left : Valid_Uint; Right : Valid_Uint) return Valid_Uint
   is
     (UI_Div (Left, Right));

end Uintp.LLVM;
