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

   function UI_From_ULL (V : ULL) return Uint is
     (UI_From_LLI (LLI (V)));
   function "+" (V : ULL) return Uint renames UI_From_ULL;
   --  Like UI_From_Int, but for ULL.
   --  ??? This implementation doesn't work for the highest half of ULL,
   --  but we're not going to see sizes that large (the only place where
   --  this is used), so that's OK.

end Uintp.LLVM;
