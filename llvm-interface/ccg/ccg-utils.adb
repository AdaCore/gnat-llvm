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

with LLVM.Core; use LLVM.Core;

package body CCG.Utils is

   --------
   -- TP --
   --------

   function TP
     (S : String;
      Op1 : Value_T;
      Op2 : Value_T := No_Value_T;
      Op3 : Value_T := No_Value_T;
      T   : Type_T  := No_Type_T) return Str
   is
      Start     : Integer := S'First;
      Result    : Str     := No_Str;
      Mark_Seen : Boolean := False;
      B_Seen    : Boolean := False;
      D_Seen    : Boolean := False;
      Op        : Value_T;
      Last      : Integer;

   begin
      for J in S'Range loop

         --  If we've seen '#', look for 'B' or 'D'

         if Mark_Seen then
            if S (J) = 'B' then
               B_Seen := True;
            elsif S (J) = 'D' then
               D_Seen := True;

            --  If neither, then this is a number, representing which operand
            --  to output, possibly as modified by 'B' or 'D'.

            else
               Op := (case S (J) is when '1' => Op1, when '2' => Op2,
                                    when others => Op3);

               --  The end of any string to output is before our mark, which
               --  may be, e.g., #1 or #B2.

               Last := J - 2 - (if B_Seen or D_Seen then 1 else 0);
               if Start <= Last then
                  Result := Result & S (Start .. Last);
               end if;

               --  Output the (possibly modified) operand and reset for the
               --  next string and/or mark.

               if B_Seen then
                  Result := Result & Value_As_Basic_Block (Op);
               elsif D_Seen then
                  Result := Result & To_Data (Op);
               elsif S (J) = 'T' then
                  Result := Result & T;
               else
                  Result := Result & Op;
               end if;

               B_Seen    := False;
               D_Seen    := False;
               Mark_Seen := False;
               Start     := J + 1;
            end if;

         elsif S (J) = '#' then
            Mark_Seen := True;
         end if;
      end loop;

      --  See if we have a final string to output and output it if so

      if Start < S'Last then
         Result := Result & S (Start .. S'Last);
      end if;

      return Result;
   end TP;

   -----------------
   -- Update_Hash --
   ----------------

   procedure Update_Hash (H : in out Hash_Type; Key : Hash_Type) is
      function Shift_Left
        (Value  : Hash_Type;
         Amount : Natural) return Hash_Type;
      pragma Import (Intrinsic, Shift_Left);
   begin
      H := Key + Shift_Left (H, 6) + Shift_Left (H, 16) - H;
   end Update_Hash;

   -----------------
   -- Update_Hash --
   ----------------

   procedure Update_Hash (H : in out Hash_Type; S : String) is
   begin
      for C of S loop
         Update_Hash (H, Character'Pos (C));
      end loop;
   end Update_Hash;

   -----------------
   -- Update_Hash --
   -----------------

   procedure Update_Hash (H : in out Hash_Type; V : Value_T) is
   begin
      Update_Hash (H, Hash (V));
   end Update_Hash;

   -----------------
   -- Update_Hash --
   -----------------

   procedure Update_Hash (H : in out Hash_Type; T : Type_T) is
   begin
      Update_Hash (H, Hash (T));
   end Update_Hash;

   -----------------
   -- Update_Hash --
   -----------------

   procedure Update_Hash (H : in out Hash_Type; B : Basic_Block_T) is
   begin
      Update_Hash (H, Hash (B));
   end Update_Hash;

end CCG.Utils;
