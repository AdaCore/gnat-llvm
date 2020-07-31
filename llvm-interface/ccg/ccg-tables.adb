------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2013-2020, AdaCore                     --
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
with Ada.Containers.Hashed_Sets;

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

package body CCG.Tables is

   function UC_V is new Ada.Unchecked_Conversion (Value_T, System.Address);
   function UC_T is new Ada.Unchecked_Conversion (Type_T, System.Address);
   function UC_B is new Ada.Unchecked_Conversion (Basic_Block_T,
                                                  System.Address);

   function Hash (V : Value_T)       return Hash_Type is
     (Hash_Type'Mod (To_Integer (UC_V (V)) / (V'Size / 8)));
   function Hash (T : Type_T)        return Hash_Type is
     (Hash_Type'Mod (To_Integer (UC_T (T)) / (T'Size / 8)));
   function Hash (B : Basic_Block_T) return Hash_Type is
     (Hash_Type'Mod (To_Integer (UC_B (B)) / (B'Size / 8)));
   --  Hash functions for LLVM values, types, and basic blocks

   --  We want to compute a hash code for a Str_Component_Array that will be
   --  the same no matter how we break up a concatentation of strings
   --  that do not involve a Value_T, so we don't want to use Ada.Strings.Hash
   --  but instead accumulate the hash value piece by piece.

   procedure Update_Hash (H : in out Hash_Type; Key : Hash_Type) with Inline;
   --  Update H by including the value of Key

   procedure Update_Hash (H : in out Hash_Type; S : String)      with Inline;
   --  Update H taking into account the characters in S

   procedure Update_Hash (H : in out Hash_Type; V : Value_T)     with Inline;
   --  Update H taking into account the value V

   procedure Update_Hash (H : in out Hash_Type; T : Type_T)     with Inline;
   --  Update H taking into account the type T

   procedure Update_Hash (H : in out Hash_Type; B : Basic_Block_T) with Inline;
   --  Update H taking into account the type T

   function Hash (S : Str_Record) return Hash_Type;
   function Hash (S : Str)        return Hash_Type is
      (Hash (S.all));
   --  Given an array of string components or an access to it (how we denote
   --  strings, return its hash value.

   package Str_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Str,
      Hash         => Hash,
      Equivalent_Elements => "=");
   Str_Set : Str_Sets.Set;
   --  The set of all strings that we've made so far

   function Undup_Str (S : aliased Str_Record) return Str;
   --  Get a unique Str corresponding to S

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

   ----------
   -- Hash --
   ----------

   function Hash (S : Str_Record) return Hash_Type is
   begin
      return H : Hash_Type := 0 do
         for J in 1 .. S.Length loop

            declare
               Comp : constant Str_Component := S.Comps (J);

            begin
               case Comp.Kind is
                  when Var_String =>
                     Update_Hash (H, Comp.Str);
                  when Value =>
                     Update_Hash (H, Comp.Val);
                  when Typ =>
                     Update_Hash (H, Comp.T);
                  when BB =>
                     Update_Hash (H, Comp.B);
               end case;
            end;
         end loop;
      end return;
   end Hash;

   -------
   -- = --
   -------

   function "=" (SL, SR : Str_Record) return Boolean is
      LenL  : constant Integer := SL.Length;
      LenR  : constant Integer := SR.Length;
      PosL  : Integer := 1;
      PosR  : Integer := 1;
      CharL : Integer := 1;
      CharR : Integer := 1;

   begin
      --  Two representations of strings are the same if all the
      --  characters, values, and types are the same.  However, we may not
      --  be dividing the strings into components the same way so we step
      --  along each component and exit if there's any difference.

      loop
         --  If we've reached the end of both strings, they're the same.
         --  Otherwise, if we've reached the end of one of them, they're
         --  different.

         if PosL > LenL and then PosR > LenR then
            return True;
         elsif PosL > LenL or else PosR > LenR then
            return False;

         --  If the types of a component differ, they're not equal

         elsif SL.Comps (PosL).Kind /= SR.Comps (PosR).Kind then
            return False;

         --  For strings, we operate one character at a time.  If the
         --  current character differs, the strings are different.
         --  Otherwise, advance to the next character, stepping to the
         --  next component if necessary.

         elsif SL.Comps (PosL).Kind = Var_String then
            if SL.Comps (PosL).Str (CharL) /= SR.Comps (PosR).Str (CharR) then
               return False;
            else
               CharL := CharL + 1;
               CharR := CharR + 1;
               if CharL > SL.Comps (PosL).Length then
                  PosL  := PosL + 1;
                  CharL := 1;
               end if;

               if CharR > SR.Comps (PosR).Length then
                  PosR  := PosR + 1;
                  CharR := 1;
               end if;
            end if;

         --  Otherwise, they're different if the LLVM objects are different
         --  and we advance to the next position if not.

         elsif SL.Comps (PosL).Kind = Value then
            if SL.Comps (PosL).Val /= SR.Comps (PosR).Val then
               return False;
            else
               PosL := PosL + 1;
               PosR := PosR + 1;
            end if;

         elsif SL.Comps (PosL).Kind = Typ then
            if SL.Comps (PosL).T /= SR.Comps (PosR).T then
               return False;
            else
               PosL := PosL + 1;
               PosR := PosR + 1;
            end if;

         elsif SL.Comps (PosL).Kind = BB then
            if SL.Comps (PosL).B /= SR.Comps (PosR).B then
               return False;
            else
               PosL := PosL + 1;
               PosR := PosR + 1;
            end if;
         end if;
      end loop;

   end "=";

   ---------------
   -- Undup_Str --
   ---------------

   function Undup_Str (S : aliased Str_Record) return Str is
      Position : constant Str_Sets.Cursor :=
        Str_Sets.Find (Str_Set, S'Unchecked_Access);
      New_S    : Str;

   begin
      --  See if we already have this string in the set.  If so, return the
      --  element.  If not, make a copy in the heap and add that to the set.
      if Str_Sets.Has_Element (Position) then
         return Str_Sets.Element (Position);
      else
         New_S := new Str_Record'(S);
         Str_Sets.Insert (Str_Set, New_S);
         return New_S;
      end if;
   end Undup_Str;

   ------------
   -- To_Str --
   ------------

   function To_Str (S : String) return Str is
   begin
      --  We have two cases.  In the most common case, S is small enough that
      --  we only need one component.

      if S'Length <= Str_Max then
         declare
            S_Rec : aliased constant Str_Record (1) :=
              (1, (1 => (Var_String, S'Length, S)));

         begin
            return Undup_Str (S_Rec);
         end;
      else
         declare
            To_Do : Integer := S'Length;
            I_Pos : Integer := S'First;
            O_Pos : Integer := 1;
            S_Rec : aliased Str_Record ((S'Length + (Str_Max - 1)) / Str_Max);

         begin
            while To_Do > 0 loop
               declare
                  Count : constant Integer := Integer'Min (To_Do, Str_Max);

               begin
                  S_Rec.Comps (O_Pos) := (Var_String, Count,
                                          S (I_Pos .. I_Pos + Count - 1));
                  I_Pos := I_Pos + Count;
                  To_Do := To_Do - Count;
                  O_Pos := O_Pos + 1;
               end;
            end loop;

            return Undup_Str (S_Rec);
         end;
      end if;
   end To_Str;

   ------------
   -- To_Str --
   ------------

   function To_Str (V : Value_T) return Str is
      S_Rec : aliased constant Str_Record (1) := (1, (1 => (Value, 1, V)));
   begin
      return Undup_Str (S_Rec);
   end To_Str;

   ------------
   -- To_Str --
   ------------

   function To_Str (T : Type_T) return Str is
      S_Rec : aliased constant Str_Record (1) := (1, (1 => (Typ, 1, T)));
   begin
      return Undup_Str (S_Rec);
   end To_Str;

   ------------
   -- To_Str --
   ------------

   function To_Str (B : Basic_Block_T) return Str is
      S_Rec : aliased constant Str_Record (1) := (1, (1 => (BB, 1, B)));
   begin
      return Undup_Str (S_Rec);
   end To_Str;

   ---------
   -- "&" --
   ---------

   function "&" (L : String; R : Value_T) return Str is
   begin
      --  If the string is small enough, we just construct a two-component
      --  object.  Otherwise (a rare case), we construct a Str for both and
      --  concatenate.  We could check for the case where we could make a
      --  new component that concatenated the strings from both sides, but
      --  the number of times that would happen isn't worth the trouble.

      if L'Length <= Str_Max then
         declare
            S_Rec : aliased constant Str_Record (2) :=
              (2, (1 => (Var_String, L'Length, L), 2 => (Value, 1, R)));

         begin
            return Undup_Str (S_Rec);
         end;
      else
         return To_Str (L) & To_Str (R);
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : String; R : Type_T) return Str is
   begin
      if L'Length <= Str_Max then
         declare
            S_Rec : aliased constant Str_Record (2) :=
              (2, (1 => (Var_String, L'Length, L), 2 => (Typ, 1, R)));

         begin
            return Undup_Str (S_Rec);
         end;
      else
         return To_Str (L) & To_Str (R);
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : String; R : Basic_Block_T) return Str is
   begin
      if L'Length <= Str_Max then
         declare
            S_Rec : aliased constant Str_Record (2) :=
              (2, (1 => (Var_String, L'Length, L), 2 => (BB, 1, R)));

         begin
            return Undup_Str (S_Rec);
         end;
      else
         return To_Str (L) & To_Str (R);
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : String; R : Str) return Str is
   begin
      if L'Length <= Str_Max then
         declare
            S_Rec : aliased Str_Record (R.Length + 1);

         begin
            S_Rec.Comps (1) := (Var_String, L'Length, L);
            S_Rec.Comps (2 .. R.Length + 1) := R.Comps;
            return Undup_Str (S_Rec);
         end;
      else
         return To_Str (L) & R;
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Value_T; R : String) return Str is
   begin
      if R'Length <= Str_Max then
         declare
            S_Rec : aliased constant Str_Record (2) :=
              (2, (1 => (Value, 1, L), 2 => (Var_String, R'Length, R)));

         begin
            return Undup_Str (S_Rec);
         end;
      else
         return To_Str (L) & To_Str (R);
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Type_T; R : String) return Str is
   begin
      if R'Length <= Str_Max then
         declare
            S_Rec : aliased constant Str_Record (2) :=
              (2, (1 => (Typ, 1, L), 2 => (Var_String, R'Length, R)));

         begin
            return Undup_Str (S_Rec);
         end;
      else
         return To_Str (L) & To_Str (R);
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Basic_Block_T; R : String) return Str is
   begin
      if R'Length <= Str_Max then
         declare
            S_Rec : aliased constant Str_Record (2) :=
              (2, (1 => (BB, 1, L), 2 => (Var_String, R'Length, R)));

         begin
            return Undup_Str (S_Rec);
         end;
      else
         return To_Str (L) & To_Str (R);
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Str; R : String) return Str is
   begin
      if R'Length <= Str_Max then
         declare
            S_Rec : aliased Str_Record (L.Length + 1);

         begin
            S_Rec.Comps (1 .. L.Length) := L.Comps;
            S_Rec.Comps (L.Length + 1) := (Var_String, R'Length, R);
            return Undup_Str (S_Rec);
         end;
      else
         return L & To_Str (R);
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Value_T; R : Value_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (Value, 1, L), 2 => (Value, 1, R)));

   begin
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Type_T; R : Type_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (Typ, 1, L), 2 => (Typ, 1, R)));

   begin
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Basic_Block_T; R : Basic_Block_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (BB, 1, L), 2 => (BB, 1, R)));

   begin
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Value_T; R : Type_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (Value, 1, L), 2 => (Typ, 1, R)));

   begin
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Value_T; R : Basic_Block_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (Value, 1, L), 2 => (BB, 1, R)));

   begin
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Type_T; R : Value_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (Typ, 1, L), 2 => (Value, 1, R)));

   begin
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Type_T; R : Basic_Block_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (Typ, 1, L), 2 => (BB, 1, R)));

   begin
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Basic_Block_T; R : Value_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (BB, 1, L), 2 => (Value, 1, R)));

   begin
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Basic_Block_T; R : Type_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (BB, 1, L), 2 => (Typ, 1, R)));

   begin
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Value_T; R : Str) return Str is
      S_Rec : aliased Str_Record (R.Length + 1);

   begin
      S_Rec.Comps (1) := (Value, 1, L);
      S_Rec.Comps (2 .. R.Length + 1) := R.Comps;
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Type_T; R : Str) return Str is
      S_Rec : aliased Str_Record (R.Length + 1);

   begin
      S_Rec.Comps (1) := (Typ, 1, L);
      S_Rec.Comps (2 .. R.Length + 1) := R.Comps;
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Basic_Block_T; R : Str) return Str is
      S_Rec : aliased Str_Record (R.Length + 1);

   begin
      S_Rec.Comps (1) := (BB, 1, L);
      S_Rec.Comps (2 .. R.Length + 1) := R.Comps;
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Str; R : Value_T) return Str is
      S_Rec : aliased Str_Record (L.Length + 1);

   begin
      S_Rec.Comps (1 .. L.Length) := L.Comps;
      S_Rec.Comps (L.Length + 1) := (Value, 1, R);
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Str; R : Type_T) return Str is
      S_Rec : aliased Str_Record (L.Length + 1);

   begin
      S_Rec.Comps (1 .. L.Length) := L.Comps;
      S_Rec.Comps (L.Length + 1) := (Typ, 1, R);
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Str; R : Basic_Block_T) return Str is
      S_Rec : aliased Str_Record (L.Length + 1);

   begin
      S_Rec.Comps (1 .. L.Length) := L.Comps;
      S_Rec.Comps (L.Length + 1) := (BB, 1, R);
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Str; R : Str) return Str is
      S_Rec : aliased Str_Record (L.Length + R.Length);

   begin
      S_Rec.Comps (1 .. L.Length) := L.Comps;
      S_Rec.Comps (L.Length + 1 .. L.Length + R.Length) := R.Comps;
      return Undup_Str (S_Rec);
   end "&";

   ------------------------
   --  Initialize_Tables --
   ------------------------

   procedure Initialize_Tables is
   begin
      null;
   end Initialize_Tables;

end CCG.Tables;
