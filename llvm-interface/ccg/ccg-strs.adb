------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with LLVM.Core; use LLVM.Core;

with Output; use Output;

with CCG.Environment; use CCG.Environment;
with CCG.Output;      use CCG.Output;
with CCG.Utils;       use CCG.Utils;

package body CCG.Strs is

   function Hash (S : Str_Record) return Hash_Type;
   function Hash (S : Str)        return Hash_Type is
     (Hash (S.all))
     with Pre => Present (S);
   --  Given an array of string components or an access to it (how we denote
   --  strings, return its hash value.

   procedure Update_Hash (H : in out Hash_Type; Flags : Value_Flags)
     with Inline;
   --  Update the hash key from Flags

   package Str_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Str,
      Hash         => Hash,
      Equivalent_Elements => "=");
   Str_Set : Str_Sets.Set;
   --  The set of all strings that we've made so far

   function Undup_Str (S : aliased Str_Record) return Str
     with Post => Present (Undup_Str'Result), Pure_Function;
   --  Get a unique Str corresponding to S

   -----------------
   -- Update_Hash --
   -----------------

   procedure Update_Hash (H : in out Hash_Type; Flags : Value_Flags) is
   begin
      Update_Hash (H, Flags.LHS);
      Update_Hash (H, Flags.Initializer);
      Update_Hash (H, Flags.Need_Unsigned);
      Update_Hash (H, Flags.Need_Signed);
      Update_Hash (H, Flags.Write_Type);
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
                     Update_Hash (H, Comp.Flags);
                     Update_Hash (H, Precedence'Pos (Comp.For_P));
                  when Typ =>
                     Update_Hash (H, Comp.T);
                  when BB =>
                     Update_Hash (H, Comp.B);
                  when Number =>
                     Update_Hash (H, Hash_Type (Comp.N));
               end case;
            end;
         end loop;
      end return;
   end Hash;

   ---------
   -- "=" --
   ---------

   function "=" (SL, SR : Str_Record) return Boolean is
      LenL  : constant Integer := SL.Length;
      LenR  : constant Integer := SR.Length;
      PosL  : Integer := 1;
      PosR  : Integer := 1;
      CharL : Integer := 1;
      CharR : Integer := 1;

   begin
      --  Two representations of strings are the same if all the
      --  characters, values, and types are the same and if the precedences
      --  are the same.

      if SL.P /= SR.P then
         return False;
      end if;

      --  We may not be dividing the strings into components the same way
      --  so we step along each component and exit if there's any
      --  difference.

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
         end if;

         --  Otherwise, we compare each type differently. For strings, we
         --  operate one character at a time. If the current character
         --  differs, the strings are different. Otherwise, advance to the
         --  next character, stepping to the next component if necessary.
         --  The string kind also must agree (and we unnecessarily check it
         --  every character for simplicity).

         if SL.Comps (PosL).Kind = Var_String then
            if SL.Comps (PosL).Str (CharL) /= SR.Comps (PosR).Str (CharR)
              or else SL.Comps (PosL).S_Kind /= SR.Comps (PosR).S_Kind
            then
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

         else
            --  Otherwise, they're different if the LLVM objects are different
            --  and we advance to the next position if not.

            case SL.Comps (PosL).Kind is
               when Var_String =>
                  pragma Assert (False);

               when Value =>
                  if SL.Comps (PosL).Val /= SR.Comps (PosR).Val
                    or else SL.Comps (PosL).Flags /= SR.Comps (PosR).Flags
                    or else SL.Comps (PosL).For_P /= SL.Comps (PosL).For_P
                  then
                     return False;
                  end if;

               when Typ =>
                  if SL.Comps (PosL).T /= SR.Comps (PosR).T then
                     return False;
                  end if;

               when BB =>
                  if SL.Comps (PosL).B /= SR.Comps (PosR).B then
                     return False;
                  end if;

               when Number =>
                  if SL.Comps (PosL).N /= SR.Comps (PosR).N then
                     return False;
                  end if;
            end case;

            PosL := PosL + 1;
            PosR := PosR + 1;
         end if;
      end loop;

   end "=";

   ---------------
   -- Undup_Str --
   ---------------

   function Undup_Str (S : aliased Str_Record) return Str is
      use Str_Sets;
      Position : constant Cursor := Find (Str_Set, S'Unchecked_Access);
      New_S    : Str;

   begin
      --  See if we already have this string in the set.  If so, return the
      --  element.  If not, make a copy in the heap and add that to the set.
      if Has_Element (Position) then
         return Element (Position);
      else
         New_S := new Str_Record'(S);
         Insert (Str_Set, New_S);
         return New_S;
      end if;
   end Undup_Str;

   ---------
   -- "+" --
   ---------

   function "+" (S : String) return Str is
   begin
      --  We have three cases. In the most common case, S is small enough that
      --  we only need one component. Also handle the null string case where
      --  we have no components.

      if S'Length = 0 then
         declare
            S_Rec  : aliased constant Str_Record (0) :=
              (0, Unknown, (1 .. 0 => <>));
            Result : constant Str                    := Undup_Str (S_Rec);

         begin
            return Result;
         end;
      elsif S'Length <= Str_Max then
         declare
            S_Rec  : aliased constant Str_Record (1) :=
              (1, Unknown, (1 => (Var_String, S'Length, Normal, S)));
            Result : constant Str                    := Undup_Str (S_Rec);

         begin
            return Result;
         end;
      else
         declare
            To_Do  : Integer := S'Length;
            I_Pos  : Integer := S'First;
            O_Pos  : Integer := 1;
            S_Rec  : aliased Str_Record ((S'Length + (Str_Max - 1)) / Str_Max);
            Result : Str;

         begin
            while To_Do > 0 loop
               declare
                  Count : constant Integer := Integer'Min (To_Do, Str_Max);

               begin
                  S_Rec.P             := Unknown;
                  S_Rec.Comps (O_Pos) := (Var_String, Count, Normal,
                                          S (I_Pos .. I_Pos + Count - 1));
                  I_Pos := I_Pos + Count;
                  To_Do := To_Do - Count;
                  O_Pos := O_Pos + 1;
               end;
            end loop;

            Result := Undup_Str (S_Rec);
            return Result;
         end;
      end if;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (V : Value_T) return Str is
      S_Rec  : aliased constant Str_Record (1) :=
        (1, Unknown, (1 => (Value, 1, V, Default_Flags, Unknown)));
      Result : constant Str := Undup_Str (S_Rec);

   begin
      Set_Is_Used (V);
      return Result;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (V : Value_T; VF : Value_Flag) return Str is
      S_Rec  : aliased constant Str_Record (1) :=
        (1, Unknown, (1 => (Value, 1, V, +VF, Unknown)));
      Result : constant Str := Undup_Str (S_Rec);

   begin
      Set_Is_Used (V);
      if VF = Write_Type then
         Maybe_Write_Typedef (Type_Of (V));
      end if;

      return Result;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (S : Str; VF : Value_Flag) return Str is
      S_Rec  : aliased constant Str_Record (1) :=
        (1, Unknown, (1 => (Value, 1, S.Comps (1).Val,
                            S.Comps (1).Flags or +VF, Unknown)));
      Result : constant Str := Undup_Str (S_Rec);

   begin
      if VF = Write_Type then
         Maybe_Write_Typedef (Type_Of (S.Comps (1).Val));
      end if;

      return Result;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (S : String; K : String_Kind) return Str is
      Orig   : constant Str := +S;
      S_Rec  : aliased Str_Record := Orig.all;
      Result : Str;

   begin
      S_Rec.Comps (1).S_Kind := K;
      Result := Undup_Str (S_Rec);
      return Result;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (S : String; P : Precedence) return Str is
   begin
      if S'Length <= Str_Max then
         declare
            S_Rec  : aliased constant Str_Record (1) :=
              (1, P, (1 => (Var_String, S'Length, Normal, S)));
            Result : constant Str := Undup_Str (S_Rec);

         begin
            return Result;
         end;
      else
         return +S + P;
      end if;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (V : Value_T; P : Precedence) return Str is
      S_Rec  : aliased constant Str_Record (1) :=
              (1, P, (1 => (Value, 1, V, Default_Flags, P)));
      Result : constant Str := Undup_Str (S_Rec);

   begin
      Set_Is_Used (V);
      return Result;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (S : Str; P : Precedence) return Str is

      procedure Update_Prec (Comps : in out Str_Component_Array);
      --  Update the precedence of any values of unknown precedence
      --  contained in Comps.

      S_Rec  : aliased Str_Record (S.Length) := (S.Length, P, S.Comps);
      Result : Str;

      -----------------
      -- Update_Prec --
      ----------------

      procedure Update_Prec (Comps : in out Str_Component_Array) is
      begin
         for Comp of Comps loop
            if Comp.Kind = Value and then Comp.For_P = Unknown then
               Comp.For_P := P;
            end if;
         end loop;
      end Update_Prec;

   begin
      --  If S is already of the desired precedence, return it

      if S.P = P then
         return S;

      --  Otherwise add parentheses if necessary.

      elsif Needs_Parens (S, P) then
         declare
            S_Rec_1 : aliased Str_Record (S.Length + 2);

         begin
            S_Rec_1.P                         := P;
            S_Rec_1.Comps (1)                 := (Var_String, 1, Normal, "(");
            S_Rec_1.Comps (2 .. S.Length + 1) := S.Comps;
            S_Rec_1.Comps (S.Length + 2)      := (Var_String, 1, Normal, ")");
            Update_Prec (S_Rec_1.Comps);
            Result := Undup_Str (S_Rec_1);
            return Result;
         end;
      end if;

      --  Otherwise, we set the precedence of the result (above) to the
      --  specified value. So we only have to update the values.

      Update_Prec (S_Rec.Comps);
      Result := Undup_Str (S_Rec);
      return Result;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (T : Type_T) return Str is
      S_Rec  : aliased constant Str_Record (1) :=
        (1, Unknown, (1 => (Typ, 1, T)));
      Result : constant Str := Undup_Str (S_Rec);
   begin
      Maybe_Write_Typedef (T);
      return Result;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (B : Basic_Block_T) return Str is
      S_Rec  : aliased constant Str_Record (1) :=
        (1, Unknown, (1 => (BB, 1, B)));
      Result : constant Str := Undup_Str (S_Rec);

   begin
      return Result;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (N : Nat) return Str is
      S_Rec  : aliased constant Str_Record (1) :=
        (1, Unknown, (1 => (Number, 1, N)));
      Result : constant Str := Undup_Str (S_Rec);

   begin
      return Result;
   end "+";

   ---------------
   -- Write_Str --
   ---------------

   procedure Write_Str (S : Str; Eol : Boolean := False) is
   begin
      for Comp of S.Comps loop
         case Comp.Kind is
            when Var_String =>
               if Comp.Str = Eol_Str then
                  Write_Eol;
               elsif Comp.S_Kind = Name then
                  Write_C_Name (Comp.Str);
               else
                  Write_Str (Comp.Str);
               end if;

            when Value =>
               Write_Value (Comp.Val, Flags => Comp.Flags,
                            For_Precedence => Comp.For_P);

            when Typ =>
               Write_Type (Comp.T);

            when BB =>
               Write_BB_Value (Comp.B);

            when Number =>
               Write_Int (Comp.N);
         end case;
      end loop;

      if Eol then
         Write_Eol;
      end if;
   end Write_Str;

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
      --  It is worth checking for a null string.

      if L'Length = 0 then
         return +R;
      elsif L'Length <= Str_Max then
         declare
            S_Rec  : aliased constant Str_Record (2) :=
              (2, Unknown,
               (1 => (Var_String, L'Length, Normal, L),
                2 => (Value, 1, R, Default_Flags, Unknown)));
            Result : constant Str := Undup_Str (S_Rec);

         begin
            Set_Is_Used (R);
            return Result;
         end;
      else
         return +L & (+R);
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : String; R : Type_T) return Str is
   begin
      if L'Length = 0 then
         return +R;
      elsif L'Length <= Str_Max then
         declare
            S_Rec  : aliased constant Str_Record (2) :=
              (2, Unknown,
               (1 => (Var_String, L'Length, Normal, L), 2 => (Typ, 1, R)));
            Result : constant Str := Undup_Str (S_Rec);

         begin
            Maybe_Write_Typedef (R);
            return Result;
         end;
      else
         return +L & (+R);
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : String; R : Basic_Block_T) return Str is
   begin
      if L'Length = 0 then
         return +R;
      elsif L'Length <= Str_Max then
         declare
            S_Rec  : aliased constant Str_Record (2) :=
              (2, Unknown,
               (1 => (Var_String, L'Length, Normal, L), 2 => (BB, 1, R)));
            Result : constant Str := Undup_Str (S_Rec);

         begin
            return Result;
         end;
      else
         return +L & (+R);
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : String; R : Nat) return Str is
   begin
      if L'Length = 0 then
         return +R;
      elsif L'Length <= Str_Max then
         declare
            S_Rec  : aliased constant Str_Record (2) :=
              (2, Unknown,
               (1 => (Var_String, L'Length, Normal, L), 2 => (Number, 1, R)));
            Result : constant Str := Undup_Str (S_Rec);

         begin
            return Result;
         end;
      else
         return +L & (Str'(+R));
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : String; R : Str) return Str is
   begin
      if L'Length = 0 then
         return R;
      elsif L'Length <= Str_Max then
         declare
            S_Rec  : aliased Str_Record (R.Length + 1);
            Result : Str;

         begin
            S_Rec.P         := R.P;
            S_Rec.Comps (1) := (Var_String, L'Length, Normal, L);
            S_Rec.Comps (2 .. R.Length + 1) := R.Comps;
            Result := Undup_Str (S_Rec);
            return Result;
         end;
      else
         return +L & R;
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Value_T; R : String) return Str is
   begin
      if R'Length = 0 then
         return +L;
      elsif R'Length <= Str_Max then
         declare
            S_Rec  : aliased constant Str_Record (2) :=
              (2, Primary,
               (1 => (Value, 1, L, Default_Flags, Unknown),
                2 => (Var_String, R'Length, Normal, R)));
            Result : constant Str := Undup_Str (S_Rec);

         begin
            Set_Is_Used (L);
            return Result;
         end;
      else
         return +L & (+R);
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Type_T; R : String) return Str is
   begin
      if R'Length = 0 then
         return +L;
      elsif R'Length <= Str_Max then
         declare
            S_Rec  : aliased constant Str_Record (2) :=
              (2, Unknown, (1 => (Typ, 1, L),
                            2 => (Var_String, R'Length, Normal, R)));
            Result : constant Str := Undup_Str (S_Rec);

         begin
            Maybe_Write_Typedef (L);
            return Result;
         end;
      else
         return +L & (+R);
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Basic_Block_T; R : String) return Str is
   begin
      if R'Length = 0 then
         return +L;
      elsif R'Length <= Str_Max then
         declare
            S_Rec  : aliased constant Str_Record (2) :=
              (2, Unknown, (1 => (BB, 1, L),
                            2 => (Var_String, R'Length, Normal, R)));
            Result : constant Str := Undup_Str (S_Rec);

         begin
            return Result;
         end;
      else
         return +L & (+R);
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Str; R : String) return Str is
   begin
      if No (L) then
         return +R;
      elsif R'Length = 0 then
         return L;
      elsif R'Length <= Str_Max then
         declare
            S_Rec  : aliased Str_Record (L.Length + 1);
            Result : Str;
         begin
            S_Rec.P                     := L.P;
            S_Rec.Comps (1 .. L.Length) := L.Comps;
            S_Rec.Comps (L.Length + 1)  := (Var_String, R'Length, Normal, R);
            Result := Undup_Str (S_Rec);
            return Result;
         end;
      else
         return L & (+R);
      end if;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Value_T; R : Nat) return Str is
      S_Rec  : aliased constant Str_Record (2) :=
        (2, Primary, (1 => (Value, 1, L, Default_Flags, Unknown),
                      2 => (Number, 1, R)));
      Result : constant Str := Undup_Str (S_Rec);

   begin
      Set_Is_Used (L);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Type_T; R : Nat) return Str is
      S_Rec  : aliased constant Str_Record (2) :=
        (2, Unknown, (1 => (Typ, 1, L), 2 => (Number, 1, R)));
      Result : constant Str := Undup_Str (S_Rec);

   begin
      Maybe_Write_Typedef (L);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Basic_Block_T; R : Nat) return Str is
      S_Rec  : aliased constant Str_Record (2) :=
        (2, Unknown, (1 => (BB, 1, L), 2 => (Number, 1, R)));
      Result : constant Str := Undup_Str (S_Rec);

   begin
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Value_T; R : Str) return Str is
      S_Rec  : aliased Str_Record (R.Length + 1);
      Result : Str;

   begin
      Set_Is_Used (L);
      S_Rec.P                         := R.P;
      S_Rec.Comps (1)                 := (Value, 1, L, Default_Flags, Unknown);
      S_Rec.Comps (2 .. R.Length + 1) := R.Comps;
      Result := Undup_Str (S_Rec);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Type_T; R : Str) return Str is
      S_Rec  : aliased Str_Record (R.Length + 1);
      Result : Str;

   begin
      Maybe_Write_Typedef (L);
      S_Rec.P                         := R.P;
      S_Rec.Comps (1)                 := (Typ, 1, L);
      S_Rec.Comps (2 .. R.Length + 1) := R.Comps;
      Result := Undup_Str (S_Rec);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Basic_Block_T; R : Str) return Str is
      S_Rec  : aliased Str_Record (R.Length + 1);
      Result : Str;

   begin
      S_Rec.P                         := R.P;
      S_Rec.Comps (1)                 := (BB, 1, L);
      S_Rec.Comps (2 .. R.Length + 1) := R.Comps;
      Result := Undup_Str (S_Rec);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Str; R : Value_T) return Str is
      S_Rec  : aliased Str_Record ((if Present (L) then L.Length + 1 else 0));
      Result : Str;

   begin
      if No (L) then
         return +R;
      end if;

      Set_Is_Used (R);
      S_Rec.P                     := L.P;
      S_Rec.Comps (1 .. L.Length) := L.Comps;
      S_Rec.Comps (L.Length + 1)  := (Value, 1, R, Default_Flags, Unknown);
      Result := Undup_Str (S_Rec);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Str; R : Type_T) return Str is
      S_Rec  : aliased Str_Record ((if Present (L) then L.Length + 1 else 0));
      Result : Str;

   begin
      if No (L) then
         return +R;
      end if;

      Maybe_Write_Typedef (R);
      S_Rec.P                     := L.P;
      S_Rec.Comps (1 .. L.Length) := L.Comps;
      S_Rec.Comps (L.Length + 1)  := (Typ, 1, R);
      Result := Undup_Str (S_Rec);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Str; R : Basic_Block_T) return Str is
      S_Rec  : aliased Str_Record ((if Present (L) then L.Length + 1 else 0));
      Result : Str;

   begin
      if No (L) then
         return +R;
      end if;

      S_Rec.P                     := L.P;
      S_Rec.Comps (1 .. L.Length) := L.Comps;
      S_Rec.Comps (L.Length + 1)  := (BB, 1, R);
      Result := Undup_Str (S_Rec);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Str; R : Nat) return Str is
      S_Rec  : aliased Str_Record ((if Present (L) then L.Length + 1 else 0));
      Result : Str;

   begin
      if No (L) then
         return +R;
      end if;

      S_Rec.P                     := L.P;
      S_Rec.Comps (1 .. L.Length) := L.Comps;
      S_Rec.Comps (L.Length + 1)  := (Number, 1, R);
      Result := Undup_Str (S_Rec);
      return Result;
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Str; R : Str) return Str is
      S_Rec  : aliased Str_Record ((if   Present (L) then L.Length + R.Length
                                    else 0));
      Result : Str;

   begin
      if No (L) or else L.Length = 0 then
         return R;
      elsif R.Length = 0 then
         return L;
      end if;

      S_Rec.P := Precedence'Max (L.P, R.P);
      S_Rec.Comps (1 .. L.Length) := L.Comps;
      S_Rec.Comps (L.Length + 1 .. L.Length + R.Length) := R.Comps;
      Result := Undup_Str (S_Rec);
      return Result;
   end "&";

   ------------------
   -- Single_Value --
   ------------------

   function Single_Value (S : Str) return Value_T is
   begin
      return Result : Value_T := No_Value_T do
         for Comp of S.Comps loop

            --  We want to ignore a reference with Phi_Temp because that's
            --  not a normal reference and shouldn't be treated as one.

            if Comp.Kind = Value and then not Comp.Flags.Phi_Temp then
               if Result /= No_Value_T then
                  Result := No_Value_T;
                  exit;
               else
                  Result := Comp.Val;
               end if;
            end if;
         end loop;
      end return;
   end Single_Value;

   ------------------
   -- Has_Unsigned --
   ------------------

   function Has_Unsigned (S : Str) return Boolean is
   begin
      for Comp of S.Comps loop

         --  It's unsigned if this is an unsigned reference to a value or
         --  the value may be unsigned (unless we've forced to signed
         --  already).

         if Comp.Kind = Value
           and then (Comp.Flags.Need_Unsigned
                       or else (not Comp.Flags.Need_Signed
                                  and then not Comp.Flags.Write_Type
                                  and then not Comp.Flags.Phi_Temp
                                  and then Might_Be_Unsigned (Comp.Val)))
         then
            return True;
         end if;
      end loop;

      return False;
   end Has_Unsigned;

   -------------
   -- Addr_Of --
   -------------

   function Addr_Of (S : Str; T : Type_T := No_Type_T) return Str is
      Result : Str := No_Str;

   begin
      --  If this is a single value handled normally that has a C expression,
      --  compute the address of that expression.

      if Is_Value (S) and then Present (Get_C_Value (S))
        and then not S.Comps (1).Flags.LHS
      then
         return Addr_Of (Get_C_Value (S), T);

      --  If this is "*" concatenated with some string, return the result
      --  of removing it.
      --  ??? What if this is "(* ... )"?

      elsif S.Length > 1 and then S.Comps (1).Kind = Var_String
        and then S.Comps (1).Str = "*"
      then
         declare
            S_Rec  : aliased constant Str_Record :=
              (S.Length - 1, S.P, S.Comps (2 .. S.Length));
            Result : constant Str                := Undup_Str (S_Rec);

         begin
            return Result;
         end;

      --  If this is an LHS, convert it into a normal reference

      elsif Is_Value (S) and then Get_Is_LHS (S)
        and then S.Comps (1).Flags.LHS
      then
         declare
            S_Rec  : aliased constant Str_Record :=
              (1, S.P, (1 => (Value, 0, S.Comps (1).Val, Default_Flags,
                              S.Comps (1).For_P)));
            Result : constant Str                := Undup_Str (S_Rec);
         begin
            return Result;
         end;

      --  If we're taking the address of a value that's of array type, we
      --  do nothing since a value of an array type represents the address
      --  of the array.

      elsif Is_Value (S) and then Get_Type_Kind (S) = Array_Type_Kind then
         return S;

      --  Otherwise, add the operator to take the address. If this is a
      --  value that's constant, we have to cast to the non-constant
      --  pointer type.

      else
         Result := "&" & (S + Unary);
         if Contains_One_Value (S) and then Get_Is_Constant (S) then
            Result :=
              "(" & (if Present (T) then T else Type_Of (S)) & "*) " & Result;
         end if;

         return Result;
      end if;
   end Addr_Of;

   -----------
   -- Deref --
   -----------

   function Deref (S : Str) return Str is
   begin
      --  If this is an LHS referenced normally, convert it into a
      --  reference to the name.

      if Is_Value (S) and then Get_Is_LHS (S)
        and then not S.Comps (1).Flags.LHS
      then
         declare
            S_Rec  : aliased constant Str_Record :=
              (1, S.P, (1 => (Value, 0, S.Comps (1).Val,
                              S.Comps (1).Flags or +LHS, S.Comps (1).For_P)));
            Result : constant Str                := Undup_Str (S_Rec);
         begin
            return Result;
         end;

      --  If this is a single value handled normally that has a C expression,
      --  compute the dereference of that expression.

      elsif Is_Value (S) and then Present (Get_C_Value (S))
        and then not S.Comps (1).Flags.LHS
      then
         return Deref (Get_C_Value (S.Comps (1).Val));

      --  If this is "&" concatenated with some string, return the result
      --  of removing it.

      elsif S.Length > 1 and then S.Comps (1).Kind = Var_String
        and then S.Comps (1).Str = "&"
      then
         declare
            S_Rec  : aliased constant Str_Record :=
              (S.Length - 1, S.P, S.Comps (2 .. S.Length));
            Result : constant Str                := Undup_Str (S_Rec);

         begin
            return Result;
         end;

      --  Otherwise, add the operator to dereference

      else
         return "*" & (S + Unary);
      end if;
   end Deref;

end CCG.Strs;
