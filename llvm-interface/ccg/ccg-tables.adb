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
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with Output; use Output;
with Table;  use Table;

with CCG.Output; use CCG.Output;

package body CCG.Tables is

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

   procedure Update_Hash (H : in out Hash_Type; V : Value_T)
     with Pre => Present (V), Inline;
   --  Update H taking into account the value V

   procedure Update_Hash (H : in out Hash_Type; T : Type_T)
     with Pre => Present (T), Inline;
   --  Update H taking into account the type T

   procedure Update_Hash (H : in out Hash_Type; B : Basic_Block_T)
     with Pre => Present (B), Inline;
   --  Update H taking into account the type T

   function Hash (S : Str_Record) return Hash_Type;
   function Hash (S : Str)        return Hash_Type is
     (Hash (S.all))
     with Pre => Present (S);
   --  Given an array of string components or an access to it (how we denote
   --  strings, return its hash value.

   package Str_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Str,
      Hash         => Hash,
      Equivalent_Elements => "=");
   Str_Set : Str_Sets.Set;
   --  The set of all strings that we've made so far

   function Undup_Str (S : aliased Str_Record) return Str
     with Post => Present (Undup_Str'Result), Pure_Function;
   --  Get a unique Str corresponding to S

   --  We maintain tables that give information we need about LLVM values,
   --  types, and basic blocks.  We use a hashed map from the LLVM address
   --  to an index in our Table.  That index can also be used to provide a
   --  unique identifier for the value or basic block (we don't need it for
   --  types) when outputting a string representing the value or block.
   --  We first create the structures that contain the data for each object,
   --  then the tables, then the maps.

   type Value_Data is record
      C_Value         : Str;
      --  If Present, a string that represents the value of the Value_T

      No_Name         : Boolean;
      --  True is there's no LLVM name for this value; we use the ordinal

      Is_Decl_Output  : Boolean;
      --  True if we wrote any needed decl for this value

      Is_Entry_Alloca : Boolean;
      --  True if this value represents and alloca in the entry block.
      --  In that case, from a C perspective, a use of a value in LLVM IR
      --  represents the address of the value; only "load" or "store"
      --  instruction actually accesses the value.

      Output_Idx      : Nat;
      --  A positive number if we've assigned an ordinal to use as
      --  part of the name for this anonymous value.

   end record;

   type Type_Data is record
      Is_Typedef_Output : Boolean;
      --  True if this is a type either for which we don't write a typedef
      --  or if it is and we've written that typedef previously.

      Output_Idx        : Nat;
      --  A positive number if we've assigned an ordinal to use as
      --  part of the name for this anonymous type.

   end record;

   type BB_Data is record
      Is_Entry   : Boolean;
      --  True if this is the entry basic block for some function

      Was_Output : Boolean;
      --  True if this basic block has already been output

      Output_Idx : Nat;
      --  A positive number if we've assigned an ordinal to use as
      --  part of the name for this block.

   end record;

   type Value_Idx is new Nat;
   type Type_Idx  is new Nat;
   type BB_Idx    is new Nat;

   No_Value_Idx : constant Value_Idx := 0;
   No_Type_Idx  : constant Type_Idx  := 0;
   No_BB_Idx    : constant BB_Idx    := 0;

   function Present (X : Value_Idx) return Boolean is (X /= No_Value_Idx);
   function Present (X : Type_Idx)  return Boolean is (X /= No_Type_Idx);
   function Present (X : BB_Idx)    return Boolean is (X /= No_BB_Idx);

   package Value_Data_Table is new Table.Table
     (Table_Component_Type => Value_Data,
      Table_Index_Type     => Value_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 500,
      Table_Increment      => 100,
      Table_Name           => "Value_Data_Table");

   package Type_Data_Table is new Table.Table
     (Table_Component_Type => Type_Data,
      Table_Index_Type     => Type_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 50,
      Table_Name           => "Type_Data_Table");

   package BB_Data_Table is new Table.Table
     (Table_Component_Type => BB_Data,
      Table_Index_Type     => BB_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 50,
      Table_Name           => "BB_Data_Table");

   package Value_Data_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Value_T,
      Element_Type    => Value_Idx,
      Hash            => Hash,
      Equivalent_Keys => "=");
   Value_Data_Map : Value_Data_Maps.Map;

   package Type_Data_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Type_T,
      Element_Type    => Type_Idx,
      Hash            => Hash,
      Equivalent_Keys => "=");
   Type_Data_Map : Type_Data_Maps.Map;

   package BB_Data_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Basic_Block_T,
      Element_Type    => BB_Idx,
      Hash            => Hash,
      Equivalent_Keys => "=");
   BB_Data_Map : BB_Data_Maps.Map;

   Output_Idx : Nat := 1;
   --  The next output index to use for values, types, and basic blocks

   --  Functions to return the corresponding index for a value, type, or
   --  basic block and whether to create one if one isn't present.

   function Value_Data_Idx (V : Value_T; Create : Boolean) return Value_Idx
     with Pre => Present (V), Pure_Function;
   function Type_Data_Idx  (T : Type_T; Create : Boolean) return Type_Idx
     with Pre => Present (T), Pure_Function;
   function BB_Data_Idx    (B : Basic_Block_T; Create : Boolean) return BB_Idx
     with Pre => Present (B), Pure_Function;

   procedure Maybe_Write_Decl (V : Value_T)
     with Pre => Present (V), Post => Get_Is_Decl_Output (V);
   --  See if we need to write a declaration for V and write one if so

   procedure Maybe_Write_Typedef (T : Type_T)
     with Pre => Present (T), Post => Get_Is_Typedef_Output (T);
   --  See if we need to write a typedef for T and write one if so

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
                  when Value | Data_Value =>
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
      Maybe_Write_Decl (V);
      return Undup_Str (S_Rec);
   end To_Str;

   --------------------
   -- To_Str_As_Data --
   --------------------

   function To_Str_As_Data (V : Value_T) return Str is
      S_Rec : aliased constant Str_Record (1) :=
        (1, (1 => (Data_Value, 1, V)));
   begin
      Maybe_Write_Decl (V);
      return Undup_Str (S_Rec);
   end To_Str_As_Data;

   ------------
   -- To_Str --
   ------------

   function To_Str (T : Type_T) return Str is
      S_Rec : aliased constant Str_Record (1) := (1, (1 => (Typ, 1, T)));
   begin
      Maybe_Write_Typedef (T);
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

   ---------------
   -- Write_Str --
   ---------------

   procedure Write_Str (S : Str; Eol : Boolean := False) is
   begin
      for Comp of S.Comps loop
         case Comp.Kind is

            when Var_String =>
               Write_Str (Comp.Str);

            when Value =>
               if Get_Is_Entry_Alloca (Comp.Val) then
                  Write_Str ("&");
               end if;

               Write_Value (Comp.Val);

            when Data_Value =>
               Write_Value (Comp.Val);

            when Typ =>
               Write_Type (Comp.T);

            when BB =>
               Write_BB (Comp.B);
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
         return To_Str (R);
      elsif L'Length <= Str_Max then
         declare
            S_Rec : aliased constant Str_Record (2) :=
              (2, (1 => (Var_String, L'Length, L), 2 => (Value, 1, R)));

         begin
            Maybe_Write_Decl (R);
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
      if L'Length = 0 then
         return To_Str (R);
      elsif L'Length <= Str_Max then
         declare
            S_Rec : aliased constant Str_Record (2) :=
              (2, (1 => (Var_String, L'Length, L), 2 => (Typ, 1, R)));

         begin
            Maybe_Write_Typedef (R);
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
      if L'Length = 0 then
         return To_Str (R);
      elsif L'Length <= Str_Max then
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
      if L'Length = 0 then
         return R;
      elsif L'Length <= Str_Max then
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
      if R'Length = 0 then
         return To_Str (L);
      elsif R'Length <= Str_Max then
         declare
            S_Rec : aliased constant Str_Record (2) :=
              (2, (1 => (Value, 1, L), 2 => (Var_String, R'Length, R)));

         begin
            Maybe_Write_Decl (L);
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
      if R'Length = 0 then
         return To_Str (L);
      elsif R'Length <= Str_Max then
         declare
            S_Rec : aliased constant Str_Record (2) :=
              (2, (1 => (Typ, 1, L), 2 => (Var_String, R'Length, R)));

         begin
            Maybe_Write_Typedef (L);
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
      if R'Length = 0 then
         return To_Str (L);
      elsif R'Length <= Str_Max then
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
      if R'Length = 0 then
         return L;
      elsif R'Length <= Str_Max then
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
      Maybe_Write_Decl (L);
      Maybe_Write_Decl (R);
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Type_T; R : Type_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (Typ, 1, L), 2 => (Typ, 1, R)));

   begin
      Maybe_Write_Typedef (L);
      Maybe_Write_Typedef (R);
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
      Maybe_Write_Decl (L);
      Maybe_Write_Typedef (R);
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Value_T; R : Basic_Block_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (Value, 1, L), 2 => (BB, 1, R)));

   begin
      Maybe_Write_Decl (L);
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Type_T; R : Value_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (Typ, 1, L), 2 => (Value, 1, R)));

   begin
      Maybe_Write_Decl (R);
      Maybe_Write_Typedef (L);
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Type_T; R : Basic_Block_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (Typ, 1, L), 2 => (BB, 1, R)));

   begin
      Maybe_Write_Typedef (L);
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Basic_Block_T; R : Value_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (BB, 1, L), 2 => (Value, 1, R)));

   begin
      Maybe_Write_Decl (R);
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Basic_Block_T; R : Type_T) return Str is
      S_Rec : aliased constant Str_Record (2) :=
        (2, (1 => (BB, 1, L), 2 => (Typ, 1, R)));

   begin
      Maybe_Write_Typedef (R);
      return Undup_Str (S_Rec);
   end "&";

   ---------
   -- "&" --
   ---------

   function "&" (L : Value_T; R : Str) return Str is
      S_Rec : aliased Str_Record (R.Length + 1);

   begin
      Maybe_Write_Decl (L);
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
      Maybe_Write_Typedef (L);
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
      Maybe_Write_Decl (R);
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
      Maybe_Write_Typedef (R);
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

   --------------------
   -- Value_Data_Idx --
   --------------------

   function Value_Data_Idx (V : Value_T; Create : Boolean) return Value_Idx
   is
      use Value_Data_Maps;
      Position : constant Cursor := Find (Value_Data_Map, V);

   begin
      if Has_Element (Position) then
         return Element (Position);
      elsif not Create then
         return No_Value_Idx;
      else
         Value_Data_Table.Append ((C_Value         => null,
                                   No_Name         => False,
                                   Is_Decl_Output  => False,
                                   Is_Entry_Alloca => False,
                                   Output_Idx      => 0));
         Insert (Value_Data_Map, V, Value_Data_Table.Last);
         return Value_Data_Table.Last;
      end if;
   end Value_Data_Idx;

   -------------------
   -- Type_Data_Idx --
   -------------------

   function Type_Data_Idx (T : Type_T; Create : Boolean) return Type_Idx
   is
      use Type_Data_Maps;
      Position : constant Cursor := Find (Type_Data_Map, T);

   begin
      if Has_Element (Position) then
         return Element (Position);
      elsif not Create then
         return No_Type_Idx;
      else
         Type_Data_Table.Append ((Is_Typedef_Output => False,
                                  Output_Idx        => 0));
         Insert (Type_Data_Map, T, Type_Data_Table.Last);
         return Type_Data_Table.Last;
      end if;
   end Type_Data_Idx;

   -----------------
   -- BB_Data_Idx --
   -----------------

   function BB_Data_Idx (B : Basic_Block_T; Create : Boolean) return BB_Idx
   is
      use BB_Data_Maps;
      Position : constant Cursor := Find (BB_Data_Map, B);

   begin
      if Has_Element (Position) then
         return Element (Position);
      elsif not Create then
         return No_BB_Idx;
      else
         BB_Data_Table.Append ((Is_Entry   => False,
                                Was_Output => False,
                                Output_Idx => 0));
         Insert (BB_Data_Map, B, BB_Data_Table.Last);
         return BB_Data_Table.Last;
      end if;
   end BB_Data_Idx;

   -----------------
   -- Get_C_Value --
   -----------------

   function Get_C_Value (V : Value_T) return Str is
      Idx : constant Value_Idx := Value_Data_Idx (V, Create => False);

   begin
      return (if   Present (Idx) then Value_Data_Table.Table (Idx).C_Value
              else null);
   end Get_C_Value;

   -----------------
   -- Get_No_Name --
   -----------------

   function Get_No_Name (V : Value_T) return Boolean is
      Idx : constant Value_Idx := Value_Data_Idx (V, Create => False);

   begin
      return Present (Idx) and then Value_Data_Table.Table (Idx).No_Name;
   end Get_No_Name;

   ------------------------
   -- Get_Is_Decl_Output --
   ------------------------

   function Get_Is_Decl_Output (V : Value_T) return Boolean is
      Idx : constant Value_Idx := Value_Data_Idx (V, Create => False);

   begin
      return Present (Idx)
        and then Value_Data_Table.Table (Idx).Is_Decl_Output;

   end Get_Is_Decl_Output;

   -------------------------
   -- Get_Is_Entry_Alloca --
   -------------------------

   function Get_Is_Entry_Alloca (V : Value_T) return Boolean is
      Idx : constant Value_Idx := Value_Data_Idx (V, Create => False);

   begin
      return Present (Idx)
        and then Value_Data_Table.Table (Idx).Is_Entry_Alloca;

   end Get_Is_Entry_Alloca;

   -----------------
   -- Set_C_Value --
   -----------------

   procedure Set_C_Value (V : Value_T; S : Str) is
      Idx : constant Value_Idx := Value_Data_Idx (V, Create => True);

   begin
      Value_Data_Table.Table (Idx).C_Value := S;
   end Set_C_Value;

   -----------------
   -- Set_No_Name --
   -----------------

   procedure Set_No_Name (V : Value_T; B : Boolean := True) is
      Idx : constant Value_Idx := Value_Data_Idx (V, Create => True);

   begin
      Value_Data_Table.Table (Idx).No_Name := B;
   end Set_No_Name;

   ------------------------
   -- Set_Is_Decl_Output --
   ------------------------

   procedure Set_Is_Decl_Output (V : Value_T; B : Boolean := True) is
      Idx : constant Value_Idx := Value_Data_Idx (V, Create => True);

   begin
      Value_Data_Table.Table (Idx).Is_Decl_Output := B;
   end Set_Is_Decl_Output;

   -------------------------
   -- Set_Is_Entry_Alloca --
   -------------------------

   procedure Set_Is_Entry_Alloca (V : Value_T; B : Boolean := True) is
      Idx : constant Value_Idx := Value_Data_Idx (V, Create => True);

   begin
      Value_Data_Table.Table (Idx).Is_Entry_Alloca := B;
   end Set_Is_Entry_Alloca;

   ---------------------------
   -- Get_Is_Typedef_Output --
   ---------------------------

   function Get_Is_Typedef_Output (T : Type_T) return Boolean is
      Idx : constant Type_Idx := Type_Data_Idx (T, Create => False);

   begin
      return Present (Idx)
        and then Type_Data_Table.Table (Idx).Is_Typedef_Output;
   end Get_Is_Typedef_Output;

   --------------------------
   -- Set_Is_Typedef_Output --
   --------------------------

   procedure Set_Is_Typedef_Output (T : Type_T; B : Boolean := True) is
      Idx : constant Type_Idx := Type_Data_Idx (T, Create => True);

   begin
      Type_Data_Table.Table (Idx).Is_Typedef_Output := B;
   end Set_Is_Typedef_Output;

   ------------------
   -- Get_Is_Entry --
   ------------------

   function Get_Is_Entry (BB : Basic_Block_T) return Boolean is
      Idx : constant BB_Idx := BB_Data_Idx (BB, Create => False);

   begin
      return Present (Idx) and then BB_Data_Table.Table (Idx).Is_Entry;
   end Get_Is_Entry;

   --------------------
   -- Get_Was_Output --
   --------------------

   function Get_Was_Output (BB : Basic_Block_T) return Boolean is
      Idx : constant BB_Idx := BB_Data_Idx (BB, Create => False);

   begin
      return Present (Idx) and then BB_Data_Table.Table (Idx).Was_Output;
   end Get_Was_Output;

   ------------------
   -- Set_Is_Entry --
   ------------------

   procedure Set_Is_Entry (BB : Basic_Block_T; B : Boolean := True) is
      Idx : constant BB_Idx := BB_Data_Idx (BB, Create => True);

   begin
      BB_Data_Table.Table (Idx).Is_Entry := B;
   end Set_Is_Entry;

   ------------------
   -- Set_Is_Entry --
   ------------------

   procedure Set_Was_Output (BB : Basic_Block_T; B : Boolean := True) is
      Idx : constant BB_Idx := BB_Data_Idx (BB, Create => True);

   begin
      BB_Data_Table.Table (Idx).Was_Output := B;
   end Set_Was_Output;

   ----------------------
   -- Maybe_Write_Decl --
   ----------------------

   procedure Maybe_Write_Decl (V : Value_T) is
   begin
      if not Get_Is_Decl_Output (V) then
         Write_Decl (V);
      end if;
   end Maybe_Write_Decl;

   -------------------------
   -- Maybe_Write_Typedef --
   -------------------------

   procedure Maybe_Write_Typedef (T : Type_T) is
   begin
      if not Get_Is_Typedef_Output (T) then
         Write_Typedef (T);
      end if;
   end Maybe_Write_Typedef;

   --------------------
   -- Get_Output_Idx --
   --------------------

   function Get_Output_Idx (V : Value_T) return Nat is
      Idx : constant Value_Idx := Value_Data_Idx (V, Create => True);
      VD  : Value_Data renames Value_Data_Table.Table (Idx);

   begin
      if VD.Output_Idx = 0 then
         VD.Output_Idx := Output_Idx;
         Output_Idx    := Output_Idx + 1;
      end if;

      return VD.Output_Idx;
   end Get_Output_Idx;

   --------------------
   -- Get_Output_Idx --
   --------------------

   function Get_Output_Idx (T : Type_T) return Nat is
      Idx : constant Type_Idx := Type_Data_Idx (T, Create => True);
      TD  : Type_Data renames Type_Data_Table.Table (Idx);

   begin
      if TD.Output_Idx = 0 then
         TD.Output_Idx := Output_Idx;
         Output_Idx    := Output_Idx + 1;
      end if;

      return TD.Output_Idx;
   end Get_Output_Idx;

   --------------------
   -- Get_Output_Idx --
   --------------------

   function Get_Output_Idx (BB : Basic_Block_T) return Nat is
      Idx : constant BB_Idx := BB_Data_Idx (BB, Create => True);
      BBD : BB_Data renames BB_Data_Table.Table (Idx);

   begin
      if BBD.Output_Idx = 0 then
         BBD.Output_Idx := Output_Idx;
         Output_Idx     := Output_Idx + 1;
      end if;

      return BBD.Output_Idx;
   end Get_Output_Idx;

   ------------------------
   --  Initialize_Tables --
   ------------------------

   procedure Initialize_Tables is
   begin
      null;
   end Initialize_Tables;

end CCG.Tables;
