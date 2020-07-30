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

with LLVM.Types; use LLVM.Types;

package CCG.Tables is

   --  This package contains the tables used by CCG to record data about
   --  LLVM values and the subprograms used to access and set such data.

   --  We use an internal representation of strings, discussed below.
   type Str is private;

   procedure Initialize_Tables;
   --  Perform any needed initialization on tables.

   function To_Str (S : String)  return Str;
   function To_Str (V : Value_T) return Str;
   function To_Str (T : Type_T)  return Str;
   --  Return an internal representation of S, V, or T

   function "&" (L : String;  R : Value_T) return Str;
   function "&" (L : String;  R : Type_T)  return Str;
   function "&" (L : String;  R : Str)     return Str;
   function "&" (L : Value_T; R : String)  return Str;
   function "&" (L : Value_T; R : Value_T) return Str;
   function "&" (L : Value_T; R : Type_T)  return Str;
   function "&" (L : Value_T; R : Str)     return Str;
   function "&" (L : Type_T;  R : String)  return Str;
   function "&" (L : Type_T;  R : Value_T) return Str;
   function "&" (L : Type_T;  R : Type_T)  return Str;
   function "&" (L : Type_T;  R : Str)     return Str;
   function "&" (L : Str;     R : String)  return Str;
   function "&" (L : Str;     R : Value_T) return Str;
   function "&" (L : Str;     R : Type_T)  return Str;
   function "&" (L : Str;     R : Str)     return Str;

private

   --  Most strings that we have are a combination of operators and
   --  keywords and a string denoting a value (which may be either the
   --  value's name, if it has one, or an expression denoting that value)
   --  or a type.  We record each string we use as a concatenation of
   --  actual strings, values, and types and create a hash table so that we
   --  only keep one copy of each string.  For the purpose of minimizing
   --  memory, we assume that each LLVM value and type has a distinct
   --  string representation.  This isn't necessarily true (e.g., the same
   --  local variable in multiple programs), but is a good compromise
   --  between time and space usage.  Most of the strings are small, so
   --  rather than creating a mechanism for variable-sized strings, each
   --  component of the concatenation is limited to a small size.  In the
   --  rare case where we need a larger string, we break it into segments.

   Str_Max : constant := 6;
   subtype Str_Length is Integer range 1 .. Str_Max;
   --  The longest string we'll use often is "struct "

   type Str_Component_Kind is (Value, Typ, Var_String);

   type Str_Component
        (Kind : Str_Component_Kind := Var_String; Length : Str_Length := 3)
      is record

      case Kind is
         when Value =>
            Val : Value_T;

         when Typ =>
            T   :  Type_T;

         when Var_String =>
            Str : String (1 .. Length);

      end case;
   end record;

   type Str_Component_Array is array (Integer range <>) of Str_Component;
   type Str_Record (Length : Integer) is record
      Comps : Str_Component_Array (1 .. Length);
   end record;

   type Str is access constant Str_Record;
   --  This is what we pass around for strings

   function "=" (SL, SR : Str_Record) return Boolean;
   function "=" (SL, SR : Str) return Boolean is
      (SL.all = SR.all);

end CCG.Tables;
