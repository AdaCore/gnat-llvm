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

with GNATLLVM; use GNATLLVM;

package CCG.Tables is

   --  This package contains the tables used by CCG to record data about
   --  LLVM values and the subprograms used to access and set such data.

   --  We use an internal representation of strings, discussed below.

   type Str is private;
   No_Str : constant Str;

   function Present (S : Str) return Boolean;
   function No      (S : Str) return Boolean;

   procedure Initialize_Tables;
   --  Perform any needed initialization on tables.

   function "+" (S : String)        return Str
     with Post => Present ("+"'Result);
   function "+" (V : Value_T)       return Str
     with Pre => Present (V), Post => Present ("+"'Result);
   function "+" (T : Type_T)        return Str
     with Pre => Present (T), Post => Present ("+"'Result);
   function "+" (B : Basic_Block_T) return Str
     with Pre => Present (B), Post => Present ("+"'Result);
   function To_Data (V : Value_T)       return Str
     with Pre => Present (V), Post => Present (To_Data'Result);
   --  Return an internal representation of S, V, T, or B

   procedure Write_Str (S : Str; Eol : Boolean := False)
     with Pre => Present (S);
   --  Write the contents of S to the current output target

   function "&" (L : String;         R : Value_T)       return Str
     with Post => Present ("&"'Result);
   function "&" (L : String;         R : Type_T)        return Str
     with Pre => Present (R), Post => Present ("&"'Result);
   function "&" (L : String;         R : Basic_Block_T) return Str
     with Pre => Present (R), Post => Present ("&"'Result);
   function "&" (L : String;         R : Str)           return Str
     with Pre => Present (R), Post => Present ("&"'Result);
   function "&" (L : Value_T;        R : String)        return Str
     with Pre => Present (L), Post => Present ("&"'Result);
   function "&" (L : Value_T;        R : Value_T)       return Str
     with Pre  => Present (L) and then Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Value_T;        R : Type_T)        return Str
     with Pre  => Present (L) and then Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Value_T;        R : Basic_Block_T) return Str
     with Pre  => Present (L) and then Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Value_T;        R : Str)           return Str
     with Pre  => Present (L) and then Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Type_T;         R : String)        return Str;
   function "&" (L : Type_T;         R : Value_T)       return Str
     with Pre  => Present (L) and then Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Type_T;         R : Type_T)        return Str
     with Pre  => Present (L) and then Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Type_T;         R : Basic_Block_T) return Str
     with Pre  => Present (L) and then Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Type_T;         R : Str)           return Str
     with Pre  => Present (L) and then Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Basic_Block_T;  R : String)        return Str
     with Pre => Present (L), Post => Present ("&"'Result);
   function "&" (L : Basic_Block_T;  R : Value_T)       return Str
     with Pre  => Present (L) and then Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Basic_Block_T;  R : Type_T)        return Str
     with Pre  => Present (L) and then Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Basic_Block_T;  R : Basic_Block_T) return Str
     with Pre  => Present (L) and then Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Basic_Block_T;  R : Str)           return Str
     with Pre  => Present (L) and then Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Str;            R : String)        return Str
     with Post => Present ("&"'Result);
   function "&" (L : Str;            R : Value_T)       return Str
     with Pre  => Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Str;            R : Type_T)        return Str
     with Pre  => Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Str;            R : Basic_Block_T) return Str
     with Pre  => Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Str;            R : Str)           return Str
     with Pre  => Present (R),
          Post => Present ("&"'Result);

   --  Get and set attributes we record of LLVM values, types, and
   --  basic blocks.

   function Get_C_Value         (V : Value_T) return Str
     with Pre => Present (V), Inline;
   function Get_No_Name         (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   function Get_Is_Decl_Output  (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   function Get_Is_Entry_Alloca (V : Value_T) return Boolean
     with Pre => Present (V), Inline;

   procedure Set_C_Value         (V : Value_T; S : Str)
     with Pre  => Present (V) and then Present (S),
          Post => Get_C_Value (V) = S, Inline;
   procedure Set_No_Name         (V : Value_T; B : Boolean := True)
     with Pre  => Present (V),
          Post => Get_No_Name (V) = B, Inline;
   procedure Set_Is_Decl_Output  (V : Value_T; B : Boolean := True)
     with Pre  => Present (V), Post => Get_Is_Decl_Output (V) = B, Inline;
   procedure Set_Is_Entry_Alloca (V : Value_T; B : Boolean := True)
     with Pre  => Present (V), Post => Get_Is_Entry_Alloca (V) = B, Inline;

   function Get_Is_Typedef_Output (T : Type_T) return Boolean
     with Pre => Present (T), Inline;
   procedure Set_Is_Typedef_Output (T : Type_T; B : Boolean := True)
     with Pre  => Present (T), Post => Get_Is_Typedef_Output (T) = B, Inline;

   function Get_Is_Entry   (BB : Basic_Block_T) return Boolean
     with Pre => Present (BB), Inline;
   function Get_Was_Output (BB : Basic_Block_T) return Boolean
     with Pre => Present (BB), Inline;

   procedure Set_Is_Entry   (BB : Basic_Block_T; B : Boolean := True)
     with Pre  => Present (BB), Post => Get_Is_Entry (BB) = B, Inline;
   procedure Set_Was_Output (BB : Basic_Block_T; B : Boolean := True)
     with Pre  => Present (BB), Post => Get_Was_Output (BB) = B, Inline;

   --  Define functions to return (and possibly create) an ordinal to use
   --  as part of the name for a value, type, or basic block.

   function Get_Output_Idx (V : Value_T) return Nat
     with Pre => Present (V), Post => Get_Output_Idx'Result /= 0, Inline;
   function Get_Output_Idx (T : Type_T) return Nat
     with Pre => Present (T), Post => Get_Output_Idx'Result /= 0, Inline;
   function Get_Output_Idx (BB : Basic_Block_T) return Nat
     with Pre => Present (BB), Post => Get_Output_Idx'Result /= 0, Inline;

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

   Str_Max : constant := 10;
   subtype Str_Length is Integer range 0 .. Str_Max;
   --  The longest string we'll use often is " (unsigned) ".  We allow empty
   --  strings, but optimize operations to not create them except when
   --  necessary.

   --  Define the types of string components we support

   type Str_Component_Kind is
     (Var_String,
      --  A short literal string

      Value,
      --  An LLVM value

      Data_Value,
      --  Like Value, but represents the data for that value in the case
      --  where the value itself represents an address (i.e., an alloca in
      --  the entry block.

      Typ,
      --  An LLVM type

      BB);
      --  An LLVM basic block

   type Str_Component
        (Kind : Str_Component_Kind := Var_String; Length : Str_Length := 3)
      is record

      case Kind is
         when Var_String =>
            Str   : String (1 .. Length);

         when Value | Data_Value =>
            Val   : Value_T;

         when Typ =>
            T     :  Type_T;

         when BB =>
            B     : Basic_Block_T;

      end case;
   end record;

   type Str_Component_Array is array (Integer range <>) of Str_Component;
   type Str_Record (Length : Integer) is record
      Comps : Str_Component_Array (1 .. Length);
   end record;

   type Str is access constant Str_Record;
   --  This is what we pass around for strings

   No_Str : constant Str := null;

   function Present (S : Str) return Boolean is (S /= No_Str);
   function No      (S : Str) return Boolean is (S =  No_Str);

   function "=" (SL, SR : Str_Record) return Boolean;
   function "=" (SL, SR : Str)        return Boolean is
      (SL.all = SR.all);

end CCG.Tables;
