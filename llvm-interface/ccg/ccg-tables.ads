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

with GNATLLVM; use GNATLLVM;

package CCG.Tables is

   --  This package contains the tables used by CCG to record data about
   --  LLVM values and the subprograms used to access and set such data.

   --  We use an internal representation of strings, discussed below.

   type Str is private;
   No_Str  : constant Str;
   Eol_Str : constant String;

   function Present (S : Str) return Boolean;
   function No      (S : Str) return Boolean;

   procedure Write_Str (S : Str; Eol : Boolean := False)
     with Pre => Present (S);
   --  Write the contents of S to the current output target

   --  In order to eliminate most parentheses, we record the operator
   --  precedence, if known, of a string. This is used when we substitute
   --  a value for a variable: if that value is of higher precedence than
   --  the string we're substituting it into, we don't need parentheses.
   --  For simplicity, we only use a subset of all of the C precedence levels.
   --  Levels are listed from lowest to highest precedence.

   type Precedence is (Unknown, Assign, Logic, Relation, Shift, Add, Mult,
                       Unary, Component, Primary);

   --  When we concatenate strings that have precedence information, the
   --  resulting string has the lowest precedence. The precedence of a
   --  value or basic block is Primary. We add a precedence to a string
   --  using the binary "+" operator.

   function "+" (S : String; P : Precedence) return Str
     with Post => Get_Precedence ("+"'Result) = P;
   function "+" (S : Str; P : Precedence) return Str
     with Pre  => Present (S),
          Post => Get_Precedence ("+"'Result) = P;

   function Has_Precedence (S : Str) return Boolean
     with Pre => Present (S);
   --  Return True if the precedence of S isn't known

   function Get_Precedence (S : Str) return Precedence
     with Pre => Has_Precedence (S);
   --  Return the precedence assigned to S

   --  Normally, a use of a variable name in the LLVM IR is the same as the
   --  use of that name in C. However, there are cases where a reference in
   --  LLVM IR is to the address of what's a variable in C and vice
   --  versa. We also have to know when an LLVM value is used as the
   --  initializer of a variable because if it's an aggregate constant,
   --  that's the only case where we can (and must) use the value of that
   --  constant. We define a Value_Kind to distinguish between these case
   --  and use the "+" operator to assign a non-default kind to a value.

   type Value_Kind is
     (Normal,
      --  The default, which indicates that the value is to be interpreted
      --  as it would be in the LLVM IR, whether that's an address or the
      --  value data.

      Value_Name,
      --  Always represent this value as a name. This is the case for the
      --  address operand of a load or store instruction and when we use
      --  the name in a declaration.

      Initializer);
      --  If this is a constant, always output the value of the constant,
      --  even if it's an aggregate constant.

   function "+" (V : Value_T; K : Value_Kind) return Str
     with Pre => Present (V);

   function "+" (S : String)        return Str
     with Post => Present ("+"'Result);
   function "+" (V : Value_T)       return Str
     with Pre => Present (V), Post => Present ("+"'Result);
   function "+" (T : Type_T)        return Str
     with Pre => Present (T), Post => Present ("+"'Result);
   function "+" (B : Basic_Block_T) return Str
     with Pre => Present (B), Post => Present ("+"'Result);
   function "+" (N : Nat)           return Str
     with Post => Present ("+"'Result);
   --  Return an internal representation of S, V, T, or B

   function "&" (L : String;         R : Value_T)       return Str
     with Post => Present ("&"'Result);
   function "&" (L : String;         R : Type_T)        return Str
     with Pre => Present (R), Post => Present ("&"'Result);
   function "&" (L : String;         R : Basic_Block_T) return Str
     with Pre => Present (R), Post => Present ("&"'Result);
   function "&" (L : String;         R : Nat)           return Str
     with Post => Present ("&"'Result);
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
   function "&" (L : Value_T;        R : Nat)           return Str
     with Pre => Present (L), Post => Present ("&"'Result);
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
   function "&" (L : Type_T;         R : Nat)           return Str
     with Pre => Present (L), Post => Present ("&"'Result);
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
   function "&" (L : Basic_Block_T;  R : Nat)           return Str
     with Pre => Present (L), Post => Present ("&"'Result);
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
   function "&" (L : Str;            R : Nat)           return Str
     with Pre => Present (L), Post => Present ("&"'Result);
   function "&" (L : Str;            R : Str)           return Str
     with Pre  => Present (R),
          Post => Present ("&"'Result);

   --  Get and set attributes we record of LLVM values, types, and
   --  basic blocks.

   function Get_C_Value        (V : Value_T) return Str
     with Pre => Present (V), Inline;
   --  If Present, a string that represents the value of the Value_T

   function Get_No_Name        (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if there's no LLVM name for this value; we use the ordinal

   function Get_Is_Decl_Output (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if we wrote any needed decl for this value

   function Get_Is_Variable    (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if this value represents a variable. This can either be a
   --  global variable or an alloca in the entry block. In that case,
   --  from a C perspective, a use of a value in LLVM IR represents
   --  the address of the value; only "load" or "store" instruction
   --  actually accesses the value.
   procedure Set_C_Value       (V : Value_T; S : Str)
     with Pre  => Present (V) and then Present (S),
          Post => Get_C_Value (V) = S, Inline;
   procedure Set_No_Name       (V : Value_T; B : Boolean := True)
     with Pre  => Present (V),
          Post => Get_No_Name (V) = B, Inline;
   procedure Set_Is_Decl_Output (V : Value_T; B : Boolean := True)
     with Pre  => Present (V), Post => Get_Is_Decl_Output (V) = B, Inline;
   procedure Set_Is_Variable    (V : Value_T; B : Boolean := True)
     with Pre  => Present (V), Post => Get_Is_Variable (V) = B, Inline;

   function Get_Is_Typedef_Output (T : Type_T) return Boolean
     with Pre => Present (T), Inline;
   procedure Set_Is_Typedef_Output (T : Type_T; B : Boolean := True)
     with Pre  => Present (T), Post => Get_Is_Typedef_Output (T) = B, Inline;

   function Get_Is_Entry   (BB : Basic_Block_T) return Boolean
     with Pre => Present (BB), Inline;
   function Get_Was_Output (BB : Basic_Block_T) return Boolean
     with Pre => Present (BB), Inline;
   function Get_No_Name    (BB : Basic_Block_T) return Boolean
     with Pre => Present (BB), Inline;

   procedure Set_Is_Entry   (BB : Basic_Block_T; B : Boolean := True)
     with Pre  => Present (BB), Post => Get_Is_Entry (BB) = B, Inline;
   procedure Set_Was_Output (BB : Basic_Block_T; B : Boolean := True)
     with Pre  => Present (BB), Post => Get_Was_Output (BB) = B, Inline;
   procedure Set_No_Name    (BB : Basic_Block_T; B : Boolean := True)
     with Pre  => Present (BB),
          Post => Get_No_Name (BB) = B, Inline;

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

      Typ,
      --  An LLVM type

      BB,
      --  An LLVM basic block

      Number);
      --  An integer

   type Str_Component
     (Kind : Str_Component_Kind := Var_String; Length : Str_Length := 3)
   is record
      case Kind is
         when Var_String =>
            Str    : String (1 .. Length);

         when Value =>
            Val    : Value_T;
            V_Kind : Value_Kind;

         when Typ =>
            T      :  Type_T;

         when BB =>
            B      : Basic_Block_T;

         when Number =>
            N     : Nat;

      end case;
   end record;

   type Str_Component_Array is array (Integer range <>) of Str_Component;
   type Str_Record (Length : Integer) is record
      P     : Precedence;
      Comps : Str_Component_Array (1 .. Length);
   end record;

   type Str is access constant Str_Record;
   --  This is what we pass around for strings

   No_Str  : constant Str    := null;
   Eol_Str : constant String := "@@";

   function Present (S : Str) return Boolean is (S /= No_Str);
   function No      (S : Str) return Boolean is (S =  No_Str);

   function "=" (SL, SR : Str_Record) return Boolean;
   function "=" (SL, SR : Str)        return Boolean is
     (SL.all = SR.all);

   function Has_Precedence (S : Str) return Boolean is
     (S.P /= Unknown);

   function Get_Precedence (S : Str) return Precedence is
     (S.P);

end CCG.Tables;
