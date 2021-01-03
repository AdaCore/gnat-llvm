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

with LLVM.Core; use LLVM.Core;

with CCG.Helper; use CCG.Helper;

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
   --  precedence, if known, of a string, and the precedence of how a value
   --  is to be used. This information is used when we substitute a value
   --  for a variable: if that value is of higher precedence than the
   --  string we're substituting it into, we don't need parentheses.

   --  We track what precedence an expression "is" and what precedence an
   --  expression is "for". If we have a simple expression, like A + B,
   --  both are the same: this is an expression whose precedence is of
   --  addition and we're using the values A and A in an expression of that
   --  precedence. However, consider an expression "(int) A + B".  The
   --  precedence of that expression (which says when parens would be
   --  needed when that's put into an outer expression) is for an addition.
   --  The value B is being used in an expression of that precedence, but
   --  the value A is being used in an expression whose precedence is that
   --  of a cast. So we record precedence in two ways: when a Str contains
   --  a value, we record the precedence of the expression that that value
   --  is "for" and we also record, in the Str, what precedence the Str "is".
   --
   --  The Needs_Parens function is used to determine whether we need to
   --  enclose a value of one precedence inside parentheses when it's being
   --  inserted into an expression of another precedence.
   --
   --  We assign a precedence to a value by using the binary "+" operator,
   --  which produces a Str that is both of the specified precedence and
   --  where the value is marked as being for an expression of that
   --  precedence. In the default case of a value being used to construct
   --  a Str, the precedence is set to a primary, if we know that's what the
   --  value is or to "unknown".
   --
   --  We also assign a precedence to a Str using the binary "+" operator.
   --  That does two things: first, it sets the precedence that any values
   --  in the expression have to the assigned precedence if the precedence
   --  of that value is unknown. Second, it sets that as the precedence of
   --  the new Str, enclosing it in parens if that would be required when
   --  putting an expression of the old precedence into an expression of
   --  the new precedence.

   --  Define the precedence levels, listed from lowest to highest

   type Precedence is
     (Unknown,
      --  Used to record that we don't (yet) know the precedence of an
      --  expression or where a value will be used.

      Comma,
      --  The comma operator, usually within a function call

      Assign,
      --  Assignment operation and comma operator (e.g., function arguments)

      Conditional,
      --  Ternary conditional operator:  a ? b : c

      Logical_OR,
      --  Logical OR operation

      Logical_AND,
      --  Logical AND operation

      Bit,
      --  Bitwise binary operations

      Relation,
      --  Comparison operations

      Shift,
      --  Left and right shit operations

      Add,
      --  Addition and subtraction

      Mult,
      --  Multiplication, division, and remainder

      Unary,
      --  All unary operations (cast, address of, dereference, and
      --  unary bitwise and arithmetic operations.

      Component,
      --  Reference to struct fields and array components and function calls

      Primary);
      --   A name

   --  When we concatenate strings that have precedence information, the
   --  resulting string has the lowest precedence. The precedence of a
   --  value or basic block is Primary. We add a precedence to a string
   --  using the binary "+" operator.

   function "+" (S : String; P : Precedence) return Str
     with Post => Get_Precedence ("+"'Result) = P;
   function "+" (S : Str; P : Precedence) return Str
     with Pre => Present (S), Post => Get_Precedence ("+"'Result) = P;

   function Get_Precedence (S : Str) return Precedence
     with Pre => Present (S);
   --  Return the precedence assigned to S

   function Needs_Parens (Is_P, For_P : Precedence) return Boolean is
      (Is_P /= Unknown and then For_P /= Unknown
         and then (Is_P < For_P
                     or else (Is_P = For_P
                                and then For_P not in Unary | Component)));
   function Needs_Parens (S : Str; For_P : Precedence) return Boolean is
     (Needs_Parens (Get_Precedence (S), For_P));
   --  Indicates whether we need to enclose S (or an expression of precedence
   --  Is_P) in parentheses if it's being inserted into an expression of
   --  precedence For_P. If the precedence is the same, we want to add
   --  parens in most cases, but we needn't in the case of unary and
   --  component operators due to the syntax.

   --  Normally, a use of a variable name in the LLVM IR is the same as the
   --  use of that name in C. However, there are cases where a reference in
   --  LLVM IR is to the address of what's a variable in C and vice
   --  versa. We also have to know when an LLVM value is used as the
   --  initializer of a variable because if it's an aggregate constant,
   --  that's the only case where we can (and must) use the value of that
   --  constant.
   --
   --  We define Value_Flag values to denote an attribute of a use of
   --  a value and use the "+" operator to add that attribute to the use of a
   --  value. This is stored in a Value_Flags record in the Str that we
   --  create. We need to provide a default value for the record and map
   --  a single flag ito a record with that flag set.

   type Value_Flag is
     (LHS,
      --  We can accept an LHS in the context where an address would normally
      --  be provided. If this flag is not present in the reference, we'll
      --  take the address of the value.

      Initializer,
      --  If this is a constant, always output the value of the constant,
      --  instead of its name, even if it's an aggregate constant.

      Phi_Temp,
      --  This must be a Phi node, in which case we output the temporary
      --  name and not the actual value name. We don't use the C_Value since
      --  that refers to that of the actual value, not the temporary.

      Need_Unsigned,
      --  We need an unsigned form of this value. If the value isn't unsigned
      --  or can't be made unsigned (e.g. an integer constant), we emit a
      --  cast to unsigned.

      Need_Signed,
      --  We need a signed form of this value. This is ignored if the
      --  value isn't of an integral type.

      Write_Type);
      --  We want to write out the type of the value (including its
      --  signedness) and not the value itself.

   type Value_Flags is record
      LHS           : Boolean;
      Initializer   : Boolean;
      Phi_Temp      : Boolean;
      Need_Unsigned : Boolean;
      Need_Signed   : Boolean;
      Write_Type    : Boolean;
   end record;

   function "or" (X, Y : Value_Flags) return Value_Flags is
     (LHS            => X.LHS           or Y.LHS,
      Initializer    => X.Initializer   or Y.Initializer,
      Phi_Temp       => X.Phi_Temp      or Y.Phi_Temp,
      Need_Unsigned  => X.Need_Unsigned or Y.Need_Unsigned,
      Need_Signed    => X.Need_Signed   or Y.Need_Signed,
      Write_Type     => X.Write_Type    or Y.Write_Type);

   type Flag_Array is array (Value_Flag) of Value_Flags;

   Default_Flags : constant Value_Flags :=
     (False, False, False, False, False, False);
   Flag_To_Flags : constant Flag_Array :=
     (LHS           => (True,  False, False, False, False, False),
      Initializer   => (False, True,  False, False, False, False),
      Phi_Temp      => (False, False, True,  False, False, False),
      Need_Unsigned => (False, False, False, True,  False, False),
      Need_Signed   => (False, False, False, False, True,  False),
      Write_Type    => (False, False, False, False, False, True));

   function "+" (F : Value_Flag) return Value_Flags is
     (Flag_To_Flags (F));

   function "+" (V : Value_T; VF : Value_Flag) return Str
     with Pre => Present (V);
   function "+" (S : Str; VF : Value_Flag) return Str
     with Pre => Is_Value (S);

   type String_Kind is (Normal, Name);
   --  A string can either be a literal string or a name, in which case we
   --  have to ensure that it's valid for C.

   function "+" (S : String; K : String_Kind) return Str
     with Post => Present ("+"'Result);

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

   function "+" (V : Value_T; P : Precedence) return Str
     with Pre => Present (V), Post => Get_Precedence ("+"'Result) = P;

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
   function "&" (L : Value_T;        R : Nat)           return Str
     with Pre => Present (L), Post => Present ("&"'Result);
   function "&" (L : Value_T;        R : Str)           return Str
     with Pre  => Present (L) and then Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Type_T;         R : String)        return Str
     with Pre => Present (L), Post => Present ("&"'Result);
   function "&" (L : Type_T;         R : Nat)           return Str
     with Pre => Present (L), Post => Present ("&"'Result);
   function "&" (L : Type_T;         R : Str)           return Str
     with Pre  => Present (L) and then Present (R),
          Post => Present ("&"'Result);
   function "&" (L : Basic_Block_T;  R : String)        return Str
     with Pre => Present (L), Post => Present ("&"'Result);
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

   function Addr_Of (S : Str; T : Type_T := No_Type_T) return Str
     with Pre => Present (S), Post => Present (Addr_Of'Result);
   function Addr_Of (V : Value_T; T : Type_T := No_Type_T) return Str is
     (Addr_Of (+V, T))
     with Pre => Present (V), Post => Present (Addr_Of'Result);
   --  Make an Str that represents taking the address of S or V. This usually
   --  means prepending "&", but we can also do that by removing a leading
   --  "*" or changing the value kind. If T is Present, use that type instead
   --  of the type of the value for the dereference (this is used if the
   --  expression is a component reference).

   function Deref (S : Str) return Str
     with Pre => Present (S), Post => Present (Deref'Result);
   function Deref (V : Value_T) return Str is
     (Deref (V + Unary))
     with Pre => Present (V), Post => Present (Deref'Result);
   --  Make an Str that represents rerefencing S or V. This usually means
   --  prepending "*", but we can also do that by removing a leading "&" or
   --  changing the value kind.

   --  Get and set attributes we record of LLVM values, types, and
   --  basic blocks.

   function Get_C_Value             (V : Value_T) return Str
     with Pre => Present (V), Inline;
   --  If Present, a string that represents the value of the Value_T

   function Get_Is_Variable         (V : Value_T) return Boolean
   --  True if V represents a variable declared at source level
     with Pre => Present (V), Inline;

   function Get_Is_Decl_Output      (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if we wrote any needed decl for this value

   function Get_Is_Temp_Decl_Output (V : Value_T) return Boolean
     with Pre => Is_APHI_Node (V), Inline;
   --  Likewise, but applies to the temporary needed for a PHI instruction

   function Get_Is_LHS              (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if this value represents an LHS. This is usually either a
   --  global variable or an alloca in the entry block. In that case, from
   --  a C perspective, a use of a value in LLVM IR represents the address
   --  of the value; only "load" or "store" instruction actually accesses
   --  the value. It can also be the result of a GEP instruction.

   function Get_Is_Constant         (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if this value is a constant and was declared that way
   --  in C.

   function Get_Is_Unsigned         (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if this value represents a variable that's unsigned

   function Get_Is_Used             (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if this value represents a variable that has been used in an
   --  expression.

   procedure Set_C_Value            (V : Value_T; S : Str)
     with Pre => Present (V), Post => Get_C_Value (V) = S, Inline;
   procedure Set_Is_Variable        (V : Value_T; B : Boolean := True)
     with Pre  => Present (V), Post => Get_Is_Variable (V) = B, Inline;
   procedure Set_Is_Decl_Output      (V : Value_T; B : Boolean := True)
     with Pre => Present (V), Post => Get_Is_Decl_Output (V) = B, Inline;
   procedure Set_Is_Temp_Decl_Output (V : Value_T; B : Boolean := True)
     with Pre => Is_APHI_Node (V), Post => Get_Is_Temp_Decl_Output (V) = B,
          Inline;
   procedure Set_Is_LHS              (V : Value_T; B : Boolean := True)
     with Pre => Present (V), Post => Get_Is_LHS (V) = B, Inline;
   procedure Set_Is_Constant         (V : Value_T; B : Boolean := True)
     with Pre => Present (V), Post => Get_Is_Constant (V) = B, Inline;
   procedure Set_Is_Unsigned        (V : Value_T; B : Boolean := True)
     with Pre => Present (V), Post => Get_Is_Unsigned (V) = B, Inline;
   procedure Set_Is_Used             (V : Value_T; B : Boolean := True)
     with Pre => Present (V), Post => Get_Is_Used (V) = B, Inline;

   function Get_Is_Typedef_Output        (T : Type_T) return Boolean
     with Pre => Present (T), Inline;
   --  True if this is a type either for which we don't write a typedef
   --  or if it is and we've written that typedef previously.

   function Get_Is_Return_Typedef_Output (T : Type_T) return Boolean
     with Pre => Present (T), Inline;
   --  True if this is an array type and we've written the struct type
   --  that we use for the return type of a function returning this type.

   function Get_Is_Incomplete_Output     (T : Type_T) return Boolean
     with Pre => Present (T), Inline;
   --  True if this is a struct type and we've just written the struct
   --  definition without fields (an incomplete type).

   function Get_Are_Writing_Typedef      (T : Type_T) return Boolean
     with Pre => Present (T), Inline;
   --  True if we're in the process of writing a typedef

   procedure Set_Is_Typedef_Output        (T : Type_T; B : Boolean := True)
     with Pre  => Present (T), Post => Get_Is_Typedef_Output (T) = B, Inline;
   procedure Set_Is_Return_Typedef_Output (T : Type_T; B : Boolean := True)
     with Pre  => Present (T), Post => Get_Is_Return_Typedef_Output (T) = B,
          Inline;
   procedure Set_Is_Incomplete_Output     (T : Type_T; B : Boolean := True)
     with Pre  => Present (T), Post => Get_Is_Incomplete_Output (T) = B,
          Inline;
   procedure Set_Are_Writing_Typedef      (T : Type_T; B : Boolean := True)
     with Pre  => Present (T), Post => Get_Are_Writing_Typedef (T) = B, Inline;

   function Get_Was_Output (BB : Basic_Block_T) return Boolean
     with Pre => Present (BB), Inline;
   procedure Set_Was_Output (BB : Basic_Block_T; B : Boolean := True)
     with Pre  => Present (BB), Post => Get_Was_Output (BB) = B, Inline;

   procedure Delete_Value_Info (V : Value_T) with Convention => C;
   --  Delete all information previously stored for V

   --  Provide a set of functions to reference an Str that contains just
   --  a value or contains exactly one value.

   function Is_Value (S : Str) return Boolean
     with Pre => Present (S), Inline;
   function Single_Value (S : Str) return Value_T
     with Pre => Present (S);
   function Contains_One_Value (S : Str) return Boolean is
     (Present (Single_Value (S)))
     with Pre => Present (S);
   function "+" (S : Str) return Value_T is
     (Single_Value (S))
     with Pre => Contains_One_Value (S), Inline;
   function Get_Is_LHS (S : Str) return Boolean is
     (Get_Is_LHS (+S))
     with Pre => Contains_One_Value (S);
   function Get_Is_Constant (S : Str) return Boolean is
     (Get_Is_Constant (+S))
     with Pre => Contains_One_Value (S);
   function Get_C_Value (S : Str) return Str is
     (Get_C_Value (+S))
     with Pre => Contains_One_Value (S);
   function Type_Of (S : Str) return Type_T is
     (Type_Of (+S))
     with Pre => Contains_One_Value (S);
   function Get_Type_Kind (S : Str) return Type_Kind_T is
     (Get_Type_Kind (Type_Of (S)))
     with Pre => Contains_One_Value (S);

   function Has_Unsigned (S : Str) return Boolean
      with Pre => Present (S);
   --  True if there is a reference within S to a value that's unsigned.
   --  This is purposely conservative in that it returns true if *anything*
   --  in S is unsigned, even though the expression that S represents
   --  may no longer be unsigned.

   --  Define functions to return (and possibly create) an ordinal to use
   --  as part of the name for a value, type, or basic block.

   function Get_Output_Idx (V : Value_T) return Nat
     with Pre => Present (V), Post => Get_Output_Idx'Result /= 0, Inline;
   function Get_Output_Idx (T : Type_T) return Nat
     with Pre => Present (T), Post => Get_Output_Idx'Result /= 0, Inline;
   function Get_Output_Idx (BB : Basic_Block_T) return Nat
     with Pre => Present (BB), Post => Get_Output_Idx'Result /= 0, Inline;
   function Get_Output_Idx                      return Nat
     with Post => Get_Output_Idx'Result /= 0, Inline;

   procedure Maybe_Write_Typedef (T : Type_T; Incomplete : Boolean := False)
     with Pre  => Present (T),
          Post => Get_Is_Typedef_Output (T) or else Get_Are_Writing_Typedef (T)
                  or else (Incomplete and then Get_Is_Incomplete_Output (T));
   --  See if we need to write a typedef for T and write one if so. If
   --  Incomplete is True, all we need is the initial portion of a struct
   --  definition.

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

   Str_Max : constant := 9;
   subtype Str_Length is Integer range 0 .. Str_Max;
   --  The longest string we'll use often is "unsigned ".  We allow empty
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
            S_Kind : String_Kind;
            Str    : String (1 .. Length);

         when Value =>
            Val    : Value_T;
            Flags  : Value_Flags;
            For_P  : Precedence;

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

   function Get_Precedence (S : Str) return Precedence is
     (S.P);

   function Is_Value (S : Str) return Boolean is
     (S.Length = 1 and then S.Comps (1).Kind = Value
        and then not S.Comps (1).Flags.Phi_Temp);

end CCG.Tables;
