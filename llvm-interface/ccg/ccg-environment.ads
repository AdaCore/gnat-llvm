------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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

with CCG.Strs;   use CCG.Strs;

package CCG.Environment is

   --  This package contains the tables used by CCG to record data about
   --  LLVM values and the subprograms used to access and set such data.

   --  Get and set attributes we record of LLVM values, types, and
   --  basic blocks.

   function Get_C_Value        (V : Value_T) return Str
     with Pre => Present (V), Inline;
   --  If Present, a string that represents the value of the Value_T

   function Get_Is_Decl_Output (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if we wrote any needed decl for this value

   function Get_Is_LHS         (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if this value represents an LHS. This is usually either a
   --  global variable or an alloca in the entry block. In that case, from
   --  a C perspective, a use of a value in LLVM IR represents the address
   --  of the value; only "load" or "store" instruction actually accesses
   --  the value. It can also be the result of a GEP instruction.

   function Get_Is_Constant    (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if this value is a constant and was declared that way
   --  in C.

   function Get_Entity         (V : Value_T) return Entity_Id
     with Pre => Present (V), Inline;
   --  Get the GNAT entity (either object or type) of this value, if known

   function Get_Entity_Is_Ref  (V : Value_T) return Boolean
     with Pre => Present (V);
   --  True if V is a reference to Get_Entity (V)

   function Get_Is_Used        (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if this value represents a variable that has been used in an
   --  expression.

   function Get_Needs_Nest     (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if V is a function that needs to have a parameter added for the
   --  static chain. This is the usually the case if its address is taken
   --  and it doesn't already have one.

   function Get_Must_Globalize (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if V, which is a constant, is used in a inlined function and so
   --  must be promoted to being a global variable if we have to make
   --  a variable for it.

   procedure Set_C_Value        (V : Value_T; S : Str)
     with Pre => Present (V), Post => Get_C_Value (V) = S, Inline;
   procedure Set_Is_Decl_Output (V : Value_T; B : Boolean := True)
     with Pre => Present (V), Post => Get_Is_Decl_Output (V) = B, Inline;
   procedure Set_Is_LHS         (V : Value_T; B : Boolean := True)
     with Pre => Present (V), Post => Get_Is_LHS (V) = B, Inline;
   procedure Set_Is_Constant    (V : Value_T; B : Boolean := True)
     with Pre => Present (V), Post => Get_Is_Constant (V) = B, Inline;
   procedure Set_Entity         (V : Value_T; E : Entity_Id)
     with Pre => Present (V), Post => Get_Entity (V) = E, Inline;
   procedure Set_Entity_Is_Ref  (V : Value_T; B : Boolean := True)
     with Pre => Present (V), Post => Get_Entity_Is_Ref (V) = B, Inline;
   procedure Set_Is_Used        (V : Value_T; B : Boolean := True)
     with Pre => Present (V), Post => Get_Is_Used (V) = B, Inline;
   procedure Set_Needs_Nest     (V : Value_T; B : Boolean := True)
     with Pre => Present (V), Post => Get_Needs_Nest (V) = B, Inline;
   procedure Set_Must_Globalize (V : Value_T; B : Boolean := True)
     with Pre => Present (V), Post => Get_Must_Globalize (V) = B, Inline;

   function Get_Entity                   (T : Type_T) return Opt_Type_Kind_Id
     with Pre => Present (T), Inline;
   --  Get the GNAT entity for non-void type T, if Present

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

   function Get_Are_Outputting_Typedef   (T : Type_T) return Boolean
     with Pre => Present (T), Inline;
   --  True if we're in the process of outputting a typedef

   function Get_Used_In_Struct           (T : Type_T) return Boolean
     with Pre => Present (T), Inline;
   --  True if T is the type of an element of a struct

   function Get_Cannot_Pack              (T : Type_T) return Boolean
     with Pre => Present (T), Inline;
   --  True if this is a type that we want to pack, but can't because of
   --  restrictions in our C compiler.

   procedure Set_Entity                   (T : Type_T; TE : Type_Kind_Id)
     with Pre => Present (T), Post => Get_Entity (T) = TE, Inline;
   procedure Set_Is_Typedef_Output        (T : Type_T; B : Boolean := True)
     with Pre  => Present (T), Post => Get_Is_Typedef_Output (T) = B, Inline;
   procedure Set_Is_Return_Typedef_Output (T : Type_T; B : Boolean := True)
     with Pre  => Present (T), Post => Get_Is_Return_Typedef_Output (T) = B,
          Inline;
   procedure Set_Is_Incomplete_Output     (T : Type_T; B : Boolean := True)
     with Pre  => Present (T), Post => Get_Is_Incomplete_Output (T) = B,
          Inline;
   procedure Set_Are_Outputting_Typedef   (T : Type_T; B : Boolean := True)
     with Pre  => Present (T), Post => Get_Are_Outputting_Typedef (T) = B,
          Inline;
   procedure Set_Used_In_Struct           (T : Type_T; B : Boolean := True)
     with Pre  => Present (T), Post => Get_Used_In_Struct (T) = B,
          Inline;
   procedure Set_Cannot_Pack              (T : Type_T; B : Boolean := True)
     with Pre  => Present (T), Post => Get_Cannot_Pack (T) = B,
          Inline;

   function Get_Flow        (BB : Basic_Block_T) return Flow_Idx
     with Pre => Present (BB), Inline;

   procedure Set_Flow        (BB : Basic_Block_T; Fidx : Flow_Idx)
     with Pre  => Present (BB) and then Present (Fidx)
                  and then No (Get_Flow (BB)),
          Post => Get_Flow (BB) = Fidx, Inline;

   procedure Delete_Value_Info (V : Value_T) with Convention => C;
   --  Delete all information previously stored for V

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

   procedure Maybe_Output_Typedef (T : Type_T; Incomplete : Boolean := False)
     with Pre  => Present (T),
          Post => Get_Is_Typedef_Output (T)
                  or else Get_Are_Outputting_Typedef (T)
                  or else (Incomplete and then Get_Is_Incomplete_Output (T));
   --  See if we need to write a typedef for T and write one if so. If
   --  Incomplete is True, all we need is the initial portion of a struct
   --  definition.

   --  Provide a set of functions to reference an Str that contains just
   --  a value or contains exactly one value.

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

end CCG.Environment;
