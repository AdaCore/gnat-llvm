------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2026, AdaCore                     --
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

   function Get_MD_Type        (V : Value_T) return MD_Type
     with Pre => Present (V), Inline;
   --  Get the MD Type associated with this value when it was created.
   --  This is used to determine the appropriate C type if we need to create
   --  and declare this value as a variable.

   function Get_Is_Multi_MD    (V : Value_T) return Boolean
     with Pre => Present (V), Inline;
   --  True if this value has been shared between multiple MD_Types

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
   procedure Set_MD_Type        (V : Value_T; M : MD_Type)
     with Pre => Present (V) and Present (M), Post => Get_MD_Type (V) = M,
          Inline;
   procedure Set_Is_Multi_MD    (V : Value_T; B : Boolean := True)
     with Pre => Present (V), Post => Get_Is_Multi_MD (V) = B, Inline;
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

   function Get_MD_Type                  (T : Type_T) return MD_Type
     with Pre => Present (T), Inline;
   --  Get the MD type that T was created from

   function Get_Is_Multi_MD              (T : Type_T) return Boolean
     with Pre => Present (T), Inline;
   --  True if this type has been shared between multiple MD_Types.
   --  This will often be the case for types since LLVM shares types.
   --  So i32 might correspond to both signed and unsigned 32-bit
   --  integers and all pointer types are the same. But in some cases,
   --  such as structs or specific-dimensioned arrays, there may only
   --  be one MD_type that a type came from and we want to take
   --  advantage of those cases.

   function Get_Entity                   (T : Type_T) return Opt_Type_Kind_Id
     with Pre => Present (T), Inline;
   --  Get the GNAT entity for non-void type T, if Present

   procedure Set_MD_Type                  (T : Type_T; M : MD_Type)
     with Pre => Present (T) and Present (M), Post => Get_MD_Type (T) = M,
          Inline;
   procedure Set_Is_Multi_MD              (T : Type_T; B : Boolean := True)
     with Pre => Present (T), Post => Get_Is_Multi_MD (T) = B, Inline;
   procedure Set_Entity                   (T : Type_T; TE : Type_Kind_Id)
     with Pre => Present (T), Post => Get_Entity (T) = TE, Inline;

   function Get_Entity                    (M : MD_Type) return Opt_Type_Kind_Id
     with Pre => Present (M), Inline;
   --  Get the GNAT entity for non-void type M, if Present

   function Get_Is_Typedef_Output         (M : MD_Type) return Boolean
     with Pre => Present (M), Inline;
   --  True if this is a type either for which we don't write a typedef
   --  or if it is and we've written that typedef previously.

   function Get_Is_Return_Typedef_Output  (M : MD_Type) return Boolean
     with Pre => Present (M), Inline;
   --  True if this is an array type and we've written the struct type
   --  that we use for the return type of a function returning this type.

   function Get_Is_Incomplete_Output      (M : MD_Type) return Boolean
     with Pre => Present (M), Inline;
   --  True if this is a struct type and we've just written the struct
   --  definition without fields (an incomplete type).

   function Get_Are_Outputting_Typedef    (M : MD_Type) return Boolean
     with Pre => Present (M), Inline;
   --  True if we're in the process of outputting a typedef

   function Get_Used_In_Struct            (M : MD_Type) return Boolean
     with Pre => Present (M), Inline;
   --  True if T is the type of an element of a struct

   function Get_Cannot_Pack               (M : MD_Type) return Boolean
     with Pre => Present (M), Inline;
   --  True if this is a type that we want to pack, but can't because of
   --  restrictions in our C compiler.

   procedure Set_Entity                   (M : MD_Type; TE : Type_Kind_Id)
     with Pre => Present (M), Post => Get_Entity (M) = TE, Inline;
   procedure Set_Is_Typedef_Output        (M : MD_Type; B : Boolean := True)
     with Pre => Present (M), Post => Get_Is_Typedef_Output (M) = B, Inline;
   procedure Set_Is_Return_Typedef_Output (M : MD_Type; B : Boolean := True)
     with Pre => Present (M), Post => Get_Is_Return_Typedef_Output (M) = B,
          Inline;
   procedure Set_Is_Incomplete_Output     (M : MD_Type; B : Boolean := True)
     with Pre => Present (M), Post => Get_Is_Incomplete_Output (M) = B,
          Inline;
   procedure Set_Are_Outputting_Typedef   (M : MD_Type; B : Boolean := True)
     with Pre => Present (M), Post => Get_Are_Outputting_Typedef (M) = B,
          Inline;
   procedure Set_Used_In_Struct           (M : MD_Type; B : Boolean := True)
     with Pre => Present (M), Post => Get_Used_In_Struct (M) = B,
          Inline;
   procedure Set_Cannot_Pack              (M : MD_Type; B : Boolean := True)
     with Pre => Present (M), Post => Get_Cannot_Pack (M) = B,
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
   function Get_Output_Idx (M : MD_Type) return Nat
     with Pre => Present (M), Post => Get_Output_Idx'Result /= 0, Inline;
   function Get_Output_Idx (BB : Basic_Block_T) return Nat
     with Pre => Present (BB), Post => Get_Output_Idx'Result /= 0, Inline;
   function Get_Output_Idx                      return Nat
     with Post => Get_Output_Idx'Result /= 0, Inline;

   procedure Maybe_Output_Typedef (MD : MD_Type; Incomplete : Boolean := False)
     with Pre  => Present (MD),
          Post => Get_Is_Typedef_Output (MD)
                  or else Get_Are_Outputting_Typedef (MD)
                  or else (Incomplete and then Get_Is_Incomplete_Output (MD));
   --  See if we need to write a typedef for MD and write one if so. If
   --  Incomplete is True, all we need is the initial portion of a struct
   --  definition.

   --  We provide a table mapping external names to their MD types in case
   --  the optimizer recreates a function or external declaration.

   function Get_MD_Type (S : String) return MD_Type;
   procedure Set_MD_Type (S : String; MD : MD_Type)
     with Post => Get_MD_Type (S) = MD;

end CCG.Environment;
