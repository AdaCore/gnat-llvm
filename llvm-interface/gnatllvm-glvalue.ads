------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with Atree; use Atree;
with Einfo; use Einfo;
with Stand; use Stand;
with Types; use Types;
with Uintp; use Uintp;

with LLVM.Core;   use LLVM.Core;
with LLVM.Types;  use LLVM.Types;

with Interfaces.C;             use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.Wrapper;     use GNATLLVM.Wrapper;

package GNATLLVM.GLValue is

   --  Define basic accesss predicates for components of GL_Value

   function Has_Known_Etype (G : GL_Value) return Boolean is
     (not G.Is_Reference and then not G.Is_Subprogram_Type)
     with Pre => Present (G);
   --  True is we know what G's Etype is

   function Etype (G : GL_Value) return Entity_Id is
     (G.Typ)
     with Pre => Present (G) and then Has_Known_Etype (G),
          Post => Present (Etype'Result);

   function LLVM_Value (G : GL_Value) return Value_T is
     (G.Value)
     with Pre => Present (G), Post => Present (LLVM_Value'Result);

   function Is_Subprogram_Type (G : GL_Value) return Boolean is
     (G.Is_Subprogram_Type)
     with Pre => Present (G);

   --  Now we have constructors for a GL_Value

   function G
     (V                    : Value_T;
      TE                   : Entity_Id;
      Is_Reference         : Boolean := False;
      Is_Raw_Array         : Boolean := False;
      Is_Subprogram_Type   : Boolean := False) return GL_Value
   is
     ((V, TE, Is_Reference, Is_Raw_Array, Is_Subprogram_Type))
     with Pre => Present (V) and then Is_Type_Or_Void (TE);
   --  Raw constructor that allow full specification of all fields

   function G_From (V : Value_T; GV : GL_Value) return GL_Value is
     (G (V, GV.Typ,
         Is_Reference       => GV.Is_Reference,
         Is_Raw_Array       => GV.Is_Raw_Array,
         Is_Subprogram_Type => GV.Is_Subprogram_Type))
     with Pre  => Present (V) and then Present (GV),
          Post => Present (G_From'Result);
   --  Constructor for most common operation cases where we aren't changing
   --  any typing information, so we just copy it from an existing value.

   function G_Ref (V : Value_T; TE : Entity_Id) return GL_Value is
     (G (V, TE, True))
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (G_Ref'Result);
   --  Constructor for case were we've create a value that's a pointer to
   --  type TE.

   function G_Ref (V : GL_Value; TE : Entity_Id) return GL_Value is
     (G (LLVM_Value (V), TE, True))
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (G_Ref'Result);
   --  Likewise but when we already have a GL_Value

   procedure Discard (G : GL_Value);
   --  Evaluate G and throw away the result

   --  Now define predicates on the GL_Value type to easily access
   --  properties of the LLVM value and the effective type.  These have the
   --  same names as those for types and Value_T's.  The first of these
   --  represent abstractions that will be used in later predicates.

   function Full_Etype (G : GL_Value) return Entity_Id is
     (Etype (G))
     with Pre => Present (G), Post => Is_Type_Or_Void (Full_Etype'Result);

   function Type_Of (G : GL_Value) return Type_T is
     (Type_Of (LLVM_Value (G)))
     with Pre => Present (G), Post => Present (Type_Of'Result);

   function Ekind (G : GL_Value) return Entity_Kind is
     ((if G.Is_Reference then E_Access_Type else Ekind (Etype (G))))
     with Pre => Present (G);

   function Is_Access_Type (G : GL_Value) return Boolean is
     (Is_Reference (G) or else Is_Access_Type (Etype (G)))
     with Pre => Present (G);

   function Full_Designated_Type (G : GL_Value) return Entity_Id
     with Pre  => Is_Access_Type (G),
          Post => Is_Type_Or_Void (Full_Designated_Type'Result);

   function Implementation_Base_Type (G : GL_Value) return Entity_Id is
     (Implementation_Base_Type (Etype (G)))
     with Pre  => not Is_Reference (G),
            Post => Is_Type (Implementation_Base_Type'Result);

   function Is_Dynamic_Size (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Dynamic_Size (Full_Etype (G)))
     with Pre => Present (G);

   function Is_Array_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Array_Type (Etype (G)))
     with Pre => Present (G);

   function Is_Access_Unconstrained (G : GL_Value) return Boolean is
     (Is_Access_Type (G) and then Ekind (G.Typ) /= E_Void
        and then Is_Array_Type (Full_Designated_Type (G))
        and then not Is_Constrained (Full_Designated_Type (G))
        and then not Is_Raw_Array (G))
     with Pre => Present (G);

   function Is_Constrained (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Constrained (Full_Etype (G)))
     with Pre => Present (G);

   function Is_Record_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Record_Type (Full_Etype (G)))
     with Pre => Present (G);

   function Is_Composite_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Composite_Type (Full_Etype (G)))
     with Pre => Present (G);

   function Is_Elementary_Type (G : GL_Value) return Boolean is
     (Is_Reference (G) or else Is_Elementary_Type (Full_Etype (G)))
     with Pre => Present (G);

   function Is_Scalar_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Scalar_Type (Full_Etype (G)))
     with Pre => Present (G);

   function Is_Discrete_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Discrete_Type (Full_Etype (G)))
     with Pre => Present (G);

   function Is_Discrete_Or_Fixed_Point_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G)
        and then Is_Discrete_Or_Fixed_Point_Type (Full_Etype (G)))
     with Pre => Present (G);

   function Is_Fixed_Point_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Fixed_Point_Type (Full_Etype (G)))
     with Pre => Present (G);

   function Is_Floating_Point_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Floating_Point_Type (Full_Etype (G)))
     with Pre => Present (G);

   function Is_Unsigned_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Unsigned_Type (Full_Etype (G)))
     with Pre => Present (G);

   function Is_Integer_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Integer_Type (Full_Etype (G)))
     with Pre => Present (G);

   function Is_Modular_Integer_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Modular_Integer_Type (Full_Etype (G)))
     with Pre => Present (G);

   function RM_Size (G : GL_Value) return Uint is
     (RM_Size (Full_Etype (G)))
     with Pre => not Is_Access_Type (G);

   function Esize (G : GL_Value) return Uint is
     (Esize (Full_Etype (G)))
     with Pre => not Is_Access_Type (G);

   function Component_Type (G : GL_Value) return Entity_Id is
     (Component_Type (Full_Etype (G)))
     with Pre => Is_Array_Type (G), Post => Present (Component_Type'Result);

   function Number_Dimensions (G : GL_Value) return Pos is
     (Number_Dimensions (Full_Etype (G)))
     with Pre => Is_Array_Type (G);

   function Make_Reference (V : GL_Value) return GL_Value is
     (G (LLVM_Value (V), Full_Designated_Type (V), Is_Reference => True))
     with Pre  => Is_Access_Type (V),
          Post => Is_Reference (Make_Reference'Result)
                  and then (Full_Designated_Type (Make_Reference'Result) =
                              Full_Designated_Type (V));
   --  Indicate that we want to consider G as a reference to its designated
   --  type.

   --  A GL_Value can either represent an LValue (the address of a value) or
   --  the value itself.  It can only represent the value itself if the value
   --  is representable as an LLVM object, so we can't represent function
   --  values (we can only represent their address) or values representing
   --  variable-sized objects.

   --  When evaluating expressions, especially when they represent values saved
   --  in the environment, we often create either an LValue or the actual
   --  value.  However, we often need either an LValue or a value.  The
   --  following two functions force a GL_Value into one of the two forms.

   function Need_Value (V : GL_Value; TE : Entity_Id) return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (Need_Value'Result);
   --  Get the Value corresponding to V, dereferencing it when needed.
   --  TE is the type of the value.

   function Need_LValue
     (V : GL_Value; TE : Entity_Id; Name : String := "lvalue")
     return GL_Value
     with Pre  => Present (V) and then Is_Type_Or_Void (TE),
          Post => Present (Need_LValue'Result);
   --  Get the LValue corresponding to V, making a new temporary to which
   --  we store the value, if needed.

   function Get_Undef (TE : Entity_Id) return GL_Value
     with Pre  => Is_Type (TE), Post => Present (Get_Undef'Result);

   function Get_Undef_Ref (TE : Entity_Id) return GL_Value
     with Pre  => Is_Type (TE), Post => Is_Reference (Get_Undef_Ref'Result);

   function Get_Undef_Ref (T : Type_T; TE : Entity_Id) return GL_Value is
     (G_Ref (Get_Undef (T), TE))
     with Pre  => Is_Type (TE), Post => Is_Reference (Get_Undef_Ref'Result);

   function Const_Null (TE : Entity_Id) return GL_Value
     with Pre  => Is_Type (TE), Post => Present (Const_Null'Result);

   function Const_Int (TE : Entity_Id; N : Uint) return GL_Value
     with Pre  => Is_Type (TE) and then N /= No_Uint,
          Post => Present (Const_Int'Result);

   function Const_Int
     (TE          : Entity_Id;
      N           : unsigned_long_long;
      Sign_Extend : Boolean := False) return GL_Value
     with Pre  => Is_Type (TE), Post => Present (Const_Int'Result);

   function Const_Ones (TE : Entity_Id) return GL_Value is
     (Const_Int (TE, unsigned_long_long'Last, Sign_Extend => True))
     with Pre  => Is_Type (TE), Post => Present (Const_Ones'Result);
   --  Return an LLVM value for the given type where all bits are set

   function Get_Undef (G : GL_Value) return GL_Value is
     (Get_Undef (Etype (G)))
     with  Pre  => Present (G), Post => Present (Get_Undef'Result);

   function Const_Null (G : GL_Value) return GL_Value is
     (Const_Null (Etype (G)))
     with Pre  => Present (G), Post => Present (Const_Null'Result);

   function Const_Null_Ptr (TE : Entity_Id) return GL_Value
     with Pre  => Is_Type (TE), Post => Present (Const_Null_Ptr'Result);

   function Const_Int (G : GL_Value; N : Uint) return GL_Value is
     (Const_Int (Etype (G), N))
     with Pre  => Present (G) and then N /= No_Uint,
          Post => Present (Const_Int'Result);

   function Const_Int
     (G           : GL_Value;
      N           : unsigned_long_long;
      Sign_Extend : Boolean := False) return GL_Value
   is
     (Const_Int (Etype (G), N, Sign_Extend))
     with Pre  => Present (G), Post => Present (Const_Int'Result);

   function Const_Ones (G : GL_Value) return GL_Value is
     (Const_Ones (Etype (G)))
     with Pre => Present (G), Post => Present (Const_Ones'Result);
   --  Return an LLVM value for the given type where all bits are set

   function Const_Real (TE : Entity_Id; V : double) return GL_Value
     with Pre  => Is_Type (TE), Post => Present (Const_Real'Result);

   function Size_Const_Int (N : Uint) return GL_Value is
     (Const_Int (Env.Size_Type, N))
     with Pre  => N /= No_Uint, Post => Present (Size_Const_Int'Result);

   function Size_Const_Int
     (N : unsigned_long_long; Sign_Extend : Boolean := False) return GL_Value
   is
     (Const_Int (Env.Size_Type, N, Sign_Extend))
     with Post => Present (Size_Const_Int'Result);

   function Const_Int_32 (N : Uint) return GL_Value is
     (Const_Int (Env.Int_32_Type, N))
     with Pre  => N /= No_Uint, Post => Present (Const_Int_32'Result);

   function Const_Int_32
     (N : unsigned_long_long; Sign_Extend : Boolean := False) return GL_Value
   is
     (Const_Int (Env.Int_32_Type, N, Sign_Extend))
     with Post => Present (Const_Int_32'Result);

   function Const_Null_32 return GL_Value
   is
     (Const_Int_32 (0))
     with Post => Present (Const_Null_32'Result);

   function Const_Real (G : GL_Value; V : double) return GL_Value is
     (Const_Real (Etype (G), V))
     with Pre  => Present (G), Post => Present (Const_Real'Result);

   function Const_True return GL_Value is
     (Const_Int (Standard_Boolean, 1));
   function Const_False return GL_Value is
     (Const_Int (Standard_Boolean, 0));

   --  Define IR builder variants which take and/or return GL_Value

   function Alloca (TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Is_Type (TE),
          Post => Present (Alloca'Result)
                  and then Is_Access_Type (Alloca'Result);

   function Array_Alloca
     (TE : Entity_Id; Num_Elts : GL_Value; Name : String := "") return GL_Value
     with Pre  => Is_Type (TE) and then Present (Num_Elts),
          Post => Present (Array_Alloca'Result)
                  and then Is_Access_Type (Array_Alloca'Result);

   function Int_To_Ptr (V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (Int_To_Ptr'Result);

   function Ptr_To_Int
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (Ptr_To_Int'Result);

   function Bit_Cast
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (Bit_Cast'Result);

   function Pointer_Cast
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (Pointer_Cast'Result);

   function Ptr_To_Ref
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (Ptr_To_Ref'Result);

   function Trunc
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (Trunc'Result);

   function S_Ext
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (S_Ext'Result);

   function Z_Ext
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (Z_Ext'Result);

   function FP_Trunc
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (FP_Trunc'Result);

   function FP_Ext
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (FP_Ext'Result);

   function FP_To_SI
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (FP_To_SI'Result);

   function FP_To_UI
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (FP_To_UI'Result);

   function UI_To_FP
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (UI_To_FP'Result);

   function SI_To_FP
     (V : GL_Value; TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (SI_To_FP'Result);

   function Int_To_Ptr
     (V, T : GL_Value; Name : String := "") return GL_Value is
     (Int_To_Ptr (V, Full_Etype (T), Name))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Int_To_Ptr'Result);

   function Ptr_To_Int
     (V, T : GL_Value; Name : String := "") return GL_Value is
     (Ptr_To_Int (V, Full_Etype (T), Name))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Ptr_To_Int'Result);

   function Bit_Cast (V, T : GL_Value; Name : String := "") return GL_Value is
     (Bit_Cast (V, Full_Etype (T), Name))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Bit_Cast'Result);

   function Pointer_Cast
     (V, T : GL_Value; Name : String := "") return GL_Value is
     (Pointer_Cast (V, Full_Etype (T), Name))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Pointer_Cast'Result);

   function Trunc (V, T : GL_Value; Name : String := "") return GL_Value is
     (Trunc (V, Etype (T), Name))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Trunc'Result);

   function S_Ext (V, T : GL_Value; Name : String := "") return GL_Value is
     (S_Ext (V, Etype (T), Name))
     with Pre  => Present (V) and then Present (T),
          Post => Present (S_Ext'Result);

   function Z_Ext (V, T : GL_Value; Name : String := "") return GL_Value is
     (Z_Ext (V, Etype (T), Name))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Z_Ext'Result);

   function FP_Trunc (V, T : GL_Value; Name : String := "") return GL_Value is
     (FP_Trunc (V, Etype (T), Name))
     with Pre  => Present (V) and then Present (T),
          Post => Present (FP_Trunc'Result);

   function FP_Ext (V, T : GL_Value; Name : String := "") return GL_Value is
     (FP_Ext (V, Etype (T), Name))
     with Pre  => Present (V) and then Present (T),
          Post => Present (FP_Ext'Result);

   function FP_To_SI (V, T : GL_Value; Name : String := "") return GL_Value is
     (FP_To_SI (V, Etype (T), Name))
     with Pre  => Present (V) and then Present (T),
          Post => Present (FP_To_SI'Result);

   function FP_To_UI (V, T : GL_Value; Name : String := "") return GL_Value is
     (FP_To_UI (V, Etype (T), Name))
     with Pre  => Present (V) and then Present (T),
          Post => Present (FP_To_UI'Result);

   function UI_To_FP (V, T : GL_Value; Name : String := "") return GL_Value is
     (UI_To_FP (V, Etype (T), Name))
     with Pre  => Present (V) and then Present (T),
          Post => Present (UI_To_FP'Result);

   function SI_To_FP (V, T : GL_Value; Name : String := "") return GL_Value is
     (SI_To_FP (V, Etype (T), Name))
     with Pre  => Present (V) and then Present (T),
          Post => Present (SI_To_FP'Result);

   procedure Store (Expr : GL_Value; Ptr : GL_Value)
     with Pre => Present (Expr)
                 and then Present (Ptr) and then Is_Access_Type (Ptr);

   function Load (Ptr : GL_Value; Name : String := "") return GL_Value
     with Pre  => Present (Ptr) and then Is_Access_Type (Ptr),
          Post => Present (Load'Result);

   function I_Cmp
     (Op       : Int_Predicate_T;
      LHS, RHS : GL_Value;
      Name     : String := "") return GL_Value
   is
     (G (I_Cmp (Env.Bld, Op, LLVM_Value (LHS), LLVM_Value (RHS), Name),
         Standard_Boolean))
     with Pre  => Present (LHS) and then Present (RHS),
          Post => Present (I_Cmp'Result);

   function F_Cmp
     (Op       : Real_Predicate_T;
      LHS, RHS : GL_Value;
      Name     : String := "") return GL_Value
   is
     (G (F_Cmp (Env.Bld, Op, LLVM_Value (LHS), LLVM_Value (RHS), Name),
         Standard_Boolean))
     with Pre  => Present (LHS) and then Present (RHS),
          Post => Present (F_Cmp'Result);

   function NSW_Add
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G_From (NSW_Add (Env.Bld, LLVM_Value (LHS), LLVM_Value (RHS), Name),
               LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (NSW_Add'Result);

   function NSW_Sub
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (NSW_Sub (Env.Bld, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (NSW_Sub'Result);

   function NSW_Mul
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (NSW_Mul (Env.Bld, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (NSW_Mul'Result);

   function S_Div
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (S_Div (Env.Bld, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (S_Div'Result);

   function U_Div
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (U_Div (Env.Bld, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (U_Div'Result);

   function S_Rem
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (S_Rem (Env.Bld, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (S_Rem'Result);

   function U_Rem
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (U_Rem (Env.Bld, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (U_Rem'Result);

   function Build_And
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Build_And (Env.Bld, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (Build_And'Result);

   function Build_Or
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Build_Or (Env.Bld, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (Build_Or'Result);

   function Build_Xor
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Build_Xor (Env.Bld, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (Build_Xor'Result);

   function F_Add
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (F_Add (Env.Bld, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (F_Add'Result);

   function F_Sub
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (F_Sub (Env.Bld, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (F_Sub'Result);

   function F_Mul
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (F_Mul (Env.Bld, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (F_Mul'Result);

   function F_Div
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (F_Div (Env.Bld, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (F_Div'Result);

   function Shl
     (E, Count : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Shl (Env.Bld, LLVM_Value (E), LLVM_Value (Count), Name),
              E))
      with Pre  => Present (E) and then Present (Count),
           Post => Present (Shl'Result);

   function L_Shr
     (E, Count : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (L_Shr (Env.Bld, LLVM_Value (E), LLVM_Value (Count), Name),
              E))
      with Pre  => Present (E) and then Present (Count),
           Post => Present (L_Shr'Result);

   function A_Shr
     (E, Count : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (A_Shr (Env.Bld, LLVM_Value (E), LLVM_Value (Count), Name),
              E))
      with Pre  => Present (E) and then Present (Count),
           Post => Present (A_Shr'Result);

   function Build_Not
     (V : GL_Value; Name : String := "") return GL_Value
   is
      (G_From (Build_Not (Env.Bld, LLVM_Value (V), Name), V))
      with Pre  => Present (V),
           Post => Present (Build_Not'Result);

   function NSW_Neg
     (V : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (NSW_Neg (Env.Bld, LLVM_Value (V), Name), V))
      with Pre  => Present (V), Post => Present (NSW_Neg'Result);

   function F_Neg
     (V : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (F_Neg (Env.Bld, LLVM_Value (V), Name), V))
     with Pre  => Present (V), Post => Present (F_Neg'Result);

   function Build_Select
     (C_If, C_Then, C_Else : GL_Value; Name : String := "")
     return GL_Value
   is
     (G_From (Build_Select (Env.Bld, C_If => LLVM_Value (C_If),
                            C_Then => LLVM_Value (C_Then),
                            C_Else => LLVM_Value (C_Else), Name => Name),
              C_Then))
     with Pre  => Present (C_If) and then Present (C_Then)
                  and then Present (C_Else),
          Post => Present (Build_Select'Result);

   procedure Build_Cond_Br
     (C_If : GL_Value; C_Then, C_Else : Basic_Block_T)
     with Pre => Present (C_If)
                 and then Present (C_Then) and then Present (C_Else);

   procedure Build_Ret (G : GL_Value)
     with Pre => Present (G);

   procedure Build_Ret_Void;

   procedure Build_Unreachable;

   function Build_Phi
     (GL_Values : GL_Value_Array;
      BBs       : Basic_Block_Array;
      Name      : String := "") return GL_Value
     with Pre  => GL_Values'First = BBs'First
                  and then GL_Values'Last = BBs'Last,
          Post => Present (Build_Phi'Result);

   function Int_To_Ref
     (V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Present (Int_To_Ref'Result)
                  and then Is_Access_Type (Int_To_Ref'Result);
   --  Similar to Int_To_Ptr, but TE is the Designed_Type, not the
   --  access type.

   function Extract_Value
     (Typ   : Entity_Id;
      Arg   : GL_Value;
      Index : unsigned;
      Name  : String := "") return GL_Value
   is
     (G (Extract_Value (Env.Bld, LLVM_Value (Arg), Index, Name), Typ))
     with  Pre  => Present (Arg) and then Is_Type (Typ),
           Post => Present (Extract_Value'Result);

   function Extract_Value_To_Ref
     (Typ   : Entity_Id;
      Arg   : GL_Value;
      Index : unsigned;
      Name  : String := "") return GL_Value
   is
      (G (Extract_Value (Env.Bld, LLVM_Value (Arg), Index, Name), Typ,
         Is_Reference => True))
     with  Pre  => Present (Arg) and then Is_Type (Typ),
           Post => Present (Extract_Value_To_Ref'Result);

   function Insert_Value
     (Arg, Elt : GL_Value;
      Index    : unsigned;
      Name     : String := "") return GL_Value
   is
     (G_From (Insert_Value (Env.Bld, LLVM_Value (Arg), LLVM_Value (Elt),
                            Index, Name),
              Arg))
     with  Pre  => Present (Arg) and then Present (Elt),
           Post => Present (Insert_Value'Result);

   type Index_Array is array (Integer range <>) of Natural;

   function Extract_Value
     (Typ     : Entity_Id;
      Arg     : GL_Value;
      Idx_Arr : Index_Array;
      Name    : String := "") return GL_Value
   is
     (G (Build_Extract_Value (Env.Bld, LLVM_Value (Arg),
                              Idx_Arr'Address, Idx_Arr'Length, Name),
         Typ))
     with  Pre  => Is_Type (Typ) and then Present (Arg),
           Post => Present (Extract_Value'Result);

   function Extract_Value_To_Ref
     (Typ     : Entity_Id;
      Arg     : GL_Value;
      Idx_Arr : Index_Array;
      Name    : String := "") return GL_Value
   is
     (G (Build_Extract_Value (Env.Bld, LLVM_Value (Arg),
                              Idx_Arr'Address, Idx_Arr'Length, Name),
         Typ, Is_Reference => True))
     with  Pre  => Is_Type (Typ) and then Present (Arg),
           Post => Present (Extract_Value_To_Ref'Result);

   function Insert_Value
     (Arg, Elt : GL_Value;
      Idx_Arr  : Index_Array;
      Name     : String := "") return GL_Value
   is
     (G_From (Build_Insert_Value (Env.Bld, LLVM_Value (Arg), LLVM_Value (Elt),
                                  Idx_Arr'Address, Idx_Arr'Length, Name),
              Arg))
     with  Pre  => Present (Arg) and then Present (Elt),
           Post => Present (Insert_Value'Result);

   function GEP
     (Result_Type : Entity_Id;
      Ptr         : GL_Value;
      Indices     : GL_Value_Array;
      Name        : String := "") return GL_Value
     with Pre  => Present (Ptr),
          Post => Present (GEP'Result);
   --  Helper for LLVM's Build_GEP

   function Call
     (Func        : GL_Value;
      Result_Type : Entity_Id;
      Args        : GL_Value_Array;
      Name        : String := "") return GL_Value
     with Pre  => Present (Func) and then Is_Type_Or_Void (Result_Type),
          Post => Present (Call'Result);

   procedure Call
     (Func : GL_Value; Args : GL_Value_Array; Name : String := "")
     with Pre  => Present (Func);

   function Build_Switch
     (V : GL_Value; Default : Basic_Block_T; Blocks : Nat) return Value_T is
     (Build_Switch (Env.Bld, LLVM_Value (V), Default, unsigned (Blocks)))
     with Pre  => Present (V) and then Present (Default),
          Post => Present (Build_Switch'Result);

   function Add_Function
     (Name : String; T : Type_T; Return_TE : Entity_Id) return GL_Value is
     (G (Add_Function (Env.Mdl, Name, T), Return_TE,
         Is_Subprogram_Type => True))
     with Pre  => Present (T) and then Is_Type_Or_Void (Return_TE),
          Post => Present (Add_Function'Result);
   --  Add a function to the environment

   function Add_Global (TE : Entity_Id; Name : String) return GL_Value
     with Pre  => Is_Type (TE), Post => Present (Add_Global'Result);
     --  Add a global to the environment which is of type TE, so the global
     --  itself represents the address of TE.

   procedure Set_Initializer (Var, Expr : GL_Value)
     with Pre => Present (Var) and then Present (Expr);
   --  Set the initializer for a global variable

   procedure Set_Linkage (Var : GL_Value; Linkage : Linkage_T)
     with Pre => Present (Var);
   --  Set the linkage type for a variable

end GNATLLVM.GLValue;
