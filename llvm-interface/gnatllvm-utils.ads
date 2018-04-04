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
with Namet; use Namet;
with Sinfo; use Sinfo;
with Stand; use Stand;
with Types; use Types;
with Uintp; use Uintp;
with Uintp.LLVM;

with LLVM.Core;   use LLVM.Core;
with LLVM.Target; use LLVM.Target;
with LLVM.Types;  use LLVM.Types;

with Interfaces.C;             use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with GNATLLVM.Environment; use GNATLLVM.Environment;

package GNATLLVM.Utils is

   function Get_Fullest_View (E : Entity_Id) return Entity_Id
     with Pre => Is_Type (E), Post => Is_Type (Get_Fullest_View'Result);
   --  Get the fullest possible view of E, looking through private,
   --  limited, and packed array implementation types.

   function Full_Etype (N : Node_Id) return Entity_Id is
     (if Ekind (Etype (N)) = E_Void then Etype (N)
      else Get_Fullest_View (Etype (N)));

   function Full_Component_Type (E : Entity_Id) return Entity_Id is
     (Get_Fullest_View (Component_Type (E)))
     with Pre  => Is_Array_Type (E),
          Post => Present (Full_Component_Type'Result);

   type Value_Array is array (Nat range <>) of Value_T;
   type Basic_Block_Array is array (Nat range <>) of Basic_Block_T;

   procedure Store (Bld : Builder_T; Expr : Value_T; Ptr : Value_T)
     with Pre => Present (Bld) and then Present (Expr) and then Present (Ptr);
   --  Helper for LLVM's Build_Store

   procedure Store_With_Type
     (Env : Environ; TE : Entity_Id; Expr : Value_T; Ptr : Value_T)
     with Pre => Env /= null and then Is_Type (TE)
                 and then Present (Expr) and then Present (Ptr);
   --  Similar, but allows annotating store

   function Load_With_Type
     (Env : Environ; TE : Entity_Id; Ptr : Value_T; Name : String := "")
     return Value_T
     with Pre  => Env /= null and then Is_Type (TE) and then Present (Ptr),
          Post => Present (Load_With_Type'Result);
   --  Likewise for a load

   function GEP
     (Bld     : Builder_T;
      Ptr     : Value_T;
      Indices : Value_Array;
      Name    : String := "")
     return Value_T
     with Pre  => Present (Bld) and then Present (Ptr),
          Post => Present (GEP'Result);
   --  Helper for LLVM's Build_GEP

   function Const_Ones (T : Type_T) return Value_T is
     (Const_Int (T, unsigned_long_long'Last, Sign_Extend => True))
     with Pre => Present (T), Post => Present (Const_Ones'Result);
   --  Return an LLVM value for the given type where all bits are set

   function G
     (V            : Value_T;
      TE           : Entity_Id;
      Is_Reference : Boolean := False;
      Is_Raw_Array : Boolean := False) return GL_Value
   is
     ((V, TE, Is_Reference, Is_Raw_Array))
     with Pre => Present (V) and then Is_Type_Or_Void (TE);

   --  Now define predicates on the GL_Value type to easily access
   --  properties of the LLVM value and the effective type.  These have the
   --  same names as those for types and Value_T's.

   function Type_Of (G : GL_Value) return Type_T is
     (Type_Of (G.Value))
     with Pre => Present (G), Post => Present (Type_Of'Result);

   function Etype (G : GL_Value) return Entity_Id is
     (G.Typ)
     with Pre => Present (G), Post => Present (Etype'Result);

   function Full_Etype (G : GL_Value) return Entity_Id is
     (G.Typ)
     with Pre => Present (G), Post => Present (Full_Etype'Result);

   function Ekind (G : GL_Value) return Entity_Kind is
     (Ekind (G.Typ))
     with Pre => Present (G);

   function Is_Access_Type (G : GL_Value) return Boolean is
     (Is_Reference (G) or else Is_Access_Type (G.Typ))
     with Pre => Present (G);

   function Designated_Type (G : GL_Value) return Entity_Id is
     ((if Is_Reference (G) then G.Typ
       else Get_Fullest_View (Designated_Type (G.Typ))))
     with Pre => Is_Access_Type (G), Post => Is_Type (Designated_Type'Result);

   function Implementation_Base_Type (G : GL_Value) return Entity_Id is
     ((if Is_Reference (G) then G.Typ else Implementation_Base_Type (G.Typ)))
       with Pre  => not Is_Reference (G),
            Post => Is_Type (Implementation_Base_Type'Result);

   function Is_Dynamic_Size (Env : Environ; G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Dynamic_Size (Env, G.Typ))
     with Pre => Env /= null and then Present (G);

   function Is_Array_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Array_Type (G.Typ))
     with Pre => Present (G);

   function Is_Access_Unconstrained (G : GL_Value) return Boolean is
     (Is_Access_Type (G) and then Is_Array_Type (Designated_Type (G))
                         and then not Is_Constrained (Designated_Type (G))
                         and then not Is_Raw_Array (G))
     with Pre => Present (G);

   function Is_Constrained (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Constrained (G.Typ))
     with Pre => Present (G);

   function Is_Record_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Record_Type (G.Typ))
     with Pre => Present (G);

   function Is_Composite_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Composite_Type (G.Typ))
     with Pre => Present (G);

   function Is_Elementary_Type (G : GL_Value) return Boolean is
     (Is_Reference (G) or else Is_Elementary_Type (G.Typ))
     with Pre => Present (G);

   function Is_Scalar_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Scalar_Type (G.Typ))
     with Pre => Present (G);

   function Is_Discrete_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Discrete_Type (G.Typ))
     with Pre => Present (G);

   function Is_Discrete_Or_Fixed_Point_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Discrete_Or_Fixed_Point_Type (G.Typ))
     with Pre => Present (G);

   function Is_Fixed_Point_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Fixed_Point_Type (G.Typ))
     with Pre => Present (G);

   function Is_Floating_Point_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Floating_Point_Type (G.Typ))
     with Pre => Present (G);

   function Is_Unsigned_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Unsigned_Type (G.Typ))
     with Pre => Present (G);

   function Is_Modular_Integer_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Modular_Integer_Type (G.Typ))
     with Pre => Present (G);

   function RM_Size (G : GL_Value) return Uint is
     (RM_Size (G.Typ))
     with Pre => not Is_Access_Type (G);

   function Esize (G : GL_Value) return Uint is
     (Esize (G.Typ))
     with Pre => not Is_Access_Type (G);

   function Component_Type (G : GL_Value) return Entity_Id is
     (Component_Type (G.Typ))
     with Pre => Is_Array_Type (G), Post => Present (Component_Type'Result);

   function Number_Dimensions (G : GL_Value) return Pos is
     (Number_Dimensions (G.Typ))
     with Pre => Is_Array_Type (G);

   function Get_Undef (Env : Environ; TE : Entity_Id) return GL_Value
     with Pre  => Env /= null and then Is_Type (TE),
          Post => Present (Get_Undef'Result);

   function Const_Null (Env : Environ; TE : Entity_Id) return GL_Value
     with Pre  => Env /= null and then Is_Type (TE),
          Post => Present (Const_Null'Result);

   function Const_Int
     (Env : Environ; TE : Entity_Id; N : Uint) return GL_Value
     with Pre  => Env /= null and then Is_Type (TE) and then N /= No_Uint,
          Post => Present (Const_Int'Result);

   function Const_Int
     (Env         : Environ;
      TE          : Entity_Id;
      N           : unsigned_long_long;
      Sign_Extend : Boolean := False) return GL_Value
     with Pre  => Env /= null and then Is_Type (TE),
          Post => Present (Const_Int'Result);

   function Const_Ones (Env : Environ; TE : Entity_Id) return GL_Value is
     (Const_Int (Env, TE, unsigned_long_long'Last, Sign_Extend => True))
     with Pre  => Env /= null and then Is_Type (TE),
          Post => Present (Const_Ones'Result);

   --  Return an LLVM value for the given type where all bits are set
   function Get_Undef (Env : Environ; G : GL_Value) return GL_Value is
     (Get_Undef (Env, G.Typ))
     with  Pre  => Env /= null and then Present (G),
           Post => Present (Get_Undef'Result);

   function Const_Null (Env : Environ; G : GL_Value) return GL_Value is
     (Const_Null (Env, G.Typ))
     with Pre  => Env /= null and then Present (G),
          Post => Present (Const_Null'Result);

   function Const_Int
     (Env : Environ; G : GL_Value; N : Uint) return GL_Value is
     (Const_Int (Env, G.Typ, N))
     with Pre  => Env /= null and then Present (G) and then N /= No_Uint,
          Post => Present (Const_Int'Result);

   function Const_Int
     (Env         : Environ;
      G           : GL_Value;
      N           : unsigned_long_long;
      Sign_Extend : Boolean := False) return GL_Value
   is
     (Const_Int (Env, G.Typ, N, Sign_Extend))
     with Pre  => Env /= null and then Present (G),
          Post => Present (Const_Int'Result);

   function Const_Ones (Env : Environ; G : GL_Value) return GL_Value is
     (Const_Ones (Env, G.Typ))
     with Pre => Present (G), Post => Present (Const_Ones'Result);
   --  Return an LLVM value for the given type where all bits are set

   function Const_Real
     (Env : Environ; TE : Entity_Id; V : double) return GL_Value
     with Pre  => Env /= null and then Present (TE),
          Post => Present (Const_Real'Result);

   function Size_Const_Int
     (Env : Environ; N : Uint) return GL_Value is
     (Const_Int (Env, Env.Size_Type, N))
     with Pre  => Env /= null and then N /= No_Uint,
          Post => Present (Size_Const_Int'Result);

   function Size_Const_Int
     (Env : Environ;
      N : unsigned_long_long;
      Sign_Extend : Boolean := False) return GL_Value
   is
     (Const_Int (Env, Env.Size_Type, N, Sign_Extend))
     with Pre => Env /= null, Post => Present (Size_Const_Int'Result);

   function Const_Real
     (Env : Environ; G : GL_Value; V : double) return GL_Value is
     (Const_Real (Env, G.Typ, V))
     with Pre  => Env /= null and then Present (G),
          Post => Present (Const_Real'Result);

   --  Define IR builder variants which take and/or return GL_Value

   function Alloca
      (Env : Environ; TE : Entity_Id; Name : String := "") return GL_Value
     with Pre  => Env /= null and then Present (TE),
          Post => Present (Alloca'Result)
                  and then Is_Access_Type (Alloca'Result);

   function Array_Alloca
     (Env      : Environ;
      TE       : Entity_Id;
      Num_Elts : GL_Value;
      Name     : String := "") return GL_Value
     with Pre  => Env /= null and then Present (TE)
                  and then Present (Num_Elts),
          Post => Present (Array_Alloca'Result)
                  and then Is_Access_Type (Array_Alloca'Result);

   function Int_To_Ptr
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (Int_To_Ptr'Result);

   function Ptr_To_Int
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (Ptr_To_Int'Result);

   function Bit_Cast
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (Bit_Cast'Result);

   function Pointer_Cast
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (Pointer_Cast'Result);

   function Ptr_To_Ref
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (Ptr_To_Ref'Result);

   function Trunc
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (Trunc'Result);

   function S_Ext
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (S_Ext'Result);

   function Z_Ext
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (Z_Ext'Result);

   function FP_Trunc
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (FP_Trunc'Result);

   function FP_Ext
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (FP_Ext'Result);

   function FP_To_SI
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (FP_To_SI'Result);

   function FP_To_UI
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (FP_To_UI'Result);

   function UI_To_FP
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (UI_To_FP'Result);

   function SI_To_FP
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (SI_To_FP'Result);

   function Trunc
     (Env : Environ; V : GL_Value; T : GL_Value; Name : String := "")
     return GL_Value
   is
     (Trunc (Env, V, T.Typ, Name))
     with Pre  => Env /= null and then Present (V) and then Present (T),
          Post => Present (Trunc'Result);

   function S_Ext
     (Env : Environ; V : GL_Value; T : GL_Value; Name : String := "")
     return GL_Value
   is
     (S_Ext (Env, V, T.Typ, Name))
     with Pre  => Env /= null and then Present (V) and then Present (T),
          Post => Present (S_Ext'Result);

   function Z_Ext
     (Env : Environ; V : GL_Value; T : GL_Value; Name : String := "")
     return GL_Value
   is
     (Z_Ext (Env, V, T.Typ, Name))
     with Pre  => Env /= null and then Present (V) and then Present (T),
          Post => Present (Z_Ext'Result);

   function FP_Trunc
     (Env : Environ; V : GL_Value; T : GL_Value; Name : String := "")
     return GL_Value
   is
     (FP_Trunc (Env, V, T.Typ, Name))
     with Pre  => Env /= null and then Present (V) and then Present (T),
          Post => Present (FP_Trunc'Result);

   function FP_Ext
     (Env : Environ; V : GL_Value; T : GL_Value; Name : String := "")
     return GL_Value
   is
     (FP_Ext (Env, V, T.Typ, Name))
     with Pre  => Env /= null and then Present (V) and then Present (T),
          Post => Present (FP_Ext'Result);

   function FP_To_SI
     (Env : Environ; V : GL_Value; T : GL_Value; Name : String := "")
     return GL_Value
   is
     (FP_To_SI (Env, V, T.Typ, Name))
     with Pre  => Env /= null and then Present (V) and then Present (T),
          Post => Present (FP_To_SI'Result);

   function FP_To_UI
     (Env : Environ; V : GL_Value; T : GL_Value; Name : String := "")
     return GL_Value
   is
     (FP_To_UI (Env, V, T.Typ, Name))
     with Pre  => Env /= null and then Present (V) and then Present (T),
          Post => Present (FP_To_UI'Result);

   function UI_To_FP
     (Env : Environ; V : GL_Value; T : GL_Value; Name : String := "")
     return GL_Value
   is
     (UI_To_FP (Env, V, T.Typ, Name))
     with Pre  => Env /= null and then Present (V) and then Present (T),
          Post => Present (UI_To_FP'Result);

   function SI_To_FP
     (Env : Environ; V : GL_Value; T : GL_Value; Name : String := "")
     return GL_Value
   is
     (SI_To_FP (Env, V, T.Typ, Name))
     with Pre  => Env /= null and then Present (V) and then Present (T),
          Post => Present (SI_To_FP'Result);

   procedure Store
     (Env : Environ; Expr : GL_Value; Ptr : GL_Value)
     with Pre => Env /= null and then Present (Expr)
                 and then Present (Ptr) and then Is_Access_Type (Ptr);

   function Load (Env : Environ; Ptr : GL_Value; Name : String := "")
                 return GL_Value is
     (G (Load_With_Type (Env, Ptr.Typ, Ptr.Value, Name),
         Designated_Type (Ptr)))
     with Pre  => Env /= null and then Present (Ptr)
                  and then  Is_Access_Type (Ptr),
          Post => Present (Load'Result);

   function I_Cmp
     (Env      : Environ;
      Op       : Int_Predicate_T;
      LHS, RHS : GL_Value;
      Name     : String := "") return GL_Value
   is
     (G (I_Cmp (Env.Bld, Op, LHS.Value, RHS.Value, Name), Standard_Boolean))
     with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
          Post => Present (I_Cmp'Result);

   function F_Cmp
     (Env      : Environ;
      Op       : Real_Predicate_T;
      LHS, RHS : GL_Value;
      Name     : String := "") return GL_Value
   is
     (G (F_Cmp (Env.Bld, Op, LHS.Value, RHS.Value, Name), Standard_Boolean))
     with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
          Post => Present (F_Cmp'Result);

   function NSW_Add
     (Env : Environ; LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G (NSW_Add (Env.Bld, LHS.Value, RHS.Value, Name),
          LHS.Typ, Is_Reference => LHS.Is_Reference or RHS.Is_Reference))
      with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
           Post => Present (NSW_Add'Result);

   function NSW_Sub
     (Env : Environ; LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G (NSW_Sub (Env.Bld, LHS.Value, RHS.Value, Name),
          LHS.Typ, Is_Reference => LHS.Is_Reference or RHS.Is_Reference))
      with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
           Post => Present (NSW_Sub'Result);

   function NSW_Mul
     (Env : Environ; LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G (NSW_Mul (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ))
      with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
           Post => Present (NSW_Mul'Result);

   function S_Div
     (Env : Environ; LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G (S_Div (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ))
      with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
           Post => Present (S_Div'Result);

   function U_Div
     (Env : Environ; LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G (U_Div (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ))
      with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
           Post => Present (U_Div'Result);

   function S_Rem
     (Env : Environ; LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G (S_Rem (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ))
      with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
           Post => Present (S_Rem'Result);

   function U_Rem
     (Env : Environ; LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G (U_Rem (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ))
      with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
           Post => Present (U_Rem'Result);

   function Build_And
     (Env : Environ; LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G (Build_And (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ))
      with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
           Post => Present (Build_And'Result);

   function Build_Or
     (Env : Environ; LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G (Build_Or (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ))
      with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
           Post => Present (Build_Or'Result);

   function Build_Xor
     (Env : Environ; LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G (Build_Xor (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ))
      with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
           Post => Present (Build_Xor'Result);

   function F_Add
     (Env : Environ; LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G (F_Add (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ))
      with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
           Post => Present (F_Add'Result);

   function F_Sub
     (Env : Environ; LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G (F_Sub (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ))
      with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
           Post => Present (F_Sub'Result);

   function F_Mul
     (Env : Environ; LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G (F_Mul (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ))
      with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
           Post => Present (F_Mul'Result);

   function F_Div
     (Env : Environ; LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      (G (F_Div (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ))
      with Pre  => Env /= null and then Present (LHS) and then Present (RHS),
           Post => Present (F_Div'Result);

   function Shl
     (Env : Environ; E, Count : GL_Value; Name : String := "") return GL_Value
   is
      (G (Shl (Env.Bld, E.Value, Count.Value, Name), E.Typ))
      with Pre  => Env /= null and then Present (E) and then Present (Count),
           Post => Present (Shl'Result);

   function L_Shr
     (Env : Environ; E, Count : GL_Value; Name : String := "") return GL_Value
   is
      (G (L_Shr (Env.Bld, E.Value, Count.Value, Name), E.Typ))
      with Pre  => Env /= null and then Present (E) and then Present (Count),
           Post => Present (L_Shr'Result);

   function A_Shr
     (Env : Environ; E, Count : GL_Value; Name : String := "") return GL_Value
   is
      (G (A_Shr (Env.Bld, E.Value, Count.Value, Name), E.Typ))
      with Pre  => Env /= null and then Present (E) and then Present (Count),
           Post => Present (A_Shr'Result);

   function Build_Not
     (Env : Environ; V : GL_Value; Name : String := "") return GL_Value
   is
      (G (Build_Not (Env.Bld, V.Value, Name), V.Typ))
      with Pre  => Env /= null and then Present (V),
           Post => Present (Build_Not'Result);

   function NSW_Neg
     (Env : Environ; V : GL_Value; Name : String := "") return GL_Value
   is
      (G (NSW_Neg (Env.Bld, V.Value, Name), V.Typ))
      with Pre  => Env /= null and then Present (V),
           Post => Present (NSW_Neg'Result);

   function F_Neg
     (Env : Environ; V : GL_Value; Name : String := "") return GL_Value
   is
      (G (F_Neg (Env.Bld, V.Value, Name), V.Typ))
      with Pre  => Env /= null and then Present (V),
           Post => Present (F_Neg'Result);

   function Build_Select
     (Env : Environ; C_If, C_Then, C_Else : GL_Value; Name : String := "")
     return GL_Value
   is
     (G (Build_Select (Env.Bld, C_If => C_If.Value, C_Then => C_Then.Value,
                     C_Else => C_Else.Value, Name => Name),
         C_Then.Typ, Is_Reference => C_If.Is_Reference))
     with Pre  => Env /= null and then Present (C_If)
                  and then Present (C_Then) and then Present (C_Else),
          Post => Present (Build_Select'Result);

   procedure Build_Cond_Br
     (Env : Environ; C_If : GL_Value; C_Then, C_Else : Basic_Block_T)
     with Pre => Env /= null and then Present (C_If)
                 and then Present (C_Then) and then Present (C_Else);

   function Build_Phi
     (Env       : Environ;
      GL_Values : GL_Value_Array;
      BBs       : Basic_Block_Array;
      Name      : String := "") return GL_Value
     with Pre  => Env /= null and then GL_Values'First = BBs'First
                  and then GL_Values'Last = BBs'Last,
          Post => Present (Build_Phi'Result);

   function Int_To_Ref
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String := "")
     return GL_Value
     with Pre  => Env /= null and then Present (V) and then Is_Type (TE),
          Post => Present (Int_To_Ref'Result)
                  and then Is_Access_Type (Int_To_Ref'Result);
   --  Similar to Int_To_Ptr, but TE is the Designed_Type, not the
   --  access type.

   function Insert_Value
     (Env      : Environ;
      Arg, Elt : GL_Value;
      Index    : unsigned;
      Name     : String := "") return GL_Value
   is
      (G (Insert_Value (Env.Bld, Arg.Value, Elt.Value, Index, Name),
          Arg.Typ, Is_Reference => Arg.Is_Reference))
     with  Pre  => Env /= null and then Present (Arg) and then Present (Elt),
           Post => Present (Insert_Value'Result);

   function GEP
     (Env         : Environ;
      Result_Type : Entity_Id;
      Ptr         : GL_Value;
      Indices     : GL_Value_Array;
      Name        : String := "") return GL_Value
     with Pre  => Env /= null and then Present (Ptr),
          Post => Present (GEP'Result);
   --  Helper for LLVM's Build_GEP

   type Type_Array is array (Nat range <>) of Type_T;

   function UI_To_Long_Long_Integer (U : Uint) return Long_Long_Integer
     with Pre => U /= No_Uint;

   function Return_Needs_Sec_Stack (Arg : Node_Id) return Boolean
     with Pre => Present (Arg);
   --  Returns true if given function needs to return its arg via the secondary
   --  stack

   function Param_Needs_Ptr (Param : Entity_Id) return Boolean
     with Pre => Present (Param);
   --  Returns true if Param needs to be passed by reference (pointer) rather
   --  than by value

   function Get_Uint_Value (Node : Node_Id) return Uint
     with Pre => Present (Node);
   --  If Node has a static Uint value, return it.  Otherwise, return No_Uint.

   function Const_Int (T : Type_T; Value : Uint)
     return Value_T renames Uintp.LLVM.UI_To_LLVM;
   --  Return an LLVM value corresponding to the universal int Value

   type Pred_Mapping is record
      Signed : Int_Predicate_T;
      Unsigned : Int_Predicate_T;
      Real : Real_Predicate_T;
   end record;

   function Get_Preds (Kind : Node_Kind) return Pred_Mapping is
     (case Kind is
        when N_Op_Eq => (Int_EQ, Int_EQ, Real_OEQ),
        when N_Op_Ne => (Int_NE, Int_NE, Real_ONE),
        when N_Op_Lt => (Int_SLT, Int_ULT, Real_OLT),
        when N_Op_Le => (Int_SLE, Int_ULE, Real_OLE),
        when N_Op_Gt => (Int_SGT, Int_UGT, Real_OGT),
        when N_Op_Ge => (Int_SGE, Int_UGE, Real_OGE),
        when others => (others => <>));

   type Entity_Iterator is array (Nat range <>) of Entity_Id;

   function Filter (Unused : Entity_Id) return Boolean is (True);

   generic
      with function Get_First (Root : Entity_Id) return Entity_Id is <>;
      with function Get_Next (Elt : Entity_Id) return Entity_Id is <>;
      with function Filter (Elt : Entity_Id) return Boolean is <>;
   function Iterate_Entities (Root : Entity_Id) return Entity_Iterator;
   --  Likewise for the linked list of entities starting at Get_First (Root)

   function Get_Params (Subp : Entity_Id) return Entity_Iterator;

   function Get_Name (E : Entity_Id) return String is
      (Get_Name_String (Chars (E)))
     with Pre => Present (E);
   --  Return the name of an entity: Get_Name_String (Chars (E))

   function Get_Acting_Spec (Subp_Body : Node_Id) return Node_Id
     with Pre => Present (Subp_Body);
   --  If Subp_Body acts as a spec, return it. Return the corresponding
   --  subprogram declaration otherwise.

   procedure Discard (V : Value_T) with Pre => Present (V);
   procedure Discard (T : Type_T)  with Pre => Present (T);

   procedure Dump_LLVM_Value (V : Value_T);
   --  Simple wrapper around LLVM.Core.Dump_Value. Gives an Ada name to this
   --  function that is usable in debugging sessions.

   procedure Dump_GL_Value (G : GL_Value);
   --  Debug routine to print the LLVM value and GNAT tree node for a GL_Value

   function Is_LValue (Node : Node_Id) return Boolean
     with Pre => Present (Node);
   --  Returns true if Node is an L value

   function Is_Access_Unconstrained (T : Entity_Id) return Boolean is
     (Is_Access_Type (T) and then Is_Array_Type (Designated_Type (T))
      and then not Is_Constrained (Designated_Type (T)))
     with Pre => Is_Type (T);

   function Get_Param_Types (Fn_Ty : Type_T) return Type_Array
     with Pre => Present (Fn_Ty);
   --  Wrapper for equivalent LLVM function, returning a proper type array.
   --  Given a function type or a pointer to function type, returns the types
   --  of the arguments.

   function Img (I : Nat) return String;
   --  Img function for Nat type that doesn't append a space in front of it
   --  (since a Nat is always positive)

   function Get_Ext_Name (E : Entity_Id) return String
     with Pre => Present (E);
   --  Returns a string corresponding to the external name of E

   pragma Annotate (Xcov, Exempt_On, "Debug helpers");
   procedure Dump_LLVM_Module (M : Module_T);
   --  Likewise, for LLVM.Core.Dump_Module

   procedure Dump_LLVM_Type (T : Type_T);
   --  Likewise, for LLVM.Core.Dump_Type

   function LLVM_Type_Of (V : Value_T) return Type_T
   is (Type_Of (V));

   function LLVM_Count_Param_Types (T : Type_T) return Nat
   is (Nat (Count_Param_Types (T)));

   function LLVM_Get_El_Type (T : Type_T) return Type_T
   is (Get_Element_Type (T));

   function LLVM_Size_Of (T_Data : Target_Data_T; Ty : Type_T) return Nat
   is (Nat (Size_Of_Type_In_Bits (T_Data, Ty)));
   pragma Annotate (Xcov, Exempt_Off, "Debug helpers");

end GNATLLVM.Utils;
