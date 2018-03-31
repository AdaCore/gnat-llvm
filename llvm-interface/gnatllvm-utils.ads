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

with LLVM.Core; use LLVM.Core;
with LLVM.Types; use LLVM.Types;

with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with Get_Targ;
with LLVM.Target; use LLVM.Target;

with GNATLLVM.Environment; use GNATLLVM.Environment;

package GNATLLVM.Utils is

   type Value_Array is array (Nat range <>) of Value_T;

   procedure Store (Bld : Builder_T; Expr : Value_T; Ptr : Value_T);
   --  Helper for LLVM's Build_Store

   procedure Store_With_Type
     (Env : Environ; TE : Entity_Id; Expr : Value_T; Ptr : Value_T);
   --  Similar, but allows annotating store

   function Load_With_Type
     (Env : Environ; TE : Entity_Id; Ptr : Value_T) return Value_T;
   --  Likewise for a load

   function GEP
     (Bld : Builder_T; Ptr : Value_T; Indices : Value_Array; Name : String)
      return Value_T;
   --  Helper for LLVM's Build_GEP

   function Is_Type_Or_Void (E : Entity_Id) return Boolean is
     (Ekind (E) = E_Void or else Is_Type (E));
   --  We can have Etype's that are E_Void for E_Procedure

   --  It's not sufficient to just pass around an LLVM Value_T when
   --  generating code because there's a lot of information lost about the
   --  value and where it came from.  Contrast with Gigi, where we pass around
   --  a GCC tree node, which already has a lot of information, and which we
   --  further annotate with flags.  So we pass the following record:

   type GL_Value is record
      Value        : Value_T;
      --  The LLVM value that was generated

      Typ          : Entity_Id;
      --  The GNAT type of this value.

      Is_Reference : Boolean;
      --  If True, this is actually a pointer to Typ, so Value's type is
      --  actually an E_Access_Type (not provided) whose Designated_Type
      --  is Typ.
   end record
     with Dynamic_Predicate => (No (GL_Value.Value) and then No (Gl_Value.Typ))
                               or else (Present (GL_Value.Value)
                                          and then Is_Type_Or_Void
                                             (GL_Value.Typ));

   function G
     (V            : Value_T;
      TE           : Entity_Id;
      Is_Reference : Boolean := False) return GL_Value
   is
     ((V, TE, Is_Reference))
     with Pre => Present (V) and then Is_Type_Or_Void (TE);

   No_GL_Value : constant GL_Value := (No_Value_T, Empty, False);

   function No (G : GL_Value) return Boolean      is (G = No_GL_Value);
   function Present (G : GL_Value) return Boolean is (G /= No_GL_Value);

   function Is_Reference (G : GL_Value) return Boolean is (G.Is_Reference);

   --  Now define predicates on this type to easily access properties of
   --  the LLVM value and the effective type.  These have the same names
   --  as those for types and Value_T's.

   function Type_Of (G : GL_Value) return Type_T is
     (Type_Of (G.Value));

   function Is_Access_Type (G : GL_Value) return Boolean is
     (Is_Reference (G) or else Is_Access_Type (G.Typ));

   function Designated_Type (G : GL_Value) return Entity_Id is
     ((if Is_Reference (G) then G.Typ else Designated_Type (G.Typ)))
     with Pre => Is_Access_Type (G), Post => Is_Type (Designated_Type'Result);

   function Is_Dynamic_Size (Env : Environ; G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Dynamic_Size (Env, G.Typ));

   function Is_Array_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Array_Type (G.Typ));

   function Is_Access_Unconstrained (G : GL_Value) return Boolean is
     (Is_Access_Type (G) and then Is_Array_Type (Designated_Type (G))
                         and then not Is_Constrained (Designated_Type (G)));

   function Is_Constrained (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Constrained (G.Typ));

   function Is_Record_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Record_Type (G.Typ));

   function Is_Composite_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Composite_Type (G.Typ));

   function Is_Elementary_Type (G : GL_Value) return Boolean is
     (Is_Reference (G) or else Is_Elementary_Type (G.Typ));

   function Is_Scalar_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Scalar_Type (G.Typ));

   function Is_Discrete_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Discrete_Type (G.Typ));

   function Is_Discrete_Or_Fixed_Point_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Discrete_Or_Fixed_Point_Type (G.Typ));

   function Is_Fixed_Point_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Fixed_Point_Type (G.Typ));

   function Is_Floating_Point_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Floating_Point_Type (G.Typ));

   function Is_Unsigned_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Unsigned_Type (G.Typ));

   function Is_Modular_Integer_Type (G : GL_Value) return Boolean is
     (not Is_Reference (G) and then Is_Modular_Integer_Type (G.Typ));

   function RM_Size (G : GL_Value) return Uint is
     (RM_Size (G.Typ))
     with Pre => not Is_Access_Type (G);

   function Esize (G : GL_Value) return Uint is
     (Esize (G.Typ))
     with Pre => not Is_Access_Type (G);

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
      Sign_Extend : Boolean) return GL_Value
     with Pre  => Env /= null and then Is_Type (TE),
          Post => Present (Const_Int'Result);

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
      Sign_Extend : Boolean) return GL_Value
   is
     (Const_Int (Env, G.Typ, N, Sign_Extend))
     with Pre  => Env /= null and then Present (G),
          Post => Present (Const_Int'Result);

   --  Define IR builder variants which take and/or return GL_Value

   function Alloca
      (Env : Environ; T : Type_T; TE : Entity_Id; Name : String)
      return GL_Value
   is
     (Alloca (Env.Bld, T, Name),
      TE, Is_Reference => True);

   function Int_To_Ptr
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;

   function Ptr_To_Int
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;

   function Bit_Cast
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;

   function Pointer_Cast
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;

   function Pointer_To_Ref
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;

   function Trunc
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;

   function S_Ext
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;

   function Z_Ext
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;

   function FP_Trunc
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;

   function FP_Ext
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;

   function FP_To_SI
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;

   function FP_To_UI
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;

   function UI_To_FP
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;

   function SI_To_FP
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;

   procedure Store
     (Env : Environ; Expr : GL_Value; Ptr : GL_Value)
     with Pre => Is_Access_Type (Ptr);

   function Load (Env : Environ; Ptr : GL_Value) return GL_Value is
     (G (Load_With_Type (Env, Ptr.Typ, Ptr.Value), Designated_Type (Ptr)))
     with Pre => Is_Access_Type (Ptr);

   function I_Cmp
     (Env      : Environ;
      Op       : Int_Predicate_T;
      LHS, RHS : GL_Value;
      Name     : String) return GL_Value
   is
     (G (I_Cmp (Env.Bld, Op, LHS.Value, RHS.Value, Name), Standard_Boolean));

   function F_Cmp
     (Env      : Environ;
      Op       : Real_Predicate_T;
      LHS, RHS : GL_Value;
      Name     : String) return GL_Value
   is
     (G (F_Cmp (Env.Bld, Op, LHS.Value, RHS.Value, Name), Standard_Boolean));

   function NSW_Add
     (Env : Environ; LHS, RHS : GL_Value; Name : String) return GL_Value
   is
      ((NSW_Add (Env.Bld, LHS.Value, RHS.Value, Name),
        LHS.Typ, LHS.Is_Reference or RHS.Is_Reference));

   function NSW_Sub
     (Env : Environ; LHS, RHS : GL_Value; Name : String) return GL_Value
   is
      ((NSW_Sub (Env.Bld, LHS.Value, RHS.Value, Name),
        LHS.Typ, LHS.Is_Reference or RHS.Is_Reference));

   function NSW_Mul
     (Env : Environ; LHS, RHS : GL_Value; Name : String) return GL_Value
   is
      (G (NSW_Mul (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ));

   function S_Div
     (Env : Environ; LHS, RHS : GL_Value; Name : String) return GL_Value
   is
      (G (S_Div (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ));

   function U_Div
     (Env : Environ; LHS, RHS : GL_Value; Name : String) return GL_Value
   is
      (G (U_Div (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ));

   function S_Rem
     (Env : Environ; LHS, RHS : GL_Value; Name : String) return GL_Value
   is
      (G (S_Rem (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ));

   function U_Rem
     (Env : Environ; LHS, RHS : GL_Value; Name : String) return GL_Value
   is
      (G (U_Rem (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ));

   function Build_And
     (Env : Environ; LHS, RHS : GL_Value; Name : String) return GL_Value
   is
      (G (Build_And (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ));

   function Build_Or
     (Env : Environ; LHS, RHS : GL_Value; Name : String) return GL_Value
   is
      (G (Build_Or (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ));

   function Build_Xor
     (Env : Environ; LHS, RHS : GL_Value; Name : String) return GL_Value
   is
      (G (Build_Xor (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ));

   function F_Add
     (Env : Environ; LHS, RHS : GL_Value; Name : String) return GL_Value
   is
      (G (F_Add (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ));

   function F_Sub
     (Env : Environ; LHS, RHS : GL_Value; Name : String) return GL_Value
   is
      (G (F_Sub (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ));

   function F_Mul
     (Env : Environ; LHS, RHS : GL_Value; Name : String) return GL_Value
   is
      (G (F_Mul (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ));

   function F_Div
     (Env : Environ; LHS, RHS : GL_Value; Name : String) return GL_Value
   is
      (G (F_Div (Env.Bld, LHS.Value, RHS.Value, Name), LHS.Typ));

   function Build_Not
     (Env : Environ; V : GL_Value; Name : String) return GL_Value
   is
      (G (Build_Not (Env.Bld, V.Value, Name), V.Typ));

   function NSW_Neg
     (Env : Environ; V : GL_Value; Name : String) return GL_Value
   is
      (G (NSW_Neg (Env.Bld, V.Value, Name), V.Typ));

   function F_Neg
     (Env : Environ; V : GL_Value; Name : String) return GL_Value
   is
      (G (F_Neg (Env.Bld, V.Value, Name), V.Typ));

   function Build_Select
     (Env : Environ; C_If, C_Then, C_Else : GL_Value; Name : String)
     return GL_Value
   is
     ((Build_Select (Env.Bld, C_If => C_If.Value, C_Then => C_Then.Value,
                     C_Else => C_Else.Value, Name => Name),
       C_Then.Typ, C_If.Is_Reference));

   function Int_To_Ref
     (Env : Environ; V : GL_Value; TE : Entity_Id; Name : String)
     return GL_Value;
   --  Similar to Int_To_Ptr, but TE is the Designed_Type, not the
   --  access type.

   type Type_Array is array (Nat range <>) of Type_T;

   function UI_To_Long_Long_Integer (U : Uint) return Long_Long_Integer;

   function Return_Needs_Sec_Stack (Arg : Node_Id) return Boolean;
   --  Returns true if given function needs to return its arg via the secondary
   --  stack

   function Param_Needs_Ptr
     (Param : Entity_Id) return Boolean;
   --  Returns true if Param needs to be passed by reference (pointer) rather
   --  than by value

   function Get_Uint_Value (Node : Node_Id) return Uint
     with Pre => Present (Node);
   --  If Node has a static Uint value, return it.  Otherwise, return No_Uint.

   function Const_Int (T : Type_T; Value : Uint)
     return Value_T renames Uintp.LLVM.UI_To_LLVM;
   --  Return an LLVM value corresponding to the universal int Value

   function Const_Ones (T : Type_T) return Value_T is
     (Const_Int (T, unsigned_long_long'Last, Sign_Extend => True));
   --  Return an LLVM value for the given type where all bits are set

   Intptr_T : constant Type_T :=
     Int_Type (Interfaces.C.unsigned (Get_Targ.Get_Pointer_Size));
   --  Return a LLVM integer type that is as big as pointers

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
      (Get_Name_String (Chars (E)));
   --  Return the name of an entity: Get_Name_String (Chars (E))

   function Get_Acting_Spec (Subp_Body : Node_Id) return Node_Id;
   --  If Subp_Body acts as a spec, return it. Return the corresponding
   --  subprogram declaration otherwise.

   procedure Discard (V : Value_T);
   procedure Discard (T : Type_T);

   procedure Dump_LLVM_Value (V : Value_T);
   --  Simple wrapper around LLVM.Core.Dump_Value. Gives an Ada name to this
   --  function that is usable in debugging sessions.

   function Is_LValue (Node : Node_Id) return Boolean;
   --  Returns true if Node is an L value

   function Is_Access_Unconstrained (T : Entity_Id) return Boolean is
     (Is_Access_Type (T) and then Is_Array_Type (Designated_Type (T))
      and then not Is_Constrained (Designated_Type (T)))
     with Pre => Is_Type (T);

   function Get_Param_Types (Fn_Ty : Type_T) return Type_Array;
   --  Wrapper for equivalent LLVM function, returning a proper type array.
   --  Given a function type or a pointer to function type, returns the types
   --  of the arguments.

   function Img (I : Nat) return String;
   --  Img function for Nat type that doesn't append a space in front of it
   --  (since a Nat is always positive)

   function Get_Subprog_Ext_Name (E : Entity_Id) return String;
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

   type Basic_Block_Array is array (Nat range <>) of Basic_Block_T;

end GNATLLVM.Utils;
