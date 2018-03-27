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
with Namet; use Namet;
with Sinfo; use Sinfo;
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

   function Const_Int (T : Type_T; Value : Uintp.Uint)
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

   type Value_Array is array (Nat range <>) of Value_T;
   type Basic_Block_Array is array (Nat range <>) of Basic_Block_T;

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

end GNATLLVM.Utils;
