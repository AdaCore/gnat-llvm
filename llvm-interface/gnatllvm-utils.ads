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
     with Pre => Is_Type_Or_Void (E),
          Post => Is_Type_Or_Void (Get_Fullest_View'Result);
   --  Get the fullest possible view of E, looking through private,
   --  limited, and packed array implementation types.

   function Full_Etype (N : Node_Id) return Entity_Id is
     (if Ekind (Etype (N)) = E_Void then Etype (N)
      else Get_Fullest_View (Etype (N)));

   function Full_Component_Type (E : Entity_Id) return Entity_Id is
     (Get_Fullest_View (Component_Type (E)))
     with Pre  => Is_Array_Type (E),
          Post => Present (Full_Component_Type'Result);

   function Full_Designated_Type (E : Entity_Id) return Entity_Id is
     (Get_Fullest_View (Designated_Type (E)))
     with Pre  => Is_Access_Type (E),
          Post => Present (Full_Designated_Type'Result);

   function Are_In_Dead_Code return Boolean;
   --  True if we're in dead code (the last instruction is a terminator)

   procedure Position_Builder_At_End (BB : Basic_Block_T)
     with Pre => Present (BB);

   procedure Build_Br (BB : Basic_Block_T)
     with Pre => Present (BB);

   procedure Store (Bld : Builder_T; Expr : Value_T; Ptr : Value_T)
     with Pre => Present (Bld) and then Present (Expr) and then Present (Ptr);
   --  Helper for LLVM's Build_Store

   procedure Store_With_Type (TE : Entity_Id; Expr : Value_T; Ptr : Value_T)
     with Pre => Is_Type (TE)
                 and then Present (Expr) and then Present (Ptr);
   --  Similar, but allows annotating store

   function Load_With_Type (TE : Entity_Id; Ptr : Value_T; Name : String := "")
     return Value_T
     with Pre  => Is_Type (TE) and then Present (Ptr),
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

   type Type_Array is array (Nat range <>) of Type_T;

   function UI_To_Long_Long_Integer (U : Uint) return Long_Long_Integer
     with Pre => U /= No_Uint;

   function Return_Needs_Sec_Stack (Arg : Node_Id) return Boolean
     with Pre => Present (Arg);
   --  Returns true if given function needs to return its arg via the secondary
   --  stack.

   function Param_Needs_Ptr (Param : Entity_Id) return Boolean
     with Pre => Present (Param);
   --  Returns true if Param needs to be passed by reference (pointer) rather
   --  than by value.

   function Get_Uint_Value (Node : Node_Id) return Uint
     with Pre => Present (Node);
   --  If Node has a static Uint value, return it.  Otherwise, return No_Uint

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

   function Is_Access_Unconstrained (T : Entity_Id) return Boolean is
     (Is_Access_Type (T) and then Is_Array_Type (Full_Designated_Type (T))
      and then not Is_Constrained (Full_Designated_Type (T)))
     with Pre => Is_Type (T);

   function Get_Param_Types (Fn_Ty : Type_T) return Type_Array
     with Pre => Present (Fn_Ty);
   --  Wrapper for equivalent LLVM function, returning a proper type array.
   --  Given a function type or a pointer to function type, returns the types
   --  of the arguments.

   function Img (I : Nat) return String;
   --  Img function for Nat type that doesn't append a space in front of it
   --  (since a Nat is always positive).

   function Get_Ext_Name (E : Entity_Id) return String
     with Pre => Present (E);
   --  Returns a string corresponding to the external name of E

   pragma Annotate (Xcov, Exempt_On, "Debug helpers");
   procedure Dump_LLVM_Module (M : Module_T);
   --  Likewise, for LLVM.Core.Dump_Module

   procedure Dump_LLVM_Type (T : Type_T);
   --  Likewise, for LLVM.Core.Dump_Type

   function LLVM_Type_Of (V : Value_T) return Type_T is
     (Type_Of (V));

   function LLVM_Count_Param_Types (T : Type_T) return Nat is
     (Nat (Count_Param_Types (T)));

   function LLVM_Get_El_Type (T : Type_T) return Type_T is
     (Get_Element_Type (T));

   function LLVM_Size_Of (T_Data : Target_Data_T; Ty : Type_T) return Nat is
     (Nat (Size_Of_Type_In_Bits (T_Data, Ty)));

   pragma Annotate (Xcov, Exempt_Off, "Debug helpers");

end GNATLLVM.Utils;
