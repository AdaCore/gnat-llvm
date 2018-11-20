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

with Nlists;     use Nlists;
with Sinfo;      use Sinfo;
with Uintp;      use Uintp;
with Uintp.LLVM; use Uintp.LLVM;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.GLValue; use GNATLLVM.GLValue;

package GNATLLVM.Utils is

   --  A type used to save a position at some earlier point in emitting
   --  code so that we can go back to it to emit more code.  Instr can
   --  be empty, in which case we mean to insert at the beginning of
   --  the basic block.

   type Position_T is record
      BB    : Basic_Block_T;
      Instr : Value_T;
   end record;

   No_Position_T : Position_T := (No_BB_T, No_Value_T);

   function No      (P : Position_T) return Boolean is (P =  No_Position_T);
   function Present (P : Position_T) return Boolean is (P /= No_Position_T);

   function  Get_Current_Position return Position_T;
   procedure Set_Current_Position (P : Position_T);

   function List_Length_Non_Pragma (List : List_Id) return Nat
     with Pre => Present (List);
   --  Like List_Length, but return only those items considered "non-pragma"

   procedure Decode_Range (N : Node_Id; Low, High : out Uint)
     with Pre => Present (N);
   --  Decode the right operand of an N_In or N_Not_In or of a Choice in
   --  a case statement into the low and high bounds.  If either Low or High
   --  is No_Uint, it means that we have a nonstatic value, a non-discrete
   --  value, or we can't find the value.  This should not happen in switch
   --  statements.

   function Range_Length
     (Low, High : Uint; Max_Length : Int := Int'Last) return Nat;
   --  Given a decoded range, return the length of the range, or Max_Length,
   --  whichever is less.

   function Are_In_Dead_Code return Boolean;
   --  True if we're in dead code (the last instruction is a terminator)

   procedure Position_Builder_At_End (BB : Basic_Block_T)
     with Pre => Present (BB);

   procedure Build_Br (BB : Basic_Block_T)
     with Pre => Present (BB);

   procedure Maybe_Build_Br (BB : Basic_Block_T);
   --  Like Build_Br, but do nothing if No (BB) or if we're in dead code

   procedure Move_To_BB (BB : Basic_Block_T);
   --  If BB is Present, generate a branch to it and position there

   function Is_Name (N : Node_Id) return Boolean
     with Pre => Present (N);
   --  Return True if N is an expression that represents a variable or
   --  something else that can be used in an LHS context.

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
     (Const_Int (T, ULL'Last, Sign_Extend => True))
     with Pre => Present (T), Post => Present (Const_Ones'Result);
   --  Return an LLVM value for the given type where all bits are set

   type Type_Array is array (Nat range <>) of Type_T;

   function Get_Uint_Value (N : Node_Id) return Uint
     with Pre => Present (N);
   --  If Node has a static Uint value, return it.  Otherwise, return No_Uint

   function Const_Int (T : Type_T; Value : Uint)
     return Value_T renames UI_To_LLVM;
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

   function Get_Name (E : Entity_Id) return String is
      (Get_Name_String (Chars (E)))
     with Pre => Present (E);
   --  Return the name of an entity: Get_Name_String (Chars (E))

   function Get_Acting_Spec (Subp_Body : Node_Id) return Node_Id
     with Pre => Present (Subp_Body);
   --  If Subp_Body acts as a spec, return it. Return the corresponding
   --  subprogram declaration otherwise.

   procedure Discard (V  : Value_T)        with Pre => Present (V);
   procedure Discard (T  : Type_T)         with Pre => Present (T);
   procedure Discard (BB : Basic_Block_T)  with Pre => Present (BB);

   function Get_Ext_Name (E : Entity_Id) return String
     with Pre => Present (E);
   --  Returns a string corresponding to the external name of E

   pragma Annotate (Xcov, Exempt_On, "Debug helpers");
   procedure Dump_LLVM_Value (V : Value_T)
     with Export => True, External_Name => "dllv";
   --  Simple wrapper around LLVM.Core.Dump_Value. Gives an Ada name to this
   --  function that is usable in debugging sessions.

   procedure Dump_GL_Value (V : GL_Value)
     with Export => True, External_Name => "dglv";

   --  Debug routine to print the LLVM value and GNAT tree node for a GL_Value

   procedure Dump_LLVM_Type (T : Type_T)
     with Export => True, External_Name => "dllt";
   --  Likewise, for LLVM.Core.Dump_Type

   procedure Dump_LLVM_Module (M : Module_T);
   --  Likewise, for LLVM.Core.Dump_Module

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
