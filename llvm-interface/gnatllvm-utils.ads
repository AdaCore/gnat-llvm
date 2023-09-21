------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2023, AdaCore                     --
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

with Einfo.Utils; use Einfo.Utils;
with Nlists;      use Nlists;
with Sem_Util;    use Sem_Util;
with Uintp.LLVM;  use Uintp.LLVM;

with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Types;       use GNATLLVM.Types;

package GNATLLVM.Utils is

   --  Define a mechanism for creating a hash table mapping one Value_T
   --  to another.

   function New_Unique_Id return Unique_Id;
   --  Return a new, unique, id

   function List_Length_Non_Pragma (List : List_Id) return Nat
     with Pre => Present (List);
   --  Like List_Length, but return only those items considered "non-pragma"

   function Num_Actuals (N : N_Subprogram_Call_Id) return Nat;
   --  The number of actual arguments in N, a subprogram call

   function Number_Bounds (TE : Type_Kind_Id) return Nat
     with Pre => Is_Array_Or_Packed_Array_Type (TE);
   --  Usually Number_Dimensions * 2, but may be lower if one or more index
   --  has a fixed lower bound.

   function Simplify_Range (N : N_Is_Index_Id) return N_Has_Bounds_Id;
   --  Given N, which can be an index of an array type, return a node that
   --  has the bounds of N.

   procedure Decode_Range (N : N_Is_Range_Id; Low, High : out Uint);
   --  Decode the right operand of an N_In or N_Not_In or of a Choice in a
   --  case statement into the low and high bounds. If either Low or High
   --  is not Present, we have a nonstatic value, a non-discrete value, or
   --  we can't find the value. This should not happen in switch
   --  statements.

   function Range_Length
     (Low, High : Uint; Max_Length : Int := Int'Last) return Nat;
   --  Given a decoded range, return the length of the range, or Max_Length,
   --  whichever is less.

   function Is_Name (N : N_Subexpr_Id) return Boolean;
   --  Return True if N is an expression that represents a variable or
   --  something else that can be used in an LHS context.

   procedure Set_Linker_Section (V : GL_Value; E : Entity_Id)
     with Pre => Present (V) and then Present (E);
   --  Add a linker section to V if one is specified for E

   procedure Check_Convention (E : Entity_Id)
     with Pre => Present (E);
   --  Validate that we support the Convention on E and give an error if we
   --  don't.

   procedure Process_Pragmas (E : Entity_Id; V : GL_Value)
     with Pre => not Is_Type (E)
                 and then (Is_A_Global_Variable (V)
                             or else Is_A_Function (V));
   --  Process any pragmas for V, whose corresponding tree node is E

   function Enclosing_Subprogram_Scope (E : Entity_Id) return Entity_Id
     with Pre => not Is_Type (E);
   --  Return any enclosing subprogram scope above E

   function Acting_Spec
     (N : N_Subprogram_Body_Id) return N_Subprogram_Specification_Id;
   --  If Subp_Body acts as a spec, return it. Otherwise, return the
   --  corresponding subprogram declaration.

   function Get_Uint_Value (N : N_Subexpr_Id) return Uint;
   --  If Node has a static Uint value, return it. Otherwise, return No_Uint

   function Const_Int (T : Type_T; Value : Uint)
     return Value_T renames UI_To_LLVM;
   --  Return an LLVM value corresponding to the universal int Value

   function Get_Name (E : Entity_Id; Suffix : String := "") return String
     with Pre => Present (E);
   --  Return the name of an entity concatenated with Suffix.

   function Has_Full_Access (N : N_Subexpr_Id) return Boolean;
   --  Return True if N is a node which needs Full_Access

   function Is_VFA_Ref (N : N_Subexpr_Id) return Boolean is
     (Nkind (N) in N_Indexed_Component | N_Selected_Component
        and then Has_Full_Access (Prefix (N)));
   --  Return True if N is an expression that has a Volatile_Full_Access
   --  prefix.

   function Get_Ext_Name (E : Entity_Id; Suffix : String := "") return Name_Id
     with Pre => Present (E), Post => Present (Get_Ext_Name'Result);
   --  Returns a Name_Id corresponding to the external name of E

   function Get_Ext_Name (E : Entity_Id; Suffix : String := "") return String
   is (Get_Name_String (Get_Ext_Name (E, Suffix)))
     with Pre => Present (E);
   --  Returns a string corresponding to the external name of E

   function To_String (J : Nat) return String;
   --  Returns a string corresponding to the image of J with no leading
   --  blanks.

   procedure Error_Msg_NE_Num
     (Msg : String; N : Node_Id; E : Entity_Id; Num : Int)
     with Pre => Msg'Length > 0 and then Present (N) and then Present (E);

   procedure Error_Msg_NE_Num
     (Msg : String; N : Node_Id; E : Entity_Id; Num : Uint)
     with Pre => Msg'Length > 0 and then Present (N) and then Present (E)
                 and then Present (Num);

   function Is_Layout_Identical (T1, T2 : Type_T) return Boolean
     with Pre => Present (T1) and then Present (T2);
   --  Return True iff types T1 and T2 have identical layout.
   --  We can't use the LLVM routine of similar name because we want to
   --  recurse into records: two fields are of types with identical
   --  layout are enough for the fields to be considered the same layout:
   --  we don't actually need identical types.

   function Get_Orig_By_Ref_Mech (E : Formal_Kind_Id) return Boolean is
     (Get_Flag1 (E));
   procedure Set_Orig_By_Ref_Mech (E : Formal_Kind_Id; F : Boolean);
   --  Set and get a flag indicating that this parameter was originally
   --  specified with a Mechanism of By_Ref.

   function Get_Added_To_Module (E : Subprogram_Kind_Id) return Boolean is
     (Get_Flag1 (E));
   procedure Set_Added_To_Module (E : Subprogram_Kind_Id; F : Boolean := True);
   --  Set and get a flag indicating that this subprogram was already added
   --  to the module.

   function Get_Allocated_For_Return
     (E : Constant_Or_Variable_Kind_Id) return Boolean
   is
     (Get_Flag1 (E));
   procedure Set_Allocated_For_Return
     (E : Constant_Or_Variable_Kind_Id; F : Boolean := True);
   --  Set and get a flag indicating that this entity was allocated
   --  specially for Is_Return_Object so the return statement need not
   --  do further allocation.

   function Is_Generic_Item (N : Node_Id) return Boolean is
     (Nkind (N) in N_Subprogram_Body | N_Function_Specification |
                     N_Procedure_Specification | N_Package_Specification |
                     N_Package_Body
        and then Ekind (Unique_Defining_Entity (N)) in Generic_Unit_Kind)
     with Pre => Present (N);
   --  Return True iff N is a node representing a generic package or
   --  subprogram.

   generic
      with procedure Scan (N : Node_Id);
   procedure Scan_Library_Item (U : Node_Id);
   --  Procedure to scan all library units calling the parameter for each

   function
     Globalize_Name (S : String; Is_Global : Boolean := True) return String;
   --  If Is_Global, return a version of S that's known to be globally unique

   pragma Annotate (Xcov, Exempt_On, "Debug helpers");

   procedure Dump_LLVM_Value (V : Value_T)
     with Export, External_Name => "dllv";
   --  Simple wrapper around LLVM.Core.Dump_Value. Gives an Ada name to this
   --  function that is usable in debugging sessions.

   procedure Dump_LLVM_Type (T : Type_T)
     with Export, External_Name => "dllt";
   --  Likewise, for LLVM.Core.Dump_Type

   procedure Dump_LLVM_Module (M : Module_T);
   --  Likewise, for LLVM.Core.Dump_Module

   procedure Dump_LLVM_Metadata (MD : Metadata_T)
     with Export, External_Name => "dllm";

   pragma Annotate (Xcov, Exempt_Off, "Debug helpers");

end GNATLLVM.Utils;
