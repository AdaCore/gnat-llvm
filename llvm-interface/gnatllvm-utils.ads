------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Conversion;

with Nlists;     use Nlists;
with Sem_Util;   use Sem_Util;
with Sinfo;      use Sinfo;
with Uintp.LLVM; use Uintp.LLVM;

with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;

package GNATLLVM.Utils is

   --  Define a mechanism for creating a hash table mapping one Value_T
   --  to another.

   function Hash_Value_T (Val : Value_T) return Hash_Type;
   --  Convert a Value_T to a hash

   package Value_Value_Map_P is new Ada.Containers.Hashed_Maps
     (Key_Type        => Value_T,
      Element_Type    => Value_T,
      Hash            => Hash_Value_T,
      Equivalent_Keys => "=");

   function List_Length_Non_Pragma (List : List_Id) return Nat
     with Pre => Present (List);
   --  Like List_Length, but return only those items considered "non-pragma"

   procedure Decode_Range (N : Node_Id; Low, High : out Uint)
     with Pre => Present (N);
   --  Decode the right operand of an N_In or N_Not_In or of a Choice in a
   --  case statement into the low and high bounds.  If either Low or High
   --  is not Present, we have a nonstatic value, a non-discrete value, or
   --  we can't find the value.  This should not happen in switch
   --  statements.

   function Range_Length
     (Low, High : Uint; Max_Length : Int := Int'Last) return Nat;
   --  Given a decoded range, return the length of the range, or Max_Length,
   --  whichever is less.

   function Is_Name (N : Node_Id) return Boolean
     with Pre => Present (N);
   --  Return True if N is an expression that represents a variable or
   --  something else that can be used in an LHS context.

   procedure Set_Linker_Section (V : GL_Value; Def_Ident : Entity_Id)
     with Pre => Present (V) and then Present (Def_Ident);
   --  Add a linker section to V if one is specified for Def_Ident

   procedure Check_Convention (Def_Ident : Entity_Id)
     with Pre => Present (Def_Ident);
   --  Validate that we support the Convention on Def_Ident and give an error
   --  if we don't.

   procedure Process_Pragmas (Def_Ident : Entity_Id; V : GL_Value)
     with Pre => not Is_Type (Def_Ident)
                 and then (Is_A_Global_Variable (V)
                             or else Is_A_Function (V));
   --  Process any pragmas for V, whose corresponding tree node is Def_Ident.

   function Enclosing_Subprogram_Scope (Def_Ident : Entity_Id) return Entity_Id
     with Pre => not Is_Type (Def_Ident);
   --  Return any enclosing subprogram scope above Def_Ident

   function Get_Uint_Value (N : Node_Id) return Uint
     with Pre => Present (N);
   --  If Node has a static Uint value, return it.  Otherwise, return No_Uint

   function Const_Int (T : Type_T; Value : Uint)
     return Value_T renames UI_To_LLVM;
   --  Return an LLVM value corresponding to the universal int Value

   function Get_Name (E : Entity_Id; Suffix : String := "") return String
     with Pre => Present (E);
   --  Return the name of an entity concatenated with Suffix.

   function Get_Acting_Spec (Subp_Body : Node_Id) return Node_Id
     with Pre => Present (Subp_Body);
   --  If Subp_Body acts as a spec, return it. Return the corresponding
   --  subprogram declaration otherwise.

   function Has_Volatile_Full_Access (N : Node_Id) return Boolean
     with Pre => Present (N);
   --  Return True if N is a node which needs Volatile_Full_Access

   function Is_VFA_Ref (N : Node_Id) return Boolean is
     (Nkind_In (N, N_Indexed_Component, N_Selected_Component)
        and then Has_Volatile_Full_Access (Prefix (N)))
     with Pre => Present (N);
   --  Return True if N is an expression that has a Volatile_Full_Access
   --  prefix.

   procedure Discard (V  : Value_T)        with Pre => Present (V);
   procedure Discard (T  : Type_T)         with Pre => Present (T);
   procedure Discard (BB : Basic_Block_T)  with Pre => Present (BB);
   procedure Discard (B  : Boolean);

   function Get_Ext_Name (E : Entity_Id) return String
     with Pre => Present (E);
   --  Returns a string corresponding to the external name of E

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

   function Get_Orig_By_Ref_Mech (E : Entity_Id) return Boolean is
     (Get_Flag1 (E))
     with Pre => Ekind_In (E, E_In_Parameter, E_Out_Parameter,
                             E_In_Out_Parameter);
   procedure Set_Orig_By_Ref_Mech (E : Entity_Id; F : Boolean)
     with Pre => Ekind_In (E, E_In_Parameter, E_Out_Parameter,
                             E_In_Out_Parameter);
   --  Set and get a flag indicating that this parameter was originally
   --  specified with a Mechanism of By_Ref.

   function Get_Added_To_Module (E : Entity_Id) return Boolean is
     (Get_Flag1 (E))
     with Pre => Ekind_In (E, E_Function, E_Procedure);
   procedure Set_Added_To_Module (E : Entity_Id; F : Boolean := True)
     with Pre => Ekind_In (E, E_Function, E_Procedure);
   --  Set and get a flag indicating that this subprogram was already added
   --  to the module.

   function Is_Generic_Item (N : Node_Id) return Boolean is
     (Nkind_In (N, N_Subprogram_Body, N_Function_Specification,
                   N_Procedure_Specification, N_Package_Specification,
                   N_Package_Body)
        and then Ekind (Unique_Defining_Entity (N)) in Generic_Unit_Kind)
     with Pre => Present (N);
   --  Return True iff N is a node representing a generic package or
   --  subprogram.

   generic
      with procedure Scan (N : Node_Id);
   procedure Scan_Library_Item (U : Node_Id);
   --  Procedure to scan all library units calling the parameter for each

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

end GNATLLVM.Utils;
