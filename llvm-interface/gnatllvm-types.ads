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

with Ada.Unchecked_Conversion;

with Sinfo; use Sinfo;
with Uintp; use Uintp;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package GNATLLVM.Types is

   --  Our strategy for handling types is very different from what's done by
   --  Gigi in the GCC implementation of GNAT.  Here, we never pass a type
   --  to any function that expects a type unless that type is the fullest
   --  possible view.
   --
   --  We also ignore freeze nodes for types.  We can safely do this
   --  because we don't actually do anything with an expression used as a
   --  bound of the type other than putting the tree node for that
   --  expression into a data structure.  The expression itself isn't
   --  evaluated until an object of the type is referenced (or some other
   --  operation that freezes the type).  We can safely do this because the
   --  front end has removed all side effects from type bounds and other
   --  constraints.
   --
   --  Finally, we always use a recursive algorithm to define types,
   --  whether they're external or internal.  This means we have to "break
   --  the loop" when there's a reference back to a type (such as a record
   --  or array with an access type to itself).  We do this by looking at
   --  whether the designated type of an access type depends on something
   --  that's being elaborated and, if so, make a dummy access type of the
   --  same width and structure.  For a designated type that's a record,
   --  that dummy type will actually be the type of the record.  For scalar
   --  types, we can fully elaborate the type and access to function types
   --  always use void pointers, so don't depend on the designated type.
   --
   --  This means the only cases where we need a dummy type are the less
   --  common cases of an array and access type.  We've also arranged
   --  things so that the only place this dummy can show up is as the
   --  component of a record, the component type of an array, or the
   --  designated type of an access type.  So whenever we get one of those,
   --  we check for the need to convert to the actual access type and not
   --  the dummy one.

   Max_Load_Size : constant := 8;
   --  LLVM supports loading and storing of arbitrarily-large values, but
   --  code generation and optimization is very slow if the value's size is
   --  too large.  We pick an arbitary constant here to cut it off.  This
   --  constant is in number of loads or stores, meaning the maximum value
   --  of the size divided by the alignment.  ??? Perhaps we should make
   --  this a command-line operand.

   function Is_Dynamic_Size
     (TE : Entity_Id; Max_Size : Boolean := False) return Boolean
     with Pre => Is_Type (TE);
   --  Returns True if TE's size is not known at compile-time.  Max_Size
   --  is True if we're to consider the maximum size of TE.

   function Create_Access_Type_To (TE : Entity_Id) return Type_T is
     (Type_For_Relationship (TE, Relationship_For_Ref (TE)))
     with Pre  => Is_Type (TE),
          Post => Present (Create_Access_Type_To'Result);
   --  Function that creates the access type for a corresponding type. Since
   --  access types are not just pointers, this is the abstraction bridge
   --  between the two.  Note that we need not have a GNAT type corresponding
   --  to this access type, which makes this different than calling
   --  Create_Type on an access to TE.

   function Create_Type (TE : Entity_Id) return Type_T
     with Pre  => Present (TE) and then TE = Get_Fullest_View (TE),
          Post => Present (Create_Type'Result);
   --  Given a GNAT type TE, return the corresponding LLVM type, building
   --  it first if necessary.

   function Create_Type_For_Component (TE : Entity_Id) return Type_T
     with Pre  => Present (TE) and then TE = Get_Fullest_View (TE),
     Post => Present (Create_Type_For_Component'Result);
   --  Similarly, but if TE is an unconstrained record, the maximum
   --  size of TE is fixed but it's not representable as a native LLVM
   --  type, return a type (an array) to correspond to that size.

   function Create_Dummy_Access_Type (TE : Entity_Id) return Type_T
     with Pre  => Is_Access_Type (TE),
          Post => Present (Create_Dummy_Access_Type'Result);
   --  Make a type to be used as a dummy type for access type TE

   function Create_TBAA (TE : Entity_Id) return Metadata_T
     with Pre => Is_Type (TE);

   procedure Bounds_From_Type (TE : Entity_Id; Low, High : out GL_Value)
     with Pre  => Ekind (TE) in Discrete_Kind,
          Post => Present (Low) and then Present (High);

   procedure Push_LValue_List;
   procedure Pop_LValue_List;
   --  Push and pop the active range of the LValue pair list

   procedure Clear_LValue_List;
   --  Remove all entries previously added to the LValue list

   procedure Add_To_LValue_List (V : GL_Value)
     with Pre => Present (V);
   --  Add V to the list that's searched by Get_Matching_Value

   function Add_To_LValue_List (V : GL_Value) return GL_Value
     with Pre => Present (V), Post => Add_To_LValue_List'Result = V;
   --  Likewise, but return V

   function Get_Matching_Value (TE : Entity_Id) return GL_Value
     with Pre  => Is_Type (TE),
          Post => Present (Get_Matching_Value'Result);
   --  Find a value that's being computed by the current Emit_LValue
   --  recursion that has the same base type as T.

   function Int_Ty (Num_Bits : Nat) return Type_T is
     (Int_Type (unsigned (Num_Bits)))
     with Post => Get_Type_Kind (Int_Ty'Result) = Integer_Type_Kind;

   function Int_Ty (Num_Bits : Uint) return Type_T is
     (Int_Type (unsigned (UI_To_Int (Num_Bits))))
     with Post => Get_Type_Kind (Int_Ty'Result) = Integer_Type_Kind;

   function Fn_Ty
     (Param_Ty : Type_Array;
      Ret_Ty   : Type_T;
      Varargs  : Boolean := False) return Type_T is
     (Function_Type
        (Ret_Ty, Param_Ty'Address, Param_Ty'Length, Varargs))
     with Pre  => Present (Ret_Ty),
          Post => Get_Type_Kind (Fn_Ty'Result) = Function_Type_Kind;

   function Build_Struct_Type
     (Types : Type_Array; Packed : Boolean := False) return Type_T
     with Post => Present (Build_Struct_Type'Result);
   --  Build an LLVM struct type containing the specified types

   function Get_Fullest_View
     (TE : Entity_Id; Include_PAT : Boolean := True) return Entity_Id
     with Pre => Is_Type_Or_Void (TE),
          Post => Is_Type_Or_Void (Get_Fullest_View'Result);
   --  Get the fullest possible view of E, looking through private,
   --  limited, packed array and other implementation types.  If Include_PAT
   --  is True, don't look inside packed array types.

   function Full_Base_Type
     (TE : Entity_Id; For_Orig : Boolean := False) return Entity_Id
   is
     (Get_Fullest_View (Implementation_Base_Type (TE), not For_Orig))
     with Pre  => Is_Type (TE),
          Post => Present (Full_Base_Type'Result);

   function Ultimate_Base_Type (TE : Entity_Id) return Entity_Id
     with Pre => Is_Type (TE), Post => Is_Type (Ultimate_Base_Type'Result);
   --  Go up TE's Etype chain until it points to itself, which will
   --  go up both base and parent types.

   function Full_Etype (N : Node_Id) return Entity_Id is
     (if Ekind (Etype (N)) = E_Void then Etype (N)
      else Get_Fullest_View (Etype (N)))
     with Pre => Present (N), Post => Is_Type_Or_Void (Full_Etype'Result);

   function Full_Entity (N : Node_Id) return Entity_Id is
     (Get_Fullest_View (Entity (N)))
     with Pre => Present (N), Post => Is_Type (Full_Entity'Result);

   function Full_Component_Type (TE : Entity_Id) return Entity_Id is
     (Get_Fullest_View (Component_Type (TE)))
     with Pre  => Is_Array_Type (TE),
          Post => Present (Full_Component_Type'Result);

   function Full_Original_Array_Type (TE : Entity_Id) return Entity_Id is
     (Get_Fullest_View (Original_Array_Type (TE), Include_PAT => False))
     with Pre  => Is_Packed_Array_Impl_Type (TE),
          Post => Is_Array_Type (Full_Original_Array_Type'Result);

   function Full_Designated_Type (TE : Entity_Id) return Entity_Id is
     (Get_Fullest_View (Designated_Type (TE)))
     with Pre  => Is_Access_Type (TE),
          Post => Present (Full_Designated_Type'Result);

   function Full_Scope (E : Entity_Id) return Entity_Id is
     (Get_Fullest_View (Scope (E)))
     with Pre => Present (E), Post => Present (Full_Scope'Result);

   function Full_Parent_Subtype (TE : Entity_Id) return Entity_Id is
     (Get_Fullest_View (Parent_Subtype (TE)))
     with Pre  => Is_Record_Type (TE),
          Post => Is_Record_Type (Full_Parent_Subtype'Result);

   function Is_Unconstrained_Record (TE : Entity_Id) return Boolean is
     (Ekind (TE) = E_Record_Type and then Has_Discriminants (TE))
     with Pre => Is_Type_Or_Void (TE);

   function Is_Unconstrained_Array (TE : Entity_Id) return Boolean is
     (Is_Array_Type (TE) and then not Is_Constrained (TE))
     with Pre => Is_Type_Or_Void (TE);

   function Is_Access_Unconstrained_Array (TE : Entity_Id) return Boolean is
     (Is_Access_Type (TE)
        and then Is_Unconstrained_Array (Full_Designated_Type (TE)))
     with Pre => Is_Type (TE);

   function Is_Unconstrained_Type (TE : Entity_Id) return Boolean is
     (Is_Unconstrained_Array (TE) or else Is_Unconstrained_Record (TE))
     with Pre => Is_Type_Or_Void (TE);

   function Is_Bit_Packed_Array_Impl_Type (TE : Entity_Id) return Boolean is
     (Is_Packed_Array_Impl_Type (TE)
        and then Is_Bit_Packed_Array (Original_Array_Type (TE)))
     with Pre => Is_Type (TE);

   function Is_Array_Or_Packed_Array_Type (TE : Entity_Id) return Boolean is
     (Is_Array_Type (TE) or else Is_Packed_Array_Impl_Type (TE))
     with Pre => Is_Type (TE);

   function Type_Needs_Bounds (TE : Entity_Id) return Boolean is
     ((Is_Constr_Subt_For_UN_Aliased (TE) and then Is_Array_Type (TE))
      or else (Is_Packed_Array_Impl_Type (TE)
                 and then Type_Needs_Bounds (Original_Array_Type (TE))))
     with Pre => Is_Type (TE);
   --  True is TE is a type that needs bounds stored with data

   function Convert
     (V              : GL_Value;
      TE             : Entity_Id;
      Float_Truncate : Boolean := False) return GL_Value
     with Pre  => Is_Data (V) and then Is_Elementary_Type (TE)
                  and then Is_Elementary_Type (V),
          Post => Is_Data (Convert'Result)
                  and then Is_Elementary_Type (Convert'Result);
   --  Convert Expr to the type TE, with both the types of Expr and TE
   --  being elementary.

   function Convert
     (V, T : GL_Value; Float_Truncate : Boolean := False) return GL_Value is
     (Convert (V, Full_Etype (T), Float_Truncate))
     with Pre  => Is_Data (V) and then Is_Elementary_Type (V)
                  and then Is_Elementary_Type (T),
          Post => Is_Data (Convert'Result)
                  and then Is_Elementary_Type (Convert'Result);
   --  Variant of above where the type is that of another value (T)

   function Convert_Ref (V : GL_Value; TE : Entity_Id) return GL_Value
     with Pre  => Is_Reference (V) and then Is_Type (TE),
          Post => Is_Reference (Convert_Ref'Result);
   --  Convert Src, which should be a reference, into a reference to TE

   function Convert_Ref
     (V : GL_Value; T : GL_Value) return GL_Value is
     (Convert_Ref (V, Full_Etype (T)))
     with Pre  => Present (V) and then Present (T),
          Post => Is_Access_Type (Convert_Ref'Result);
   --  Likewise, but get type from V

   function Convert_To_Access (V : GL_Value; TE : Entity_Id) return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Is_Access_Type (Convert_To_Access'Result);
   --  Convert Src, which should be an access or reference, into an access
   --  type TE

   function Convert_To_Access
     (V : GL_Value; T : GL_Value) return GL_Value is
     (Convert_To_Access (V, Full_Etype (T)))
     with Pre  => Present (V) and then Present (T),
          Post => Is_Access_Type (Convert_To_Access'Result);
   --  Likewise, but get type from V

   function Are_Arrays_With_Different_Index_Types
     (T1, T2 : Entity_Id) return Boolean
     with Pre => Is_Unconstrained_Array (T1) and then Is_Array_Type (T2);
   --  Return True iff T1 and T2 are array types that have at least
   --  one index for whose LLVM types are different.  T1 must be unconstrained.

   function Emit_Conversion
     (N                   : Node_Id;
      TE                  : Entity_Id;
      From_N              : Node_Id := Empty;
      For_LHS             : Boolean := False;
      Is_Unchecked        : Boolean := False;
      Need_Overflow_Check : Boolean := False;
      Float_Truncate      : Boolean := False) return GL_Value
     with Pre  => Is_Type (TE) and then Present (N)
                  and then TE = Get_Fullest_View (TE)
                  and then not (Is_Unchecked and Need_Overflow_Check),
          Post => Present (Emit_Conversion'Result);
   --  Emit code to convert Expr to Dest_Type, optionally in unchecked mode
   --  and optionally with an overflow check.  From_N is the conversion node,
   --  if there is a corresponding source node.

   function Emit_Convert_Value (N : Node_Id; TE : Entity_Id) return GL_Value is
     (Get (Emit_Conversion (N, TE), Object))
     with Pre  => Is_Type (TE) and then Present (N)
                  and then TE = Get_Fullest_View (TE),
          Post => Present (Emit_Convert_Value'Result);
   --  Emit code to convert Expr to Dest_Type and get it as a value

   function Convert_Pointer (V : GL_Value; TE : Entity_Id) return GL_Value
     with Pre  => Is_Access_Type (V),
          Post => Is_Access_Type (Convert_Pointer'Result);
   --  V is a reference to some object.  Convert it to a reference to TE
   --  with the same relationship.

   function Convert_Pointer_To_Dummy (V : GL_Value) return GL_Value
     with Pre  => Is_Access_Type (V);
   --  Likewise, but convert to the dummy form of V's type.

   function Normalize_LValue_Reference (V : GL_Value) return GL_Value
     with Pre  => Present (V),
          Post => Present (Normalize_LValue_Reference'Result);
   --  There are times when the component of an object or a dereference may
   --  have a different LLVM type than the proper type for the type.
   --  Check here for that situation and convert the LValue if required.

   function Strip_Complex_Conversions (N : Node_Id) return Node_Id;
   --  Remove any conversion from N, if Present, if they are record or array
   --  conversions that increase the complexity of the size of the
   --  type because the caller will be doing any needed conversions.

   function Strip_Conversions (N : Node_Id) return Node_Id;
   --  Likewise, but remove all conversions

   function Get_Type_Size (T : Type_T) return ULL is
     (ABI_Size_Of_Type (Module_Data_Layout, T))
     with Pre => Present (T);
   --  Return the size of an LLVM type, in bytes

   function Get_Type_Size (T : Type_T) return GL_Value is
     (Size_Const_Int (Get_Type_Size (T)));
   --  Return the size of an LLVM type, in bytes, as an LLVM constant

   function Get_Type_Size_In_Bits (T : Type_T) return ULL is
     (Size_Of_Type_In_Bits (Module_Data_Layout, T))
     with Pre => Present (T);
   --  Return the size of an LLVM type, in bits

   function Get_Type_Size_In_Bits (V : GL_Value) return ULL is
     (Size_Of_Type_In_Bits (Module_Data_Layout, Type_Of (V.Value)))
     with Pre => Present (V);
   --  Return the size of an LLVM type, in bits

   function Get_Type_Size_In_Bits (T : Type_T) return GL_Value is
     (Const_Int (Size_Type, Get_Type_Size_In_Bits (T), False))
     with Pre  => Present (T),
          Post => Present (Get_Type_Size_In_Bits'Result);
   --  Return the size of an LLVM type, in bits, as an LLVM constant

   function Get_Type_Size_In_Bits (TE : Entity_Id) return GL_Value
     with Pre  => Present (TE),
          Post => Present (Get_Type_Size_In_Bits'Result);
   --  Likewise, but convert from a GNAT type

   function Get_Type_Size_In_Bits (V : GL_Value) return GL_Value is
     (Get_Type_Size_In_Bits (V.Typ))
     with Pre  => Present (V),
          Post => Present (Get_Type_Size_In_Bits'Result);
   --  Variant of above to get type from a GL_Value

   function Get_Type_Alignment (T : Type_T) return ULL is
     (ULL (ABI_Alignment_Of_Type (Module_Data_Layout, T)))
     with Pre => Present (T);
   --  Return the size of an LLVM type, in bits

   function Is_Loadable_Type (T : Type_T) return Boolean
     with Pre => Present (T);
   --  Used for the function below

   function Is_Loadable_Type (TE : Entity_Id) return Boolean is
     (not Is_Nonnative_Type (TE) and then Is_Loadable_Type (Create_Type (TE)))
     with Pre => Is_Type (TE);
   --  Returns True if we should use a load/store instruction to copy values
   --  of this type.  We can't do this if it's of dynamic size, but LLVM
   --  also doesn't do well with large load/store instructions.

   function Allocate_For_Type
     (TE       : Entity_Id;
      Alloc_TE : Entity_Id;
      N        : Node_Id;
      V        : GL_Value := No_GL_Value;
      Expr     : Node_Id  := Empty;
      Name     : String   := "";
      Max_Size : Boolean  := False) return GL_Value
     with Pre  => Is_Type (TE) and then Is_Type (Alloc_TE),
          Post => Is_Access_Type (Allocate_For_Type'Result);
   --  Allocate space on the stack for an object of type TE and return a
   --  pointer to the space.  Name is the name to use for the LLVM value.
   --  V, if Present, is a value to be copyied to the temporary and can be
   --  used to size the allocated space.  Likewise For Expr, but both Expr
   --  and V can't be Present.  N is a node used for a Sloc if we have to
   --  raise an exception.

   function Heap_Allocate_For_Type
     (TE       : Entity_Id;
      Alloc_TE : Entity_Id;
      V        : GL_Value  := No_GL_Value;
      N        : Node_Id   := Empty;
      Expr     : Node_Id   := Empty;
      Proc     : Entity_Id := Empty;
      Pool     : Entity_Id := Empty;
      Max_Size : Boolean   := False) return GL_Value
     with Pre  => Is_Type (TE) and then Is_Type (Alloc_TE)
                  and then (No (Proc) or else Present (Pool)),
          Post => Is_Access_Type (Heap_Allocate_For_Type'Result);
   --  Similarly, but allocate storage on the heap.  This handles default
   --  allocation, secondary stack, and storage pools.

   procedure Heap_Deallocate
     (V        : GL_Value;
      Desig_TE : Entity_Id;
      Proc     : Entity_Id;
      Pool     : Entity_Id)
     with Pre => Present (V)
                 and then (No (Proc) or else Present (Pool))
                 and then (No (Desig_TE) or else Is_Type (Desig_TE));
   --  Free memory allocated by Heap_Allocate_For_Type

   function To_Size_Type (V : GL_Value) return GL_Value is
     (Convert (V, Size_Type))
     with Pre  => Present (V),
          Post => Type_Of (To_Size_Type'Result) = LLVM_Size_Type;
   --  Convert V to Size_Type.  This is always Size_Type's width, but may
   --  actually be a different GNAT type.

   function Get_Type_Alignment (TE : Entity_Id) return ULL
     with Pre => Is_Type (TE);
   --  Return the size of a GNAT type, in bits

   function Get_Type_Size
     (TE       : Entity_Id;
      V        : GL_Value := No_GL_Value;
      Max_Size : Boolean  := False) return GL_Value
     with Pre => Is_Type (TE), Post => Present (Get_Type_Size'Result);
   --  Return the size of a type, in bytes, as a GL_Value.  If TE is
   --  an unconstrained array type, V must be the value of the array.

   function Compute_Size
     (Left_Type, Right_Type   : Entity_Id;
      Left_Value, Right_Value : GL_Value) return GL_Value
     with Pre  => Is_Type (Left_Type) and then Present (Right_Type)
                  and then Present (Right_Value),
          Post =>  Present (Compute_Size'Result);
   --  Used for comparison and assignment: compute the size to be used in
   --  the operation.  Right_Value must be specified.  Left_Value is
   --  optional and will be specified in the comparison case, but not the
   --  assignment case.  If Right_Value is a discriminated record, we
   --  assume here that the last call to Emit_LValue was to compute
   --  Right_Value so that we can use Get_Matching_Value to return the
   --  proper object.  In the comparison case, where Left_Value is
   --  specified, we can only be comparing arrays, so we won't need to
   --  use Get_Matching_Value.

   function Get_Type_Size_Complexity
     (TE : Entity_Id; Max_Size : Boolean := False) return Nat
     with Pre  => Is_Type (TE);
   --  Return the complexity of computing the size of a type.  This roughly
   --  gives the number of "things" needed to access to compute the size.
   --  This returns zero iff the type is of a constant size.

   procedure Add_Type_Data_To_Instruction (Inst : Value_T; TE : Entity_Id);
   --  Add type data (e.g., volatility and TBAA info) to an Instruction

   --  In order to use the generic functions that computing sizing
   --  information to compute whether a size is dynamic, we need versions
   --  of the routines that actually compute the size that instead only
   --  record the size if it's a constant.  We use the data structure
   --  below.

   type IDS is record
      Is_None     : Boolean;
      Value       : GL_Value;
   end record;

   No_IDS  : constant IDS := (True,  No_GL_Value);
   Var_IDS : constant IDS := (False, No_GL_Value);

   function No      (V : IDS) return Boolean is (V =  No_IDS);
   function Present (V : IDS) return Boolean is (V /= No_IDS);

   function IDS_Is_Const  (V : IDS) return Boolean is (Present (V.Value));

   function IDS_Const (C : ULL; Sign_Extend : Boolean := False) return IDS is
     ((False,  Size_Const_Int (C, Sign_Extend)))
     with Post => IDS_Is_Const (IDS_Const'Result);

   function IDS_Const_Int (TE : Entity_Id; C : Uint) return IDS is
     ((False, Const_Int (TE, C)))
     with Pre  => C /= No_Uint;

   function IDS_Type_Size
     (TE       : Entity_Id;
      V        : GL_Value := No_GL_Value;
      Max_Size : Boolean := False) return IDS
     with Pre => Is_Type (TE), Post => Present (IDS_Type_Size'Result);

   function IDS_I_Cmp
     (Op       : Int_Predicate_T;
      LHS, RHS : IDS;
      Name     : String := "") return IDS is
     (if   IDS_Is_Const (LHS) and then IDS_Is_Const (RHS)
      then (False, I_Cmp (Op, LHS.Value, RHS.Value, Name)) else Var_IDS)
     with Pre  => Present (LHS) and then Present (RHS),
          Post => Present (IDS_I_Cmp'Result);

   function IDS_Add (V1, V2 : IDS; Name : String := "") return IDS is
     (if   IDS_Is_Const (V1) and then IDS_Is_Const (V2)
      then (False, Add (V1.Value, V2.Value, Name)) else Var_IDS)
     with Pre  => Present (V1) and then Present (V2),
          Post => Present (IDS_Add'Result);

   function IDS_Sub (V1, V2 : IDS; Name : String := "") return IDS is
     (if   IDS_Is_Const (V1) and then IDS_Is_Const (V2)
      then (False, Sub (V1.Value, V2.Value, Name)) else Var_IDS)
     with Pre  => Present (V1) and then Present (V2),
          Post => Present (IDS_Sub'Result);

   function IDS_Mul (V1, V2 : IDS; Name : String := "") return IDS is
     (if   IDS_Is_Const (V1) and then IDS_Is_Const (V2)
      then (False, Mul (V1.Value, V2.Value, Name)) else Var_IDS)
     with Pre  => Present (V1) and then Present (V2),
          Post => Present (IDS_Mul'Result);

   function IDS_U_Div (V1, V2 : IDS; Name : String := "") return IDS is
     (if   IDS_Is_Const (V1) and then IDS_Is_Const (V2)
      then (False, U_Div (V1.Value, V2.Value, Name)) else Var_IDS)
     with Pre  => Present (V1) and then Present (V2),
          Post => Present (IDS_U_Div'Result);

   function IDS_S_Div (V1, V2 : IDS; Name : String := "") return IDS is
     (if   IDS_Is_Const (V1) and then IDS_Is_Const (V2)
      then (False, S_Div (V1.Value, V2.Value, Name)) else Var_IDS)
     with Pre  => Present (V1) and then Present (V2),
          Post => Present (IDS_S_Div'Result);

   function IDS_Min (V1, V2 : IDS) return IDS
     with Pre  => Present (V1) and then Present (V2),
          Post => Present (IDS_Min'Result);

   function IDS_Max (V1, V2 : IDS) return IDS
     with Pre  => Present (V1) and then Present (V2),
          Post => Present (IDS_Max'Result);

   function IDS_And (V1, V2 : IDS; Name : String := "") return IDS is
     (if   IDS_Is_Const (V1) and then IDS_Is_Const (V2)
        then (False, Build_And (V1.Value, V2.Value, Name)) else Var_IDS)
     with Pre  => Present (V1) and then Present (V2),
          Post => Present (IDS_And'Result);

   function IDS_Neg (V : IDS; Name : String := "") return IDS is
     (if   IDS_Is_Const (V) then (False, Neg (V.Value, Name)) else Var_IDS)
     with Pre => Present (V), Post => Present (IDS_Neg'Result);

   function IDS_Select
     (V_If, V_Then, V_Else : IDS; Unused_Name : String := "") return IDS
   is
     (if    IDS_Is_Const (V_If) and then V_If.Value = Const_True then V_Then
      elsif IDS_Is_Const (V_If) then V_Else else Var_IDS)
     with Pre => Present (V_If) and then Present (V_Then)
                 and then Present (V_Else);

   function IDS_Const_Val_ULL (V : IDS) return ULL is
     (Get_Const_Int_Value_ULL (V.Value))
     with Pre => IDS_Is_Const (V);

   function IDS_Const_Int (V : IDS) return LLI is
     (Get_Const_Int_Value (V.Value))
     with Pre => IDS_Is_Const (V);

   function IDS_Extract_Value
     (TE             : Entity_Id;
      V              : GL_Value;
      Unused_Idx_Arr : Index_Array;
      Unused_Name    : String := "") return IDS
   is
      (Var_IDS)
     with Pre  => Is_Type (TE) and then Present (V),
          Post => Present (IDS_Extract_Value'Result);

   function IDS_Convert
     (V              : IDS;
      TE             : Entity_Id;
      Float_Truncate : Boolean := False) return IDS
   is
     (if   IDS_Is_Const (V) then (False, Convert (V.Value, TE, Float_Truncate))
      else Var_IDS)
     with Pre => Present (V) and then Is_Type (TE);

   function IDS_Emit_Expr (V : Node_Id; LHS : IDS := No_IDS) return IDS
     with Pre => Present (V), Post => Present (IDS_Emit_Expr'Result);

   function IDS_Emit_Convert (N : Node_Id; TE : Entity_Id) return IDS is
     (IDS_Convert (IDS_Emit_Expr (N), TE))
     with Pre  => Present (N) and then Is_Type (TE),
          Post => Present (IDS_Emit_Convert'Result);

   function IDS_Undef (TE : Entity_Id) return IDS is
     (Var_IDS)
     with Pre => Is_Type (TE), Post => Present (IDS_Undef'Result);

   Disable_LV_Append : Nat := 0;
   --  If nonzero, disable appending expressions to the LValue list.

end GNATLLVM.Types;
