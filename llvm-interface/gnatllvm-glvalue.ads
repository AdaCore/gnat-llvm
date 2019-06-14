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

with Ada.Unchecked_Deallocation;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

package GNATLLVM.GLValue is

   --  It's not sufficient to just pass around an LLVM Value_T when
   --  generating code because there's a lot of information lost about
   --  the value and where it came from.  We contruct a record of type
   --  GL_Value, which contains the LLVM Value_T (which, in turn
   --  contains it's LLVM Type_T), a GNAT type to which it's related,
   --  and a field indicating the relationship between the value and
   --  the type.  For example, the value may contain bits of the type
   --  or the value may be the address of the bits of the type.

   type GL_Relationship is
     (Data,
      --  Value is actual bits of Typ.  This can never be set for
      --  subprogram types or for types of variable size.  It can be set
      --  for non-first-class types in the LLVM sense as long as LLVM can
      --  represent a value of that object.  If Typ is an access type, this
      --  is requivalent to a relationship of Reference to the
      --  Designated_Type of Typ.

      Boolean_Data,
      --  Like Data, but this is an actual LLVM boolean value (i1) instead
      --  of the normal i8 that we'd use for a Boolean type.  In this case,
      --  the type must be Standard_Boolean.

      Reference,
      --  Value contains the address of an object of Typ.  This is always
      --  the case for types of variable size or for names corresponding to
      --  globals because those names represent the address of the global,
      --  either for data or functions.

      Reference_To_Reference,
      --  Value contains the address of memory that contains the address of
      --  an object of Typ.  This occurs for globals where either an
      --  'Address attribute was specifed or where an object of dynamic
      --  size was allocated because in both of those cases the global name
      --  is a pointer to a location containing the address of the object.

      Fat_Pointer,
      --  Value contains a "fat pointer", an object containing information
      --  about both the data and bounds of an array object of Typ.

      Bounds,
      --  Value contains data representing the bounds of an object of Typ,
      --  which must be an array type.

      Bounds_And_Data,
      --  Value contains data representing the bounds of an object of Typ,
      --  which must be an array type, followed by the actual data, with
      --  only padding required by the alignment of the array between the
      --  bounds and data.

      Reference_To_Bounds,
      --  Value contains an address that points to the bounds of an object
      --  of Typ, which must be an array type.

      Reference_To_Bounds_And_Data,
      --  Value contains an address that points to the bounds of an object
      --  of Typ, which must be an array type, followed by the data.

      Thin_Pointer,
      --  Similar to Reference, except that the bounds are guaranteed to be
      --  in memory in front of the data (with the minimal padding between
      --  then).  Also similar to Reference_To_Bounds_And_Data, except for
      --  exactly where the pointer references.

      Reference_To_Thin_Pointer,
      --  Similar to Reference_To_Reference, except that the underlying
      --  object is an aliased object with a nominal constrained type.

      Reference_To_Subprogram,
      --  Value contains the address of a subprogram which is a procedure
      --  if Typ is an E_Void or which is a function returning type Typ
      --  if Typ is not a Void.  If Typ is a subprogram type, then
      --  Reference should be used instead and if Typ is an access
      --  to subprogram type, then Data is the appropriate relationship.

      Reference_To_Ref_To_Subprogram,
      --  Value contains an address at which the address of a subprogram
      --  is found.  That address is of relationship Reference_To_Subprogram.

      Activation_Record,
      --  Value is an activation record for a subprogram of Typ

      Reference_To_Activation_Record,
      --  Value is a reference to an activation record for a subprogram of Typ

      Fat_Reference_To_Subprogram,
      --  Similar to Reference_To_Subprogram except that it contains both
      --  a pointer to the subprogram and to the activation record.

      Trampoline,
      --  A pointer to a piece of code that can be called.  This can
      --  either be the subprogram itself or a fragment on the stack
      --  that can be called and encapsulates both the address of the
      --  subprogram and the address of the static link.

      Unknown,
      --  Object is an unknown relation to the type.  Used for peculiar
      --  LLVM objects such as landing pads or the structure representing
      --  the return from a function.

      Reference_To_Unknown,
      --  Similar to Unknown, but we know that this is a reference and a
      --  dereference to it will be Unknown.

      Any_Reference,
      --  Valid only as an operand to Get and indicates that a value with
      --  any reference to data can be returned.  This includes fat and
      --  thin pointers, but not such things as references to bounds
      --  or references to references.

      Reference_For_Integer,
      --  Valid only as an operand to Get and indicates that a value
      --  with a single-word reference to data can be returned.  This
      --  includes thin pointers, but not such things as references to
      --  bounds or any fat structure.  This is used when we want to compare
      --  two access types or convert an address to an integer.

      Object,
      --  Valid only as an operand to Get and means Any_Reference if
      --  the type of the value is of dynamic size and Data otherwise.

      Invalid);
      --  This is invalid relationship, which will result from, e.g.,
      --  doing a dereference operation on something that isn't a reference.

   --  We define some properties on each relationship type so we can
   --  do some reasoning on them.  This record and array are used to express
   --  those properties.

   type Relationship_Property is record
     Is_Ref : Boolean;
     --  True if this is a reference to something

     Is_Any_Ref : Boolean;
     --  True if this can be returned for Any_Reference

     Deref  : GL_Relationship;
     --  The relationship, if any, corresponding to a dereference (Load) from a
     --  GL_Value that has this relationship.

     Ref    : GL_Relationship;
     --  The relationship, if any, corresponding to a reference (taking the
     --  address of) A GL_Value that has this relationship.

   end record;

   type Relationship_Array is array (GL_Relationship) of Relationship_Property;

   Relation_Props : constant Relationship_Array :=
     (Data                           =>
        (Is_Ref => False, Is_Any_Ref => False,
         Deref  => Invalid,          Ref => Reference),
      Boolean_Data                   =>
        (Is_Ref => False, Is_Any_Ref => False,
         Deref  => Invalid,          Ref => Invalid),
      Reference                      =>
        (Is_Ref => True,  Is_Any_Ref => True,
         Deref  => Data,             Ref => Reference_To_Reference),
      Reference_To_Reference         =>
        (Is_Ref => True,  Is_Any_Ref => False,
         Deref  => Reference,        Ref => Invalid),
      Fat_Pointer                    =>
        (Is_Ref => True,  Is_Any_Ref => True,
         Deref  => Invalid,          Ref => Invalid),
      Bounds                         =>
        (Is_Ref => False, Is_Any_Ref => False,
         Deref  => Invalid,          Ref => Reference_To_Bounds),
      Bounds_And_Data                =>
        (Is_Ref => False, Is_Any_Ref => False,
         Deref  => Invalid,          Ref => Reference_To_Bounds_And_Data),
      Reference_To_Bounds            =>
        (Is_Ref => True,  Is_Any_Ref => False,
         Deref  => Bounds,           Ref => Invalid),
      Reference_To_Bounds_And_Data   =>
        (Is_Ref => True,  Is_Any_Ref => False,
         Deref  => Bounds_And_Data,  Ref => Invalid),
      Reference_To_Thin_Pointer      =>
        (Is_Ref => True,  Is_Any_Ref => False,
         Deref  => Thin_Pointer,     Ref => Invalid),
      Thin_Pointer                   =>
        (Is_Ref => True,  Is_Any_Ref => True,
         Deref  => Invalid,          Ref => Reference_To_Thin_Pointer),
      Reference_To_Subprogram        =>
        (Is_Ref => True,  Is_Any_Ref => True,
         Deref  => Invalid,          Ref => Reference_To_Ref_To_Subprogram),
      Reference_To_Ref_To_Subprogram =>
        (Is_Ref => True,  Is_Any_Ref => False,
         Deref  => Reference_To_Subprogram, Ref => Invalid),
      Activation_Record              =>
        (Is_Ref => False, Is_Any_Ref => False,
         Deref  => Invalid,          Ref => Reference_To_Activation_Record),
      Reference_To_Activation_Record =>
        (Is_Ref => True,  Is_Any_Ref => False,
         Deref  => Activation_Record, Ref => Invalid),
      Fat_Reference_To_Subprogram    =>
        (Is_Ref => True,  Is_Any_Ref => True,
         Deref  => Invalid,          Ref => Invalid),
      Trampoline                     =>
        (Is_Ref => True,  Is_Any_Ref => True,
         Deref  => Invalid,          Ref => Invalid),
      Reference_To_Unknown           =>
        (Is_Ref => True, Is_Any_Ref => False,
         Deref  => Unknown,          Ref => Unknown),
      Unknown                        =>
        (Is_Ref => False, Is_Any_Ref => False,
         Deref  => Invalid,          Ref => Reference_To_Unknown),
      Any_Reference                  =>
        (Is_Ref => True,  Is_Any_Ref => False,
         Deref  => Invalid,          Ref => Invalid),
      Reference_For_Integer          =>
        (Is_Ref => True,  Is_Any_Ref => False,
         Deref  => Invalid,          Ref => Invalid),
      Object                         =>
        (Is_Ref => True,  Is_Any_Ref => False,
         Deref  => Invalid,          Ref => Invalid),
      Invalid                        =>
        (Is_Ref => False, Is_Any_Ref => False,
         Deref  => Invalid,          Ref => Invalid));

   function Deref (R : GL_Relationship) return GL_Relationship is
     (Relation_Props (R).Deref);
   function Ref (R : GL_Relationship)   return GL_Relationship is
     (Relation_Props (R).Ref);

   function Is_Reference (R : GL_Relationship)            return Boolean is
     (Relation_Props (R).Is_Ref);
   function Is_Any_Reference (R : GL_Relationship)        return Boolean is
     (Relation_Props (R).Is_Any_Ref);
   function Is_Double_Reference (R : GL_Relationship)     return Boolean is
     (Is_Reference (Deref (R)));
   function Is_Single_Reference (R : GL_Relationship)     return Boolean is
     (Is_Reference (R) and then not Is_Double_Reference (R));

   function Is_Subprogram_Reference (R : GL_Relationship) return Boolean is
     (R = Reference_To_Subprogram);

   function Is_Data (R : GL_Relationship)                 return Boolean is
     (R in Data | Boolean_Data | Bounds_And_Data);

   function Relationship_For_Ref (GT : GL_Type) return GL_Relationship
     with Pre => Present (GT);
   --  Return the relationship to use for a reference to GT

   function Relationship_For_Ref (TE : Entity_Id) return GL_Relationship
     with Pre => Is_Type (TE);
   --  Return the relationship to use for a reference to TE

   function Relationship_For_Access_Type (GT : GL_Type) return GL_Relationship
     with Pre => Present (GT);
   --  Given an access type, return the Relationship that a value of this
   --  type would have with its Designated_Type.  Similar to
   --  Relationship_For_Ref on the Designated_Type of GT, but takes into
   --  account anything special about TE, such as its size.

   function Relationship_For_Access_Type
     (TE : Entity_Id) return GL_Relationship
     with Pre => Is_Access_Type (TE);
   --  Given an access type, return the Relationship that a value of this
   --  type would have with its Designated_Type.  Similar to
   --  Relationship_For_Ref on the Designated_Type of TE, but takes into
   --  account anything special about TE, such as its size.

   function Relationship_For_Alloc (TE : Entity_Id) return GL_Relationship
     with Pre => Is_Type (TE);
   --  Return the relationship to TE that allocating memory for TE produces.
   --  Similar to Relationship_For_Ref, but take into account the need to
   --  also allocate space for bounds in some situations.

   function Relationship_For_Alloc (GT : GL_Type) return GL_Relationship
     with Pre => Present (GT);
   --  Return the relationship to GL that allocating memory for GL produces.
   --  Similar to Relationship_For_Ref, but take into account the need to
   --  also allocate space for bounds in some situations.

   function Type_For_Relationship
     (GT : GL_Type; R : GL_Relationship) return Type_T
     with Pre => Present (GT), Post => Present (Type_For_Relationship'Result);
   --  Return the LLVM type corresponding to a value of relationship R to GT

   function Type_For_Relationship
     (TE : Entity_Id; R : GL_Relationship) return Type_T
     with Pre => Is_Type (TE), Post => Present (Type_For_Relationship'Result);
   --  Return the LLVM type corresponding to a value of relationship R to TE

   type GL_Value_Base is record
      Value                : Value_T;
      --  The LLVM value that was generated

      Typ                  : GL_Type;
      --  The GL_Type of this value, which points to the GNAT type

      Relationship         : GL_Relationship;
      --  The relationship between Value and Typ.

      Is_Pristine          : Boolean;
      --  Set when this value has just been allocated and there's no chance
      --  yet of it being written.  We know that no expression can conflict
      --  with it.

   end record;
   --  We want to put a Predicate on this, but can't, so we need to make
   --  a subtype for that purpose.

   function GL_Value_Is_Valid (V : GL_Value_Base) return Boolean;
   --  Return whether V is a valid GL_Value or not

   subtype GL_Value is GL_Value_Base
     with Predicate => GL_Value_Is_Valid (GL_Value);
   --  Subtype used by everybody except validation function

   function "<" (LHS : GL_Value; RHS : Int) return Boolean
     with Pre => Present (LHS);
   function "<=" (LHS : GL_Value; RHS : Int) return Boolean
     with Pre => Present (LHS);
   function ">" (LHS : GL_Value; RHS : Int) return Boolean
     with Pre => Present (LHS);
   function ">=" (LHS : GL_Value; RHS : Int) return Boolean
     with Pre => Present (LHS);
   function "=" (LHS : GL_Value; RHS : Int) return Boolean
     with Pre => Present (LHS);

   type GL_Value_Array is array (Nat range <>) of GL_Value;
   type Access_GL_Value_Array is access all GL_Value_Array;
   procedure Free is new Ada.Unchecked_Deallocation (GL_Value_Array,
                                                     Access_GL_Value_Array);

   No_GL_Value : constant GL_Value := (No_Value_T, No_GL_Type, Data, False);
   function No      (V : GL_Value) return Boolean      is (V =  No_GL_Value);
   function Present (V : GL_Value) return Boolean      is (V /= No_GL_Value);

   --  Define basic accessors for components of GL_Value

   function LLVM_Value (V : GL_Value)   return Value_T is
     (V.Value)
     with Pre => Present (V), Post => Present (LLVM_Value'Result);
   --  Return the LLVM value in the GL_Value

   function Related_Type (V : GL_Value) return GL_Type is
     (V.Typ)
     with Pre => Present (V), Post => Present (Related_Type'Result);
   --  Return the GL_Type to which V is related, irrespective of the
   --  relationship.

   function Relationship (V : GL_Value) return GL_Relationship is
     (V.Relationship)
     with Pre => Present (V);

   function Is_Pristine (V : GL_Value)  return Boolean is
     (V.Is_Pristine)
     with Pre => Present (V);

   --  Define functions about relationships

   function Equiv_Relationship (R1, R2 : GL_Relationship) return Boolean is
     (R1 = R2 or else (R1 = Any_Reference and then Is_Any_Reference (R2))
      or else (R2 = Any_Reference and then Is_Any_Reference (R1)));
   --  True if R1 and R2 are equivalent relationships in terms of the operand
   --  passed to Get and the relationship in its return value.

   --  Now some predicates derived from the above

   function Is_Reference (V : GL_Value)            return Boolean is
     (Is_Reference (Relationship (V)))
     with Pre => Present (V);

   function Is_Any_Reference (V : GL_Value)        return Boolean is
     (Is_Any_Reference (Relationship (V)))
     with Pre => Present (V);

   function Is_Double_Reference (V : GL_Value)     return Boolean is
     (Is_Double_Reference (Relationship (V)))
     with Pre => Present (V);

   function Is_Single_Reference (V : GL_Value)     return Boolean is
     (Is_Single_Reference (Relationship (V)))
     with Pre => Present (V);

   function Is_Subprogram_Reference (V : GL_Value) return Boolean is
     (Is_Subprogram_Reference (Relationship (V)))
     with Pre => Present (V);

   function Is_Data (V : GL_Value)                 return Boolean is
     (Is_Data (Relationship (V)))
     with Pre => Present (V);

   function Etype (V : GL_Value)                   return Entity_Id
     with Pre  => Present (V)
                  and then (Is_Data (V) or else Relationship (V) = Unknown),
          Post => Is_Type_Or_Void (Etype'Result);

   --  Constructors for a GL_Value

   function G
     (V                    : Value_T;
      GT                   : GL_Type;
      Relationship         : GL_Relationship := Data;
      Is_Pristine          : Boolean := False) return GL_Value is
     ((V, GT, Relationship, Is_Pristine))
     with Pre => Present (V) and then Present (GT);
   --  Raw constructor that allow full specification of all fields

   function G_From (V : Value_T; GV : GL_Value) return GL_Value is
     (G (V, GL_Type'(Related_Type (GV)), Relationship (GV), Is_Pristine (GV)))
     with Pre  => Present (V) and then Present (GV),
          Post => Present (G_From'Result);
   --  Constructor for most common operation cases where we aren't changing
   --  any typing information, so we just copy it from an existing value.

   function G_Is (V : GL_Value; GT : GL_Type) return GL_Value is
     (G (LLVM_Value (V), GT, Relationship (V), Is_Pristine (V)))
     with Pre  => Present (V) and then Present (GT),
          Post => Present (G_Is'Result);
   --  Constructor for case where we want to show that V has a different type

   function G_Is (V : GL_Value; T : GL_Value) return GL_Value is
     (G (LLVM_Value (V), GL_Type'(Related_Type (T)), Relationship (V),
         Is_Pristine (V)))
     with Pre  => Present (V) and then Present (T),
          Post => Present (G_Is'Result);

   function G_Is_Relationship
     (V : GL_Value; GT : GL_Type; R : GL_Relationship) return GL_Value
   is
     (G (LLVM_Value (V), GT, R, Is_Pristine (V)))
     with Pre  => Present (V) and then Present (GT),
          Post => Present (G_Is_Relationship'Result);
   --  Constructor for case where we want to show that V has a different type
   --  and relationship.

   function G_Is_Relationship
     (V : GL_Value; T : GL_Value; R : GL_Relationship) return GL_Value
   is
     (G (LLVM_Value (V), GL_Type'(Related_Type (T)), R, Is_Pristine (V)))
     with Pre  => Present (V) and then Present (T),
          Post => Present (G_Is_Relationship'Result);
   --  Constructor for case where we want to show that V has a different type
   --  and relationship.

   function G_Is_Relationship (V : GL_Value; T : GL_Value) return GL_Value is
      (G (LLVM_Value (V), GL_Type'(Related_Type (T)), Relationship (T),
          Is_Pristine (V)))
     with Pre  => Present (V) and then Present (T),
          Post => Present (G_Is_Relationship'Result);
   --  Constructor for case where we want to show that V has a different type
   --  and relationship.

   function G_Ref
     (V           : Value_T;
      GT          : GL_Type;
      Is_Pristine : Boolean := False) return GL_Value
   is
     (G (V, GT, Relationship_For_Ref (GT), Is_Pristine))
     with Pre  => Present (V) and then Present (GT),
          Post => Is_Reference (G_Ref'Result);
   --  Constructor for case where we create a value that's a pointer
   --  to type GT.

   function Not_Pristine (V : GL_Value) return GL_Value is
     (G (LLVM_Value (V), GL_Type'(Related_Type (V)), Relationship (V), False))
     with Pre => Present (V), Post => not Is_Pristine (Not_Pristine'Result);
   --  Make a copy of V with the Is_Pristine flag cleared

   procedure Discard (V : GL_Value);
   --  Evaluate V and throw away the result

   procedure Set_Value (VE : Entity_Id; VL : GL_Value);
   --  Set a value for an entity.  This turns off the Is_Pristine flag.

   --  Now define predicates on the GL_Value type to easily access
   --  properties of the LLVM value and the effective type.  These have the
   --  same names as those for types and Value_T's.  The first of these
   --  represent abstractions that will be used in later predicates.

   function Full_Etype (V : GL_Value) return Entity_Id is
     (Etype (V))
     with Pre => Present (V), Post => Is_Type_Or_Void (Full_Etype'Result);

   function Type_Of (V : GL_Value) return Type_T is
     (Type_Of (LLVM_Value (V)))
     with Pre => Present (V), Post => Present (Type_Of'Result);

   function Ekind (V : GL_Value) return Entity_Kind is
     ((if Is_Reference (V) then E_Access_Type else Ekind (Etype (V))))
     with Pre => Present (V);

   function Is_Access_Type (V : GL_Value) return Boolean is
     (Is_Single_Reference (V)
      or else (not Is_Reference (V) and then Is_Access_Type (Etype (V))))
     with Pre => Present (V);

   function Is_Pointer (V : GL_Value) return Boolean is
     (Is_Reference (V)
      or else (not Is_Reference (V) and then Is_Access_Type (Etype (V))))
     with Pre => Present (V);

   function Full_Designated_Type (V : GL_Value) return Entity_Id
     with Pre  => Is_Access_Type (V) and then not Is_Subprogram_Reference (V),
          Post => Is_Type_Or_Void (Full_Designated_Type'Result);

   function Full_Base_Type (V : GL_Value) return Entity_Id
     with Pre  => not Is_Reference (V),
          Post => Is_Type (Full_Base_Type'Result);

   function Is_Dynamic_Size (V : GL_Value) return Boolean
     with Pre => Present (V);

   function Is_Nonnative_Type (V : GL_Value) return Boolean
     with Pre => Present (V);

   function Is_Loadable_Type (V : GL_Value) return Boolean
     with Pre => Present (V);

   function Is_Array_Type (V : GL_Value) return Boolean is
     (not Is_Reference (V) and then Is_Array_Type (Etype (V)))
     with Pre => Present (V);

   function Is_Access_Subprogram_Type (V : GL_Value) return Boolean is
    (Is_Access_Type (V)
       and then Ekind (Full_Designated_Type (V)) = E_Subprogram_Type)
     with Pre => Present (V);

   function Is_Constrained (V : GL_Value) return Boolean is
     (not Is_Reference (V) and then Is_Constrained (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Record_Type (V : GL_Value) return Boolean is
     (not Is_Reference (V) and then Is_Record_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Composite_Type (V : GL_Value) return Boolean is
     (not Is_Reference (V) and then Is_Composite_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Elementary_Type (V : GL_Value) return Boolean is
     (Is_Reference (V) or else Is_Elementary_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Scalar_Type (V : GL_Value) return Boolean is
     (not Is_Reference (V) and then Is_Scalar_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Discrete_Type (V : GL_Value) return Boolean is
     (not Is_Reference (V) and then Is_Discrete_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Integer_Type (V : GL_Value) return Boolean is
     (not Is_Reference (V) and then Is_Integer_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Boolean_Type (V : GL_Value) return Boolean is
     (not Is_Reference (V) and then Is_Boolean_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Fixed_Point_Type (V : GL_Value) return Boolean is
     (not Is_Reference (V) and then Is_Fixed_Point_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Floating_Point_Type (V : GL_Value) return Boolean is
     (not Is_Reference (V) and then Is_Floating_Point_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Unsigned_Type (V : GL_Value) return Boolean is
     (not Is_Reference (V) and then Is_Unsigned_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Discrete_Or_Fixed_Point_Type (V : GL_Value) return Boolean is
     (not Is_Reference (V)
        and then Is_Discrete_Or_Fixed_Point_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Modular_Integer_Type (V : GL_Value) return Boolean is
     (not Is_Reference (V) and then Is_Modular_Integer_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Unconstrained_Record (V : GL_Value) return Boolean
     with Pre => Present (V);

   function Is_Unconstrained_Array (V : GL_Value) return Boolean
     with Pre => Present (V);

   function Is_Unconstrained_Type (V : GL_Value) return Boolean
     with Pre => Present (V);

   function Is_Access_Unconstrained_Array (V : GL_Value) return Boolean
     with Pre => Present (V);

   function Is_Packed_Array_Impl_Type (V : GL_Value) return Boolean
     with Pre => Present (V);

   function Is_Bit_Packed_Array_Impl_Type (V : GL_Value) return Boolean
     with Pre => Present (V);

   function Is_Constr_Subt_For_UN_Aliased (V : GL_Value) return Boolean
     with Pre => Present (V);

   function Type_Needs_Bounds (V : GL_Value) return Boolean
     with Pre => Present (V);

   function RM_Size (V : GL_Value) return Uint is
     (RM_Size (Full_Etype (V)))
     with Pre => not Is_Access_Type (V);

   function Esize (V : GL_Value) return Uint is
     (Esize (Full_Etype (V)))
     with Pre => not Is_Access_Type (V);

   function Component_Type (V : GL_Value) return Entity_Id is
     (Component_Type (Full_Etype (V)))
     with Pre => Is_Array_Type (V), Post => Present (Component_Type'Result);

   function Number_Dimensions (V : GL_Value) return Pos is
     (Number_Dimensions (Full_Etype (V)))
     with Pre => Is_Array_Type (V);

   --  Next are useful functions to manipulate GL_Values

   function Get (V : GL_Value; Rel : GL_Relationship) return GL_Value
     with Pre => Present (V), Post => Equiv_Relationship (Get'Result, Rel);
   --  Produce a GL_Value from V whose relationship to its type is given
   --  by Rel.

   function To_Access (V : GL_Value; GT : GL_Type) return GL_Value
     with Pre  => Present (GT) and then Is_Reference (V),
          Post => Relationship (To_Access'Result) = Data
                  and then Related_Type (To_Access'Result) = GT;
   --  V is a reference to an object whose type is the designated type of
   --  GT.  Convert it to being viewed as an object of type GT.

   function From_Access (V : GL_Value) return GL_Value
     with Pre  => Is_Data (V) and then Is_Access_Type (Full_Etype (V)),
          Post => Is_Reference (From_Access'Result);
   --  V is a value of an access type.  Instead, represent it as a reference
   --  to the designated type of that access type.

   function Equiv_Relationship
     (V : GL_Value; Rel : GL_Relationship) return Boolean
     with Pre => Present (V);
   --  Return True if V has relationship Rel or one that can be returned
   --  by a call to Get with Rel as an operand.

   --  Finally, we have versions of subprograms defined elsewhere that
   --  accept and/or return GL_Value.

   function Get_Value_Kind (V : GL_Value) return Value_Kind_T is
     (Get_Value_Kind (LLVM_Value (V)))
     with Pre => Present (V);

   function Is_A_Global_Variable (V : GL_Value) return Boolean is
     (Present (Is_A_Global_Variable (LLVM_Value (V))))
     with Pre => Present (V);
   --  Return True if V is a global variable

   function Is_A_Function (V : GL_Value) return Boolean is
     (Present (Is_A_Function (LLVM_Value (V))))
     with Pre => Present (V);
   --  Return True if V is a function

   function Is_Undef (V : GL_Value) return Boolean is
     (Is_Undef (LLVM_Value (V)))
     with Pre => Present (V);

   function Is_Constant (V : GL_Value) return Boolean is
     (Is_Constant (LLVM_Value (V)))
     with Pre => Present (V);

   function Is_A_Const_Int (V : GL_Value) return Boolean is
     (Present (Is_A_Constant_Int (LLVM_Value (V))))
     with Pre => Present (V);
   --  Return True if V is a constant integer

   function Is_A_Const_FP (V : GL_Value) return Boolean is
     (Present (Is_A_Constant_FP (LLVM_Value (V))))
     with Pre => Present (V);
   --  Return True if V is a constant floating point value

   function Get_Const_Int_Value (V : GL_Value) return LLI is
     (Const_Int_Get_S_Ext_Value (LLVM_Value (V)))
     with Pre => Is_A_Const_Int (V);
   --  V is a constant integer; get its value

   function Get_Const_Int_Value_ULL (V : GL_Value) return ULL is
     (ULL (Const_Int_Get_S_Ext_Value (LLVM_Value (V))))
     with Pre => Is_A_Const_Int (V);

   function UI_From_GL_Value (V : GL_Value) return Uint is
     (UI_From_LLI (Get_Const_Int_Value (V)))
     with Pre => Is_A_Const_Int (V);

   function Get_Value_Name (V : GL_Value) return String is
     (Get_Value_Name (LLVM_Value (V)))
     with Pre => Present (V);

   procedure Set_Value_Name (V : GL_Value; Name : String)
     with Pre => Present (V);

   procedure Add_Cold_Attribute (V : GL_Value)
     with Pre => Is_A_Function (V);
   --  Add the Cold attribute to function V

   procedure Add_Dereferenceable_Attribute
     (V : GL_Value; Idx : Integer; GT : GL_Type)
     with Pre => Is_A_Function (V) and then Present (GT);
   --  Add the Dereferenceable attribute to parameter with index Idx

   procedure Add_Dereferenceable_Or_Null_Attribute
     (V : GL_Value; Idx : Integer; GT : GL_Type)
     with Pre => Is_A_Function (V) and then Present (GT);
   --  Add the Dereferenceableornull attribute to parameter with index Idx

   procedure Add_Inline_Attribute (V : GL_Value; Subp : Entity_Id)
     with Pre => Is_A_Function (V);
   --  Add the appropropriate Inline attributes, if any, to the LLVM
   --  function V based on the flags in Subp.

   procedure Add_Nest_Attribute (V : GL_Value; Idx : Integer)
     with Pre => Is_A_Function (V);
   --  Add the Nest attribute to parameter with index Idx

   procedure Add_Noalias_Attribute (V : GL_Value; Idx : Integer)
     with Pre => Is_A_Function (V);
   --  Add the Noalias attribute to parameter with index Idx

   procedure Add_Nocapture_Attribute (V : GL_Value; Idx : Integer)
     with Pre => Is_A_Function (V);
   --  Add the Nocapture attribute to parameter with index Idx

   procedure Add_Non_Null_Attribute (V : GL_Value; Idx : Integer)
     with Pre => Is_A_Function (V);
   --  Add the Nonnull attribute to parameter with index Idx

   procedure Add_Readonly_Attribute (V : GL_Value; Idx : Integer)
     with Pre => Is_A_Function (V);
   --  Add the Readonly attribute to parameter with index Idx

   procedure Add_Writeonly_Attribute (V : GL_Value; Idx : Integer)
     with Pre => Is_A_Function (V);
   --  Add the Writeonly attribute to parameter with index Idx

   function Is_Const_Int_Value (V : GL_Value; Val : ULL) return Boolean is
     (Is_A_Const_Int (V) and then Get_Const_Int_Value (V) = LLI (Val))
     with Pre => Present (V);
   --  Return True if V is a constant integer of value Val

   function Set_Arith_Attrs (Inst : Value_T; V : GL_Value) return Value_T
     with Pre  => Present (Inst) and then Present (V),
          Post => Set_Arith_Attrs'Result = Inst;
   --  Set NUW and/or NSW on Inst depending on the type and relationship
   --  of V and return Inst.

   function Get_Undef (GT : GL_Type) return GL_Value
     with Pre => Present (GT), Post => Present (Get_Undef'Result);

   function Get_Undef_Ref (GT : GL_Type) return GL_Value
     with Pre => Present (GT), Post => Is_Reference (Get_Undef_Ref'Result);

   function Get_Undef_Relationship
     (GT : GL_Type; R : GL_Relationship) return GL_Value
   is
     (G (Get_Undef (Type_For_Relationship (GT, R)), GT, R, True))
     with Pre  => Present (GT),
          Post => Present (Get_Undef_Relationship'Result);

   function Get_Undef_Fn_Ret (V : GL_Value) return GL_Value is
     (G (Get_Undef (Get_Return_Type (Get_Element_Type (Type_Of (V)))),
         GL_Type'(Related_Type (V)), Unknown, Is_Pristine => True))
     with Pre => Is_A_Function (V), Post => Is_Undef (Get_Undef_Fn_Ret'Result);

   function Const_Null (GT : GL_Type) return GL_Value
     with Pre => Present (GT), Post => Present (Const_Null'Result);

   function Const_Null_Relationship
     (GT : GL_Type; R : GL_Relationship) return GL_Value
   is
     (G (Const_Null (Type_For_Relationship (GT, R)), GT, R))
     with Pre  => Present (GT),
          Post => Present (Const_Null_Relationship'Result);

   function Const_Null_Alloc (GT : GL_Type) return GL_Value
     with Pre => Present (GT), Post => Present (Const_Null_Alloc'Result);

   function Const_Int (GT : GL_Type; N : Uint) return GL_Value
     with Pre  => Present (GT) and then N /= No_Uint,
          Post => Present (Const_Int'Result);

   function Const_Int
     (GT : GL_Type; N : ULL; Sign_Extend : Boolean := False) return GL_Value
     with Pre  => Present (GT), Post => Present (Const_Int'Result);
   --  ?? This should really test Is_Discrete_Or_Fixed_Point_Type, but we
   --  have a circular elaboration if we try.

   function Const_Ones (GT : GL_Type) return GL_Value is
     (Const_Int (GT, ULL'Last, Sign_Extend => True))
     with Pre => Present (GT), Post => Present (Const_Ones'Result);
   --  Return an LLVM value for the given type where all bits are set

   function Get_Undef (V : GL_Value) return GL_Value is
     (Get_Undef (Related_Type (V)))
     with  Pre  => Present (V), Post => Present (Get_Undef'Result);

   function Const_Null (V : GL_Value) return GL_Value is
     (Const_Null (Related_Type (V)))
     with Pre  => Present (V), Post => Present (Const_Null'Result);

   function Const_Null_Ref (GT : GL_Type) return GL_Value
     with Pre  => Present (GT), Post => Is_Reference (Const_Null_Ref'Result);

   function Const_Int (V : GL_Value; N : Uint) return GL_Value
     with Pre  => Present (V) and then N /= No_Uint,
          Post => Present (Const_Int'Result);

   function Const_Int
     (V : GL_Value; N : ULL; Sign_Extend : Boolean := False) return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V),
          Post => Present (Const_Int'Result);

   function Const_Int
     (V           : GL_Value;
      N           : unsigned;
      Sign_Extend : Boolean := False) return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V),
          Post => Present (Const_Int'Result);

   function Const_Ones (V : GL_Value) return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V),
          Post => Present (Const_Ones'Result);
   --  Return an LLVM value for the given type where all bits are set

   function Const_Real
     (GT : GL_Type; V : Interfaces.C.double) return GL_Value
     with Pre  => Present (GT), Post => Present (Const_Real'Result);

   function Size_Const_Int (N : Uint) return GL_Value is
     (Const_Int (Size_GL_Type, N))
     with Pre  => N /= No_Uint, Post => Present (Size_Const_Int'Result);

   function Size_Const_Int
     (N : ULL; Sign_Extend : Boolean := False) return GL_Value
   is
     (Const_Int (Size_GL_Type, N, Sign_Extend))
     with Post => Present (Size_Const_Int'Result);

   function Size_Const_Null return GL_Value is
     (Size_Const_Int (ULL (0)))
     with Post => Present (Size_Const_Null'Result);

   function Const_Int_32 (N : Uint) return GL_Value is
     (Const_Int (Int_32_GL_Type, N))
     with Pre  => N /= No_Uint, Post => Present (Const_Int_32'Result);

   function Const_Int_32
     (N : ULL; Sign_Extend : Boolean := False) return GL_Value
   is
     (Const_Int (Int_32_GL_Type, N, Sign_Extend))
     with Post => Present (Const_Int_32'Result);

   function Const_Int_32
     (N : unsigned; Sign_Extend : Boolean := False) return GL_Value
   is
     (Const_Int (Int_32_GL_Type, unsigned_long_long (N), Sign_Extend))
     with Post => Present (Const_Int_32'Result);

   function Const_Null_32 return GL_Value is
     (Const_Int_32 (ULL (0)))
     with Post => Present (Const_Null_32'Result);

   function Const_Real
     (V : GL_Value; F : Interfaces.C.double) return GL_Value
   is
     (Const_Real (GL_Type'(Related_Type (V)), F))
     with Pre  => Is_Floating_Point_Type (V),
          Post => Present (Const_Real'Result);

   function Const_True return GL_Value
     with Post => Relationship (Const_True'Result) = Boolean_Data;
   function Const_False return GL_Value
     with Post => Relationship (Const_False'Result) = Boolean_Data;

   function Const_Array
     (Elmts : GL_Value_Array; GT : GL_Type) return GL_Value
     with Pre => Present (GT), Post => Present (Const_Array'Result);

   function Const_String (S : String; GT : GL_Type) return GL_Value is
     (G (Const_String (S, unsigned (S'Length), True), GT))
     with Pre => Present (GT), Post => Is_Constant (Const_String'Result);

   function Const_Struct
     (Elmts : GL_Value_Array; GT : GL_Type; Packed : Boolean) return GL_Value
     with Pre => Present (GT), Post => Present (Const_Struct'Result);

   function Get_Float_From_Words_And_Exp
     (GT : GL_Type; Exp : Int; Words : Word_Array) return GL_Value
     with Pre  => Present (GT),
          Post => Present (Get_Float_From_Words_And_Exp'Result);

   function Pred_FP (V : GL_Value) return GL_Value
     with Pre => Present (V), Post => Present (Pred_FP'Result);

   procedure Set_Object_Align (Obj : Value_T; GT : GL_Type; E : Entity_Id)
     with Pre => Present (Obj) and then Present (GT);
   --  Set the alignment of alloca inst or global from GT and E (if present)

   --  Define IR builder variants which take and/or return GL_Value

   function Alloca
     (GT        : GL_Type;
      Def_Ident : Entity_Id := Empty;
      Name      : String    := "") return GL_Value
     with Pre  => Present (GT),
          Post => Is_Access_Type (Alloca'Result);

   function Array_Alloca
     (GT        : GL_Type;
      Num_Elts  : GL_Value;
      Def_Ident : Entity_Id := Empty;
      Name      : String    := "") return GL_Value
     with Pre  => Present (GT) and then Present (Num_Elts),
          Post => Is_Access_Type (Array_Alloca'Result);

   function Int_To_Ptr (V : GL_Value; GT : GL_Type; Name : String := "")
     return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Access_Type (Int_To_Ptr'Result);

   function Ptr_To_Int
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Pointer (V) and then Present (GT),
          Post => Is_Discrete_Or_Fixed_Point_Type (Ptr_To_Int'Result);

   function Ptr_To_Size_Type
     (V : GL_Value; Name : String := "") return GL_Value
   is
     (Ptr_To_Int (V, Size_GL_Type, Name))
     with Pre  => Is_Pointer (V),
          Post => Is_Discrete_Or_Fixed_Point_Type (Ptr_To_Size_Type'Result);

   function Bit_Cast
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Present (V) and then not Is_Access_Type (V)
                  and then Present (GT),
          Post => Present (Bit_Cast'Result);

   function Bit_Cast (V, T : GL_Value; Name : String := "") return GL_Value is
     (G_From (Bit_Cast (IR_Builder, LLVM_Value (V), Type_Of (T), Name), T))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Bit_Cast'Result);

   function Bit_Cast
     (V : GL_Value; T : Type_T; Name : String := "") return GL_Value
   is
     (G_From (Bit_Cast (IR_Builder, LLVM_Value (V), T, Name), V))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Bit_Cast'Result);

   function Bit_Cast_To_Relationship
     (V    : GL_Value;
      T    : Type_T;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
   is
     (G (Bit_Cast (IR_Builder, LLVM_Value (V), T, Name), Related_Type (V), R))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Bit_Cast_To_Relationship'Result);

   function Pointer_Cast
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Pointer (V) and then Present (GT),
          Post => Is_Pointer (Pointer_Cast'Result);

   function Pointer_Cast
     (V, T : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Pointer_Cast (IR_Builder, LLVM_Value (V), Type_Of (T), Name), T))
     with Pre  => Is_Pointer (V) and then Is_Access_Type (T),
          Post => Is_Pointer (Pointer_Cast'Result);

   function Pointer_Cast
     (V : GL_Value; T : Type_T; Name : String := "") return GL_Value
   is
     (G_From (Pointer_Cast (IR_Builder, LLVM_Value (V), T, Name), V))
     with Pre  => Is_Pointer (V) and then Present (T),
          Post => Is_Pointer (Pointer_Cast'Result);

   function Ptr_To_Ref
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Pointer (V) and then Present (GT),
          Post => Is_Pointer (Ptr_To_Ref'Result);

   function Ptr_To_Ref (V, T : GL_Value; Name : String := "") return GL_Value
     with Pre  => Is_Pointer (V) and then Is_Access_Type (T),
          Post => Is_Access_Type (Ptr_To_Ref'Result);

   function Ptr_To_Relationship
     (V    : GL_Value;
      GT   : GL_Type;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
     with Pre  => Is_Pointer (V) and then Present (GT),
          Post => Is_Pointer (Ptr_To_Relationship'Result);

   function Ptr_To_Relationship
     (V    : GL_Value;
      T    : Type_T;
      R    : GL_Relationship;
      Name : String := "") return GL_Value is
     (G (Pointer_Cast (IR_Builder, LLVM_Value (V), T, Name),
         Related_Type (V), R, Is_Pristine (V)))
     with Pre  => Is_Pointer (V) and then Present (T),
          Post => Is_Pointer (Ptr_To_Relationship'Result);

   function Ptr_To_Relationship
     (V, T : GL_Value;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
     with Pre  => Is_Pointer (V) and then Present (T),
          Post => Is_Pointer (Ptr_To_Relationship'Result);

   function Trunc
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Discrete_Or_Fixed_Point_Type (Trunc'Result);

   function Trunc
     (V : GL_Value; T : Type_T; Name : String := "") return GL_Value
   is
     (G_From (Trunc (IR_Builder, LLVM_Value (V), T, Name), V))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Trunc'Result);

   function S_Ext
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Discrete_Or_Fixed_Point_Type (S_Ext'Result);

   function Z_Ext
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Discrete_Or_Fixed_Point_Type (Z_Ext'Result);

   function Z_Ext
     (V : GL_Value; T : Type_T; Name : String := "") return GL_Value
   is
     (G_From (Z_Ext (IR_Builder, LLVM_Value (V), T, Name), V))
     with Pre  => Present (V) and then Present (T),
          Post => Present (Z_Ext'Result);

   function FP_Trunc
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Floating_Point_Type (V) and then Present (GT),
          Post => Is_Floating_Point_Type (FP_Trunc'Result);

   function FP_Ext
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Floating_Point_Type (V) and then Present (GT),
          Post => Is_Floating_Point_Type (FP_Ext'Result);

   function FP_To_SI
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Floating_Point_Type (V) and then Present (GT),
          Post => Is_Discrete_Or_Fixed_Point_Type (FP_To_SI'Result);

   function FP_To_UI
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Floating_Point_Type (V) and then Present (GT),
          Post => Is_Discrete_Or_Fixed_Point_Type (FP_To_UI'Result);

   function UI_To_FP
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Floating_Point_Type (UI_To_FP'Result);

   function SI_To_FP
     (V : GL_Value; GT : GL_Type; Name : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Floating_Point_Type (SI_To_FP'Result);

   function Int_To_Ptr
     (V, T : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Int_To_Ptr (IR_Builder, LLVM_Value (V), Type_Of (T), Name), T))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V)
                  and then Is_Access_Type (T),
          Post => Is_Access_Type (Int_To_Ptr'Result);

   function Ptr_To_Int
     (V, T : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Ptr_To_Int (IR_Builder, LLVM_Value (V), Type_Of (T), Name), T))
     with Pre  => Is_Pointer (V)
                  and then Is_Discrete_Or_Fixed_Point_Type (T),
          Post => Is_Discrete_Or_Fixed_Point_Type (Ptr_To_Int'Result);

   function Trunc (V, T : GL_Value; Name : String := "") return GL_Value is
     (Trunc (V, GL_Type'(Related_Type (T)), Name))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V)
                  and then Is_Discrete_Or_Fixed_Point_Type (T),
          Post => Is_Discrete_Or_Fixed_Point_Type (Trunc'Result);

   function S_Ext (V, T : GL_Value; Name : String := "") return GL_Value is
     (S_Ext (V, GL_Type'(Related_Type (T)), Name))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V)
                  and then Is_Discrete_Or_Fixed_Point_Type (T),
          Post => Is_Discrete_Or_Fixed_Point_Type (S_Ext'Result);

   function Z_Ext (V, T : GL_Value; Name : String := "") return GL_Value is
     (Z_Ext (V, GL_Type'(Related_Type (T)), Name))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V)
                  and then Is_Discrete_Or_Fixed_Point_Type (T),
          Post => Is_Discrete_Or_Fixed_Point_Type (Z_Ext'Result);

   function FP_Trunc (V, T : GL_Value; Name : String := "") return GL_Value is
     (FP_Trunc (V, GL_Type'(Related_Type (T)), Name))
     with Pre  => Is_Floating_Point_Type (V)
                  and then Is_Floating_Point_Type (T),
          Post => Is_Floating_Point_Type (FP_Trunc'Result);

   function FP_Ext (V, T : GL_Value; Name : String := "") return GL_Value is
     (FP_Ext (V, GL_Type'(Related_Type (T)), Name))
     with Pre  => Is_Floating_Point_Type (V)
                  and then Is_Floating_Point_Type (T),
          Post => Is_Floating_Point_Type (FP_Ext'Result);

   function FP_To_SI (V, T : GL_Value; Name : String := "") return GL_Value is
     (FP_To_SI (V, GL_Type'(Related_Type (T)), Name))
     with Pre  => Is_Floating_Point_Type (V)
                  and then Is_Discrete_Or_Fixed_Point_Type (T),
          Post => Is_Discrete_Or_Fixed_Point_Type (FP_To_SI'Result);

   function FP_To_UI (V, T : GL_Value; Name : String := "") return GL_Value is
     (FP_To_UI (V, GL_Type'(Related_Type (T)), Name))
     with Pre  => Is_Floating_Point_Type (V)
                  and then Is_Discrete_Or_Fixed_Point_Type (T),
          Post => Is_Discrete_Or_Fixed_Point_Type (FP_To_UI'Result);

   function UI_To_FP (V, T : GL_Value; Name : String := "") return GL_Value is
     (UI_To_FP (V, GL_Type'(Related_Type (T)), Name))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V)
                  and then Is_Floating_Point_Type (T),
          Post => Is_Floating_Point_Type (UI_To_FP'Result);

   function SI_To_FP (V, T : GL_Value; Name : String := "") return GL_Value is
     (SI_To_FP (V, GL_Type'(Related_Type (T)), Name))
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V)
                  and then Is_Floating_Point_Type (T),
          Post => Is_Floating_Point_Type (SI_To_FP'Result);

   procedure Store (Expr : GL_Value; Ptr : GL_Value)
     with Pre => Present (Expr)
                 and then Present (Ptr) and then Is_Reference (Ptr);

   function Load (Ptr : GL_Value; Name : String := "") return GL_Value
     with Pre  => Is_Reference (Ptr),
          Post => Present (Load'Result);

   function I_Cmp
     (Op       : Int_Predicate_T;
      LHS, RHS : GL_Value;
      Name     : String := "") return GL_Value
   is
     (G (I_Cmp (IR_Builder, Op, LLVM_Value (LHS), LLVM_Value (RHS), Name),
         Boolean_GL_Type, Boolean_Data))
     with Pre  => Present (LHS) and then Present (RHS),
          Post => Present (I_Cmp'Result);

   function F_Cmp
     (Op       : Real_Predicate_T;
      LHS, RHS : GL_Value;
      Name     : String := "") return GL_Value
   is
     (G (F_Cmp (IR_Builder, Op, LLVM_Value (LHS), LLVM_Value (RHS), Name),
         Boolean_GL_Type, Boolean_Data))
     with Pre  => Is_Floating_Point_Type (LHS)
                  and then Is_Floating_Point_Type (RHS),
          Post => Present (F_Cmp'Result);

   function Add
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
      ((if    Is_Const_Int_Value (RHS, 0) then LHS
        elsif Is_Const_Int_Value (LHS, 0) then RHS
        else G_From (Set_Arith_Attrs
                       (Add (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS),
                             Name),
                        LHS),
                     LHS)))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (Add'Result);

   function Sub
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     ((if   Is_Const_Int_Value (RHS, 0) then LHS
       else G_From (Set_Arith_Attrs
                      (Sub (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS),
                            Name),
                       LHS),
                    LHS)))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (Sub'Result);

   function Mul
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     ((if    Is_Const_Int_Value (RHS, 1) then LHS
       elsif Is_Const_Int_Value (LHS, 1) then RHS
       else G_From (Set_Arith_Attrs
                      (Mul (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS),
                            Name),
                       LHS),
                    LHS)))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (Mul'Result);

   function S_Div
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (S_Div (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (S_Div'Result);

   function U_Div
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (U_Div (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (U_Div'Result);

   function S_Rem
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (S_Rem (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (S_Rem'Result);

   function U_Rem
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (U_Rem (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (LHS)
                   and then Is_Discrete_Or_Fixed_Point_Type (RHS),
           Post => Is_Discrete_Or_Fixed_Point_Type (U_Rem'Result);

   function Build_And
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Build_And (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (Build_And'Result);

   function Build_Or
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Build_Or (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (Build_Or'Result);

   function Build_Xor
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Build_Xor (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Present (LHS) and then Present (RHS),
           Post => Present (Build_Xor'Result);

   function F_Add
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (F_Add (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Is_Floating_Point_Type (LHS)
                   and then Is_Floating_Point_Type (RHS),
           Post => Is_Floating_Point_Type (F_Add'Result);

   function F_Sub
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (F_Sub (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Is_Floating_Point_Type (LHS)
                   and then Is_Floating_Point_Type (RHS),
           Post => Is_Floating_Point_Type (F_Sub'Result);

   function F_Mul
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (F_Mul (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Is_Floating_Point_Type (LHS)
                   and then Is_Floating_Point_Type (RHS),
           Post => Is_Floating_Point_Type (F_Mul'Result);

   function F_Div
     (LHS, RHS : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (F_Div (IR_Builder, LLVM_Value (LHS), LLVM_Value (RHS), Name),
              LHS))
      with Pre  => Is_Floating_Point_Type (LHS)
                   and then Is_Floating_Point_Type (RHS),
           Post => Is_Floating_Point_Type (F_Div'Result);

   function Shl
     (V              : GL_Value;
      Count          : GL_Value;
      Name           : String  := "";
      Allow_Overflow : Boolean := False) return GL_Value
   is
      (G_From ((if   Allow_Overflow
                then Shl (IR_Builder, LLVM_Value (V), LLVM_Value (Count), Name)
                else Set_Arith_Attrs
                  (Shl (IR_Builder, LLVM_Value (V), LLVM_Value (Count), Name),
                   V)),
               V))
      with Pre  => Present (V) and then Present (Count),
           Post => Present (Shl'Result);

   function L_Shr
     (V, Count : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (L_Shr (IR_Builder, LLVM_Value (V), LLVM_Value (Count), Name), V))
      with Pre  => Present (V) and then Present (Count),
           Post => Present (L_Shr'Result);

   function A_Shr
     (V, Count : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (A_Shr (IR_Builder, LLVM_Value (V), LLVM_Value (Count), Name), V))
      with Pre  => Present (V) and then Present (Count),
           Post => Present (A_Shr'Result);

   function Build_Not
     (V : GL_Value; Name : String := "") return GL_Value
   is
      (G_From (Build_Not (IR_Builder, LLVM_Value (V), Name), V))
      with Pre  => Present (V),
           Post => Present (Build_Not'Result);

   function Neg
     (V : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (Set_Arith_Attrs (Neg (IR_Builder, LLVM_Value (V), Name), V), V))
      with Pre  => Is_Discrete_Or_Fixed_Point_Type (V),
           Post => Is_Discrete_Or_Fixed_Point_Type (Neg'Result);

   function F_Neg
     (V : GL_Value; Name : String := "") return GL_Value
   is
     (G_From (F_Neg (IR_Builder, LLVM_Value (V), Name), V))
     with Pre  => Is_Floating_Point_Type (V),
          Post => Is_Floating_Point_Type (F_Neg'Result);

   function "+" (LHS, RHS : GL_Value) return GL_Value is
     (Add (LHS, RHS));
   function "-" (LHS, RHS : GL_Value) return GL_Value is
     (Sub (LHS, RHS));
   function "-" (V : GL_Value) return GL_Value is
     (Neg (V));
   function "*" (LHS, RHS : GL_Value) return GL_Value is
     (Mul (LHS, RHS));
   function "/" (LHS, RHS : GL_Value) return GL_Value is
     (S_Div (LHS, RHS));

   function "<" (LHS, RHS : GL_Value) return Boolean is
     (I_Cmp (Int_SLT, LHS, RHS) = Const_True);
   function "<=" (LHS, RHS : GL_Value) return Boolean is
     (I_Cmp (Int_SLE, LHS, RHS) = Const_True);
   function ">" (LHS, RHS : GL_Value) return Boolean is
     (I_Cmp (Int_SGT, LHS, RHS) = Const_True);
   function ">=" (LHS, RHS : GL_Value) return Boolean is
     (I_Cmp (Int_SGE, LHS, RHS) = Const_True);

   function "+" (LHS : GL_Value; RHS : Int) return GL_Value is
     (LHS + Const_Int (LHS, UI_From_Int (RHS)));
   function "-" (LHS : GL_Value; RHS : Int) return GL_Value is
     (LHS - Const_Int (LHS, UI_From_Int (RHS)));
   function "*" (LHS : GL_Value; RHS : Int) return GL_Value is
     (LHS * Const_Int (LHS, UI_From_Int (RHS)));
   function "/" (LHS : GL_Value; RHS : Int) return GL_Value is
     (LHS / Const_Int (LHS, UI_From_Int (RHS)));

   function To_Bytes (V : GL_Value) return GL_Value is
     ((V + (BPU - 1)) / BPU)
     with Pre => Present (V), Post => Present (To_Bytes'Result);

   function Build_Select
     (C_If, C_Then, C_Else : GL_Value; Name : String := "")
     return GL_Value
   is
     ((if   C_If = Const_True then C_Then elsif C_If = Const_False then C_Else
       else G_From (Build_Select (IR_Builder, C_If => LLVM_Value (C_If),
                                  C_Then => LLVM_Value (C_Then),
                                  C_Else => LLVM_Value (C_Else), Name => Name),
                    C_Then)))
     with Pre  => Ekind (Full_Etype (C_If)) in Enumeration_Kind
                  and then Is_Elementary_Type (C_Then)
                  and then Is_Elementary_Type (C_Else),
          Post => Is_Elementary_Type (Build_Select'Result);

   procedure Build_Cond_Br
     (C_If : GL_Value; C_Then, C_Else : Basic_Block_T)
     with Pre => Ekind (Full_Etype (C_If)) in Enumeration_Kind
                 and then Present (C_Then) and then Present (C_Else);

   procedure Build_Ret (V : GL_Value)
     with Pre => Present (V);

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
     (V : GL_Value; GT : GL_Type; Name : String := "")
     return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Access_Type (Int_To_Ref'Result);
   --  Similar to Int_To_Ptr, but GT is the Designed_Type, not the
   --  access type.

   function Int_To_Relationship
     (V    : GL_Value;
      GT   : GL_Type;
      R    : GL_Relationship;
      Name : String := "") return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V) and then Present (GT),
          Post => Is_Pointer (Int_To_Relationship'Result);
   --  Similar to Int_To_Ptr, but specify the relationship to GT

   function Atomic_RMW
     (Op            : Atomic_RMW_Bin_Op_T;
      Ptr           : GL_Value;
      V             : GL_Value;
      Order         : Atomic_Ordering_T :=
        Atomic_Ordering_Sequentially_Consistent;
      Single_Thread : Boolean := False) return GL_Value is
      (G_From (Atomic_RMW (IR_Builder, Op, LLVM_Value (Ptr), LLVM_Value (V),
                           Order, Single_Thread),
               V))
      with Pre  => Is_Access_Type (Ptr) and then Present (V),
           Post => Present (Atomic_RMW'Result);

   function Extract_Value
     (GT    : GL_Type;
      Arg   : GL_Value;
      Index : unsigned;
      Name  : String := "") return GL_Value
   is
     (G (Extract_Value (IR_Builder, LLVM_Value (Arg), Index, Name), GT))
     with  Pre  => Present (Arg) and then Present (GT),
           Post => Present (Extract_Value'Result);

   function Extract_Value_To_Ref
     (GT    : GL_Type;
      Arg   : GL_Value;
      Index : unsigned;
      Name  : String := "") return GL_Value
   is
     (G_Ref (Extract_Value (IR_Builder, LLVM_Value (Arg), Index, Name), GT))
     with  Pre  => Present (Arg) and then Present (GT),
           Post => Is_Pointer (Extract_Value_To_Ref'Result);

   function Extract_Value_To_Relationship
     (GT    : GL_Type;
      Arg   : GL_Value;
      Index : unsigned;
      R     : GL_Relationship;
      Name  : String := "") return GL_Value
   is
     (G (Extract_Value (IR_Builder, LLVM_Value (Arg), Index, Name),
         GT, R))
     with  Pre  => Present (Arg) and then Present (GT),
           Post => Present (Extract_Value_To_Relationship'Result);

   function Insert_Value
     (Arg, Elt : GL_Value;
      Index    : unsigned;
      Name     : String := "") return GL_Value
   is
     (G_From (Insert_Value (IR_Builder, LLVM_Value (Arg), LLVM_Value (Elt),
                            Index, Name),
              Arg))
     with  Pre  => Present (Arg) and then Present (Elt),
           Post => Present (Insert_Value'Result);

   type Index_Array is array (Nat range <>) of unsigned;

   function Extract_Value
     (GT      : GL_Type;
      Arg     : GL_Value;
      Idx_Arr : Index_Array;
      Name    : String := "") return GL_Value
   is
     (G (Build_Extract_Value (IR_Builder, LLVM_Value (Arg),
                              Idx_Arr'Address, Idx_Arr'Length, Name),
         GT))
     with  Pre  => Present (GT) and then Present (Arg),
           Post => Present (Extract_Value'Result);

   function Extract_Value_To_Ref
     (GT      : GL_Type;
      Arg     : GL_Value;
      Idx_Arr : Index_Array;
      Name    : String := "") return GL_Value
   is
     (G_Ref (Build_Extract_Value (IR_Builder, LLVM_Value (Arg),
                                  Idx_Arr'Address, Idx_Arr'Length, Name), GT))
     with  Pre  => Present (GT) and then Present (Arg),
           Post => Present (Extract_Value_To_Ref'Result);

   function Extract_Value_To_Relationship
     (GT      : GL_Type;
      Arg     : GL_Value;
      Idx_Arr : Index_Array;
      R       : GL_Relationship;
      Name    : String := "") return GL_Value
   is
     (G (Build_Extract_Value (IR_Builder, LLVM_Value (Arg),
                              Idx_Arr'Address, Idx_Arr'Length, Name),
         GT, R))
     with  Pre  => Present (GT) and then Present (Arg),
           Post => Present (Extract_Value_To_Relationship'Result);

   function Insert_Value
     (Arg, Elt : GL_Value;
      Idx_Arr  : Index_Array;
      Name     : String := "") return GL_Value
   is
     (G_From (Build_Insert_Value (IR_Builder, LLVM_Value (Arg),
                                  LLVM_Value (Elt),
                                  Idx_Arr'Address, Idx_Arr'Length, Name),
              Arg))
     with  Pre  => Present (Arg) and then Present (Elt),
           Post => Present (Insert_Value'Result);

   function GEP_To_Relationship
     (GT      : GL_Type;
      R       : GL_Relationship;
      Ptr     : GL_Value;
      Indices : GL_Value_Array;
      Name    : String := "") return GL_Value
     with Pre  => Is_Access_Type (Ptr) and then Present (GT),
          Post => Is_Access_Type (GEP_To_Relationship'Result);

   function GEP_Idx_To_Relationship
     (GT      : GL_Type;
      R       : GL_Relationship;
      Ptr     : GL_Value;
      Indices : Index_Array;
      Name    : String := "") return GL_Value
     with Pre  => Is_Access_Type (Ptr) and then Present (GT),
          Post => Is_Access_Type (GEP_Idx_To_Relationship'Result);

   function GEP
     (GT      : GL_Type;
      Ptr     : GL_Value;
      Indices : GL_Value_Array;
      Name    : String := "") return GL_Value is
     (GEP_To_Relationship (GT, Reference, Ptr, Indices, Name))
     with Pre  => Is_Access_Type (Ptr) and then Present (GT),
          Post => Is_Access_Type (GEP'Result);

   function GEP_Idx
     (GT      : GL_Type;
      Ptr     : GL_Value;
      Indices : Index_Array;
      Name    : String := "") return GL_Value is
     (GEP_Idx_To_Relationship (GT, Reference, Ptr, Indices, Name))
     with Pre  => Is_Access_Type (Ptr) and then Present (GT),
          Post => Is_Access_Type (GEP_Idx'Result);

   function Call
     (Func : GL_Value;
      GT   : GL_Type;
      Args : GL_Value_Array;
      Name : String := "") return GL_Value
     with Pre  => Present (Func) and then Present (GT),
          Post => Present (Call'Result);

   function Call_Ref
     (Func : GL_Value;
      GT   : GL_Type;
      Args : GL_Value_Array;
      Name : String := "") return GL_Value
     with Pre  => Present (Func) and then Present (GT),
          Post => Is_Reference (Call_Ref'Result);

   function Call_Struct
     (Func : GL_Value;
      GT   : GL_Type;
      Args : GL_Value_Array;
      Name : String := "") return GL_Value
     with Pre  => Present (Func) and then Present (GT),
          Post => Present (Call_Struct'Result);
   --  Used when an LLVM function is returning a structure for multiple
   --  values.

   procedure Call
     (Func : GL_Value; Args : GL_Value_Array; Name : String := "")
     with Pre  => Present (Func);

   procedure Call_With_Align
     (Func : GL_Value; Args : GL_Value_Array; Align : Nat; Name : String := "")
     with Pre  => Present (Func);

   procedure Call_With_Align_2
     (Func             : GL_Value;
      Args             : GL_Value_Array;
      Align_1, Align_2 : Nat;
      Name             : String := "")
     with Pre  => Present (Func);

   function Landing_Pad
     (T                : Type_T;
      Personality_Func : GL_Value;
      Num_Clauses      : Nat    := 5;
      Name             : String := "") return GL_Value
   is
      (G (Landing_Pad (IR_Builder, T, LLVM_Value (Personality_Func),
                       unsigned (Num_Clauses), Name),
         A_Char_GL_Type, Unknown))
     with Pre  => Present (T) and then Present (Personality_Func),
          Post => Present (Landing_Pad'Result);

   procedure Add_Clause (V, Exc : GL_Value)
     with Pre => Present (V) and then Present (Exc);

   procedure Set_Cleanup (V : GL_Value)
     with Pre => Present (V);

   procedure Build_Resume (V : GL_Value)
     with Pre => Present (V);

   function Inline_Asm
     (Args           : GL_Value_Array;
      Output_Value   : Entity_Id;
      Template       : String;
      Constraints    : String;
      Is_Volatile    : Boolean := False;
      Is_Stack_Align : Boolean := False) return GL_Value;

   function Block_Address
     (Func : GL_Value; BB : Basic_Block_T) return GL_Value
   is
      (G (Block_Address (LLVM_Value (Func), BB), A_Char_GL_Type))
     with Pre  => Present (Func) and then Present (BB),
          Post => Present (Block_Address'Result);

   function Build_Switch
     (V : GL_Value; Default : Basic_Block_T; Blocks : Nat := 15) return Value_T
   is
     (Build_Switch (IR_Builder, LLVM_Value (V), Default, unsigned (Blocks)))
     with Pre  => Present (V) and then Present (Default),
          Post => Present (Build_Switch'Result);

   function Get_Type_Size (V : GL_Value) return GL_Value
     with Pre => Present (V), Post => Present (Get_Type_Size'Result);

   function Get_Type_Size (V : GL_Value) return ULL
     with Pre => Present (V);

   function Get_Scalar_Bit_Size (V : GL_Value) return ULL
     with Pre => Present (V);

   function Get_Type_Alignment (V : GL_Value) return Nat
     with Pre => Present (V);

   function Get_Type_Alignment (GT : GL_Type) return GL_Value
     with Pre  => Present (GT),
          Post => Type_Of (Get_Type_Alignment'Result) = LLVM_Size_Type;

   function Add_Function
     (Name : String; T : Type_T; Return_GT : GL_Type) return GL_Value
   is
     (G (Add_Function (Module, Name, T),
         Return_GT, Reference_To_Subprogram))
     with Pre  => Present (T) and then Present (Return_GT),
          Post => Present (Add_Function'Result);
   --  Add a function to the environment

   function Add_Global
     (GT             : GL_Type;
      Name           : String;
      Need_Reference : Boolean := False) return GL_Value
     with Pre  => Present (GT), Post => Present (Add_Global'Result);
     --  Add a global to the environment which is of type TE, so the global
     --  itself represents the address of TE.

   function Get_Param
     (Func      : GL_Value;
      Param_Num : Nat;
      GT        : GL_Type;
      R         : GL_Relationship) return GL_Value
   is
     (G (Get_Param (LLVM_Value (Func), unsigned (Param_Num)), GT, R))
     with Pre  => Present (Func) and then Present (GT),
          Post => Present (Get_Param'Result);

   function Get_Stack_Alignment return Nat is
     (Nat (Get_Stack_Alignment (Module_Data_Layout)));

   function Get_Insert_Block return Basic_Block_T is
     (Get_Insert_Block (IR_Builder))
     with Post => Present (Get_Insert_Block'Result);

   function Does_Not_Throw (V : GL_Value) return Boolean is
     (Does_Not_Throw (LLVM_Value (V)))
     with Pre => Present (V);
   --  Return True if V is a function

   procedure Set_Does_Not_Throw (V : GL_Value)
     with Pre => Present (V);
   --  Indicate that V does not throw exceptions

   procedure Set_Does_Not_Return (V : GL_Value)
     with Pre => Present (V);
   --  Indicate that V does not return

   procedure Set_Initializer (V, Expr : GL_Value)
     with Pre => Present (V) and then Present (Expr);
   --  Set the initializer for a global variable

   function Get_Linkage (V : GL_Value) return Linkage_T is
     (Get_Linkage (LLVM_Value (V)))
     with Pre => Is_A_Global_Variable (V) or else Is_A_Function (V);
   --  Get the linkage type for a variable or function

   procedure Set_Linkage (V : GL_Value; Linkage : Linkage_T)
     with Pre => Is_A_Global_Variable (V) or else Is_A_Function (V);
   --  Set the linkage type for a variable or function

   procedure Set_Global_Constant (V : GL_Value; B : Boolean := True)
     with Pre => Is_A_Global_Variable (V);

   procedure Set_Thread_Local (V : GL_Value; Thread_Local : Boolean := True)
     with Pre => Is_A_Global_Variable (V);

   procedure Set_Section (V : GL_Value; S : String)
     with Pre => Is_A_Global_Variable (V) or else Is_A_Function (V);

   procedure Set_Unnamed_Addr
     (V : GL_Value; Has_Unnamed_Addr : Boolean := True)
     with Pre => Is_A_Global_Variable (V);

   function Is_Layout_Identical (V : GL_Value; GT : GL_Type) return Boolean
     with Pre => Present (V) and then Present (GT);

   function Is_Layout_Identical (GT1, GT2 : GL_Type) return Boolean
     with Pre => Present (GT1) and then Present (GT2);

   function Convert_Struct_Constant
     (V : GL_Value; GT : GL_Type) return GL_Value
     with Pre  => Present (V) and then Present (GT),
          Post => Present (Convert_Struct_Constant'Result);
   --  Convert V, a constant of a struct type, to TE

   function Idxs_To_GL_Values (Idxs : Index_Array) return GL_Value_Array;
   --  Convert an array of integer indices into the corresponding constant
   --  GL_Values (of Size_Type).

   function Get_Alloca_Name
     (Def_Ident : Entity_Id; Name : String) return String;
   --  Get name to be used for an alloc instruction

   procedure Error_Msg_NE_Num
     (Msg : String; N : Node_Id; E : Entity_Id; V : GL_Value)
     with Pre => Msg'Length > 0 and then Present (N) and then Present (E)
                 and then Present (V);
end GNATLLVM.GLValue;
