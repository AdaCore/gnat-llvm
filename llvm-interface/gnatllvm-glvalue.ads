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

with Ada.Unchecked_Deallocation;

with Einfo.Utils; use Einfo.Utils;
with Sem_Util;    use Sem_Util; use Sem_Util.Storage_Model_Support;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

package GNATLLVM.GLValue is

   --  It's not sufficient to just pass around an LLVM Value_T when
   --  generating code because there's a lot of information lost about the
   --  value and where it came from. We construct a record of type
   --  GL_Value, which contains the LLVM Value_T (which in turn contains
   --  its LLVM Type_T), a GNAT type to which it is related, and a field
   --  indicating the relationship between the value and the type. For
   --  example, the value may contain bits of the type or the value may be
   --  the address of the bits of the type.

   type GL_Relationship is
     (Data,
      --  Value is actual bits of Typ. This can never be set for subprogram
      --  types or for types of variable size. It can be set for
      --  non-first-class types in the LLVM sense as long as LLVM can
      --  represent a value of that object. If Typ is an access type, this
      --  is equivalent to a relationship of Reference to the
      --  Designated_Type of Typ.

      Boolean_Data,
      --  Like Data, but this is an actual LLVM boolean value (i1) instead
      --  of the normal i8 that we'd use for a Boolean type. In this case,
      --  the type must be Standard_Boolean.

      Boolean_And_Data,
      --  Like Data, but a struct consisting of the data followed by an
      --  i1 item that indicates something about the result, such as
      --  whether it represents an overflow or not.

      Reference,
      --  Value contains the address of an object of Typ. This is always
      --  the case for types of variable size or for names corresponding to
      --  globals because those names represent the address of the global,
      --  either for data or functions.

      Reference_To_Reference,
      --  Value contains the address of memory that contains the address of
      --  an object of Typ. This occurs for globals where either an
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
      --  them). Also similar to Reference_To_Bounds_And_Data, except for
      --  exactly where the pointer references.

      Reference_To_Thin_Pointer,
      --  Similar to Reference_To_Reference, except that the underlying
      --  object is an aliased object with a nominal constrained type.

      Reference_To_Subprogram,
      --  Value contains the address of a subprogram which is a procedure
      --  if Typ is an E_Void or which is a function returning type Typ if
      --  Typ is not a Void. If Typ is a subprogram type, then Reference
      --  should be used instead and if Typ is an access to subprogram
      --  type, then Data is the appropriate relationship.

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
      --  A pointer to a piece of code that can be called. This can either
      --  be the subprogram itself or a fragment on the stack that can be
      --  called and encapsulates both the address of the subprogram and
      --  the address of the static link.

      Unknown,
      --  Object is an unknown relation to the type. Used for peculiar LLVM
      --  objects such as landing pads, the structure representing the
      --  return from a function, and fields that represent multiple
      --  bitfields.

      Reference_To_Unknown,
      --  Similar to Unknown, but we know that this is a reference and a
      --  dereference to it will be Unknown.

      Any_Reference,
      --  Valid only as an operand to Get and indicates that a value with
      --  any reference to data can be returned. This includes fat and
      --  thin pointers, but not such things as references to bounds
      --  or references to references.

      Reference_For_Integer,
      --  Valid only as an operand to Get and indicates that a value with a
      --  single-word reference to data can be returned. This includes thin
      --  pointers, but not such things as references to bounds or any fat
      --  structure. This is used when we want to compare two access types
      --  or convert an address to an integer.

      Object,
      --  Valid only as an operand to Get and means Any_Reference if
      --  the type of the value is of dynamic size and Data otherwise.

      Invalid);
      --  This is invalid relationship, which will result from, e.g.,
      --  doing a dereference operation on something that isn't a reference.

   --  We define some properties on each relationship type so we can do
   --  some reasoning on them. This record and array are used to express
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
      Boolean_And_Data               =>
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
   function Can_Deref (R : GL_Relationship) return Boolean is
     (Deref (R) /= Invalid);

   function Is_Reference (R : GL_Relationship)            return Boolean is
     (Relation_Props (R).Is_Ref);
   function Is_Any_Reference (R : GL_Relationship)        return Boolean is
     (Relation_Props (R).Is_Any_Ref);
   function Is_Double_Reference (R : GL_Relationship)     return Boolean is
     (Is_Reference (Deref (R)));
   function Is_Single_Reference (R : GL_Relationship)     return Boolean is
     (Is_Reference (R) and then not Is_Double_Reference (R));

   function Contains_Bounds (R : GL_Relationship)         return Boolean is
     (R in Fat_Pointer | Bounds | Bounds_And_Data | Reference_To_Bounds |
           Reference_To_Bounds_And_Data | Thin_Pointer);

   function Is_Subprogram_Reference (R : GL_Relationship) return Boolean is
     (R = Reference_To_Subprogram);

   function Is_Data (R : GL_Relationship)                 return Boolean is
     (R in Data | Boolean_Data | Boolean_And_Data | Bounds_And_Data);

   function Relationship_For_Ref (GT : GL_Type) return GL_Relationship
     with Pre => Present (GT), Inline;
   --  Return the relationship to use for a reference to GT

   function Relationship_For_Ref (TE : Type_Kind_Id) return GL_Relationship
     with Inline;
   --  Return the relationship to use for a reference to TE

   function Relationship_For_Access_Type (GT : GL_Type) return GL_Relationship
     with Pre => Present (GT), Inline;
   --  Given an access type, return the Relationship that a value of this
   --  type would have with its Designated_Type. Similar to
   --  Relationship_For_Ref on the Designated_Type of GT, but takes into
   --  account anything special about TE, such as its size.

   function Relationship_For_Access_Type
     (TE : Access_Kind_Id) return GL_Relationship with Inline;
   --  Given an access type, return the Relationship that a value of this
   --  type would have with its Designated_Type. Similar to
   --  Relationship_For_Ref on the Designated_Type of TE, but takes into
   --  account anything special about TE, such as its size.

   function Relationship_For_Alloc (TE : Type_Kind_Id) return GL_Relationship
     with Inline;
   --  Return the relationship to TE that allocating memory for TE produces.
   --  Similar to Relationship_For_Ref, but take into account the need to
   --  also allocate space for bounds in some situations.

   function Relationship_For_Alloc (GT : GL_Type) return GL_Relationship
     with Pre => Present (GT), Inline;
   --  Return the relationship to GL that allocating memory for GL produces.
   --  Similar to Relationship_For_Ref, but take into account the need to
   --  also allocate space for bounds in some situations.

   function Type_For_Relationship
     (GT : GL_Type; R : GL_Relationship) return Type_T
     with Post => Present (Type_For_Relationship'Result);
   --  Return the LLVM type corresponding to a value of relationship R to GT.
   --  If this is a kind of relationship where we don't need a GT, it may
   --  be omitted. This applies to subprogram types.

   type GL_Value_Base is record
      Value                : Value_T;
      --  The LLVM value that was generated

      GT                   : GL_Type;
      --  The GL_Type of this value, which points to the GNAT and LLVM types

      Relationship         : GL_Relationship;
      --  The relationship between Value and GT

      Alignment            : Nat;
      --  The maximum alignment, expressed in bits, and no larger than the
      --  largest supported alignment, that the represented value is known
      --  to have if used as an address. This corresponds to the largest
      --  power of two known to divide the value multiplied by the number
      --  of bits per unit. If we know nothing about this value, the
      --  alignment is BPU. If this value is the address of a variable, we
      --  set the alignment to that of the variables. If we have a pointer
      --  to the type, either by dereferencing an access type or as a
      --  parameter to or return value from a subprogram, we initialize
      --  this to the alignment of the type. For other operations (e.g.,
      --  arithmetic), we track the effect on the alignment.

      Is_Pristine          : Boolean;
      --  Set when this value has just been allocated and there's no chance
      --  yet of it being written. We know that no expression can conflict
      --  with it.

      Is_Volatile          : Boolean;
      --  Set when this value represents a volatile object or type

      Is_Atomic            : Boolean;
      --  Set when this value represents an atomic object or type

      Overflowed           : Boolean;
      --  Set when an arithmetic operation overflowed. This is propagated
      --  on further arithmetic or conversions. When we're ready to post an
      --  error message about the overflow, this will be converted to an
      --  undef.

      Aliases_All          : Boolean;
      --  Set when this GL_Value is allowed to alias anything. This is used
      --  to implement the No_Strict_Aliasing pragma on an access type.  In
      --  that case, all references to objects of that type can alias
      --  anything. We need a separate flag here rather than relying on no
      --  TBAA_Type because without this flag, we might deduce some
      --  type-based TBAA as we manipulate this value.

      SM_Object            : Opt_E_Variable_Id;
      --  If this is a value of an access type that has a nonstandard
      --  Storage_Model, this is the value returned by Storage_Model_Object.

      TBAA_Type            : Metadata_T;
      --  The TBAA node representing the type that this value, treated as
      --  an address, points to (for a scalar) or points into (for an
      --  aggregate).

      TBAA_Offset          : ULL;
      --  The offset in bytes that this value, treated as an address, points
      --  to from the start of the type given by TBAA_Type (always zero
      --  if a scalar type). If TBAA_Type isn't Present, this value is
      --  undefined.

      Unknown_T            : Type_T;
      --  If Relationship is Reference_To_Unknown, this gives the
      --  LLVM type of the object being referenced.

   end record;
   --  We want to put a Predicate on this, but can't, so we need to make
   --  a subtype for that purpose.

   function "=" (V1, V2 : GL_Value_Base) return Boolean is
     (V1.Value = V2.Value and then V1.GT = V2.GT);
   --  Two GL_Types are the same if their LLVM values and GL_types are
   --  the same. We don't want to compare flags.

   function GL_Value_Is_Valid (V : GL_Value_Base) return Boolean;
   --  Return whether V is a valid GL_Value or not

   subtype GL_Value is GL_Value_Base
     with Predicate => GL_Value_Is_Valid (GL_Value);
   --  Subtype used by everybody except validation function

   function "<" (LHS : GL_Value; RHS : Int) return Boolean
     with Pre => Present (LHS), Inline;
   function "<=" (LHS : GL_Value; RHS : Int) return Boolean
     with Pre => Present (LHS), Inline;
   function ">" (LHS : GL_Value; RHS : Int) return Boolean
     with Pre => Present (LHS), Inline;
   function ">=" (LHS : GL_Value; RHS : Int) return Boolean
     with Pre => Present (LHS), Inline;
   function "=" (LHS : GL_Value; RHS : Int) return Boolean
     with Pre => Present (LHS), Inline;

   type GL_Value_Array is array (Nat range <>) of GL_Value;
   type Access_GL_Value_Array is access all GL_Value_Array;
   procedure Free is new Ada.Unchecked_Deallocation (GL_Value_Array,
                                                     Access_GL_Value_Array);

   No_GL_Value : constant GL_Value :=
     (Value        => No_Value_T,
      GT           => No_GL_Type,
      Relationship => Data,
      Alignment    => BPU,
      Is_Pristine  => False,
      Is_Volatile  => False,
      Is_Atomic    => False,
      Overflowed   => False,
      Aliases_All  => False,
      SM_Object    => Empty,
      TBAA_Type    => No_Metadata_T,
      TBAA_Offset  => 0,
      Unknown_T    => No_Type_T);

   function Present (V : GL_Value) return Boolean      is (Present (V.Value));
   function No      (V : GL_Value) return Boolean      is (No      (V.Value));

   procedure Discard (V : GL_Value) is null;

   --  Define basic accessors for components of GL_Value

   function "+" (V : GL_Value)    return Value_T is
     (V.Value)
     with Pre => Present (V), Post => Present ("+"'Result);
   --  Return the LLVM value in the GL_Value

   function Related_Type (V : GL_Value)  return GL_Type is
     (V.GT)
     with Pre => Present (V), Post => Present (Related_Type'Result);
   --  Return the GL_Type to which V is related, irrespective of the
   --  relationship.

   function Relationship (V : GL_Value)  return GL_Relationship is
     (V.Relationship)
     with Pre => Present (V);

   function Deref (V : GL_Value) return GL_Relationship is
     (Deref (Relationship (V)))
     with Pre => Present (V);

   function Ref (V : GL_Value)   return GL_Relationship is
     (Ref (Relationship (V)))
     with Pre => Present (V);

   function Alignment    (V : GL_Value)  return Nat        is (V.Alignment)
     with Pre => Present (V);

   function Is_Pristine  (V : GL_Value)  return Boolean    is (V.Is_Pristine)
     with Pre => Present (V);

   function Is_Volatile  (V : GL_Value)  return Boolean    is (V.Is_Volatile)
     with Pre => Present (V);

   function Is_Atomic    (V : GL_Value)  return Boolean    is (V.Is_Atomic)
     with Pre => Present (V);

   function Overflowed   (V : GL_Value)  return Boolean    is (V.Overflowed)
     with Pre => Present (V);

   function Aliases_All  (V : GL_Value)  return Boolean    is (V.Aliases_All)
     with Pre => Present (V);

   function SM_Object    (V : GL_Value)  return Entity_Id  is (V.SM_Object)
     with Pre => Present (V);

   function TBAA_Type    (V : GL_Value)  return Metadata_T is (V.TBAA_Type)
     with Pre => Present (V);

   function TBAA_Offset  (V : GL_Value)  return ULL        is (V.TBAA_Offset)
     with Pre => Present (V);

   function Unknown_T    (V : GL_Value)  return Type_T     is (V.Unknown_T)
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

   function Can_Deref (V : GL_Value) return Boolean is
     (Deref (V) /= Invalid)
     with Pre => Present (V);

   function Full_Etype (V : GL_Value)              return Void_Or_Type_Kind_Id
     with Pre  => Present (V), Inline;

   --  Functions and predicates related to Storage_Model support

   function Has_Storage_Model (V : GL_Value) return Boolean is
     (Present (SM_Object (V)))
     with Pre => Present (V);
   --  Return True if V has a storage model specified

   function SM_Copy_From (V : GL_Value) return Opt_E_Procedure_Id is
     (Storage_Model_Copy_From (SM_Object (V)))
     with Pre => Has_Storage_Model (V);

   function SM_Copy_To (V : GL_Value) return Opt_E_Procedure_Id is
     (Storage_Model_Copy_To (SM_Object (V)))
     with Pre => Has_Storage_Model (V);

   function SM_Address_Type (V : GL_Value) return Type_Kind_Id is
     (Get_Fullest_View (Storage_Model_Address_Type (SM_Object (V))))
     with Pre => Has_Storage_Model (V);

   function Has_SM_Copy_From (V : GL_Value) return Boolean is
     (Has_Storage_Model (V) and then Present (SM_Copy_From (V)))
      with Pre => Present (V);
   function Has_SM_Copy_To (V : GL_Value) return Boolean is
     (Has_Storage_Model (V) and then Present (SM_Copy_To (V)))
      with Pre => Present (V);

   --  Constructors for a GL_Value

   function G
     (V           : Value_T;
      GT          : GL_Type;
      R           : GL_Relationship   := Data;
      Alignment   : Nat               := BPU;
      Is_Pristine : Boolean           := False;
      Is_Volatile : Boolean           := False;
      Is_Atomic   : Boolean           := False;
      Overflowed  : Boolean           := False;
      Aliases_All : Boolean           := False;
      SM_Object   : Opt_E_Variable_Id := Empty;
      TBAA_Type   : Metadata_T        := No_Metadata_T;
      TBAA_Offset : ULL               := 0;
      Unknown_T   : Type_T            := No_Type_T) return GL_Value
     with Pre => Present (V) and then Present (GT), Inline;
   --  Raw constructor that allows full specification of all fields

   function GM
     (V  : Value_T;
      GT : GL_Type;
      R  : GL_Relationship := Data;
      GV : GL_Value) return GL_Value is
     (G (V, GT, R,
         Alignment   => Alignment   (GV),
         Is_Pristine => Is_Pristine (GV),
         Is_Volatile => Is_Volatile (GV),
         Is_Atomic   => Is_Atomic   (GV),
         Overflowed  => Overflowed  (GV),
         Aliases_All => Aliases_All (GV),
         SM_Object   => SM_Object   (GV),
         TBAA_Type   => TBAA_Type   (GV),
         TBAA_Offset => TBAA_Offset (GV),
         Unknown_T   => Unknown_T   (GV)))
     with Pre  => Present (V) and then Present (GT) and then Present (GV),
          Post => Present (GM'Result);
   --  Likewise, but copy all but type and relationship from an existing value

   function G_From (V : Value_T; GV : GL_Value) return GL_Value is
     (GM (V, Related_Type (GV), Relationship (GV), GV))
     with Pre  => Present (V) and then Present (GV),
          Post => Present (G_From'Result);
   --  Constructor for most common operation cases where we aren't changing
   --  any typing information, so we just copy it from an existing value.

   function G_Is (V : GL_Value; GT : GL_Type) return GL_Value is
     (GM (+V, GT, Relationship (V), V))
     with Pre  => Present (V) and then Present (GT),
          Post => Present (G_Is'Result);
   --  Constructor for case where we want to show that V has a different type

   function G_Is_Ref (V : GL_Value; GT : GL_Type) return GL_Value is
     (GM (+V, GT, Ref (Relationship (V)), V))
     with Pre  => Present (V) and then Present (GT),
          Post => Present (G_Is_Ref'Result);
   --  Constructor for case where we want to show that V has a different type

   function G_Is (V : GL_Value; T : GL_Value) return GL_Value is
     (GM (+V, Related_Type (T), Relationship (V), V))
     with Pre  => Present (V) and then Present (T),
          Post => Present (G_Is'Result);

   function G_Is_Relationship
     (V : GL_Value; GT : GL_Type; R : GL_Relationship) return GL_Value
   is
     (GM (+V, GT, R, V))
     with Pre  => Present (V) and then Present (GT),
          Post => Present (G_Is_Relationship'Result);
   --  Constructor for case where we want to show that V has a different type
   --  and relationship.

   function G_Is_Relationship
     (V : GL_Value; T : GL_Value; R : GL_Relationship) return GL_Value
   is
     (GM (+V, Related_Type (T), R, V))
     with Pre  => Present (V) and then Present (T),
          Post => Present (G_Is_Relationship'Result);
   --  Constructor for case where we want to show that V has a different type
   --  and relationship.

   function G_Is_Relationship
     (V : GL_Value; R : GL_Relationship) return GL_Value
   is
     (GM (+V, Related_Type (V), R, V))
     with Pre => Present (V), Post => Present (G_Is_Relationship'Result);
   --  Constructor for case where we want to show that V has a
   --  different relationship.

   function G_Is_Relationship (V : GL_Value; T : GL_Value) return GL_Value is
      (GM (+V, Related_Type (T), Relationship (T), V))
     with Pre  => Present (V) and then Present (T),
          Post => Present (G_Is_Relationship'Result);
   --  Constructor for case where we want to show that V has a different type
   --  and relationship.

   function G_Ref
     (V           : Value_T;
      GT          : GL_Type;
      Alignment   : Nat               := BPU;
      Is_Pristine : Boolean           := False;
      Is_Volatile : Boolean           := False;
      Is_Atomic   : Boolean           := False;
      Overflowed  : Boolean           := False;
      Aliases_All : Boolean           := False;
      SM_Object   : Opt_E_Variable_Id := Empty;
      TBAA_Type   : Metadata_T        := No_Metadata_T;
      TBAA_Offset : ULL               := 0;
      Unknown_T   : Type_T            := No_Type_T) return GL_Value
   is
     (G (V, GT, Relationship_For_Ref (GT),
         Alignment   => Alignment,
         Is_Pristine => Is_Pristine,
         Is_Volatile => Is_Volatile,
         Is_Atomic   => Is_Atomic,
         Overflowed  => Overflowed,
         Aliases_All => Aliases_All,
         SM_Object   => SM_Object,
         TBAA_Type   => TBAA_Type,
         TBAA_Offset => TBAA_Offset,
         Unknown_T   => Unknown_T))
     with Pre  => Present (V) and then Present (GT),
          Post => Is_Reference (G_Ref'Result);
   --  Constructor for case where we create a value that's a pointer
   --  to type GT.

   function GM_Ref
     (V : Value_T; GT : GL_Type; GV : GL_Value) return GL_Value
   is
     (G_Ref (V, GT,
             Alignment   => Alignment   (GV),
             Is_Pristine => Is_Pristine (GV),
             Is_Volatile => Is_Volatile (GV),
             Is_Atomic   => Is_Atomic   (GV),
             Overflowed  => Overflowed  (GV),
             Aliases_All => Aliases_All (GV),
             SM_Object   => SM_Object   (GV),
             TBAA_Type   => TBAA_Type   (GV),
             TBAA_Offset => TBAA_Offset (GV),
             Unknown_T   => Unknown_T   (GV)))
     with Pre  => Present (V) and then Present (GT) and then Present (GV),
          Post => Is_Reference (GM_Ref'Result);
   --  Likewise, but copy the rest of the attributes from GV

   procedure Set_Alignment (V : in out GL_Value; Align : Nat)
     with Pre => Present (V), Inline;
   function Set_Alignment (V : GL_Value; Align : Nat) return GL_Value
     with Pre => Present (V), Inline;
   --  Set the alignment of V to the specified value, possibly restricting
   --  it to a smaller range.

   procedure Clear_Alignment (V : in out GL_Value)
     with Pre => Present (V), Post => Alignment (V) = BPU, Inline;
   function Clear_Alignment (V : GL_Value) return GL_Value
     with Pre => Present (V), Post => Alignment (Clear_Alignment'Result) = BPU,
          Inline;
   --  Show that we know nothing about the alignment of V

   procedure Initialize_Alignment (V : in out GL_Value)
     with Pre => Present (V);
   function Initialize_Alignment (V : GL_Value) return GL_Value
     with Pre => Present (V);
   --  V is a value that we know nothing about except for its type. If it's
   --  data, we have no idea of its alignment, but if it's a reference or
   --  double reference, we know at least a minimum alignment, from either
   --  the type or the alignment of a pointer to the type.

   procedure Set_SM_Object (V : in out GL_Value; TE : E_Variable_Id)
     with Pre => Present (V), Inline;
   function Set_SM_Object (V : GL_Value; TE : E_Variable_Id) return GL_Value
     with Pre => Present (V), Inline;
   --  Set the Storage_Module object for V

   procedure Not_Pristine (V : in out GL_Value)
     with Pre => Present (V), Post => not Is_Pristine (V),
          Inline;
   --  Clear the Is_Pristine flag from V

   procedure Mark_Volatile (V : in out GL_Value; Flag : Boolean := True)
     with Pre => Present (V), Post => not Flag or else Is_Volatile (V), Inline;
   --  If Flag is true, mark V as volatile

   procedure Mark_Atomic (V : in out GL_Value; Flag : Boolean := True)
     with Pre => Present (V), Post => not Flag or else Is_Atomic (V), Inline;
   --  If Flag is true, mark V as atomic

   procedure Mark_Overflowed (V : in out GL_Value; Flag : Boolean := True)
     with Pre => Present (V), Post => not Flag or else Overflowed (V), Inline;
   --  If Flag is true, mark V as overflowed

   function Mark_Overflowed
     (V : GL_Value; Flag : Boolean := True) return GL_Value
     with Pre  => Present (V),
          Post => not Flag or else Overflowed (Mark_Overflowed'Result), Inline;
   --  Likewise, but return a copy and mark as overflowed

   procedure Clear_Overflowed (V : in out GL_Value)
     with Pre => Present (V), Post => not Overflowed (V), Inline;
   --  Clear the overflow flag in V

   procedure Set_Aliases_All (V : in out GL_Value; AA : Boolean := True)
     with Pre => Present (V), Post => not AA or else Aliases_All (V), Inline;

   procedure Set_Unknown_T   (V : in out GL_Value; T : Type_T)
     with Pre => Present (V), Post => Unknown_T (V) = T, Inline;

   procedure Set_TBAA_Type (V : in out GL_Value; MD : Metadata_T)
     with Pre => Present (V), Post => TBAA_Type (V) = MD, inline;
   procedure Set_TBAA_Offset (V : in out GL_Value; Offset : ULL)
     with Pre => Present (V), Post => TBAA_Offset (V) = Offset, Inline;
   function Set_TBAA_Type (V : GL_Value; MD : Metadata_T) return GL_Value
     with Pre => Present (V), Post => TBAA_Type (Set_TBAA_Type'Result) = MD,
          Inline;
   --  Set the TBAA type and offset of V

   procedure Set_Value (VE : Node_Id; VL : GL_Value)
     with Pre => Present (VE) and then Present (VL), Inline;
   --  Set a value for a node (usually an entity). This turns off the
   --  Is_Pristine flag.

   --  Now define predicates on the GL_Value type to easily access
   --  properties of the LLVM value and the effective type. These have the
   --  same names as those for types and Value_T's. The first of these
   --  represent abstractions that will be used in later predicates.

   function Type_Of (V : GL_Value) return Type_T is
     (Type_Of (+V))
     with Pre => Present (V), Post => Present (Type_Of'Result);

   function Element_Type_Of (V : GL_Value) return Type_T
     with Pre => Is_Double_Reference (V) or else Is_Access_Type (V)
                 or else Relationship (V) = Thin_Pointer or else Can_Deref (V);

   function Data_Type_Of (V : GL_Value) return Type_T is
     ((if Is_Reference (V) then Element_Type_Of (V) else Type_Of (V)))
     with Pre => Present (V), Post => Present (Data_Type_Of'Result);

   function Get_Type_Kind (V : GL_Value) return Type_Kind_T is
     (Get_Type_Kind (Type_Of (V)))
     with Pre => Present (V);

   function Ekind (V : GL_Value) return Entity_Kind is
     (Ekind (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Access_Type (V : GL_Value) return Boolean is
     (Is_Access_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Pointer (V : GL_Value) return Boolean is
     (Is_Reference (V)
      or else (not Is_Reference (V) and then Is_Access_Type ((V))))
     with Pre => Present (V);

   function Full_Designated_Type (V : GL_Value) return Void_Or_Type_Kind_Id
     with Pre  => Is_Access_Type (V) and then not Is_Subprogram_Reference (V);

   function Full_Base_Type (V : GL_Value) return Type_Kind_Id
     with Pre => Present (V), Inline;

   function Is_Dynamic_Size (V : GL_Value) return Boolean
     with Pre => Present (V), Inline;

   function Is_Nonnative_Type (V : GL_Value) return Boolean
     with Pre => Present (V), Inline;

   function Is_Loadable_Type (V : GL_Value) return Boolean
     with Pre => Present (V), Inline;

   function Is_Array_Type (V : GL_Value) return Boolean is
     (not Is_Reference (V) and then Is_Array_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Access_Subprogram_Type (V : GL_Value) return Boolean is
    (Is_Access_Type (V)
       and then Ekind (Full_Designated_Type (V)) = E_Subprogram_Type)
     with Pre => Present (V);

   function Is_Descendant_Of_Address (V : GL_Value) return Boolean is
     (Is_Descendant_Of_Address (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Address (V : GL_Value) return Boolean is
     (Is_Address_Compatible_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Constrained (V : GL_Value) return Boolean is
     (Is_Constrained (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Record_Type (V : GL_Value) return Boolean is
     (Is_Record_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Composite_Type (V : GL_Value) return Boolean is
     (Is_Composite_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Aggregate_Type (V : GL_Value) return Boolean is
     (Is_Aggregate_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Elementary_Type (V : GL_Value) return Boolean is
     (Is_Elementary_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Scalar_Type (V : GL_Value) return Boolean is
     (Is_Scalar_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Discrete_Type (V : GL_Value) return Boolean is
     (Is_Discrete_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Integer_Type (V : GL_Value) return Boolean is
     (Is_Integer_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Boolean_Type (V : GL_Value) return Boolean is
     (Is_Boolean_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Fixed_Point_Type (V : GL_Value) return Boolean is
     (Is_Fixed_Point_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Floating_Point_Type (V : GL_Value) return Boolean is
     (Is_Floating_Point_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Unsigned_Type (V : GL_Value) return Boolean
     with Pre => Present (V);

   function Is_Discrete_Or_Fixed_Point_Type (V : GL_Value) return Boolean is
     (Is_Discrete_Or_Fixed_Point_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Modular_Integer_Type (V : GL_Value) return Boolean is
     (Is_Modular_Integer_Type (Full_Etype (V)))
     with Pre => Present (V);

   function Is_Unconstrained_Record (V : GL_Value) return Boolean
     with Pre => Present (V), Inline;

   function Is_Unconstrained_Array (V : GL_Value) return Boolean
     with Pre => Present (V), Inline;

   function Is_Constrained_Array (V : GL_Value) return Boolean
     with Pre => Present (V), Inline;

   function Is_Unconstrained_Type (V : GL_Value) return Boolean
     with Pre => Present (V), Inline;

   function Is_Access_Unconstrained_Array (V : GL_Value) return Boolean
     with Pre => Present (V), Inline;

   function Is_Packed_Array_Impl_Type (V : GL_Value) return Boolean
     with Pre => Present (V), Inline;

   function Is_Bit_Packed_Array_Impl_Type (V : GL_Value) return Boolean
     with Pre => Present (V), Inline;

   function Is_Constr_Array_Subt_With_Bounds (V : GL_Value) return Boolean
     with Pre => Present (V), Inline;

   function Type_Needs_Bounds (V : GL_Value) return Boolean
     with Pre => Present (V), Inline;

   function RM_Size (V : GL_Value) return Uint is
     (RM_Size (Full_Etype (V)))
     with Pre => Present (V);

   function Esize (V : GL_Value) return Uint is
     (Esize (Full_Etype (V)))
     with Pre => Present (V);

   function Component_Type (V : GL_Value) return Type_Kind_Id is
     (Component_Type (Full_Etype (V)))
     with Pre => Is_Array_Type (V);

   function Number_Dimensions (V : GL_Value) return Pos is
     (Number_Dimensions (Full_Etype (V)))
     with Pre => Is_Array_Type (V);

   --  Next are useful functions to manipulate GL_Values

   function Get (In_V : GL_Value; Rel : GL_Relationship) return GL_Value
     with Pre => Present (In_V), Post => Equiv_Relationship (Get'Result, Rel);
   --  Produce a GL_Value from In_V whose relationship to its type is given
   --  by Rel.

   function To_Access (V : GL_Value; GT : GL_Type) return GL_Value
     with Pre  => Present (GT) and then Is_Reference (V),
          Post => Relationship (To_Access'Result) = Data
                  and then Related_Type (To_Access'Result) = GT, Inline;
   --  V is a reference to an object whose type is the designated type of
   --  GT. Convert it to being viewed as an object of type GT.

   function From_Access (V : GL_Value) return GL_Value
     with Pre  => Is_Data (V) and then Is_Access_Type (Full_Etype (V)),
          Post => Is_Reference (From_Access'Result), Inline;
   --  V is a value of an access type. Instead, represent it as a reference
   --  to the designated type of that access type.

   function Equiv_Relationship
     (V : GL_Value; Rel : GL_Relationship) return Boolean
     with Pre => Present (V), Inline;
   --  Return True if V has relationship Rel or one that can be returned
   --  by a call to Get with Rel as an operand.

   --  Finally, we have versions of subprograms defined elsewhere that
   --  accept and/or return GL_Value.

   function Get_Value_Kind (V : GL_Value) return Value_Kind_T is
     (Get_Value_Kind (+V))
     with Pre => Present (V);

   function Get_Function_Type (V : GL_Value) return Type_T is
     (Get_Function_Type (+V))
     with Pre => Is_A_Function (V);

   function Get_Source_Element_Type (V : GL_Value) return Type_T is
     (Get_Source_Element_Type (+V))
     with Pre => Present (V);

   function Is_A_Global_Variable (V : GL_Value) return Boolean is
     (Present (Is_A_Global_Variable (+V)))
     with Pre => Present (V);

   function Is_Global_Constant (V : GL_Value) return Boolean is
     (Is_Global_Constant (+V))
     with Pre => Is_A_Global_Variable (V);

   function Global_Get_Value_Type (V : GL_Value) return Type_T is
     (Global_Get_Value_Type (+V))
     with Pre => Is_A_Global_Variable (V);

   function Is_A_Function (V : GL_Value) return Boolean is
     (Present (Is_A_Function (+V)))
     with Pre => Present (V);

   function Is_A_Alloca_Inst (V : GL_Value) return Boolean is
     (Present (Is_A_Alloca_Inst (+V)))
     with Pre => Present (V);

   function Is_A_Get_Element_Ptr_Inst (V : GL_Value) return Boolean is
     (Present (Is_A_Get_Element_Ptr_Inst (+V)))
     with Pre => Present (V);

   function Is_A_GEP (V : GL_Value) return Boolean is
     ((Present (Is_A_Instruction (+V))
       and then Get_Instruction_Opcode (+V) = Op_Get_Element_Ptr)
      or else (Present (Is_A_Constant_Expr (+V))
               and then Get_Const_Opcode (+V) = Op_Get_Element_Ptr))
     with Pre => Present (V);

   function Is_Undef (V : GL_Value) return Boolean is
     (Is_Undef (+V))
     with Pre => Present (V);

   function Is_Constant (V : GL_Value) return Boolean is
     (Is_Constant (+V))
     with Pre => Present (V);

   function Is_Nonsymbolic_Constant (V : GL_Value) return Boolean
     with Pre => Present (V);

   function Is_A_Constant_Int (V : GL_Value) return Boolean is
     (Present (Is_A_Constant_Int (+V)))
     with Pre => Present (V);
   --  Return True if V is a constant integer

   function Is_A_Constant_FP (V : GL_Value) return Boolean is
     (Present (Is_A_Constant_FP (+V)))
     with Pre => Present (V);
   --  Return True if V is a constant floating point value

   function Is_A_Constant_Aggregate_Zero (V : GL_Value) return Boolean is
     (Present (Is_A_Constant_Aggregate_Zero (+V)))
     with Pre => Present (V);
   --  Return True if V is a constant integer

   function Is_A_Constant_Pointer_Null (V : GL_Value) return Boolean is
     (Present (Is_A_Constant_Pointer_Null (+V)))
     with Pre => Present (V);
   --  Return True if V is a null pointer

   function Get_Const_Int_Value (V : GL_Value) return LLI is
     (LLI (Const_Int_Get_S_Ext_Value (+V)))
     with Pre => Is_A_Constant_Int (V);
   --  V is a constant integer; get its value

   function Get_Const_Int_Value_ULL (V : GL_Value) return ULL is
     (ULL (Const_Int_Get_S_Ext_Value (+V)))
     with Pre => Is_A_Constant_Int (V);

   function Get_Const_Int_Value_Nat (V : GL_Value) return Nat is
     (Nat (Const_Int_Get_S_Ext_Value (+V)))
     with Pre => Is_A_Constant_Int (V);

   function "+" (V : GL_Value) return LLI is
     (Get_Const_Int_Value (V));
   function "+" (V : GL_Value) return Nat is
     (Get_Const_Int_Value_Nat (V));
   function "+" (V : GL_Value) return ULL is
     (Get_Const_Int_Value_ULL (V));

   function UI_From_GL_Value (V : GL_Value) return Uint is
     (UI_From_LLI (+V))
     with Pre => Is_A_Constant_Int (V);
   function "+" (V : GL_Value) return Uint renames UI_From_GL_Value;

   function Get_Value_Name (V : GL_Value) return String is
     (Get_Value_Name (+V))
     with Pre => Present (V);

   function Get_Num_Operands (V : GL_Value) return Nat is
     (Int (Get_Num_Operands (+V)))
     with Pre => Present (V);

   function Get_Operand (V : GL_Value; Idx : Nat) return Value_T is
     (Get_Operand (+V, unsigned (Idx)))
     with Pre => Present (V);

   procedure Set_Value_Name (V : GL_Value; Name : String)
     with Pre => Present (V), Inline;

   function Has_Inline_Attribute (V : GL_Value) return Boolean is
     (Has_Inline_Attribute (+V))
      with Pre => Present (V);

   function Has_Inline_Always_Attribute (V : GL_Value) return Boolean is
     (Has_Inline_Always_Attribute (+V))
      with Pre => Present (V);

   procedure Add_Cold_Attribute (V : GL_Value)
     with Pre => Is_A_Function (V), Inline;
   --  Add the Cold attribute to function V

   procedure Add_Dereferenceable_Attribute
     (V : GL_Value; Idx : Integer; GT : GL_Type)
     with Pre => Is_A_Function (V) and then Present (GT), Inline;
   --  Add the Dereferenceable attribute to parameter with index Idx

   procedure Add_Dereferenceable_Attribute
     (V : GL_Value; GT : GL_Type)
     with Pre => Is_A_Function (V) and then Present (GT), Inline;
   --  Add the Dereferenceable attribute to return value

   procedure Add_Dereferenceable_Or_Null_Attribute
     (V : GL_Value; Idx : Integer; GT : GL_Type)
     with Pre => Is_A_Function (V) and then Present (GT), Inline;
   --  Add the Dereferenceableornull attribute to parameter with index Idx

   procedure Add_Dereferenceable_Or_Null_Attribute
     (V : GL_Value; GT : GL_Type)
     with Pre => Is_A_Function (V) and then Present (GT), Inline;
   --  Add the Dereferenceableornull attribute to return value

   procedure Add_Inline_Attribute (V : GL_Value; Subp : Subprogram_Kind_Id)
     with Pre => Is_A_Function (V), Inline;
   --  Add the appropropriate Inline attributes, if any, to the LLVM
   --  function V based on the flags in Subp.

   procedure Add_Named_Attribute (V : GL_Value; Name, Value : String)
     with Pre => Is_A_Function (V), Inline;

   procedure Add_Nest_Attribute (V : GL_Value; Idx : Integer)
     with Pre => Is_A_Function (V), Inline;
   --  Add the Nest attribute to parameter with index Idx

   procedure Add_Noalias_Attribute (V : GL_Value; Idx : Integer)
     with Pre => Is_A_Function (V), Inline;
   --  Add the Noalias attribute to parameter with index Idx

   procedure Add_Noalias_Attribute (V : GL_Value)
     with Pre => Is_A_Function (V), Inline;
   --  Add the Noalias attribute to the return value of function V

   procedure Add_Nocapture_Attribute (V : GL_Value; Idx : Integer)
     with Pre => Is_A_Function (V), Inline;
   --  Add the Nocapture attribute to parameter with index Idx

   procedure Add_Non_Null_Attribute (V : GL_Value; Idx : Integer)
     with Pre => Is_A_Function (V), Inline;
   --  Add the Nonnull attribute to parameter with index Idx

   procedure Add_Non_Null_Attribute (V : GL_Value)
     with Pre => Is_A_Function (V), Inline;
   --  Add the Nonnull attribute to parameter the return value of function V

   procedure Add_Readonly_Attribute (V : GL_Value; Idx : Integer)
     with Pre => Is_A_Function (V), Inline;
   --  Add the Readonly attribute to parameter with index Idx

   procedure Add_Readonly_Attribute (V : GL_Value)
     with Pre => Is_A_Function (V), Inline;
   --  Add the Readonly attribute to V

   procedure Add_Writeonly_Attribute (V : GL_Value; Idx : Integer)
     with Pre => Is_A_Function (V), Inline;
   --  Add the Writeonly attribute to parameter with index Idx

   procedure Add_Opt_For_Fuzzing_Attribute (V : GL_Value)
     with Pre => Is_A_Function (V), Inline;
   --  Add the OptForFuzzing attribute to function V

   procedure Add_Sanitize_Address_Attribute (V : GL_Value)
     with Pre => Is_A_Function (V), Inline;
   --  Add the SanitizeAddress attribute to function V

   procedure Add_No_Implicit_Float_Attribute (V : GL_Value)
     with Pre => Is_A_Function (V), Inline;
   --  Add the NoImplicitFloat attribute to function V

   procedure Set_DSO_Local (V : GL_Value)
     with Pre => Is_A_Function (V) or else Is_A_Global_Variable (V), Inline;
   --  Add the DSOlocal attribute to a global (variable or function)

   function Is_Const_Int_Value (V : GL_Value; Val : ULL) return Boolean is
     (Is_A_Constant_Int (V) and then not Overflowed (V)
        and then Equals_Int (+V, Val))
     with Pre => Present (V);
   --  Return True if V is a constant integer of value Val

   function Is_Const_Int_Values_Equal (V1, V2 : GL_Value) return Boolean is
     (Is_A_Constant_Int (V1) and then Is_A_Constant_Int (V2)
        and then not Overflowed (V1) and then not Overflowed (V2)
        and then Equal_Constants (+V1, +V2))
     with Pre => Present (V1) and then Present (V2);

   function Set_Arith_Attrs (Inst : Value_T; V : GL_Value) return Value_T
     with Pre  => Present (Inst) and then Present (V),
          Post => Set_Arith_Attrs'Result = Inst, Inline;
   --  Set NUW and/or NSW on Inst depending on the type and relationship
   --  of V and return Inst.

   function Get_Undef (GT : GL_Type) return GL_Value
     with Pre => Present (GT), Post => Present (Get_Undef'Result), Inline;

   function Get_Undef_Ref (GT : GL_Type) return GL_Value
     with Pre => Present (GT), Post => Is_Reference (Get_Undef_Ref'Result),
          Inline;

   function Get_Undef_Relationship
     (GT : GL_Type; R : GL_Relationship) return GL_Value
   is
     (Initialize_Alignment
        (G (Get_Undef (Type_For_Relationship (GT, R)), GT, R,
            Is_Pristine => True)))
     with Pre  => Present (GT),
          Post => Present (Get_Undef_Relationship'Result);

   function Get_Undef_Fn_Ret (V : GL_Value) return GL_Value is
     (G (Get_Undef (Get_Return_Type (Get_Function_Type ((V)))),
         Related_Type (V), Unknown, Is_Pristine => True))
     with Pre => Is_A_Function (V), Post => Is_Undef (Get_Undef_Fn_Ret'Result);

   function Const_Null (GT : GL_Type) return GL_Value
     with Pre => Present (GT), Post => Present (Const_Null'Result), Inline;

   function Const_Null_Relationship
     (GT : GL_Type; R : GL_Relationship) return GL_Value
   is
     (G (Const_Null (Type_For_Relationship (GT, R)), GT, R))
     with Pre  => Present (GT),
          Post => Present (Const_Null_Relationship'Result);

   function Const_Null_Alloc (GT : GL_Type) return GL_Value
     with Pre => Present (GT), Post => Present (Const_Null_Alloc'Result),
          Inline;

   function Const_Int (GT : GL_Type; N : Uint) return GL_Value
     with Pre  => Present (GT) and then Present (N),
          Post => Present (Const_Int'Result), Inline;

   function Const_Int
     (GT : GL_Type; N : ULL; Sign_Extend : Boolean := False) return GL_Value
     with Pre  => Present (GT), Post => Present (Const_Int'Result), Inline;

   function Const_Ones (T : Type_T) return Value_T is
     (Const_Int (T, ULL'Last, Sign_Extend => True))
     with Pre => Present (T), Post => Present (Const_Ones'Result);
   --  Return an LLVM value for the given type where all bits are set

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

   function Const_Infinity (GT : GL_Type) return GL_Value
     with Pre => Present (GT);

   function Const_Infinity (V : GL_Value) return GL_Value is
     (Const_Infinity (Related_Type (V)))
     with Pre  => Present (V), Post => Present (Const_Infinity'Result);

   function Const_Int (V : GL_Value; N : Uint) return GL_Value
     with Pre  => Present (V) and then Present (N),
          Post => Present (Const_Int'Result), Inline;

   function Const_Int
     (V : GL_Value; N : ULL; Sign_Extend : Boolean := False) return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V),
          Post => Present (Const_Int'Result), Inline;

   function Const_Int
     (V           : GL_Value;
      N           : unsigned;
      Sign_Extend : Boolean := False) return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V),
          Post => Present (Const_Int'Result), Inline;

   function Const_Ones (V : GL_Value) return GL_Value
     with Pre  => Is_Discrete_Or_Fixed_Point_Type (V),
          Post => Present (Const_Ones'Result), Inline;
   --  Return an LLVM value for the given type where all bits are set

   function Const_Real
     (GT : GL_Type; V : Interfaces.C.double) return GL_Value
     with Pre  => Present (GT), Post => Present (Const_Real'Result), Inline;

   function Size_Const_Int (N : Uint) return GL_Value is
     (Const_Int (Size_GL_Type, N))
     with Pre  => Present (N), Post => Present (Size_Const_Int'Result);

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
     with Pre  => Present (N), Post => Present (Const_Int_32'Result);

   function Const_Int_32
     (N : ULL; Sign_Extend : Boolean := False) return GL_Value
   is
     (Const_Int (Int_32_GL_Type, N, Sign_Extend))
     with Post => Present (Const_Int_32'Result);

   function Const_Int_32
     (N : Nat; Sign_Extend : Boolean := False) return GL_Value
   is
     (Const_Int (Int_32_GL_Type, unsigned_long_long (N), Sign_Extend))
     with Post => Present (Const_Int_32'Result);

   function Const_Null_32 return GL_Value is
     (Const_Int_32 (ULL (0)))
     with Post => Present (Const_Null_32'Result);

   function Const_Int_64
     (N : ULL; Sign_Extend : Boolean := False) return GL_Value
   is
     (Const_Int (Int_64_GL_Type, N, Sign_Extend))
     with Post => Present (Const_Int_64'Result);

   function Const_Null_64 return GL_Value is
     (Const_Int_64 (ULL (0)))
     with Post => Present (Const_Null_64'Result);

   function Const_Real
     (V : GL_Value; F : Interfaces.C.double) return GL_Value
   is
     (Const_Real (Related_Type (V), F))
     with Pre  => Is_Floating_Point_Type (V),
          Post => Present (Const_Real'Result);

   function Const_True return GL_Value
     with Post => Relationship (Const_True'Result) = Boolean_Data, Inline;
   function Const_False return GL_Value
     with Post => Relationship (Const_False'Result) = Boolean_Data, Inline;

   function Const_Array
     (Elmts : GL_Value_Array; GT : GL_Type) return GL_Value
     with Pre  => Present (GT)
                  and then (for all V of Elmts => Present (V)),
          Post => Present (Const_Array'Result), Inline;

   function Const_String (S : String; GT : GL_Type) return GL_Value is
     (G (Const_String (S, unsigned (S'Length), True), GT))
     with Pre => Present (GT), Post => Is_Constant (Const_String'Result);

   function Const_Struct
     (Elmts : GL_Value_Array; GT : GL_Type; Packed : Boolean) return GL_Value
     with Pre =>  Present (GT)
                  and then (for all V of Elmts => Present (V)),
          Post => Present (Const_Struct'Result), Inline;

   function Get_Float_From_Words_And_Exp
     (GT : GL_Type; Exp : Int; Words : Word_Array) return GL_Value
     with Pre  => Present (GT),
          Post => Present (Get_Float_From_Words_And_Exp'Result), Inline;

   function Pred_FP (V : GL_Value) return GL_Value
     with Pre => Present (V), Post => Present (Pred_FP'Result), Inline;

   function Set_Object_Align
     (Obj   : Value_T;
      GT    : GL_Type;
      E     : Entity_Id := Empty;
      Align : Nat       := 0) return Nat
     with Pre => Present (Obj) and then Present (GT), Inline;
   function Set_Object_Align
     (Obj   : GL_Value;
      GT    : GL_Type;
      E     : Entity_Id := Empty;
      Align : Nat       := 0) return Nat
     with Pre => Present (Obj) and then Present (GT), Inline;
   --  Set the alignment of alloca inst or global from GT and E (if
   --  present), allowing Align (in bits if nonzero) to override, and
   --  return the resulting alignment (in bits).

   function Block_Address
     (Func : GL_Value; BB : Basic_Block_T) return GL_Value
   is
      (G (Block_Address (+Func, BB), SSI_GL_Type, Reference))
     with Pre  => Present (Func) and then Present (BB),
          Post => Present (Block_Address'Result);

   function Get_Type_Size (V : GL_Value) return GL_Value
     with Pre => Present (V), Post => Present (Get_Type_Size'Result), Inline;

   function Get_Type_Size (V : GL_Value) return ULL
     with Pre => Present (V), Inline;

   function Get_Scalar_Bit_Size (V : GL_Value) return ULL
     with Pre => Present (V), Inline;

   function Get_Type_Alignment
     (V : GL_Value; Use_Specified : Boolean := True) return Nat
     with Pre => Present (V), Inline;

   function Get_Type_Alignment
     (GT : GL_Type; Use_Specified : Boolean := True) return GL_Value
     with Pre  => Present (GT),
          Post => Type_Of (Get_Type_Alignment'Result) = Size_T,
          Inline;

   function Add_Function
     (Name       : String;
      T          : Type_T;
      Return_GT  : GL_Type;
      Is_Builtin : Boolean := False) return GL_Value
   is
     (G (Add_Function ((if Is_Builtin then Module else No_Module_T), Name, T),
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
     (Func        : GL_Value;
      Param_Num   : Nat;
      GT          : GL_Type;
      R           : GL_Relationship;
      Is_Pristine : Boolean := False) return GL_Value
   is
     (G (Get_Param (+Func, unsigned (Param_Num)), GT, R,
         Is_Pristine => Is_Pristine))
     with Pre  => Present (Func) and then Present (GT),
          Post => Present (Get_Param'Result);

   function Get_Insert_Block return Basic_Block_T is
     (Get_Insert_Block (IR_Builder))
     with Post => Present (Get_Insert_Block'Result);

   function Does_Not_Throw (V : GL_Value) return Boolean is
     (Does_Not_Throw (+V))
     with Pre => Present (V);
   --  Return True if V is a function

   procedure Set_Does_Not_Throw (V : GL_Value)
     with Pre => Present (V), Inline;
   --  Indicate that V does not throw exceptions

   procedure Set_Does_Not_Return (V : GL_Value)
     with Pre => Present (V), Inline;
   --  Indicate that V does not return

   function Get_Initializer (V : GL_Value) return GL_Value is
     (Initialize_Alignment (G (Get_Initializer (+V), Related_Type (V), Data)))
     with Pre  => Is_A_Global_Variable (V) and then Is_Global_Constant (V),
          Post => Present (Get_Initializer'Result);

   procedure Set_Initializer (V, Expr : GL_Value)
     with Pre => Present (V) and then Present (Expr), Inline;
   --  Set the initializer for a global variable

   function Get_Linkage (V : GL_Value) return Linkage_T is
     (Get_Linkage (+V))
     with Pre => Is_A_Global_Variable (V) or else Is_A_Function (V);
   --  Get the linkage type for a variable or function

   procedure Set_Linkage (V : GL_Value; Linkage : Linkage_T)
     with Pre => Is_A_Global_Variable (V) or else Is_A_Function (V), Inline;
   --  Set the linkage type for a variable or function

   procedure Set_Absolute_Address (V : GL_Value; Addr : GL_Value)
     with Pre => Tagged_Pointers
                 and then Present (V)
                 and then Is_Discrete_Type (Addr);
   --  Place a global at a statically known constant address

   procedure Set_Global_Constant (V : GL_Value; B : Boolean := True)
     with Pre => Present (V), Inline;

   procedure Set_Thread_Local (V : GL_Value; Thread_Local : Boolean := True)
     with Pre => Is_A_Global_Variable (V), Inline;

   procedure Set_Section (V : GL_Value; S : String)
     with Pre => Is_A_Global_Variable (V) or else Is_A_Function (V), Inline;

   procedure Set_Unnamed_Addr
     (V : GL_Value; Has_Unnamed_Addr : Boolean := True)
     with Pre => Is_A_Global_Variable (V), Inline;

   procedure Set_Volatile_For_Atomic (V : GL_Value)
     with Pre => Present (V), Inline;

   procedure Add_Function_To_Module
     (V : GL_Value; Allow_Deduplication : Boolean)
     with Pre => Present (V), Inline;

   function Is_Layout_Identical (V : GL_Value; GT : GL_Type) return Boolean
     with Pre => Present (V) and then Present (GT), Inline;

   function Is_Layout_Identical (GT1, GT2 : GL_Type) return Boolean
     with Pre => Present (GT1) and then Present (GT2), Inline;

   function Convert_Struct_Constant
     (V : GL_Value; GT : GL_Type) return GL_Value
     with Pre  => Present (V) and then Present (GT),
          Post => Present (Convert_Struct_Constant'Result), Inline;
   --  Convert V, a constant of a struct type, to GT

   function Convert_Struct_Constant (V : Value_T; T : Type_T) return Value_T
     with Pre  => Present (V) and then Present (T),
          Post => Present (Convert_Struct_Constant'Result);
   --  Likewise but for native LLVM objects

   function Idxs_From_GL_Values (Idxs : GL_Value_Array) return Index_Array
     with Pre => (for all V of Idxs => Present (V)),
          Post => Idxs'Length = Idxs_From_GL_Values'Result'Length, Inline;
   --  Convert an array of GL_Value indices into the corresponding arrays
   --  of constants.

   function Get_GEP_Constant_Offset
     (GEP : GL_Value; Offset : out ULL) return Boolean
     with Pre => Present (GEP);
   --  If all the offsets of GEP are constant, return the total offset

   function Get_GEP_Offset_Alignment (GEP : GL_Value) return Nat
     with Pre => Present (GEP);
   --  Return the best known alignment of the cumulative offset of GEP

   function Get_Alloca_Name (E : Entity_Id; Name : String) return String;
   --  Get name to be used for an alloc instruction

   procedure Error_Msg_NE_Num
     (Msg : String; N : Node_Id; E : Entity_Id; V : GL_Value)
     with Pre => Msg'Length > 0 and then Present (N) and then Present (E)
                 and then Present (V);

   function Contains_Bounds (V : GL_Value) return Boolean is
     (Contains_Bounds (Relationship (V))
        or else (Is_Array_Type (V) and then Is_Constrained (V)))
     with Pre => Present (V);
   --  True if V is something that we can find bounds from, either because
   --  it's a relationship that points to bounds or it's a constrained array
   --  (which has bounds).

   procedure C_Set_Entity (V : GL_Value; E : Entity_Id)
     with Pre => Present (V) and then Present (E), Inline;
   --  Set the GNAT entity or type of V

   procedure C_Set_Function (UID : Unique_Id; V : GL_Value)
     with Pre => Present (V), Inline;
   --  Set which function corresponds to UID

   procedure C_Set_Elab_Proc (V : GL_Value; For_Body : Boolean)
     with Pre => Present (V);
   --  Set which function is an elab proc and whether it's for the spec or body

   pragma Annotate (Xcov, Exempt_On, "Debug helpers");

   --  Debug routine to print the LLVM value and GNAT tree node for a GL_Value

   procedure Dump_GL_Value (V : GL_Value)
     with Export, External_Name => "dglv";

   pragma Annotate (Xcov, Exempt_Off, "Debug helpers");
end GNATLLVM.GLValue;
