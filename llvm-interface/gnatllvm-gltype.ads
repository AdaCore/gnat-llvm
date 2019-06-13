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

with Sem_Aux;  use Sem_Aux;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;

with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Types;       use GNATLLVM.Types;

package GNATLLVM.GLType is

   --  To support representation clauses on objects and components, we need
   --  multiple LLVM types for the same GNAT tree type (Ada type), each
   --  corresponding to a different length and alignment.  Some of these
   --  may be biased types or may represent the maximum size of an
   --  unconstrained discriminated record with default discriminant values.
   --
   --  We maintain a table of such types, referring to the index of the
   --  table as a GL_Type.  Each table entry contains the GNAT Entity_Id of
   --  the type, the LLVM type, the size, alignment, and related flags, and
   --  a chain to record all the alternates for the GNAT type.  We create a
   --  link from the GNAT type to its first GL_Type.  One entry is
   --  designated as "primitive", meaning it's the actual type used for the
   --  value (in the case of scalar types) or the natural type (without any
   --  padding) in the case of aggregates.  One GL_Type (possibly the same
   --  one, but not necessarily) is the default for that type.

   procedure Dump_GL_Type_Int (GT : GL_Type; Full_Dump : Boolean);

   procedure Discard (GT : GL_Type)
     with Pre => Present (GT);

   function New_GT (TE : Entity_Id) return GL_Type
     with Pre  => Is_Type_Or_Void (TE),
          Post => Present (New_GT'Result);
   --  Create a new GL_Type with None kind for type TE

   function Make_GT_Alternative
     (GT            : GL_Type;
      Def_Ident     : Entity_Id;
      Size          : Uint    := No_Uint;
      Align         : Uint    := No_Uint;
      For_Type      : Boolean := False;
      For_Component : Boolean := False;
      Max_Size      : Boolean := False;
      Is_Biased     : Boolean := False) return GL_Type
     with Pre  => Present (GT),
          Post => Full_Etype (Make_GT_Alternative'Result) = Full_Etype (GT);
   --  Return a GL_Type (creating one if necessary) with the specified
   --  parameters.  For_Type is True if we're doing this for a type; in that
   --  case the size needs to be rounded to the alignment.  Max_Size is True
   --  if we're computing the maximum size of an unconstrained record and
   --  Biased is True if we're using a biased representation to store this
   --  integral value.  Def_Ident is the identifier that we're doing this
   --  for and is used for a warning message if we're padding.  For_Component
   --  is true if we're changing the component size and is used for
   --  any warning message.

   procedure Update_GL_Type (GT : GL_Type; T : Type_T; Is_Dummy : Boolean)
     with Pre => Is_Empty_GL_Type (GT) or else Is_Dummy_Type (GT)
                 or else T = Type_Of (GT);
   --  Update GT with a new type and dummy status

   function Primitive_GL_Type (TE : Entity_Id) return GL_Type
     with Pre  => Is_Type_Or_Void (TE),
          Post => (Is_Primitive_GL_Type (Primitive_GL_Type'Result)
                     or else Is_Dummy_Type (Primitive_GL_Type'Result))
                  and then TE = Full_Etype (Primitive_GL_Type'Result);
   --  Return the GT_Type for TE that corresponds to its basic computational
   --  form, creating it if it doesn't exist.

   function Primitive_GL_Type (GT : GL_Type) return GL_Type
     with Pre  => Present (GT),
          Post => (Is_Primitive_GL_Type (Primitive_GL_Type'Result)
                     or else Is_Dummy_Type (Primitive_GL_Type'Result))
                  and then Full_Etype (GT) =
                      Full_Etype (Primitive_GL_Type'Result);
   --  Return the GT_Type for TE that corresponds to its basic computational
   --  form, creating it if it doesn't exist.

   function Primitive_GL_Type (V : GL_Value) return GL_Type
     with Pre  => Present (V),
          Post => (Is_Primitive_GL_Type (Primitive_GL_Type'Result)
                     or else Is_Dummy_Type (Primitive_GL_Type'Result))
                  and then Full_Etype (V) = Full_Etype
                             (Primitive_GL_Type'Result);

   function Dummy_GL_Type (TE : Entity_Id) return GL_Type
     with Pre  => Is_Type_Or_Void (TE),
          Post => Present (Dummy_GL_Type'Result);
   --  Return the GT_Type for TE that corresponds to a dummy form

   function Default_GL_Type
     (TE : Entity_Id; Create : Boolean := True) return GL_Type
     with Pre  => Is_Type_Or_Void (TE),
          Post => not Create or else Present (Default_GL_Type'Result);
   --  Return the GT_TYpe for TE that's to be used as the default for
   --  objects or components of the type.  If Create is True, make one if
   --  it doesn't already exist.  This may or may not be the same as what
   --  Primitive_GL_Type returns.

   procedure Mark_Default (GT : GL_Type)
     with Pre => Present (GT);
   --  Mark GT as the type to be used as the default representation of
   --  its corresponding GNAT type.

   function Full_GL_Type (N : Node_Id) return GL_Type is
     (Default_GL_Type (Full_Etype (N)))
     with Pre => Present (N), Post => Present (Full_GL_Type'Result);
   --  Return the default GL_Type corresponding to the type of N

   function Full_Alloc_GL_Type (N : Node_Id) return GL_Type
     with Pre => Present (N), Post => Present (Full_Alloc_GL_Type'Result);
   --  Likewise, but use the type for an allocation, which may be an
   --  Actual_Subtype.

   function Base_GL_Type (TE : Entity_Id) return GL_Type
     with Pre  => Is_Type (TE),
          Post => Is_Primitive_GL_Type (Base_GL_Type'Result);
   function Base_GL_Type (GT : GL_Type) return GL_Type
     with Pre  => Present (GT),
          Post => Is_Primitive_GL_Type (Base_GL_Type'Result);
   function Base_GL_Type (V : GL_Value) return GL_Type is
     (Base_GL_Type (GL_Type'(Related_Type (V))))
     with Pre  => Present (V),
          Post => Is_Primitive_GL_Type (Base_GL_Type'Result);
   --  Given a or GL_Type, return a GL_Type that corresponds to the
   --  primitive GL_Type of the base type of a type.  This is used to perform
   --  computation on a type.

   --  Here are the access function to obtain fields from a GL_Type.
   --  Many are overloaded from the functions that obtain these fields from
   --  a GNAT type.

   function Full_Etype (GT : GL_Type)            return Entity_Id
     with Pre => Present (GT), Post => Is_Type_Or_Void (Full_Etype'Result);

   function Type_Of (GT : GL_Type)               return Type_T
     with Pre => Present (GT);

   function GT_Size (GT : GL_Type)               return GL_Value
     with Pre => Present (GT);

   function Is_Max_Size (GT : GL_Type)           return Boolean
     with Pre => Present (GT);

   function GT_Alignment (GT : GL_Type)          return GL_Value
     with Pre => Present (GT);

   function Is_Dummy_Type (GT : GL_Type)         return Boolean
     with Pre => Present (GT);

   function Is_Primitive_GL_Type (GT : GL_Type)  return Boolean
     with Pre => Present (GT);

   function Is_Primitive_GL_Type (V : GL_Value)  return Boolean is
     (Is_Primitive_GL_Type (Related_Type (V)))
     with Pre => Present (V);

   function Is_Biased_GL_Type (GT : GL_Type)     return Boolean
     with Pre => Present (GT);

   function Is_Biased_GL_Type (V : GL_Value)     return Boolean is
     (Is_Biased_GL_Type (Related_Type (V)))
     with Pre => Present (V);

   function Is_Padded_GL_Type (GT : GL_Type)     return Boolean
     with Pre => Present (GT);

   function Is_Padded_GL_Type (V : GL_Value)     return Boolean is
     (Is_Padded_GL_Type (Related_Type (V)))
     with Pre => Present (V);

   function Is_Byte_Array_GL_Type (GT : GL_Type) return Boolean
     with Pre => Present (GT);

   function Is_Byte_Array_GL_Type (V : GL_Value) return Boolean is
     (Is_Byte_Array_GL_Type (Related_Type (V)))
     with Pre => Present (V);

   function Has_Padding (GT : GL_Type)           return Boolean is
     (Is_Padded_GL_Type (GT) or else Is_Byte_Array_GL_Type (GT))
     with Pre => Present (GT);

   function Has_Padding (V : GL_Value)           return Boolean is
     (Has_Padding (Related_Type (V)))
     with Pre => Present (V);

   function Is_Empty_GL_Type (GT : GL_Type)      return Boolean
     with Pre => Present (GT);

   function To_Primitive (V : GL_Value)          return GL_Value
     with Pre  => Present (V),
          Post => Is_Primitive_GL_Type (Related_Type (To_Primitive'Result));
   --  Convert V to its primitive GL_Type

   function Remove_Padding (V : GL_Value)        return GL_Value is
     ((if Has_Padding (V) then To_Primitive (V) else V))
     with Pre  => Present (V),
          Post => not Has_Padding (Remove_Padding'Result);
   --  Likewise, but only if the type is padded

   function From_Primitive (V : GL_Value; GT : GL_Type) return GL_Value
     with Pre  => Is_Primitive_GL_Type (Related_Type (V))
                  and then Full_Etype (Related_Type (V)) = Full_Etype (GT),
          Post => Related_Type (From_Primitive'Result) = GT;
   --  Convert V, which must be of a primitive GL_Type, to GT

   --  Now define functions that operate on GNAT types that we want to
   --  also operate on GL_Type's.

   function Ekind (GT : GL_Type) return Entity_Kind is
     (Ekind (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Access_Type (GT : GL_Type) return Boolean is
     (Is_Access_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Full_Original_Array_Type (GT : GL_Type) return Entity_Id is
     (Full_Original_Array_Type (Full_Etype (GT)))
     with Pre  => Is_Array_Or_Packed_Array_Type (GT),
          Post => Is_Type (Full_Original_Array_Type'Result);

   function Full_Designated_Type (GT : GL_Type) return Entity_Id is
     (Full_Designated_Type (Full_Etype (GT)))
     with Pre  => Is_Access_Type (GT),
          Post => Is_Type (Full_Designated_Type'Result);

   function Full_Designated_GL_Type (GT : GL_Type) return GL_Type
     with Pre  => Is_Access_Type (GT),
          Post => Present (Full_Designated_GL_Type'Result);

   function Full_Designated_GL_Type (TE : Entity_Id) return GL_Type is
     (Get_Associated_GL_Type (TE))
     with Pre  => Is_Access_Type (TE),
          Post => Present (Full_Designated_GL_Type'Result);

   function Full_Designated_GL_Type (V : GL_Value) return GL_Type
     with Pre  => Is_Access_Type (V),
          Post => Present (Full_Designated_GL_Type'Result);

   function Full_Component_Type (GT : GL_Type) return Entity_Id is
     (Full_Component_Type (Full_Etype (GT)))
     with Pre  => Is_Array_Type (GT),
          Post => Is_Type (Full_Component_Type'Result);

   function Full_Component_GL_Type (GT : GL_Type) return GL_Type is
     (Get_Associated_GL_Type (Full_Etype (GT)))
     with Pre  => Is_Array_Type (GT),
          Post => Present (Full_Component_GL_Type'Result);

   function Full_Component_GL_Type (V : GL_Value) return GL_Type is
     (Get_Associated_GL_Type (Full_Etype (Related_Type (V))))
     with Pre  => Is_Array_Type (Related_Type (V)),
          Post => Present (Full_Component_GL_Type'Result);

   function Full_Component_GL_Type (TE : Entity_Id) return GL_Type is
     (Get_Associated_GL_Type (TE))
     with Pre  => Is_Array_Type (TE),
          Post => Present (Full_Component_GL_Type'Result);

   function Full_Base_Type (GT : GL_Type) return Entity_Id is
     (Full_Base_Type (Full_Etype (GT)))
     with Pre  => Present (GT), Post => Is_Type (Full_Base_Type'Result);

   function Ultimate_Base_Type (GT : GL_Type) return Entity_Id is
     (Ultimate_Base_Type (Full_Etype (GT)))
     with Pre  => Present (GT), Post => Is_Type (Ultimate_Base_Type'Result);

   function Is_Nonnative_Type (GT : GL_Type) return Boolean
     with Pre => Present (GT);

   function Is_Base_Type (GT : GL_Type) return Boolean is
     (Is_Base_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Array_Type (GT : GL_Type) return Boolean is
     (Is_Array_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Array_Or_Packed_Array_Type (GT : GL_Type) return Boolean is
     (Is_Array_Or_Packed_Array_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Access_Subprogram_Type (GT : GL_Type) return Boolean is
    (Is_Access_Type (GT)
       and then Ekind (Full_Designated_GL_Type (GT)) = E_Subprogram_Type)
     with Pre => Present (GT);

   function Is_Tagged_Type (GT : GL_Type) return Boolean is
     (Is_Tagged_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Constrained (GT : GL_Type) return Boolean is
     (Is_Constrained (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Record_Type (GT : GL_Type) return Boolean is
     (Is_Record_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Composite_Type (GT : GL_Type) return Boolean is
     (Is_Composite_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Elementary_Type (GT : GL_Type) return Boolean is
     (Is_Elementary_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Scalar_Type (GT : GL_Type) return Boolean is
     (Is_Scalar_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Discrete_Type (GT : GL_Type) return Boolean is
     (Is_Discrete_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Integer_Type (GT : GL_Type) return Boolean is
     (Is_Integer_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Boolean_Type (GT : GL_Type) return Boolean is
     (Is_Boolean_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Fixed_Point_Type (GT : GL_Type) return Boolean is
     (Is_Fixed_Point_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Floating_Point_Type (GT : GL_Type) return Boolean is
     (Is_Floating_Point_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Unsigned_Type (GT : GL_Type) return Boolean is
     (Is_Biased_GL_Type (GT) or else Is_Unsigned_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Discrete_Or_Fixed_Point_Type (GT : GL_Type) return Boolean is
     (Is_Discrete_Or_Fixed_Point_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Modular_Integer_Type (GT : GL_Type) return Boolean is
     (Is_Modular_Integer_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Non_Binary_Modulus (GT : GL_Type) return Boolean is
     (Non_Binary_Modulus (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Unconstrained_Record (GT : GL_Type) return Boolean is
     (Is_Unconstrained_Record (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Unconstrained_Array (GT : GL_Type) return Boolean is
     (Is_Unconstrained_Array (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Unconstrained_Type (GT : GL_Type) return Boolean is
     (Is_Unconstrained_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Access_Unconstrained_Array (GT : GL_Type) return Boolean is
     (Is_Access_Unconstrained_Array (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Packed_Array_Impl_Type (GT : GL_Type) return Boolean is
     (Is_Packed_Array_Impl_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Bit_Packed_Array_Impl_Type (GT : GL_Type) return Boolean is
     (Is_Bit_Packed_Array_Impl_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Constr_Subt_For_UN_Aliased (GT : GL_Type) return Boolean is
     (Is_Constr_Subt_For_UN_Aliased (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Class_Wide_Equivalent_Type (GT : GL_Type) return Boolean is
     (Is_Class_Wide_Equivalent_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_By_Reference_Type (GT : GL_Type) return Boolean is
     (Is_By_Reference_Type (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Unchecked_Union (GT : GL_Type) return Boolean is
     (Is_Unchecked_Union (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Volatile (GT : GL_Type) return Boolean is
     (Is_Volatile (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Atomic_Or_VFA (GT : GL_Type) return Boolean is
     (Is_Atomic_Or_VFA (Full_Etype (GT)))
     with Pre => Present (GT);

   function Is_Independent (GT : GL_Type) return Boolean is
     (Is_Independent (Full_Etype (GT)))
     with Pre => Present (GT);

   function Universal_Aliasing (GT : GL_Type) return Boolean is
     (Universal_Aliasing (Full_Etype (GT)))
     with Pre => Present (GT);

   function Has_Foreign_Convention (GT : GL_Type) return Boolean is
     (Has_Foreign_Convention (Full_Etype (GT)))
     with Pre => Present (GT);

   function Has_Discriminants (GT : GL_Type) return Boolean is
     (Has_Discriminants (Full_Etype (GT)))
     with Pre => Present (GT);

   function Requires_Transient_Scope (GT : GL_Type) return Boolean is
     (Requires_Transient_Scope (Full_Etype (GT)))
     with Pre => Present (GT);

   function Has_Task (GT : GL_Type) return Boolean is
     (Has_Task (Full_Etype (GT)))
     with Pre => Present (GT);

   function Has_Protected (GT : GL_Type) return Boolean is
     (Has_Protected (Full_Etype (GT)))
     with Pre => Present (GT);

   function Type_Needs_Bounds (GT : GL_Type) return Boolean is
     (Type_Needs_Bounds (Full_Etype (GT)))
     with Pre => Present (GT);

   function Can_Never_Be_Null (GT : GL_Type) return Boolean is
     (Can_Never_Be_Null (Full_Etype (GT)))
     with Pre => Present (GT);

   function RM_Size (GT : GL_Type) return Uint is
     (RM_Size (Full_Etype (GT)))
     with Pre => Present (GT);

   function Modulus (GT : GL_Type) return Uint is
     (Modulus (Full_Etype (GT)))
     with Pre => Is_Modular_Integer_Type (GT);

   function Esize (GT : GL_Type) return Uint is
     (Esize (Full_Etype (GT)))
     with Pre => Present (GT);

   function Unknown_Esize (GT : GL_Type) return Boolean is
     (Unknown_Esize (Full_Etype (GT)))
     with Pre => Present (GT);

   function Unknown_RM_Size (GT : GL_Type) return Boolean is
     (Unknown_RM_Size (Full_Etype (GT)))
     with Pre => Present (GT);

   function Scalar_Range (GT : GL_Type) return Node_Id is
     (Scalar_Range (Full_Etype (GT)))
     with Pre => not Is_Access_Type (GT);

   function Type_Low_Bound (GT : GL_Type) return Node_Id is
     (Type_Low_Bound (Full_Etype (GT)))
     with Pre => not Is_Access_Type (GT);

   function Type_High_Bound (GT : GL_Type) return Node_Id is
     (Type_High_Bound (Full_Etype (GT)))
     with Pre => not Is_Access_Type (GT);

   function First_Index (GT : GL_Type) return Entity_Id is
     (First_Index (Full_Etype (GT)))
     with Pre => Is_Array_Type (GT);

   function Strict_Alignment (GT : GL_Type) return Boolean is
     (Strict_Alignment (Full_Etype (GT)))
     with Pre => Present (GT);

   function First_Component_Or_Discriminant (GT : GL_Type) return Entity_Id is
     (First_Component_Or_Discriminant (Full_Etype (GT)))
     with Pre => Is_Record_Type (GT);

   function Convention (GT : GL_Type) return Convention_Id is
     (Convention (Full_Etype (GT)))
     with Pre => Present (GT);

   function Component_Type (GT : GL_Type) return Entity_Id is
     (Component_Type (Full_Etype (GT)))
     with Pre => Is_Array_Type (GT), Post => Present (Component_Type'Result);

   function Default_Aspect_Value (GT : GL_Type) return Node_Id is
     (Default_Aspect_Value (Full_Etype (GT)))
     with Pre => Present (GT);

   function Number_Dimensions (GT : GL_Type) return Pos is
     (Number_Dimensions (Full_Etype (GT)))
     with Pre => Is_Array_Type (GT);

end GNATLLVM.GLType;
