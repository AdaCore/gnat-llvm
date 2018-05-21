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

with Nlists;   use Nlists;
with Sem_Aggr; use Sem_Aggr;
with Sinfo;    use Sinfo;

with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Types;       use GNATLLVM.Types;

package GNATLLVM.Arrays is

   function Create_Array_Type (TE  : Entity_Id) return Type_T
     with Pre  => Is_Array_Type (TE),
          Post => Present (Create_Array_Type'Result);
   --  Return the type used to represent Array_Type_Node.  This will be
   --  an opaque type if LLVM can't represent it directly.

   function Create_Array_Fat_Pointer_Type (TE : Entity_Id) return Type_T
     with Pre  => Is_Array_Type (TE),
          Post => Present (Create_Array_Fat_Pointer_Type'Result);
   --  Return the type used to store fat pointers to Array_Type

   function Create_Array_Bounds_Type (TE : Entity_Id) return Type_T
     with Pre  => Is_Array_Type (TE),
          Post => Present (Create_Array_Bounds_Type'Result);
   --  Helper that returns the type used to store array bounds. This is a
   --  structure that that follows the following pattern: { LB0, UB0, LB1,
   --  UB1, ... }

   function Get_Bound_Size (TE : Entity_Id) return GL_Value
     with Pre  => Is_Array_Type (TE),
          Post => Present (Get_Bound_Size'Result);
   --  Get the size of the Bounds part of array and data of TE, taking into
   --  account both the size of the bounds and the alignment of the bounds
   --  and TE.

   function Get_Dim_Range (N : Node_Id) return Node_Id
     with Pre  => Present (N), Post => Present (Get_Dim_Range'Result);
   --  Return the N_Range for an array type

   function Use_Discriminant_For_Bound (E : Entity_Id) return GL_Value
     with Pre  => Ekind (E) = E_Discriminant,
          Post => Present (Use_Discriminant_For_Bound'Result);
   --  E is an E_Discriminant that we've run into while emitting an expression.
   --  If we are expecting one as a possible bound, evaluate this discriminant
   --  as required to compute that bound.

   function Get_Array_Bound
     (TE       : Entity_Id;
      Dim      : Nat;
      Is_Low   : Boolean;
      V        : GL_Value;
      For_Type : Boolean := False) return GL_Value
     with Pre  => Is_Array_Type (TE) and then Dim < Number_Dimensions (TE)
                  and then (Present (V) or else Is_Constrained (TE)
                              or else For_Type),
          Post => Present (Get_Array_Bound'Result);
   --  Get the bound (lower if Is_Low, else upper) for dimension number
   --  Dim (0-origin) of an array whose LValue is Value and is of type
   --  Arr_Typ.

   function Get_Array_Length
     (TE      : Entity_Id;
      Dim     : Nat;
      V       : GL_Value;
      For_Type : Boolean := False) return GL_Value
     with Pre  => Is_Array_Type (TE) and then Dim < Number_Dimensions (TE)
                  and then (Present (V) or else Is_Constrained (TE)
                              or else For_Type),
          Post => Type_Of (Get_Array_Length'Result) = LLVM_Size_Type;
   --  Similar, but get the length of that dimension of the array.  This is
   --  always Size_Type's width, but may actually be a different GNAT type.

   function Get_Array_Size_Complexity
     (TE : Entity_Id; For_Type : Boolean := False) return Natural
     with Pre => Is_Array_Type (TE);
   --  Return the complexity of computing the size of an array.  This roughly
   --  gives the number of "things" needed to access to compute the size.
   --  This returns zero iff the array type is of a constant size.

   function Get_Indexed_LValue
     (Indexes : List_Id; V : GL_Value) return GL_Value
     with Pre  => Is_Array_Type (Full_Designated_Type (V))
                  and then List_Length (Indexes) =
                    Number_Dimensions (Full_Designated_Type (V)),
          Post => Present (Get_Indexed_LValue'Result);
   --  Get an LValue corresponding to indexing Value by Indexes.  Arr_Type
   --  is the array type.

   function Get_Slice_LValue
     (TE : Entity_Id; Rng : Node_Id; V : GL_Value) return GL_Value
     with Pre  => Is_Array_Type (Full_Designated_Type (V))
                  and then Number_Dimensions (Full_Designated_Type (V)) = 1,
          Post => Present (Get_Slice_LValue'Result);
   --  Similar, but Rng is the Discrete_Range for the slice

   function Get_Array_Elements
     (V        : GL_Value;
      TE       : Entity_Id;
      For_Type : Boolean := False) return GL_Value
     with Pre  => Is_Array_Type (TE)
                  and then (Present (V) or else Is_Constrained (TE)
                              or else For_Type),
          Post => Present (Get_Array_Elements'Result);
   --  Return the number of elements contained in an Array_Type object as an
   --  integer as large as a pointer for the target architecture. If it is an
   --  unconstrained array, Array_Descr must be an expression that evaluates
   --  to the array.

   function Get_Array_Type_Size
     (TE       : Entity_Id;
      V        : GL_Value;
      For_Type : Boolean := False) return GL_Value
     with Pre  => Is_Array_Type (TE),
          Post => Present (Get_Array_Type_Size'Result);

   procedure Emit_Others_Aggregate (LValue : GL_Value; N : Node_Id)
     with Pre => Present (LValue)
                 and then Nkind_In (N, N_Aggregate, N_Extension_Aggregate)
                 and then Is_Others_Aggregate (N);
   --  Use memset to do an aggregate assignment from N to LValue

   function Emit_Array_Aggregate
     (N              : Node_Id;
      Dims_Left      : Pos;
      Indices_So_Far : Index_Array;
      Value_So_Far   : GL_Value) return GL_Value
     with Pre  => Nkind_In (N, N_Aggregate, N_Extension_Aggregate)
                  and then Present (Value_So_Far)
                  and then Is_Array_Type (Full_Etype (N)),
               Post => Present (Emit_Array_Aggregate'Result);
   --  Emit an N_Aggregate which is an array, returning the GL_Value that
   --  contains the data.  Value_So_Far is any of the array whose value
   --  we've accumulated so far.  Dims_Left says how many dimensions of the
   --  outer array type we still can recurse into.  Indices_So_Far are the
   --  indexes of any outer N_Aggregate expressions we went through.

   procedure Maybe_Store_Bounds
     (Dest, Src : GL_Value; Src_Type : Entity_Id; For_Unconstrained : Boolean)
     with Pre => Present (Dest) and then Is_Type (Src_Type);
   --  If the type of Dest is a nominal constrained type for an aliased
   --  unconstrained array or if For_Unconstrained is True and the type of
   --  Dest is an unconstrained array, store bounds into Dest, taking them
   --  from Src_Type and Src, if the latter is Present.

   function Get_Array_Bounds (TE : Entity_Id; V : GL_Value) return GL_Value
     with Pre  => Is_Array_Type (TE),
          Post => Present (Get_Array_Bounds'Result);
   --  Get the bounds of the array TE using V if necessary

   function Update_Fat_Pointer
     (Fat_Ptr : GL_Value; Array_Data : GL_Value) return GL_Value
     with Pre  => Is_Access_Unconstrained (Fat_Ptr)
                  and then Is_Raw_Array (Array_Data),
          Post => Is_Access_Unconstrained (Update_Fat_Pointer'Result);
   --  We have a fat pointer and have copied the underlying data.  We
   --  now want to make a fat pointer with the same bounds but with the
   --  new data.

end GNATLLVM.Arrays;
