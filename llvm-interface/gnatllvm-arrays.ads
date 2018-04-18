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

with Atree;  use Atree;
with Einfo;  use Einfo;
with Nlists; use Nlists;
with Sinfo;  use Sinfo;
with Types;  use Types;

with LLVM.Core;  use LLVM.Core;
with LLVM.Types; use LLVM.Types;

with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package GNATLLVM.Arrays is

   function Create_Array_Type (TE  : Entity_Id) return Type_T
     with Pre  => Is_Array_Type (TE),
          Post => Present (Create_Array_Type'Result);
   --  Return the type used to represent Array_Type_Node.  This will be
   --  an opaque type if LLVM can't represent it directly.

   function Create_Array_Raw_Pointer_Type
     (Array_Type_Node : Entity_Id) return Type_T
     with Pre  => Is_Array_Type (Array_Type_Node),
          Post => (Get_Type_Kind (Create_Array_Raw_Pointer_Type'Result) =
                   Pointer_Type_Kind);
   --  Return the type used to store thin pointers to Array_Type

   function Create_Array_Fat_Pointer_Type
     (Array_Type : Entity_Id) return Type_T
     with Pre  => Is_Array_Type (Array_Type),
          Post => Present (Create_Array_Fat_Pointer_Type'Result);
   --  Return the type used to store fat pointers to Array_Type

   function Create_Array_Bounds_Type
     (Array_Type_Node : Entity_Id) return Type_T
     with Pre  => Is_Array_Type (Array_Type_Node),
          Post => Present (Create_Array_Bounds_Type'Result);
   --  Helper that returns the type used to store array bounds. This is a
   --  structure that that follows the following pattern: { LB0, UB0, LB1,
   --  UB1, ... }

   function Get_Array_Bound
     (Arr_Typ  : Entity_Id;
      Dim      : Nat;
      Is_Low   : Boolean;
      Value    : GL_Value;
      For_Type : Boolean := False) return GL_Value
     with Pre  => Is_Array_Type (Arr_Typ)
                  and then Dim < Number_Dimensions (Arr_Typ)
                  and then (Present (Value)
                              or else Is_Constrained (Arr_Typ))
                  and then (not For_Type or else No (Value)),
          Post => Present (Get_Array_Bound'Result);
   --  Get the bound (lower if Is_Low, else upper) for dimension number
   --  Dim (0-origin) of an array whose LValue is Value and is of type
   --  Arr_Typ.

   function Get_Array_Length
     (Arr_Typ : Entity_Id;
      Dim     : Nat;
      Value   : GL_Value;
      For_Type : Boolean := False) return GL_Value
     with Pre  => Is_Array_Type (Arr_Typ)
                  and then Dim < Number_Dimensions (Arr_Typ)
                  and then (Present (Value)
                              or else Is_Constrained (Arr_Typ))
                  and then (not For_Type or else No (Value)),
          Post => Type_Of (Get_Array_Length'Result) = Env.LLVM_Size_Type;
   --  Similar, but get the length of that dimension of the array.  This is
   --  always Size_Type's width, but may actually be a different GNAT type.

   function Get_Array_Size_Complexity (TE : Entity_Id) return Natural
     with Pre => Is_Array_Type (TE);
   --  Return the complexity of computing the size of an array.  This roughly
   --  gives the number of "things" needed to access to compute the size.
   --  This returns zero iff the array type is of a constant size.

   function Get_Indexed_LValue
     (Indexes : List_Id; Value : GL_Value) return GL_Value
     with Pre  => Is_Array_Type (Full_Designated_Type (Value))
                  and then List_Length (Indexes) =
                    Number_Dimensions (Full_Designated_Type (Value)),
          Post => Present (Get_Indexed_LValue'Result);
   --  Get an LValue corresponding to indexing Value by Indexes.  Arr_Type
   --  is the array type.

   function Get_Slice_LValue
     (Result_Type : Entity_Id; Rng : Node_Id; Value : GL_Value) return GL_Value
     with Pre  => Is_Array_Type (Full_Designated_Type (Value))
                  and then Number_Dimensions
                    (Full_Designated_Type (Value)) = 1,
          Post => Present (Get_Slice_LValue'Result);
   --  Similar, but Rng is the Discrete_Range for the slice

   function Get_Array_Elements
     (Array_Descr : GL_Value;
      Array_Type  : Entity_Id;
      For_Type    : Boolean := False) return GL_Value
     with Pre  => Is_Array_Type (Array_Type)
                  and then (Present (Array_Descr)
                              or else Is_Constrained (Array_Type))
                  and then (not For_Type or else No (Array_Descr)),
          Post => Present (Get_Array_Elements'Result);
   --  Return the number of elements contained in an Array_Type object as an
   --  integer as large as a pointer for the target architecture. If it is an
   --  unconstrained array, Array_Descr must be an expression that evaluates
   --  to the array.

   function Get_Array_Type_Size
     (TE       : Entity_Id;
      V        : GL_Value;
      For_Type : Boolean := False) return GL_Value
     with Pre  => Present (TE), Post => Present (Get_Array_Type_Size'Result);

   function Emit_Array_Aggregate
     (Node           : Node_Id;
      Dims_Left      : Pos;
      Indices_So_Far : Index_Array;
      Value_So_Far   : GL_Value) return GL_Value
     with Pre  => Nkind (Node) = N_Aggregate and then Present (Value_So_Far)
                  and then Is_Array_Type (Full_Etype (Node)),
               Post => Present (Emit_Array_Aggregate'Result);
   --  Emit an N_Aggregate which is an array, returning the GL_Value that
   --  contains the data.  Value_So_Far is any of the array whose value
   --  we've accumulated so far.  Dims_Left says how many dimensions of the
   --  outer array type we still can recurse into.  Indices_So_Far are the
   --  indexes of any outer N_Aggregate expressions we went through.

   function Array_Data (Array_Descr : GL_Value) return GL_Value
     with Pre  => Is_Access_Type (Array_Descr)
                  and then Is_Array_Type (Full_Designated_Type (Array_Descr)),
          Post => Present (Array_Data'Result);
   --  Emit code to compute the address of the array data and return the
   --  corresponding value. Handle both constrained and unconstrained arrays,
   --  depending on Array_Type. If this is a constrained array, Array_Descr
   --  must already be a pointer to the array data, otherwise, it must be a
   --  fat pointer.

   function Array_Fat_Pointer
     (Array_Type : Entity_Id; Array_Data : GL_Value) return GL_Value
     with Pre  => Is_Access_Type (Array_Data)
                  and then Is_Array_Type (Array_Type)
                  and then not Is_Constrained (Array_Type)
                  and then Is_Array_Type (Full_Designated_Type (Array_Data))
                  and then Is_Constrained (Full_Designated_Type (Array_Data)),
          Post => Present (Array_Fat_Pointer'Result);
   --  Wrap a fat pointer around Array_Data and return the created fat pointer.
   --  Array_Type gives the destination array type, which is what specifies
   --  the types of the bounds.

end GNATLLVM.Arrays;
