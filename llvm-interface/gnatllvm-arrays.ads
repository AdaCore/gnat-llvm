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
with Sem_Aggr;    use Sem_Aggr;
with Table;       use Table;

with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLType;      use GNATLLVM.GLType;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Types;       use GNATLLVM.Types;

package GNATLLVM.Arrays is

   function Contains_Discriminant (N : N_Subexpr_Id) return Boolean;
   --  Return True if N contains a reference to a discriminant

   function Is_Self_Referential_Type (GT : GL_Type) return Boolean
     with Pre => Present (GT);
   --  Return True if GT is a type where computing the size of the type
   --  requires an instance of the type. This is true for an unconstrained
   --  type or an array subtype that has a discriminant as a bound.

   function Get_Bound_Size (GT : Array_Or_PAT_GL_Type) return GL_Value
     with Post => Present (Get_Bound_Size'Result);
   --  Get the size of the Bounds part of array and data of GT, taking into
   --  account both the size of the bounds and the alignment of the bounds
   --  and GT.

   function Has_Bounds_In_Fat_Pointer
     (GT : Array_Or_PAT_GL_Type) return Boolean
   is
     (Number_Bounds (GT) = 1
      and then Get_Bound_Size (GT) <= Thin_Pointer_Size);
   --  True if the fat pointer for GT contains the bounds themselves instead
   --  of a pointer to the bounds. We do this iff it has a single bound
   --  (meaning a single-dimensional array with a fixed lower bound) but
   --  not if the bound is wider than the size of a pointer (which is rare)
   --  since we have lots of code that assumes it knows the size of a fat
   --  pointer.

   function Bounds_To_Length
     (In_Low, In_High : GL_Value;
      GT              : GL_Type;
      Not_Superflat   : Boolean := False) return GL_Value
     with Pre  => Present (In_Low) and then Present (In_High)
                  and then Present (GT)
                  and then Type_Of (In_Low) = Type_Of (In_High),
          Post => Related_Type (Bounds_To_Length'Result) = GT;
   --  Low and High are bounds of a discrete type. Compute the length of
   --  that type, taking into account the superflat case, and do that
   --  computation in GT. We would like to have the above test be that the
   --  two types be identical, but that's too strict (for example, one
   --  may be Integer and the other Integer'Base), so just check the width.

   function Get_Bound_Alignment (GT : Array_Or_PAT_GL_Type) return Nat;
   --  Get the alignment of the Bounds part of array and data of GT

   function Get_Array_Bound
     (GT       : Array_Or_PAT_GL_Type;
      Dim      : Nat;
      Is_Low   : Boolean;
      V        : GL_Value;
      Max_Size : Boolean := False;
      For_Orig : Boolean := False) return GL_Value
     with Post => Present (Get_Array_Bound'Result);
   --  Get the bound (lower if Is_Low, else upper) for dimension number Dim
   --  (0-origin) of an array whose LValue is Value and is of type Arr_Typ.
   --  If For_Orig is True, get the information from Original_Array_Type of GT.

   function Get_Array_Length
     (TE       : Array_Kind_Id;
      Dim      : Nat;
      V        : GL_Value;
      Max_Size : Boolean := False) return GL_Value
     with Pre  => Dim < Number_Dimensions (TE)
                  and then (Present (V) or else Is_Constrained (TE)
                              or else Max_Size),
          Post => Type_Of (Get_Array_Length'Result) = Size_T;
   --  Similar, but get the length of that dimension of the array.  This is
   --  always Size_Type's width, but may actually be a different GNAT type.

   function Array_Not_Superflat (TE : Array_Kind_Id; Dim : Nat) return Boolean
     with Pre => Dim < Number_Dimensions (TE);
   --  Return True if TE is known to not be superflat in dimension Dim

   function Array_Index_GT (GT : Array_GL_Type; Dim : Nat) return GL_Type
     with Pre  => Dim < Number_Dimensions (GT),
          Post => Present (Array_Index_GT'Result);
   function Array_Index_GT (TE : Array_Kind_Id; Dim : Nat) return GL_Type
     with Pre  => Dim < Number_Dimensions (TE),
          Post => Present (Array_Index_GT'Result);
   --  Get the type of the Dim'th index of a type

   function Array_Index_Has_FLB (TE : Array_Kind_Id; Dim : Nat) return Boolean
     with Pre  => Dim < Number_Dimensions (TE);
   --  See if Dim of TE has a fixed lower bound

   function Get_Array_Size_Complexity
     (TE : Array_Kind_Id; Max_Size : Boolean := False) return Nat;
   --  Return the complexity of computing the size of an array. This roughly
   --  gives the number of "things" needed to access to compute the size.
   --  This returns zero iff the array type is of a constant size.

   function Get_Indices
     (Indices : List_Id; V : GL_Value) return GL_Value_Array
     with Pre  => Is_Array_Type (Related_Type (V))
                  and then List_Length (Indices) =
                             Number_Dimensions (Related_Type (V)),
          Post => Get_Indices'Result'Length = List_Length (Indices);
   --  Given a list of indices and V, return a list where we've evaluated
   --  all the indices and subtracted the lower bounds of each dimension.
   --  This list consists of the constant zero followed by the indices.

   function Get_Indexed_LValue
     (Idxs : GL_Value_Array; V : GL_Value) return GL_Value
     with Pre  => Is_Reference (V) and then Is_Array_Type (Related_Type (V))
                  and then Idxs'Length =
                             Number_Dimensions (Related_Type (V)),
          Post => Present (Get_Indexed_LValue'Result);
   --  Get an LValue corresponding to indexing V by the list of indices
   --  in Idxs. This list is the constant zero followed by the actual indices
   --  (i.e., with the lower bound already subtracted).

   function Get_Slice_LValue (GT : Array_GL_Type; V : GL_Value) return GL_Value
     with Pre  => Is_Reference (V) and then Is_Array_Type (Related_Type (V))
                  and then Number_Dimensions (Related_Type (V)) = 1
                  and then Number_Dimensions (GT) = 1,
          Post => Present (Get_Slice_LValue'Result);
   --  Similar, but we get the position from the First_Index of GT

   function Get_Array_Elements
     (V        : GL_Value;
      TE       : Array_Kind_Id;
      Max_Size : Boolean := False) return GL_Value
     with Pre  => Present (V) or else Is_Constrained (TE) or else Max_Size,
          Post => Present (Get_Array_Elements'Result);
   --  Return the number of elements contained in an Array_Type object as an
   --  integer as large as a pointer for the target architecture. If it is an
   --  unconstrained array, Array_Descr must be an expression that evaluates
   --  to the array.

   function Get_Array_Type_Size
     (TE       : Array_Kind_Id;
      V        : GL_Value;
      Max_Size : Boolean := False) return GL_Value
     with Post => Present (Get_Array_Type_Size'Result);

   function Get_Array_Type_Size
     (TE       : Array_Kind_Id;
      V        : GL_Value;
      Max_Size : Boolean := False) return IDS
     with Post => Present (Get_Array_Type_Size'Result);

   function Get_Array_Type_Size
     (TE       : Array_Kind_Id;
      V        : GL_Value;
      Max_Size : Boolean := False) return BA_Data;

   function Get_Unc_Array_Type_Size
     (TE       : Array_Kind_Id;
      V        : GL_Value;
      Max_Size : Boolean := False) return GL_Value
   is
     (Get_Array_Type_Size (TE, V, Max_Size))
     with Post => Present (Get_Unc_Array_Type_Size'Result);

   function Get_Unc_Array_Type_Size
     (Unused_TE       : Array_Kind_Id;
      Unused_V        : GL_Value;
      Unused_Max_Size : Boolean := False) return IDS
   is
     (Var_IDS)
     with Post => Present (Get_Unc_Array_Type_Size'Result);

   function Get_Unc_Array_Type_Size
     (Unused_TE       : Array_Kind_Id;
      Unused_V        : GL_Value;
      Unused_Max_Size : Boolean := False) return BA_Data
   is
     (No_BA);

   function Get_Array_Type_Alignment (TE : Array_Kind_Id) return Nat;
   --  Like Get_Type_Alignment, but only for arrays and passed the GNAT type

   function Is_Native_Component_GT (GT : GL_Type) return Boolean is
     (not Is_Nonnative_Type (GT) and then not Is_Truncated_GL_Type (GT)
        and then Get_Type_Size (Type_Of (GT)) = Get_Type_Size (GT))
     with Pre => Present (GT);
   --  True if this is a component type that we can use natively; i.e.
   --  without making the array [N x i8] and doing our own indexing.
   --  If we have a type like i24, where the size of the LLVM type
   --  isn't consistent with the number of bits, we can't use it either.

   function Bounds_To_Length
     (In_Low, In_High : BA_Data;
      GT              : GL_Type;
      Not_Superflat   : Boolean := False) return BA_Data;

   function Data_Index_In_BD_Type (V : GL_Value) return unsigned
     with Pre  => Relationship (V)
                  in Reference_To_Bounds_And_Data | Bounds_And_Data,
          Post => Data_Index_In_BD_Type'Result in 1 | 2;
   --  Get the index of the data in a Bounds and Data value

   procedure Emit_Single_Aggregate (LValue : GL_Value; N : N_Subexpr_Id)
     with Pre => Present (LValue)
                 and then Nkind (N) in N_Aggregate | N_Extension_Aggregate
                 and then Is_Single_Aggregate (N);
   --  Use memset to do an aggregate assignment from N to LValue

   function Emit_Array_Aggregate
     (N              : N_Subexpr_Id;
      Dims_Left      : Pos;
      Indices_So_Far : GL_Value_Array;
      Value_So_Far   : GL_Value) return GL_Value
     with Pre  => Nkind (N) in N_Aggregate | N_Extension_Aggregate
                  and then Is_Array_Type (Full_Etype (N)),
               Post => Present (Emit_Array_Aggregate'Result);
   --  Emit an aggregate which is an array, returning the GL_Value that
   --  contains the data. Value_So_Far, if Present, is any of the array
   --  whose value we've accumulated so far. Dims_Left says how many
   --  dimensions of the outer array type we still can recurse into.
   --  Indices_So_Far are the indices of any outer N_Aggregate expressions
   --  we went through.

   procedure Maybe_Store_Bounds
     (Dest, Src : GL_Value; Src_GT : GL_Type; For_Unconstrained : Boolean)
     with Pre => Present (Dest) and then Present (Src_GT);
   --  If the type of Dest is a nominal constrained type for an aliased
   --  unconstrained array or if For_Unconstrained is True and the type of
   --  Dest is an unconstrained array, store bounds into Dest, taking them
   --  from Src_GT and Src, if the latter is Present.

   function Get_Array_Bounds
     (GT, V_GT : Array_Or_PAT_GL_Type; V : GL_Value) return GL_Value
     with Post => Present (Get_Array_Bounds'Result);
   --  Get the bounds of the array type V_GT using V if necessary. GT is
   --  the type of the array we're getting the bounds for, in case they're
   --  different.

   function Build_Indexed_Load
     (V          : GL_Value;
      Idxs       : GL_Value_Array;
      For_LHS    : Boolean := False;
      Prefer_LHS : Boolean := False;
      VFA        : Boolean := False) return GL_Value
     with Pre => Present (V), Post => Present (Build_Indexed_Load'Result);
   --  Perform an indexed load operation with prefix V and indices Idxs.

   function Build_Indexed_Store
     (In_LHS : GL_Value;
      Idxs   : GL_Value_Array;
      RHS    : GL_Value;
      VFA    : Boolean := False) return GL_Value
     with Pre => Present (In_LHS) and then Present (RHS);
   --  Perform an indexed store operation with prefix In_LHS, indices Idxs,
   --  and RHS, the data to store.  If we return a value, that's the array
   --  that needs to be stored into the actual LHS. If no value if
   --  returned, all our work is done.

   procedure Build_Indexed_Store
     (LHS  : GL_Value;
      Idxs : GL_Value_Array;
      RHS  : GL_Value;
      VFA  : Boolean := False)
     with Pre => Present (LHS) and then Present (RHS);
   --  Similar to the function version, but we always update LHS

private

   --  A bound of a constrained array can either be a compile-time
   --  constant, which we record as a Uint or some dynamic value that was
   --  known at the declaration of the type, which can include a refdrence
   --  to a discriminant. We use the structures and table below to
   --  indicate which. The value return by Get_Array_Info is the index
   --  into this table for the first index of a constrained array whose
   --  size isn't known at compile-time. The remaining bounds are
   --  subsequent entries in the table.

   type One_Bound is record
      Cnst    : Uint;
      Value   : Opt_N_Subexpr_Id;
   end record
     --  Only one item can be specified. We might think that exactly one
     --  item must be specified, but that's not the case for an
     --  unconstrained array.
     with Predicate => ((if No (Cnst)  then 0 else 1) +
                        (if No (Value) then 0 else 1)) <= 1;

   function Is_Unconstrained (OB : One_Bound) return Boolean is
      (No (OB.Cnst) and then No (OB.Value));
   --  True if the specified bound indicates to get the value from the bounds
   --  of an unconstrained array.

   type Index_Bounds is record
      Bound_GT      : GL_Type;
      Bound_Sub_GT  : GL_Type;
      Low, High     : One_Bound;
      Bound_Range   : GL_Value;
      First_Field   : Nat;
      Not_Superflat : Boolean;
   end record
     with Predicate => Is_Discrete_Type (Bound_GT)
                       and then Is_Discrete_Type (Bound_Sub_GT);

   function Is_FLB (IB : Index_Bounds) return Boolean is
     (not Is_Unconstrained (IB.Low));
   --  True if the specified index has a fixed lower bound

   package Array_Info is new Table.Table
     (Table_Component_Type => Index_Bounds,
      Table_Index_Type     => Array_Info_Id,
      Table_Low_Bound      => Array_Info_Low_Bound,
      Table_Initial        => 1024,
      Table_Increment      => 100,
      Table_Name           => "Array_Info_Table");
   --  Table of representation of arrays indices

end GNATLLVM.Arrays;
