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

with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with Sinfo; use Sinfo;
with Uintp; use Uintp;

with LLVM.Core;   use LLVM.Core;

with GNATLLVM.GLValue;      use GNATLLVM.GLValue;
with GNATLLVM.Utils;        use GNATLLVM.Utils;

package GNATLLVM.Types is

   pragma Annotate (Xcov, Exempt_On, "Defensive programming");

   function Create_Access_Type (TE : Entity_Id) return Type_T
     with Pre  => Is_Type (TE),
          Post => Present (Create_Access_Type'Result);
   --  Function that creates the access type for a corresponding type. Since
   --  access types are not just pointers, this is the abstraction bridge
   --  between the two.

   function GNAT_To_LLVM_Type
     (TE : Entity_Id; Definition : Boolean) return Type_T
     with Pre  => Is_Type (TE), Post => Present (GNAT_To_LLVM_Type'Result);

   function Create_Type (TE : Entity_Id) return Type_T is
      (GNAT_To_LLVM_Type (TE, False))
     with Pre => Present (TE), Post => Present (Create_Type'Result);

   function Create_TBAA (TE : Entity_Id) return Metadata_T
     with Pre => Is_Type (TE);

   procedure Create_Discrete_Type
     (TE : Entity_Id; T : out Type_T; Low, High : out GL_Value)
     with Pre  => Ekind (TE) in Discrete_Kind,
          Post => Present (T) and then Present (Low) and then Present (High);

   function Int_Ty (Num_Bits : Natural) return Type_T is
     (Int_Type (unsigned (Num_Bits)))
     with Post => Get_Type_Kind (Int_Ty'Result) = Integer_Type_Kind;

   function Int_Ty (Num_Bits : Uint) return Type_T is
     (Int_Type (unsigned (UI_To_Int (Num_Bits))))
     with Post => Get_Type_Kind (Int_Ty'Result) = Integer_Type_Kind;

   function Fn_Ty (Param_Ty : Type_Array; Ret_Ty : Type_T) return Type_T is
     (Function_Type
        (Ret_Ty, Param_Ty'Address, Param_Ty'Length, False))
     with Pre  => Present (Ret_Ty),
          Post => Get_Type_Kind (Fn_Ty'Result) = Function_Type_Kind;

   function Build_Struct_Type
     (Types : Type_Array; Packed : Boolean := False) return Type_T
     with Post => Present (Build_Struct_Type'Result);
   --  Build an LLVM struct type containing the specified types

   function Get_Fullest_View (TE : Entity_Id) return Entity_Id
     with Pre => Is_Type_Or_Void (TE),
          Post => Is_Type_Or_Void (Get_Fullest_View'Result);
   --  Get the fullest possible view of E, looking through private,
   --  limited, packed array and other implementation types.

   function Ultimate_Base_Type (TE : Entity_Id) return Entity_Id
     with Pre => Is_Type (TE), Post => Is_Type (Ultimate_Base_Type'Result);
   --  Go up TE's Etype chain until it points to itself, which will
   --  go up both base and parent types.

   function Full_Etype (N : Node_Id) return Entity_Id is
     (if Ekind (Etype (N)) = E_Void then Etype (N)
      else Get_Fullest_View (Etype (N)))
     with Pre => Present (N), Post => Is_Type_Or_Void (Full_Etype'Result);

   function Full_Component_Type (E : Entity_Id) return Entity_Id is
     (Get_Fullest_View (Component_Type (E)))
     with Pre  => Is_Array_Type (E),
          Post => Present (Full_Component_Type'Result);

   function Full_Designated_Type (E : Entity_Id) return Entity_Id is
     (Get_Fullest_View (Designated_Type (E)))
     with Pre  => Is_Access_Type (E),
          Post => Present (Full_Designated_Type'Result);

   function Full_Scope (E : Entity_Id) return Entity_Id is
     (Get_Fullest_View (Scope (E)))
     with Pre  => Present (E), Post => Present (Full_Scope'Result);

   function Is_Access_Unconstrained (T : Entity_Id) return Boolean is
     (Is_Access_Type (T) and then Is_Array_Type (Full_Designated_Type (T))
      and then not Is_Constrained (Full_Designated_Type (T)))
     with Pre => Is_Type (T);

   function Convert_To_Elementary_Type
     (V : GL_Value; TE : Entity_Id) return GL_Value
     with Pre  => Is_Elementary_Type (TE) and then Is_Elementary_Type (V),
          Post => Is_Elementary_Type (Convert_To_Elementary_Type'Result);
   --  Convert Expr to the type TE, with both the types of Expr and TE
   --  being elementary.

   function Convert_To_Access_To (V : GL_Value; TE : Entity_Id) return GL_Value
     with Pre  => Present (V) and then Is_Type (TE),
          Post => Is_Access_Type (Convert_To_Access_To'Result);
   --  Convert Src, which should be an access, into an access to Desig_Type

   function Convert_To_Access_To
     (V : GL_Value; T : GL_Value) return GL_Value is
     (Convert_To_Access_To (V, Full_Etype (T)))
     with Pre  => Present (V) and then Present (T),
          Post => Is_Access_Type (Convert_To_Access_To'Result);
   --  Likewise, but get type from V

   function Build_Type_Conversion (N : Node_Id; TE : Entity_Id) return GL_Value
     with Pre  => Is_Type (TE) and then Present (N)
                  and then TE = Get_Fullest_View (TE),
          Post => Present (Build_Type_Conversion'Result);
   --  Emit code to convert Expr to Dest_Type

   function Build_Unchecked_Conversion
     (N : Node_Id; TE : Entity_Id) return GL_Value
     with Pre  => Is_Type (TE) and then TE = Get_Fullest_View (TE)
                  and then Present (N),
          Post => Present (Build_Unchecked_Conversion'Result);
   --  Emit code to emit an unchecked conversion of Expr to Dest_Type

   function Convert_To_Elementary_Type
     (V : GL_Value; T : GL_Value) return GL_Value
   is
     (Convert_To_Elementary_Type (V, Full_Etype (T)))
     with Pre  => Is_Elementary_Type (V) and then Is_Elementary_Type (T),
          Post => Is_Elementary_Type (Convert_To_Elementary_Type'Result);
   --  Variant of above where the type is that of another value (T)

   function Strip_Complex_Conversions (N : Node_Id) return Node_Id;
   --  Remove any conversion from N, if Present, if they are record or array
   --  conversions that increase the complexity of the size of the
   --  type because the caller will be doing any needed conversions.

   function Bounds_To_Length
     (Low, High : GL_Value; TE : Entity_Id) return GL_Value
     with Pre  => Present (Low) and then Present (High) and then Is_Type (TE)
                  and then Type_Of (Low) = Type_Of (High),
          Post => Full_Etype (Bounds_To_Length'Result) = TE;
   --  Low and High are bounds of a discrete type.  Compute the length of
   --  that type, taking into account the superflat case, and do that
   --  computation in TE.  We would like to have the above test be that the
   --  two types be identical, but that's too strict (for example, one
   --  may be Integer and the other Integer'Base), so just check the width.

   function Get_LLVM_Type_Size (T : Type_T) return unsigned_long_long is
     ((Size_Of_Type_In_Bits (Module_Data_Layout, T) + 7) / 8)
     with Pre => Present (T);
   --  Return the size of an LLVM type, in bytes

   function Get_LLVM_Type_Size (T : Type_T) return GL_Value is
     (Const_Int (Size_Type, Get_LLVM_Type_Size (T), False));
   --  Return the size of an LLVM type, in bytes, as an LLVM constant

   function Get_LLVM_Type_Size_In_Bits (T : Type_T) return unsigned_long_long
   is
     (Size_Of_Type_In_Bits (Module_Data_Layout, T))
     with Pre => Present (T);
   --  Return the size of an LLVM type, in bits

   function Get_LLVM_Type_Size_In_Bits (V : GL_Value) return unsigned_long_long
   is
     (Size_Of_Type_In_Bits (Module_Data_Layout, Type_Of (V.Value)))
     with Pre => Present (V);
   --  Return the size of an LLVM type, in bits

   function Get_LLVM_Type_Size_In_Bits (T : Type_T) return GL_Value is
     (Const_Int (Size_Type, Get_LLVM_Type_Size_In_Bits (T), False))
     with Pre  => Present (T),
          Post => Present (Get_LLVM_Type_Size_In_Bits'Result);
   --  Return the size of an LLVM type, in bits, as an LLVM constant

   function Get_LLVM_Type_Size_In_Bits (TE : Entity_Id) return GL_Value
     with Pre  => Present (TE),
          Post => Present (Get_LLVM_Type_Size_In_Bits'Result);
   --  Likewise, but convert from a GNAT type

   function Get_LLVM_Type_Size_In_Bits (V : GL_Value) return GL_Value is
     (Get_LLVM_Type_Size_In_Bits (V.Typ))
     with Pre  => Present (V),
          Post => Present (Get_LLVM_Type_Size_In_Bits'Result);
   --  Variant of above to get type from a GL_Value

   function Allocate_For_Type
     (TE         : Entity_Id;
      Alloc_Type : Entity_Id;
      V          : GL_Value := No_GL_Value;
      Name       : String := "") return GL_Value
     with Pre  => Is_Type (TE) and then Is_Type (Alloc_Type),
          Post => Is_Access_Type (Allocate_For_Type'Result);
   --  Allocate space on the stack for an object of type TE and return
   --  a pointer to the space.  Name is the name to use for the LLVM
   --  value.  If Value is Present, it's a value to be copyied to the
   --  temporary and can be used to size the allocated space.

   function Heap_Allocate_For_Type
     (TE         : Entity_Id;
      Alloc_Type : Entity_Id;
      V          : GL_Value  := No_GL_Value;
      Proc       : Entity_Id := Empty;
      Pool       : Entity_Id := Empty) return GL_Value
     with Pre  => Is_Type (TE) and then Is_Type (Alloc_Type)
                  and then (No (Proc) or else Present (Pool)),
          Post => Is_Access_Type (Heap_Allocate_For_Type'Result);
   --  Similarly, but allocate storage on the heap.  This will handle
   --  default allocation, secondary stack, and storage pools.

   procedure Heap_Deallocate (V : GL_Value; Proc : Entity_Id; Pool : Entity_Id)
     with Pre => Present (V)
                  and then (No (Proc) or else Present (Pool));
   --  Free memory allocated by Heap_Allocate_For_Type

   function Convert_To_Size_Type (V : GL_Value) return GL_Value
     with Pre  => Present (V),
          Post => Type_Of (Convert_To_Size_Type'Result) = LLVM_Size_Type;
   --  Convert V to Size_Type.  This is always Size_Type's width, but may
   --  actually be a different GNAT type.

   function Get_Type_Alignment (T : Type_T) return unsigned is
     (ABI_Alignment_Of_Type (Module_Data_Layout, T))
     with Pre => Present (T);
   --  Return the size of an LLVM type, in bits

   function Get_Type_Alignment (TE : Entity_Id) return unsigned
     with Pre => Is_Type (TE);
   --  Return the size of a GNAT type, in bits

   function Get_Type_Size
     (TE       : Entity_Id;
      V        : GL_Value := No_GL_Value;
      For_Type : Boolean  := False) return GL_Value
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

   function Compute_Alignment
     (Left_Type, Right_Type : Entity_Id) return unsigned is
     (unsigned'Max (Get_Type_Alignment (Left_Type),
                    Get_Type_Alignment (Right_Type)))
     with Pre  => Is_Type (Left_Type) and then Is_Type (Right_Type);
   --  Likewise, but compute strictest alignment in bits

   function Get_Type_Size_Complexity
     (TE : Entity_Id; For_Type : Boolean := False) return Natural
     with Pre  => Is_Type (TE);
   --  Return the complexity of computing the size of a type.  This roughly
   --  gives the number of "things" needed to access to compute the size.
   --  This returns zero iff the type is of a constant size.

end GNATLLVM.Types;
