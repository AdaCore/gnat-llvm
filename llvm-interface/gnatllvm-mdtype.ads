------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                          --
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

package GNATLLVM.MDType is

   --  This package defines a "mid-level" type that's used to track the
   --  low-level, LLVM, type of a value. It's at the semantic level of LLVM
   --  types, but records information that's not visible in an LLVM type,
   --  namely signedness, volatility, field names, whether an array is zero
   --  length or variable, and what type a pointer points to.
   --
   --  These types are only needed for CCG, but we need them in GNAT LLVM
   --  proper in order to properly track them. We have only one instance
   --  of identical types.
   --
   --  All implementation details are in the package body and only creation
   --  and accessors are defined here.

   MD_Type_Low_Bound  : constant := 1_000_000_000;
   MD_Type_High_Bound : constant := 1_099_999_999;
   type MD_Type is range MD_Type_Low_Bound .. MD_Type_High_Bound;
   No_MD_Type         : constant MD_Type := MD_Type_Low_Bound;

   function No      (MDT : MD_Type) return Boolean is (MDT =  No_MD_Type);
   function Present (MDT : MD_Type) return Boolean is (MDT /= No_MD_Type);

   procedure Discard (MDT : MD_Type) is null;

   type MD_Type_Array is array (Nat range <>) of MD_Type;

   --  Define MD_type versions of types that we use throughout

   Bit_MD      : MD_Type := No_MD_Type;
   Byte_MD     : MD_Type := No_MD_Type;
   Size_MD     : MD_Type := No_MD_Type;
   Max_Int_MD  : MD_Type := No_MD_Type;
   Void_Ptr_MD : MD_Type := No_MD_Type;
   Address_MD  : MD_Type := No_MD_Type;
   SSI_MD      : MD_Type := No_MD_Type;
   Int_32_MD   : MD_Type := No_MD_Type;
   Int_64_MD   : MD_Type := No_MD_Type;
   Boolean_MD  : MD_Type := No_MD_Type;

   --  Accessor functions for MD_Type, starting with predicates for
   --  type kind.

   function Is_Void (MDT : MD_Type) return Boolean
     with Pre => Present (MDT);
   function Is_Integer (MDT : MD_Type) return Boolean
     with Pre => Present (MDT);
   function Is_Signed (MDT : MD_Type) return Boolean
     with Pre => Is_Integer (MDT);
   function Is_Unsigned (MDT : MD_Type) return Boolean
     with Pre => Is_Integer (MDT);
   function Is_Float (MDT : MD_Type) return Boolean
     with Pre => Present (MDT);
   function Is_Array (MDT : MD_Type) return Boolean
     with Pre => Present (MDT);
   function Is_Fixed_Array (MDT : MD_Type) return Boolean
     with Pre => Is_Array (MDT);
   function Is_Variable_Array (MDT : MD_Type) return Boolean
     with Pre => Is_Array (MDT);
   function Is_Function_Type (MDT : MD_Type) return Boolean
     with Pre => Present (MDT);
   function Is_Varargs_Function (MDT : MD_Type) return Boolean
     with Pre => Is_Function_Type (MDT);
   function Is_Struct (MDT : MD_Type) return Boolean
     with Pre => Present (MDT);
   function Is_Packed (MDT : MD_Type) return Boolean
     with Pre => Is_Struct (MDT);
   function Is_Pointer (MDT : MD_Type) return Boolean
     with Pre => Present (MDT);

   function Is_Same_Kind (MDT1, MDT2 : MD_Type) return Boolean
     with Pre => Present (MDT1) and then Present (MDT2);
   --  True if both types have the same kind

   function Is_Volatile (MDT : MD_Type) return Boolean
     with Pre => Present (MDT);
   function MD_Name (MDT : MD_Type) return Name_Id
     with Pre => Present (MDT);
   function Has_Name (MDT : MD_Type) return Boolean is
     (Present (MD_Name (MDT)))
     with Pre => Present (MDT);

   function Have_Fields (MDT : MD_Type) return Boolean
     with Pre => Is_Struct (MDT);
   --  True if we've called Struct_Create_Name but not yet Struct_Set_Body

   function LLVM_Type_Of (MDT : MD_Type) return Type_T
     with Pre => Present (MDT), Post => Present (LLVM_Type_Of'Result);
   --  Create an LLVM type from an MD_Type
   function "+" (MDT : MD_Type) return Type_T renames LLVM_Type_Of;

   function From_Type (T : Type_T) return MD_Type
     with Pre => Present (T), Post => +From_Type'Result = T;
   --  Create a MD_Type from a type. This is mostly used for intrinsic
   --  functions and isn't guaranteed to work in all cases.

   function Int_Bits (MDT : MD_Type) return Nat
     with Pre => Is_Integer (MDT), Post => Int_Bits'Result /= 0;
   function Float_Bits (MDT : MD_Type) return Nat
     with Pre => Is_Float (MDT), Post => Float_Bits'Result /= 0;
   --  Number of bits for integer and float types

   function Pointer_Space (MDT : MD_Type) return Nat
     with Pre => Is_Pointer (MDT);
   --  Address space for a pointer type

   function Array_Count (MDT : MD_Type) return Nat
     with Pre => Is_Fixed_Array (MDT);
   --  Number of elements for a fixed-size array

   function Element_Count (MDT : MD_Type) return Nat
     with Pre => Is_Struct (MDT) and then Have_Fields (MDT);
   --  Number of elements in a structure

   function Parameter_Count (MDT : MD_Type) return Nat
     with Pre => Is_Function_Type (MDT);
   --  Number of parameters in a function type

   function Element_Type (MDT : MD_Type) return MD_Type
     with Pre => Is_Array (MDT);
   --  Type for the elements of an array

   function Designated_Type (MDT : MD_Type) return MD_Type
     with Pre => Is_Pointer (MDT);
   --  Type that a pointer is referencing

   function Return_Type (MDT : MD_Type) return MD_Type
     with Pre => Is_Function_Type (MDT);
   --  Return type of a function

   function Element_Name (MDT : MD_Type; Idx : Nat) return Name_Id
     with Pre =>  Is_Struct (MDT) and then Idx < Element_Count (MDT);
   function Element_Type (MDT : MD_Type; Idx : Nat) return MD_Type
     with Pre =>  Is_Struct (MDT) and then Idx < Element_Count (MDT),
          Post => Present (Element_Type'Result);
   --  Name and type, respectively, for structs. Idx is 0-origin.

   function Parameter_Type (MDT : MD_Type; Idx : Nat) return MD_Type
     with Pre =>  Is_Function_Type (MDT) and then Idx < Parameter_Count (MDT),
          Post => Present (Parameter_Type'Result);
   --  Parameter type for a function type

   function Atomic_Kind (MDT : MD_Type) return Boolean is
     (Is_Integer (MDT) or else Is_Float (MDT) or else Is_Pointer (MDT))
   with Pre => Present (MDT);
   --  Return True if the type is valid for an atomic operation

   --  Operations on LLVM types that we can call on an MD_Type

   function Get_Type_Size (MDT : MD_Type) return ULL
     with Pre => Present (MDT);
   function Get_Scalar_Bit_Size (MDT : MD_Type) return ULL
     with Pre => Present (MDT);
   function Get_Type_Alignment (MDT : MD_Type) return Nat
     with Pre => Present (MDT);

   --  Now functions to create types

   function Void_Ty return MD_Type
     with Post => Is_Void (Void_Ty'Result);
   --  Create a void type. We'll end up with only one of them

   function Int_Ty (Bits : Nat; Unsigned : Boolean := False) return MD_Type
     with Post => Is_Unsigned (Int_Ty'Result) = Unsigned
                  and then Int_Bits (Int_Ty'Result) = Bits;
   --  Make an integer type with specified bitsize and signedness.

   function Float_Ty (Bits : Nat) return MD_Type
     with Post => Is_Float (Float_Ty'Result)
                  and then Float_Bits (Float_Ty'Result) = Bits;
   --  Make a float type with the corresponding number of bits

   function Pointer_Type
     (Elem_Type : MD_Type;
      Space     : Nat := Address_Space) return MD_Type
     with Pre  => Present (Elem_Type),
          Post => Is_Pointer (Pointer_Type'Result)
                  and then Pointer_Space (Pointer_Type'Result) = Space;
   --  Make a pointer type to the specified type in the specifed address space

   function Array_Type (Elem_Type : MD_Type; Count : Nat) return MD_Type
     with Pre  => Present (Elem_Type),
          Post => Is_Fixed_Array (Array_Type'Result)
                  and then Array_Count (Array_Type'Result) = Count
                  and then Element_Type (Array_Type'Result) = Elem_Type;
   --  Make a fixed-size array with the specifed count and element type

   function Variable_Array_Type (Elem_Type : MD_Type) return MD_Type
     with Post => Is_Variable_Array (Variable_Array_Type'Result)
                  and then Element_Type (Variable_Array_Type'Result) =
                Elem_Type;
   --  Make a variable-sized array type

   function Struct_Create_Named (Name : Name_Id) return MD_Type
     with Post => Is_Struct (Struct_Create_Named'Result)
                  and then MD_Name (Struct_Create_Named'Result) = Name
                  and then not Have_Fields (Struct_Create_Named'Result);
   --  Create a named struct. These aren't merged because we may have
   --  two structs of the same name at this level.

   type Field_Id_Array is array (Int range <>) of Opt_Record_Field_Kind_Id;

   function Build_Struct_Type
     (Types       : MD_Type_Array;
      Field_Names : Name_Id_Array;
      Fields      : Field_Id_Array := (1 .. 0 => Empty);
      Packed      : Boolean := False;
      Name        : Name_Id := No_Name) return MD_Type
     with Pre  => Field_Names'First = Types'First
                  and then Field_Names'Last = Types'Last
                  and then (for all MDT of Types => Present (MDT)),
          Post => Is_Struct (Build_Struct_Type'Result)
                  and then Is_Packed (Build_Struct_Type'Result) = Packed
                  and then MD_Name (Build_Struct_Type'Result) = Name
                  and then (for all J in Field_Names'Range =>
                              Element_Name (Build_Struct_Type'Result,
                                            J - Field_Names'First) =
                              Field_Names (J)
                              and then Element_Type
                                (Build_Struct_Type'Result,
                                   J - Field_Names'First) = Types (J));
   --  Create a Struct type with the specified field types and field names,
   --  also specifying if it's packed and it's name, if any.

   procedure Struct_Set_Body
     (MDT    : MD_Type;
      Types  : MD_Type_Array;
      Names  : Name_Id_Array;
      Fields : Field_Id_Array := (1 .. 0 => Empty);
      Packed : Boolean := False)
     with Pre  => Names'First = Types'First and then Names'Last = Types'Last
                  and then (for all F_MDT of Types => Present (F_MDT))
                  and then Is_Struct (MDT) and then not Have_Fields (MDT),
          Post => Is_Packed (MDT) = Packed and then Have_Fields (MDT)
                  and then (for all J in Names'Range =>
                              Element_Name (MDT, J - Names'First) =
                              Names (J)
                              and then Element_Type (MDT,
                                                     J - Names'First) =
                                       Types (J));
   --  Similar to Build_Struct_Type, but modify a type created with
   --  Struct_Create_Named.

   function Fn_Ty
     (Arg_Types   : MD_Type_Array;
      Return_Type : MD_Type;
      Varargs     : Boolean := False) return MD_Type
   with Pre  => Present (Return_Type)
                and then (for all MDT of Arg_Types => Present (MDT)),
     Post => Is_Function_Type (Fn_Ty'Result)
             and then Is_Varargs_Function (Fn_Ty'Result) = Varargs
             and then Parameter_Count (Fn_Ty'Result) = Arg_Types'Length
             and then (for all J in Arg_Types'Range =>
                         Parameter_Type (Fn_Ty'Result,
                                         J - Arg_Types'First) =
                           Arg_Types (J));
   --  Make a function type with the specified return and argument types

   function Name_Type (MDT : MD_Type; New_Name : Name_Id) return MD_Type
     with Pre => Present (MDT), Post => MD_Name (Name_Type'Result) = New_Name;
   --  Create a copy of MDT that has the specified name

   function Make_Volatile (MDT : MD_Type) return MD_Type
     with Pre => Present (MDT), Post => Is_Volatile (Make_Volatile'Result);
   --  Create a copy of MDT that's marked as volatile

   pragma Annotate (Xcov, Exempt_On, "Debug helpers");

   function To_String (MDT : MD_Type; Top : Boolean := False) return String
     with Pre => Present (MDT);

   procedure Dump_MD_Type (MDT : MD_Type)
     with Export, External_Name => "dmdt";

   pragma Annotate (Xcov, Exempt_Off, "Debug helpers");

end GNATLLVM.MDType;
