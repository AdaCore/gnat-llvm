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

   function No      (MD : MD_Type) return Boolean is (MD =  No_MD_Type);
   function Present (MD : MD_Type) return Boolean is (MD /= No_MD_Type);

   procedure Discard (MD : MD_Type) is null;

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

   function Is_Void (MD : MD_Type) return Boolean
     with Pre => Present (MD);
   function Is_Integer (MD : MD_Type) return Boolean
     with Pre => Present (MD);
   function Is_Signed (MD : MD_Type) return Boolean
     with Pre => Is_Integer (MD);
   function Is_Unsigned (MD : MD_Type) return Boolean
     with Pre => Is_Integer (MD);
   function Is_Unknown_Sign (MD : MD_Type) return Boolean
     with Pre => Is_Integer (MD);
   function Is_Float (MD : MD_Type) return Boolean
     with Pre => Present (MD);
   function Is_Array (MD : MD_Type) return Boolean
     with Pre => Present (MD);
   function Is_Fixed_Array (MD : MD_Type) return Boolean
     with Pre => Is_Array (MD);
   function Is_Variable_Array (MD : MD_Type) return Boolean
     with Pre => Is_Array (MD);
   function Is_Function_Type (MD : MD_Type) return Boolean
     with Pre => Present (MD);
   function Is_Varargs_Function (MD : MD_Type) return Boolean
     with Pre => Is_Function_Type (MD);
   function Is_Struct (MD : MD_Type) return Boolean
     with Pre => Present (MD);
   function Is_Packed (MD : MD_Type) return Boolean
     with Pre => Is_Struct (MD);
   function Is_Pointer (MD : MD_Type) return Boolean
     with Pre => Present (MD);

   function Is_Same_Kind (MD1, MD2 : MD_Type) return Boolean
     with Pre => Present (MD1) and then Present (MD2);
   --  True if both types have the same kind

   function Is_Volatile (MD : MD_Type) return Boolean
     with Pre => Present (MD);
   function MD_Name (MD : MD_Type) return Name_Id
     with Pre => Present (MD);
   function Has_Name (MD : MD_Type) return Boolean is
     (Present (MD_Name (MD)))
     with Pre => Present (MD);

   function Has_Fields (MD : MD_Type) return Boolean
     with Pre => Is_Struct (MD);
   --  True if we've called Struct_Create_Name but not yet Struct_Set_Body

   function LLVM_Type_Of (MD : MD_Type) return Type_T
     with Pre => Present (MD), Post => Present (LLVM_Type_Of'Result);
   --  Create an LLVM type from an MD_Type
   function "+" (MD : MD_Type) return Type_T renames LLVM_Type_Of;

   function Contains_Void (MD : MD_Type) return Boolean
     with Pre => Present (MD);
   --  True if any part of MD is void, with the exception of a pointer
   --  to void.

   function Check_From_Type (T1, T2 : Type_T) return Boolean
     with Pre => Present (T1) and then Present (T2);
   --  Used to check the result of the next function. This is a renaming
   --  of Is_Layout_Identical, but we 'with' that unit here.

   function From_Type (T : Type_T) return MD_Type
     with Pre  => Present (T),
          Post => Contains_Void (From_Type'Result)
                  or else Check_From_Type (T, +From_Type'Result);
   --  Create an MD_Type from a type. This is used for intrinsic functions
   --  and values created by the optimzer and isn't guaranteed to work in
   --  all cases. If T was a named struct type, the LLVM type generated
   --  from the result may not precisely agree with the input and likewise
   --  for an array of such, so we just check that the layout is the same.
   --  We also can return Void for a number of reasons, so we allow that
   --  to always be valid.

   function Is_Layout_Identical (MD1, MD2 : MD_Type) return Boolean
     with Pre => Present (MD1) and then Present (MD2);
   --  Return True iff types MD1 and MD2 have identical layouts.

   function Int_Bits (MD : MD_Type) return Nat
     with Pre => Is_Integer (MD), Post => Int_Bits'Result /= 0;
   function Float_Bits (MD : MD_Type) return Nat
     with Pre => Is_Float (MD), Post => Float_Bits'Result /= 0;
   --  Number of bits for integer and float types

   function Pointer_Space (MD : MD_Type) return Nat
     with Pre => Is_Pointer (MD);
   --  Address space for a pointer type

   function Array_Count (MD : MD_Type) return Nat
     with Pre => Is_Fixed_Array (MD);
   --  Number of elements for a fixed-size array

   function Element_Count (MD : MD_Type) return Nat
     with Pre => Is_Struct (MD) and then Has_Fields (MD);
   --  Number of elements in a structure

   function Parameter_Count (MD : MD_Type) return Nat
     with Pre => Is_Function_Type (MD);
   --  Number of parameters in a function type

   function Element_Type (MD : MD_Type) return MD_Type
     with Pre => Is_Array (MD);
   --  Type for the elements of an array

   function Designated_Type (MD : MD_Type) return MD_Type
     with Pre => Is_Pointer (MD);
   --  Type that a pointer is referencing

   function Return_Type (MD : MD_Type) return MD_Type
     with Pre => Is_Function_Type (MD);
   --  Return type of a function

   function Element_Name (MD : MD_Type; Idx : Nat) return Name_Id
     with Pre =>  Is_Struct (MD) and then Idx < Element_Count (MD);
   function Element_Type (MD : MD_Type; Idx : Nat) return MD_Type
     with Pre =>  Is_Struct (MD) and then Idx < Element_Count (MD),
          Post => Present (Element_Type'Result);
   function Element_Entity (MD : MD_Type; Idx : Nat) return Entity_Id
     with Pre =>  Is_Struct (MD) and then Idx < Element_Count (MD);
   function Is_Padding (MD : MD_Type; Idx : Nat) return Boolean
     with Pre =>  Is_Struct (MD) and then Idx < Element_Count (MD);
   --  Name, type, and whether field is padding, respectively, for
   --  structs. Idx is 0-origin.

   function Parameter_Name (MD : MD_Type; Idx : Nat) return Name_Id
     with Pre =>  Is_Function_Type (MD) and then Idx < Parameter_Count (MD);
   function Parameter_Type (MD : MD_Type; Idx : Nat) return MD_Type
     with Pre =>  Is_Function_Type (MD) and then Idx < Parameter_Count (MD),
          Post => Present (Parameter_Type'Result);
   --  Parameter type for a function type

   function Atomic_Kind (MD : MD_Type) return Boolean is
     (Is_Integer (MD) or else Is_Float (MD) or else Is_Pointer (MD))
   with Pre => Present (MD);
   --  Return True if the type is valid for an atomic operation

   function Is_Function_Pointer (MD : MD_Type) return Boolean is
     (Is_Pointer (MD) and then Is_Function_Type (Designated_Type (MD)))
     with Pre => Present (MD);

   --  Operations on LLVM types that we can call on an MD_Type

   function Get_Type_Size (MD : MD_Type) return ULL
     with Pre => Present (MD);
   function Get_Scalar_Bit_Size (MD : MD_Type) return ULL
     with Pre => Present (MD);
   function Get_Type_Alignment (MD : MD_Type) return Nat
     with Pre => Present (MD);

   function Make_Volatile (MD : MD_Type; B : Boolean := True) return MD_Type
     with Pre => Present (MD), Post => Is_Volatile (Make_Volatile'Result) = B;
   procedure Make_Volatile (MD : in out MD_Type; B : Boolean := False)
     with Pre => Present (MD), Post => Is_Volatile (MD) = B;
   --  Create a copy of MD that's marked as volatile

   --  Now functions to create types

   function Void_Ty return MD_Type
     with Post => Is_Void (Void_Ty'Result);
   --  Create a void type. We'll end up with only one of them

   function Int_Ty
     (Bits     : Nat;
      Unsigned : Boolean := False;
      Unknown  : Boolean := False) return MD_Type
     with Post => Is_Unsigned (Int_Ty'Result) = Unsigned
                  and then Is_Signed (Int_Ty'Result) =
                           (not Unknown and not Unsigned)
                  and then Is_Unknown_Sign (Int_Ty'Result) = Unknown
                  and then Int_Bits (Int_Ty'Result) = Bits;
   --  Make an integer type with specified bitsize and signedness.

   function Signed_Type (MD : MD_Type) return MD_Type is
     (Make_Volatile (Int_Ty (Int_Bits (MD), Unsigned => False),
                     Is_Volatile (MD)))
     with Pre  => Is_Integer (MD),
          Post => Is_Signed (Signed_Type'Result)
                  and then Is_Volatile (Signed_Type'Result) =
                           Is_Volatile (MD);

   function Unsigned_Type (MD : MD_Type) return MD_Type is
     (Make_Volatile (Int_Ty (Int_Bits (MD), Unsigned => True),
                     Is_Volatile (MD)))
     with Pre  => Is_Integer (MD),
          Post => Is_Unsigned (Unsigned_Type'Result)
                  and then Is_Volatile (Unsigned_Type'Result) =
                           Is_Volatile (MD);
   --  Make signed or unsigned variants of a type

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
                  and then Array_Count (Array_Type'Result) = Count;
   --  ?? for now: and then Element_Type (Array_Type'Result) = Elem_Type;
   --  Make a fixed-size array with the specifed count and element type

   function Variable_Array_Type (Elem_Type : MD_Type) return MD_Type
     with Post => Is_Variable_Array (Variable_Array_Type'Result)
                  and then Element_Type (Variable_Array_Type'Result) =
                Elem_Type;
   --  Make a variable-sized array type

   function Struct_Create_Named (Name : Name_Id) return MD_Type
     with Post => Is_Struct (Struct_Create_Named'Result)
                  and then MD_Name (Struct_Create_Named'Result) = Name
                  and then not Has_Fields (Struct_Create_Named'Result);
   --  Create a named struct. These aren't merged because we may have
   --  two structs of the same name at this level.

   type Field_Id_Array is array (Int range <>) of Opt_Record_Field_Kind_Id;

   function Build_Struct_Type
     (Types       : MD_Type_Array;
      Field_Names : Name_Id_Array;
      Fields      : Field_Id_Array := (1 .. 0 => Empty);
      Padding     : Boolean_Array  := (1 .. 0 => False);
      Packed      : Boolean        := False;
      Name        : Name_Id        := No_Name) return MD_Type
     with Pre  => Field_Names'First = Types'First
                  and then Field_Names'Last = Types'Last
                  and then (for all MD of Types => Present (MD)),
          Post => Is_Struct (Build_Struct_Type'Result)
                  and then Is_Packed (Build_Struct_Type'Result) = Packed
                  and then MD_Name (Build_Struct_Type'Result) = Name
                  and then (for all J in Field_Names'Range =>
                              Element_Name (Build_Struct_Type'Result,
                                            J - Field_Names'First) =
                              Field_Names (J)
                              and then Element_Type
                                        (Build_Struct_Type'Result,
                                          J - Field_Names'First) = Types (J)
                              and then (Fields'Length = 0
                                        or else Element_Entity
                                                  (Build_Struct_Type'Result,
                                                     J - Fields'First) =
                                        Fields (J))
                              and then (Padding'Length = 0
                                        or else Is_Padding
                                                  (Build_Struct_Type'Result,
                                                     J - Padding'First) =
                                        Padding (J)));

   --  Create a Struct type with the specified field types and field names,
   --  also specifying if it's packed and it's name, if any.

   procedure Struct_Set_Body
     (MD      : MD_Type;
      Types   : MD_Type_Array;
      Names   : Name_Id_Array;
      Fields  : Field_Id_Array := (1 .. 0 => Empty);
      Padding : Boolean_Array  := (1 .. 0 => False);
      Packed  : Boolean        := False)
     with Pre  => Names'First = Types'First and then Names'Last = Types'Last
                  and then (for all F_MD of Types => Present (F_MD))
                  and then Is_Struct (MD) and then not Has_Fields (MD),
          Post => Is_Packed (MD) = Packed and then Has_Fields (MD)
                  and then (for all J in Names'Range =>
                              Element_Name (MD, J - Names'First) =
                              Names (J)
                              and then Element_Type (MD,
                                                     J - Names'First) =
                                       Types (J)
                              and then (Fields'Length = 0
                                        or else Element_Entity
                                                  (MD, J - Fields'First) =
                                        Fields (J))
                              and then (Padding'Length = 0
                                        or else Is_Padding
                                                  (MD, J - Padding'First) =
                                        Padding (J)));

   --  Similar to Build_Struct_Type, but modify a type created with
   --  Struct_Create_Named.

   function Fn_Ty
     (Arg_Types   : MD_Type_Array;
      Return_Type : MD_Type;
      Arg_Names   : Name_Id_Array := (1 .. 0 => No_Name);
      Varargs     : Boolean       := False) return MD_Type
   with Pre  => Present (Return_Type)
                and then (for all MD of Arg_Types => Present (MD))
                and then (Arg_Names'Length = 0
                          or else Arg_Names'First = Arg_Types'First)
                and then (Arg_Names'Length = 0
                          or else Arg_Names'Last = Arg_Types'Last),
     Post => Is_Function_Type (Fn_Ty'Result)
             and then Is_Varargs_Function (Fn_Ty'Result) = Varargs
             and then Parameter_Count (Fn_Ty'Result) = Arg_Types'Length
             and then (for all J in Arg_Types'Range =>
                         Parameter_Type (Fn_Ty'Result,
                                         J - Arg_Types'First) =
                           Arg_Types (J))
             and then (Arg_Names'Length = 0
                       or else (for all J in Arg_Names'Range =>
                                  Parameter_Name (Fn_Ty'Result,
                                                  J - Arg_Names'First) =
                                    Arg_Names (J)));
   --  Make a function type with the specified return and argument types

   function Name_Type (MD : MD_Type; New_Name : Name_Id) return MD_Type
     with Pre => Present (MD), Post => MD_Name (Name_Type'Result) = New_Name;
   --  Create a copy of MD that has the specified name

   pragma Annotate (Xcov, Exempt_On, "Debug helpers");

   function To_String (MD : MD_Type; Top : Boolean := False) return String
     with Pre => Present (MD);

   procedure Dump_MD_Type (MD : MD_Type)
     with Export, External_Name => "dmdt";

   pragma Annotate (Xcov, Exempt_Off, "Debug helpers");

end GNATLLVM.MDType;
