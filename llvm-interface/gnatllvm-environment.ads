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

with Repinfo; use Repinfo;
with Table;   use Table;

with GNATLLVM.GLValue; use GNATLLVM.GLValue;

package GNATLLVM.Environment is

   --  Define bounds and types for record, field, and array information

   Record_Info_Low_Bound  : constant := 300_000_000;
   Record_Info_High_Bound : constant := 399_999_999;
   type Record_Info_Id is
     range Record_Info_Low_Bound .. Record_Info_High_Bound;
   First_Record_Info_Id   : constant Record_Info_Id := Record_Info_Low_Bound;
   Empty_Record_Info_Id   : constant Record_Info_Id := First_Record_Info_Id;

   type Record_Info_Id_Array is array (Nat range <>) of Record_Info_Id;

   Field_Info_Low_Bound  : constant := 400_000_000;
   Field_Info_High_Bound : constant := 499_999_999;
   type Field_Info_Id is range Field_Info_Low_Bound .. Field_Info_High_Bound;
   First_Field_Info_Id   : constant Field_Info_Id := Field_Info_Low_Bound;
   Empty_Field_Info_Id   : constant Field_Info_Id := First_Field_Info_Id;

   Array_Info_Low_Bound  : constant := 500_000_000;
   Array_Info_High_Bound : constant := 599_999_999;
   type Array_Info_Id is range Array_Info_Low_Bound .. Array_Info_High_Bound;
   First_Array_Info_Id   : constant Array_Info_Id := Array_Info_Low_Bound;
   Empty_Array_Info_Id   : constant Array_Info_Id := First_Array_Info_Id;

   Label_Info_Low_Bound  : constant := 600_000_000;
   Label_Info_High_Bound : constant := 699_999_999;
   type Label_Info_Id is range Label_Info_Low_Bound .. Label_Info_High_Bound;
   First_Label_Info_Id   : constant Label_Info_Id := Label_Info_Low_Bound;
   Empty_Label_Info_Id   : constant Label_Info_Id := First_Label_Info_Id;

   function "+" (A : Array_Info_Id; N : Nat) return Array_Info_Id is
     (Array_Info_Id (Nat (A) + N));

   function No (R : Record_Info_Id)      return Boolean is
      (R = Empty_Record_Info_Id);
   function No (F : Field_Info_Id)       return Boolean is
      (F = Empty_Field_Info_Id);
   function No (A : Array_Info_Id)       return Boolean is
      (A = Empty_Array_Info_Id);
   function No (L : Label_Info_Id)       return Boolean is
      (L = Empty_Label_Info_Id);
   function Present (R : Record_Info_Id) return Boolean is
      (R /= Empty_Record_Info_Id);
   function Present (F : Field_Info_Id)  return Boolean is
      (F /= Empty_Field_Info_Id);
   function Present (A : Array_Info_Id)  return Boolean is
      (A /= Empty_Array_Info_Id);
   function Present (L : Label_Info_Id)  return Boolean is
      (L /= Empty_Label_Info_Id);

   pragma Inline (No);
   pragma Inline (Present);

   --  For each GNAT entity, we store various information.  Not all of this
   --  information is used for each Ekind.

   type LLVM_Info is record
      Value                 : GL_Value;
      --  The GL_Value corresponding to this entity, if a value

      Typ                   : Type_T;
      --  The LLVM Type corresponding to this entity, if a type.  Set for
      --  all types.  If the GNAT type doesn't correspond directly to an
      --  LLVM type (e.g., some variable size arrays and records), this can
      --  be an opaque type and we get the information from other fields of
      --  this record.

      TBAA                  : Metadata_T;
      --  An LLVM TBAA Metadata node corresponding to the type.  Set only
      --  For types that are sufficiently primitive.

      Is_Nonnative_Type     : Boolean;
      --  True if this GNAT type can't be fully represented as a single
      --  LLVM type. This is always the case if the saved type is an opaque
      --  type, but if we have an array type with zero size, we need to use
      --  this flag to disambiguate the cases of a zero-length array and a
      --  variable-sized array.  This usually, but not always, means that
      --  the type's size is not known at compile time.

      Is_Being_Elaborated   : Boolean;
      --  True if we're in the process of elaborating this type.

      Is_Dummy_Type         : Boolean;
      --  Only set for access types and means that we're using a
      --  temporary value for the type because we're currently
      --  elaborating the designated type.

      Array_Info            : Array_Info_Id;
      --  For arrays, an index into bounds information maintained by
      --  GNATLLVM.Arrays.

      Record_Info           : Record_Info_Id;
      --  For records, gives the first index of the descriptor of the record

      Field_Info            : Field_Info_Id;
      --  For fields, gives the index of the descriptor of the field

      Label_Info            : Label_Info_Id;
      --  For labels, points to information about that label

      Orig_Array_Info       : Array_Info_Id;
      --  For a packed array implementation type, the bound information for
      --  the original array type.

      SO_Info               : Dynamic_SO_Ref;
      --  For an expression, the value returned by Create_Dynamic_SO_Ref,
      --  used for back-annotation purposes.

   end record;

   LLVM_Info_Low_Bound  : constant := 200_000_000;
   LLVM_Info_High_Bound : constant := 299_999_999;
   type LLVM_Info_Id is range LLVM_Info_Low_Bound .. LLVM_Info_High_Bound;
   First_LLVM_Info_Id   : constant LLVM_Info_Id := LLVM_Info_Low_Bound;
   Empty_LLVM_Info_Id   : constant LLVM_Info_Id := First_LLVM_Info_Id;

   package LLVM_Info_Table is new Table.Table
     (Table_Component_Type => LLVM_Info,
      Table_Index_Type     => LLVM_Info_Id'Base,
      Table_Low_Bound      => LLVM_Info_Low_Bound,
      Table_Initial        => 1024,
      Table_Increment      => 100,
      Table_Name           => "LLVM_Info_Table");

   type LLVM_Info_Array is array (Node_Id range <>) of aliased LLVM_Info_Id;
   type Ptr_LLVM_Info_Array is access all LLVM_Info_Array;

   LLVM_Info_Map             : Ptr_LLVM_Info_Array;
   --  The mapping between a GNAT tree object and the corresponding LLVM data

   function Get_Type            (TE : Entity_Id) return Type_T
     with Pre => Is_Type (TE);

   function Is_Nonnative_Type   (TE : Entity_Id) return Boolean
     with Pre => Is_Type (TE);

   function Is_Being_Elaborated (TE : Entity_Id) return Boolean
     with Pre => Is_Type (TE);

   function Is_Dummy_Type       (TE : Entity_Id) return Boolean
     with Pre => Is_Type (TE);

   function Get_TBAA            (TE : Entity_Id) return Metadata_T
     with Pre => Is_Type (TE);

   function Get_Value           (VE : Entity_Id) return GL_Value
     with Pre => Present (VE);

   function Get_SO_Ref          (N : Node_Id)    return Dynamic_SO_Ref
     with Pre => Present (N);

   function Get_Array_Info      (TE : Entity_Id) return Array_Info_Id
     with Pre => Is_Array_Type (TE);

   function Get_Orig_Array_Info (TE : Entity_Id) return Array_Info_Id
     with Pre => Is_Packed_Array_Impl_Type (TE);

   function Get_Record_Info (TE : Entity_Id) return Record_Info_Id
     with Pre => Is_Record_Type (TE);

   function Get_Field_Info      (VE : Entity_Id)  return Field_Info_Id
     with Pre => Ekind_In (VE, E_Discriminant, E_Component);

   function Get_Label_Info      (VE : Entity_Id)  return Label_Info_Id
     with Pre => Present (VE);

   procedure Set_Type            (TE : Entity_Id; TL : Type_T)
     with Pre  => Is_Type (TE)
                  and then (Present (TL) or else Present (Get_Type (TE)))
                  and then (No (Get_Type (TE)) or else Get_Type (TE) = TL
                              or else TL = No_Type_T
                              or else Is_Access_Type (TE)),
          Post => Get_Type (TE) = TL;

   procedure Set_Is_Nonnative_Type (TE : Entity_Id; B : Boolean := True)
     with Pre  => Is_Type (TE) and then Present (Get_Type (TE)),
          Post => Is_Nonnative_Type (TE) = B;

   procedure Set_Is_Being_Elaborated (TE : Entity_Id; B : Boolean)
     with Pre  => Is_Type (TE), Post => Is_Being_Elaborated (TE) = B;

   procedure Set_Is_Dummy_Type   (TE : Entity_Id; B : Boolean)
     with Pre  => Is_Type (TE), Post => Is_Dummy_Type (TE) = B;

   procedure Set_TBAA            (TE : Entity_Id; TBAA : Metadata_T)
     with Pre  => Is_Type (TE) and then Present (TBAA)
                  and then Present (Get_Type (TE)),
          Post => Get_TBAA (TE) = TBAA;

   procedure Set_Value_R         (VE : Entity_Id; VL : GL_Value)
     with Pre  => Present (VE) and then Present (VL)
                  and then (No (Get_Value (VE)) or else Get_Value (VE) = VL),
          Post => Get_Value (VE) = VL;

   procedure Set_SO_Ref          (N : Node_Id; U : Dynamic_SO_Ref)
     with Pre  => Present (N) and then U /= No_Uint
                  and then (Get_SO_Ref (N) = No_Uint
                              or else Get_SO_Ref (N) = U),
          Post => Get_SO_Ref (N) = U;

   procedure Set_Array_Info      (TE : Entity_Id; AI : Array_Info_Id)
     with Pre  => Is_Array_Type (TE) and then Present (Get_Type (TE))
                  and then (No (Get_Array_Info (TE))
                              or else Get_Array_Info (TE) = AI),
          Post => Get_Array_Info (TE) = AI;

   procedure Set_Orig_Array_Info (TE : Entity_Id; AI : Array_Info_Id)
     with Pre  => Is_Packed_Array_Impl_Type (TE)
                  and then Present (Get_Type (TE))
                  and then (No (Get_Orig_Array_Info (TE))
                              or else Get_Orig_Array_Info (TE) = AI),
          Post => Get_Orig_Array_Info (TE) = AI;

   procedure Set_Record_Info     (TE : Entity_Id; RI : Record_Info_Id)
     with Pre  => Is_Record_Type (TE)
                  and then (No (Get_Record_Info (TE))
                              or else Get_Record_Info (TE) = RI),
          Post => Get_Record_Info (TE) = RI;

   procedure Set_Field_Info      (VE : Entity_Id; FI : Field_Info_Id)
     with Pre  => Ekind_In (VE, E_Discriminant, E_Component)
                  and then (No (Get_Field_Info (VE))
                              or else Get_Field_Info (VE) = FI),
          Post => Get_Field_Info (VE) = FI;

   procedure Set_Label_Info      (VE : Entity_Id; LI : Label_Info_Id)
     with Pre  => Present (VE)
                  and then (No (Get_Label_Info (VE))
                              or else Get_Label_Info (VE) = LI),
          Post => Get_Label_Info (VE) = LI;

   pragma Inline (Get_Type);
   pragma Inline (Is_Nonnative_Type);
   pragma Inline (Is_Being_Elaborated);
   pragma Inline (Is_Dummy_Type);
   pragma Inline (Get_TBAA);
   pragma Inline (Get_Value);
   pragma Inline (Get_SO_Ref);
   pragma Inline (Get_Array_Info);
   pragma Inline (Get_Orig_Array_Info);
   pragma Inline (Get_Record_Info);
   pragma Inline (Get_Label_Info);
   pragma Inline (Set_Type);
   pragma Inline (Set_Is_Nonnative_Type);
   pragma Inline (Set_Is_Being_Elaborated);
   pragma Inline (Set_Is_Dummy_Type);
   pragma Inline (Set_TBAA);
   pragma Inline (Set_Value_R);
   pragma Inline (Set_SO_Ref);
   pragma Inline (Set_Array_Info);
   pragma Inline (Set_Orig_Array_Info);
   pragma Inline (Set_Record_Info);
   pragma Inline (Set_Label_Info);

end GNATLLVM.Environment;
