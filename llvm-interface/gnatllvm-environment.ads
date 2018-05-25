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

with Table; use Table;

with LLVM.Core;   use LLVM.Core;

with GNATLLVM.GLValue; use GNATLLVM.GLValue;

package GNATLLVM.Environment is

   --  Define bounds and types for record and field information

   Record_Info_Low_Bound  : constant := 300_000_000;
   Record_Info_High_Bound : constant := 399_999_999;
   type Record_Info_Id is
     range Record_Info_Low_Bound .. Record_Info_High_Bound;
   First_Record_Info_Id   : constant Record_Info_Id := Record_Info_Low_Bound;
   Empty_Record_Info_Id   : constant Record_Info_Id := First_Record_Info_Id;

   Field_Info_Low_Bound  : constant := 400_000_000;
   Field_Info_High_Bound : constant := 499_999_999;
   type Field_Info_Id is range Field_Info_Low_Bound .. Field_Info_High_Bound;
   First_Field_Info_Id   : constant Field_Info_Id := Field_Info_Low_Bound;
   Empty_Field_Info_Id   : constant Field_Info_Id := First_Field_Info_Id;

   function No (R : Record_Info_Id)      return Boolean is
      (R = Empty_Record_Info_Id);
   function No (F : Field_Info_Id)       return Boolean is
      (F = Empty_Field_Info_Id);
   function Present (R : Record_Info_Id) return Boolean is
      (R /= Empty_Record_Info_Id);
   function Present (F : Field_Info_Id)  return Boolean is
      (F /= Empty_Field_Info_Id);

   --  Define information saying how deep we are in the block stack.  The
   --  stack itself is in GNATLLVM.Blocks.

   type Block_Info is record
      Depth    : Integer;   --  Depth in the block stack
      In_Stmts : Boolean;   --  True if in statement part.
   end record;

   No_Block_Info : Block_Info := (-1, False);
   function No      (BI : Block_Info) return Boolean is (BI =  No_Block_Info);
   function Present (BI : Block_Info) return Boolean is (BI /= No_Block_Info);

   --  For each GNAT entity, we store various information.  Not all of this
   --  information is used for each Ekind.

   type LLVM_Info is record
      Value            : GL_Value;
      --  The GL_Value corresponding to this entity, if a value

      Typ              : Type_T;
      --  The LLVM Type corresponding to this entity, if a type.  Set for
      --  all types.  If the GNAT type doesn't correspond directly to an
      --  LLVM type (e.g., some variable size arrays and records), this can
      --  be an opaque type and we get the information from other fields of
      --  this record.

      Is_Dynamic_Size  : Boolean;
      --  True if the size of this type is dynamic.  This is always the case
      --  if the saved type is an opaque type, but if we have an array type
      --  with zero size, we need to use this flag to disambiguate the cases
      --  of a zero-length array and a variable-sized array.

      Array_Bound_Info : Nat;
      --  For arrays, an index into bounds information maintained by
      --  GNATLLVM.Arrays.

      TBAA        : Metadata_T;
      --  An LLVM TBAA Metadata node corresponding to the type.  Set only
      --  For types that are sufficiently primitive.

      Basic_Block : Basic_Block_T;
      --  For labels and loop ids, records the corresponding basic block

      Block_Inf   : Block_Info;
      --  For labeles, records the depth and "in statements" status

      Record_Inf  : Record_Info_Id;
      --  For records, gives the first index of the descriptor of the record

      Field_Inf   : Field_Info_Id;
      --  For fields, gives the index of the descriptor of the field
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

   type LLVM_Info_Array is array (Node_Id range <>) of LLVM_Info_Id;
   type Ptr_LLVM_Info_Array is access all LLVM_Info_Array;

   LLVM_Info_Map             : Ptr_LLVM_Info_Array;
   --  The mapping between a GNAT tree object and the corresponding LLVM data

   function Library_Level return Boolean;

   function Get_Type        (TE : Entity_Id) return Type_T
     with Pre => Is_Type (TE);

   function Is_Dynamic_Size (TE : Entity_Id) return Boolean
     with Pre => Is_Type (TE);

   function Get_TBAA        (TE : Entity_Id) return Metadata_T
     with Pre => Is_Type (TE);

   function Get_Value       (VE : Entity_Id) return GL_Value
     with Pre => Present (VE);

   function Get_Array_Info  (TE : Entity_Id) return Nat
     with Pre => Is_Array_Type (TE);

   function Get_Record_Info (TE : Entity_Id) return Record_Info_Id
     with Pre => Is_Record_Type (TE);

   function Get_Field_Info (VE : Entity_Id)  return Field_Info_Id
     with Pre => Ekind_In (VE, E_Discriminant, E_Component);

   function Get_Basic_Block (BE : Entity_Id) return Basic_Block_T
     with Pre => Present (BE);

   function Get_Block_Info  (BE : Entity_Id) return Block_Info
     with Pre => Present (BE);

   function Has_Type        (TE : Entity_Id) return Boolean is
      (Present (Get_Type (TE)))
     with Pre => Is_Type (TE);

   function Has_TBAA        (TE : Entity_Id) return Boolean is
      (Present (Get_TBAA (TE)))
     with Pre => Is_Type (TE);

   function Has_Value       (VE : Entity_Id) return Boolean is
      (Present (Get_Value (VE)))
     with Pre => Present (VE);

   function Has_BB          (BE : Entity_Id) return Boolean is
      (Present (Get_Basic_Block (BE)))
     with Pre => Present (BE);

   function Has_Block_Info  (BE : Entity_Id) return Boolean is
      (Present (Get_Block_Info (BE)))
     with Pre => Present (BE);

   function Has_Record_Info (TE : Entity_Id) return Boolean is
      (Present (Get_Record_Info (TE)))
     with Pre => Is_Record_Type (TE);

   function Has_Field_Info (VE : Entity_Id)  return Boolean is
      (Present (Get_Field_Info (VE)))
     with Pre => Ekind_In (VE, E_Discriminant, E_Component);

   procedure Set_Type       (TE : Entity_Id; TL : Type_T)
     with Pre  => Is_Type (TE) and then Present (TL),
          Post => Get_Type (TE) = TL;

   procedure Set_Dynamic_Size (TE : Entity_Id; B : Boolean)
     with Pre  => Is_Type (TE) and then Has_Type (TE),
          Post => Is_Dynamic_Size (TE) = B;

   procedure Set_TBAA (TE : Entity_Id; TBAA : Metadata_T)
     with Pre  => Is_Type (TE) and then Present (TBAA) and then Has_Type (TE),
          Post => Get_TBAA (TE) = TBAA;

   procedure Set_Value (VE : Entity_Id; VL : GL_Value)
     with Pre  => Present (VE) and then Present (VL),
          Post => Get_Value (VE) = VL;

   procedure Set_Array_Info (TE : Entity_Id; AI : Nat)
     with Pre  => Is_Array_Type (TE) and then Has_Type (TE),
          Post => Get_Array_Info (TE) = AI;

   procedure Set_Record_Info (TE : Entity_Id; RI : Record_Info_Id)
     with Pre  => Is_Record_Type (TE) and then Has_Type (TE),
          Post => Get_Record_Info (TE) = RI;

   procedure Set_Field_Info (VE : Entity_Id; FI : Field_Info_Id)
     with Pre  => Ekind_In (VE, E_Discriminant, E_Component),
          Post => Get_Field_Info (VE) = FI;

   procedure Set_Basic_Block (BE : Entity_Id; BL : Basic_Block_T)
     with Pre => Present (BE), Post => Get_Basic_Block (BE) = BL;

   procedure Set_Block_Info  (BE : Entity_Id; BI : Block_Info)
     with Pre => Present (BE), Post => Get_Block_Info (BE) = BI;

   procedure Copy_Type_Info (Old_T, New_T : Entity_Id)
     with Pre  => Has_Type (Old_T), Post => Has_Type (New_T);
   --  Copy type-related information from Old_T to New_T

   procedure Enter_Subp (Func : GL_Value)
     with Pre  => Present (Func) and then Library_Level,
          Post => not Library_Level;
   --  Create an entry basic block for this subprogram and position
   --  the builder at its end. Mark that we're in a subprogram.  To be
   --  used when starting the compilation of a subprogram body.

   procedure Leave_Subp
     with Pre  => not Library_Level,
          Post => Library_Level;
   --  Indicate that we're no longer compiling a subprogram

   function Create_Basic_Block (Name : String := "") return Basic_Block_T
     with Post => Present (Create_Basic_Block'Result);

   function Get_Insert_Block return Basic_Block_T is
     (Get_Insert_Block (IR_Builder))
     with Post => Present (Get_Insert_Block'Result);
end GNATLLVM.Environment;
