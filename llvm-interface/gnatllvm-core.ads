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

with System; use System;

with Atree; use Atree;
with Einfo; use Einfo;
with Types; use Types;

with LLVM.Target; use LLVM.Target;
with LLVM.Types;  use LLVM.Types;

package GNATLLVM.Core is

   --  This package contains very low-level types, objects, and functions,
   --  mostly corresponding to LLVM objects, and exists to avoid circular
   --  dependencies between other specs.  The intent is that every child
   --  package of LLVM with's this child, but that this with no other
   --  children.

   type MD_Builder_T is new System.Address;
   --  Metadata builder type: opaque for us

   type Value_Array is array (Nat range <>) of Value_T;
   type Basic_Block_Array is array (Nat range <>) of Basic_Block_T;

   No_Value_T    : constant Value_T       := Value_T (Null_Address);
   No_Type_T     : constant Type_T        := Type_T (Null_Address);
   No_BB_T       : constant Basic_Block_T := Basic_Block_T (Null_Address);
   No_Metadata_T : constant Metadata_T    := Metadata_T (Null_Address);
   No_Builder_T  : constant Builder_T     := Builder_T (Null_Address);
   --  Constant for null objects of various LLVM types

   function No (V : Value_T) return Boolean            is (V = No_Value_T);
   function No (T : Type_T) return Boolean             is (T = No_Type_T);
   function No (B : Basic_Block_T) return Boolean      is (B = No_BB_T);
   function No (M : Metadata_T) return Boolean         is (M = No_Metadata_T);
   function No (M : Builder_T) return Boolean          is (M = No_Builder_T);

   function Present (V : Value_T) return Boolean       is (V /= No_Value_T);
   function Present (T : Type_T) return Boolean        is (T /= No_Type_T);
   function Present (B : Basic_Block_T) return Boolean is (B /= No_BB_T);
   function Present (M : Metadata_T) return Boolean    is (M /= No_Metadata_T);
   function Present (M : Builder_T) return Boolean     is (M /= No_Builder_T);
   --  Test for presence and absence of field of LLVM types

   function Is_Type_Or_Void (E : Entity_Id) return Boolean is
     (Ekind (E) = E_Void or else Is_Type (E));
   --  We can have Etype's that are E_Void for E_Procedure

   LLVM_Context             : Context_T;
   --  The current LLVM Context

   IR_Builder               : Builder_T;
   --  The current LLVM Instruction builder

   LLVM_Module              : Module_T;
   --  The LLVM Module being compiled

   MD_Builder               : MD_Builder_T;
   --  The current LLVM Metadata builder

   TBAA_Root                : Metadata_T;
   --  Root of tree for Type-Based alias Analysis (TBAA) metadata

   Module_Data_Layout       : Target_Data_T;
   --  LLVM current module data layout.

   In_Main_Unit             : Boolean := False;
   --  True if we're currently processing the main unit

   Special_Elaboration_Code : Boolean := False;
   --  True if we're compiling an elaboration procedure

   Size_Type                : Entity_Id;
   LLVM_Size_Type           : Type_T;
   --  Types to use for sizes

   Void_Ptr_Type            : Type_T;
   --  Pointer to arbitrary memory (we use i8 *); equivalent of
   --  Standard_A_Char.

   Int_32_Type              : Entity_Id;
   --  GNAT type for 32-bit integers (for GEP indexes)

end GNATLLVM.Core;
