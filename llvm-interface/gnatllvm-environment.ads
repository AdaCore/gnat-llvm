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

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Atree; use Atree;
with Einfo; use Einfo;
with Table; use Table;
with Types; use Types;
with Namet; use Namet;

with LLVM.Target; use LLVM.Target;
with LLVM.Types; use LLVM.Types;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with System; use System;

package GNATLLVM.Environment is

   No_Value_T : constant Value_T := Value_T (Null_Address);
   No_Type_T : constant Type_T := Type_T (Null_Address);
   No_BB_T : constant Basic_Block_T := Basic_Block_T (Null_Address);
   No_Metadata_T : constant Metadata_T := Metadata_T (Null_Address);
   --  Constant for null objects of various LLVM types

   type Field_Info is record
      Containing_Struct_Index : Nat;
      Index_In_Struct : Nat;
      Entity : Entity_Id;
      LLVM_Type : Type_T;
   end record;

   package Field_Info_Vectors is new Ada.Containers.Vectors (Pos, Field_Info);

   type Struct_Info is record
      LLVM_Type : Type_T;
      Preceding_Fields : Field_Info_Vectors.Vector;
   end record;

   package Struct_Info_Vectors is
     new Ada.Containers.Vectors (Pos, Struct_Info);

   package Field_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Entity_Id, Element_Type => Field_Info);

   --  Keep information about record types, to link their front-end
   --  representation to their LLVM representations
   --  TODO: Document the LLVM representation of records here
   type Record_Info is record
      Fields : Field_Maps.Map;
      Structs : Struct_Info_Vectors.Vector;
      Dynamic_Size : Boolean := False;
   end record;

   package Name_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Name_Id,
      Element_Type => Nat);

   --  Expanded Ada-to-LLVM translation context: gathers global information
   type Environ_Record;
   type Environ is access all Environ_Record;

   --  For each GNAT entity, we store various information.  Not all of this
   --  information is used for each Ekind.
   type LLVM_Info is record
      Value            : Value_T;
      --  The LLVM Value corresponding to this entity, if a value

      Typ              : Type_T;
      --  The LLVM Type corresponding to this entity, if a type.  Set for
      --  all types.  If the GNAT type doesn't correspond directly to an LLVM
      --  type (e.g., variable size arrays and records), this is an opaque
      --  type and we get the information from other fields of this record.

      Array_Bound_Info : Nat;
      --  For constrained arrays of type not of known size, an index into
      --  bounds information maintained in GNATLLVM.Arrays.

      TBAA        : Metadata_T;
      --  An LLVM TBAA Metadata node corresponding to the type.  Set only
      --  for types that are sufficiently primitive.

      Basic_Block : Basic_Block_T;
      --  For labels and loop ids, records the corresponding basic block

      Record_Inf  : Record_Info;
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

   type Exit_Point is record
      Label_Entity : Entity_Id;
      Exit_BB      : Basic_Block_T;
   end record;

   Exit_Point_Low_Bound : constant := 1;

   package Exit_Point_Table is new Table.Table
     (Table_Component_Type => Exit_Point,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => Exit_Point_Low_Bound,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Exit_Point_Table");
   --  Table of scoped loop exit points. Last inserted exit point correspond
   --  to the innermost loop.

   type Environ_Record (Max_Nodes : Node_Id) is record
      Ctx                       : LLVM.Types.Context_T;
      Bld                       : Builder_T;
      MDBld                     : MD_Builder_T;
      Mdl                       : LLVM.Types.Module_T;
      TBAA_Root                 : LLVM.Types.Metadata_T;
      Module_Data_Layout        : LLVM.Target.Target_Data_T;
      --  Pure-LLVM environment : LLVM context, instruction builder, current
      --  module, and current module data layout.

      Func                      : Value_T;
      --  LLVM value for current function.

      Activation_Rec_Param      : Value_T;
      --  Parameter to this subprogram that represents an activtion record.

      Default_Alloc_Fn          : Value_T;
      Memory_Cmp_Fn             : Value_T;
      Memory_Copy_Fn            : Value_T;
      Memory_Move_Fn            : Value_T;
      Memory_Set_Fn             : Value_T;
      Stack_Save_Fn             : Value_T;
      Stack_Restore_Fn          : Value_T;
      LCH_Fn                    : Value_T;

      In_Main_Unit              : Boolean := False;
      Special_Elaboration_Code  : Boolean := False;
      Current_Elab_Entity       : Node_Id := Empty;

      LLVM_Info                 : LLVM_Info_Array (First_Node_Id .. Max_Nodes);
   end record;

   function Library_Level (Env : Environ) return Boolean;

   No_Such_Type        : exception;
   No_Such_Value       : exception;
   No_Such_Basic_Block : exception;

   function Has_Type        (Env : Environ; TE : Entity_Id) return Boolean
     with Pre => Env /= null and then Is_Type (TE);

   function Has_TBAA        (Env : Environ; TE : Entity_Id) return Boolean
     with Pre => Env /= null and then Is_Type (TE);

   function Has_Value       (Env : Environ; VE : Entity_Id) return Boolean
     with Pre => Env /= null and then Present (VE);

   function Has_BB          (Env : Environ; BE : Entity_Id) return Boolean
     with Pre => Env /= null and then Present (BE);

   function Get_Type        (Env : Environ; TE : Entity_Id) return Type_T
     with Pre => Env /= null and then Is_Type (TE);

   function Get_TBAA        (Env : Environ; TE : Entity_Id) return Metadata_T
     with Pre => Env /= null and then Is_Type (TE);

   function Get_Value       (Env : Environ; VE : Entity_Id) return Value_T
     with Pre => Env /= null and then Present (VE);

   function Get_Array_Info (Env : Environ; TE : Entity_Id) return Nat
     with Pre => Env /= null and then Is_Array_Type (TE);

   function Get_Record_Info (Env : Environ; TE : Entity_Id) return Record_Info
     with Pre => Env /= null and then Is_Record_Type (TE);

   function Get_Basic_Block
     (Env : Environ; BE : Entity_Id) return Basic_Block_T
     with Pre => Env /= null and then Present (BE);

   procedure Set_Type (Env : Environ; TE : Entity_Id; TL : Type_T)
     with Pre  => Env /= null and then Is_Type (TE) and then TL /= No_Type_T,
          Post => Get_Type (Env, TE) = TL;

   procedure Set_TBAA (Env : Environ; TE : Entity_Id; TBAA : Metadata_T)
     with Pre  => Env /= null and then Is_Type (TE)
                  and then TBAA /= No_Metadata_T,
          Post => Get_TBAA (Env, TE) = TBAA;

   procedure Set_Value (Env : Environ; VE : Entity_Id; VL : Value_T)
     with Pre  => Env /= null and then Present (VE) and then VL /= No_Value_T,
          Post => Get_Value (Env, VE) = VL;

   procedure Set_Array_Info (Env : Environ; TE : Entity_Id; AI : Nat)
     with Pre  => Env /= null and then Is_Array_Type (TE),
          Post => Get_Array_Info (Env, TE) = AI;

   procedure Set_Record_Info (Env : Environ; TE : Entity_Id; RI : Record_Info)
     with Pre  => Env /= null and then Is_Record_Type (TE),
          Post => Get_Record_Info (Env, TE) = RI;

   procedure Set_Basic_Block
     (Env : Environ; BE : Entity_Id; BL : Basic_Block_T)
     with Pre  => Env /= null and then Present (BE),
          Post => Get_Basic_Block (Env, BE) = BL;

   procedure Copy_Type_Info (Env : Environ; Old_T, New_T : Entity_Id)
     with Pre  => Env /= null and then Has_Type (Env, Old_T),
          Post => Has_Type (Env, New_T);
   --  Copy type-related information from Old_T to New_T

   procedure Push_Loop (LE : Entity_Id; Exit_Point : Basic_Block_T)
     with Pre => Exit_Point /= No_BB_T;
   procedure Pop_Loop;

   function Get_Exit_Point (LE : Entity_Id) return Basic_Block_T;
   function Get_Exit_Point return Basic_Block_T
     with Post => Get_Exit_Point'Result /= No_BB_T;

   procedure Enter_Subp (Env : Environ; Func : Value_T)
     with Pre  => Env /= null and then Func /= No_Value_T
                  and then Library_Level (Env),
          Post => not Library_Level (Env);
   --  Create an entry basic block for this subprogram and position
   --  the builder at its end. Mark that we're in a subprogram.  To be
   --  used when starting the compilation of a subprogram body.

   procedure Leave_Subp (Env : Environ)
     with Pre  => Env /= null and not Library_Level (Env),
          Post => Library_Level (Env);
   --  Indicate that we're no longer compiling a subprogram.

   function Create_Basic_Block
     (Env : Environ; Name : String) return Basic_Block_T
     with Pre  => Env /= null,
          Post => Create_Basic_Block'Result /= No_BB_T;

   function Get_Fullest_View (E : Entity_Id) return Entity_Id is
   (if Ekind (E) in Incomplete_Kind and then From_Limited_With (E)
    then Non_Limited_View (E)
    elsif Present (Full_View (E))
    then Full_View (E)
    elsif Ekind (E) in Private_Kind
      and then Present (Underlying_Full_View (E))
    then Underlying_Full_View (E)
    else E);

end GNATLLVM.Environment;
