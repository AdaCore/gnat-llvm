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

with Table; use Table;
with Types; use Types;
with Namet; use Namet;

with LLVM.Target; use LLVM.Target;
with LLVM.Types; use LLVM.Types;

with GNATLLVM.Builder;

package GNATLLVM.Environment is

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

   type Exit_Point is record
      Label_Entity : Entity_Id;
      Exit_BB      : Basic_Block_T;
   end record;

   package Exit_Point_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Exit_Point);

   --  Expanded Ada-to-LLVM translation context: gathers global information
   type Environ_Record;
   type Environ is access all Environ_Record;

   type LLVM_Info is record
      Value       : Value_T;
      Typ         : Type_T;
      Basic_Block : Basic_Block_T;
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

   type Subp_Env is record
      Env                    : Environ;
      Func                   : Value_T;
      Saved_Builder_Position : Basic_Block_T;
      Activation_Rec_Param   : Value_T;
   end record;

   package Subp_Table is new Table.Table
     (Table_Component_Type => Subp_Env,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 2,
      Table_Increment      => 2,
      Table_Name           => "Subp_Table");

   type Environ_Record (Max_Nodes : Node_Id) is record
      Ctx                       : LLVM.Types.Context_T;
      Bld                       : GNATLLVM.Builder.Builder;
      Mdl                       : LLVM.Types.Module_T;
      Module_Data_Layout        : LLVM.Target.Target_Data_T;
      --  Pure-LLVM environment : LLVM context, instruction builder, current
      --  module, and current module data layout.

      Exit_Points               : Exit_Point_Vectors.Vector;
      --  Stack of scoped loop exit points. Last inserted exit point correspond
      --  to the innermost loop.

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

   function Library_Level return Boolean;

   No_Such_Type        : exception;
   No_Such_Value       : exception;
   No_Such_Basic_Block : exception;

   function Has_Type
     (Env : access Environ_Record; TE : Entity_Id) return Boolean;
   function Has_Value
     (Env : access Environ_Record; VE : Entity_Id) return Boolean;
   function Has_BB
     (Env : access Environ_Record; BE : Entity_Id) return Boolean;
   function Get (Env : access Environ_Record; TE : Entity_Id) return Type_T;
   function Get (Env : access Environ_Record; VE : Entity_Id) return Value_T;
   function Get
     (Env : access Environ_Record; BE : Entity_Id) return Basic_Block_T;
   function Get
     (Env : access Environ_Record; RI : Entity_Id) return Record_Info;

   procedure Set
     (Env : access Environ_Record; TE : Entity_Id; RI : Record_Info);
   procedure Set (Env : access Environ_Record; TE : Entity_Id; TL : Type_T);
   procedure Set (Env : access Environ_Record; VE : Entity_Id; VL : Value_T);
   procedure Set
     (Env : access Environ_Record; BE : Entity_Id; BL : Basic_Block_T);

   procedure Push_Loop
     (Env : access Environ_Record; LE : Entity_Id; Exit_Point : Basic_Block_T);
   procedure Pop_Loop (Env : access Environ_Record);
   function Get_Exit_Point
     (Env : access Environ_Record; LE : Entity_Id) return Basic_Block_T;
   function Get_Exit_Point
     (Env : access Environ_Record) return Basic_Block_T;

   function Enter_Subp
     (Env       : access Environ_Record;
      Func      : Value_T) return Subp_Env;
   --  Create, push and return a subprogram environment. Also create an entry
   --  basic block for this subprogram and position the builder at its end. To
   --  be used when starting the compilation of a subprogram body.

   procedure Leave_Subp (Env  : access Environ_Record);
   --  Pop and free the most recent subprogram environment. Restore the
   --  previous builder position, if any. To be used when finishing the
   --  compilation of a subprogram body.

   function Create_Basic_Block
     (Env : access Environ_Record; Name : String) return Basic_Block_T;

   function Current_Subp return Subp_Env;

end GNATLLVM.Environment;
