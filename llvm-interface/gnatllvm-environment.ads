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

with Types; use Types;
with Namet; use Namet;

with LLVM.Target; use LLVM.Target;
with LLVM.Types; use LLVM.Types;
with Ada.Containers.Doubly_Linked_Lists;
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

   package Record_Info_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Entity_Id,
      Element_Type => Record_Info);

   package Type_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Entity_Id,
      Element_Type => Type_T);

   package Value_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Entity_Id,
      Element_Type => Value_T);

   package Entity_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Entity_Id,
      Element_Type => Nat);

   package Name_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Name_Id,
      Element_Type => Nat);

   type Scope_Type is record
      Types         : Type_Maps.Map;
      Values        : Value_Maps.Map;
      --  Types and values registered so far for this scope

      Records_Infos : Record_Info_Maps.Map;
   end record;
   type Scope_Acc is access Scope_Type;

   package Scope_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Scope_Acc);

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

   type Subp_Env_Record is record
      Env                    : Environ;
      Func                   : Value_T;
      Saved_Builder_Position : Basic_Block_T;
   end record;
   type Subp_Env is access all Subp_Env_Record;

   package Subp_Lists is new Ada.Containers.Doubly_Linked_Lists (Subp_Env);

   type Environ_Record is record
      Ctx                       : LLVM.Types.Context_T;
      Bld                       : GNATLLVM.Builder.Builder;
      Mdl                       : LLVM.Types.Module_T;
      Module_Data_Layout        : LLVM.Target.Target_Data_T;
      --  Pure-LLVM environment : LLVM context, instruction builder, current
      --  module, and current module data layout.

      Scopes                    : Scope_Vectors.Vector;
      --  Stack of scopes, to associate LLVM types/values to expansed tree's
      --  entities.

      Exit_Points               : Exit_Point_Vectors.Vector;
      --  Stack of scoped loop exit points. Last inserted exit point correspond
      --  to the innermost loop.

      Subprograms               : Subp_Lists.List;
      Current_Subps             : Subp_Lists.List;
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
   end record;

   procedure Push_Scope (Env : access Environ_Record);
   procedure Pop_Scope (Env : access Environ_Record);

   function Library_Level (Env : access Environ_Record) return Boolean;

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

   function Current_Subp (Env : access Environ_Record) return Subp_Env;

end GNATLLVM.Environment;
