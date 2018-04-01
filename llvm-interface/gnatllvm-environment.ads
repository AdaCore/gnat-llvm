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
with Namet; use Namet;
with Table; use Table;
with Types; use Types;

with LLVM.Target; use LLVM.Target;
with LLVM.Types; use LLVM.Types;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with System; use System;

package GNATLLVM.Environment is

   No_Value_T : constant Value_T := Value_T (Null_Address);
   No_Type_T : constant Type_T := Type_T (Null_Address);
   No_BB_T : constant Basic_Block_T := Basic_Block_T (Null_Address);
   No_Metadata_T : constant Metadata_T := Metadata_T (Null_Address);
   No_Builder_T : constant Builder_T := Builder_T (Null_Address);
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
      --  all types.  If the GNAT type doesn't correspond directly to an
      --  LLVM type (e.g., some variable size arrays and records), this is
      --  an opaque type and we get the information from other fields of
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

      Record_Inf  : Record_Info;
   end record;

   function Is_Type_Or_Void (E : Entity_Id) return Boolean is
     (Ekind (E) = E_Void or else Is_Type (E));
     --  We can have Etype's that are E_Void for E_Procedure; needed for
     --  aspect of below record.

   --  It's not sufficient to just pass around an LLVM Value_T when
   --  generating code because there's a lot of information lost about the
   --  value and where it came from.  Contrast with Gigi, where we pass around
   --  a GCC tree node, which already has a lot of information, and which we
   --  further annotate with flags.  So we pass the following record:

   type GL_Value is record
      Value        : Value_T;
      --  The LLVM value that was generated

      Typ          : Entity_Id;
      --  The GNAT type of this value.

      Is_Reference : Boolean;
      --  If True, this is actually a pointer to Typ, so Value's type is
      --  actually an E_Access_Type (not provided) whose Designated_Type
      --  is Typ.

      Is_Raw_Array : Boolean;
      --  If True, means that, even though the type here is unconstrained,
      --  we've extracted the actual address of the array.
   end record
     with Dynamic_Predicate => (No (GL_Value.Value) and then No (Gl_Value.Typ))
                               or else (Present (GL_Value.Value)
                                          and then Is_Type_Or_Void
                                             (GL_Value.Typ));

   type GL_Value_Array is array (Nat range <>) of GL_Value;

   No_GL_Value : constant GL_Value := (No_Value_T, Empty, False, False);

   function No (G : GL_Value) return Boolean      is (G = No_GL_Value);
   function Present (G : GL_Value) return Boolean is (G /= No_GL_Value);

   function Is_Reference (G : GL_Value) return Boolean is (G.Is_Reference);
   function Is_Raw_Array (G : GL_Value) return Boolean is (G.Is_Raw_Array);

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
      --  Parameter to this subprogram, if any, that represents an
      --  activtion record.

      Return_Address_Param      : GL_Value;
      --  Parameter to this subprogram, if any, that represent the address
      --  to which we are to copy the return value.

      Size_Type                 : Entity_Id;
      LLVM_Size_Type            : Type_T;
      --  Types to use for sizes.

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

   function Get_Type         (Env : Environ; TE : Entity_Id) return Type_T
     with Pre => Env /= null and then Is_Type (TE);

   function Is_Dynamic_Size  (Env : Environ; TE : Entity_Id) return Boolean
     with Pre => Env /= null and then Is_Type (TE);

   function Get_TBAA         (Env : Environ; TE : Entity_Id) return Metadata_T
     with Pre => Env /= null and then Is_Type (TE);

   function Get_Value       (Env : Environ; VE : Entity_Id) return Value_T
     with Pre => Env /= null and then Present (VE);

   function Get_Array_Info  (Env : Environ; TE : Entity_Id) return Nat
     with Pre => Env /= null and then Is_Array_Type (TE);

   function Get_Record_Info (Env : Environ; TE : Entity_Id) return Record_Info
     with Pre => Env /= null and then Is_Record_Type (TE);

   function Get_Basic_Block
     (Env : Environ; BE : Entity_Id) return Basic_Block_T
     with Pre => Env /= null and then Present (BE);

   function Has_Type        (Env : Environ; TE : Entity_Id) return Boolean is
      (Present (Get_Type (Env, TE)))
     with Pre => Env /= null and then Is_Type (TE);

   function Has_TBAA        (Env : Environ; TE : Entity_Id) return Boolean is
      (Present (Get_TBAA (Env, TE)))
     with Pre => Env /= null and then Is_Type (TE);

   function Has_Value       (Env : Environ; VE : Entity_Id) return Boolean is
      (Present (Get_Value (Env, VE)))
     with Pre => Env /= null and then Present (VE);

   function Has_BB           (Env : Environ; BE : Entity_Id) return Boolean is
      (Present (Get_Basic_Block (Env, BE)))
     with Pre => Env /= null and then Present (BE);

   procedure Set_Type (Env : Environ; TE : Entity_Id; TL : Type_T)
     with Pre  => Env /= null and then Is_Type (TE) and then Present (TL),
          Post => Get_Type (Env, TE) = TL;

   procedure Set_Dynamic_Size (Env : Environ; TE : Entity_Id; B : Boolean)
     with Pre  => Env /= null and then Is_Type (TE)
                  and then Has_Type (Env, TE),
          Post => Is_Dynamic_Size (Env, TE) = B;

   procedure Set_TBAA (Env : Environ; TE : Entity_Id; TBAA : Metadata_T)
     with Pre  => Env /= null and then Is_Type (TE)
                  and then Present (TBAA) and then Has_Type (Env, TE),
          Post => Get_TBAA (Env, TE) = TBAA;

   procedure Set_Value (Env : Environ; VE : Entity_Id; VL : Value_T)
     with Pre  => Env /= null and then Present (VE) and then Present (VL),
          Post => Get_Value (Env, VE) = VL;

   procedure Set_Array_Info (Env : Environ; TE : Entity_Id; AI : Nat)
     with Pre  => Env /= null and then Is_Array_Type (TE)
                  and then Has_Type (Env, TE),
          Post => Get_Array_Info (Env, TE) = AI;

   procedure Set_Record_Info (Env : Environ; TE : Entity_Id; RI : Record_Info)
     with Pre  => Env /= null and then Is_Record_Type (TE)
                  and then Has_Type (Env, TE),
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
     with Pre => Present (Exit_Point);
   procedure Pop_Loop;

   function Get_Exit_Point (LE : Entity_Id) return Basic_Block_T;
   function Get_Exit_Point return Basic_Block_T
     with Post => Present (Get_Exit_Point'Result);

   procedure Enter_Subp (Env : Environ; Func : Value_T)
     with Pre  => Env /= null and then Present (Func)
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
          Post => Present (Create_Basic_Block'Result);

end GNATLLVM.Environment;
