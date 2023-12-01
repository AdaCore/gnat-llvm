------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2023, AdaCore                     --
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

with GNATLLVM.GLType;  use GNATLLVM.GLType;
with GNATLLVM.Types;   use GNATLLVM.Types;

package body GNATLLVM.Environment is

   --  For each GNAT entity, we store various information. Not all of this
   --  information is used for each Ekind.

   type LLVM_Data is record
      Value                 : GL_Value;
      --  The GL_Value corresponding to this entity, if a value

      GLType                : GL_Type;
      --  The head of the GL_Type chain for this entity, if a type

      Associated_GL_Type    : GL_Type;
      --  For arrays, the GL_Type for the component of the array. For
      --  access types, the GL_Type of the Designated type. In both cases,
      --  this takes into account any Component_Size clause.

      TBAA                  : Metadata_T;
      --  An LLVM TBAA Metadata node corresponding to the type. Set only
      --  For types that are sufficiently primitive.

      Is_Nonnative_Type     : Boolean;
      --  True if this GNAT type can't be fully represented as a single
      --  LLVM type. This is always the case if the saved type is an opaque
      --  type, but if we have an array type with zero size, we need to use
      --  this flag to disambiguate the cases of a zero-length array and a
      --  variable-sized array. This usually, but not always, means that
      --  the type's size is not known at compile time.

      Is_Being_Elaborated   : Boolean;
      --  True if we're in the process of elaborating this type.

      Debug_Type            : Metadata_T;
      --  Cache for debug information for this entity, if it's a type.
      --  LLVM will also cache this, but it'll save us the time of
      --  recomputing debug info, especially for complex types.

      Array_Info            : Array_Info_Id;
      --  For arrays, an index into bounds information maintained by
      --  GNATLLVM.Arrays.

      Record_Info           : Record_Info_Id;
      --  For records, gives the first index of the descriptor of the record

      Field_Info            : Field_Info_Id;
      --  For fields, gives the index of the descriptor of the field

      Label_Info            : Label_Info_Id;
      --  For labels, points to information about that label

      TBAA_Array_Info       : TBAA_Info_Id;
      --  For arrays, points to various TBAA information related to the array

      Orig_Array_Info       : Array_Info_Id;
      --  For a packed array implementation type, the bound information for
      --  the original array type.

      SO_Info               : Dynamic_SO_Ref;
      --  For an expression, the value returned by Create_Dynamic_SO_Ref,
      --  used for back-annotation purposes.

      Subprogram_Type       : Type_T;
      --  For a subprogram or subprogram type, the LLVM type created

      Flag1                 : Boolean;
      --  Used for multiple purposes, depending on Ekind

   end record;

   LLVM_Info_Low_Bound  : constant := 200_000_000;
   LLVM_Info_High_Bound : constant := 299_999_999;
   type LLVM_Info_Id is range LLVM_Info_Low_Bound .. LLVM_Info_High_Bound;
   First_LLVM_Info_Id   : constant LLVM_Info_Id := LLVM_Info_Low_Bound;
   Empty_LLVM_Info_Id   : constant LLVM_Info_Id := First_LLVM_Info_Id;

   package LLVM_Info is new Table.Table
     (Table_Component_Type => LLVM_Data,
      Table_Index_Type     => LLVM_Info_Id,
      Table_Low_Bound      => LLVM_Info_Low_Bound,
      Table_Initial        => 1024,
      Table_Increment      => 100,
      Table_Name           => "LLVM_Info");

   type LLVM_Info_Array is array (Node_Id range <>) of aliased LLVM_Info_Id;
   type Ptr_LLVM_Info_Array is access all LLVM_Info_Array;

   LLVM_Info_Map             : Ptr_LLVM_Info_Array;
   --  The mapping between a GNAT tree object and the corresponding LLVM data

   type Access_LLVM_Data is access all LLVM_Data;

   function Get_LLVM_Info (TE : Void_Or_Type_Kind_Id) return Access_LLVM_Data
     with Inline;
   function Get_LLVM_Info_For_Set (N : Node_Id)  return Access_LLVM_Data
     with Inline;
   --  Helpers for below to either create type and then return entry or
   --  or to allocate LLVM_Info_Table entry if needed (for set). In either
   --  case, the returned access type must only be used immediately since
   --  it will be invalid if the table is reallocated.

   --  Define functions to get values from LLVM_Info

   function Raw_Get_GLT    (LI : Access_LLVM_Data) return GL_Type is
     (LI.GLType);
   function Raw_Get_AGLT   (LI : Access_LLVM_Data) return GL_Type is
     (LI.Associated_GL_Type);
   function Raw_Get_Value  (LI : Access_LLVM_Data) return GL_Value is
     (LI.Value);
   function Raw_Get_SO     (LI : Access_LLVM_Data) return Dynamic_SO_Ref is
     (LI.SO_Info);
   function Raw_Get_Elab   (LI : Access_LLVM_Data) return Boolean is
     (LI.Is_Being_Elaborated);
   function Raw_Get_Field  (LI : Access_LLVM_Data) return Field_Info_Id is
     (LI.Field_Info);
   function Raw_Get_Label  (LI : Access_LLVM_Data) return Label_Info_Id is
     (LI.Label_Info);
   function Raw_Get_NN     (LI : Access_LLVM_Data) return Boolean is
     (LI.Is_Nonnative_Type);
   function Raw_Get_TBAA   (LI : Access_LLVM_Data) return Metadata_T is
     (LI.TBAA);
   function Raw_Get_TBAA_I (LI : Access_LLVM_Data) return TBAA_Info_Id is
     (LI.TBAA_Array_Info);
   function Raw_Get_Debug  (LI : Access_LLVM_Data) return Metadata_T is
     (LI.Debug_Type);
   function Raw_Get_Array  (LI : Access_LLVM_Data) return Array_Info_Id is
     (LI.Array_Info);
   function Raw_Get_O_A    (LI : Access_LLVM_Data) return Array_Info_Id is
     (LI.Orig_Array_Info);
   function Raw_Get_Record (LI : Access_LLVM_Data) return Record_Info_Id is
     (LI.Record_Info);
   function Raw_Get_S_T    (LI : Access_LLVM_Data) return Type_T is
     (LI.Subprogram_Type);
   function Raw_Get_Flag1  (LI : Access_LLVM_Data) return Boolean is
     (LI.Flag1);

   --  Define procedures to set values into LLVM_Info

   procedure Raw_Set_GLT    (LI : Access_LLVM_Data; Val : GL_Type)
       with Inline;
   procedure Raw_Set_AGLT   (LI : Access_LLVM_Data; Val : GL_Type)
       with Inline;
   procedure Raw_Set_Value  (LI : Access_LLVM_Data; Val : GL_Value)
       with Inline;
   procedure Raw_Set_SO     (LI : Access_LLVM_Data; Val : Dynamic_SO_Ref)
       with Inline;
   procedure Raw_Set_Elab   (LI : Access_LLVM_Data; Val : Boolean)
       with Inline;
   procedure Raw_Set_Field  (LI : Access_LLVM_Data; Val : Field_Info_Id)
       with Inline;
   procedure Raw_Set_Label  (LI : Access_LLVM_Data; Val : Label_Info_Id)
       with Inline;
   procedure Raw_Set_NN     (LI : Access_LLVM_Data; Val : Boolean)
       with Inline;
   procedure Raw_Set_TBAA   (LI : Access_LLVM_Data; Val : Metadata_T)
       with Inline;
   procedure Raw_Set_TBAA_I (LI : Access_LLVM_Data; Val : TBAA_Info_Id)
       with Inline;
   procedure Raw_Set_Debug  (LI : Access_LLVM_Data; Val : Metadata_T)
       with Inline;
   procedure Raw_Set_Array  (LI : Access_LLVM_Data; Val : Array_Info_Id)
       with Inline;
   procedure Raw_Set_O_A    (LI : Access_LLVM_Data; Val : Array_Info_Id)
       with Inline;
   procedure Raw_Set_Record (LI : Access_LLVM_Data; Val : Record_Info_Id)
       with Inline;
   procedure Raw_Set_S_T    (LI : Access_LLVM_Data; Val : Type_T)
       with Inline;
   procedure Raw_Set_Flag1  (LI : Access_LLVM_Data; Val : Boolean)
       with Inline;

   procedure Raw_Set_GLT    (LI : Access_LLVM_Data; Val : GL_Type) is
   begin LI.GLType := Val; end Raw_Set_GLT;

   procedure Raw_Set_AGLT   (LI : Access_LLVM_Data; Val : GL_Type) is
   begin LI.Associated_GL_Type := Val; end Raw_Set_AGLT;

   procedure Raw_Set_Value  (LI : Access_LLVM_Data; Val : GL_Value) is
   begin LI.Value := Val; end Raw_Set_Value;

   procedure Raw_Set_SO     (LI : Access_LLVM_Data; Val : Dynamic_SO_Ref) is
   begin LI.SO_Info := Val; end Raw_Set_SO;

   procedure Raw_Set_Elab   (LI : Access_LLVM_Data; Val : Boolean) is
   begin LI.Is_Being_Elaborated := Val; end Raw_Set_Elab;

   procedure Raw_Set_Field  (LI : Access_LLVM_Data; Val : Field_Info_Id) is
   begin LI.Field_Info := Val; end Raw_Set_Field;

   procedure Raw_Set_Label  (LI : Access_LLVM_Data; Val : Label_Info_Id) is
   begin LI.Label_Info := Val; end Raw_Set_Label;

   procedure Raw_Set_NN     (LI : Access_LLVM_Data; Val : Boolean) is
   begin LI.Is_Nonnative_Type := Val; end Raw_Set_NN;

   procedure Raw_Set_TBAA   (LI : Access_LLVM_Data; Val : Metadata_T) is
   begin LI.TBAA := Val; end Raw_Set_TBAA;

   procedure Raw_Set_TBAA_I (LI : Access_LLVM_Data; Val : TBAA_Info_Id) is
   begin LI.TBAA_Array_Info := Val; end Raw_Set_TBAA_I;

   procedure Raw_Set_Debug  (LI : Access_LLVM_Data; Val : Metadata_T) is
   begin LI.Debug_Type := Val; end Raw_Set_Debug;

   procedure Raw_Set_Array  (LI : Access_LLVM_Data; Val : Array_Info_Id) is
   begin LI.Array_Info := Val; end Raw_Set_Array;

   procedure Raw_Set_O_A    (LI : Access_LLVM_Data; Val : Array_Info_Id) is
   begin LI.Orig_Array_Info := Val; end Raw_Set_O_A;

   procedure Raw_Set_Record (LI : Access_LLVM_Data; Val : Record_Info_Id) is
   begin LI.Record_Info := Val; end Raw_Set_Record;

   procedure Raw_Set_S_T    (LI : Access_LLVM_Data; Val : Type_T) is
   begin LI.Subprogram_Type := Val; end Raw_Set_S_T;

   procedure Raw_Set_Flag1  (LI : Access_LLVM_Data; Val : Boolean) is
   begin LI.Flag1 := Val; end Raw_Set_Flag1;

   ----------------------------
   -- Initialize_Environment --
   ----------------------------

   procedure Initialize_Environment is
   begin
      --  We can't use a qualified expression here because that will
      --  cause a temporary to be placed in our stack and if the array
      --  is very large, it will blow our stack.

      LLVM_Info_Map := new LLVM_Info_Array (First_Node_Id .. Last_Node_Id);
      for J in LLVM_Info_Map'Range loop
         LLVM_Info_Map (J) := Empty_LLVM_Info_Id;
      end loop;
   end Initialize_Environment;

   -------------------
   -- Get_LLVM_Info --
   -------------------

   function Get_LLVM_Info (TE : Void_Or_Type_Kind_Id) return Access_LLVM_Data
   is
      GT : constant GL_Type := Default_GL_Type (TE, Create => False);

   begin
      --  If we're not already elaborating TE and we either don't already
      --  have a type or we have a dummy type, do elaborate it.

      if not Is_Being_Elaborated (TE)
        and then (No (GT) or else Is_Dummy_Type (GT))
      then
         Discard (Type_Of (TE));
      end if;

      return LLVM_Info.Table (LLVM_Info_Map (TE))'Unrestricted_Access;
   end Get_LLVM_Info;

   ---------------------------
   -- Get_LLVM_Info_For_Set --
   ---------------------------

   function Get_LLVM_Info_For_Set (N : Node_Id) return Access_LLVM_Data is
      Id : LLVM_Info_Id := LLVM_Info_Map (N);

   begin
      if Id = Empty_LLVM_Info_Id then
         LLVM_Info.Append ((Value               => No_GL_Value,
                            GLType              => No_GL_Type,
                            Associated_GL_Type  => No_GL_Type,
                            TBAA                => No_Metadata_T,
                            TBAA_Array_Info     => Empty_TBAA_Info_Id,
                            Is_Nonnative_Type   => False,
                            Is_Being_Elaborated => False,
                            Record_Info         => Empty_Record_Info_Id,
                            Field_Info          => Empty_Field_Info_Id,
                            Debug_Type          => No_Metadata_T,
                            Array_Info          => Empty_Array_Info_Id,
                            Label_Info          => Empty_Label_Info_Id,
                            Orig_Array_Info     => Empty_Array_Info_Id,
                            SO_Info             => No_Uint,
                            Subprogram_Type     => No_Type_T,
                            Flag1               => False));
         Id := LLVM_Info.Last;
         LLVM_Info_Map (N) := Id;
      end if;

      return LLVM_Info.Table (Id)'Unrestricted_Access;
   end Get_LLVM_Info_For_Set;

   --  Define a generic package for those operations where we return Empty
   --  of the appropriate type if there's no entry in the environment.

   --------------
   -- Pkg_None --
   --------------

   generic
      type Obj is private;
      None : Obj;
      with function  Getter (LI : Access_LLVM_Data) return Obj;
      with procedure Setter (LI : Access_LLVM_Data; Val : Obj);
   package Pkg_None is
      function  Get (E : Entity_Id) return Obj with Inline;
      procedure Set (E : Entity_Id; Val : Obj) with Inline;
   end Pkg_None;

   package body Pkg_None is

      ---------
      -- Get --
      ---------

      function Get (E : Entity_Id) return Obj is
         Id : constant LLVM_Info_Id := LLVM_Info_Map (E);
      begin
         if Id = Empty_LLVM_Info_Id then
            return None;
         else
            return Getter (LLVM_Info.Table (Id)'Unrestricted_Access);
         end if;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (E : Entity_Id; Val : Obj) is
      begin
         Setter (Get_LLVM_Info_For_Set (E), Val);
      end Set;

   end Pkg_None;

   --  Likewise for when we're getting values for subexpressions

   --------------
   -- Pkg_Expr --
   --------------

   generic
      type Obj is private;
      None : Obj;
      with function  Getter (LI : Access_LLVM_Data) return Obj;
      with procedure Setter (LI : Access_LLVM_Data; Val : Obj);
   package Pkg_Expr is
      function  Get (N : N_Subexpr_Id) return Obj with Inline;
      procedure Set (N : N_Subexpr_Id; Val : Obj) with Inline;
   end Pkg_Expr;

   package body Pkg_Expr is

      ---------
      -- Get --
      ---------

      function Get (N : N_Subexpr_Id) return Obj is
         Id : constant LLVM_Info_Id := LLVM_Info_Map (N);
      begin
         if Id = Empty_LLVM_Info_Id then
            return None;
         else
            return Getter (LLVM_Info.Table (Id)'Unrestricted_Access);
         end if;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (N : N_Subexpr_Id; Val : Obj) is
      begin
         Setter (Get_LLVM_Info_For_Set (N), Val);
      end Set;

   end Pkg_Expr;

   --  Likewise when we always elaborate the type first

   --------------
   -- Pkg_Elab --
   --------------

   generic
      type Obj is private;
      with function  Getter (LI : Access_LLVM_Data) return Obj;
      with procedure Setter (LI : Access_LLVM_Data; Val : Obj);
   package Pkg_Elab is
      function  Get (E : Entity_Id) return Obj is (Getter (Get_LLVM_Info (E)));
      procedure Set (E : Entity_Id; Val : Obj) with Inline;
   end Pkg_Elab;

   package body Pkg_Elab is

      ---------
      -- Set --
      ---------

      procedure Set (E : Entity_Id; Val : Obj) is
      begin
         Setter (Get_LLVM_Info_For_Set (E), Val);
      end Set;

   end Pkg_Elab;

   --  Instantiate the above packages to make the rest of the
   --  subprograms in this package.

   package Env_GLT      is new Pkg_None (GL_Type, No_GL_Type,
                                         Raw_Get_GLT, Raw_Set_GLT);
   package Env_Value    is new Pkg_None (GL_Value, No_GL_Value,
                                         Raw_Get_Value, Raw_Set_Value);
   package Env_Elab     is new Pkg_None (Boolean, False,
                                         Raw_Get_Elab, Raw_Set_Elab);
   package Env_Field    is new Pkg_None (Field_Info_Id, Empty_Field_Info_Id,
                                         Raw_Get_Field, Raw_Set_Field);
   package Env_Label    is new Pkg_None (Label_Info_Id, Empty_Label_Info_Id,
                                         Raw_Get_Label, Raw_Set_Label);
   package Env_AGLT_N   is new Pkg_None (GL_Type, No_GL_Type,
                                         Raw_Get_AGLT, Raw_Set_AGLT);
   package Env_TBAA_N   is new Pkg_None (Metadata_T, No_Metadata_T,
                                         Raw_Get_TBAA, Raw_Set_TBAA);
   package Env_TBAA_N_I is new Pkg_None (TBAA_Info_Id, Empty_TBAA_Info_Id,
                                         Raw_Get_TBAA_I, Raw_Set_TBAA_I);
   package Env_O_A_N    is new Pkg_None (Array_Info_Id, Empty_Array_Info_Id,
                                         Raw_Get_O_A, Raw_Set_O_A);
   package Env_Record_N is new Pkg_None (Record_Info_Id, Empty_Record_Info_Id,
                                         Raw_Get_Record, Raw_Set_Record);
   package Env_Debug_N  is new Pkg_None (Metadata_T, No_Metadata_T,
                                         Raw_Get_Debug, Raw_Set_Debug);
   package Env_Array_N  is new Pkg_None (Array_Info_Id, Empty_Array_Info_Id,
                                         Raw_Get_Array, Raw_Set_Array);
   package Env_NN_N     is new Pkg_None (Boolean, False,
                                         Raw_Get_NN, Raw_Set_NN);
   package Env_S_T      is new Pkg_None (Type_T, No_Type_T,
                                         Raw_Get_S_T, Raw_Set_S_T);
   package Env_Flag1    is new Pkg_None (Boolean, False,
                                         Raw_Get_Flag1, Raw_Set_Flag1);

   package Env_SO       is new Pkg_Expr (Dynamic_SO_Ref, No_Uint,
                                         Raw_Get_SO, Raw_Set_SO);

   package Env_AGLT     is new Pkg_Elab (GL_Type, Raw_Get_AGLT, Raw_Set_AGLT);
   package Env_TBAA     is new Pkg_Elab (Metadata_T,
                                         Raw_Get_TBAA, Raw_Set_TBAA);
   package Env_TBAA_I   is new Pkg_Elab (TBAA_Info_Id, Raw_Get_TBAA_I,
                                         Raw_Set_TBAA_I);
   package Env_O_A      is new Pkg_Elab (Array_Info_Id,
                                         Raw_Get_O_A, Raw_Set_O_A);
   package Env_Record   is new Pkg_Elab (Record_Info_Id,
                                         Raw_Get_Record, Raw_Set_Record);
   package Env_Debug    is new Pkg_Elab (Metadata_T,
                                         Raw_Get_Debug, Raw_Set_Debug);
   package Env_Array    is new Pkg_Elab (Array_Info_Id,
                                         Raw_Get_Array, Raw_Set_Array);
   package Env_NN       is new Pkg_Elab (Boolean, Raw_Get_NN, Raw_Set_NN);

   --  Now complete our job by renaming the subprograms created above

   function  Get_GL_Type              (TE : Entity_Id) return GL_Type
     renames Env_GLT.Get;
   procedure Set_GL_Type              (TE : Entity_Id; GT : GL_Type)
     renames Env_GLT.Set;

   function  Get_Associated_GL_Type   (TE : Entity_Id) return GL_Type
     renames Env_AGLT.Get;
   function  Get_Associated_GL_Type_N (TE : Entity_Id) return GL_Type
     renames Env_AGLT_N.Get;
   procedure Set_Associated_GL_Type   (TE : Entity_Id; GT : GL_Type)
     renames Env_AGLT.Set;

   function  Get_Value                (VE : Entity_Id) return GL_Value
     renames Env_Value.Get;
   procedure Set_Value_R              (VE : Entity_Id; VL : GL_Value)
     renames Env_Value.Set;

   function  Get_SO_Ref               (N : N_Subexpr_Id) return Dynamic_SO_Ref
     renames Env_SO.Get;
   procedure Set_SO_Ref               (N : N_Subexpr_Id; U : Dynamic_SO_Ref)
     renames Env_SO.Set;

   function  Is_Being_Elaborated      (TE : Entity_Id) return Boolean
     renames Env_Elab.Get;
   procedure Set_Is_Being_Elaborated  (TE : Entity_Id; B : Boolean)
     renames Env_Elab.Set;

   function  Get_Field_Info           (VE : Entity_Id) return Field_Info_Id
     renames Env_Field.Get;
   procedure Set_Field_Info           (VE : Entity_Id; FI : Field_Info_Id)
     renames Env_Field.Set;

   function  Get_Label_Info           (VE : Entity_Id) return Label_Info_Id
     renames Env_Label.Get;
   procedure Set_Label_Info           (VE : Entity_Id; LI : Label_Info_Id)
     renames Env_Label.Set;

   function  Is_Nonnative_Type        (TE : Entity_Id) return Boolean
     renames Env_NN.Get;
   function  Is_Nonnative_Type_N      (TE : Entity_Id) return Boolean
     renames Env_NN_N.Get;
   procedure Set_Is_Nonnative_Type    (TE : Entity_Id; B : Boolean := True)
     renames Env_NN.Set;

   function  Get_TBAA                 (TE : Entity_Id) return Metadata_T
     renames Env_TBAA.Get;
   function  Get_TBAA_N               (TE : Entity_Id) return Metadata_T
     renames Env_TBAA_N.Get;
   procedure Set_TBAA                 (TE : Entity_Id; TBAA : Metadata_T)
     renames Env_TBAA.Set;

   function  Get_TBAA_Info            (TE : Entity_Id) return TBAA_Info_Id
     renames Env_TBAA_I.Get;
   function  Get_TBAA_Info_N          (TE : Entity_Id) return TBAA_Info_Id
     renames Env_TBAA_N_I.Get;
   procedure Set_TBAA_Info            (TE : Entity_Id; T : TBAA_Info_Id)
     renames Env_TBAA_I.Set;

   function  Get_Debug_Type           (TE : Entity_Id) return Metadata_T
     renames Env_Debug.Get;
   function  Get_Debug_Type_N         (TE : Entity_Id) return Metadata_T
     renames Env_Debug_N.Get;
   procedure Set_Debug_Type           (TE : Entity_Id; DT : Metadata_T)
     renames Env_Debug.Set;

   function  Get_Array_Info           (TE : Entity_Id) return Array_Info_Id
     renames Env_Array.Get;
   function  Get_Array_Info_N         (TE : Entity_Id) return Array_Info_Id
     renames Env_Array_N.Get;
   procedure Set_Array_Info           (TE : Entity_Id; AI : Array_Info_Id)
     renames Env_Array.Set;

   function  Get_Orig_Array_Info      (TE : Entity_Id) return Array_Info_Id
     renames Env_O_A.Get;
   function  Get_Orig_Array_Info_N    (TE : Entity_Id) return Array_Info_Id
     renames Env_O_A_N.Get;
   procedure Set_Orig_Array_Info      (TE : Entity_Id; AI : Array_Info_Id)
     renames Env_O_A.Set;

   function  Get_Record_Info          (TE : Entity_Id) return Record_Info_Id
     renames Env_Record.Get;
   function  Get_Record_Info_N        (TE : Entity_Id) return Record_Info_Id
     renames Env_Record_N.Get;
   procedure Set_Record_Info          (TE : Entity_Id; RI : Record_Info_Id)
     renames Env_Record.Set;

   function  Get_Subprogram_Type      (VE : Entity_Id) return Type_T
     renames Env_S_T.Get;
   procedure Set_Subprogram_Type      (VE : Entity_Id; T : Type_T)
     renames Env_S_T.Set;

   function  Get_Flag1                (VE : Entity_Id) return Boolean
     renames Env_Flag1.Get;
   procedure Set_Flag1                (VE : Entity_Id; F : Boolean)
     renames Env_Flag1.Set;

begin

   LLVM_Info.Increment_Last;
   --  Ensure the first LLVM_Info entry isn't Empty_LLVM_Info_Id

end GNATLLVM.Environment;
