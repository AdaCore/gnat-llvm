------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package body GNATLLVM.Environment is

   type Access_LLVM_Info is access all LLVM_Info;

   function Get_LLVM_Info         (TE : Entity_Id) return Access_LLVM_Info
     with Pre => Is_Type_Or_Void (TE);
   function Get_LLVM_Info_For_Set (E : Entity_Id)  return Access_LLVM_Info;
   --  Helpers for below to either create type and then return entry or
   --  or to allocate LLVM_Info_Table entry if needed (for set).  In either
   --  case, the returned access type must only be used immediately since
   --  it will be invalid if the table is reallocated.

   pragma Inline (Get_LLVM_Info);
   pragma Inline (Get_LLVM_Info_For_Set);

   --  Define functions to get values from LLVM_Info

   function Raw_Get_Type   (LI : Access_LLVM_Info) return Type_T is
     (LI.Typ);
   function Raw_Get_GLT    (LI : Access_LLVM_Info) return GL_Type is
     (LI.GLType);
   function Raw_Get_CGLT   (LI : Access_LLVM_Info) return GL_Type is
     (LI.Component_GL_Type);
   function Raw_Get_Value  (LI : Access_LLVM_Info) return GL_Value is
     (LI.Value);
   function Raw_Get_SO     (LI : Access_LLVM_Info) return Dynamic_SO_Ref is
     (LI.SO_Info);
   function Raw_Get_Elab   (LI : Access_LLVM_Info) return Boolean is
     (LI.Is_Being_Elaborated);
   function Raw_Get_Dummy  (LI : Access_LLVM_Info) return Boolean is
     (LI.Is_Dummy_Type);
   function Raw_Get_Field  (LI : Access_LLVM_Info) return Field_Info_Id is
     (LI.Field_Info);
   function Raw_Get_Label  (LI : Access_LLVM_Info) return Label_Info_Id is
     (LI.Label_Info);
   function Raw_Get_NN     (LI : Access_LLVM_Info) return Boolean is
     (LI.Is_Nonnative_Type);
   function Raw_Get_TBAA   (LI : Access_LLVM_Info) return Metadata_T is
     (LI.TBAA);
   function Raw_Get_Array  (LI : Access_LLVM_Info) return Array_Info_Id is
     (LI.Array_Info);
   function Raw_Get_O_A    (LI : Access_LLVM_Info) return Array_Info_Id is
     (LI.Orig_Array_Info);
   function Raw_Get_Record (LI : Access_LLVM_Info) return Record_Info_Id is
     (LI.Record_Info);

   --  Define procedures to set values into LLVM_Info

   procedure Raw_Set_Type   (LI : Access_LLVM_Info; Val : Type_T);
   procedure Raw_Set_GLT    (LI : Access_LLVM_Info; Val : GL_Type);
   procedure Raw_Set_CGLT   (LI : Access_LLVM_Info; Val : GL_Type);
   procedure Raw_Set_Value  (LI : Access_LLVM_Info; Val : GL_Value);
   procedure Raw_Set_SO     (LI : Access_LLVM_Info; Val : Dynamic_SO_Ref);
   procedure Raw_Set_Elab   (LI : Access_LLVM_Info; Val : Boolean);
   procedure Raw_Set_Dummy  (LI : Access_LLVM_Info; Val : Boolean);
   procedure Raw_Set_Field  (LI : Access_LLVM_Info; Val : Field_Info_Id);
   procedure Raw_Set_Label  (LI : Access_LLVM_Info; Val : Label_Info_Id);
   procedure Raw_Set_NN     (LI : Access_LLVM_Info; Val : Boolean);
   procedure Raw_Set_TBAA   (LI : Access_LLVM_Info; Val : Metadata_T);
   procedure Raw_Set_Array  (LI : Access_LLVM_Info; Val : Array_Info_Id);
   procedure Raw_Set_O_A    (LI : Access_LLVM_Info; Val : Array_Info_Id);
   procedure Raw_Set_Record (LI : Access_LLVM_Info; Val : Record_Info_Id);

   pragma Inline (Raw_Set_Type);
   pragma Inline (Raw_Set_GLT);
   pragma Inline (Raw_Set_CGLT);
   pragma Inline (Raw_Set_Value);
   pragma Inline (Raw_Set_SO);
   pragma Inline (Raw_Set_Elab);
   pragma Inline (Raw_Set_Dummy);
   pragma Inline (Raw_Set_Field);
   pragma Inline (Raw_Set_Label);
   pragma Inline (Raw_Set_NN);
   pragma Inline (Raw_Set_TBAA);
   pragma Inline (Raw_Set_Array);
   pragma Inline (Raw_Set_O_A);
   pragma Inline (Raw_Set_Record);

   procedure Raw_Set_Type   (LI : Access_LLVM_Info; Val : Type_T) is
   begin LI.Typ := Val; end Raw_Set_Type;

   procedure Raw_Set_GLT    (LI : Access_LLVM_Info; Val : GL_Type) is
   begin LI.GLType := Val; end Raw_Set_GLT;

   procedure Raw_Set_CGLT   (LI : Access_LLVM_Info; Val : GL_Type) is
   begin LI.Component_GL_Type := Val; end Raw_Set_CGLT;

   procedure Raw_Set_Value  (LI : Access_LLVM_Info; Val : GL_Value) is
   begin LI.Value := Val; end Raw_Set_Value;

   procedure Raw_Set_SO     (LI : Access_LLVM_Info; Val : Dynamic_SO_Ref) is
   begin LI.SO_Info := Val; end Raw_Set_SO;

   procedure Raw_Set_Elab   (LI : Access_LLVM_Info; Val : Boolean) is
   begin LI.Is_Being_Elaborated := Val; end Raw_Set_Elab;

   procedure Raw_Set_Dummy  (LI : Access_LLVM_Info; Val : Boolean) is
   begin LI.Is_Dummy_Type := Val; end Raw_Set_Dummy;

   procedure Raw_Set_Field  (LI : Access_LLVM_Info; Val : Field_Info_Id) is
   begin LI.Field_Info := Val; end Raw_Set_Field;

   procedure Raw_Set_Label  (LI : Access_LLVM_Info; Val : Label_Info_Id) is
   begin LI.Label_Info := Val; end Raw_Set_Label;

   procedure Raw_Set_NN     (LI : Access_LLVM_Info; Val : Boolean) is
   begin LI.Is_Nonnative_Type := Val; end Raw_Set_NN;

   procedure Raw_Set_TBAA   (LI : Access_LLVM_Info; Val : Metadata_T) is
   begin LI.TBAA := Val; end Raw_Set_TBAA;

   procedure Raw_Set_Array  (LI : Access_LLVM_Info; Val : Array_Info_Id) is
   begin LI.Array_Info := Val; end Raw_Set_Array;

   procedure Raw_Set_O_A    (LI : Access_LLVM_Info; Val : Array_Info_Id) is
   begin LI.Orig_Array_Info := Val; end Raw_Set_O_A;

   procedure Raw_Set_Record (LI : Access_LLVM_Info; Val : Record_Info_Id) is
   begin LI.Record_Info := Val; end Raw_Set_Record;

   -------------------
   -- Get_LLVM_Info --
   -------------------

   function Get_LLVM_Info (TE : Entity_Id) return Access_LLVM_Info is
   begin
      if No (Get_Type (TE)) then
         Discard (Create_Type (TE));
      end if;

      return LLVM_Info_Table.Table (LLVM_Info_Map (TE))'Unrestricted_Access;
   end Get_LLVM_Info;

   ---------------------------
   -- Get_LLVM_Info_For_Set --
   ---------------------------

   function Get_LLVM_Info_For_Set (E : Entity_Id) return Access_LLVM_Info is
      Id : LLVM_Info_Id := LLVM_Info_Map (E);

   begin
      if Id = Empty_LLVM_Info_Id then
         LLVM_Info_Table.Append ((Value               => No_GL_Value,
                                  Typ                 => No_Type_T,
                                  GLType              => No_GL_Type,
                                  Component_GL_Type   => No_GL_Type,
                                  TBAA                => No_Metadata_T,
                                  Is_Nonnative_Type   => False,
                                  Is_Being_Elaborated => False,
                                  Is_Dummy_Type       => False,
                                  Record_Info         => Empty_Record_Info_Id,
                                  Field_Info          => Empty_Field_Info_Id,
                                  Array_Info          => Empty_Array_Info_Id,
                                  Label_Info          => Empty_Label_Info_Id,
                                  Orig_Array_Info     => Empty_Array_Info_Id,
                                  SO_Info             => No_Uint));
         Id := LLVM_Info_Table.Last;
         LLVM_Info_Map (E) := Id;
      end if;

      return LLVM_Info_Table.Table (Id)'Unrestricted_Access;
   end Get_LLVM_Info_For_Set;

   --  Define a generic package for those operations where we return Empty
   --  of the appropriate type if there's no entry in the environment.

   --------------
   -- Pkg_None --
   --------------

   generic
      type Obj is private;
      None : Obj;
      with function  Getter (LI : Access_LLVM_Info) return Obj;
      with procedure Setter (LI : Access_LLVM_Info; Val : Obj);
   package Pkg_None is
      function  Get (E : Entity_Id) return Obj;
      procedure Set (E : Entity_Id; Val : Obj);

      pragma Inline (Get);
      pragma Inline (Set);

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
            return Getter (LLVM_Info_Table.Table (Id)'Unrestricted_Access);
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

   --  Likewise when we always elaborate the type first

   --------------
   -- Pkg_Elab --
   --------------

   generic
      type Obj is private;
      with function  Getter (LI : Access_LLVM_Info) return Obj;
      with procedure Setter (LI : Access_LLVM_Info; Val : Obj);
   package Pkg_Elab is
      function  Get (E : Entity_Id) return Obj is (Getter (Get_LLVM_Info (E)));
      procedure Set (E : Entity_Id; Val : Obj);

      pragma Inline (Set);

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

   package Env_Type   is new Pkg_None (Type_T, No_Type_T,
                                       Raw_Get_Type, Raw_Set_Type);
   package Env_GLT    is new Pkg_None (GL_Type, No_GL_Type,
                                       Raw_Get_GLT, Raw_Set_GLT);
   package Env_Value  is new Pkg_None (GL_Value, No_GL_Value,
                                       Raw_Get_Value, Raw_Set_Value);
   package Env_SO     is new Pkg_None (Dynamic_SO_Ref, No_Uint,
                                       Raw_Get_SO, Raw_Set_SO);
   package Env_Elab   is new Pkg_None (Boolean, False,
                                       Raw_Get_Elab, Raw_Set_Elab);
   package Env_Dummy  is new Pkg_None (Boolean, False,
                                       Raw_Get_Dummy, Raw_Set_Dummy);
   package Env_Field  is new Pkg_None (Field_Info_Id, Empty_Field_Info_Id,
                                       Raw_Get_Field, Raw_Set_Field);
   package Env_Label  is new Pkg_None (Label_Info_Id, Empty_Label_Info_Id,
                                       Raw_Get_Label, Raw_Set_Label);

   package Env_NN     is new Pkg_Elab (Boolean, Raw_Get_NN, Raw_Set_NN);
   package Env_CGLT   is new Pkg_Elab (GL_Type, Raw_Get_CGLT, Raw_Set_CGLT);
   package Env_TBAA   is new Pkg_Elab (Metadata_T, Raw_Get_TBAA, Raw_Set_TBAA);
   package Env_Array  is new Pkg_Elab (Array_Info_Id,
                                       Raw_Get_Array, Raw_Set_Array);
   package Env_O_A    is new Pkg_Elab (Array_Info_Id,
                                       Raw_Get_O_A, Raw_Set_O_A);
   package Env_Record is new Pkg_Elab (Record_Info_Id,
                                       Raw_Get_Record, Raw_Set_Record);

   --  Now complete our job by renaming the subprograms created above

   function  Get_Type                (TE : Entity_Id) return Type_T
     renames Env_Type.Get;
   procedure Set_Type                (TE : Entity_Id; TL : Type_T)
     renames Env_Type.Set;

   function  Get_GL_Type             (TE : Entity_Id) return GL_Type
     renames Env_GLT.Get;
   procedure Set_GL_Type             (TE : Entity_Id; GT : GL_Type)
     renames Env_GLT.Set;

   function  Get_Component_GL_Type   (TE : Entity_Id) return GL_Type
     renames Env_CGLT.Get;
   procedure Set_Component_GL_Type   (TE : Entity_Id; GT : GL_Type)
     renames Env_CGLT.Set;

   function  Get_Value               (VE : Entity_Id) return GL_Value
     renames Env_Value.Get;
   procedure Set_Value_R             (VE : Entity_Id; VL : GL_Value)
     renames Env_Value.Set;

   function  Get_SO_Ref              (N : Node_Id) return Dynamic_SO_Ref
     renames Env_SO.Get;
   procedure Set_SO_Ref              (N : Node_Id; U : Dynamic_SO_Ref)
     renames Env_SO.Set;

   function  Is_Being_Elaborated     (TE : Entity_Id) return Boolean
     renames Env_Elab.Get;
   procedure Set_Is_Being_Elaborated (TE : Entity_Id; B : Boolean)
     renames Env_Elab.Set;

   function  Is_Dummy_Type           (TE : Entity_Id) return Boolean
     renames Env_Dummy.Get;
   procedure Set_Is_Dummy_Type       (TE : Entity_Id; B : Boolean)
     renames Env_Dummy.Set;

   function  Get_Field_Info          (VE : Entity_Id) return Field_Info_Id
     renames Env_Field.Get;
   procedure Set_Field_Info          (VE : Entity_Id; FI : Field_Info_Id)
     renames Env_Field.Set;

   function  Get_Label_Info          (VE : Entity_Id) return Label_Info_Id
     renames Env_Label.Get;
   procedure Set_Label_Info          (VE : Entity_Id; LI : Label_Info_Id)
     renames Env_Label.Set;

   function  Is_Nonnative_Type       (TE : Entity_Id) return Boolean
     renames Env_NN.Get;
   procedure Set_Is_Nonnative_Type   (TE : Entity_Id; B : Boolean := True)
     renames Env_NN.Set;

   function  Get_TBAA                (TE : Entity_Id) return Metadata_T
     renames Env_TBAA.Get;
   procedure Set_TBAA                (TE : Entity_Id; TBAA : Metadata_T)
     renames Env_TBAA.Set;

   function  Get_Array_Info          (TE : Entity_Id) return Array_Info_Id
     renames Env_Array.Get;
   procedure Set_Array_Info          (TE : Entity_Id; AI : Array_Info_Id)
     renames Env_Array.Set;

   function  Get_Orig_Array_Info     (TE : Entity_Id) return Array_Info_Id
     renames Env_O_A.Get;
   procedure Set_Orig_Array_Info     (TE : Entity_Id; AI : Array_Info_Id)
     renames Env_O_A.Set;

   function  Get_Record_Info         (TE : Entity_Id) return Record_Info_Id
     renames Env_Record.Get;
   procedure Set_Record_Info         (TE : Entity_Id; RI : Record_Info_Id)
     renames Env_Record.Set;

begin

   LLVM_Info_Table.Increment_Last;
   --  Ensure the first LLVM_Info entry isn't Empty_LLVM_Info_Id

end GNATLLVM.Environment;
