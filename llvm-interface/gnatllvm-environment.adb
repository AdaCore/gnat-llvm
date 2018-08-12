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

with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package body GNATLLVM.Environment is

   function Get_LLVM_Info_Id         (TE : Entity_Id) return LLVM_Info_Id
     with Pre => Is_Type (TE);
   --  Helper for below to get LLVM_Info_Table entry, forcing type
   --  creation if not done

   function Get_LLVM_Info_Id_For_Set (N : Node_Id) return LLVM_Info_Id;
   --  Helper for below to allocate LLVM_Info_Table entry if needed.
   pragma Inline (Get_LLVM_Info_Id);
   pragma Inline (Get_LLVM_Info_Id_For_Set);

   ----------------------
   -- Get_LLVM_Info_Id --
   ----------------------

   function Get_LLVM_Info_Id (TE : Entity_Id) return LLVM_Info_Id is
   begin
      if not Has_Type (TE) then
         Discard (Create_Type (TE));
      end if;

      return LLVM_Info_Map (TE);
   end Get_LLVM_Info_Id;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (TE : Entity_Id) return Type_T is
   begin
      if LLVM_Info_Map (TE) = Empty_LLVM_Info_Id then
         return No_Type_T;
      else
         return LLVM_Info_Table.Table (LLVM_Info_Map (TE)).Typ;
      end if;
   end Get_Type;

   -------------------------
   -- Is_Being_Elaborated --
   -------------------------

   function Is_Being_Elaborated (TE : Entity_Id) return Boolean is
   begin
      return LLVM_Info_Map (TE) /= Empty_LLVM_Info_Id
        and then (LLVM_Info_Table.Table
                    (LLVM_Info_Map (TE)).Is_Being_Elaborated);
   end Is_Being_Elaborated;

   -------------------
   -- Is_Dummy_Type --
   -------------------

   function Is_Dummy_Type (TE : Entity_Id) return Boolean is
   begin
      return LLVM_Info_Map (TE) /= Empty_LLVM_Info_Id
        and then LLVM_Info_Table.Table (LLVM_Info_Map (TE)).Is_Dummy_Type;
   end Is_Dummy_Type;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (VE : Entity_Id) return GL_Value is
   begin
      if LLVM_Info_Map (VE) = Empty_LLVM_Info_Id then
         return No_GL_Value;
      else
         return LLVM_Info_Table.Table (LLVM_Info_Map (VE)).Value;
      end if;
   end Get_Value;

   --------------------
   -- Get_Field_Info --
   --------------------

   function Get_Field_Info (VE : Entity_Id) return Field_Info_Id is
   begin
      if LLVM_Info_Map (VE) = Empty_LLVM_Info_Id then
         return Empty_Field_Info_Id;
      else
         return LLVM_Info_Table.Table (LLVM_Info_Map (VE)).Field_Info;
      end if;

   end Get_Field_Info;

   --------------------
   -- Get_Label_Info --
   --------------------

   function Get_Label_Info (VE : Entity_Id) return Label_Info_Id is
   begin
      if LLVM_Info_Map (VE) = Empty_LLVM_Info_Id then
         return Empty_Label_Info_Id;
      else
         return LLVM_Info_Table.Table (LLVM_Info_Map (VE)).Label_Info;
      end if;

   end Get_Label_Info;

   ---------------------
   -- Is_Dynamic_Size --
   ---------------------

   function Is_Dynamic_Size (TE : Entity_Id) return Boolean is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id (TE);

   begin
      return LLVM_Info_Table.Table (Id).Is_Dynamic_Size;
   end Is_Dynamic_Size;

   --------------
   -- Get_TBAA --
   --------------

   function Get_TBAA (TE : Entity_Id) return Metadata_T is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id (TE);

   begin
      return LLVM_Info_Table.Table (Id).TBAA;
   end Get_TBAA;

   ---------------------
   -- Get_Array_Info --
   ---------------------

   function Get_Array_Info (TE : Entity_Id) return Array_Info_Id is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id (TE);

   begin
      return LLVM_Info_Table.Table (Id).Array_Info;
   end Get_Array_Info;

   ---------------------
   -- Get_Orig_Array_Info --
   ---------------------

   function Get_Orig_Array_Info (TE : Entity_Id) return Array_Info_Id is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id (TE);

   begin
      return LLVM_Info_Table.Table (Id).Orig_Array_Info;
   end Get_Orig_Array_Info;

   ---------------------
   -- Get_Record_Info --
   ---------------------

   function Get_Record_Info (TE : Entity_Id) return Record_Info_Id is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id (TE);

   begin
      return LLVM_Info_Table.Table (Id).Record_Info;
   end Get_Record_Info;

   ------------------------------
   -- Get_LLVM_Info_Id_For_Set --
   ------------------------------

   function Get_LLVM_Info_Id_For_Set (N : Node_Id) return LLVM_Info_Id
   is
      Id : LLVM_Info_Id := LLVM_Info_Map (N);

   begin
      if Id /= Empty_LLVM_Info_Id then
         return Id;
      else
         LLVM_Info_Table.Append ((Value               => No_GL_Value,
                                  Typ                 => No_Type_T,
                                  TBAA                => No_Metadata_T,
                                  Is_Dynamic_Size     => False,
                                  Is_Being_Elaborated => False,
                                  Is_Dummy_Type       => False,
                                  Record_Info         => Empty_Record_Info_Id,
                                  Field_Info          => Empty_Field_Info_Id,
                                  Array_Info          => Empty_Array_Info_Id,
                                  Label_Info          => Empty_Label_Info_Id,
                                  Orig_Array_Info     => Empty_Array_Info_Id));
         Id := LLVM_Info_Table.Last;
         LLVM_Info_Map (N) := Id;
         return Id;
      end if;
   end Get_LLVM_Info_Id_For_Set;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type (TE : Entity_Id; TL : Type_T) is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id_For_Set (TE);

   begin
      LLVM_Info_Table.Table (Id).Typ := TL;
   end Set_Type;

   -------------------------
   -- Set_Is_Dynamic_Size --
   -------------------------

   procedure Set_Is_Dynamic_Size (TE : Entity_Id; B : Boolean := True) is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id_For_Set (TE);

   begin
      LLVM_Info_Table.Table (Id).Is_Dynamic_Size := B;
   end Set_Is_Dynamic_Size;

   -----------------------------
   -- Set_Is_Being_Elaborated --
   -----------------------------

   procedure Set_Is_Being_Elaborated (TE : Entity_Id; B : Boolean) is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id_For_Set (TE);

   begin
      LLVM_Info_Table.Table (Id).Is_Being_Elaborated := B;
   end Set_Is_Being_Elaborated;

   -----------------------
   -- Set_Is_Dummy_Type --
   -----------------------

   procedure Set_Is_Dummy_Type (TE : Entity_Id; B : Boolean) is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id_For_Set (TE);

   begin
      LLVM_Info_Table.Table (Id).Is_Dummy_Type := B;
   end Set_Is_Dummy_Type;

   --------------
   -- Set_TBAA --
   --------------

   procedure Set_TBAA (TE : Entity_Id; TBAA : Metadata_T) is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id_For_Set (TE);

   begin
      LLVM_Info_Table.Table (Id).TBAA := TBAA;
   end Set_TBAA;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (VE : Entity_Id; VL : GL_Value) is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id_For_Set (VE);

   begin
      LLVM_Info_Table.Table (Id).Value :=  VL;
   end Set_Value;

   ---------------------
   -- Set_Array_Info --
   ---------------------

   procedure Set_Array_Info (TE : Entity_Id; AI : Array_Info_Id)
   is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id_For_Set (TE);

   begin
      LLVM_Info_Table.Table (Id).Array_Info := AI;
   end Set_Array_Info;

   ---------------------
   -- Set_Orig_Array_Info --
   ---------------------

   procedure Set_Orig_Array_Info (TE : Entity_Id; AI : Array_Info_Id)
   is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id_For_Set (TE);

   begin
      LLVM_Info_Table.Table (Id).Orig_Array_Info := AI;
   end Set_Orig_Array_Info;

   ---------------------
   -- Set_Record_Info --
   ---------------------

   procedure Set_Record_Info (TE : Entity_Id; RI : Record_Info_Id)
   is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id_For_Set (TE);

   begin
      LLVM_Info_Table.Table (Id).Record_Info := RI;
   end Set_Record_Info;

   --------------------
   -- Set_Field_Info --
   --------------------

   procedure Set_Field_Info (VE : Entity_Id; FI : Field_Info_Id)
   is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id_For_Set (VE);

   begin
      LLVM_Info_Table.Table (Id).Field_Info := FI;
   end Set_Field_Info;

   --------------------
   -- Set_Label_Info --
   --------------------

   procedure Set_Label_Info (VE : Entity_Id; LI : Label_Info_Id)
   is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id_For_Set (VE);

   begin
      LLVM_Info_Table.Table (Id).Label_Info := LI;
   end Set_Label_Info;

end GNATLLVM.Environment;
