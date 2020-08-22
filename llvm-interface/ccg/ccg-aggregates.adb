------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020, AdaCore                          --
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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;

with Table;

with GNATLLVM.Codegen; use GNATLLVM.Codegen;

with CCG.Helper; use CCG.Helper;
with CCG.Utils;  use CCG.Utils;

package body CCG.Aggregates is

   --  This package contains routines used to process aggregate data,
   --  which are arrays and structs.
   --  We want to record information about each field in an LLVM struct
   --  type corresponding to an Ada record or part thereof so we can use
   --  those names in the generated code. The following record is used
   --  to store information about fields.

   type Field_Name_Idx is new Nat;
   No_Field_Name_Idx : constant Field_Name_Idx := 0;

   function Present (F : Field_Name_Idx) return Boolean is
     (F /= No_Field_Name_Idx);

   type Field_Name_Info is record
      T           : Type_T;
      --  LLVM "struct" type containing this field

      F_Number    : Nat;
      --  0-origin count of field in type

      Name        : Name_Id;
      --  If Present, the name of the field

      TE          : Entity_Id;
      --  The GNAT type for the field; used only when initializing field info

      Next        : Field_Name_Idx;
      --  Index of next field entry for this type

      Is_Padding  : Boolean;
      --  True if this field is padding and doesn't correspond to any
      --  source-level field.

      Is_Bitfield : Boolean;
      --  True if this is a field that's used to store one or more bitfields

   end record;

   --  Define the table that records all of the field name info

   package Field_Name_Info_Table is new Table.Table
     (Table_Component_Type => Field_Name_Info,
      Table_Index_Type     => Field_Name_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 500,
      Table_Increment      => 100,
      Table_Name           => "Field_Name_Info_Table");

   --  We need two maps into the above table. One maps a GNAT type into
   --  a table entry. This is used to track the initial setting of field info
   --  and is used when we set the struct type.  The second maps a
   --  (struct type, field index) pair into the name info for that field.

   function Hash (TE : Entity_Id) return Hash_Type is (Hash_Type (TE));

   package Entity_To_FNI_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Entity_Id,
      Element_Type    => Field_Name_Idx,
      Hash            => Hash,
      Equivalent_Keys => "=");
   Entity_To_FNI_Map : Entity_To_FNI_Maps.Map;

   type FN_Key is record
      T   : Type_T;
      Idx : Nat;
   end record;

   function Hash (K : FN_Key) return Hash_Type is
     (Hash (K.T) + Hash_Type (K.Idx));

   package FNI_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => FN_Key,
      Element_Type    => Field_Name_Idx,
      Hash            => Hash,
      Equivalent_Keys => "=");
   FNI_Map : FNI_Maps.Map;

   -------------------------
   -- Set_Field_Name_Info --
   -------------------------

   procedure Set_Field_Name_Info
     (TE          : Entity_Id;
      Idx         : Nat;
      Name        : Name_Id := No_Name;
      Is_Padding  : Boolean := False;
      Is_Bitfield : Boolean := False)
   is
      use Entity_To_FNI_Maps;
      Position : constant Cursor         := Find (Entity_To_FNI_Map, TE);
      F_Idx    : constant Field_Name_Idx :=
        (if   Has_Element (Position) then Element (Position)
         else No_Field_Name_Idx);

   begin
      --  If we're not generating C code, don't do anything

      if Code_Generation /= Write_C then
         return;
      end if;

      --  Otherwise, start by adding an entry to our table. Then either
      --  update the head of the chain or set a new head.

      Field_Name_Info_Table.Append ((T           => No_Type_T,
                                     F_Number    => Idx,
                                     Name        => Name,
                                     TE          => TE,
                                     Next        => F_Idx,
                                     Is_Padding  => Is_Padding,
                                     Is_Bitfield => Is_Bitfield));
      if Has_Element (Position) then
         Replace_Element (Entity_To_FNI_Map, Position,
                          Field_Name_Info_Table.Last);
      else
         Insert (Entity_To_FNI_Map, TE, Field_Name_Info_Table.Last);
      end if;

   end Set_Field_Name_Info;

   ----------------
   -- Set_Struct --
   ----------------

   procedure Set_Struct (TE : Entity_Id; T : Type_T) is
      package EFM renames Entity_To_FNI_Maps;
      package TFM renames FNI_Maps;
      Position : constant EFM.Cursor := EFM.Find (Entity_To_FNI_Map, TE);
      F_Idx    : Field_Name_Idx;

   begin
      --  If we didn't make any entry in the Field Name Info table for
      --  this type, we don't have anything to do. This could have happened
      --  either if we weren't generating C or if TE is a null record.

      if not EFM.Has_Element (Position) then
         return;
      end if;

      --  Otherwise get the first entry we made and loop over all
      --  Field_Name_Info entries for TE, looking for entries where the
      --  LLVM type hasn't yet been set. For each, set the type and add the
      --  (type, field index) pair to the hash table, but if the type has
      --  no name, don't insert it into the table since it'll be a shared
      --  struct.

      F_Idx := EFM.Element (Position);
      while Present (F_Idx) loop
         declare
            FNI : Field_Name_Info renames Field_Name_Info_Table.Table (F_Idx);

         begin
            if No (FNI.T) then
               FNI.T := T;
               if Has_Name (T) then
                  TFM.Insert (FNI_Map, (T, FNI.F_Number), F_Idx);
               end if;
            end if;

            F_Idx := FNI.Next;
         end;
      end loop;
   end Set_Struct;

   --------------------
   -- Get_Field_Name --
   --------------------

   function Get_Field_Name (T : Type_T; Idx : Nat) return Str is
      use FNI_Maps;
      Position : constant Cursor := Find (FNI_Map, (T, Idx));
      FNI      : Field_Name_Info :=
        (T, Idx, No_Name, Empty, No_Field_Name_Idx, False, False);

   begin
      --  If we have information for this field in our table (we should),
      --  replace the default above with that information.

      if Has_Element (Position) then
         FNI := Field_Name_Info_Table.Table (Element (Position));
      end if;

      --  Now create a name for the field, based on the saved information.
      --  We really shouldn't be requesting a padding field, but handle it
      --  anyway.

      if Present (FNI.Name) then
         return +Get_Name_String (FNI.Name);
      elsif FNI.Is_Padding then
         return "ccg_pad_" & Idx;
      elsif FNI.Is_Bitfield then
         return "ccg_bits_" & Idx;
      else
         return "ccg_field_" & Idx;
      end if;

   end Get_Field_Name;

   --------------------------
   -- Write_Struct_Typedef --
   --------------------------

   procedure Write_Struct_Typedef (T : Type_T) is
   begin
      Write_Str ("typedef struct " & T & " {", Eol => True);
      for J in Nat range 0 .. Count_Struct_Element_Types (T) - 1 loop
         Write_Str ("    " & Struct_Get_Type_At_Index (T, J) & " " &
                      Get_Field_Name (T, J) & ";", Eol => True);
      end loop;

      --  ??? We have many ways of handling packed, but don't worry about that
      --  in the initial support.

      Write_Str ("} __attribute__ ((packed)) " & T & ";", Eol => True);
   end Write_Struct_Typedef;

end CCG.Aggregates;
