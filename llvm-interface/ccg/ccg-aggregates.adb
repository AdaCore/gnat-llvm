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

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with CCG.Instructions; use CCG.Instructions;
with CCG.Output;       use CCG.Output;
with CCG.Utils;        use CCG.Utils;

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

   function Value_Piece (V : Value_T; T : in out Type_T; Idx : Nat) return Str
     with Pre  => Get_Opcode (V) in Op_Extract_Value | Op_Insert_Value
                  and then Get_Type_Kind (T)
                    in Struct_Type_Kind | Array_Type_Kind,
          Post => Present (Value_Piece'Result) and then T /= T'Old;
   --  T is the type of a component of the aggregate in an extractvalue or
   --  insertvalue instruction V. Return an Str saying how to access that
   --  component and update T to be the type of that component.

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
      --  Start by adding an entry to our table. Then either update the
      --  head of the chain or set a new head.

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
        (T, Idx, No_Name, Types.Empty, No_Field_Name_Idx, False, False);

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
         return Get_Name_String (FNI.Name) + Name;
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
      Types : constant Nat := Count_Struct_Element_Types (T);
      S     : Str;
   begin
      --  Because this struct may contain a pointer to itself, we always have
      --  to write an incomplete struct. So we write, e.g.,
      --
      --       typedef struct foo foo;
      --       struct foo { ... full definition ..}
      --
      --  Write the typedef first, then build an Str corresponding to the
      --  full definition and output it. Doing it that way ensure that any
      --  inner typedefs get written first.

      Write_Str ("typedef struct " & T & " " & T & ";" & Eol_Str);
      S := "struct " & T & " {" & Eol_Str;

      for J in 0 .. Types - 1 loop
         S := (S & "    " & Struct_Get_Type_At_Index (T, J) & " " &
                 Get_Field_Name (T, J) & ";" & Eol_Str);
      end loop;

      --  If this is an empty struct, we need to add a dummy field since
      --  ISO C89 doesn't allow an empty struct.

      if Types = 0 then
         S := S & "    char dummy_for_null_recordC; " & Eol_Str;
      end if;

      --  ??? We have many ways of handling packed, but don't worry about that
      --  in the initial support.

      Write_Str (S & "} __attribute__ ((packed));", Eol => True);
   end Write_Struct_Typedef;

   -------------------------
   -- Write_Array_Typedef --
   -------------------------

   procedure Write_Array_Typedef (T : Type_T) is
   begin
      Write_Str ("typedef " & Get_Element_Type (T) & " " & T & "[" &
                   Effective_Array_Length (T) & "];", Eol => True);
   end Write_Array_Typedef;

   -----------------
   -- Value_Piece --
   -----------------

   function Value_Piece
     (V : Value_T; T : in out Type_T; Idx : Nat) return Str is
   begin
      return Result : Str do
         declare
            Ins_Idx : constant Nat := Get_Index (V, Idx);
         begin
            --  We know this is either a struct or an array

            if Get_Type_Kind (T) = Struct_Type_Kind then
               Result := "." + Component & Get_Field_Name (T, Ins_Idx);
               T      := Struct_Get_Type_At_Index (T, Ins_Idx);
            else
               Result := " [" & Ins_Idx & "]" + Component;
               T      := Get_Element_Type (T);
            end if;
         end;
      end return;
   end Value_Piece;

   -------------------------------
   -- Extract_Value_Instruction --
   -------------------------------

   function Extract_Value_Instruction (V : Value_T; Op : Value_T) return Str is
      Idxs : constant Nat := Get_Num_Indices (V);
      T    : Type_T       := Type_Of (Op);
   begin
      return Result : Str := +Op do

         --  We process each index in turn, stripping off the reference.

         for J in 0 .. Idxs - 1 loop
            Result := Result & Value_Piece (V, T, J);
         end loop;
      end return;
   end Extract_Value_Instruction;

   ------------------------------
   -- Insert_Value_Instruction --
   ------------------------------

   procedure Insert_Value_Instruction (V, Aggr, Op : Value_T) is
      Idxs : constant Nat := Get_Num_Indices (V);
      T    : Type_T       := Type_Of (Aggr);
      Acc  : Str          := +V;

   begin
      --  If Aggr is undef, we don't need to do any copy. Otherwise, we
      --  first copy it to the result variable.

      Maybe_Decl (V);
      if Is_Undef (Aggr) then
         null;
      else
         Write_Copy (+V, +Aggr, T);
      end if;

      --  Next we generate the string that represents the access of this
      --  instruction.

      for J in 0 .. Idxs - 1 loop
         Acc := Acc & Value_Piece (V, T, J);
      end loop;

      --  The resulting type must be that of Op and we emit the assignment

      pragma Assert (T = Type_Of (Op));
      Write_Copy (Acc, Op + Assign, T);
   end Insert_Value_Instruction;

   ---------------------
   -- GEP_Instruction --
   ---------------------

   function GEP_Instruction (Ops : Value_Array) return Str is
      Aggr   : constant Value_T := Ops (Ops'First);
      --  The pointer to aggregate that we're dereferencing

      Aggr_T : Type_T           := Get_Element_Type (Type_Of (Aggr));
      --  The type that Aggr, which is always a pointer, points to

      Is_LHS : Boolean          := Get_Is_Variable (Aggr);
      --  Whether our result so far is an LHS as opposed to a pointer.
      --  If it is, then we can use normal derefrence operations and we must
      --  take the address at the end of the instruction processing.

      Result : Str;
      --  The resulting operation so far

   begin
      --  The first operand is special in that it represents a value to
      --  be multiplied by the size of the type pointed to and added to
      --  the value of the pointer input. Normally, we have a GEP that either
      --  has a nonzero value for this operand and no others or that has a
      --  zero for this value, those aren't requirements. However, it's
      --  very worth special-casing the zero case here because we have
      --  nothing to do in that case.

      if Is_A_Constant_Int (Ops (Ops'First + 1))
        and then Equals_Int (Ops (Ops'First + 1), 0)
      then
         Result := Aggr + LHS + Component;
      else
         Result := TP ("#1[#2]", Aggr, Ops (Ops'First + 1)) + Component;
         Is_LHS := True;
      end if;

      --  Now process any other operands, which must always dereference into
      --  an array or struct.

      for Op of Ops (Ops'First + 2 .. Ops'Last) loop
         if Get_Type_Kind (Aggr_T) = Array_Type_Kind then

            --  If this isn't an LHS, we have to make it one

            if not Is_LHS then
               Result := Deref (Result);
            end if;

            Result := Result & TP ("[#1]", Op) + Component;
            Aggr_T := Get_Element_Type (Aggr_T);
            Is_LHS := True;

         else
            pragma Assert (Get_Type_Kind (Aggr_T) = Struct_Type_Kind);

            declare
               Idx : constant Nat := Nat (Const_Int_Get_S_Ext_Value (Op));
            begin
               Result := Result & (if Is_LHS then "." else "->") + Component &
                 Get_Field_Name (Aggr_T, Idx);
               Aggr_T := Struct_Get_Type_At_Index (Aggr_T, Idx);
               Is_LHS := True;
            end;
         end if;
      end loop;

      --  If we ended up with a LHS, we have to take the address

      if Is_LHS then
         Result := Addr_Of (Result, T => Aggr_T);
      end if;

      return Result;
   end GEP_Instruction;

end CCG.Aggregates;
