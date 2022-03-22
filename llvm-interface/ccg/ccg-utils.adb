------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2022, AdaCore                     --
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

with Ada.Containers.Hashed_Maps;

with Output; use Output;
with Table;

with Sinfo.Nodes; use Sinfo.Nodes;
with Sinfo.Utils; use Sinfo.Utils;

with GNATLLVM.Types; use GNATLLVM.Types;
with GNATLLVM.Utils; use GNATLLVM.Utils;

package body CCG.Utils is

   --  We want to record information about each field in an LLVM struct
   --  type corresponding to an Ada record or part thereof so we can use
   --  those names in the generated code. The following record is used
   --  to store information about fields.

   type Field_C_Info_Idx is new Nat;
   No_Field_C_Info_Idx : constant Field_C_Info_Idx := 0;

   function Present (F : Field_C_Info_Idx) return Boolean is
     (F /= No_Field_C_Info_Idx);

   type Field_C_Data is record
      T           : Type_T;
      --  LLVM "struct" type containing this field

      F_Number    : Nat;
      --  0-origin count of field in type

      Name        : Name_Id;
      --  If Present, the name of the field

      Entity      : Entity_Id;
      --  If Present, the GNAT entity for the field

      SID         : Struct_Id;
      --  The Struct_Id for the field; used only when initializing field info

      Next        : Field_C_Info_Idx;
      --  Index of next field entry for this type

      Is_Padding  : Boolean;
      --  True if this field is padding and doesn't correspond to any
      --  source-level field.

      Is_Bitfield : Boolean;
      --  True if this is a field that's used to store one or more bitfields

   end record;

   --  Define the table that records all of the field name info

   package Field_C_Info is new Table.Table
     (Table_Component_Type => Field_C_Data,
      Table_Index_Type     => Field_C_Info_Idx,
      Table_Low_Bound      => 1,
      Table_Initial        => 500,
      Table_Increment      => 100,
      Table_Name           => "Field_C_Info");

   --  We need two maps into the above table. One maps a Struct_Id into
   --  a table entry. This is used to track the initial setting of field info
   --  and is used when we set the struct type.  The second maps a
   --  (struct type, field index) pair into the name info for that field.

   function Hash (SID : Struct_Id) return Hash_Type is (Hash_Type (SID));

   package Entity_To_FCI_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Struct_Id,
      Element_Type    => Field_C_Info_Idx,
      Hash            => Hash,
      Equivalent_Keys => "=");
   Entity_To_FCI_Map : Entity_To_FCI_Maps.Map;

   type FC_Key is record
      T   : Type_T;
      Idx : Nat;
   end record;

   function Hash (K : FC_Key) return Hash_Type is
     (Hash (K.T) + Hash_Type (K.Idx));

   package FCI_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => FC_Key,
      Element_Type    => Field_C_Info_Idx,
      Hash            => Hash,
      Equivalent_Keys => "=");
   FCI_Map : FCI_Maps.Map;

   ----------------------
   -- Set_Field_C_Info --
   ----------------------

   procedure Set_Field_C_Info
     (SID         : Struct_Id;
      Idx         : Nat;
      Name        : Name_Id   := No_Name;
      Entity      : Entity_Id := Empty;
      Is_Padding  : Boolean   := False;
      Is_Bitfield : Boolean   := False)
   is
      use Entity_To_FCI_Maps;
      Position : constant Cursor           := Find (Entity_To_FCI_Map, SID);
      F_Idx    : constant Field_C_Info_Idx :=
        (if   Has_Element (Position) then Element (Position)
         else No_Field_C_Info_Idx);

   begin
      --  Start by adding an entry to our table. Then either update the
      --  head of the chain or set a new head.

      Field_C_Info.Append ((T           => No_Type_T,
                            F_Number    => Idx,
                            Name        => Name,
                            Entity      => Entity,
                            SID         => SID,
                            Next        => F_Idx,
                            Is_Padding  => Is_Padding,
                            Is_Bitfield => Is_Bitfield));
      if Has_Element (Position) then
         Replace_Element (Entity_To_FCI_Map, Position,
                          Field_C_Info.Last);
      else
         Insert (Entity_To_FCI_Map, SID, Field_C_Info.Last);
      end if;

   end Set_Field_C_Info;

   ----------------
   -- Set_Struct --
   ----------------

   procedure Set_Struct (SID : Struct_Id; T : Type_T) is
      package EFM renames Entity_To_FCI_Maps;
      package TFM renames FCI_Maps;
      Position : constant EFM.Cursor := EFM.Find (Entity_To_FCI_Map, SID);
      F_Idx    : Field_C_Info_Idx;

   begin
      --  If we didn't make any entry in the Field Name Info table for
      --  this type, we don't have anything to do. This could have happened
      --  either if we weren't generating C or if SID denotes a null record.

      if not EFM.Has_Element (Position) then
         return;
      end if;

      --  Otherwise get the first entry we made and loop over all
      --  Field_Name_Info entries for SID, looking for entries where the
      --  LLVM type hasn't yet been set. For each, set the type and add the
      --  (LLVM type, field index) pair to the hash table, but if the type has
      --  no name, don't insert it into the table since it'll be a shared
      --  struct.

      F_Idx := EFM.Element (Position);
      while Present (F_Idx) loop
         declare
            FCI : Field_C_Data renames Field_C_Info.Table (F_Idx);
         begin
            if No (FCI.T) then
               FCI.T := T;

               if Has_Name (T) then
                  TFM.Insert (FCI_Map, (T, FCI.F_Number), F_Idx);
               end if;
            end if;

            F_Idx := FCI.Next;
         end;
      end loop;
   end Set_Struct;

   --------------------
   -- Get_Field_Name --
   --------------------

   function Get_Field_Name (T : Type_T; Idx : Nat) return Str is
      use FCI_Maps;
      Position : constant Cursor := Find (FCI_Map, (T, Idx));
      FCI      : Field_C_Data    :=
        (T, Idx, No_Name, Types.Empty, No_Struct_Id, No_Field_C_Info_Idx,
         False, False);

   begin
      --  If we have information for this field in our table (we should),
      --  replace the default above with that information.

      if Has_Element (Position) then
         FCI := Field_C_Info.Table (Element (Position));
      end if;

      --  Now create a name for the field, based on the saved information.
      --  We really shouldn't be requesting a padding field, but handle it
      --  anyway.

      if Present (FCI.Name) then
         return Get_Name_String (FCI.Name) + C_Name;
      elsif Present (FCI.Entity) then
         return Get_Ext_Name (FCI.Entity) + C_Name;
      elsif FCI.Is_Padding then
         return "ccg_pad_" & Idx;
      elsif FCI.Is_Bitfield then
         return "ccg_bits_" & Idx;
      else
         return "ccg_field_" & Idx;
      end if;
   end Get_Field_Name;

   ----------------------
   -- Get_Field_Entity --
   ----------------------

   function Get_Field_Entity (T : Type_T; Idx : Nat) return Entity_Id is
      use FCI_Maps;
      Position : constant Cursor := Find (FCI_Map, (T, Idx));

   begin
      return (if   Has_Element (Position)
              then Field_C_Info.Table (Element (Position)).Entity
              else Types.Empty);
   end Get_Field_Entity;

   --------
   -- TP --
   --------

   function TP
     (S           : String;
      Op1         : Value_T := No_Value_T;
      Op2         : Value_T := No_Value_T;
      Op3         : Value_T := No_Value_T) return Str
   is
      Start     : Integer   := S'First;
      Result    : Str       := No_Str;
      Mark_Seen : Boolean   := False;
      Modifier  : Character := ' ';
      Op        : Value_T;
      Last      : Integer;

   begin
      for J in S'Range loop

         --  If we've seen '#', look for a modifier

         if Mark_Seen then
            if S (J) in 'A' | 'B' | 'D' | 'I' | 'L' | 'P' | 'T' then
               Modifier := S (J);

            --  If not, then this is a number, representing which operand
            --  to output, possibly as modified by a modifier.

            else
               Op := (case S (J) is when '1' => Op1, when '2' => Op2,
                                    when others => Op3);

               --  The end of any string to output is before our mark, which
               --  may be, e.g., #1 or #B2.

               Last := J - 2 - (if Modifier = ' ' then 0 else 1);
               if Start <= Last then
                  Result := Result & S (Start .. Last);
               end if;

               --  Output the (possibly modified) operand

               case Modifier is
                  when 'A' =>
                     Result := Result & Addr_Of (Op);
                  when 'B' =>
                     Result := Result & Value_As_Basic_Block (Op);
                  when 'D' =>
                     Result := Result & Deref (Op);
                  when 'I' =>
                     Result := Result & (Op + Initializer);
                  when 'L' =>
                     Result := Result & (Op + LHS);
                  when 'T' =>
                     Result := Result & (Op + Write_Type);
                  when others =>
                        Result := Result & Op;
               end case;

               --  Reset for the next string and/or mark

               Mark_Seen := False;
               Modifier  := ' ';
               Start     := J + 1;
            end if;

         elsif S (J) = '#' then
            Mark_Seen := True;
         end if;
      end loop;

      --  See if we have a final string to output and output it if so

      if Start <= S'Last then
         Result := Result & S (Start .. S'Last);
      end if;

      return Result;
   end TP;

   --------------
   -- Num_Uses --
   --------------

   function Num_Uses (V : Value_T) return Nat is
      V_Use : Use_T := Get_First_Use (V);

   begin
      return J : Nat := 0 do
         while Present (V_Use) loop
            J := J + 1;
            V_Use := Get_Next_Use (V_Use);
         end loop;
      end return;
   end Num_Uses;

   ---------------
   -- GNAT_Type --
   ---------------

   function GNAT_Type (V : Value_T) return Opt_Type_Kind_Id is
      E : constant Entity_Id := Get_Entity (V);

   begin
      return (if   No (E) then Types.Empty elsif Is_Type (E) then E
              else Full_Etype (E));
   end GNAT_Type;

   -----------------
   -- Is_Unsigned --
   -----------------

   function Is_Unsigned (V : Value_T) return Boolean is
      TE : constant Opt_Type_Kind_Id := GNAT_Type (V);
      T  : constant Type_T := Type_Of (V);

   begin
      --  Only check for unsigned if V has an integral type or is a function
      --  type that returns an integral type.

      return Present (TE)
        and then ((Is_Function_Type (T)
                     and then Is_Function_Type (Get_Return_Type (T)))
                  or else Is_Integral_Type (T))
        and then Is_Unsigned_Type (TE);
   end Is_Unsigned;

   -----------------
   -- Is_Variable --
   -----------------

   function Is_Variable (V : Value_T) return Boolean is
      E : constant Entity_Id := Get_Entity (V);

   begin
      return Present (E) and then not Is_Type (E) and then Has_Name (V)
        and then Comes_From_Source (E);
   end Is_Variable;

   -----------------------
   -- Might_Be_Unsigned --
   -----------------------

   function Might_Be_Unsigned (V : Value_T) return Boolean is
   begin
      return Is_Unsigned (V)
        or else (Present (Get_C_Value (V))
                   and then Has_Unsigned (Get_C_Value (V)));
   end Might_Be_Unsigned;

   ----------------------
   -- Has_Side_Effects --
   ----------------------

   function Has_Side_Effects (V : Value_T) return Boolean is
   begin
      --  If this isn't an instruction, it doesn't have a side effect. If
      --  it's a call instruction, a terminator, or a load that's either
      --  volatile or not from a variable, it does have side effects.
      --  Otherwise, it has a side effect iff any operand does. We treat a
      --  Phi node as volatile since we can have infinite recursion if we
      --  try to walk its operands.

      return (if    not Is_A_Instruction (V) then False
      elsif Is_A_Call_Inst (V) or else Is_APHI_Node (V)
                    or else Is_A_Terminator_Inst (V)
                    or else Is_A_Store_Inst (V)
                    or else (Is_A_Load_Inst (V)
                             and then (Get_Volatile (V)
                                       or else not Is_Variable
                                                     (Get_Operand0 (V))))
              then True
              else (for some J in Nat range 0 .. Get_Num_Operands (V) - 1 =>
                Has_Side_Effects (Get_Operand (V, J))));

   end Has_Side_Effects;

   -----------------
   -- Update_Hash --
   ----------------

   procedure Update_Hash (H : in out Hash_Type; Key : Hash_Type) is
      function Shift_Left
        (Value  : Hash_Type;
         Amount : Natural) return Hash_Type;
      pragma Import (Intrinsic, Shift_Left);
   begin
      H := Key + Shift_Left (H, 6) + Shift_Left (H, 16) - H;
   end Update_Hash;

   -----------------
   -- Update_Hash --
   ----------------

   procedure Update_Hash (H : in out Hash_Type; S : String) is
   begin
      for C of S loop
         Update_Hash (H, Character'Pos (C));
      end loop;
   end Update_Hash;

   -----------------
   -- Update_Hash --
   -----------------

   procedure Update_Hash (H : in out Hash_Type; B : Boolean) is
   begin
      Update_Hash (H, Boolean'Pos (B));
   end Update_Hash;

   -----------------
   -- Update_Hash --
   -----------------

   procedure Update_Hash (H : in out Hash_Type; V : Value_T) is
   begin
      Update_Hash (H, Hash (V));
   end Update_Hash;

   -----------------
   -- Update_Hash --
   -----------------

   procedure Update_Hash (H : in out Hash_Type; T : Type_T) is
   begin
      Update_Hash (H, Hash (T));
   end Update_Hash;

   -----------------
   -- Update_Hash --
   -----------------

   procedure Update_Hash (H : in out Hash_Type; B : Basic_Block_T) is
   begin
      Update_Hash (H, Hash (B));
   end Update_Hash;

end CCG.Utils;
