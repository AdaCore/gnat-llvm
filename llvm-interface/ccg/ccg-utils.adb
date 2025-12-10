------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2025, AdaCore                     --
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
with Ada.Containers.Hashed_Sets;

with Einfo.Utils; use Einfo.Utils;
with Errout;      use Errout;
with Lib;         use Lib;
with Set_Targ;    use Set_Targ;
with Table;

with GNATLLVM.Utils; use GNATLLVM.Utils;

with CCG.Codegen;      use CCG.Codegen;
with CCG.Instructions; use CCG.Instructions;

package body CCG.Utils is

   --  We have common code to scan a GEP. Return a record giving our result
   --  and then the external functions just extract the needed result
   --  from that.

   type VF is record
      V_Is_Volatile  : Boolean;
      V_Is_Unsigned  : Boolean;
      V_Is_Access_SP : Boolean;
   end record;

   function GNAT_Ref_Type (V : Value_T) return Opt_Type_Kind_Id
     with Pre => Present (V), Inline;
   --  Get the GNAT type that V is referencing, if known

   function GEP_Volatile_Unsigned_Access_SP  (V : Value_T) return VF
     with Pre => Is_A_Get_Element_Ptr_Inst (V);

   function Is_Volatile_GEP (V : Value_T) return Boolean
     with Pre => Is_A_Get_Element_Ptr_Inst (V);
   --  True if V, a GEP instruction, points to a volatile variable or field

   function Is_Unsigned_GEP (V : Value_T) return Boolean
     with Pre => Is_A_Get_Element_Ptr_Inst (V);
   --  True if V, a GEP instruction, points to an unsigned field

   function Is_Access_SP_GEP (V : Value_T) return Boolean
     with Pre => Is_A_Get_Element_Ptr_Inst (V);
   --  True if V, a GEP instruction, points to an access to a subprogram

   function Is_Access_SP_Ref (V : Value_T) return Boolean
     with Pre => Present (V);
   --  True if V is a reference to an access to a subprogram, meaning that the
   --  result of a "load" instruction with that operand will be an access
   --  to a subprogram.

   procedure Update_Function_Type (MD : in out MD_Type; V : Value_T)
     with Pre =>  Is_A_Function (V)
                  and then Is_Function_Type (Designated_Type (MD)),
          Post => Is_Function_Type (Designated_Type (MD))
                  and then Count_Params (V) =
                           Parameter_Count (Designated_Type (MD))
                  and then Is_Layout_Identical
                             (Get_Return_Type (Get_Function_Type (V)),
                              +Return_Type (Designated_Type (MD)));

   --  MD is a type that was previously used for a function of the name as
   --  V, but one or more parameters have been removed as dead or the
   --  return type has changed. Use the parameter names to construct a new
   --  MD_Type corresponding to the new type of the function with the
   --  removed parameters eliminated and the new return type.

   --  We have cases where we want to record information about components
   --  of LLVM types or values, such as fields of a struct type and
   --  parameters of a subprogram. We have a generic that allows us to
   --  record this for both types and values.

   generic
      type Key_T is private;
      No_Key_T : Key_T;
      with function Hash (K : Key_T) return Hash_Type;
      with function Should_Insert (K : Key_T) return Boolean;

   package Component_Info_P is

      type Component_Info_Idx is new Nat;
      No_Component_Info_Idx : constant Component_Info_Idx := 0;

      function Present (C : Component_Info_Idx) return Boolean is
        (C /= No_Component_Info_Idx);

      function Present (K : Key_T) return Boolean is (K /= No_Key_T);
      function No      (K : Key_T) return Boolean is (K = No_Key_T);

      procedure Set_Component_Info
        (UID         : Unique_Id;
         Idx         : Nat;
         Name        : Name_Id   := No_Name;
         Entity      : Entity_Id := Empty;
         Is_Padding  : Boolean   := False;
         Is_Bitfield : Boolean   := False);
      --  Set component info corresponding to Idx in the value or type to be
      --  denoted by UID.

      procedure Set_Key (UID : Unique_Id; K : Key_T)
        with Pre => Present (K);
      --  Indicate that UID corresponds to K

      procedure Delete_Key (K : Key_T)
        with Pre => Present (K);
      --  Delete any mention in our tables of K

      function Get_Component_Name (K : Key_T; Idx : Nat) return Str
        with Pre => Present (K);
      --  Get the name previously stored for index Idx in key K

      function Get_Component_Entity (K : Key_T; Idx : Nat) return Entity_Id;
      --  Get the Entity previously stored for index Idx in key K

      function Is_Component_Padding (K : Key_T; Idx : Nat) return Boolean;
      --  Say whether the field at index Idx in key K is a padding field

   end Component_Info_P;

   package body Component_Info_P is

      type Component_Data is record
         K           : Key_T;
         --  LLVM type or value containing this component

         C_Number    : Nat;
         --  0-origin count of component in key

         Name        : Name_Id;
         --  If Present, the name of the component

         Entity      : Entity_Id;
         --  If Present, the GNAT entity for the component

         UID         : Unique_Id;
         --  A Unique_Id for the component; used only when initializing info

         Next        : Component_Info_Idx;
         --  Index of next field entry for this key

         Is_Padding  : Boolean;
         --  True if this componment is a field, is padding, and doesn't
         --  correspond to any source-level field.

         Is_Bitfield : Boolean;
         --  True if this is a field that's used to store one or more
         --  bitfields.

      end record;

      --  Define the table that records all of the component info

      package Component_Info is new Table.Table
        (Table_Component_Type => Component_Data,
         Table_Index_Type     => Component_Info_Idx,
         Table_Low_Bound      => 1,
         Table_Initial        => 500,
         Table_Increment      => 100,
         Table_Name           => "Component_Info");

      --  We need two maps into the above table. One maps a Unique_Id into
      --  a table entry. This is used to track the initial setting and is
      --  used again when we set the key. The second maps a (key,
      --  component index) pair into the name info for that component.

      function Hash (UID : Unique_Id) return Hash_Type is (Hash_Type (UID));

      package Entity_To_CI_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Unique_Id,
         Element_Type    => Component_Info_Idx,
         Hash            => Hash,
         Equivalent_Keys => "=");
      Entity_To_CI_Map : Entity_To_CI_Maps.Map;

      type FC_Key is record
         K   : Key_T;
         Idx : Nat;
      end record;

      function Hash (K : FC_Key) return Hash_Type is
        (Hash (K.K) + Hash_Type (K.Idx));

      package CI_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => FC_Key,
         Element_Type    => Component_Info_Idx,
         Hash            => Hash,
         Equivalent_Keys => "=");
      CI_Map : CI_Maps.Map;

      ------------------------
      -- Set_Component_Info --
      ------------------------

      procedure Set_Component_Info
        (UID         : Unique_Id;
         Idx         : Nat;
         Name        : Name_Id   := No_Name;
         Entity      : Entity_Id := Empty;
         Is_Padding  : Boolean   := False;
         Is_Bitfield : Boolean   := False)
      is
         use Entity_To_CI_Maps;
         Position : constant Cursor             :=
           Find (Entity_To_CI_Map, UID);
         C_Idx    : constant Component_Info_Idx :=
           (if   Has_Element (Position) then Element (Position)
            else No_Component_Info_Idx);

      begin
         --  Start by adding an entry to our table. Then either update the
         --  head of the chain or set a new head.

         Component_Info.Append ((K           => No_Key_T,
                                 C_Number    => Idx,
                                 Name        => Name,
                                 Entity      => Entity,
                                 UID         => UID,
                                 Next        => C_Idx,
                                 Is_Padding  => Is_Padding,
                                 Is_Bitfield => Is_Bitfield));

         if Has_Element (Position) then
            Replace_Element (Entity_To_CI_Map, Position,
                             Component_Info.Last);
         else
            Insert (Entity_To_CI_Map, UID, Component_Info.Last);
         end if;

      end Set_Component_Info;

      ----------------
      -- Set_Key --
      ----------------

      procedure Set_Key (UID : Unique_Id; K : Key_T) is
         package ECM renames Entity_To_CI_Maps;
         package TCM renames CI_Maps;
         Position : constant ECM.Cursor := ECM.Find (Entity_To_CI_Map, UID);
         C_Idx    : Component_Info_Idx;

      begin
         --  If we didn't make any entry in the table for this key, we
         --  don't have anything to do. This could have happened either if
         --  we weren't generating C or if UID denotes a null record or
         --  a subprogram with no parameters

         if not ECM.Has_Element (Position) then
            return;
         end if;

         --  Otherwise get the first entry we made and loop over all
         --  entries for UID, looking for entries where the key hasn't yet
         --  been set. For each, set the key and add the (key, field index)
         --  pair to the hash table, but if the type has no name, don't
         --  insert it into the table since it'll be a shared struct.

         C_Idx := ECM.Element (Position);
         while Present (C_Idx) loop
            declare
               CD : Component_Data renames Component_Info.Table (C_Idx);
            begin
               if No (CD.K) then
                  CD.K := K;

                  if Should_Insert (K) then
                     TCM.Insert (CI_Map, (K, CD.C_Number), C_Idx);
                  end if;
               end if;

               C_Idx := CD.Next;
            end;
         end loop;
      end Set_Key;

      ----------------
      -- Delete_Key --
      ----------------

      procedure Delete_Key (K : Key_T) is
         use CI_Maps;
      begin
         --  We can potentially do this one of two ways. The first is to
         --  try to "guess" what values of indices are have been stored, so
         --  we can search all of those and delete them. We can't assume
         --  that index 0 exists. We could potentially get the upper bound
         --  from K, but that's iffy because we're called when K is being
         --  deleted. The other approach is to scan our table for all
         --  entries with K. That's quadratic in the number of deleted
         --  keys. We could adopt a hybrid approach of first checking for
         --  the presence of a zero key, but that's probably not worthwhile
         --  because deleted functions are relatively rare.

         for C_Idx in 1 .. Component_Info.Last loop
            declare
               CD : constant Component_Data := Component_Info.Table (C_Idx);

            begin
               if K = CD.K then
                  Exclude (CI_Map, (K, CD.C_Number));
               end if;
            end;
         end loop;
      end Delete_Key;

      ------------------------
      -- Get_Component_Name --
      ------------------------

      function Get_Component_Name (K : Key_T; Idx : Nat) return Str is
         use CI_Maps;
         Position : constant Cursor := Find (CI_Map, (K, Idx));
         CD       : Component_Data    :=
           (K, Idx, No_Name, Types.Empty, No_Unique_Id, No_Component_Info_Idx,
            False, False);

      begin
         --  If we have information for this field in our table (we should),
         --  replace the default above with that information.

         if Has_Element (Position) then
            CD := Component_Info.Table (Element (Position));
         end if;

         --  Now create a name for the component, based on the saved
         --  information. We really shouldn't be requesting a padding
         --  field, but handle it anyway.

         if Present (CD.Name) then
            return Get_Name_String (CD.Name) + C_Name;
         elsif Present (CD.Entity) then
            return Get_Ext_Name (CD.Entity) + C_Name;
         elsif CD.Is_Padding then
            return "ccg_pad_" & Idx;
         elsif CD.Is_Bitfield then
            return "ccg_bits_" & Idx;
         else
            return "ccg_field_" & Idx;
         end if;
      end Get_Component_Name;

      --------------------------
      -- Get_Component_Entity --
      --------------------------

      function Get_Component_Entity (K : Key_T; Idx : Nat) return Entity_Id is
         use CI_Maps;
         Position : constant Cursor := Find (CI_Map, (K, Idx));

      begin
         return (if   Has_Element (Position)
                 then Component_Info.Table (Element (Position)).Entity
                 else Types.Empty);
      end Get_Component_Entity;

      --------------------------
      -- Is_Component_Padding --
      --------------------------

      function Is_Component_Padding (K : Key_T; Idx : Nat) return Boolean is
         use CI_Maps;
         Position : constant Cursor := Find (CI_Map, (K, Idx));

      begin
         return Has_Element (Position)
           and then Component_Info.Table (Element (Position)).Is_Padding;
      end Is_Component_Padding;

   end Component_Info_P;

   --  Now set up instantiations of the package for types and values

   function Should_Insert (Unused_T : Type_T) return Boolean renames Has_Name;
   function Should_Insert (Unused_V : Value_T) return Boolean is (True);

   package CI_T is new Component_Info_P  (Key_T         => Type_T,
                                          No_Key_T      => No_Type_T,
                                          Hash          => Hash,
                                          Should_Insert => Should_Insert);
   package CI_V is new Component_Info_P  (Key_T         => Value_T,
                                          No_Key_T      => No_Value_T,
                                          Hash          => Hash,
                                          Should_Insert => Should_Insert);

   --  And now use them to define the needed functions

   procedure Set_Field_C_Info
     (UID         : Unique_Id;
      Idx         : Nat;
      Name        : Name_Id   := No_Name;
      Entity      : Entity_Id := Empty;
      Is_Padding  : Boolean   := False;
      Is_Bitfield : Boolean   := False) renames CI_T.Set_Component_Info;

   procedure Set_Struct (UID : Unique_Id; T : Type_T) renames CI_T.Set_Key;
   function Get_Field_Name (T : Type_T; Idx : Nat) return Str
     renames CI_T.Get_Component_Name;
   function Get_Field_Entity (T : Type_T; Idx : Nat) return Entity_Id
     renames CI_T.Get_Component_Entity;
   function Is_Field_Padding (T : Type_T; Idx : Nat) return Boolean
     renames CI_T.Is_Component_Padding;

   procedure Set_Function (UID : Unique_Id; V : Value_T) renames CI_V.Set_Key;
   procedure Delete_Function_Info (V : Value_T) renames CI_V.Delete_Key;
   function Get_Parameter_Entity (V : Value_T; Idx : Nat) return Entity_Id
     renames CI_V.Get_Component_Entity;

   -------------------
   -- Set_Parameter --
   -------------------

   procedure Set_Parameter (UID : Unique_Id; Idx : Nat; Entity : Entity_Id) is
   begin
      CI_V.Set_Component_Info (UID, Idx, Entity => Entity);
   end Set_Parameter;

   ------------------------
   -- Is_Ref_To_Volatile --
   ------------------------

   function Is_Ref_To_Volatile (V : Value_T) return Boolean is
   begin
      --  If it is volatile, then it's a reference to a volatile object

      if Is_Volatile (V) then
         return True;

      --  If it's not an operation, it's not a reference to volatile

      elsif not Has_Operands (V) then
         return False;
      end if;

      --  Otherwise, look at the opcode

      case Get_Opcode (V) is

         --  For addition and subtraction look at the first operand

         when Op_Add | Op_Sub =>
            return Is_Ref_To_Volatile (Get_Operand0 (V));

         --  For GEP, first look at the first operand

         when Op_Get_Element_Ptr =>
            return Is_Volatile_GEP (V);

         --  All else isn't known to be volatile

         when others =>
            null;

      end case;

      return False;
   end Is_Ref_To_Volatile;

   -------------------------------
   -- GEP_Volatile_And_Unsigned --
   -------------------------------

   function GEP_Volatile_Unsigned_Access_SP (V : Value_T) return VF is
      Aggr           : constant Value_T := Get_Operand0 (V);
      N_Ops          : constant Nat     := Get_Num_Operands (V);
      Aggr_T         : Type_T           := Get_Element_Type (Aggr);
      V_Is_Volatile  : Boolean          := False;
      V_Is_Unsigned  : Boolean          := False;
      V_Is_Access_SP : Boolean          := False;

   begin
      --  If the input to GEP is volatile, its a reference to volatile

      if Is_Ref_To_Volatile (Aggr) then
         V_Is_Volatile := True;
      end if;

      --  Now look at all operands and find the relevant type for each.
      --  If its a struct, see if we know that the type is volatile.
      --  Otherwise, look at the field and its signedness/volatile
      --  status. We mark the result as volatile if any operand is and
      --  the signedness comes just from the last operand.

      for J in 2 .. N_Ops - 1 loop
         if Is_Array_Type (Aggr_T) then
            Aggr_T        := Get_Element_Type (Aggr_T);
            V_Is_Unsigned := False;
         else
            pragma Assert (Is_Struct_Type (Aggr_T));

            declare
               Idx   : constant Nat                      :=
                 Nat (Const_Int_Get_S_Ext_Value (Get_Operand (V, J)));
               F     : constant Opt_Record_Field_Kind_Id :=
                 Get_Field_Entity (Aggr_T, Idx);

            begin
               if Present (F) then
                  V_Is_Unsigned   :=
                    Is_Unsigned_Type (Full_Base_Type (Full_Etype (F)));
                  V_Is_Volatile   :=
                    V_Is_Volatile or Treat_As_Volatile (F)
                    or Treat_As_Volatile (Full_Etype (F));
                  V_Is_Access_SP := Is_Access_Subprogram_Type (Full_Etype (F));
               end if;

               Aggr_T := Struct_Get_Type_At_Index (Aggr_T, Idx);
            end;
         end if;
      end loop;

      return (V_Is_Volatile, V_Is_Unsigned, V_Is_Access_SP);
   end GEP_Volatile_Unsigned_Access_SP;

   ---------------------
   -- Is_Volatile_GEP --
   ---------------------

   function Is_Volatile_GEP (V : Value_T) return Boolean is
     (GEP_Volatile_Unsigned_Access_SP (V).V_Is_Volatile);

   ---------------------
   -- Is_Unsigned_GEP --
   ---------------------

   function Is_Unsigned_GEP (V : Value_T) return Boolean is
     (GEP_Volatile_Unsigned_Access_SP (V).V_Is_Unsigned);

   ----------------------
   -- Is_Access_SP_GEP --
   ----------------------

   function Is_Access_SP_GEP (V : Value_T) return Boolean is
     (GEP_Volatile_Unsigned_Access_SP (V).V_Is_Unsigned);

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
                  when 'P' =>
                     Result := Result & Process_Operand (Op, X, Unknown);
                  when 'T' =>
                     Result := Result & (Op + Only_Type);
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

   ------------------------
   -- Find_Type_From_Use --
   ------------------------

   function Find_Type_From_Use (V : Value_T) return MD_Type is
      V_Use : Use_T   := Get_First_Use (V);
      MD    : MD_Type;

   begin
      --  Loop through all uses and look at each instruction to see if it's
      --  of a form where multiple operands must be the same type.

      while Present (V_Use) loop
         declare
            Inst  : constant Value_T := Get_User (V_Use);
            N_Ops : constant Nat     := Get_Num_Operands (Inst);
            Op1   : constant Value_T  :=
              (if N_Ops >= 1 then Get_Operand0 (Inst) else No_Value_T);
            Op2   : constant Value_T  :=
              (if N_Ops >= 2 then Get_Operand1 (Inst) else No_Value_T);
            Op3   : constant Value_T  :=
              (if N_Ops >= 3 then Get_Operand2 (Inst) else No_Value_T);

         begin
            if Acts_As_Instruction (Inst) then

               case Get_Opcode (Inst) is

                  --  For binary operations, see if we know the type of the
                  --  result.  Then check the type of the other operand.

                  when Op_Add  | Op_F_Add | Op_F_Sub | Op_Mul   | Op_F_Mul
                 | Op_F_Div | Op_F_Rem | Op_And   | Op_Or    | Op_Xor
                 | Op_U_Div | Op_U_Rem | Op_L_Shr | Op_S_Div | Op_S_Rem
                 | Op_A_Shr =>

                     MD := Declaration_Type (Inst, No_Force => True);
                     if Present (MD) then
                        return MD;

                     elsif V = Op1 then
                        MD := Declaration_Type (Op2, No_Force => True);

                        if Present (MD) then
                           return MD;
                        end if;

                     else
                        pragma Assert (V = Op2);
                        MD := Declaration_Type (Op1, No_Force => True);

                        if Present (MD) then
                           return MD;
                        end if;
                     end if;

                  --  For unary, just check the result

                  when Op_F_Neg =>
                     MD := Declaration_Type (Inst, No_Force => True);

                     if Present (MD) then
                        return MD;
                     end if;

                  --  For select, check the result and the opposite operand

                  when Op_Select =>
                     MD := Declaration_Type (Inst, No_Force => True);

                     if Present (MD) then
                        return MD;

                     elsif V = Op2 then
                        MD := Declaration_Type (Op3, No_Force => True);

                        if Present (MD) then
                           return MD;
                        end if;

                     elsif V = Op3 then
                        MD := Declaration_Type (Op2, No_Force => True);

                        if Present (MD) then
                           return MD;
                        end if;
                     end if;

                  --  For comparisons, both inputs should be the same type

                  when Op_I_Cmp | Op_F_Cmp =>

                     if V = Op1 then
                        MD := Declaration_Type (Op2, No_Force => True);

                        if Present (MD) then
                           return MD;
                        end if;

                     elsif V = Op2 then
                        MD := Declaration_Type (Op1, No_Force => True);

                        if Present (MD) then
                           return MD;
                        end if;
                     end if;

                  --  For load, we have the type of the load

                  when Op_Load =>
                     return Pointer_Type (Declaration_Type
                                            (Get_Load_Store_Type (Inst)));

                  --  Likewise for store, but the result depends on which
                  --  operand this is.

                  when Op_Store =>
                     MD := Declaration_Type (Get_Load_Store_Type (Inst));
                     return (if V = Op1 then MD else Pointer_Type (MD));

                  --  For GEP, we have the source element type

                  when Op_Get_Element_Ptr =>
                     if V = Op1 then
                        return Pointer_Type (Declaration_Type
                                               (Get_GEP_Source_Element_Type
                                                  (Inst)));
                     end if;

                  --  For other instructions, we can't deduce anything

                  when others =>
                     null;
               end case;
            end if;
         end;

         V_Use := Get_Next_Use (V_Use);
      end loop;

      return No_MD_Type;
   end Find_Type_From_Use;

   ---------------
   -- GNAT_Type --
   ---------------

   function GNAT_Type (V : Value_T) return Opt_Type_Kind_Id is
      E : constant Entity_Id := Get_Entity (V);

   begin
      return (if    No (E) then Types.Empty elsif Is_Type (E) then E
              elsif Ekind (Full_Etype (E)) /= E_Void then Full_Etype (E)
              else Empty);
   end GNAT_Type;

   ---------------
   -- GNAT_Ref_Type --
   ---------------

   function GNAT_Ref_Type (V : Value_T) return Opt_Type_Kind_Id is
      TE : constant Opt_Type_Kind_Id := GNAT_Type (V);

   begin
      return (if    Get_Entity_Is_Ref (V) then TE
              elsif Present (TE) and then Is_Access_Type (TE)
              then  Full_Designated_Type (TE) else  Types.Empty);
   end GNAT_Ref_Type;

   ---------------------
   -- Is_Access_SP_Ref --
   ---------------------

   function Is_Access_SP_Ref (V : Value_T) return Boolean is
      TE : constant Opt_Type_Kind_Id := GNAT_Ref_Type (V);

   begin
      --  Note that what we care about here is whether the C compiler will
      --  interpret our generated code for V as a pointer to access to
      --  subprogram, not whether it actually IS such. The only two cases
      --  where we have a pointer to access subprogram are when we have the
      --  address of such a variable or field.

      --  If this is an LHS and a variable, there has to be a declaration,
      --  and we either declared it as access subprogram or we didn't. We
      --  did if the condition below is true.

      if Get_Is_LHS (V) and then Is_Variable (V, False) then
         return Present (TE) and then Is_Access_Subprogram_Type (TE);
      else
         return Is_A_Get_Element_Ptr_Inst (V) and then Is_Access_SP_GEP (V);
      end if;

   end Is_Access_SP_Ref;

   -----------------
   -- Is_Unsigned --
   -----------------

   function Is_Unsigned (V : Value_T) return Boolean is
      MDT : constant MD_Type := Actual_Type (V);

   begin
      return Is_Integer (MDT) and then Is_Unsigned (MDT);
   end Is_Unsigned;

   -------------------------
   -- Is_Unsigned_Pointer --
   -------------------------

   function Is_Unsigned_Pointer (V : Value_T) return Boolean is
      MDT : constant MD_Type := Actual_Type (V);

   begin
      return Is_Pointer (MDT)
        and then Is_Integer (Designated_Type (MDT))
        and then Is_Unsigned (Designated_Type (MDT));
   end Is_Unsigned_Pointer;

   --------------------------
   -- Is_Access_Subprogram --
   --------------------------

   function Is_Access_Subprogram (V : Value_T) return Boolean is
      E      : constant Entity_Id        := Get_Entity (V);
      Is_Ref : constant Boolean          := Get_Entity_Is_Ref (V);
      TE     : constant Opt_Type_Kind_Id := GNAT_Type (V);

   begin
      --  If this is an access subprogram type or a subprogram, this is
      --  the address of a subprogram (a name represents an address in the
      --  LLVM IR).

      if (not Is_Ref and then Present (TE)
          and then Is_Access_Subprogram_Type (TE))
        or else (Is_Ref and then Present (E)
                 and then Ekind (E) in Subprogram_Kind)
      then
         return True;

      --  If it doesn't have operands, we know nothing about it

      elsif not Has_Operands (V) then
         return False;
      end if;

      --  Now handle instructions that could produce an access to a subprogram

      case Get_Opcode (V) is

         --  A load is an access to a subprogram iff the pointer is a
         --  reference to an access to a subprogram

         when Op_Load =>
            return Is_Access_SP_Ref (Get_Operand0 (V));

         --  A call instruction returns an access to a subprogram if the
         --  function called is known and has that as a return type.

         when Op_Call =>

            declare
               TE : constant Opt_Type_Kind_Id := GNAT_Type (Get_Operand0 (V));

            begin
               return Present (TE) and then Is_Access_Subprogram_Type (TE);
            end;

         --  Some conversions don't change whether it is or isn't
         --  an access subprogram.

         when Op_Bit_Cast =>
            return Is_Access_Subprogram (Get_Operand0 (V));

         when others =>
            null;

      end case;

      --  In all other case, it isn't

      return False;
   end Is_Access_Subprogram;

   -----------------
   -- Is_Variable --
   -----------------

   function Is_Variable
     (V : Value_T; Need_From_Source : Boolean := True) return Boolean
   is
      E : constant Entity_Id := Get_Entity (V);

   begin
      return Present (E) and then not Is_Type (E) and then Has_Name (V)
        and then (not Need_From_Source or else Comes_From_Source (E));
   end Is_Variable;

   -----------------
   -- Is_Volatile --
   -----------------

   function Is_Volatile (V : Value_T) return Boolean is
      E : constant Entity_Id := Get_Entity (V);

   begin
      return Present (E) and then Treat_As_Volatile (E);
   end Is_Volatile;

   ----------------------
   -- Has_Side_Effects --
   ----------------------

   function Has_Side_Effects (V : Value_T) return Boolean is
   begin
      --  If we've already written a decl for this or if it isn't an
      --  instruction, it doesn't have a side effect. If it's a call
      --  instruction, a terminator, or a load that's either volatile or
      --  not from a variable, it does have side effects.  Otherwise, it
      --  has a side effect iff any operand does.
      --
      --  ??? The criteria for loads is really loading from a variable
      --  address, not whether the thing loaded corresponds to an Ada
      --  variable.

      return (if Get_Is_Decl_Output (V) or else not Is_A_Instruction (V)
              then False
              elsif Is_A_Call_Inst (V)
                or else (Is_A_Alloca_Inst (V) and then not Get_Is_LHS (V))
                or else Is_A_Terminator_Inst (V)
                or else Is_A_Store_Inst (V)
                or else (Is_A_Load_Inst (V)
                         and then (Get_Volatile (V)
                                   or else not Is_Variable (Get_Operand0 (V))))
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
         Update_Hash (H, Hash_Type (Character'Pos (C)));
      end loop;
   end Update_Hash;

   -----------------
   -- Update_Hash --
   -----------------

   procedure Update_Hash (H : in out Hash_Type; B : Boolean) is
   begin
      Update_Hash (H, Hash_Type (Boolean'Pos (B)));
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

   procedure Update_Hash (H : in out Hash_Type; MD : MD_Type) is
   begin
      Update_Hash (H, Hash (MD));
   end Update_Hash;

   -----------------
   -- Update_Hash --
   -----------------

   procedure Update_Hash (H : in out Hash_Type; B : Basic_Block_T) is
   begin
      Update_Hash (H, Hash (B));
   end Update_Hash;

   -----------------
   -- Update_Hash --
   -----------------

   procedure Update_Hash (H : in out Hash_Type; S : Str) is
   begin
      Update_Hash (H, Hash (S));
   end Update_Hash;

   ----------------------
   -- Safe_Single_User --
   ----------------------

   function Safe_Single_User (V : Value_T) return Value_T is
      User : constant Value_T := Single_User (V);
      Next : Value_T;

   begin
      --  If there isn't a unique single user or V isn't an instruction,
      --  we're done.

      if No (User) or else not Is_A_Instruction (V) then
         return No_Value_T;
      end if;

      --  Otherwise try to find User

      Next := Get_Next_Instruction (V);
      while Present (Next) loop

         --  If we've reached User, we're good. If we reached an instruction
         --  with side-effects, we're bad.

         if Next = User then
            return User;
         elsif Has_Side_Effects (Next) then
            return No_Value_T;
         end if;

         Next := Get_Next_Instruction (Next);
      end loop;

      return No_Value_T;

   end Safe_Single_User;

   -----------------
   -- Walk_Object --
   -----------------

   procedure Walk_Object (V : Value_T) is
      package Walked_Sets is new Ada.Containers.Hashed_Sets
        (Element_Type        => Value_T,
         Hash                => Hash,
         Equivalent_Elements => "=");
      Walked : Walked_Sets.Set;
      use Walked_Sets;

      procedure Walk_Value (V : Value_T; Walk_Outer : Boolean := True);

      ----------------
      -- Walk_Value --
      ----------------

      procedure Walk_Value (V : Value_T; Walk_Outer : Boolean := True) is
      begin
         if Present (V) then

            --  If we've walked this already, we're done

            if Contains (Walked, V) then
               return;
            end if;

            --  Otherwise, show that we've walked it and call the procedure
            --  on this value (unless we're asked not to).

            Insert (Walked, V);

            if Walk_Outer then
               Process (V);
            end if;

            --  If this is a global variable with an initializer, walk the
            --  value of that initializer.

            if Is_A_Global_Variable (V) and then Present (Get_Initializer (V))
            then
               Walk_Value (Get_Initializer (V));

            --  Otherwise, see if there are any values below this to walk

            elsif Has_Operands (V) or else Is_A_Constant_Struct (V)
              or else Is_A_Constant_Array (V)
            then
               for J in Nat (0) .. Get_Num_Operands (V) - 1 loop
                  Walk_Value (Get_Operand (V, J));
               end loop;
            end if;
         end if;
      end Walk_Value;

   begin
      --  If this is a function, walk every instruction in each BB

      if Is_A_Function (V) then
         declare
            BB   : Basic_Block_T := Get_First_Basic_Block (V);
            Inst : Value_T;

         begin
            while Present (BB) loop
               Inst := Get_First_Instruction (BB);
               while Present (Inst) loop
                  Walk_Value (Inst);
                  Inst := Get_Next_Instruction (Inst);
               end loop;

               BB := Get_Next_Basic_Block (BB);
            end loop;
         end;

      --  Otherwise, we just walk the value

      else
         Walk_Value (V, False);
      end if;
   end Walk_Object;

   ---------------------
   -- Is_Same_C_Types --
   ---------------------

   function Is_Same_C_Types
     (MD1, MD2 : MD_Type; Match_Void : Boolean := False) return Boolean
   is
   begin
      --  If the types are the same, they're identical, but if they have
      --  different kinds, they aren't.

      if MD1 = MD2 then
         return True;
      elsif Class (MD1) /= Class (MD2) then
         return False;
      end if;

      --  Otherwise, it's kind-specific

      case Class (MD1) is

         when Void_Class =>
            return True;

         when Integer_Class =>

            --  The two types are the same if they have the same bitsize and
            --  signedness.
            --
            --  ??? We want to treat unknown signedness as a "don't
            --  care", but we can't because mixing a pointer to sign
            --  and a pointer to unsigned can cause a warning in C.
            --  When we've gotten here, we know that the MD_Types are
            --  different, so if either has unknown signedness, we must
            --  treat them as different, at least for now.

            return Int_Bits (MD1) = Int_Bits (MD2)
              and then Is_Unsigned (MD1) = Is_Unsigned (MD2)
              and then not Is_Unknown_Sign (MD1)
              and then not Is_Unknown_Sign (MD2);

         when Float_Class =>
            return Float_Bits (MD1) = Float_Bits (MD2);

         when Array_Class =>
            return
              Is_Fixed_Array (MD1) = Is_Fixed_Array (MD2)
              and then (Is_Variable_Array (MD1)
                          or else Array_Count (MD1) = Array_Count (MD2))
              and then Is_Same_C_Types (Element_Type (MD1),
                                        Element_Type (MD2), Match_Void);

         when Struct_Class =>

            --  If either struct is named, they are the same C type
            --  if and only if they have the same name.

            if Has_Name (MD1) or else Has_Name (MD2) then
               return Has_Name (MD1) and then Has_Name (MD2)
                 and then MD_Name (MD1) = MD_Name (MD2);
            end if;

            --  Structures are identical if their packed status is the
            --  same, they have the same number of fields, and each field
            --  is identical.

            return Is_Packed (MD1) = Is_Packed (MD2)
              and then Has_Fields (MD1) = Has_Fields (MD2)
              and then (not Has_Fields (MD1)
                          or else Element_Count (MD1) = Element_Count (MD2))
              and then (not Has_Fields (MD1)
                          or else (for all J in 0 .. Element_Count (MD1) - 1 =>
                                     Is_Same_C_Types
                                       (Element_Type (MD1, J),
                                        Element_Type (MD2, J), Match_Void)));

         when Pointer_Class =>

            --  Pointers are the same if they're in the same address
            --  space and pointing to the same type, unless one is a
            --  void pointer and we're asked to match them.

            return Pointer_Space (MD1) = Pointer_Space (MD2)
              and then ((Match_Void and then (MD1 = Void_Ptr_MD
                                                or else MD2 = Void_Ptr_MD))
                          or else Is_Same_C_Types (Designated_Type (MD1),
                                                   Designated_Type (MD2),
                                                   Match_Void));

         when Function_Class =>

            --  Two function types have different layouts if their return
            --  types have different layouts, they have a different number
            --  of parameter types, or any parameter type is not the
            --  identical layout of the corresponding parameter type.

            return Is_Same_C_Types (Return_Type (MD1), Return_Type (MD2),
                                    Match_Void)
              and then Parameter_Count (MD1) = Parameter_Count (MD2)
              and then (for all J in 0 .. Parameter_Count (MD1) - 1 =>
                          Is_Same_C_Types (Parameter_Type (MD1, J),
                                           Parameter_Type (MD2, J),
                                           Match_Void));
      end case;

   end Is_Same_C_Types;

   --------------------
   -- Is_Better_Typs --
   --------------------

   function Is_Better_Type (MD1, MD2 : MD_Type) return Boolean
   is
   begin
      --  If the types are the same, they're identical, so neither
      --  is better. If they're different kinds, they aren't the same.

      if MD1 = MD2 or else Class (MD1) /= Class (MD2) then
         return False;
      end if;

      --  Otherwise, it's kind-specific

      case Class (MD1) is

         when Void_Class =>
            return False;

         when Integer_Class =>

            --  If they're different bit sizes, they aren't the same.
            --  Otherwise, MD2 is better only if MD1 has unknown signedness.

            return Int_Bits (MD1) = Int_Bits (MD2)
              and then Is_Unknown_Sign (MD1);

         when Float_Class =>
            return False;

         when Array_Class =>
            return
              Is_Fixed_Array (MD1) = Is_Fixed_Array (MD2)
              and then (Is_Variable_Array (MD1)
                        or else Array_Count (MD1) = Array_Count (MD2))
              and then Is_Better_Type (Element_Type (MD1), Element_Type (MD2));

         when Struct_Class =>

            --  If either struct is named, they are the same C type
            --  if and only if they have the same name, which means that
            --  neither is better.

            if Has_Name (MD1) or else Has_Name (MD2) then
               return False;
            end if;

            --  One structure is better if their packed status is the same,
            --  they have the same number of fields, and each field is
            --  identical, but one is better.

            if Is_Packed (MD1) /= Is_Packed (MD2)
              or else Has_Fields (MD1) /= Has_Fields (MD2)
              or else (Has_Fields (MD1)
                       and then Element_Count (MD1) /= Element_Count (MD2))
              or else (Has_Fields (MD1)
                       and then (for some J in 0 .. Element_Count (MD1) - 1 =>
                                   not Is_Same_C_Types
                                         (Element_Type (MD1, J),
                                          Element_Type (MD2, J),
                                          Match_Void => True)))
            then
               return False;
            else
               return (for some J in 0 .. Element_Count (MD1) - 1 =>
                         Is_Better_Type (Element_Type (MD1, J),
                                         Element_Type (MD2, J)));
            end if;

         when Pointer_Class =>

            --  MD2 is better if they're in the same address space and
            --  MD1 is a pointer to void or if what MD2 points to is better.

            return Pointer_Space (MD1) = Pointer_Space (MD2)
              and then (MD1 = Void_Ptr_MD
                        or else Is_Better_Type (Designated_Type (MD1),
                                                Designated_Type (MD2)));

         when Function_Class =>

            --  If the types aren't identical, one isn't better.  Otherwise,
            --  one is better if either the return type or one parameter
            --  type is better.

            if not Is_Same_C_Types (MD1, MD2, Match_Void => True) then
               return False;
            else
               return Is_Better_Type (Return_Type (MD1), Return_Type (MD2))
                 or else (for some J in 0 .. Parameter_Count (MD1) - 1 =>
                            Is_Better_Type (Parameter_Type (MD1, J),
                                            Parameter_Type (MD2, J)));
            end if;
      end case;

   end Is_Better_Type;

   --------------------------
   -- Update_Function_Type --
   --------------------------

   procedure Update_Function_Type (MD : in out MD_Type; V : Value_T) is
      Num_Params  : constant Nat     := Count_Params (V);
      Fn_MD       : constant MD_Type := Designated_Type (MD);
      Fn_T        : constant Type_T  := Get_Function_Type (V);
      Return_T    : constant Type_T  := Get_Return_Type (Fn_T);
      Return_MD   : constant MD_Type :=
        (if   Return_T = +Return_Type (Fn_MD)
         then Return_Type (Fn_MD) else Declaration_Type (Return_T));
      New_Idx     : Nat              := 0;
      Param_Types : MD_Type_Array (0 .. Num_Params - 1);
      Param_Names : Name_Id_Array (0 .. Num_Params - 1);

   begin
      --  We loop through each original parameter. If it's not the same name
      --  as the next parameter in the new function, it's been removed.

      for Old_Idx in 0 .. Parameter_Count (Fn_MD) - 1 loop
         if New_Idx < Num_Params
           and then Get_Name_String (Parameter_Name (Fn_MD, Old_Idx)) =
                    Get_Value_Name (Get_Param (V, New_Idx))
         then
            Param_Types (New_Idx) := Parameter_Type (Fn_MD, Old_Idx);
            Param_Names (New_Idx) := Parameter_Name (Fn_MD, Old_Idx);
            New_Idx               := New_Idx + 1;
         end if;
      end loop;

      --  We should have accounted for all of the current arguments

      pragma Assert (New_Idx = Num_Params);

      MD := Pointer_Type (Fn_Ty (Param_Types, Return_MD, Param_Names));

   end Update_Function_Type;

   ----------------------
   -- Declaration_Type --
   ----------------------

   function Declaration_Type
     (V        : Value_T;
      No_Force : Boolean := False;
      No_Scan  : Boolean := False) return MD_Type
   is
      T  : constant Type_T := Type_Of (V);
      MD : MD_Type         := No_MD_Type;

   begin
      --  If we have a single type for this value, use it

      if Present (Get_MD_Type (V)) and then not Get_Is_Multi_MD (V) then
         MD := Get_MD_Type (V);

         --  Otherwise, if this is a global variable or a function and we have
         --  a type set for that, use it.

      elsif (Is_A_Global_Variable (V) or else Is_A_Function (V))
        and then Present (Get_MD_Type (Get_Value_Name (V)))
      then
         MD := Get_MD_Type (Get_Value_Name (V));

         --  We may have a situation where the optimizer removed one or
         --  more dead arguments or changed the return type. So check for
         --  and handle that case.

         if Is_A_Function (V) then
            if Count_Params (V) /= Parameter_Count (Designated_Type (MD))
              or else not Is_Same_C_Types
                (From_Type (Get_Return_Type (Get_Function_Type (V))),
                 Return_Type (Designated_Type (MD)), True)
            then
               Update_Function_Type (MD, V);
            end if;
         end if;

         --  Otherwise, if this is a global variable or a function, see if
         --  there's an LLVM type corresponding to the value (not V's type,
         --  which is a pointer).

      elsif (Is_A_Global_Variable (V) or else Is_A_Function (V))
        and then Present (Declaration_Type (Global_Get_Value_Type (V)))
      then
         MD := Pointer_Type (Declaration_Type (Global_Get_Value_Type (V)));

         --  Othewise, if we have a single MD_Type corresponding to V's
         --  type, use that

      elsif Present (Get_MD_Type (T)) and then not Get_Is_Multi_MD (T) then
         MD := Get_MD_Type (T);

         --  If it's an operation, see if we can deduce the type from
         --  the operands of the operation. This is simplest in the
         --  case of unary or binary operations. In most other cases, all
         --  we have is the type of the result.

      elsif Has_Operands (V) then

         case Get_Opcode (V) is

            --  We start with simple cases where we can either derive
            --  the type directly from the operation or we look at the
            --  type of the operand(s) of the instruction.

            when Op_F_Neg =>
               MD := Declaration_Type (Get_Operand0 (V), No_Force => True);

            when Op_Add  | Op_F_Add | Op_F_Sub | Op_Mul | Op_F_Mul
              | Op_F_Div | Op_F_Rem | Op_And   | Op_Or  | Op_Xor =>

               MD := Declaration_Type (Get_Operand0 (V), No_Force => True);

               if No (MD) then
                  MD := Declaration_Type (Get_Operand1 (V), No_Force => True);
               end if;

            when Op_U_Div | Op_U_Rem | Op_L_Shr =>
               MD := Declaration_Type (Get_Operand0 (V), No_Force => True);

               if Present (MD) and then Is_Integer (MD) then
                  MD := Unsigned_Type (MD);
               end if;

            when Op_S_Div | Op_S_Rem | Op_A_Shr =>
               MD := Declaration_Type (Get_Operand0 (V), No_Force => True);

               if Present (MD) and then Is_Integer (MD) then
                  MD := Signed_Type (MD);
               end if;

            when Op_Alloca =>
               MD := Pointer_Type (From_Type (Get_Allocated_Type (V)));

            when Op_Bit_Cast =>

               --  A cast from one pointer type to another is a nop, so
               --  we use the type of the operand. Otherwise, we use the
               --  type of the instruction.

               MD := Declaration_Type (Get_Operand0 (V), No_Force => True);
               if No (MD) or else not Is_Pointer (MD)
                 or else not Is_Pointer_Type (V)
               then
                  MD := From_Type (Type_Of (V));
               end if;

            when Op_Load =>

               --  The load might be loading the full data from the pointer
               --  or might be casting it into a pointer to a smaller portion
               --  of the data. We can only get an accurate MD_Type for the
               --  load in the former case, which we detect by comparing the
               --  LLVM type corresponding to the MD Type with the type of
               --  the load.

               MD := Declaration_Type (Get_Operand0 (V), No_Force => True);
               MD := (if Present (MD) then Designated_Type (MD) else MD);

               if Present (MD) and then T /= +MD then
                  MD := No_MD_Type;
               end if;

               --  For GEP, we're dealing with pointers and we have a "hint"
               --  of the type of the aggregate. We always use that type
               --  because the processing of the GEP will force the input to
               --  that type.

            when Op_Get_Element_Ptr =>

               MD := Declaration_Type (Get_GEP_Source_Element_Type (V));

               for J in Nat (2) .. Get_Num_Operands (V) - 1 loop
                  if Is_Struct (MD) then
                     MD := Element_Type
                       (MD, Nat (Const_Int_Get_S_Ext_Value
                                   (Get_Operand (V, J))));
                  else
                     MD := Element_Type (MD);
                  end if;
               end loop;

               MD := Pointer_Type (MD);

            when Op_Call =>
               return
                 Return_Type (Declaration_Type (Get_Called_Function_Type (V)));

            when others =>
               null;
         end case;
      end if;

      --  If we haven't found a type or if the best we've been able to do
      --  is pointer to void, see if we can get a type from any use of this
      --  value unless its too early to safely do that or we're not to try
      --  to hard.

      if not No_Scan and then not No_Force
        and then (No (MD) or else MD = Void_Ptr_MD)
      then
         MD := Find_Type_From_Use (V);
      end if;

      --  If we have a type, we're done. But first see if this value
      --  hasn't already had a type assigned for it. If not, assign
      --  this one so that we don't have to repeat the above logic the
      --  next time we do this.

      if Present (MD) then
         if not Get_Is_Multi_MD (V) then
            Set_MD_Type (V, MD);
         end if;

      --  Otherwise, if we're supposed to force a type from V's LLVM
      --  type, do that.

      elsif not No_Force then
         MD := From_Type (T);
      end if;

      return MD;
   end Declaration_Type;

   ----------------------
   -- Declaration_Type --
   ----------------------

   function Declaration_Type
     (T : Type_T; No_Force : Boolean := False) return MD_Type
   is
      (if   Present (Get_MD_Type (T)) and then not Get_Is_Multi_MD (T)
       then Get_MD_Type (T) elsif not No_Force then From_Type (T)
       else No_MD_Type);

   -----------------
   -- Actual_Type --
   -----------------

   function Actual_Type (V : Value_T; As_LHS : Boolean := False) return MD_Type
   is
      MD : MD_Type;

   begin
      --  If V has been declared, its actual type corresponds to the
      --  declared type. If it's not an operation, that's the best
      --  we're going to do by looking backwards.

      if Get_Is_Decl_Output (V) or else not Has_Operands (V) then
         MD := Declaration_Type (V);
         MD := (if   Get_Is_LHS (V) and then not As_LHS
                     and then not Is_Function_Pointer (MD)
                then Designated_Type (MD) else MD);

      --  Otherwise, what we do depends on the operation

      else
         case Get_Opcode (V) is

            --  There are also some instructions that "start over" in that
            --  the type of the result of that instruction isn't a function
            --  of the types of the operands, but is always the same.

            when Op_Alloca | Op_U_Div | Op_U_Rem | Op_S_Div | Op_S_Rem
               | Op_L_Shr | Op_A_Shr | Op_I_Cmp | Op_F_Cmp | Op_Trunc
               | Op_SI_To_FP | Op_FP_Trunc | Op_FP_Ext | Op_S_Ext
               | Op_UI_To_FP | Op_FP_To_SI | Op_FP_To_UI | Op_Z_Ext
               | Op_Ptr_To_Int | Op_Int_To_Ptr | Op_Get_Element_Ptr =>

               MD := Declaration_Type (V);

            --  For FP operations, the type of the output is the same as
            --  that of the input.  If the second input is declared, use
            --  its type. Otherwise, just look at the first operand (for
            --  simplicity).

            when Op_F_Add | Op_F_Sub | Op_F_Mul | Op_F_Div | Op_F_Rem =>

               MD := (if   Get_Is_Decl_Output (Get_Operand1 (V))
                      then Actual_Type (Get_Operand1 (V), As_LHS)
                      else Actual_Type (Get_Operand0 (V), As_LHS));

            --  We have a similar situation with the two branches of SELECT.

            when Op_Select =>

               MD := (if   Get_Is_Decl_Output (Get_Operand2 (V))
                      then Actual_Type (Get_Operand2 (V), As_LHS)
                      else Actual_Type (Get_Operand1 (V), As_LHS));

            --  Similarly for integer arithmetic operations that don't
            --  force signedness, except that we need to deal with C
            --  integral promotion.

            when Op_Add | Op_Sub | Op_Mul | Op_Shl | Op_And | Op_Or | Op_Xor =>

               MD := (if   Get_Is_Decl_Output (Get_Operand1 (V))
                      then Actual_Type (Get_Operand1 (V), As_LHS)
                      else Actual_Type (Get_Operand0 (V), As_LHS));
               MD := (if   Int_Bits (MD) >= Int_Size then MD
                      else Int_Ty (Int_Bits (MD), Is_Unsigned (MD)));

            --  There are some operations where the input type is the same
            --  as the output type. We can get a return here in unusual
            --  circumstances.

            when Op_F_Neg | Op_Insert_Value | Op_Freeze | Op_Ret =>
               MD := Actual_Type (Get_Operand0 (V), As_LHS);

            --  A bit cast is a noop when both input and output are pointers
            --  but "start over" when they aren't.

            when Op_Bit_Cast =>

               MD := (if   Is_Pointer_Type (V)
                           and then Is_Pointer_Type (Get_Operand0 (V))
                      then Actual_Type (Get_Operand0 (V), As_LHS)
                      else Declaration_Type (V));

            --  For a load, get the type used for the load

            when Op_Load =>

               return LS_Op_MD (V, Get_Operand0 (V));

            --  For a call, see if the actual type of the called function
            --  is known to be a pointer to a function type. If not, take
            --  the type of the called function.  Then use the return type.

            when Op_Call =>

               MD := Actual_Type (Get_Operand0 (V));
               MD := Return_Type ((if   Is_Function_Pointer (MD)
                                   then Designated_Type (MD)
                                   else Declaration_Type
                                          (Get_Called_Function_Type (V))));

            --  For extractvalue, we start with the value that's in the first
            --  operand and then look inside to get the element types as we
            --  go deeper inside it.

            when Op_Extract_Value =>

               MD := Actual_Type (Get_Operand0 (V), As_LHS);

               for J in Nat (0) .. Get_Num_Indices (V) - 1 loop
                  if Is_Struct (MD) then
                     MD := Element_Type (MD, Get_Index (V, J));
                  else
                     MD := Element_Type (MD);
                  end if;
               end loop;

            --  Otherwise, someting went wrong

            when others =>
               pragma Assert (Standard.False);
               MD := No_MD_Type;
         end case;
      end if;

      return MD;
   end Actual_Type;

   ----------------
   -- Maybe_Cast --
   ----------------

   function Maybe_Cast
     (MD       : MD_Type;
      V        : Value_T;
      As_LHS   : Boolean := False;
      For_Call : Boolean := False) return Str
   is
      A_MD   : constant MD_Type := Actual_Type (V, As_LHS);
      C_MD   : constant MD_Type :=
        (if As_LHS then Designated_Type (MD) else MD);
      C_A_MD : constant MD_Type :=
        (if As_LHS then Designated_Type (A_MD) else A_MD);

   begin
     --  Never cast to a C aggregate or function type because it's useless
     --  and can cause errors with some compilers.

      if Class (MD) in Struct_Class | Array_Class | Function_Class then
         return +V;

      --  If V is a function that needs a "nest" parameter, we must cast
      --  because we have inconsistent types in that case. But don't do
      --  this is this is for a call itself.
      --  ??? This mechanism is a kludge.

      elsif not For_Call and then Is_A_Function (V) and then Needs_Nest (V)
      then
         return "(" & MD & ") " & (V + Unary);

      --  If the types to be compared are viewed as the same type with
      --  void * matching any pointer, but the type we have is better,
      --  don't cast.

      elsif Is_Same_C_Types (C_MD, C_A_MD, Match_Void => True)
        and then not Is_Better_Type (C_A_MD, C_MD)
      then
         return +V;

      --  Otherwise see if they're the same type or point to the same
      --  type, depending on whether we care about the type or what they
      --  point to.

      elsif Is_Same_C_Types (C_MD, C_A_MD) then
         return +V;
      else
         return "(" & MD & ") " & (V + Unary);
      end if;

   end Maybe_Cast;

   ---------------------
   -- Int_Type_String --
   ---------------------

   function Int_Type_String (Size : Pos; Unsigned_P : Boolean) return Str is
   begin
      --  If we're to use the <stdint.h> sizes, our logic is simple.

      if Use_Stdint then
         declare
            Our_Size : constant Pos :=
              (if    Size <= 8 then 8 elsif Size <= 16 then 16
               elsif Size <= 32 then 32 else 64);

         begin
            return
              (if Unsigned_P then +"u" else +"") & "int" & Our_Size & "_t";
         end;

      --  ??? There are a number of issues here: Ada supports a "long long
      --  long" type, which could correspond to C's int128_t. But for now
      --  we'll keep it simple.

      else
         declare
            Result : constant Str :=
              (if Unsigned_P then +"unsigned " else +"");

         begin
            if Size > Long_Size and then Size > Int_Size
              and then Size <= Long_Long_Size
            then
               return Result & "long long";
            elsif Size > Int_Size and then Size <= Long_Size then
               return Result & "long";
            elsif Size > Short_Size and then Size <= Int_Size then
               return Result & "int";
            elsif Size > Char_Size and then Size <= Short_Size then
               return Result & "short";
            elsif Size <= Char_Size then
               return Result & (if Unsigned_P then "char" else "signed char");
            else
               return +"<unknown int type:" & Size'Image & ">";
            end if;
         end;
      end if;
   end Int_Type_String;

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg (Msg : String; V : Value_T) is
      E : constant Entity_Id :=
        (if Present (V) then Get_Entity (V) else Empty);

   begin
      --  First see if this corresponds to an entity that we can get a
      --  Sloc from.

      if Present (E) and then not Is_Type (E) then
         Error_Msg_N (Msg, E);

      --  Otherwise, post it on the main unit and try to find a line and
      --  column.

      elsif Present (V)
        and then (Is_A_Instruction (V) or else Is_A_Function (V)
                  or else Is_A_Global_Variable (V))
      then
         declare
            File : constant String := Get_Debug_Loc_Filename (V);
            Line : constant String := CCG.Helper.Get_Debug_Loc_Line (V)'Image;
         begin
            if File /= "" then
               Error_Msg_N (Msg & " at " & File & ":" & Line (2 .. Line'Last),
                            Cunit (Main_Unit));
               return;
            end if;
         end;
      end if;

      Error_Msg_N (Msg, Cunit (Main_Unit));
   end Error_Msg;

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg (Msg : String; MD : MD_Type) is
      T : constant Type_T := +MD;
   begin
      --  First see if this corresponds to a type that we can get a Sloc
      --  from.

      if Present (Get_Entity (T)) then
         Error_Msg_NE (Msg & " for type &", Get_Entity (T), Get_Entity (T));

      --  Otherwise, post it on the main unit and try to find a name for
      --  the type.

      elsif Is_Struct (MD) and then Has_Name (MD) then
         Error_Msg_N (Msg & " for type `" & Get_Name_String (MD_Name (MD)) &
                      "`",
                      Cunit (Main_Unit));
      else
         Error_Msg_N (Msg, Cunit (Main_Unit));
      end if;
   end Error_Msg;

end CCG.Utils;
