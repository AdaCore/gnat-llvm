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

with Einfo.Utils; use Einfo.Utils;
with Elists;      use Elists;
with Errout;      use Errout;
with Lib;         use Lib;
with Opt;
with Sem;         use Sem;
with Sem_Aux;     use Sem_Aux;
with Sem_Ch13;    use Sem_Ch13;
with Sem_Eval;    use Sem_Eval;
with Sem_Util;    use Sem_Util;
with Table;       use Table;

with GNATLLVM.Aliasing.Params; use GNATLLVM.Aliasing.Params;
with GNATLLVM.Arrays;          use GNATLLVM.Arrays;
with GNATLLVM.Environment;     use GNATLLVM.Environment;
with GNATLLVM.GLType;          use GNATLLVM.GLType;
with GNATLLVM.Helper;          use GNATLLVM.Helper;
with GNATLLVM.Instructions;    use GNATLLVM.Instructions;
with GNATLLVM.Records;         use GNATLLVM.Records;
with GNATLLVM.Types;           use GNATLLVM.Types;
with GNATLLVM.Utils;           use GNATLLVM.Utils;
with GNATLLVM.Wrapper;         use GNATLLVM.Wrapper;

package body GNATLLVM.Aliasing is

   --  Define accessor functions for type tags

   function TBAA_Parent (MD : Metadata_T) return Metadata_T is
     (Get_Metadata_Operand (MD, 0))
     with Pre => Present (MD);

   function Size_In_Bytes (MD : Metadata_T) return ULL is
     (Get_Metadata_Operand_Constant_Value (MD, 1))
     with Pre => Present (MD);

   function Is_Struct_Tag (MD : Metadata_T) return Boolean is
     (Get_Metadata_Num_Operands (MD) > 3)
     with Pre => Present (MD);

   function Last_Field_Index (MD : Metadata_T) return Nat is
     ((Get_Metadata_Num_Operands (MD) - 3) / 3 - 1)
     with Pre => Is_Struct_Tag (MD);

   function Field_Type (MD : Metadata_T; Idx : Nat) return Metadata_T is
     (Get_Metadata_Operand (MD, 3 + Idx * 3))
     with Pre => Is_Struct_Tag (MD);

   function Field_Offset (MD : Metadata_T; Idx : Nat) return ULL is
     (Get_Metadata_Operand_Constant_Value (MD, 3 + Idx * 3 + 1))
     with Pre => Is_Struct_Tag (MD);

   function Field_Size (MD : Metadata_T; Idx : Nat) return ULL is
     (Get_Metadata_Operand_Constant_Value (MD, 3 + Idx * 3 + 2))
     with Pre => Is_Struct_Tag (MD);

   --  We need to record all types that are the designated types of access
   --  types that are unchecked-converted into each other. All of those
   --  types need to have the same TBAA value. Likewise for a UC where one
   --  of the types is an aggregate.
   --
   --  We rely on the fact that UC's are uncommon, so we can have a table
   --  that we traverse inefficiently. The table maps the entity for a type
   --  (the designated type) into an ordinal corresponding to the types
   --  which have access types that are UC'ed to each other. If any of
   --  these objects are aggregates, we can't do this because we can't use
   --  the same struct type tag for other than that struct. We also can't
   --  do this if any objects in the group are of different sizes. So
   --  check for that and invalidate the group if so.

   type UC_Group_Idx is new Nat;
   Empty_UC_Group_Idx : constant UC_Group_Idx := 0;

   function No      (Idx : UC_Group_Idx) return Boolean is
     (Idx = Empty_UC_Group_Idx);
   function Present (Idx : UC_Group_Idx) return Boolean is
     (Idx /= Empty_UC_Group_Idx);

   type UC_Entry is record
      TE    : Type_Kind_Id;
      Group : UC_Group_Idx;
      Valid : Boolean;
   end record;

   package UC is new Table.Table
     (Table_Component_Type => UC_Entry,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 3,
      Table_Increment      => 1,
      Table_Name           => "UC");

   Last_UC_Group : UC_Group_Idx := 0;
   --  Last UC group number used when we need a new one

   function Find_UC_Group (TE : Type_Kind_Id) return UC_Group_Idx;
   --  Return the UC_Group_Idx corresponding to TE, if any

   procedure Search_For_UCs;
   --  Look through all units for UC's between two access types

   --  The front end often makes distinct subtypes that have the same
   --  relationship to the base type. For example, there may be multiple
   --  array subtypes with bounds 1 .. 2 and there may be multiple record
   --  subtypes with the same discriminant value. The front end uses these
   --  subtypes interchangably, so we need to be sure that we use the same
   --  TBAA type tag for each of them. To do this, we maintain a list of
   --  subtypes of a base type for which we've made a TBAA and see if a new
   --  subtype is the same as any of those. The chain is maintained via the
   --  following table (where each entry points to another entry in the
   --  chain, ending in zero) and the head of the chain in is the TBAA_Data
   --  record below.

   type TBAA_Equiv is record
      Next : Nat;
      TE   : Type_Kind_Id;
   end record;

   package TBAA_Equiv_Subtype is new Table.Table
     (Table_Component_Type => TBAA_Equiv,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 50,
      Table_Name           => "TBAA_Equivalent_Subtype");

   function Find_Equiv_Subtype (TE : Type_Kind_Id) return Type_Kind_Id
     with Post => Base_Type_For_Aliasing (TE) =
                  Base_Type_For_Aliasing (Find_Equiv_Subtype'Result);
   --  If TE is a subtype and there's another subtype of its base type
   --  with an equivalent layout, return it. Otherwise, return TE.

   --  There are various objects related to arrays that each need to have
   --  their own TBAAA type tags. These basically correspond to different
   --  GL_Relationships. For most values here, the type data for both the
   --  base type and subtypes are the same. The exception is
   --  Bounds_And_Data, which is only defined for constrained subtypes. In
   --  that case, it represents a structure consisting of both the bounds
   --  (which are the same for all subtypes) and the data (which is unique
   --  to the subtype and defined only for that subtype). Note that the
   --  TBAA type tag for the data isn't stored here, but rather as the TBAA
   --  data for the subtype. For records, the only data used is for base
   --  types and contains the subtype chain.

   type TBAA_Data is record
      Bounds          : Metadata_T;
      Bounds_And_Data : Metadata_T;
      Component       : Metadata_T;
      Subtype_Chain   : Nat;
   end record;

   package TBAA_Info is new Table.Table
     (Table_Component_Type => TBAA_Data,
      Table_Index_Type     => TBAA_Info_Id,
      Table_Low_Bound      => TBAA_Info_Low_Bound,
      Table_Initial        => 100,
      Table_Increment      => 50,
      Table_Name           => "TBAA_Info");

   TBAA_Root : Metadata_T;
   --  Root of tree for Type-Based alias Analysis (TBAA) metadata

   function Base_Type_For_Aliasing (TE : Type_Kind_Id) return Type_Kind_Id;
   --  Given a type, return the GNAT type to be used to as the base type
   --  for aliasing purposes.

   function Is_Subtype_For_Aliasing (TE1, TE2 : Type_Kind_Id) return Boolean is
     (Base_Type_For_Aliasing (TE1) = Base_Type_For_Aliasing (TE2)
        or else (Is_Tagged_Type (TE1) and then Is_Derived_Type (TE2)
           and then Is_Subtype_For_Aliasing (TE1, Full_Etype (TE2))));
   --  Return True iff TE1 is TE2 or a subtype of it using the above
   --  definition of base type or using a parent relationship among
   --  tagged types.

   function Universal_Aliasing_Including_Bases
     (TE : Type_Kind_Id) return Boolean;
   --  Return True iff TE or any base type for aliasing has Universal_Aliasing
   --  set.

   function Get_TBAA_Type
     (TE : Void_Or_Type_Kind_Id; Kind : TBAA_Kind) return Metadata_T;
   function Get_TBAA_Type (GT : GL_Type; Kind : TBAA_Kind) return Metadata_T
     with Pre => Present (GT);
   --  Get a TBAA type entry for the specified type and kind

   function Create_TBAA_Type
     (GT     : GL_Type;
      Kind   : TBAA_Kind;
      Parent : Metadata_T := No_Metadata_T) return Metadata_T
     with Pre => Present (GT);
   function Create_TBAA_Type
     (TE     : Type_Kind_Id;
      Kind   : TBAA_Kind;
      Parent : Metadata_T := No_Metadata_T) return Metadata_T;

   function Create_TBAA_For_Elementary_Type
     (TE     : Elementary_Kind_Id;
      Kind   : TBAA_Kind;
      Parent : Metadata_T) return Metadata_T
     with Pre => Present (Parent);
   function Create_TBAA_For_Record_Type
     (TE     : Record_Kind_Id;
      Kind   : TBAA_Kind;
      Parent : Metadata_T) return Metadata_T
     with Pre => Present (Parent);
   --  Subprograms of above

   function TBAA_Data_For_Array_Type (TE : E_Array_Type_Id) return TBAA_Info_Id
     with Post => Present (TBAA_Data_For_Array_Type'Result);
   --  If not already created, make a TBAA_Info entity for TE, a base type

   function TBAA_Data_For_Array_Subtype
     (TE : Array_Kind_Id) return TBAA_Info_Id
     with Post => Present (TBAA_Data_For_Array_Subtype'Result);
   --  Likewise for either a base type or subtype

   function Create_TBAA_For_Array_Data
     (TE     : Array_Kind_Id;
      Kind   : TBAA_Kind;
      Parent : Metadata_T) return Metadata_T;
   --  If TE is a small, constrained array, create metadata for it

   function Get_TBAA_Name
     (Kind   : TBAA_Kind;
      TE     : Opt_Void_Or_Type_Kind_Id := Empty;
      GT     : GL_Type                  := No_GL_Type;
      Suffix : String                   := "") return String;
   --  Return the name to use for a TBAA type entry for GT and TE, if present

   Name_Idx      : Nat := 0;
   --  The index used to create a unique name for the above function

   function Extract_Access_Type
     (MD                : Metadata_T;
      Offset            : in out ULL;
      Our_Size_In_Bytes : ULL) return Metadata_T
     with Pre  => Present (MD) and then Our_Size_In_Bytes /= 0
                  and then (Offset = 0 or else Is_Struct_Tag (MD))
                  and then (Is_Struct_Tag (MD)
                              or else Our_Size_In_Bytes = Size_In_Bytes (MD)),
          Post => Present (Extract_Access_Type'Result)
                  and then Offset'Old >= Offset;
   --  MD is a type tag and we are doing an access of Size_In_Bytes wide
   --  from it at Offset bytes from it. Return the corresponding access
   --  tag and new offset. If MD is not a struct tag or if we're accessing
   --  the entire structure, we keep Offset unchanged and return MD.

   function Get_Field_TBAA
     (F : Record_Field_Kind_Id; GT : GL_Type) return Metadata_T;
   --  Get (and maybe create) a TBAA tag for field F of type GT

   -----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if Opt.No_Strict_Aliasing or else Decls_Only then
         No_Strict_Aliasing_Flag := True;
      end if;

      TBAA_Root := Create_TBAA_Root;

      if not No_Strict_Aliasing_Flag then
         Search_For_UCs;
      end if;
   end Initialize;

   ----------------------------
   -- Base_Type_For_Aliasing --
   ----------------------------

   function Base_Type_For_Aliasing (TE : Type_Kind_Id) return Type_Kind_Id is
   begin
      --  If this isn't a base type, start with that

      if not Is_Base_Type (TE) then
         return Base_Type_For_Aliasing (Full_Base_Type (TE));

      --  If this is a composite derived type with the same representation
      --  as its parent, use the parent. But don't do this for tagged types
      --  since we can track those using a parent relationship.

      elsif Is_Composite_Type (TE) and then Is_Derived_Type (TE)
        and then Has_Compatible_Representation (TE, Full_Etype (TE))
        and then not Is_Tagged_Type (TE)
      then
         return Base_Type_For_Aliasing (Full_Etype (TE));

      --  Otherwise, this is the type to use

      else
         return TE;
      end if;

   end Base_Type_For_Aliasing;

   ----------------------------------------
   -- Universal_Aliasing_Including_Bases --
   ----------------------------------------

   function Universal_Aliasing_Including_Bases
     (TE : Type_Kind_Id) return Boolean
   is
      BT : constant Type_Kind_Id := Base_Type_For_Aliasing (TE);

   begin
      return Universal_Aliasing (TE)
        or else (BT /= TE and then Universal_Aliasing_Including_Bases (BT));
   end Universal_Aliasing_Including_Bases;

   --------------------
   -- Search_For_UCs --
   --------------------

   procedure Search_For_UCs is

      procedure Add_To_UC
        (STE, TTE : Type_Kind_Id; Valid : in out Boolean);
      --  Make an entry in the UC table showing that STE and TTE are to
      --  be treated identically. If we can't do that, clear Valid.

      function Check_For_UC (N : Node_Id) return Traverse_Result;

      procedure Scan is new Traverse_Proc (Check_For_UC);

      procedure Scan_Unit is new Scan_Library_Item (Scan => Scan);
      --  Scan one library item looking for UCs between access types

      procedure Scan_All_Units is
         new Sem.Walk_Library_Items (Action => Scan_Unit);

      ---------------
      -- Add_To_UC --
      ---------------

      procedure Add_To_UC
        (STE, TTE : Type_Kind_Id; Valid : in out Boolean)
      is
         SBT       : constant Type_Kind_Id := Base_Type_For_Aliasing (STE);
         TBT       : constant Type_Kind_Id := Base_Type_For_Aliasing (TTE);
         S_Is_Base : constant Boolean      := SBT = STE;
         T_Is_Base : constant Boolean      := TBT = TTE;
         S_Grp     : constant UC_Group_Idx := Find_UC_Group (STE);
         T_Grp     : constant UC_Group_Idx := Find_UC_Group (TTE);
         Our_Valid : Boolean               :=
           Valid and then Known_Esize (STE) and then Known_Esize (TTE)
           and then Esize (STE) = Esize (TTE)
           and then not Is_Aggregate_Type (STE)
           and then not Is_Aggregate_Type (TTE) and then S_Is_Base = T_Is_Base;

      begin
         --  If neither of these are base types for aliasing purposes, make
         --  an entry for the base types. This will produce entries all the
         --  way up the base chain to make those types equivalent. If any
         --  aren't valid, then the below call will make the subtypes invalid.

         if not S_Is_Base and then not T_Is_Base then
            Add_To_UC (SBT, TBT, Our_Valid);
         end if;

         --  If neither was seen before, allocate a new group and put them
         --  both in it.

         if No (S_Grp) and then No (T_Grp) then
            Last_UC_Group := Last_UC_Group + 1;
            UC.Append ((STE, Last_UC_Group, Our_Valid));
            UC.Append ((TTE, Last_UC_Group, Our_Valid));

         --  If one has a group and the other doesn't, add the other pointing
         --  to that group.

         elsif No (S_Grp) and then Present (T_Grp) then
            UC.Append ((STE, T_Grp, Our_Valid));
         elsif Present (S_Grp) and then No (T_Grp) then
            UC.Append ((TTE, S_Grp, Our_Valid));

         --  If both were assigned groups, move everything in the target's
         --  group to the group of the source.

         else
            for J in 1 .. UC.Last loop
               if UC.Table (J).Group = T_Grp then
                  UC.Table (J).Group := S_Grp;
                  UC.Table (J).Valid :=
                    UC.Table (J).Valid and Our_Valid;

                  if not UC.Table (J).Valid then
                     Our_Valid := False;
                  end if;
               end if;
            end loop;
         end if;

         --  Update our input validity flag to correspond with what we found

         Valid := Valid and Our_Valid;
      end Add_To_UC;

      ------------------
      -- Check_For_UC --
      ------------------

      function Check_For_UC (N : Node_Id) return Traverse_Result is
         STE : Type_Kind_Id;
         TTE : Type_Kind_Id;

         function OK_Unit
           (N  : N_Validate_Unchecked_Conversion_Id;
            TE : Type_Kind_Id) return Boolean
         is
           (In_Same_Extended_Unit (N, TE)
              or else In_Extended_Main_Code_Unit (TE));
         --  We can do something with TE if it's either in the code unit
         --  that we're compiling or in the same (extended) unit as the UC.

         function Is_Data_Access_Type (TE : Type_Kind_Id) return Boolean is
           (Is_Access_Type (TE)
              and then Ekind (TE) /= E_Access_Subprogram_Type);
         --  We don't want to treat access to subprogram as an access type
         --  since we never load a subprogram as data.

      begin
         --  If we run into a stub, we have to search inside it because
         --  Library_Unit is a semantic, not syntactic, field.

         if Nkind (N) in N_Body_Stub and then Present (Library_Unit (N)) then
            Scan (Library_Unit (N));
            return OK;

         --  Ignore if this is generic

         elsif Is_Generic_Item (N) then
            return Skip;

         --  All we care about are N_Validate_Unchecked_Conversion nodes
         --  between access types. If the target type has
         --  No_Strict_Aliasing set, we're taking care of this another way,
         --  so we're OK here.

         elsif Nkind (N) /= N_Validate_Unchecked_Conversion then
            return OK;
         end if;

         STE := Get_Fullest_View (Source_Type (N));
         TTE := Get_Fullest_View (Target_Type (N));

         if not Is_Data_Access_Type (STE)
           or else not Is_Data_Access_Type (TTE)
           or else No_Strict_Aliasing (TTE)
         then
            return OK;
         end if;

         --  There's a potential issue with these types, so we have to check
         --  further and possibly take some action.

         declare
            SDT   : constant Type_Kind_Id := Full_Designated_Type (STE);
            TDT   : constant Type_Kind_Id := Full_Designated_Type (TTE);
            Valid : Boolean               := True;

         begin
            --  If the the target is either the same or a subtype of the
            --  source, we have nothing to do. This can happen when we
            --  have two access types to the same underlying type or in
            --  some subtype cases. Likewise if the target has been marked
            --  for universal aliasing.

            if Is_Subtype_For_Aliasing (TDT, SDT)
              or else Universal_Aliasing_Including_Bases (TDT)
            then
               return OK;

            --  The most efficient way of dealing with this if the
            --  target is an access type is to set No_Strict_Aliasing on
            --  that type because that will only affect references to the
            --  designated type via that access type. The front end has
            --  already done that for us in the cases where it can be done
            --  and we've checked for that above.
            --
            --  The next best option is to make a table entry. However, we
            --  can't do that if this UC is in a body and one of the types
            --  isn't in the same compilation unit.

            elsif Nkind (Unit (Enclosing_Comp_Unit_Node (N)))
                    in N_Package_Body | N_Subprogram_Body
              and then (not OK_Unit (N, SDT) or else not OK_Unit (N, TDT))
            then
               --  If the target type is in the same unit (meaning that the
               --  source type isn't), we can still make this work by
               --  setting Universal_Aliasing on the target if it's not
               --  an access type.

               if OK_Unit (N, TDT) and then not Is_Data_Access_Type (TTE) then
                  Set_Universal_Aliasing (TDT, True);
                  return OK;

               --  Otherwise, issue a warning that there may be an issue.
               --  But if we aren't optimizing, there's actually no problem.

               elsif Code_Gen_Level /= Code_Gen_Level_None
                 and then not Is_Unconstrained_Array (TDT)
               then
                  Error_Msg_NE
                    ("??possible aliasing problem for type&", N, TTE);
                  Error_Msg_N
                    ("\\??use -fno-strict-aliasing switch for references", N);
                  Error_Msg_NE
                    ("\\??or use `pragma No_Strict_Aliasing (&);`", N, TTE);
               end if;
            end if;

            Add_To_UC (SDT, TDT, Valid);
         end;

         return OK;
      end Check_For_UC;

   begin
      --  Start of processing for Search_For_UCs

      Scan_All_Units;
   end Search_For_UCs;

   -------------------
   -- Find_UC_Group --
   -------------------

   function Find_UC_Group (TE : Type_Kind_Id) return UC_Group_Idx is
   begin
      for J in 1 .. UC.Last loop
         if UC.Table (J).TE = TE then
            return UC.Table (J).Group;
         end if;
      end loop;

      return Empty_UC_Group_Idx;
   end Find_UC_Group;

   ---------------------
   -- Initialize_TBAA --
   ---------------------

   procedure Initialize_TBAA (V : in out GL_Value; Kind : TBAA_Kind := Native)
   is
      GT : constant GL_Type         := Related_Type (V);
      R  : constant GL_Relationship := Relationship (V);

   begin
      --  Start by indicating we know nothing

      Set_TBAA_Type   (V, No_Metadata_T);
      Set_TBAA_Offset (V, 0);

      --  If this isn't a reference, we don't have any TBAA data.

      if not Is_Reference (V) then
         return;
      end if;

      --  If it's a double reference, this will be a unique scalar node

      if Is_Double_Reference (V) then
         Set_TBAA_Type (V, Create_TBAA_Scalar_Type_Node
                          (Get_TBAA_Name (Unique, GT => GT, Suffix => "#DR"),
                           To_Bytes (Get_Type_Size (Type_Of (V))), TBAA_Root));

      --  If this is a Reference, we set it to GT's TBAA type tag

      elsif R = Reference then
         Set_TBAA_Type (V, Get_TBAA_Type (GT, Kind));

      --  If this is an array type and we may have a reference to
      --  various parts of the array.

      elsif Is_Array_Type (GT) then
         declare
            TE   : constant Type_Kind_Id := Full_Etype (GT);
            Tidx : constant TBAA_Info_Id := TBAA_Data_For_Array_Subtype (TE);
            TI   : constant TBAA_Data    := TBAA_Info.Table (Tidx);

         begin
            if R = Reference_To_Bounds then
               Set_TBAA_Type (V, TI.Bounds);
            elsif R = Reference_To_Bounds_And_Data then
               Set_TBAA_Type (V, TI.Bounds_And_Data);
            end if;
         end;
      end if;

   end Initialize_TBAA;

   ---------------------
   -- Initialize_TBAA --
   ---------------------

   function Initialize_TBAA
     (V : GL_Value; Kind : TBAA_Kind := Native) return GL_Value
   is
      New_V : GL_Value := V;
   begin
      Initialize_TBAA (New_V, Kind);
      return New_V;
   end Initialize_TBAA;

   --------------------------------
   -- Initialize_TBAA_If_Changed --
   --------------------------------

   procedure Initialize_TBAA_If_Changed
     (V : in out GL_Value; Old_V : GL_Value) is
   begin
      if Related_Type (V) /= Related_Type (Old_V)
        or else Relationship (V) /= Relationship (Old_V)
      then
         Initialize_TBAA (V);
      end if;
   end Initialize_TBAA_If_Changed;

   --------------------------------------
   --  Maybe_Initialize_TBAA_For_Field --
   --------------------------------------

   procedure Maybe_Initialize_TBAA_For_Field
     (V : in out GL_Value; F : Record_Field_Kind_Id; F_GT : GL_Type) is
   begin
      if No (TBAA_Type (V)) and then not Is_Bitfield (F) then
         Set_TBAA_Type   (V, Get_Field_TBAA (F, F_GT));
         Set_TBAA_Offset (V, 0);
      end if;
   end Maybe_Initialize_TBAA_For_Field;

   ------------------------------------------------
   --  Maybe_Initialize_TBAA_For_Array_Component --
   ------------------------------------------------

   procedure Maybe_Initialize_TBAA_For_Array_Component
     (V : in out GL_Value; GT : GL_Type)
   is
      Tidx   : constant TBAA_Info_Id :=
        Get_TBAA_Info (Base_Type_For_Aliasing (Full_Etype (GT)));
      C_TBAA : constant Metadata_T   :=
        (if   Present (Tidx) then TBAA_Info.Table (Tidx).Component
         else No_Metadata_T);

   begin
      if No (TBAA_Type (V)) and then Present (C_TBAA) then
         Set_TBAA_Type   (V, C_TBAA);
         Set_TBAA_Offset (V, 0);
      end if;
   end Maybe_Initialize_TBAA_For_Array_Component;

   -----------------
   -- Common_TBAA --
   -----------------

   function Common_TBAA (M1, M2 : Metadata_T) return Metadata_T is
   begin
      if No (M1) or else No (M2) then
         return No_Metadata_T;
      elsif M1 = M2 then
         return M1;

      --  We only want to go up to a parent if the size doesn't change.
      --  Otherwise, we'll mess up access tags.

      elsif TBAA_Parent (M1) /= TBAA_Root
        and then Size_In_Bytes (M1) = Size_In_Bytes (TBAA_Parent (M1))
      then
         return Common_TBAA (TBAA_Parent (M1), M2);
      elsif TBAA_Parent (M2) /= TBAA_Root
        and then Size_In_Bytes (M2) = Size_In_Bytes (TBAA_Parent (M2))
      then
         return Common_TBAA (M1, TBAA_Parent (M2));
      else
         return No_Metadata_T;
      end if;
   end Common_TBAA;

   ------------------------
   -- Find_Equiv_Subtype --
   ------------------------

   function Find_Equiv_Subtype (TE : Type_Kind_Id) return Type_Kind_Id is
      procedure Get_String_Bounds (TE : Array_Kind_Id; LB, HB : out Uint);
      --  Get the bounds of TE, which is known to be a subtype of String.
      --  Handle both string literal and normal array case. If a bound
      --  isn't constant (which isn't the case for a string literal), set
      --  that value to No_Uint.

      BT   : constant Type_Kind_Id := Base_Type_For_Aliasing (TE);
      Tidx : constant TBAA_Info_Id := Get_TBAA_Info (BT);
      Idx  : Nat;
      E_TE : Type_Kind_Id;

      -----------------------
      -- Get_String_Bounds --
      -----------------------

      procedure Get_String_Bounds (TE : Array_Kind_Id; LB, HB : out Uint) is
      begin
         if Ekind (TE) = E_String_Literal_Subtype then
            declare
               First      : constant Uint :=
                 Get_Uint_Value (String_Literal_Low_Bound (TE));
               Length     : constant Uint := String_Literal_Length (TE);

            begin
               LB := First;
               HB := First + Length - 1;
            end;
         else
            declare
               Index     : constant N_Is_Index_Id   := First_Index (TE);
               Idx_Range : constant N_Has_Bounds_Id := Simplify_Range (Index);
               R_LB      : constant N_Subexpr_Id    := Low_Bound (Idx_Range);
               R_HB      : constant N_Subexpr_Id    := High_Bound (Idx_Range);

            begin
               LB := (if   Compile_Time_Known_Value (R_LB)
                      then Expr_Value (R_LB) else No_Uint);
               HB := (if   Compile_Time_Known_Value (R_HB)
                        then Expr_Value (R_HB) else No_Uint);
            end;
         end if;
      end Get_String_Bounds;

   begin
      --  If we're a base type, not an aggregate, or nonnative, return
      --  ourselves. Likewise if no subtypes are chained yet.

      if Is_Full_Base_Type (TE) or else Is_Elementary_Type (TE)
        or else Is_Nonnative_Type (TE) or else No (Tidx)
      then
         return TE;
      end if;

      --  Now look through all the subtypes which we chained and see if any
      --  are equivalent to our type.

      Idx := TBAA_Info.Table (Tidx).Subtype_Chain;
      while Idx /= 0 loop
         E_TE := TBAA_Equiv_Subtype.Table (Idx).TE;
         Idx  := TBAA_Equiv_Subtype.Table (Idx).Next;

         --  Only if the LLVM types and GNAT representations are the same
         --  if the a chance that they can be equivalent.

         if Is_Layout_Identical (Type_Of (TE), Type_Of (E_TE))
           and then Has_Compatible_Representation (TE, E_TE)
         then
            --  We have to check differently for arrays and records. For
            --  arrays, we need to have identical bounds.

            if Is_Array_Type (TE) then

               --  If either is a string literal subtype, we know we're a
               --  subtype of String and one dimension. So get the bounds
               --  and compare.

               if Ekind (TE) = E_String_Literal_Subtype
                 or else Ekind (E_TE) = E_String_Literal_Subtype
               then
                  declare
                     LB, HB, E_LB, E_HB : Uint;

                  begin
                     Get_String_Bounds (TE,   LB,   HB);
                     Get_String_Bounds (E_TE, E_LB, E_HB);

                     if Present (LB) and then Present (HB)
                       and then Present (E_LB) and then Present (E_HB)
                       and then LB = E_LB and then HB = E_HB
                     then
                        return E_TE;
                     end if;
                  end;
               else
                  declare
                     Index   : Opt_N_Is_Index_Id := First_Index (TE);
                     E_Index : Opt_N_Is_Index_Id := First_Index (E_TE);
                     Matches : Boolean           := True;

                  begin
                     while Present (Index) loop
                        declare
                           Rng   : constant N_Has_Bounds_Id :=
                             Simplify_Range (Index);
                           E_Rng : constant N_Has_Bounds_Id :=
                             Simplify_Range (E_Index);
                           LB    : constant N_Subexpr_Id    :=
                             Low_Bound (Rng);
                           HB    : constant N_Subexpr_Id    :=
                             High_Bound (Rng);
                           E_LB  : constant N_Subexpr_Id    :=
                             Low_Bound (E_Rng);
                           E_HB  : constant N_Subexpr_Id    :=
                             High_Bound (E_Rng);

                        begin
                           Matches := Matches
                             and then Compile_Time_Known_Value (LB)
                             and then Compile_Time_Known_Value (HB)
                             and then Compile_Time_Known_Value (E_LB)
                             and then Compile_Time_Known_Value (E_HB)
                             and then Expr_Value (LB) = Expr_Value (E_LB)
                             and then Expr_Value (HB) = Expr_Value (E_HB);
                        end;

                        Next_Index (Index);
                        Next_Index (E_Index);
                     end loop;

                     --  If all matches, we have an equivalent subtype

                     if Matches then
                        return E_TE;
                     end if;
                  end;
               end if;

            --  For records, all discriminant contraints, if any, must match

            elsif Is_Record_Type (TE) then

               --  We only have the potential of a match either if both
               --  have constraints or neither do. But if neither, we know
               --  we have a match.

               declare
                  Constraint   : constant Elist_Id := Stored_Constraint (TE);
                  E_Constraint : constant Elist_Id := Stored_Constraint (E_TE);
                  Matches      : Boolean           := True;
                  Elmt         : Elmt_Id;
                  E_Elmt       : Elmt_Id;

               begin
                  if Present (Constraint) = Present (E_Constraint) then
                     if No (Constraint) then
                        return E_TE;
                     end if;

                     Elmt   := First_Elmt (Constraint);
                     E_Elmt := First_Elmt (E_Constraint);
                     while Present (Elmt) loop
                        Matches := Matches
                          and then Compile_Time_Known_Value (Node (Elmt))
                          and then Compile_Time_Known_Value (Node (E_Elmt))
                          and then Expr_Value (Node (Elmt)) =
                                     Expr_Value (Node (E_Elmt));
                        Next_Elmt (Elmt);
                        Next_Elmt (E_Elmt);
                     end loop;

                     --  If all matches, we have an equivalent subtype

                     if Matches then
                        return E_TE;
                     end if;
                  end if;
               end;
            end if;
         end if;
      end loop;

      --  If we didn't have an equivalent, return our input

      return TE;
   end Find_Equiv_Subtype;

   --------------------
   -- Get_Field_TBAA --
   --------------------

   function Get_Field_TBAA
     (F : Record_Field_Kind_Id; GT : GL_Type) return Metadata_T
   is
      Fidx   : constant Field_Info_Id            := Get_Field_Info (F);
      Kind   : constant TBAA_Kind                :=
        Kind_From_Aliased (Is_Aliased (F));
      TBAA   : Metadata_T                        := TBAA_Type (Fidx);
      PF     : constant Opt_Record_Field_Kind_Id := Parent_Field (F);
      Parent : Metadata_T             := No_Metadata_T;

   begin
      if No (TBAA) then

         --  If we have a parent field, its TBAA type tag, if any, is our
         --  parent and we make a new TBAA type tag for our type. If not,
         --  we get a new TBAA type tag for GT.

         if Present (PF)
           and then not (Ekind (PF) = E_Discriminant
                           and then Is_Completely_Hidden (PF))
         then
            Parent := Get_Field_TBAA (PF, Field_Type (PF));
         end if;

         if Present (Parent) then
            TBAA := Create_TBAA_Type (GT, Kind, Parent);
         else
            TBAA := Get_TBAA_Type (GT, Kind);
         end if;

         Set_TBAA_Type (Fidx, TBAA);
      end if;

      return TBAA;
   end Get_Field_TBAA;

   -------------------
   -- Get_TBAA_Name --
   -------------------

   function Get_TBAA_Name
     (Kind   : TBAA_Kind;
      TE     : Opt_Void_Or_Type_Kind_Id := Empty;
      GT     : GL_Type                  := No_GL_Type;
      Suffix : String                   := "") return String
   is
      Our_TE : constant Type_Kind_Id :=
        (if Present (TE) then TE else Full_Etype (GT));
      Buf : Bounded_String;

   begin
      if Present (Our_TE) then
         Append (Buf, Chars (Our_TE));

         --  If this is an integer type in Standard, its first subtype will
         --  have the same name, so distinguish.

         if Ekind (Our_TE) = E_Signed_Integer_Type
           and then Sloc (Our_TE) = Standard_Location
         then
            Append (Buf, "B");
         end if;

         if Present (GT) and then not Is_Primitive_GL_Type (GT) then
            Append (Buf, "#GL");
            Append (Buf, Int (GT - GL_Type_Low_Bound));
         end if;
      else
         Append (Buf, "TBAA");
      end if;

      case Kind is
         when Native =>
            Append (Buf, "#TN");
         when For_Aliased =>
            null;
         when Unique | Unique_Aliased =>
            Append (Buf, "#T");
            Append (Buf, Name_Idx);
            Name_Idx := Name_Idx + 1;
      end case;

      Append (Buf, Suffix);
      return +Buf;
   end Get_TBAA_Name;

   -------------------
   -- Get_TBAA_Type --
   -------------------

   function Get_TBAA_Type
     (GT : GL_Type; Kind : TBAA_Kind) return Metadata_T
   is
      TBAA        : Metadata_T := TBAA_Type (GT);
      Native_TBAA : Metadata_T;

   begin
      --  If we haven't saved a TBAA type tag, we first need to make native
      --  and aliased TBAA type tags and save the latter.

      if Present (TBAA) then
         Native_TBAA := TBAA_Parent (TBAA);
      else
         Native_TBAA := Create_TBAA_Type (GT, Native);

         if No (Native_TBAA) then
            return No_Metadata_T;
         end if;

         TBAA := Create_TBAA_Type (GT, For_Aliased, Parent => Native_TBAA);
         Set_TBAA_Type (GT, TBAA);
      end if;

      --  Finally, return the proper TBAA type tag for our usage

      case Kind is
         when Native =>
            return Native_TBAA;
         when For_Aliased =>
            return TBAA;
         when Unique =>
            return Create_TBAA_Type (GT, Unique, Parent => Native_TBAA);
         when Unique_Aliased =>
            return Create_TBAA_Type (GT, Unique_Aliased, Parent => TBAA);
      end case;
   end Get_TBAA_Type;

   -------------------
   -- Get_TBAA_Type --
   -------------------

   function Get_TBAA_Type
     (TE : Void_Or_Type_Kind_Id; Kind : TBAA_Kind) return Metadata_T
   is
      Grp         : constant UC_Group_Idx          := Find_UC_Group (TE);
      E_TE        : constant Void_Or_Type_Kind_Id  := Find_Equiv_Subtype (TE);
      TBAA        : Metadata_T                     := Get_TBAA (E_TE);
      Native_TBAA : Metadata_T;

   begin
      --  If we have -fno-strict-aliasing, this type isn't to use
      --  type-based aliasing, or this is a void type, don't create a
      --  TBAA tag.

      if No_Strict_Aliasing_Flag or else Ekind (E_TE) = E_Void
        or else Universal_Aliasing_Including_Bases (TE)
      then
         return No_Metadata_T;

      --  If we haven't already saved TBAA data and this type is in a
      --  group related by UC's between access types, use any TBAA
      --  we've already made for a type in that group.

      elsif No (TBAA) and then Present (Grp) then
         for J in 1 .. UC.Last loop
            declare
               UCE : constant UC_Entry := UC.Table (J);

            begin
               if UCE.Group = Grp and then Present (Get_TBAA_N (UCE.TE))
                 and then No (TBAA)
               then
                  TBAA := Get_TBAA_N (UCE.TE);
               end if;

               --  Check for something invalidating the group, such as an
               --  aggregate type, because can't use the same type tag
               --  because looking into the structures will fail. We have
               --  no choice here but to not return a type tag in this
               --  case.

               if UCE.Group = Grp and then not UCE.Valid then
                  return No_Metadata_T;
               end if;
            end;
         end loop;
      end if;

      --  If we haven't saved a TBAA type tag, we first need to make native
      --  and aliased TBAA type tags and save the latter.

      if Present (TBAA) then
         Native_TBAA := TBAA_Parent (TBAA);
      else
         Native_TBAA := Create_TBAA_Type (TE, Native);

         if No (Native_TBAA) then
            return No_Metadata_T;
         end if;

         TBAA := Create_TBAA_Type (E_TE, For_Aliased, Parent => Native_TBAA);
         Set_TBAA (E_TE, TBAA);

         --  If TE is a subtype of an aggregate, add it to the chain of
         --  subtypes for which we made a TBAA type tag.

         if Is_Aggregate_Type (E_TE) and then not Is_Full_Base_Type (E_TE) then
            declare
               BT  : constant Type_Kind_Id := Base_Type_For_Aliasing (E_TE);
               Idx : constant TBAA_Info_Id := Get_TBAA_Info (BT);

            begin
               --  If we already have a TBAA_Info entry for the base type,
               --  chain us to that entry and update it to refer to us.
               --  Otherwise, make a new entry indicating the end of list
               --  and make a new entry that points to it.

               if Present (Idx) then
                  declare
                     TI : TBAA_Data renames TBAA_Info.Table (Idx);

                  begin
                     TBAA_Equiv_Subtype.Append ((TI.Subtype_Chain, E_TE));
                     TI.Subtype_Chain := TBAA_Equiv_Subtype.Last;
                  end;
               else
                  TBAA_Equiv_Subtype.Append ((0, E_TE));
                  TBAA_Info.Append ((Subtype_Chain => TBAA_Equiv_Subtype.Last,
                                     others => No_Metadata_T));
                  Set_TBAA_Info (BT, TBAA_Info.Last);
               end if;
            end;
         end if;
      end if;

      --  Finally, return the proper TBAA type tag for our usage

      case Kind is
         when Native =>
            return Native_TBAA;
         when For_Aliased =>
            return TBAA;
         when Unique =>
            return Create_TBAA_Type (E_TE, Unique, Parent => Native_TBAA);
         when Unique_Aliased =>
            return Create_TBAA_Type (E_TE, Unique_Aliased, Parent => TBAA);
      end case;

   end Get_TBAA_Type;

   ----------------------
   -- Create_TBAA_Type --
   ----------------------

   function Create_TBAA_Type
     (GT     : GL_Type;
      Kind   : TBAA_Kind;
      Parent : Metadata_T := No_Metadata_T) return Metadata_T
   is
      TE         : constant Void_Or_Type_Kind_Id  := Full_Etype (GT);
      TBAA       : constant Metadata_T            :=
        Get_TBAA_Type (TE, For_Aliased);
      Our_Parent : constant Metadata_T            :=
        (if Present (Parent) then Parent else TBAA_Root);
      Prim_GT    : constant GL_Type               := Primitive_GL_Type (GT);
      Prim_TBAA  : constant Metadata_T            :=
        (if   No (TBAA) then No_Metadata_T
         else (case Kind is
                 when For_Aliased             => TBAA,
                 when Native                  => TBAA_Parent (TBAA),
                 when Unique | Unique_Aliased =>
                    Create_TBAA_Type (TE, Kind, Our_Parent)));

   begin
      --  If we couldn't get a type tag for our base type, or if this is a
      --  byte array or truncated type, we can't get one for this GT.

      if No (TBAA) or else Is_Byte_Array_GL_Type (GT)
        or else Is_Truncated_GL_Type (GT)
      then
         return No_Metadata_T;

      --  Otherwise, if this is a primitive type, return its type tag

      elsif Is_Primitive_GL_Type (GT) then
         return Prim_TBAA;

      --  If this is a padded type, make a struct type with the primitive
      --  tag as the only field since we don't care about padding. But we
      --  can't do this for subtypes of aggregates since two identical
      --  GL_Types for equivalent types must have the same TBAA type tag
      --  and it's not at all worth making that happen.

      elsif Is_Padded_GL_Type (GT)
        and then (Is_Elementary_Type (TE) or else Full_Base_Type (TE) = TE)
      then
         declare
            TBAAs   : constant Metadata_Array (1 .. 1) := (1 => Prim_TBAA);
            Sizes   : constant ULL_Array (1 .. 1)      :=
              (1 => To_Bytes (Get_Type_Size (Type_Of (Prim_GT))));
            Offsets : constant ULL_Array (1 .. 1)      := (1 => 0);

         begin
            return Create_TBAA_Struct_Type_Node
              (Get_TBAA_Name (Kind, GT => GT, TE => TE),
               +To_Bytes (GT_Size (GT)), Our_Parent, Offsets, Sizes, TBAAs);
         end;

      --  If this is an alternate integer representation (including biased),
      --  make a new type tag for this as a scalar type.

      elsif Is_Int_Alt_GL_Type (GT) or else Is_Biased_GL_Type (GT) then
         return Create_TBAA_Scalar_Type_Node
           (Get_TBAA_Name (Kind, GT => GT, TE => TE),
            To_Bytes (Get_Type_Size (Type_Of (GT))), Our_Parent);

      --  We don't support any other cases for now

      else
         return No_Metadata_T;
      end if;
   end Create_TBAA_Type;

   ----------------------
   -- Create_TBAA_Type --
   ----------------------

   function Create_TBAA_Type
     (TE     : Type_Kind_Id;
      Kind   : TBAA_Kind;
      Parent : Metadata_T := No_Metadata_T) return Metadata_T
   is
      BT           : constant Type_Kind_Id  := Base_Type_For_Aliasing (TE);
      Inner_Parent : constant Metadata_T    :=
        (if    Present (Parent) then Parent
         elsif BT /= TE then Get_TBAA_Type (BT, Native)
         elsif Is_Tagged_Type (TE) and then Is_Derived_Type (TE)
         then  Get_TBAA_Type (Full_Etype (TE), Native) else TBAA_Root);
      Our_Parent   : constant Metadata_T    :=
        (if Present (Inner_Parent) then Inner_Parent else TBAA_Root);

   begin
      --  If this isn't a native type, we can't make a TBAA type entry for it

      if Is_Nonnative_Type (TE) then
         return No_Metadata_T;

      --  All other types are done by subprograms, if supported

      elsif Is_Elementary_Type (TE) then
         return Create_TBAA_For_Elementary_Type (TE, Kind, Our_Parent);

      elsif Is_Record_Type (TE) then
         return Create_TBAA_For_Record_Type (TE, Kind, Our_Parent);

      elsif Is_Array_Type (TE) then
         return Create_TBAA_For_Array_Data (TE, Kind, Our_Parent);

      --  Otherwise, we can't make a type entry for it

      else
         return No_Metadata_T;
      end if;

   end Create_TBAA_Type;

   -------------------------------------
   -- Create_TBAA_For_Elementary_Type --
   -------------------------------------

   function Create_TBAA_For_Elementary_Type
     (TE     : Elementary_Kind_Id;
      Kind   : TBAA_Kind;
      Parent : Metadata_T) return Metadata_T
   is
      GT   : constant GL_Type  := Primitive_GL_Type (TE);
      Size : constant ULL      := Get_Type_Size (Type_Of (GT));

   begin
      --  ??? We don't make a TBAA type tag for access subprogram types.
      --  We treat fat pointers as scalars since we never address into them.

      if Is_Access_Subprogram_Type (TE) then
         return No_Metadata_T;
      else
         return Create_TBAA_Scalar_Type_Node
           (Get_TBAA_Name (Kind, TE => TE), To_Bytes (Size), Parent);
      end if;

   end Create_TBAA_For_Elementary_Type;

   ---------------------------------
   -- Create_TBAA_For_Record_Type --
   ---------------------------------

   function Create_TBAA_For_Record_Type
     (TE     : Record_Kind_Id;
      Kind   : TBAA_Kind;
      Parent : Metadata_T) return Metadata_T
   is
      Ridx          : constant Record_Info_Id     := Get_Record_Info (TE);
      Struct_Fields : constant Struct_Field_Array :=
        RI_To_Struct_Field_Array (Ridx);
      Offsets       : ULL_Array (Struct_Fields'Range);
      Sizes         : ULL_Array (Struct_Fields'Range);
      TBAAs         : Metadata_Array (Struct_Fields'Range);

   begin
      --  If we have no data, this is an empty structure, so we can't have
      --  an access into it.

      if Struct_Fields'Length = 0 then
         return No_Metadata_T;
      end if;

      --  Otherwise fill in the three arrays above. If we can't get a TBAA
      --  entry for a field, we can't make a TBAA type for the struct.

      for J in Struct_Fields'Range loop
         declare
            SF : constant Struct_Field := Struct_Fields (J);

         begin
            Offsets (J) := SF.Offset;
            Sizes   (J) := To_Bytes (Get_Type_Size (SF.T));

            --  If there's no GT for the field, this is a field used to
            --  store bitfields. So we make a unique scalar TBAA type
            --  entry for it.

            if No (SF.GT) then
               TBAAs (J) :=
                 Create_TBAA_Scalar_Type_Node ("BF", Sizes (J), TBAA_Root);

               --  Otherwise, try to get or make a type entry

            else
               TBAAs (J) := Get_Field_TBAA (SF.Field, SF.GT);
            end if;

            --  If we found an entry, store it. Otherwise, we fail.

            if No (TBAAs (J)) then
               return No_Metadata_T;
            end if;
         end;
      end loop;

      return Create_TBAA_Struct_Type_Node
        (Get_TBAA_Name (Kind, TE => TE),
         +To_Bytes (Get_Type_Size (Primitive_GL_Type (TE))), Parent, Offsets,
         Sizes, TBAAs);

   end Create_TBAA_For_Record_Type;

   ------------------------------
   -- TBAA_Data_For_Array_Type --
   ------------------------------

   function TBAA_Data_For_Array_Type (TE : E_Array_Type_Id) return TBAA_Info_Id
   is
      A_TE    : constant Array_Kind_Id   := Get_Fullest_View (TE);
      A_GT    : constant GL_Type         := Default_GL_Type (A_TE);
      Comp_GT : constant GL_Type         := Full_Component_GL_Type (A_TE);
      Tidx    : TBAA_Info_Id             := Get_TBAA_Info (A_TE);
      TI      : TBAA_Data                :=
        (Subtype_Chain => 0, others => No_Metadata_T);

   begin
      --  If we already made one, return it

      if Present (Tidx) then
         return Tidx;
      end if;

      --  Start by setting the component type tag, if there is one

      TI.Component   :=
        Get_TBAA_Type (Comp_GT,
                       Kind_From_Aliased (Has_Aliased_Components (A_TE)));

      --  Now compute the TBAA struct tag for bounds. Since bounds can't
      --  be modified, use a non-aliased unique version of the bound type.

      declare
         Nbounds  : constant Nat    := Number_Bounds (A_TE);
         Ndims    : constant Nat    := Number_Dimensions (A_TE);
         Bound_T  : constant Type_T := Type_For_Relationship (A_GT, Bounds);
         Idx      : Nat             := 0;
         Offset   : ULL             := 0;
         Offsets  : ULL_Array (0 .. Nbounds - 1);
         Sizes    : ULL_Array (0 .. Nbounds - 1);
         TBAAs    : Metadata_Array (0 .. Nbounds - 1);

      begin
         --  If we have just one bound (a single-dimension array with a
         --  fixed lower bound, this is just the TBAA type of the bound.

         if Nbounds = 1 then
            TI.Bounds := Get_TBAA_Type (Array_Index_GT (A_TE, 0), Unique);
         else
            for Dim in 0 .. Ndims - 1 loop
               declare
                  Dim_GT : constant GL_Type := Array_Index_GT (A_TE, Dim);
                  Dim_T  : constant Type_T  := Type_Of (Dim_GT);
                  Size   : constant ULL     :=
                    To_Bytes (Get_Type_Size (Dim_T));
                  FLB    : constant Boolean := Array_Index_Has_FLB (A_TE, Dim);

               begin
                  if not FLB then
                     Offsets (Idx) := Offset;
                     Sizes   (Idx) := Size;
                     TBAAs   (Idx) := Get_TBAA_Type (Dim_GT, Unique);
                     Offset        := Offset + Size;
                     Idx           := Idx + 1;
                  end if;

                  Offsets (Idx) := Offset;
                  Sizes   (Idx) := Size;
                  TBAAs   (Idx) := Get_TBAA_Type (Dim_GT, Unique);
                  Offset        := Offset + Size;
                  Idx           := Idx + 1;
               end;
            end loop;

            --  If one of the index types has Universal_Aliasing, we won't
            --  have a TBAA type for it above. So we can't form a TBAA type
            --  for our bounds either.
            --  ??? Should we do something about this?

            if (for all T of TBAAs => Present (T)) then
               TI.Bounds :=
                 Create_TBAA_Struct_Type_Node
                 (Get_TBAA_Name (Unique, TE => TE, Suffix => "#BND"),
                  To_Bytes (Get_Type_Size (Bound_T)), TBAA_Root, Offsets,
                  Sizes, TBAAs);
            end if;
         end if;
      end;

      TBAA_Info.Append (TI);
      Tidx := TBAA_Info.Last;
      Set_TBAA_Info (TE, Tidx);
      return Tidx;
   end TBAA_Data_For_Array_Type;

   ---------------------------------
   -- TBAA_Data_For_Array_Subtype --
   ---------------------------------

   function TBAA_Data_For_Array_Subtype
     (TE : Array_Kind_Id) return TBAA_Info_Id
   is
      O_TE      : constant Array_Kind_Id   :=
        (if   Is_Packed_Array_Impl_Type (TE) then Original_Array_Type (TE)
         else TE);
      BT        : constant E_Array_Type_Id := Full_Base_Type (O_TE, True);
      BTidx     : constant TBAA_Info_Id    := TBAA_Data_For_Array_Type (BT);
      Tidx      : TBAA_Info_Id             := Get_TBAA_Info (TE);
      TI        : TBAA_Data                := TBAA_Info.Table (BTidx);
      TBAA_Data : constant Metadata_T      := Get_TBAA_Type (TE, For_Aliased);
      GT        : constant GL_Type         := Primitive_GL_Type (TE);

   begin
      --  If we already made one, return it

      if Present (Tidx) then
         return Tidx;
      end if;

      --  Most of the fields are the same as for the base type and were
      --  initialized above. However, if we have a type for the array data,
      --  we have a type for the array bounds + data.

      if Present (TBAA_Data) then
         declare
            Bound_Size  : constant ULL            := Size_In_Bytes (TI.Bounds);
            Data_Size   : constant ULL            := Size_In_Bytes (TBAA_Data);
            BD_T        : constant Type_T         :=
              Type_For_Relationship (GT, Bounds_And_Data);
            Size        : constant ULL            :=
              To_Bytes (Get_Type_Size (BD_T));
            Align       : constant ULL            :=
              ULL (To_Bytes (Get_Array_Type_Alignment (TE)));
            Align_BS    : constant ULL            :=
              (Bound_Size + (Align - 1)) / Align * Align;
            Offsets     : constant ULL_Array      := (1 => 0, 2 => Align_BS);
            Sizes       : constant ULL_Array      :=
              (1 => Bound_Size, 2 => Data_Size);
            TBAAs       : constant Metadata_Array :=
              (1 => TI.Bounds, 2 => TBAA_Data);

         begin
            TI.Bounds_And_Data := Create_TBAA_Struct_Type_Node
              (Get_TBAA_Name (For_Aliased, TE => TE, Suffix => "#BD"), Size,
               TBAA_Root, Offsets, Sizes, TBAAs);
         end;
      end if;

      --  Now add the new entry to the table and record its location

      TBAA_Info.Append (TI);
      Tidx := TBAA_Info.Last;
      Set_TBAA_Info (TE, Tidx);
      return Tidx;
   end TBAA_Data_For_Array_Subtype;

   --------------------------------
   -- Create_TBAA_For_Array_Data --
   --------------------------------

   function Create_TBAA_For_Array_Data
     (TE     : Array_Kind_Id;
      Kind   : TBAA_Kind;
      Parent : Metadata_T) return Metadata_T
   is
      C_GT   : constant GL_Type         := Full_Component_GL_Type (TE);
      C_Size : constant ULL             := +To_Bytes (Get_Type_Size (C_GT));
      GT     : constant GL_Type         := Primitive_GL_Type (TE);
      BT     : constant E_Array_Type_Id := Base_Type_For_Aliasing (TE);
      Tidx   : constant TBAA_Info_Id    := TBAA_Data_For_Array_Type (BT);
      C_TBAA : constant Metadata_T      := TBAA_Info.Table (Tidx).Component;

   begin
      --  If this isn't a loadable type, we don't need to handle this and
      --  we either might not be able to (if it's of variable size) or don't
      --  want to (if it has too many elements). We also can't do anything
      --  if there's no type tag for the component or if the component is
      --  of zero size.

      if not Is_Loadable_Type (GT) or else No (C_TBAA) or else C_Size = 0 then
         return No_Metadata_T;
      end if;

      --  Otherwise, we know that everything is of constant (and small) size,
      --  so set up to make a structure type tag for this array.

      declare
         Size      : constant ULL     := +To_Bytes (Get_Type_Size (GT));
         Elmts     : constant Nat     := Nat (Size / C_Size);
         Offset    : ULL              := 0;
         Offsets   : ULL_Array (1 .. Elmts);
         Sizes     : ULL_Array (1 .. Elmts);
         TBAAs     : Metadata_Array (1 .. Elmts);

      begin
         --  We'd like to use a unique TBAA tag for each array element if
         --  components aren't aliased, but we can't do that because we'd
         --  then have two different chains, one per-component and one
         --  per-object, and they won't conflict as they should. It's more
         --  important to detect that different objects don't conflict than
         --  different elements don't because it's usually easier to detect
         --  non-conflicting elements by seeing the different addresses.

         for J in 1 .. Elmts loop
            Offsets (J) := Offset;
            Sizes (J)   := C_Size;
            TBAAs (J)   := C_TBAA;
            Offset      := Offset + C_Size;
         end loop;

         return Create_TBAA_Struct_Type_Node
           (Get_TBAA_Name (Kind, TE => TE, Suffix => "#AD"), Size, Parent,
            Offsets, Sizes, TBAAs);
      end;
   end Create_TBAA_For_Array_Data;

   --------------------------
   --  Extract_Access_Type --
   --------------------------

   function Extract_Access_Type
     (MD                : Metadata_T;
      Offset            : in out ULL;
      Our_Size_In_Bytes : ULL) return Metadata_T is
   begin
      --  If we're accessing the entire object, we return it and we're done

      if Size_In_Bytes (MD) = Our_Size_In_Bytes then
         return MD;
      end if;

      --  Otherwise we know (from the preconditions) that MD is a struct
      --  tag. We find the last field whose offset is less than or equal
      --  to our offset and recurse in case we're into a nested struct.

      for J in 0 .. Last_Field_Index (MD) loop
         if J = Last_Field_Index (MD)
           or else Field_Offset (MD, J + 1) > Offset
         then
            Offset := Offset - Field_Offset (MD, J);
            return Extract_Access_Type (Field_Type (MD, J), Offset,
                                        Our_Size_In_Bytes);
         end if;
      end loop;

      --  We should never hit here because the above will always stop us
      --  at the last field, at worst

      return No_Metadata_T;
   end Extract_Access_Type;

   ---------------------------------
   -- Add_Aliasing_To_Instruction --
   ---------------------------------

   procedure Add_Aliasing_To_Instruction (Inst : Value_T; V : GL_Value) is
      GT            : constant GL_Type :=
        (if   Is_Data (V) then Full_Designated_GL_Type (V)
         else Related_Type (V));
      Size_In_Bytes : constant ULL     :=
        To_Bytes (Get_Type_Size (Element_Type_Of (V)));
      Offset        : ULL              := TBAA_Offset  (V);
      Base_Type     : Metadata_T       := TBAA_Type    (V);
      Access_Type   : Metadata_T;

   begin
      --  If this object is marked to alias everything, we don't add any
      --  anotations.

      if Aliases_All (V) then
         return;

      --  If the version of LLVM that we're linking with has a bug in the
      --  handling of structure tags overlapping with scalars, don't put
      --  an access tag on an instruction that references an aggregate type.

      elsif LLVM_Struct_Tag_Bug and then Is_Aggregate_Type (V) then
         return;

      --  If we couldn't track V's TBAA information, we can try to just use
      --  the TBAA information from the type if we have data.

      elsif No (Base_Type) and then Is_Data (V) then
         Base_Type   := Get_TBAA_Type (GT, Native);
         Offset      := 0;
      end if;

      --  If we still couldn't find a tag or if our size is zero, don't do
      --  anything.

      if Present (Base_Type) and then Size_In_Bytes /= 0 then
         Access_Type := Extract_Access_Type (Base_Type, Offset, Size_In_Bytes);
         Add_TBAA_Access
           (Inst, Create_TBAA_Access_Tag (Access_Type, Access_Type, Offset,
                                          Size_In_Bytes));
      end if;
   end Add_Aliasing_To_Instruction;

   -------------------------
   -- Compute_TBAA_Access --
   -------------------------

   function Compute_TBAA_Access (LHS, RHS, Size : GL_Value) return Metadata_T
   is
      TBAA        : Metadata_T;
      Orig_Offset : ULL;
      Offset      : ULL;
      Access_TBAA : Metadata_T;

   begin
      --  If size isn't a constant, we can't do anything or if either side
      --  aliases everything or if we can't use access tags for struct
      --  types due to an LLVM bug. Otherwise, see if we can find a tag on
      --  both sides that have a common tag if both sides are Present or
      --  use the one for LHS if only it's Present.

      if not Is_A_Constant_Int (Size) or else Aliases_All (LHS)
        or else Size = 0 or else LLVM_Struct_Tag_Bug
      then
         return No_Metadata_T;
      elsif No (RHS) then
         TBAA := TBAA_Type (LHS);
      elsif not Aliases_All (RHS) and then Present (TBAA_Type (RHS))
        and then TBAA_Offset (LHS) /= TBAA_Offset (RHS)
      then
         TBAA := Common_TBAA (TBAA_Type (LHS), TBAA_Type (RHS));
      else
         TBAA := No_Metadata_T;
      end if;

      --  If we did, make an access tag

      if Present (TBAA) then
         Orig_Offset := TBAA_Offset (LHS);
         Offset      := Orig_Offset;
         Access_TBAA := Extract_Access_Type (TBAA, Offset, +Size);
         return Create_TBAA_Access_Tag (TBAA, Access_TBAA, Orig_Offset, +Size);
      else
         return No_Metadata_T;
      end if;

   end Compute_TBAA_Access;

   -------------------------
   -- Compute_TBAA_Struct --
   -------------------------

   function Compute_TBAA_Struct (LHS, RHS, Size : GL_Value) return Metadata_T
   is
      TBAA : Metadata_T;

      function Is_Our_Struct (TBAA : Metadata_T) return Boolean is
        (Present (TBAA) and then Is_Struct_Tag (TBAA)
         and then Size_In_Bytes (TBAA) = +Size);

   begin
      --  If size isn't a constant, we can't do anything or if either side
      --  aliases everything. Otherwise, see if we can find a struct type
      --  tag on either side that corresponds to the specified size.

      if not Is_A_Constant_Int (Size) or else Aliases_All (LHS)
        or else Aliases_All (RHS)
      then
         return No_Metadata_T;
      elsif Is_Our_Struct (TBAA_Type (LHS)) and then TBAA_Offset (LHS) = 0 then
         TBAA := TBAA_Type (LHS);
      elsif Is_Our_Struct (TBAA_Type (RHS)) and then TBAA_Offset (RHS) = 0 then
         TBAA := TBAA_Type (RHS);
      else
         return No_Metadata_T;
      end if;

      --  Make the arrays containing the values needed for the tbaa.struct

      declare
         Last_Field : constant Nat := Last_Field_Index (TBAA);
         TBAAs      : Metadata_Array (0 .. Last_Field);
         Offsets    : ULL_Array (0 .. Last_Field);
         Sizes      : ULL_Array (0 .. Last_Field);

      begin
         for J in 0 .. Last_Field loop
            declare
               F_Size : constant ULL        := Field_Size (TBAA, J);
               F_TBAA : constant Metadata_T := Field_Type (TBAA, J);
               Tag    : constant Metadata_T :=
                 Create_TBAA_Access_Tag (F_TBAA, F_TBAA, 0, F_Size);

            begin
               TBAAs   (J) := Tag;
               Offsets (J) := Field_Offset (TBAA, J);
               Sizes   (J) := Field_Size   (TBAA, J);
            end;
         end loop;

         return Create_TBAA_Struct_Node (TBAAs, Offsets, Sizes);
      end;

   end Compute_TBAA_Struct;

begin
   --  Make a dummy entry so the "Empty" entry is never used.

   TBAA_Info.Increment_Last;
end GNATLLVM.Aliasing;
