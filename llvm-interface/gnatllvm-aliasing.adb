------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2020, AdaCore                     --
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

with Errout;   use Errout;
with Lib;      use Lib;
with Opt;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch13; use Sem_Ch13;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Table;    use Table;

with GNATLLVM.Aliasing.Params; use GNATLLVM.Aliasing.Params;
with GNATLLVM.Arrays;          use GNATLLVM.Arrays;
with GNATLLVM.Arrays.Create;   use GNATLLVM.Arrays.Create;
with GNATLLVM.Environment;     use GNATLLVM.Environment;
with GNATLLVM.GLType;          use GNATLLVM.GLType;
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

   --  We need to record all types that are the designated types of access
   --  types that are unchecked-converted into each other.  All of those
   --  types need to have the same TBAA value.  Likewise for a UC where one
   --  of the types is an aggregate.
   --
   --  We rely on the fact that UC's are uncommon, so we can have a table
   --  that we traverse inefficiently.  The table maps the entity for a
   --  type (the designated type) into an ordinal corresponding to the
   --  types which have access types that are UC'ed to each other.  If any
   --  of these objects are aggregates, we can't do this because we can't
   --  use the same struct type tag for other than that struct.  We also
   --  can't do this if any objects in the group are of different sizes.
   --  So check for that and invalidate the group if so.

   type UC_Group_Idx is new Nat;
   Empty_UC_Group_Idx : constant UC_Group_Idx := 0;

   function No      (Idx : UC_Group_Idx) return Boolean is
     (Idx = Empty_UC_Group_Idx);
   function Present (Idx : UC_Group_Idx) return Boolean is
     (Idx /= Empty_UC_Group_Idx);

   type UC_Entry is record
      TE    : Entity_Id;
      Group : UC_Group_Idx;
      Valid : Boolean;
   end record;

   package UC_Table is new Table.Table
     (Table_Component_Type => UC_Entry,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 3,
      Table_Increment      => 1,
      Table_Name           => "UC_Table");

   Last_UC_Group : UC_Group_Idx := 0;
   --  Last UC group number used when we need a new one

   function Find_UC_Group (TE : Entity_Id) return UC_Group_Idx
     with Pre => Is_Type (TE);
   --  Return the UC_Group_Idx corresponding to TE, if any

   procedure Search_For_UCs;
   --  Look through all units for UC's between two access types

   --  There are various objects related to arrays that each need to have
   --  their own TBAAA type tags.  These basically correspond to different
   --  GL_Relationships.  In most cases, these are always aliased, but
   --  fat pointers usually aren't.  So in that case, we do the same as we
   --  do for other TBAA type tags and store both a native and For_Aliased
   --  values.  For most values here, the type data for both the base type
   --  and subtypes are the same.  The exception is Bounds_And_Data, which
   --  is only defined for constrained subtypes.  In that case, it represents
   --  a structure consisting of both the bounds (which are the same for
   --  all subtypes) and the data (which is unique to the subtype and defined
   --  only for that subtype).  Note that the TBAA type tag for the data
   --  isn't stored here, but rather as the TBAA data for the subtype.

   type TBAA_Array_Info is record
      Fat_Pointer     : Metadata_T;
      Bounds          : Metadata_T;
      Bounds_And_Data : Metadata_T;
      Component       : Metadata_T;
   end record;

   package TBAA_Array_Info_Table is new Table.Table
     (Table_Component_Type => TBAA_Array_Info,
      Table_Index_Type     => TBAA_Info_Id'Base,
      Table_Low_Bound      => TBAA_Info_Low_Bound,
      Table_Initial        => 100,
      Table_Increment      => 50,
      Table_Name           => "TBAA_Array_Info_Table");

   TBAA_Root : Metadata_T;
   --  Root of tree for Type-Based alias Analysis (TBAA) metadata

   function Base_Type_For_Aliasing (TE : Entity_Id) return Entity_Id
     with Pre => Is_Type (TE), Post => Is_Type (Base_Type_For_Aliasing'Result);
   --  Given a type, return the GNAT type to be used to as the base type
   --  for aliasing purposes.

   function Is_Subtype_For_Aliasing (TE1, TE2 : Entity_Id) return Boolean
     with Pre => Is_Type (TE1) and then Is_Type (TE2);
   --  Return True iff TE1 is TE2 or a subtype of it using the above
   --  definition of base type.

   function Universal_Aliasing_Including_Bases (TE : Entity_Id) return Boolean
     with Pre => Is_Type (TE);
   --  Return True iff TE or any base type for aliasing has Universal_Aliasing
   --  set.

   function Get_TBAA_Type
     (TE : Entity_Id; Kind : TBAA_Kind) return Metadata_T
     with Pre => Is_Type_Or_Void (TE);
   function Get_TBAA_Type (GT : GL_Type; Kind : TBAA_Kind) return Metadata_T
     with Pre => Present (GT);
   --  Get a TBAA type entry for the specified type and kind

   function Create_TBAA_Type
     (GT     : GL_Type;
      Kind   : TBAA_Kind;
      Parent : Metadata_T := No_Metadata_T) return Metadata_T
     with Pre => Present (GT);
   function Create_TBAA_Type
     (TE     : Entity_Id;
      Kind   : TBAA_Kind;
      Parent : Metadata_T := No_Metadata_T) return Metadata_T
     with Pre => Is_Type (TE);

   function Create_TBAA_For_Elementary_Type
     (TE : Entity_Id; Kind : TBAA_Kind; Parent : Metadata_T) return Metadata_T
     with Pre => Is_Elementary_Type (TE) and then Present (Parent);
   function Create_TBAA_For_Record_Type
     (TE : Entity_Id; Kind : TBAA_Kind; Parent : Metadata_T) return Metadata_T
     with Pre => Is_Record_Type (TE) and then Present (Parent);
   --  Subprograms of above

   function Create_TBAA_For_Fat_Pointer
     (TE : Entity_Id; Kind : TBAA_Kind; Parent : Metadata_T) return Metadata_T
     with Pre  => Is_Array_Type (TE) and then Present (Parent),
          Post => Present (Create_TBAA_For_Fat_Pointer'Result);
   --  Create a TBAA type for a fat pointer

   function TBAA_Data_For_Array_Type (TE : Entity_Id) return TBAA_Info_Id
     with Pre  => Is_Array_Type (TE) and then Is_Base_Type (TE),
          Post => Present (TBAA_Data_For_Array_Type'Result);
   --  If not already created, make a TBAA_Array_Info entity for TE

   function Create_TBAA_For_Array_Data
     (TE : Entity_Id; Kind : TBAA_Kind; Parent : Metadata_T) return Metadata_T
     with Pre => Is_Array_Type (TE);
   --  If TE is a small, constrained array, create metadata for it

   function Get_TBAA_Name
     (Kind   : TBAA_Kind;
      TE     : Entity_Id := Empty;
      GT     : GL_Type   := No_GL_Type;
      Suffix : String    := "") return String;
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
   --  from it at Offset bytes from it.  Return the corresponding access
   --  tag and new offset.  If MD is not a struct tag or if we're accessing
   --  the entire structure, we keep Offset unchanged and return MD.

   function Get_Field_TBAA (F : Entity_Id; GT : GL_Type) return Metadata_T
     with Pre => Is_Field (F);
   --  Get (and maybe create) a TBAA tag for field F of type GT

   -----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if Opt.No_Strict_Aliasing or else Decls_Only then
         Flag_No_Strict_Aliasing := True;
      end if;

      TBAA_Root := Create_TBAA_Root (MD_Builder);

      if not Flag_No_Strict_Aliasing then
         Search_For_UCs;
      end if;
   end Initialize;

   ----------------------------
   -- Base_Type_For_Aliasing --
   ----------------------------

   function Base_Type_For_Aliasing (TE : Entity_Id) return Entity_Id is
   begin
      --  If this isn't a base type, that's what we want

      if not Is_Base_Type (TE) then
         return Full_Base_Type (TE);

      --  If this is a derived record type with the same
      --  representation as its parent or a tagged type use the parent.

      elsif Is_Record_Type (TE)
        and then ((Is_Derived_Type (TE)
                    and then Same_Representation (TE, Full_Etype (TE)))
                  or else (Is_Tagged_Type (TE) and then Full_Etype (TE) /= TE))
      then
         return Full_Etype (TE);

      --  Otherwise, this is the type to use

      else
         return TE;
      end if;

   end Base_Type_For_Aliasing;

   -----------------------------
   -- Is_Subtype_For_Aliasing --
   -----------------------------

   function Is_Subtype_For_Aliasing (TE1, TE2 : Entity_Id) return Boolean is
      BT : constant Entity_Id := Base_Type_For_Aliasing (TE2);

   begin
      return TE1 = TE2
        or else (BT /= TE2 and then Is_Subtype_For_Aliasing (TE1, BT));
   end Is_Subtype_For_Aliasing;

   ----------------------------------------
   -- Universal_Aliasing_Including_Bases --
   ----------------------------------------

   function Universal_Aliasing_Including_Bases (TE : Entity_Id) return Boolean
   is
      BT : constant Entity_Id := Base_Type_For_Aliasing (TE);

   begin
      return Universal_Aliasing (TE)
        or else (BT /= TE and then Universal_Aliasing_Including_Bases (BT));
   end Universal_Aliasing_Including_Bases;

   --------------------
   -- Search_For_UCs --
   --------------------

   procedure Search_For_UCs is

      procedure Add_To_UC_Table (STE, TTE : Entity_Id; Valid : in out Boolean)
        with Pre => Is_Type (STE) and then Is_Type (TTE);
      --  Make an entry in the UC table showing that STE and TTE are to
      --  be treated identically.  If we can't do that, clear Valid.

      function Check_For_UC (N : Node_Id) return Traverse_Result;

      procedure Scan is new Traverse_Proc (Check_For_UC);

      procedure Scan_Unit is new Scan_Library_Item (Scan => Scan);
      --  Scan one library item looking for UCs between access types

      procedure Scan_All_Units is
         new Sem.Walk_Library_Items (Action => Scan_Unit);

      ---------------------
      -- Add_To_UC_Table --
      ---------------------

      procedure Add_To_UC_Table (STE, TTE : Entity_Id; Valid : in out Boolean)
      is
         SBT       : constant Entity_Id    := Base_Type_For_Aliasing (STE);
         TBT       : constant Entity_Id    := Base_Type_For_Aliasing (TTE);
         S_Is_Base : constant Boolean      := SBT = STE;
         T_Is_Base : constant Boolean      := TBT = TTE;
         S_Grp     : constant UC_Group_Idx := Find_UC_Group (STE);
         T_Grp     : constant UC_Group_Idx := Find_UC_Group (TTE);
         Our_Valid : Boolean               :=
           Valid and then Esize (STE) = Esize (TTE)
           and then not Is_Aggregate_Type (STE)
           and then not Is_Aggregate_Type (TTE) and then S_Is_Base = T_Is_Base;

      begin
         --  If neither of these are base types for aliasing purposes, make
         --  an entry for the base types.  This will produce entries all the
         --  way up the base chain to make those types equivalent.  If any
         --  aren't valid, then the below call will make the subtypes invalid.

         if not S_Is_Base and then not T_Is_Base then
            Add_To_UC_Table (SBT, TBT, Our_Valid);
         end if;

         --  If neither was seen before, allocate a new group and put them
         --  both in it.

         if No (S_Grp) and then No (T_Grp) then
            Last_UC_Group := Last_UC_Group + 1;
            UC_Table.Append ((STE, Last_UC_Group, Our_Valid));
            UC_Table.Append ((TTE, Last_UC_Group, Our_Valid));

         --  If one has a group and the other doesn't, add the other pointing
         --  to that group.

         elsif No (S_Grp) and then Present (T_Grp) then
            UC_Table.Append ((STE, T_Grp, Our_Valid));
         elsif Present (S_Grp) and then No (T_Grp) then
            UC_Table.Append ((TTE, S_Grp, Our_Valid));

         --  If both were assigned groups, move everything in the target's
         --  group to the group of the source.

         else
            for J in 1 .. UC_Table.Last loop
               if UC_Table.Table (J).Group = T_Grp then
                  UC_Table.Table (J).Group := S_Grp;
                  UC_Table.Table (J).Valid :=
                    UC_Table.Table (J).Valid and Our_Valid;

                  if not UC_Table.Table (J).Valid then
                     Our_Valid := False;
                  end if;
               end if;
            end loop;
         end if;

         --  Update our input validity flag to correspond with what we found

         Valid := Valid and Our_Valid;
      end Add_To_UC_Table;

      ------------------
      -- Check_For_UC --
      ------------------

      function Check_For_UC (N : Node_Id) return Traverse_Result is
         STE : Entity_Id;
         TTE : Entity_Id;

         function OK_Unit (N : Node_Id; TE : Entity_Id) return Boolean is
           (In_Same_Extended_Unit (N, TE)
              or else In_Extended_Main_Code_Unit (TE))
           with Pre => Present (N) and then Is_Type (TE);
         --  We can do something with TE if it's either in the code unit
         --  that we're compiling or in the same (extended) unit as the UC.

         function Is_Data_Access_Type (TE : Entity_Id) return Boolean is
           (Is_Access_Type (TE)
              and then Ekind (TE) /= E_Access_Subprogram_Type)
           with Pre => Is_Type (TE);
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
         --  between access types.  If the target type has
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
            SDT   : constant Entity_Id    := Full_Designated_Type (STE);
            TDT   : constant Entity_Id    := Full_Designated_Type (TTE);
            Valid : Boolean               := True;

         begin
            --  If the the target is either the same or a subtype of the
            --  source, we have nothing to do.  This can happen when we
            --  have two access types to the same underlying type or in
            --  some subtype cases.  Likewise if the target has been marked
            --  for universal aliasing.

            if Is_Subtype_For_Aliasing (TDT, SDT)
              or else Universal_Aliasing_Including_Bases (TDT)
            then
               return OK;

            --  The most efficient way of dealing with this if the
            --  target is an access type is to set No_Strict_Aliasing on
            --  that type because that will only affect references to the
            --  designated type via that access type.   The front end has
            --  already done that for us in the cases where it can be done
            --  and we've checked for that above.
            --
            --  The next best option is to make a table entry.  However, we
            --  can't do that if this UC is in a body and one of the types
            --  isn't in the same compilation unit.

            elsif Nkind_In (Unit (Enclosing_Comp_Unit_Node (N)),
                                 N_Package_Body, N_Subprogram_Body)
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
                    ("?possible aliasing problem for type&", N, TTE);
                  Error_Msg_N
                    ("\\?use -fno-strict-aliasing switch for references", N);
                  Error_Msg_NE
                    ("\\?or use `pragma No_Strict_Aliasing (&);`", N, TTE);
               end if;
            end if;

            Add_To_UC_Table (SDT, TDT, Valid);
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

   function Find_UC_Group (TE : Entity_Id) return UC_Group_Idx is
   begin
      for J in 1 .. UC_Table.Last loop
         if UC_Table.Table (J).TE = TE then
            return UC_Table.Table (J).Group;
         end if;
      end loop;

      return Empty_UC_Group_Idx;
   end Find_UC_Group;

   ---------------------
   -- Initialize_TBAA --
   ---------------------

   procedure Initialize_TBAA
     (V : in out GL_Value; Kind : TBAA_Kind := Native)
   is
   begin
      if Relationship (V) = Reference then
         Set_TBAA_Type   (V, Get_TBAA_Type (Related_Type (V), Kind));
         Set_TBAA_Offset (V, 0);
      else
         Set_TBAA_Type (V, No_Metadata_T);
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
     (V : in out GL_Value; F : Entity_Id; F_GT : GL_Type) is
   begin
      if No (TBAA_Type (V)) and then not Is_Bitfield (F) then
         Set_TBAA_Type   (V, Get_Field_TBAA (F, F_GT));
         Set_TBAA_Offset (V, 0);
      end if;
   end Maybe_Initialize_TBAA_For_Field;

   --------------------
   -- Get_Field_TBAA --
   --------------------

   function Get_Field_TBAA (F : Entity_Id; GT : GL_Type) return Metadata_T is
      Fidx   : constant Field_Info_Id := Get_Field_Info (F);
      Kind   : constant TBAA_Kind     := Kind_From_Aliased (Is_Aliased (F));
      TBAA   : Metadata_T             := TBAA_Type (Fidx);
      PF     : constant Entity_Id     := Parent_Field (F);
      Parent : Metadata_T             := No_Metadata_T;

   begin
      if No (TBAA) then

         --  If we have a parent field, its TBAA type tag, if any, is our
         --  parent and we make a new TBAA type tag for our type.  If not,
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
      TE     : Entity_Id := Empty;
      GT     : GL_Type   := No_GL_Type;
      Suffix : String    := "") return String
   is
      Our_TE : constant Entity_Id :=
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
     (TE : Entity_Id; Kind : TBAA_Kind) return Metadata_T
   is
      Grp         : constant UC_Group_Idx := Find_UC_Group (TE);
      TBAA        : Metadata_T            := Get_TBAA (TE);
      Native_TBAA : Metadata_T;

   begin
      --  If we have -fno-strict-aliasing, this type isn't to use
      --  type-based aliasing, or this is a void type, don't create a
      --  TBAA tag.

      if Flag_No_Strict_Aliasing or else Ekind (TE) = E_Void
        or else Universal_Aliasing_Including_Bases (TE)
      then
         return No_Metadata_T;

      --  If we haven't already saved TBAA data and this type is in a
      --  group related by UC's between access types, use any TBAA
      --  we've already made for a type in that group.

      elsif No (TBAA) and then Present (Grp) then
         for J in 1 .. UC_Table.Last loop
            declare
               UCE : constant UC_Entry := UC_Table.Table (J);

            begin
               if UCE.Group = Grp and then Present (Get_TBAA_N (UCE.TE))
                 and then No (TBAA)
               then
                  TBAA := Get_TBAA_N (UCE.TE);
               end if;

               --  Check for something invalidating the group, such as an
               --  aggregate type, because can't use the same type tag
               --  because looking into the structures will fail.  We have
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

         TBAA := Create_TBAA_Type (TE, For_Aliased, Parent => Native_TBAA);
         Set_TBAA (TE, TBAA);
      end if;

      --  Finally, return the proper TBAA type tag for our usage

      case Kind is
         when Native =>
            return Native_TBAA;
         when For_Aliased =>
            return TBAA;
         when Unique =>
            return Create_TBAA_Type (TE, Unique, Parent => Native_TBAA);
         when Unique_Aliased =>
            return Create_TBAA_Type (TE, Unique_Aliased, Parent => TBAA);
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
      TE         : constant Entity_Id  := Full_Etype (GT);
      TBAA       : constant Metadata_T := Get_TBAA_Type (TE, For_Aliased);
      Our_Parent : constant Metadata_T :=
        (if Present (Parent) then Parent else TBAA_Root);
      Prim_GT    : constant GL_Type    := Primitive_GL_Type (GT);
      Prim_TBAA  : constant Metadata_T :=
        (if   No (TBAA) then No_Metadata_T
         else (case Kind is
                 when For_Aliased => TBAA,
                 when Native => TBAA_Parent (TBAA),
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
      --  tag as the only field since we don't care about padding.

      elsif Is_Padded_GL_Type (GT) then
         declare
            TBAAs   : constant Metadata_Array (1 .. 1) := (1 => Prim_TBAA);
            Sizes   : constant GL_Value_Array (1 .. 1)    :=
              (1 => To_Bytes (Get_Type_Size (Type_Of (Prim_GT))));
            Offsets : constant GL_Value_Array (1 .. 1)    :=
              (1 => Size_Const_Null);

         begin
            return Create_TBAA_Struct_Type_Node
              (Get_TBAA_Name (Kind, GT => GT, TE => TE),
               To_Bytes (GT_Size (GT)), Our_Parent, Offsets, Sizes, TBAAs);
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
     (TE     : Entity_Id;
      Kind   : TBAA_Kind;
      Parent : Metadata_T := No_Metadata_T) return Metadata_T
   is
      BT           : constant Entity_Id  := Base_Type_For_Aliasing (TE);
      Inner_Parent : constant Metadata_T :=
        (if    Present (Parent) then Parent
         elsif BT /= TE then Get_TBAA_Type (BT, Native) else TBAA_Root);
      Our_Parent   : constant Metadata_T :=
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
     (TE : Entity_Id; Kind : TBAA_Kind; Parent : Metadata_T) return Metadata_T
   is
      GT   : constant GL_Type  := Primitive_GL_Type (TE);
      Size : constant GL_Value := Get_Type_Size (Type_Of (GT));

   begin
      --  ??? This is a fat pointer, we currently have no mechanism to make
      --  a node for it (but it's not a scalar node in any event.

      if (Is_Access_Type (TE)
            and then Relationship_For_Access_Type (GT) = Fat_Pointer)
        or else Is_Access_Subprogram_Type (TE)
      then
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
     (TE : Entity_Id; Kind : TBAA_Kind; Parent : Metadata_T) return Metadata_T
   is
      Ridx          : constant Record_Info_Id     := Get_Record_Info (TE);
      Struct_Fields : constant Struct_Field_Array :=
        RI_To_Struct_Field_Array (Ridx);
      Offsets       : GL_Value_Array (Struct_Fields'Range);
      Sizes         : GL_Value_Array (Struct_Fields'Range);
      TBAAs         : Metadata_Array (Struct_Fields'Range);

   begin
      --  If we have no data, this is an empty structure, so we can't have
      --  an access into it.

      if Struct_Fields'Length = 0 then
         return No_Metadata_T;
      end if;

      --  Otherwise fill in the three arrays above.  If we can't get a TBAA
      --  entry for a field, we can't make a TBAA type for the struct.

      for J in Struct_Fields'Range loop
         declare
            SF : constant Struct_Field := Struct_Fields (J);

         begin
            Offsets (J) := Size_Const_Int (SF.Offset);
            Sizes   (J) := To_Bytes (Get_Type_Size (SF.T));

            --  If there's no GT for the field, this is a field used to
            --  store bitfields.  So we make a unique scalar TBAA type
            --  entry for it.

            if No (SF.GT) then
               TBAAs (J) :=
                 Create_TBAA_Scalar_Type_Node ("BF", Sizes (J), TBAA_Root);

               --  Otherwise, try to get or make a type entry

            else
               TBAAs (J) := Get_Field_TBAA (SF.Field, SF.GT);
            end if;

            --  If we found an entry, store it.  Otherwise, we fail.

            if No (TBAAs (J)) then
               return No_Metadata_T;
            end if;
         end;
      end loop;

      return Create_TBAA_Struct_Type_Node
        (Get_TBAA_Name (Kind, TE => TE),
         To_Bytes (Get_Type_Size (Primitive_GL_Type (TE))), Parent, Offsets,
         Sizes, TBAAs);

   end Create_TBAA_For_Record_Type;

   ---------------------------------
   -- Create_TBAA_For_Fat_Pointer --
   ---------------------------------

   function Create_TBAA_For_Fat_Pointer
     (TE : Entity_Id; Kind : TBAA_Kind; Parent : Metadata_T) return Metadata_T
   is
      FP_T   : constant Type_T     := Create_Array_Fat_Pointer_Type (TE);
      Size   : constant GL_Value   := To_Bytes (Get_Type_Size (FP_T));

   begin
      --  A fat pointer really isn't a scalar, but we're not going to be
      --  addressing into it and it's simpler if we treat it that way.

      return Create_TBAA_Scalar_Type_Node
        (Get_TBAA_Name (Kind, TE => TE, Suffix => "#FP"), Size, Parent);
   end Create_TBAA_For_Fat_Pointer;

   ------------------------------
   -- TBAA_Data_For_Array_Type --
   ------------------------------

   function TBAA_Data_For_Array_Type (TE : Entity_Id) return TBAA_Info_Id is
      Comp_GT : constant GL_Type    := Full_Component_GL_Type (TE);
      Tidx    : TBAA_Info_Id        := Get_TBAA_Info (TE);
      TI      : TBAA_Array_Info     := (others => No_Metadata_T);

   begin
      --  If we already made one, return it

      if Present (Tidx) then
         return Tidx;
      end if;

      --  Start by setting the component type tag, if there is one, and the
      --  fat pointer, which has both a Native and For_Aliased TBAA type.

      TI.Component   :=
        Get_TBAA_Type (Comp_GT,
                       Kind_From_Aliased (Has_Aliased_Components (TE)));
      TI.Fat_Pointer :=
        Create_TBAA_For_Fat_Pointer
        (TE, For_Aliased, Create_TBAA_For_Fat_Pointer (TE, Native, TBAA_Root));

      --  Now compute the TBAA struct tag for bounds.  Since bounds can't
      --  be modified, use a non-aliased unique version of the bound type.

      declare
         Ndims   : constant Nat := Number_Dimensions (TE);
         Offset  : GL_Value     := Size_Const_Null;
         Offsets : GL_Value_Array (0 .. Ndims * 2 - 1);
         Sizes   : GL_Value_Array (0 .. Ndims * 2 - 1);
         TBAAs   : Metadata_Array (0 .. Ndims * 2 - 1);
         Dim_GT  : GL_Type;
         Size    : GL_Value;

      begin
         for Dim in 0 .. Number_Dimensions (TE) - 1 loop
            Dim_GT := Array_Index_GT (TE, Dim);
            Size   := To_Bytes (Get_Type_Size (Type_Of (Dim_GT)));

            Offsets (Dim * 2)     := Offset;
            Offsets (Dim * 2 + 1) := Offset + Size;
            Sizes   (Dim * 2)     := Size;
            Sizes   (Dim * 2 + 1) := Size;
            TBAAs   (Dim * 2)     := Get_TBAA_Type (Dim_GT, Unique);
            TBAAs   (Dim * 2 + 1) := Get_TBAA_Type (Dim_GT, Unique);

            Offset := Offset + (Size * 2);
         end loop;

         --  If one of the index types has Universal_Aliasing, we won't
         --  have a TBAA type for it above.  So we can't form a TBAA type
         --  for our bounds either.
         --  ??? Should we do something about this?

         if (for all T of TBAAs => Present (T)) then
            TI.Bounds :=
              Create_TBAA_Struct_Type_Node
              (Get_TBAA_Name (Unique, TE => TE, Suffix => "#BND"),
               Offset, TBAA_Root, Offsets, Sizes, TBAAs);
         end if;
      end;

      TBAA_Array_Info_Table.Append (TI);
      Tidx := TBAA_Array_Info_Table.Last;
      Set_TBAA_Info (TE, Tidx);
      return Tidx;
   end TBAA_Data_For_Array_Type;

   --------------------------------
   -- Create_TBAA_For_Array_Data --
   --------------------------------

   function Create_TBAA_For_Array_Data
     (TE : Entity_Id; Kind : TBAA_Kind; Parent : Metadata_T) return Metadata_T
   is
      C_GT   : constant GL_Type      := Full_Component_GL_Type (TE);
      C_Size : constant GL_Value     := To_Bytes (Get_Type_Size (C_GT));
      GT     : constant GL_Type      := Primitive_GL_Type (TE);
      BT     : constant Entity_Id    := Full_Base_Type (TE);
      Tidx   : constant TBAA_Info_Id := TBAA_Data_For_Array_Type (BT);
      C_TBAA : constant Metadata_T   :=
        TBAA_Array_Info_Table.Table (Tidx).Component;

   begin
      --  If this isn't a loadable type, we don't need to handle this and
      --  we either might not be able to (if it's of variable size) or don't
      --  want to (if it has too many elements).  We also can't do anything
      --  if there's no type tag for the component or if the component is
      --  of zero size.

      if not Is_Loadable_Type (GT) or else No (C_TBAA) or else C_Size = 0 then
         return No_Metadata_T;
      end if;

      --  Otherwise, we know that everything is of constant (and small) size,
      --  so set up to make a structure type tag for this array.

      declare
         Size      : constant GL_Value     := To_Bytes (Get_Type_Size (GT));
         C_Aliased : constant Boolean      := Has_Aliased_Components (TE);
         Elmts     : constant Nat          := +(Size / C_Size);
         Offset    : GL_Value              := Size_Const_Null;
         Offsets   : GL_Value_Array (1 .. Elmts);
         Sizes     : GL_Value_Array (1 .. Elmts);
         TBAAs     : Metadata_Array (1 .. Elmts);

      begin
         for J in 1 .. Elmts loop
            Offsets (J) := Offset;
            Sizes (J)   := C_Size;
            TBAAs (J)   :=
              Create_TBAA_Type (C_GT, Kind_From_Aliased (C_Aliased), C_TBAA);
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
      --  tag.  We find the last field whose offset is less than or equal
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
        To_Bytes (Get_Type_Size (Type_Of (GT)));
      Orig_Offset   : ULL              := TBAA_Offset  (V);
      Base_Type     : Metadata_T       := TBAA_Type    (V);
      Offset        : ULL              := Orig_Offset;
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
      --  the TBAA information from the type.

      elsif No (Base_Type) then
         Base_Type   := Get_TBAA_Type (GT, Native);
         Offset      := 0;
         Orig_Offset := 0;
      end if;

      --  If we still couldn't find a tag or if our size is zero, don't do
      --  anything.

      if Present (Base_Type) and then Size_In_Bytes /= 0 then
         Access_Type := Extract_Access_Type (Base_Type, Offset, Size_In_Bytes);
         Add_TBAA_Access
           (Inst, Create_TBAA_Access_Tag (MD_Builder, Base_Type, Access_Type,
                                          Orig_Offset, Size_In_Bytes));
      end if;
   end Add_Aliasing_To_Instruction;

begin
   --  Make a dummy entry so the "Empty" entry is never used.

   TBAA_Array_Info_Table.Increment_Last;
end GNATLLVM.Aliasing;
