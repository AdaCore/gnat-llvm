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

with Sem;      use Sem;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Table;    use Table;

with GNATLLVM.Codegen;      use GNATLLVM.Codegen;
with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.GLType;       use GNATLLVM.GLType;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;
with GNATLLVM.Records;      use GNATLLVM.Records;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Wrapper;      use GNATLLVM.Wrapper;

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

   TBAA_Root : Metadata_T;
   --  Root of tree for Type-Based alias Analysis (TBAA) metadata

   function Create_TBAA_Type
     (TE     : Entity_Id;
      Ridx   : Record_Info_Id;
      Parent : Metadata_T;
      Kind   : TBAA_Kind) return Metadata_T
     with Pre => Present (Ridx) and then Present (Parent);
   --  Create a TBAA type entry for the specified Record_Info Id.
   --  If TE is Present, this is the only RI for TE

   function Create_TBAA_Type_Internal
     (TE     : Entity_Id;
      Ridx   : Record_Info_Id;
      Parent : Metadata_T;
      Kind   : TBAA_Kind) return Metadata_T
     with Pre => Present (Ridx) and then Present (Parent);
   --  Helper for above function when we know that we can't use an existing
   --  TBAA type

   function New_TBAA_Type
     (TE : Entity_Id; Parent : Metadata_T; Kind : TBAA_Kind) return Metadata_T
     with Pre => Is_Full_Base_Type (TE);
   --  Make a new TBAA type entry for TE, which is known to be a base type

   function Get_TBAA_Name (TE : Entity_Id; Kind : TBAA_Kind) return String;
   --  Return the name to use for a TBAA type entry for TE, if present

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      TBAA_Root := Create_TBAA_Root (MD_Builder);
      Search_For_UCs;
   end Initialize;

   ---------------------
   -- Initialize_TBAA --
   ---------------------

   procedure Initialize_TBAA (V : in out GL_Value) is
   begin
      if Relationship (V) = Reference then
         V.TBAA_Type   := Create_TBAA_Type (Related_Type (V), Base);
         V.TBAA_Offset := 0;
      else
         V.TBAA_Type := No_Metadata_T;
      end if;
   end Initialize_TBAA;

   ---------------------
   -- Initialize_TBAA --
   ---------------------

   function Initialize_TBAA (V : GL_Value) return GL_Value is
      New_V : GL_Value := V;
   begin
      Initialize_TBAA (New_V);
      return New_V;
   end Initialize_TBAA;

   --------------------
   -- Search_For_UCs --
   --------------------

   procedure Search_For_UCs is

      function Check_For_UC (N : Node_Id) return Traverse_Result;

      procedure Scan is new Traverse_Proc (Check_For_UC);

      procedure Scan_Unit is new Scan_Library_Item (Scan => Scan);
      --  Scan one library item looking for UCs between access types

      procedure Scan_All_Units is
         new Sem.Walk_Library_Items (Action => Scan_Unit);

      ------------------
      -- Check_For_UC --
      ------------------

      function Check_For_UC (N : Node_Id) return Traverse_Result is
         STE            : Entity_Id;
         TTE            : Entity_Id;
         Access_Types   : Boolean;

      begin
         --  If we run into a stub, we have to search inside it because
         --  Library_Unit is a semantic, not syntactic, field.

         if Nkind (N) in N_Body_Stub and then Present (Library_Unit (N)) then
            Scan (Library_Unit (N));
            return OK;

         --  Ignore if this is generic

         elsif Is_Generic_Item (N) then
            return Skip;

         --  Otherwise, all we care about are N_Validate_Unchecked_Conversion
         --  nodes between access types or where one type is an aggregate.

         elsif Nkind (N) /= N_Validate_Unchecked_Conversion then
            return OK;
         end if;

         STE          := Get_Full_View (Source_Type (N));
         TTE          := Get_Full_View (Target_Type (N));
         Access_Types := Is_Access_Type (STE) and then Is_Access_Type (TTE);
         if not Access_Types
           and then Is_Elementary_Type (STE) and then Is_Elementary_Type (TTE)
         then
            return OK;
         end if;

         --  We have to add one or more entries showing that the designated
         --  types should have the same TBAA value.

         declare
            SDT   : constant Entity_Id    :=
              (if Access_Types then Full_Designated_Type (STE) else STE);
            TDT   : constant Entity_Id    :=
              (if Access_Types then Full_Designated_Type (TTE) else TTE);
            SBT   : constant Entity_Id    := Full_Base_Type (SDT);
            TBT   : constant Entity_Id    := Full_Base_Type (TDT);
            S_Grp : constant UC_Group_Idx := Find_UC_Group (SBT);
            T_Grp : constant UC_Group_Idx := Find_UC_Group (TBT);
            Valid : constant Boolean      :=
              Esize (SBT) = Esize (TBT) and then not Is_Aggregate_Type (SBT)
              and then not Is_Aggregate_Type (TBT);

         begin
            --  If neither was seen before, allocate a new group and put them
            --  both in it.

            if No (S_Grp) and then No (T_Grp) then
               Last_UC_Group := Last_UC_Group + 1;
               UC_Table.Append ((SBT, Last_UC_Group, Valid));
               UC_Table.Append ((TBT, Last_UC_Group, Valid));

            --  If one has a group and the other doesn't, add the other
            --  pointing to that group.

            elsif No (S_Grp) and then Present (T_Grp) then
               UC_Table.Append ((SBT, T_Grp, Valid));
            elsif Present (S_Grp) and then No (T_Grp) then
               UC_Table.Append ((TBT, S_Grp, Valid));

            --  If both were assigned groups, move everything in the target's
            --  group to the group of the source.

            else
               for J in 1 .. UC_Table.Last loop
                  if UC_Table.Table (J).Group = T_Grp then
                     UC_Table.Table (J).Group := S_Grp;
                     UC_Table.Table (J).Valid :=
                       UC_Table.Table (J).Valid and Valid;
                  end if;
               end loop;
            end if;
         end;

         return OK;
      end Check_For_UC;

   begin
      --  Start of processing for Search_For_UCs

      Scan_All_Units;
   end Search_For_UCs;

   -------------------
   -- Get_TBAA_Name --
   -------------------

   function Get_TBAA_Name (TE : Entity_Id; Kind : TBAA_Kind) return String is
      Buf : Bounded_String;
   begin
      if Present (TE) then
         Append (Buf, Chars (TE));
      else
         Append (Buf, "TBAA");
      end if;

      case Kind is
         when Base =>
            Append (Buf, "#TB");
         when For_Aliased =>
            null;
         when Unique =>
            Append (Buf, "#T");
            Append (Buf, Name_Idx);
            Name_Idx := Name_Idx + 1;
      end case;

      return +Buf;
   end Get_TBAA_Name;

   ----------------------
   -- Create_TBAA_Type --
   ----------------------

   function Create_TBAA_Type
     (TE : Entity_Id; Kind : TBAA_Kind) return Metadata_T
   is
      BT        : constant Entity_Id    := Full_Base_Type (TE);
      Grp       : constant UC_Group_Idx := Find_UC_Group (BT);
      TBAA      : Metadata_T            := Get_TBAA (BT);
      Base_TBAA : Metadata_T;

   begin
      --  If we have -fno-strict-aliasing or this is a void type, don't
      --  create a TBAA.

      if Flag_No_Strict_Aliasing or else Ekind (BT) = E_Void then
         return No_Metadata_T;

      --  ??? For now, if this is a scalar and the size of the base type
      --  differs from this type, don't create a TBAA.

      elsif Esize (TE) /= Esize (BT) then
         return No_Metadata_T;

      --  If we haven't already saved TBAA data and this type is in a
      --  group related by UC's between access types, use any TBAA
      --  we've already made for a type in that group.

      elsif No (TBAA) and then Present (Grp) then

         --  If this is an aggregate type, we can't use the same type tag
         --  because looking into the structures will fail.  We have no choice
         --  here but to not return a type tag in this case.

         if Is_Aggregate_Type (BT) then
            return No_Metadata_T;
         end if;

         for J in 1 .. UC_Table.Last loop
            declare
               UCE : constant UC_Entry := UC_Table.Table (J);

            begin
               if UCE.Group = Grp and then Present (Get_TBAA_N (UCE.TE))
                 and then No (TBAA)
               then
                  TBAA := Get_TBAA (UCE.TE);
               end if;

               --  Check for something invalidating the group

               if UCE.Group = Grp and then not UCE.Valid then
                  return No_Metadata_T;
               end if;
            end;
         end loop;
      end if;

      --  If we haven't saved a TBAA type tag, we first need to make base
      --  and aliased TBAA type tags and save the latter.

      if Present (TBAA) then
         Base_TBAA := TBAA_Parent (TBAA);
      else
         Base_TBAA := New_TBAA_Type (BT, TBAA_Root, Base);
         if No (Base_TBAA) then
            return No_Metadata_T;
         end if;

         TBAA      := New_TBAA_Type (BT, Base_TBAA, For_Aliased);
         Set_TBAA (BT, TBAA);
      end if;

      --  Finally, return the proper TBAA type tag for our usage

      case Kind is
         when Base =>
            return Base_TBAA;
         when For_Aliased =>
            return TBAA;
         when Unique =>
            return New_TBAA_Type (BT, Base_TBAA, Unique);
      end case;

   end Create_TBAA_Type;

   ----------------------
   -- Create_TBAA_Type --
   ----------------------

   function Create_TBAA_Type
     (GT : GL_Type; Kind : TBAA_Kind) return Metadata_T
   is
     ((if   Is_Primitive_GL_Type (GT)
       then Create_TBAA_Type (Full_Etype (GT), Kind) else No_Metadata_T));

   ----------------------
   -- Create_TBAA_Type --
   ----------------------

   function Create_TBAA_Type
     (TE     : Entity_Id;
      Ridx   : Record_Info_Id;
      Parent : Metadata_T;
      Kind   : TBAA_Kind) return Metadata_T
   is
      TBAA : Metadata_T := TBAA_Type (Ridx);

   begin
      if No (TBAA) or else TBAA_Parent (TBAA) /= Parent
        or else Kind = Unique
      then
         TBAA := Create_TBAA_Type_Internal (TE, Ridx, Parent, Kind);
      end if;

      if Present (TBAA) and then Kind /= Unique then
         Set_TBAA_Type (Ridx, TBAA);
      end if;

      return TBAA;
   end Create_TBAA_Type;

   -------------------------------
   -- Create_TBAA_Type_Internal --
   -------------------------------

   function Create_TBAA_Type_Internal
     (TE     : Entity_Id;
      Ridx   : Record_Info_Id;
      Parent : Metadata_T;
      Kind   : TBAA_Kind) return Metadata_T
   is
      Struct_Fields : constant Struct_Field_Array :=
        RI_To_Struct_Field_Array (Ridx);
      Offsets       : Value_Array    (Struct_Fields'Range);
      Sizes         : Value_Array    (Struct_Fields'Range);
      TBAAs         : Metadata_Array (Struct_Fields'Range);
      TBAA          : Metadata_T;

   begin
      --  If we have no data, this is an empty structure, so we can't have
      --  an access into it.

      if Struct_Fields'Length = 0 then
         return No_Metadata_T;
      end if;

      --  Otherwise fill in the three arrays above.  If we can't get a
      --  TBAA entry for a field, we can't make a TBAA type for the struct.

      for J in Struct_Fields'Range loop
         Offsets (J) := Const_Int (LLVM_Size_Type,
                                   Struct_Fields (J).Offset, False);
         Sizes   (J) := Const_Int
           (LLVM_Size_Type, To_Bytes (Get_Type_Size (Struct_Fields (J).T)),
            False);

         --  If there's no GT for the field, this is a field used to store
         --  bitfields.  So we make a unique scalar TBAA type entry for it.

         if No (Struct_Fields (J).GT) then
            TBAA := Create_TBAA_Scalar_Type_Node
              ("BF", G (Sizes (J), Size_GL_Type), TBAA_Root);

         --  Otherwise, if this GT isn't the primitive GT, we can't make
         --  a TBAA type entry for it, at least for now.

         elsif not Is_Primitive_GL_Type (Struct_Fields (J).GT) then
            TBAA := No_Metadata_T;

         --  Otherwise, try to get or make a type entry

         else
            TBAA := Create_TBAA_Type (Struct_Fields (J).GT,
                                      (if   Struct_Fields (J).Is_Aliased
                                       then For_Aliased else Unique));
         end if;

         --  If we found an entry, store it.  Otherwise, we fail.

         if Present (TBAA) then
            TBAAs (J) := TBAA;
         else
            return No_Metadata_T;
         end if;
      end loop;

      return Create_TBAA_Struct_Type_Node
        (Context, MD_Builder, Get_TBAA_Name (TE, Kind),
         LLVM_Value (RI_Size_In_Bytes (Ridx)), Struct_Fields'Length,
         Parent, TBAAs'Address, Offsets'Address, Sizes'Address);

   end Create_TBAA_Type_Internal;

   -------------------
   -- New_TBAA_Type --
   -------------------

   function New_TBAA_Type
     (TE     : Entity_Id;
      Parent : Metadata_T;
      Kind   : TBAA_Kind) return Metadata_T is
   begin
      --  If this isn't a native type, we can't make a TBAA type entry for it

      if Is_Nonnative_Type (TE) then
         return No_Metadata_T;

      --  If it's a scalar type, make a scalar type node

      elsif Is_Scalar_Type (TE) then
         declare
            Size : constant GL_Value := Get_Type_Size (Default_GL_Type (TE));

         begin
            return Create_TBAA_Scalar_Type_Node (Get_TBAA_Name (TE, Kind),
                                                 To_Bytes (Size), Parent);
         end;

      --  If it's a record type, we know above that its a native type, meaning
      --  that it just has one Record_Info entry, so its TBAA type entry is
      --  that of that entry.

      elsif Is_Record_Type (TE) then
         return Create_TBAA_Type (TE, Get_Record_Info (TE), Parent, Kind);

      --  Otherwise, we can't (yet) make a type entry for it

      else
         return No_Metadata_T;
      end if;

   end New_TBAA_Type;

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
      Orig_Offset   : constant ULL     := TBAA_Offset  (V);
      Base_Type     : Metadata_T       := TBAA_Type    (V);
      Offset        : ULL              := Orig_Offset;
      Access_Type   : Metadata_T;

   begin
      --  If we couldn't track V's TBAA information, we can try to just use
      --  the TBAA information from the type.

      if No (Base_Type) then
         Base_Type := Create_TBAA_Type (GT, Base);
         Offset    := 0;
      end if;

      --  If we still couldn't find a tag, if this type is marked to alias
      --  everything, or if our size is zero, don't do anything.

      if Present (Base_Type) and then not Universal_Aliasing (GT)
        and then Size_In_Bytes /= 0
      then
         Access_Type := Extract_Access_Type (Base_Type, Offset, Size_In_Bytes);
         Add_TBAA_Access
           (Inst, Create_TBAA_Access_Tag (MD_Builder, Base_Type, Access_Type,
                                          Orig_Offset, Size_In_Bytes));
      end if;
   end Add_Aliasing_To_Instruction;

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
end GNATLLVM.Aliasing;
