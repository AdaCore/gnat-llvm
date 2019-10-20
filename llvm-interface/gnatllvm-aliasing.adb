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

with GNATLLVM.Codegen;     use GNATLLVM.Codegen;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLType;      use GNATLLVM.GLType;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Utils;       use GNATLLVM.Utils;
with GNATLLVM.Wrapper;     use GNATLLVM.Wrapper;

package body GNATLLVM.Aliasing is

   --  We need to record all types that are the designated types of access
   --  types that are unchecked-converted into each other.  All of those
   --  types need to have the same TBAA value.
   --
   --  We rely on the fact that such UC's are rare, so we can have a table
   --  that we traverse inefficiently.  The table maps the entity for a
   --  type (the designated type) into an ordinal corresponding to the
   --  types which have access types that are UC'ed to each other.

   type UC_Group_Idx is new Nat;
   Empty_UC_Group_Idx : constant UC_Group_Idx := 0;

   function No      (Idx : UC_Group_Idx) return Boolean is
     (Idx = Empty_UC_Group_Idx);
   function Present (Idx : UC_Group_Idx) return Boolean is
     (Idx /= Empty_UC_Group_Idx);

   type UC_Entry is record
      TE    : Entity_Id;
      Group : UC_Group_Idx;
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

   TBAA_Root : Metadata_T;
   --  Root of tree for Type-Based alias Analysis (TBAA) metadata

   function Create_TBAA_For_Type (TE : Entity_Id) return Metadata_T
     with Pre => Is_Type_Or_Void (TE);
   --  Create a TBAA type entry for the specified GNAT type

   procedure Search_For_UCs;
   --  Look through all units for UC's between two access types

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      TBAA_Root := Create_TBAA_Root (MD_Builder);
      Search_For_UCs;
   end Initialize;

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
         --  nodes between access types.

         elsif Nkind (N) /= N_Validate_Unchecked_Conversion
           or else not Is_Access_Type (Get_Full_View (Source_Type (N)))
           or else not Is_Access_Type (Get_Full_View (Target_Type (N)))
         then
            return OK;
         end if;

         --  We have to add one or more entries showing that the designated
         --  types should have the same TBAA value.

         declare
            STE   : constant Entity_Id    := Get_Full_View (Source_Type (N));
            TTE   : constant Entity_Id    := Get_Full_View (Target_Type (N));
            SDT   : constant Entity_Id    := Full_Designated_Type (STE);
            TDT   : constant Entity_Id    := Full_Designated_Type (TTE);
            SBT   : constant Entity_Id    := Full_Base_Type (SDT);
            TBT   : constant Entity_Id    := Full_Base_Type (TDT);
            S_Grp : constant UC_Group_Idx := Find_UC_Group (SBT);
            T_Grp : constant UC_Group_Idx := Find_UC_Group (TBT);

         begin
            --  If neither was seen before, allocate a new group and put them
            --  both in it.

            if No (S_Grp) and then No (T_Grp) then
               Last_UC_Group := Last_UC_Group + 1;
               UC_Table.Append ((SBT, Last_UC_Group));
               UC_Table.Append ((TBT, Last_UC_Group));

            --  If one has a group and the other doesn't, add the other
            --  pointing to that group.

            elsif No (S_Grp) and then Present (T_Grp) then
               UC_Table.Append ((SBT, T_Grp));
            elsif Present (S_Grp) and then No (T_Grp) then
               UC_Table.Append ((TBT, S_Grp));

            --  If both were assigned groups, move everything in the target's
            --  group to the group of the source.

            else
               for J in 1 .. UC_Table.Last loop
                  if UC_Table.Table (J).Group = T_Grp then
                     UC_Table.Table (J).Group := S_Grp;
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

   --------------------------
   -- Create_TBAA_For_Type --
   --------------------------

   function Create_TBAA_For_Type (TE : Entity_Id) return Metadata_T is
      BT   : constant Entity_Id    := Full_Base_Type (TE);
      TBAA : constant Metadata_T   := Get_TBAA (BT);
      Grp  : constant UC_Group_Idx := Find_UC_Group (BT);

   begin
      --  If we have -fno-strict-aliasing, don't create a TBAA

      if Flag_No_Strict_Aliasing then
         return No_Metadata_T;

      --  If the base type has a TBAA, use it for this type

      elsif Present (TBAA) then
         return TBAA;

      --  If this type is in a group related by UC's between access types,
      --  use any TBAA we've already made for a type in that group.

      elsif Present (Grp) then
         for J in 1 .. UC_Table.Last loop
            declare
               UCE : constant UC_Entry := UC_Table.Table (J);

            begin
               if UCE.Group = Grp and then Present (Get_TBAA_N (UCE.TE)) then
                  return Get_TBAA (UCE.TE);
               end if;
            end;
         end loop;
      end if;

      --  Otherwise, make a new TBAA for this type.  If it's a type that we
      --  don't currently make TBAA information for, return none.

      if Is_Scalar_Type (BT) then
         return Create_TBAA_Scalar_Type_Node (MD_Builder, Get_Name (BT),
                                              TBAA_Root);
      else
         return No_Metadata_T;
      end if;

   end Create_TBAA_For_Type;

   --------------------------
   -- Record_TBAA_For_Type --
   --------------------------

   procedure Record_TBAA_For_Type (TE : Entity_Id) is
      TBAA : constant Metadata_T := Create_TBAA_For_Type (TE);
   begin
      if Present (TBAA) then
         Set_TBAA (TE, TBAA);
      end if;
   end Record_TBAA_For_Type;

   ---------------------------------
   -- Add_Aliasing_To_Instruction --
   ---------------------------------

   procedure Add_Aliasing_To_Instruction (Inst : Value_T; V : GL_Value) is
      GT           : constant GL_Type    := Related_Type (V);
      TBAA         : constant Metadata_T := Get_TBAA (Full_Etype (GT));

   begin
      if Present (TBAA) and then not Universal_Aliasing (GT) then
         Add_TBAA_Access
           (Inst, Create_TBAA_Access_Tag (MD_Builder, TBAA, TBAA, 0));
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
