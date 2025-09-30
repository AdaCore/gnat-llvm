------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                  Copyright (C) 2013-2025, AdaCore                        --
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

with Nlists;     use Nlists;
with Sinput;     use Sinput;
with Uintp.LLVM; use Uintp.LLVM;

with GNATLLVM.DebugInfo;   use GNATLLVM.DebugInfo;
with GNATLLVM.Helper;      use GNATLLVM.Helper;
with GNATLLVM.Utils;       use GNATLLVM.Utils;
with GNATLLVM.Wrapper;     use GNATLLVM.Wrapper;

with LLVM.Debug_Info; use LLVM.Debug_Info;

package body GNATLLVM.Records.Debug is

   function Hash (F : Record_Field_Kind_Id) return Ada.Containers.Hash_Type
     is (Hash_Type (F));
   --  A hash function for use in the discriminant map.

   package Discriminant_Map is new Ada.Containers.Hashed_Maps
     (Key_Type        => Record_Field_Kind_Id,
      Element_Type    => Metadata_T,
      Hash            => Hash,
      Equivalent_Keys => "=");
   --  A map from a discriminant's entity to the LLVM debuginfo.

   function Convert_Choices (Choices : List_Id) return Word_Array;
   --  Convert a list of choices (the discrete choices selecting a
   --  variant part) to a word array.  The resulting word array has an
   --  even number of entries, with each pair holding the low and high
   --  bounds of a given choice.  Returns an empty array for 'others'
   --  or if the choice list is not recognized in some way.

   function Convert_One_Field (M : in out Discriminant_Map.Map;
                               F : Record_Field_Kind_Id) return Metadata_T;
   --  Convert a single field to LLVM debug metadata.  M is a map to
   --  update; if the field is a discriminant, then it is recorded in
   --  the map for later lookup.  Returns the LLVM debug metadata.

   function Convert_Variant_Part (M : in out Discriminant_Map.Map;
                                  RI : Record_Info;
                                  Original_Type : Entity_Id;
                                  Is_Union : Boolean) return Metadata_Array
     with pre => RI.Variants /= null;
   --  Convert a variant part to LLVM debug metadata, returning an
   --  array holding the LLVM debug metadata for all the relevant
   --  fields.

   function Convert_RI_Chain (M : in out Discriminant_Map.Map;
                              Start : Record_Info_Id;
                              Original_Type : Entity_Id;
                              Is_Union : Boolean) return Metadata_Array;
   --  Convert a chain of Record_Info_Ids to LLVM debug metadata,
   --  returning an array holding the metadata for the relevant
   --  fields.

   ---------------------
   -- Convert_Choices --
   ---------------------

   function Convert_Choices (Choices : List_Id) return Word_Array is
      package Choice_Table is new Table.Table
        (Table_Component_Type => uint64_t,
         Table_Index_Type     => Int,
         Table_Low_Bound      => 1,
         Table_Initial        => 20,
         Table_Increment      => 5,
         Table_Name           => "Choice_Table");

      Low : Uint;
      High : Uint;
      Choice : Entity_Id := First (Choices);
      Empty_Result : Word_Array (1 .. 0);
   begin
      while Present (Choice) loop
         if Nkind (Choice) = N_Others_Choice then
            return Empty_Result;
         end if;
         Decode_Range (Choice, Low, High);
         Choice_Table.Append (uint64_t (UI_To_ULL (Low)));
         Choice_Table.Append (uint64_t (UI_To_ULL (High)));
         Next (Choice);
      end loop;

      declare
         Result : Word_Array (1 .. Choice_Table.Last);
      begin
         for J in Result'Range loop
            Result (J) := Choice_Table.Table (J);
         end loop;
         return Result;
      end;
   end Convert_Choices;

   -----------------------
   -- Convert_One_Field --
   -----------------------

   function Convert_One_Field (M : in out Discriminant_Map.Map;
                               F : Record_Field_Kind_Id) return Metadata_T
   is
      F_GT           : constant GL_Type    := Field_Type (F);
      Mem_MD         : constant Metadata_T :=
        Create_Type_Data (F_GT);
      Name           : constant String     := Get_Name (F);
      F_S            : constant Source_Ptr := Sloc (F);
      File           : constant Metadata_T :=
        Get_Debug_File_Node (Get_Source_File_Index (F_S));
      Offset         : constant ULL        :=
        UI_To_ULL (Component_Bit_Offset (F));
      Storage_Offset : constant ULL        :=
        (Offset / UBPU) * UBPU;
      MD             : constant Metadata_T :=
        (if   Is_Bitfield (F)
         then DI_Create_Bit_Field_Member_Type
           (No_Metadata_T, Name, File,
            Get_Physical_Line_Number (F_S),
            UI_To_ULL (Esize (F)), Offset,
            Storage_Offset, Mem_MD)
         else DI_Create_Member_Type
           (No_Metadata_T, Name, File,
            Get_Physical_Line_Number (F_S),
            UI_To_ULL (Esize (F)),
            Get_Type_Alignment (F_GT), Offset, Mem_MD));
   begin
      --  Ensure the field is available so that a later lookup of a
      --  discriminant will succeed.
      if Ekind (F) = E_Discriminant then
         --  Should not have been seen before.
         pragma Assert (not M.Contains (Original_Record_Component (F)));
         M.Insert (Original_Record_Component (F), MD);
      end if;
      return MD;
   end Convert_One_Field;

   --------------------------
   -- Convert_Variant_Part --
   --------------------------

   function Convert_Variant_Part (M : in out Discriminant_Map.Map;
                                  RI : Record_Info;
                                  Original_Type : Entity_Id;
                                  Is_Union : Boolean) return Metadata_Array
   is
      package Member_Table is new Table.Table
        (Table_Component_Type => Metadata_T,
         Table_Index_Type     => Int,
         Table_Low_Bound      => 1,
         Table_Initial        => 20,
         Table_Increment      => 5,
         Table_Name           => "Member_Table");

      Var_Node : Node_Id := First (RI.Variant_List);
   begin
      for J in RI.Variants'Range loop
         declare
            Empty_Fields : Metadata_Array (1 .. 0);
            Fields : constant Metadata_Array :=
              (if Present (RI.Variants (J))
               then Convert_RI_Chain (M, RI.Variants (J), Original_Type,
                                      Is_Union)
               else Empty_Fields);
         begin
            if Is_Union then
               for I in Fields'Range loop
                  Member_Table.Append (Fields (I));
               end loop;
            else
               declare
                  Choices : constant Word_Array :=
                    Convert_Choices (Discrete_Choices (Var_Node));
                  MD : Metadata_T;
               begin
                  MD := Create_Variant_Member (DI_Builder, Fields, Choices);
                  Member_Table.Append (MD);
               end;
            end if;
         end;
         Next (Var_Node);
      end loop;

      --  ??? handle Overlap_Variants here.

      declare
         Members : Metadata_Array (1 .. Member_Table.Last);
      begin
         for J in Members'Range loop
            Members (J) := Member_Table.Table (J);
         end loop;
         return Members;
      end;
   end Convert_Variant_Part;

   ----------------------
   -- Convert_RI_Chain --
   ----------------------

   function Convert_RI_Chain (M : in out Discriminant_Map.Map;
                              Start : Record_Info_Id;
                              Original_Type : Entity_Id;
                              Is_Union : Boolean) return Metadata_Array
   is
      package Member_Table is new Table.Table
        (Table_Component_Type => Metadata_T,
         Table_Index_Type     => Int,
         Table_Low_Bound      => 1,
         Table_Initial        => 20,
         Table_Increment      => 5,
         Table_Name           => "Member_Table");

      Ridx : Record_Info_Id := Start;
      RI : Record_Info;
      F : Record_Field_Kind_Id;
      F_Idx : Field_Info_Id;
      FI : Field_Info;
   begin
      --  Convert the ordinary fields.
      while Present (Ridx) loop
         RI := Record_Info_Table.Table (Ridx);
         F_Idx := RI.First_Field;
         while Present (F_Idx) loop
            FI := Field_Info_Table.Table (F_Idx);
            F := FI.Field;

            if FI.Is_Inherited then
               --  Inherited component, so we can skip it here.
               null;
            elsif Known_Static_Component_Bit_Offset (F) and then
                  Known_Static_Esize (F)
            then
               Member_Table.Append (Convert_One_Field (M, F));
            end if;

            F_Idx := FI.Next;
         end loop;

         Ridx := RI.Next;
      end loop;

      --  Convert the variant part, if any -- but only if compiled
      --  against an LLVM that supports variant parts with multiple
      --  members.
      if Types_Can_Have_Multiple_Variant_Members and then
         RI.Variants /= null
      then
         declare
            MD : Metadata_T;
            Parts : constant Metadata_Array :=
              Convert_Variant_Part (M, RI, Original_Type, Is_Union);
            Discrim : constant Entity_Id := Entity (RI.Variant_Expr);
         begin
            if Is_Union then
               for I in Parts'Range loop
                  Member_Table.Append (Parts (I));
               end loop;
            else
               if Ekind (Discrim) = E_Discriminant then
                  pragma Assert
                    (M.Contains (Original_Record_Component (Discrim)));
                  MD := Create_Variant_Part
                    (DI_Builder, M (Original_Record_Component (Discrim)),
                     Parts);
                  Member_Table.Append (MD);
               end if;
            end if;
         end;
      end if;

      declare
         Members : Metadata_Array (1 .. Member_Table.Last);

      begin
         for J in Members'Range loop
            Members (J) := Member_Table.Table (J);
         end loop;

         return Members;
      end;

   end Convert_RI_Chain;

   ------------------------------
   -- Create_Record_Debug_Info --
   ------------------------------

   function Create_Record_Debug_Info (TE : Void_Or_Type_Kind_Id;
                                      Original_Type : Entity_Id;
                                      Debug_Scope : Metadata_T;
                                      Name : String;
                                      Size : ULL;
                                      Align : Nat;
                                      S : Source_Ptr) return Metadata_T
   is
      Empty_Fields : Metadata_Array (1 .. 0);
      Result : Metadata_T;
      Is_Union : constant Boolean := Is_Unchecked_Union (Original_Type);
   begin
      if Original_Type /= TE
         and then Present (Get_Debug_Metadata (Original_Type))
      then
         return Get_Debug_Metadata (Original_Type);
      end if;

      --  A type might be self-referential.  For example, a record may
      --  have a member whose type refers back to the same record
      --  type.  To handle this case, we construct a empty composite
      --  type and record it; then later we update the members of the
      --  type.  Note that we pass a unique identifier here; that
      --  prevents LLVM from reusing an existing type in the case that
      --  this composite is nameless.
      if Is_Unchecked_Union (TE) then
         Result := DI_Create_Union_Type
           (Debug_Scope, Name,
            Get_Debug_File_Node (Get_Source_File_Index (S)),
            Get_Physical_Line_Number (S), Size, Align, DI_Flag_Zero,
            Empty_Fields, 0, Name);
      else
         Result := DI_Create_Struct_Type
           (Debug_Scope, Name,
            Get_Debug_File_Node (Get_Source_File_Index (S)),
            Get_Physical_Line_Number (S), Size, Align, DI_Flag_Zero,
            No_Metadata_T, Empty_Fields, 0, No_Metadata_T, Name);
      end if;

      Set_Debug_Metadata (TE, Result);
      if Original_Type /= TE then
         Set_Debug_Metadata (Original_Type, Result);
      end if;

      declare
         M : Discriminant_Map.Map;
         Ridx : constant Record_Info_Id := Get_Record_Info (Original_Type);
         Members : constant Metadata_Array :=
           Convert_RI_Chain (M, Ridx, Original_Type, Is_Union);
      begin
         --  At least in theory it seems that LLVM may replace
         --  the object entirely, so don't assume Result will be
         --  the same, and be sure to clear it from the cache.
         Result := Replace_Composite_Elements (DI_Builder, Result, Members);

         if Original_Type /= TE then
            Clear_Debug_Metadata (Original_Type);
            Set_Debug_Metadata (Original_Type, Result);
         end if;
         Clear_Debug_Metadata (TE);

         return Result;
      end;

   end Create_Record_Debug_Info;

end GNATLLVM.Records.Debug;
