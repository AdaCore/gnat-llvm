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

with Sem_Util;   use Sem_Util;
with Sinput;     use Sinput;
with Uintp.LLVM; use Uintp.LLVM;

with GNATLLVM.DebugInfo;   use GNATLLVM.DebugInfo;
with GNATLLVM.Helper;      use GNATLLVM.Helper;
with GNATLLVM.Utils;       use GNATLLVM.Utils;
with GNATLLVM.Wrapper;     use GNATLLVM.Wrapper;

with LLVM.Debug_Info; use LLVM.Debug_Info;

package body GNATLLVM.Records.Debug is

   function Convert_One_Field (F : Record_Field_Kind_Id) return Metadata_T;
   --  Convert a single field to LLVM debuginfo metadata.

   function Convert_RI_Chain (Start : Record_Info_Id;
                              Original_Type : Entity_Id) return Metadata_Array;
   --  Convert a chain of Record_Info_Ids to LLVM debug metadata,
   --  returning an array holding the metadata for the relevant
   --  fields.

   -----------------------
   -- Convert_One_Field --
   -----------------------

   function Convert_One_Field (F : Record_Field_Kind_Id) return Metadata_T
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
      return MD;
   end Convert_One_Field;

   ----------------------
   -- Convert_RI_Chain --
   ----------------------

   function Convert_RI_Chain (Start : Record_Info_Id;
                              Original_Type : Entity_Id) return Metadata_Array
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
      while Present (Ridx) loop
         RI := Record_Info_Table.Table (Ridx);
         F_Idx := RI.First_Field;
         while Present (F_Idx) loop
            FI := Field_Info_Table.Table (F_Idx);
            F := FI.Field;

            if Present (Original_Type) and then
               Get_Fullest_View (Scope (Ancestor_Field (F))) /= Original_Type
            then
               --  Inherited component, so we can skip it here.
               null;
            elsif Known_Static_Component_Bit_Offset (F)
                  and then Known_Static_Esize (F)
            then
               Member_Table.Append (Convert_One_Field (F));
            end if;

            F_Idx := FI.Next;
         end loop;

         Ridx := RI.Next;
      end loop;

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

   begin
      --  A type might be self-referential.  For example, a
      --  record may have a member whose type refers back to the
      --  same record type.  To handle this case, we construct a
      --  empty composite type and record it; then later we
      --  update the members of the type.
      if Is_Unchecked_Union (TE) then
         Result := DI_Create_Union_Type
           (Debug_Scope, Name,
            Get_Debug_File_Node (Get_Source_File_Index (S)),
            Get_Physical_Line_Number (S), Size, Align, DI_Flag_Zero,
            Empty_Fields, 0, "");
      else
         Result := DI_Create_Struct_Type
           (Debug_Scope, Name,
            Get_Debug_File_Node (Get_Source_File_Index (S)),
            Get_Physical_Line_Number (S), Size, Align, DI_Flag_Zero,
            No_Metadata_T, Empty_Fields, 0, No_Metadata_T, "");
      end if;

      Set_Debug_Metadata (TE, Result);

      declare
         Ridx : constant Record_Info_Id := Get_Record_Info (TE);
         Members : constant Metadata_Array :=
           Convert_RI_Chain (Ridx, Original_Type);
      begin
         --  At least in theory it seems that LLVM may replace
         --  the object entirely, so don't assume Result will be
         --  the same, and be sure to clear it from the cache.
         Result := Replace_Composite_Elements (DI_Builder, Result, Members);

         Clear_Debug_Metadata (TE);
         return Result;
      end;

   end Create_Record_Debug_Info;

end GNATLLVM.Records.Debug;
