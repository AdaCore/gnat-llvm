------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with LLVM.Debug_Info;      use LLVM.Debug_Info;

with Sinput; use Sinput;
with Table;  use Table;

with GNATLLVM.Subprograms; use GNATLLVM.Subprograms;
with GNATLLVM.Wrapper;     use GNATLLVM.Wrapper;

package body GNATLLVM.DebugInfo is

   --  We maintain a stack of debug info contexts, with the outermost
   --  context being global (??? not currently supported), then a subprogram,
   --  and then lexical blocks.

   Debug_Scope_Low_Bound : constant := 1;

   type Debug_Scope is record
      SFI   : Source_File_Index;
      --  Source file index for this scope

      Scope : Metadata_T;
      --  LLVM debugging metadata for this scope
   end record;

   package Debug_Scope_Table is new Table.Table
     (Table_Component_Type => Debug_Scope,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => Debug_Scope_Low_Bound,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Debug_Scope_Table");
   --  Table of debugging scopes. The last inserted scope point corresponds
   --  to the current scope.

   function Has_Debug_Scope return Boolean is
     (Debug_Scope_Table.Last >= Debug_Scope_Low_Bound);
   --  Says whether we do or don't currently have a debug scope.
   --  Won't be needed when we support a global scope.

   function Current_Debug_Scope return Metadata_T is
     (Debug_Scope_Table.Table (Debug_Scope_Table.Last).Scope)
     with Post => Present (Current_Debug_Scope'Result);
   --  Current debug info scope

   function Current_Debug_SFI return Source_File_Index is
     (Debug_Scope_Table.Table (Debug_Scope_Table.Last).SFI);
   --  Current debug info source file index

   Freeze_Pos_Level : Natural := 0;
   --  Current level of pushes of requests to freeze debug position

   subtype UL is Interfaces.C.unsigned_long;

   ----------------------
   -- Push_Debug_Scope --
   ----------------------

   procedure Push_Debug_Scope (SFI : Source_File_Index; Scope : Metadata_T) is
   begin
      if Emit_Debug_Info then
         Debug_Scope_Table.Append ((SFI, Scope));
      end if;
   end Push_Debug_Scope;

   ---------------------
   -- Pop_Debug_Scope --
   ---------------------

   procedure Pop_Debug_Scope is
   begin
      if Emit_Debug_Info and then not Library_Level then
         Debug_Scope_Table.Decrement_Last;
      end if;
   end Pop_Debug_Scope;

   --------------------------
   -- Initialize_Debugging --
   --------------------------

   procedure Initialize is
   begin
      if Emit_Debug_Info then
         Add_Debug_Flags (Module);
         DI_Builder         := Create_DI_Builder (Module);
         Debug_Compile_Unit :=
           Create_Debug_Compile_Unit
           (DI_Builder, Get_Debug_File_Node (Main_Source_File));
      end if;
   end Initialize;

   ------------------------
   -- Finalize_Debugging --
   ------------------------

   procedure Finalize_Debugging is
   begin
      if Emit_Debug_Info then
         DI_Builder_Finalize (DI_Builder);
      end if;
   end Finalize_Debugging;

   -------------------------
   -- Get_Debug_File_Node --
   -------------------------

   function Get_Debug_File_Node (File : Source_File_Index) return Metadata_T is
   begin
      if DI_Cache = null then
         DI_Cache :=
           new DI_File_Cache'(1 .. Last_Source_File => No_Metadata_T);
      end if;

      if DI_Cache (File) /= No_Metadata_T then
         return DI_Cache (File);
      end if;

      declare
         Full_Name : constant String :=
           Get_Name_String (Full_Debug_Name (File));
         Name      : constant String :=
           Get_Name_String (Debug_Source_Name (File));
         DIFile    : constant Metadata_T :=
           DI_Create_File (DI_Builder, Name, UL (Name'Length),
                           Full_Name (1 .. Full_Name'Length - Name'Length),
                           UL (Full_Name'Length - Name'Length));
      begin
         DI_Cache (File) := DIFile;
         return DIFile;
      end;
   end Get_Debug_File_Node;

   ----------------------------------
   -- Create_Subprogram_Debug_Info --
   ----------------------------------

   function Create_Subprogram_Debug_Info
     (Func           : GL_Value;
      Def_Ident      : Entity_Id;
      N              : Node_Id;
      Name, Ext_Name : String) return Metadata_T
   is
      pragma Unreferenced (Def_Ident);
   begin
      if Emit_Debug_Info then
         return Create_Debug_Subprogram
           (DI_Builder, LLVM_Value (Func),
            Get_Debug_File_Node (Get_Source_File_Index (Sloc (N))),
            Name, Ext_Name, Get_Logical_Line_Number (Sloc (N)));
      else
         return No_Metadata_T;
      end if;
   end Create_Subprogram_Debug_Info;

   ------------------------------
   -- Push_Lexical_Debug_Scope --
   ------------------------------

   procedure Push_Lexical_Debug_Scope (N : Node_Id) is
      SFI : constant Source_File_Index := Get_Source_File_Index (Sloc (N));

   begin
      if Emit_Debug_Info and then not Library_Level then
         Push_Debug_Scope
           (SFI, Create_Debug_Lexical_Block
              (DI_Builder, Current_Debug_Scope, Get_Debug_File_Node (SFI),
               Get_Logical_Line_Number (Sloc (N)),
               Get_Column_Number (Sloc (N))));
      end if;
   end Push_Lexical_Debug_Scope;

   ---------------------------
   -- Push_Debug_Freeze_Pos --
   ---------------------------

   procedure Push_Debug_Freeze_Pos is
   begin
      Freeze_Pos_Level := Freeze_Pos_Level + 1;
   end Push_Debug_Freeze_Pos;

   --------------------------
   -- Pop_Debug_Freeze_Pos --
   --------------------------

   procedure Pop_Debug_Freeze_Pos is
   begin
      Freeze_Pos_Level := Freeze_Pos_Level - 1;
   end Pop_Debug_Freeze_Pos;

   ---------------------------
   -- Set_Debug_Pos_At_Node --
   ---------------------------

   procedure Set_Debug_Pos_At_Node (N : Node_Id) is
      SFI : constant Source_File_Index := Get_Source_File_Index (Sloc (N));

   begin
      if Emit_Debug_Info and then Has_Debug_Scope
        and then Freeze_Pos_Level = 0 and then SFI = Current_Debug_SFI
      then
         Set_Debug_Loc (IR_Builder, Current_Debug_Scope,
                        Get_Logical_Line_Number (Sloc (N)),
                        Get_Column_Number (Sloc (N)));
      end if;
   end Set_Debug_Pos_At_Node;

end GNATLLVM.DebugInfo;
