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

with LLVM.Core;       use LLVM.Core;

with Opt;        use Opt;
with Sinput;     use Sinput;
with Table;      use Table;
with Uintp.LLVM; use Uintp.LLVM;

with GNATLLVM.Arrays;      use GNATLLVM.Arrays;
with GNATLLVM.Codegen;     use GNATLLVM.Codegen;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLType;      use GNATLLVM.GLType;
with GNATLLVM.Records;     use GNATLLVM.Records;
with GNATLLVM.Subprograms; use GNATLLVM.Subprograms;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Utils;       use GNATLLVM.Utils;
with GNATLLVM.Wrapper;     use GNATLLVM.Wrapper;

package body GNATLLVM.DebugInfo is

   Debug_Compile_Unit  : Metadata_T;
   --  DICompileUnit metadata for the main compile unit

   type DI_File_Cache is array (Source_File_Index range <>) of Metadata_T;
   type DI_File_Cache_Access is access all DI_File_Cache;

   DI_Cache : DI_File_Cache_Access := null;

   --  We maintain a stack of debug info contexts, with the outermost
   --  context being global, then a subprogram, and then lexical blocks.
   --  We can only write line number information for code in the non-global
   --  scope.

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

   function Has_Local_Debug_Scope return Boolean is
     (Debug_Scope_Table.Last >= Debug_Scope_Low_Bound);
   --  Says whether we do or don't currently have a local scope.

   function Current_Debug_Scope return Metadata_T is
     ((if   Has_Local_Debug_Scope
      then Debug_Scope_Table.Table (Debug_Scope_Table.Last).Scope
      else Debug_Compile_Unit))
     with Post => Present (Current_Debug_Scope'Result);
   --  Current debug info scop, either global or local

   function Current_Debug_SFI return Source_File_Index is
     (Debug_Scope_Table.Table (Debug_Scope_Table.Last).SFI);
   --  Current debug info source file index

   Freeze_Pos_Level : Natural := 0;
   --  Current level of pushes of requests to freeze debug position

   function Create_Debug_Location (N : Node_Id) return Metadata_T
     with Pre  => Present (N),
          Post => Present (Create_Debug_Location'Result);
   --  Return debug metadata for a location

   Debug_Loc_Sloc  : Source_Ptr := No_Location;
   Debug_Loc_Scope : Metadata_T := No_Metadata_T;
   Debug_Loc       : Metadata_T := No_Metadata_T;
   --  One-entry cache for Create_Debug_Location

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Exp : aliased stdint_h.int64_t;

   begin
      --  If we're emitting debug info, set up everything we need to do  so.

      if Emit_Debug_Info then
         Add_Debug_Flags (Module);
         DI_Builder         := Create_DI_Builder (Module);
         Debug_Compile_Unit :=
           DI_Create_Compile_Unit
           (DI_Builder,
            (if   Ada_Version = Ada_83 then DWARF_Source_Language_Ada83
             else DWARF_Source_Language_Ada95),
            Get_Debug_File_Node (Main_Source_File), "GNAT/LLVM", 9,
            Code_Gen_Level /= Code_Gen_Level_None, "", 0, 0, "", 0,
            DWARF_Emission_Full, 0, False, False);
         Empty_DI_Expr      :=
           DI_Builder_Create_Expression (DI_Builder, Exp'Access, 0);
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
      --  If we haven't already created a cache of DIFile values, do so now

      if DI_Cache = null then
         DI_Cache :=
           new DI_File_Cache'(1 .. Last_Source_File => No_Metadata_T);
      end if;

      --  See if we previously made the entry for this file and return it
      --  if so.

      if DI_Cache (File) /= No_Metadata_T then
         return DI_Cache (File);
      end if;

      --  Otherwise, make a new DIFile entry and cache and return it

      declare
         Full_Name : constant String     :=
           Get_Name_String (Full_Debug_Name (File));
         Name      : constant String     :=
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
     (Func      : GL_Value;
      Def_Ident : Entity_Id;
      N         : Node_Id;
      Name      : String := "";
      Ext_Name  : String := "") return Metadata_T
   is
      Types      : constant Type_Array (1 .. 0) := (others => <>);
      S_Name     : constant String              :=
        (if Name /= "" then Name else Get_Name (Def_Ident));
      S_Ext_Name : constant String              :=
        (if Ext_Name /= "" then Ext_Name else Get_Ext_Name (Def_Ident));

   begin
      --  ??? We don't make the subprogram type from the types of the
      --  arguments because they may not match the actual args and
      --  it's tricky to get this right.

      if Emit_Debug_Info then
         declare
            Dyn_Scope_E   : constant Entity_Id  :=
              Enclosing_Subprogram_Scope (Def_Ident);
            File_Node     : constant Metadata_T :=
              Get_Debug_File_Node (Get_Source_File_Index (Sloc (N)));
            Sub_Type_Node : constant Metadata_T :=
              DI_Builder_Create_Subroutine_Type
              (DI_Builder, File_Node, Types'Address, 0, DI_Flag_Zero);
            Line_Number   : constant unsigned   :=
              unsigned (Get_Logical_Line_Number (Sloc (N)));
            Dyn_Scope     : constant Metadata_T :=
              (if   Present (Dyn_Scope_E)
                    and then Present (Get_Value (Dyn_Scope_E))
                    and then Present (Get_Subprogram (Get_Value (Dyn_Scope_E)))
               then Get_Subprogram (Get_Value (Dyn_Scope_E))
               else File_Node);
            Function_Node : constant Metadata_T :=
              DI_Create_Function
                (DI_Builder, Dyn_Scope, S_Name, S_Name'Length,
                 (if S_Ext_Name = S_Name then "" else S_Ext_Name),
                 (if S_Ext_Name = S_Name then 0  else S_Ext_Name'Length),
                 File_Node, Line_Number, Sub_Type_Node, False, True,
                 Line_Number, DI_Flag_Zero,
                 Code_Gen_Level /= Code_Gen_Level_None);

         begin
            Set_Subprogram (Func, Function_Node);
            return Function_Node;
         end;
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
           (SFI, DI_Builder_Create_Lexical_Block
              (DI_Builder, Current_Debug_Scope, Get_Debug_File_Node (SFI),
               unsigned (Get_Logical_Line_Number (Sloc (N))),
               unsigned (Get_Column_Number (Sloc (N)))));
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
   -- Create_Debug_Location --
   ---------------------------

   function Create_Debug_Location (N : Node_Id) return Metadata_T is
      S      : constant Source_Ptr := Sloc (N);
      Result : Metadata_T;

   begin
      --  If this is the same as our last request, return the result.
      --  Note that we can have multiple scopes at the same sloc (it can
      --  generate multiple subprograms).

      if S = Debug_Loc_Sloc and Current_Debug_Scope = Debug_Loc_Scope then
         return Debug_Loc;
      end if;

      --  Otherwise, make a new one, set up our cache, and return it

      Result          := DI_Builder_Create_Debug_Location
        (Context, unsigned (Get_Logical_Line_Number (S)),
         unsigned (Get_Column_Number (S)), Current_Debug_Scope, No_Metadata_T);
      Debug_Loc_Sloc  := S;
      Debug_Loc_Scope := Current_Debug_Scope;
      Debug_Loc       := Result;
      return Result;

   end Create_Debug_Location;

   ---------------------------
   -- Set_Debug_Pos_At_Node --
   ---------------------------

   procedure Set_Debug_Pos_At_Node (N : Node_Id) is
      SFI : constant Source_File_Index := Get_Source_File_Index (Sloc (N));

   begin
      if Emit_Debug_Info and then Has_Local_Debug_Scope
        and then Freeze_Pos_Level = 0 and then SFI = Current_Debug_SFI
      then
         Set_Current_Debug_Location
           (IR_Builder,
            Metadata_As_Value (Context, Create_Debug_Location (N)));
      end if;
   end Set_Debug_Pos_At_Node;

   ----------------------------
   -- Create_Debug_Type_Data --
   ----------------------------

   function Create_Debug_Type_Data (GT : GL_Type) return Metadata_T is
      TE          : constant Entity_Id  := Full_Etype (GT);
      Name        : constant String     := Get_Name (TE);
      T           : constant Type_T     := Type_Of (GT);
      Size        : constant UL         :=
        (if Type_Is_Sized (T) then UL (ULL'(Get_Type_Size (T))) else 0);
      Align       : constant unsigned   :=
        unsigned (Nat'(Get_Type_Alignment (GT)));
      S           : constant Source_Ptr := Sloc (TE);
      Result      : Metadata_T          := Get_Debug_Type (TE);

   begin
      --  If we already made debug info for this type, return it

      if Present (Result) then
         return Result;

      --  Do nothing if not emitting debug info

      elsif not Emit_Debug_Info then
         return No_Metadata_T;

      --  If we've seen this type as part of elaboration (e.g., an access
      --  type that points to itself) or if this is a nonnative type, this
      --  is an "unspecified" type.

      elsif Is_Being_Elaborated (TE) or else Is_Nonnative_Type (TE) then
         return DI_Create_Unspecified_Type (DI_Builder, Name, Name'Length);
      end if;

      --  Mark as being elaborated and create debug information based on
      --  the kind of the type.

      Set_Is_Being_Elaborated (TE, True);
      case Ekind (TE) is
         when Integer_Kind | Fixed_Point_Kind =>
            Result := DI_Create_Basic_Type
              (DI_Builder, Name, Name'Length, Size,
               (if    Size = UL (BPU)
                then  (if   Is_Unsigned_Type (TE) then DW_ATE_Unsigned_Char
                       else DW_ATE_Signed_Char)
                elsif Is_Unsigned_Type (TE) then DW_ATE_Unsigned
                else  DW_ATE_Signed),
               DI_Flag_Zero);

         when Float_Kind =>
            Result := DI_Create_Basic_Type (DI_Builder, Name, Name'Length,
                                            Size, DW_ATE_Float, DI_Flag_Zero);
         when Access_Kind =>

            --  Get the type info for what this points to.  If we have
            --  something, make our type.

            Result := DI_Create_Pointer_Type
              (DI_Builder,
               Create_Debug_Type_Data (Full_Designated_GL_Type (GT)),
               Size, Align, 0, Name, Name'Length);

         when Array_Kind =>

            --  Get the component type's data.  If it exists and this
            --  is of fixed size, get info for each of the bounds and
            --  make a description of the type.

            declare
               Num_Dims   : constant Nat        := Number_Dimensions (TE);
               Inner_Type : constant Metadata_T :=
                 Create_Debug_Type_Data (Full_Component_GL_Type (GT));
               Ranges     : Metadata_Array (0 .. Num_Dims - 1);

            begin
               for J in 0 .. Num_Dims - 1 loop
                  declare
                     Low_Bound  : constant GL_Value   :=
                       Get_Array_Bound (GT, J, True, No_GL_Value);
                     Length     : constant GL_Value   :=
                       Get_Array_Length (TE, J, No_GL_Value);

                  begin
                     Ranges (J) := DI_Builder_Get_Or_Create_Subrange
                       (DI_Builder,
                        int64_t (Get_Const_Int_Value (Low_Bound)),
                        int64_t (Get_Const_Int_Value (Length)));
                  end;
               end loop;

               Result := DI_Builder_Create_Array_Type
                 (DI_Builder, Size, Align, Inner_Type,
                  Ranges'Address, unsigned (Num_Dims));
            end;

         when Record_Kind =>

            declare
               package Member_Table is new Table.Table
                 (Table_Component_Type => Metadata_T,
                  Table_Index_Type     => Int,
                  Table_Low_Bound      => 1,
                  Table_Initial        => 20,
                  Table_Increment      => 5,
                  Table_Name           => "Member_Table");

               F : Entity_Id;

            begin
               --  Go through each field.  If we can make debug info for the
               --  type and the position and size are known and static,
               --  add that field as a member.

               F := First_Component_Or_Discriminant (TE);
               while Present (F) loop
                  if Known_Static_Component_Bit_Offset (F)
                    and then Known_Static_Esize (F)
                  then
                     declare
                        F_GT     : constant GL_Type    := Get_Field_Type (F);
                        Mem_Type : constant Metadata_T :=
                          Create_Debug_Type_Data (F_GT);
                        Name     : constant String     := Get_Name (F);
                        F_S      : constant Source_Ptr := Sloc (F);

                     begin
                        --  Add the member type to the table.  ???  Maybe
                        --  we should use
                        --  DI_Builder_Create_Bit_Field_Member_type when
                        --  appropriate.

                        Member_Table.Append
                          (DI_Create_Member_Type
                             (DI_Builder, No_Metadata_T, Name, Name'Length,
                              Get_Debug_File_Node
                                (Get_Source_File_Index (F_S)),
                              unsigned (Get_Logical_Line_Number (F_S)),
                              uint64_t (UI_To_ULL (Esize (F))),
                              unsigned (Nat'(Get_Type_Alignment (F_GT))),
                              uint64_t (UI_To_ULL (Component_Bit_Offset (F))),
                              (if   Is_Bitfield (F) then DI_Flag_Bit_Field
                               else DI_Flag_Zero),
                              Mem_Type));
                     end;
                  end if;

                  Next_Component_Or_Discriminant (F);
               end loop;

               Result := DI_Create_Struct_Type
                 (DI_Builder, No_Metadata_T, Name, Name'Length,
                  Get_Debug_File_Node (Get_Source_File_Index (S)),
                  unsigned (Get_Logical_Line_Number (S)),
                  Size, Align, DI_Flag_Zero, No_Metadata_T,
                  Member_Table.Table (1)'Address, unsigned (Member_Table.Last),
                  0, No_Metadata_T, "", 0);
            end;

         when Enumeration_Kind =>

            declare
               package Member_Table is new Table.Table
                 (Table_Component_Type => Metadata_T,
                  Table_Index_Type     => Int,
                  Table_Low_Bound      => 1,
                  Table_Initial        => 20,
                  Table_Increment      => 5,
                  Table_Name           => "Member_Table");

               Member : Entity_Id;

            begin
               Member := First_Literal (TE);
               while Present (Member) loop
                  Member_Table.Append (Create_Enumerator
                                         (DI_Builder, Get_Name (Member),
                                          UI_To_ULL (Enumeration_Rep (Member)),
                                          Enumeration_Rep (Member) >= 0));
                  Next_Literal (Member);
               end loop;

               Result := DI_Create_Enumeration_Type
                 (DI_Builder, Debug_Compile_Unit, Name, Name'Length,
                  Get_Debug_File_Node (Get_Source_File_Index (S)),
                  unsigned (Get_Logical_Line_Number (S)),
                  Size, Align, Member_Table.Table (1)'Address,
                  unsigned (Member_Table.Last), No_Metadata_T);
            end;

         when others =>
            Result :=
              DI_Create_Unspecified_Type (DI_Builder, Name, Name'Length);
      end case;

      --  Show no longer elaborating this type and save and return the result

      Set_Is_Being_Elaborated (TE, False);
      Set_Debug_Type (TE, Result);
      return Result;
   end Create_Debug_Type_Data;

   ---------------------------------------
   -- Create_Global_Variable_Debug_Data --
   ---------------------------------------

   procedure Create_Global_Variable_Debug_Data
     (Def_Ident : Entity_Id; V : GL_Value)
   is
      GT        : constant GL_Type    := Related_Type (V);
      Type_Data : constant Metadata_T := Create_Debug_Type_Data (GT);
      Name      : constant String     := Get_Name (Def_Ident);
      Ext_Name  : constant String     := Get_Ext_Name (Def_Ident);
      S         : constant Source_Ptr := Sloc (Def_Ident);

   begin
      --  ??? For globals, we only do something now if this is a normal
      --  reference to the data and if it's not imported.

      if Emit_Debug_Info and then Present (Type_Data)
        and then Relationship (V) = Reference
        and then Is_A_Global_Variable (V)
        and then not Is_Imported (Def_Ident)
      then
         Global_Set_Metadata
           (LLVM_Value (V), 0,
            DI_Create_Global_Variable_Expression
              (DI_Builder, Debug_Compile_Unit, Name, Name'Length,
               (if Ext_Name = Name then "" else Ext_Name),
               (if Ext_Name = Name then 0  else Ext_Name'Length),
               Get_Debug_File_Node (Get_Source_File_Index (S)),
               unsigned (Get_Logical_Line_Number (S)),
               Type_Data, False, Empty_DI_Expr, No_Metadata_T,
               unsigned (Nat'(Get_Type_Alignment (GT)) * BPU)));
      end if;
   end Create_Global_Variable_Debug_Data;

   --------------------------------------
   -- Create_Local_Variable_Debug_Data --
   --------------------------------------

   procedure Create_Local_Variable_Debug_Data
     (Def_Ident : Entity_Id; V : GL_Value; Arg_Num : Nat := 0)
   is
      GT        : constant GL_Type    := Related_Type (V);
      Type_Data : constant Metadata_T := Create_Debug_Type_Data (GT);
      Name      : constant String     := Get_Name (Def_Ident);
      Var_Data  : Metadata_T;

   begin
      --  ??? We nly support the simple case of where we have data or a
      --  reference to data.

      if Emit_Debug_Info and then Present (Type_Data)
        and then Relationship (V) in Reference | Data
      then
         if Arg_Num = 0 then
            Var_Data :=
              DI_Create_Auto_Variable
              (DI_Builder, Current_Debug_Scope, Name, Name'Length,
               Get_Debug_File_Node (Get_Source_File_Index (Sloc (Def_Ident))),
               unsigned (Get_Logical_Line_Number (Sloc (Def_Ident))),
               Type_Data, False, DI_Flag_Zero,
               unsigned (Nat'(Get_Type_Alignment (GT)) * BPU));
         else
            Var_Data :=
              DI_Create_Parameter_Variable
              (DI_Builder, Current_Debug_Scope, Name, Name'Length,
               unsigned (Arg_Num),
               Get_Debug_File_Node (Get_Source_File_Index (Sloc (Def_Ident))),
               unsigned (Get_Logical_Line_Number (Sloc (Def_Ident))),
               Type_Data, False, DI_Flag_Zero);
         end if;

         --  If this is a reference, insert a dbg.declare call.  Otherwise,
         --  a dbg.value call.  ???  However, there sees to be an LLVM issue
         --  when we do this for a zero-length array.

         if Is_Data (V) then
            if Get_Type_Size (V) /= Nat (0) then
               Discard (DI_Builder_Insert_Dbg_Value_At_End
                          (DI_Builder, LLVM_Value (V), Var_Data, Empty_DI_Expr,
                           Create_Debug_Location (Def_Ident),
                           Get_Insert_Block));
            end if;
         else
            Discard (DI_Builder_Insert_Declare_At_End
                       (DI_Builder, LLVM_Value (V), Var_Data, Empty_DI_Expr,
                        Create_Debug_Location (Def_Ident), Get_Insert_Block));
         end if;
      end if;
   end Create_Local_Variable_Debug_Data;

end GNATLLVM.DebugInfo;
