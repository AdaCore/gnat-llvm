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

with Opt;        use Opt;
with Sinput;     use Sinput;
with Stand;      use Stand;
with Table;      use Table;
with Uintp.LLVM; use Uintp.LLVM;

with GNATLLVM.Arrays;      use GNATLLVM.Arrays;
with GNATLLVM.Codegen;     use GNATLLVM.Codegen;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLType;      use GNATLLVM.GLType;
with GNATLLVM.Helper;      use GNATLLVM.Helper;
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

   package Debug_Scopes is new Table.Table
     (Table_Component_Type => Debug_Scope,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => Debug_Scope_Low_Bound,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Debug_Scopes");
   --  Table of debugging scopes. The last inserted scope point corresponds
   --  to the current scope.

   function Has_Local_Debug_Scope return Boolean is
     (Debug_Scopes.Last >= Debug_Scope_Low_Bound);
   --  Says whether we do or don't currently have a local scope

   function Current_Debug_Scope return Metadata_T is
     ((if   Has_Local_Debug_Scope
       then Debug_Scopes.Table (Debug_Scopes.Last).Scope
       else Debug_Compile_Unit))
     with Post => Present (Current_Debug_Scope'Result);
   --  Current debug info scop, either global or local

   function Current_Debug_SFI return Source_File_Index is
     (Debug_Scopes.Table (Debug_Scopes.Last).SFI);
   --  Current debug info source file index

   Freeze_Pos_Level : Natural := 0;
   --  Current level of pushes of requests to freeze debug position

   function Create_Location (N : Node_Id) return Metadata_T
     with Pre  => Present (N),
          Post => Present (Create_Location'Result);
   --  Return debug metadata for a location

   function Create_Pointer_To
     (MD : Metadata_T; E : Entity_Id; Suffix : String := "") return Metadata_T
   is
     (DI_Create_Pointer_Type (MD, ULL (Thin_Pointer_Size), Thin_Pointer_Size,
                              0, Get_Name (E, Suffix & "_RF")))
     with Pre  => Present (MD) and then Present (E),
          Post => Present (Create_Pointer_To'Result);
   --  Given MD, debug metadata for some type, create debug metadata for a
   --  pointer to that type. E and Suffix is used for naming the type.

   function Create_Type_Data (GT : GL_Type) return Metadata_T
     with Pre => Present (GT);
   --  Create metadata corresponding to the type of GT. Return
   --  No_Metadata_T if the type is too complex.

   function Create_Type_Data (V : GL_Value) return Metadata_T
     with Pre => Present (V);
   --  Create metadata for the type and relationship of R. Don't return
   --  anything if we don't know how to create the metadata.

   function Add_Field
     (Field_MDs : in out Metadata_Array;
      Idx       : in out Nat;
      Offset    : in out ULL;
      Rec_Align : in out Nat;
      Name      : String;
      S         : Source_Ptr;
      Align     : Nat;
      Size      : ULL;
      GT        : GL_Type    := No_GL_Type;
      MD        : Metadata_T := No_Metadata_T) return Boolean
   with Pre => Present (GT) or else Present (MD);
   --  Add debug information for one field with Name, S, Align, and either
   --  GT or MD to the Field_MDs, starting at Idx, which we update. Update
   --  Offset to point to the byte past this field and Rec_Align to reflect
   --  what alignment is required past this point. Return False if we
   --  aren't able to properly update Offset, for example because we have a
   --  variable-sized object.

   function Create_Bounds_Type_Data
     (GT : GL_Type; Size : out ULL) return Metadata_T
     with Pre  => Present (GT)
                  and then Is_Unconstrained_Array (Full_Base_Type (GT)),
          Post => Present (Create_Bounds_Type_Data'Result);
   --  Create debug information for the bounds of type GT and return the
   --  size of the resulting structure.

   function Create_Bounds_And_Data_Type_Data (GT : GL_Type) return Metadata_T
     with Pre  => Present (GT)
                  and then Is_Unconstrained_Array (Full_Base_Type (GT));
   --  Create debug information for the bounds and data of type GT if
   --  possible.

   function Create_Fat_Pointer_Type_Data (GT : GL_Type) return Metadata_T
     with Pre => Present (GT)
                 and then Is_Unconstrained_Array (Full_Designated_Type (GT));
   --  Create debug information for a fat pointer to GT. We cant't
   --  do this if we can't make debug information for the component type
   --  of the array type.

   Debug_Loc_Sloc  : Source_Ptr := No_Location;
   Debug_Loc_Scope : Metadata_T := No_Metadata_T;
   Debug_Loc       : Metadata_T := No_Metadata_T;
   --  One-entry cache for Create_Location

   ----------------------
   -- Push_Debug_Scope --
   ----------------------

   procedure Push_Debug_Scope (SFI : Source_File_Index; Scope : Metadata_T) is
   begin
      if Emit_Debug_Info then
         Debug_Scopes.Append ((SFI, Scope));
      end if;
   end Push_Debug_Scope;

   ---------------------
   -- Pop_Debug_Scope --
   ---------------------

   procedure Pop_Debug_Scope is
   begin
      if Emit_Debug_Info and then not Library_Level then
         Debug_Scopes.Decrement_Last;
      end if;
   end Pop_Debug_Scope;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Exp : aliased stdint_h.uint64_t;

   begin
      --  If we're emitting debug info, set up everything we need to do  so.

      if Emit_Debug_Info then
         Add_Debug_Flags (Module);
         DI_Builder         := Create_DI_Builder (Module);
         Debug_Compile_Unit :=
           DI_Create_Compile_Unit
           ((if   Ada_Version = Ada_83 then DWARF_Source_Language_Ada_83
             else DWARF_Source_Language_Ada_95),
            Get_Debug_File_Node (Our_Source_File), "GNAT/LLVM",
            Code_Gen_Level /= Code_Gen_Level_None, "", 0, "",
            DWARF_Emission_Full, 0, False, False, "", "");

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
           Get_Name_String (Full_Debug_Name   (File));
         File_Name : constant String     :=
           Get_Name_String (Debug_Source_Name (File));
         DIFile    : constant Metadata_T :=
           DI_Create_File
           (File_Name, Full_Name (1 .. Full_Name'Length - File_Name'Length));

      begin
         DI_Cache (File) := DIFile;
         return DIFile;
      end;
   end Get_Debug_File_Node;

   ---------------
   -- Add_Field --
   ---------------

   function Add_Field
     (Field_MDs : in out Metadata_Array;
      Idx       : in out Nat;
      Offset    : in out ULL;
      Rec_Align : in out Nat;
      Name      : String;
      S         : Source_Ptr;
      Align     : Nat;
      Size      : ULL;
      GT        : GL_Type    := No_GL_Type;
      MD        : Metadata_T := No_Metadata_T) return Boolean
   is
      Our_MD    : constant Metadata_T :=
        (if Present (GT) then Create_Type_Data (GT) else MD);
      Size_V : constant GL_Value   :=
        (if   No (GT) or else Is_Dynamic_Size (GT) then No_GL_Value
         else Get_Type_Size (GT));
      Our_Size  : constant ULL     :=
        (if   Present (Size_V) and then Is_A_Constant_Int (Size_V) then +Size_V
         else Size);

   begin
      --  If the size of this type isn't a constant, we can't do anything
      --  and can't continue since we don't know where things will be.

      if Present (GT)
        and then (No (Size_V) or else not Is_A_Constant_Int (Size_V))
      then
         return False;
      end if;

      --  Otherwise get the size of the type, round the offset to the
      --  alignment of this type, and update the type alignment.

      Rec_Align := Nat'Max (Rec_Align, Align);
      Offset    :=
        ((Offset + ULL (Align) - 1) / ULL (Align)) * ULL (Align);

      --  If we have debug info for this type, add the member type to
      --  the above fields.

      if Present (Our_MD) then
         Field_MDs (Idx) := DI_Create_Member_Type
           (No_Metadata_T, Name,
            Get_Debug_File_Node (Get_Source_File_Index (S)),
            Get_Physical_Line_Number (S), Size, Align, Offset, Our_MD);
         Idx := Idx + 1;
      end if;

      --  Allow for the size of this entry

      Offset := Offset + Our_Size;
      return True;
   end Add_Field;

   ----------------------------------
   -- Create_Subprogram_Debug_Info --
   ----------------------------------

   function Create_Subprogram_Debug_Info
     (Func     : GL_Value;
      N        : Node_Id;
      E        : Opt_Subprogram_Kind_Id := Empty;
      Name     : String                 := "";
      Ext_Name : String                 := "") return Metadata_T
   is
      function Create_Return_Debug_Info return Metadata_T
        with Pre => LRK in Struct_Out | Struct_Out_Subprog;
      --  Return the debug information for the return of this subprogram
      --  in the case where we return a structure consisting of the Out
      --  parameters of the subprogram and possibly its return value.

      RK         : constant Return_Kind            :=
        (if Present (E) then Get_Return_Kind (E) else None);
      LRK        : constant L_Ret_Kind             :=
        (if Present (E) then Get_L_Ret_Kind (E)  else Void);
      Ret_MD     : constant Metadata_T             :=
        (if   RK = None then No_Metadata_T
         else Create_Type_Data (Full_GL_Type (E)));
      Num_MDs     : constant Nat                   :=
        (if   Present (E) then (Number_In_Params (E) +
                                (if RK = Return_By_Parameter then 1 else 0))
         else 0);
      Types      : Metadata_Array (0 .. Num_MDs);
      S_Name     : constant String                 :=
        (if Name /= "" then Name else Get_Name (E));
      S_Ext_Name : constant String                 :=
        (if Ext_Name /= "" then Ext_Name else Get_Ext_Name (E));
      File_Node     : constant Metadata_T          :=
        (if   Emit_Debug_Info
         then Get_Debug_File_Node (Get_Source_File_Index (Sloc (N)))
         else No_Metadata_T);
      Line_Number   : constant Physical_Line_Number :=
        Get_Physical_Line_Number (Sloc (N));
      P          : Opt_Formal_Kind_Id              :=
        (if Present (E) then First_In_Param (E) else Empty);
      Idx        : Nat                          := 1;

      ------------------------------
      -- Create_Return_Debug_Info --
      ------------------------------

      function Create_Return_Debug_Info return Metadata_T is

         Num_Fields : constant Nat       :=
           Number_Out_Params (E) + (if LRK = Struct_Out_Subprog then 1 else 0);
         Rec_Align  : Nat                := BPU;
         Offset     : ULL                := 0;
         Field_MDs  : Metadata_Array (1 .. Num_Fields);
         Idx        : Nat                := Field_MDs'First;
         Formal     : Opt_Formal_Kind_Id := First_Out_Param (E);
         OK         : Boolean            := True;

      begin -- Start of processing for Create_Return_Debug_Info

         --  If the subprogram has a return value, add it to the list of fields

         if LRK = Struct_Out_Subprog then
            OK := Add_Field (Field_MDs, Idx, Offset, Rec_Align, "return_",
                             Sloc (E), Get_Type_Alignment (Full_GL_Type (E)),
                             0, GT => Full_GL_Type (E));
         end if;

         --  Now add each Out parameter

         while OK and then Present (Formal) loop
            OK := Add_Field (Field_MDs, Idx, Offset, Rec_Align,
                             Get_Name (Formal), Sloc (Formal),
                             Get_Type_Alignment (Full_GL_Type (Formal)), 0,
                             GT => Full_GL_Type (Formal));
            Next_Out_Param (Formal);
         end loop;

         --  Lastly, round the offset to the alignment to obtain the size,
         --  and return the debug info.

         Offset :=
           ((Offset + ULL (Rec_Align) - 1) / ULL (Rec_Align)) *
           ULL (Rec_Align);

         return DI_Create_Struct_Type
           (No_Metadata_T, S_Name & "_RET", File_Node, Line_Number, Offset,
            Rec_Align, DI_Flag_Zero, No_Metadata_T,
            Field_MDs (1 .. Idx - 1), 0, No_Metadata_T, "");

      end Create_Return_Debug_Info;

   begin -- Start of processing for Create_Subprogram_Debug_Info

      if Emit_Debug_Info then

         --  Collect the types of all the parameters, handling types passed
         --  by reference in a simplistic manner by just making a pointer
         --  type.

         while Present (P) loop
            declare
               MD : Metadata_T := Create_Type_Data (Full_GL_Type (P));

            begin
               if Present (MD) and then Param_Is_Reference (P) then
                  MD := Create_Pointer_To (MD, P);
               end if;

               Types (Idx) := MD;
               Idx         := Idx + 1;
               Next_In_Param (P);
            end;
         end loop;

         --  Next deal with the return

         if LRK = Out_Return then
            Types (0) := Create_Type_Data (Full_GL_Type (First_Out_Param (E)));
         elsif LRK in Struct_Out | Struct_Out_Subprog then
            Types (0) := Create_Return_Debug_Info;
         elsif No (Ret_MD) then
            Types (0) := No_Metadata_T;
         elsif LRK = Subprog_Return and then RK = RK_By_Reference then
            Types (0) := Create_Pointer_To (Ret_MD, E);
         elsif LRK = Subprog_Return and then RK = Value_Return then
            Types (0) := Ret_MD;
         elsif RK = Return_By_Parameter then
            Types (0) := No_Metadata_T;
            Types (Idx) := Create_Pointer_To (Ret_MD, E);
         else
            Types (0) := No_Metadata_T;
         end if;

         --  Now create and return the metadata

         declare
            Dyn_Scope_E   : constant Entity_Id           :=
              (if Present (E) then Enclosing_Subprogram_Scope (E) else Empty);
            Dyn_Scope_Val : constant GL_Value            :=
              (if   Present (Dyn_Scope_E) then Get_Value (Dyn_Scope_E)
               else No_GL_Value);
            Dyn_Scope     : constant Metadata_T          :=
              (if   Present (Dyn_Scope_Val)
               then Get_Subprogram_Debug_Metadata (Dyn_Scope_Val)
               else File_Node);
            Sub_Type_Node : constant Metadata_T          :=
              DI_Builder_Create_Subroutine_Type
              (File_Node, Types, DI_Flag_Zero);
            Function_Node : constant Metadata_T          :=
              DI_Create_Function
                (Dyn_Scope, S_Name,
                 (if S_Ext_Name = S_Name then "" else S_Ext_Name),
                 File_Node, Line_Number, Sub_Type_Node, False, True,
                 Line_Number, DI_Flag_Zero,
                 Code_Gen_Level /= Code_Gen_Level_None);

         begin
            Set_Subprogram_Debug_Metadata (Func, Function_Node);
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
              (Current_Debug_Scope, Get_Debug_File_Node (SFI),
               Get_Physical_Line_Number (Sloc (N)),
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
   -- Create_Location --
   ---------------------------

   function Create_Location (N : Node_Id) return Metadata_T is
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
        (Get_Physical_Line_Number (S), Get_Column_Number (S),
         Current_Debug_Scope, No_Metadata_T);
      Debug_Loc_Sloc  := S;
      Debug_Loc_Scope := Current_Debug_Scope;
      Debug_Loc       := Result;
      return Result;

   end Create_Location;

   ---------------------------
   -- Set_Debug_Pos_At_Node --
   ---------------------------

   procedure Set_Debug_Pos_At_Node (N : Node_Id) is
      SFI : constant Source_File_Index := Get_Source_File_Index (Sloc (N));

   begin
      if Emit_Debug_Info and then Has_Local_Debug_Scope
        and then not Is_Entity_Name (N)
        and then Freeze_Pos_Level = 0 and then SFI = Current_Debug_SFI
      then
         Set_Current_Debug_Location (Create_Location (N));
      end if;
   end Set_Debug_Pos_At_Node;

   -----------------------------
   -- Create_Bounds_Type_Data --
   -----------------------------

   function Create_Bounds_Type_Data
     (GT : GL_Type; Size : out ULL) return Metadata_T
   is
      Ndim      : constant Nat        := Number_Dimensions (GT);
      S         : constant Source_Ptr := Sloc (GT);
      Rec_Align : Nat                 := Get_Bound_Alignment (GT);
      Offset    : ULL                 := 0;
      Field_MDs : Metadata_Array (1 .. Ndim * 2);
      Idx       : Nat                 := Field_MDs'First;

   begin
      for J in 0 .. Ndim - 1 loop
         declare
            B_GT  : constant GL_Type := Array_Index_GT (GT, J);
            Align : constant Nat     := Get_Type_Alignment (B_GT);

         begin
            pragma Assert (Add_Field (Field_MDs, Idx, Offset, Rec_Align,
                                      "LB" & To_String (J), S, Align, 0,
                                      GT => B_GT));
            pragma Assert (Add_Field (Field_MDs, Idx, Offset, Rec_Align,
                                      "UB" & To_String (J), S, Align, 0,
                                      GT => B_GT));
         end;
      end loop;

      --  Compute the size taking into account the record alignment

      Size    :=
        ((Offset + ULL (Rec_Align) - 1) / ULL (Rec_Align)) * ULL (Rec_Align);

      return DI_Create_Struct_Type
        (No_Metadata_T, Get_Name (GT, "__XUB"),
         Get_Debug_File_Node (Get_Source_File_Index (S)),
         Get_Physical_Line_Number (S), Size, Rec_Align, DI_Flag_Zero,
         No_Metadata_T, Field_MDs (1 .. Idx - 1), 0, No_Metadata_T, "");

   end Create_Bounds_Type_Data;

   --------------------------------------
   -- Create_Bounds_And_Data_Type_Data --
   --------------------------------------

   function Create_Bounds_And_Data_Type_Data
     (GT : GL_Type) return Metadata_T
   is
      S           : constant Source_Ptr := Sloc (GT);
      Size_V      : constant GL_Value   :=
        (if Is_Dynamic_Size (GT) then No_GL_Value else Get_Type_Size (GT));
      Bound_Align : constant Nat        := Get_Bound_Alignment (GT);
      Data_Align  : constant Nat        := Get_Type_Alignment (GT);
      Align       : Nat                 := Nat'Max (Bound_Align, Data_Align);
      Offset      : ULL                 := 0;
      Idx         : Nat                 := 1;
      Data_MD     : constant Metadata_T := Create_Type_Data (GT);
      Bounds_Size : ULL;
      Bound_MD    : constant Metadata_T :=
          Create_Bounds_Type_Data (GT, Bounds_Size);
      Field_MDs   : Metadata_Array (1 .. 2);

   begin
      --  If GT is of variable size, we can't return debug info for it
      --  or if we have no debug info for GT (which will usually be because
      --  the size isn't constant.

      if No (Size_V) or else not Is_A_Constant_Int (Size_V)
        or else No (Data_MD)
      then
         return No_Metadata_T;
      end if;

      --  Bounds and data are two fields, the bounds and the data

      pragma Assert (Add_Field (Field_MDs, Idx, Offset, Align, "BOUNDS", S,
                                Bound_Align, Bounds_Size, MD => Bound_MD));
      pragma Assert (Add_Field (Field_MDs, Idx, Offset, Align, "ARRAY", S,
                                Data_Align, +Size_V, MD => Data_MD));

      return DI_Create_Struct_Type
        (No_Metadata_T, Get_Name (GT, "__XUT"),
         Get_Debug_File_Node (Get_Source_File_Index (S)),
         Get_Physical_Line_Number (S), Offset, Align, DI_Flag_Zero,
         No_Metadata_T, Field_MDs, 0, No_Metadata_T, "");

   end Create_Bounds_And_Data_Type_Data;

   ----------------------------------
   -- Create_Fat_Pointer_Type_Data --
   ----------------------------------

   function Create_Fat_Pointer_Type_Data (GT : GL_Type) return Metadata_T is
      S           : constant Source_Ptr := Sloc (GT);
      DT          : constant GL_Type    := Full_Designated_GL_Type (GT);
      CT          : constant GL_Type    := Full_Component_GL_Type (DT);
      Size        : constant ULL        := ULL (Thin_Pointer_Size);
      Align       : constant Nat        := Thin_Pointer_Size;
      Bounds_Size : ULL;
      Bounds_MD   : constant Metadata_T :=
        Create_Bounds_Type_Data (DT, Bounds_Size);
      P_Bounds_MD : constant Metadata_T :=
        Create_Pointer_To (Bounds_MD, Full_Etype (GT), "_B");
      Comp_MD     : constant Metadata_T := Create_Type_Data (CT);
      P_Comp_MD   : Metadata_T;
      Field_MDs   : Metadata_Array (1 .. 2);
      Rec_Align   : Nat                 := Thin_Pointer_Size;
      Offset      : ULL                 := 0;
      Idx         : Nat                 := 1;

   begin
      --  If we can't make data for the component type, we can't make
      --  data for the fat pointer.

      if No (Comp_MD) then
         return No_Metadata_T;
      end if;

      --  Add fields for pointers to bounds and component and create debug
      --  data for that structure.

      P_Comp_MD := Create_Pointer_To (Comp_MD, Full_Component_Type (DT));
      pragma Assert (Add_Field (Field_MDs, Idx, Offset, Rec_Align, "p_array",
                                S, Align, Size, MD => P_Comp_MD));
      pragma Assert (Add_Field (Field_MDs, Idx, Offset, Rec_Align, "p_bounds",
                                S, Align, Size, MD => P_Bounds_MD));

      return DI_Create_Struct_Type
        (No_Metadata_T, Get_Name (GT),
         Get_Debug_File_Node (Get_Source_File_Index (S)),
         Get_Physical_Line_Number (S), Offset, Align, DI_Flag_Zero,
         No_Metadata_T, Field_MDs, 0, No_Metadata_T, "");

   end Create_Fat_Pointer_Type_Data;

   ----------------------
   -- Create_Type_Data --
   ----------------------

   function Create_Type_Data (GT : GL_Type) return Metadata_T is
      TE          : constant Void_Or_Type_Kind_Id := Full_Etype (GT);
      Name        : constant String               := Get_Name (TE);
      T           : constant Type_T               := Type_Of (GT);
      Size        : constant ULL                  :=
        (if Type_Is_Sized (T) then Get_Type_Size (T) else 0);
      Align       : constant Nat                  := Get_Type_Alignment (GT);
      S           : constant Source_Ptr           := Sloc (TE);
      Result      : Metadata_T                    := Get_Debug_Type (TE);

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
         return DI_Create_Unspecified_Type (Name);
      end if;

      --  Mark as being elaborated and create debug information based on
      --  the kind of the type.

      Set_Is_Being_Elaborated (TE, True);
      case Ekind (TE) is

         --  For scalar, non-enumeration types, we create the corresponding
         --  debug type.

         when Integer_Kind | Fixed_Point_Kind =>
            Result := DI_Create_Basic_Type
              (Name, Size,
               (if    Size = UBPU
                then  (if   Is_Unsigned_Type (TE) then DW_ATE_Unsigned_Char
                       else DW_ATE_Signed_Char)
                elsif Is_Unsigned_Type (TE) then DW_ATE_Unsigned
                else  DW_ATE_Signed),
               DI_Flag_Zero);

         when Float_Kind =>
            Result := DI_Create_Basic_Type (Name, Size, DW_ATE_Float,
                                            DI_Flag_Zero);

         --  For access types, handle fat pointer specially. Otherwise,
         --  get the type info for what this points to. If we have
         --  something, make our type.

         when Access_Kind =>

            if Is_Unconstrained_Array (Full_Designated_Type (TE))
              and then Esize (TE) = Fat_Pointer_Size
            then
               Result := Create_Fat_Pointer_Type_Data (GT);
            else
               Result := Create_Type_Data (Full_Designated_GL_Type (GT));

               if Present (Result) then
                  Result :=
                    DI_Create_Pointer_Type (Result, Size, Align, 0, Name);
               end if;
            end if;

         --  For arrays, get the component type's data. If it exists and
         --  this is of fixed size, get info for each of the bounds and
         --  make a description of the type.

         when Array_Kind => Array_Type : declare

            Inner_Type : constant Metadata_T :=
              Create_Type_Data (Full_Component_GL_Type (GT));
            Ranges     : Metadata_Array (0 .. Number_Dimensions (TE) - 1);

         begin
            for J in Ranges'Range loop
               declare
                  Low_Bound  : constant GL_Value   :=
                    Get_Array_Bound (GT, J, True, No_GL_Value);
                  Length     : constant GL_Value   :=
                    Get_Array_Length (TE, J, No_GL_Value);

               begin
                  Ranges (J) := DI_Builder_Get_Or_Create_Subrange (+Low_Bound,
                                                                   +Length);
               end;
            end loop;

            Result := DI_Builder_Create_Array_Type (Size, Align, Inner_Type,
                                                    Ranges);
         end Array_Type;

         --  For records, go through each field. If we can make debug info
         --  for the type and the position and size are known and static,
         --  add that field as a member.

         when Record_Kind => Record_Type : declare

            package Member_Table is new Table.Table
              (Table_Component_Type => Metadata_T,
               Table_Index_Type     => Int,
               Table_Low_Bound      => 1,
               Table_Initial        => 20,
               Table_Increment      => 5,
               Table_Name           => "Member_Table");

            F : Opt_Record_Field_Kind_Id;

         begin
            F := First_Component_Or_Discriminant (TE);
            while Present (F) loop
               if Known_Static_Component_Bit_Offset (F)
                 and then Known_Static_Esize (F)
               then
                  declare
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
                     --  Add the member type to the table

                     Member_Table.Append (MD);
                  end;
               end if;

               Next_Component_Or_Discriminant (F);
            end loop;

            declare
               Members : Metadata_Array (1 .. Member_Table.Last);

            begin
               for J in Members'Range loop
                  Members (J) := Member_Table.Table (J);
               end loop;

               Result := DI_Create_Struct_Type
                 (No_Metadata_T, Name,
                  Get_Debug_File_Node (Get_Source_File_Index (S)),
                  Get_Physical_Line_Number (S), Size, Align, DI_Flag_Zero,
                  No_Metadata_T, Members, 0, No_Metadata_T, "");
            end;
         end Record_Type;

         --  For an enumeration type, make an enumerator metadata for each
         --  entry. The code below is a bit convoluted to avoid needing a
         --  UI_To_LLI function just for this purpose.

         when Enumeration_Kind => Enumeration : declare

            package Member_Table is new Table.Table
              (Table_Component_Type => Metadata_T,
               Table_Index_Type     => Int,
               Table_Low_Bound      => 1,
               Table_Initial        => 20,
               Table_Increment      => 5,
               Table_Name           => "Member_Table");

            Member : Opt_E_Enumeration_Literal_Id;

         begin
            Member := First_Literal (TE);
            while Present (Member) loop
               declare
                  UI  : constant Uint       := Enumeration_Rep (Member);
                  Val : constant LLI        :=
                    Const_Int_Get_S_Ext_Value (Const_Int (Int_Ty (Uint_64),
                                                             UI));
                  MD  : constant Metadata_T :=
                    DI_Create_Enumerator (Get_Name (Member), Val, UI >= 0);

               begin
                  Member_Table.Append (MD);
                  Next_Literal (Member);
               end;
            end loop;

            declare
               Members : Metadata_Array (1 .. Member_Table.Last);

            begin
               if TE = Standard_Character or TE = Standard_Wide_Character
                 or TE = Standard_Wide_Wide_Character
               then
                  Result := DI_Create_Basic_Type
                    (Name, Size, DW_ATE_Unsigned_Char, DI_Flag_Zero);
               else
                  for J in Members'Range loop
                     Members (J) := Member_Table.Table (J);
                  end loop;

                  Result := DI_Create_Enumeration_Type
                    (Debug_Compile_Unit, Name,
                     Get_Debug_File_Node (Get_Source_File_Index (S)),
                     Get_Physical_Line_Number (S), Size, Align, Members,
                     No_Metadata_T);
               end if;
            end;
         end Enumeration;

         --  Use "unspecified" for every other kind

         when others =>
            Result := DI_Create_Unspecified_Type (Name);
      end case;

      --  Show no longer elaborating this type and save and return the result

      Set_Is_Being_Elaborated (TE, False);
      Set_Debug_Type (TE, Result);
      return Result;
   end Create_Type_Data;

   ----------------------
   -- Create_Type_Data --
   ----------------------

   function Create_Type_Data (V : GL_Value) return Metadata_T is
      GT     : constant GL_Type         := Related_Type (V);
      R      : constant GL_Relationship := Relationship (V);
      Base_R : constant GL_Relationship :=
        (if Is_Reference (R) then Deref (R) else R);
      MD     : constant Metadata_T      := Create_Type_Data (GT);
      Size   : ULL;

   begin
      --  If we weren't able to get debug info for the underlying type
      --  or if this was normal data, return what we have.

      if No (MD) or else Base_R in Data | Boolean_Data then
         return MD;

      --  If what we started with was a thin pointer, treat it as a
      --  normal pointer since we can't access negative offsets.

      elsif R = Thin_Pointer then
         return MD;

      --  Handle bounds and bounds and data relationships

      elsif Base_R = Bounds then
         return Create_Bounds_Type_Data (GT, Size);
      elsif Base_R = Bounds_And_Data then
         return Create_Bounds_And_Data_Type_Data (GT);

      --  Otherwise, we don't (yet) support this.

      else
         return No_Metadata_T;
      end if;
   end Create_Type_Data;

   ---------------------------------------
   -- Create_Global_Variable_Debug_Data --
   ---------------------------------------

   procedure Create_Global_Variable_Debug_Data (E : Entity_Id; V : GL_Value)
   is
      GT        : constant GL_Type    := Related_Type (V);
      Type_Data : constant Metadata_T := Create_Type_Data (V);
      Name      : constant String     := Get_Name     (E);
      Ext_Name  : constant String     := Get_Ext_Name (E);
      S         : constant Source_Ptr := Sloc         (E);

   begin
      --  For globals, we only do something if it's not imported

      if Emit_Debug_Info and then Present (Type_Data)
        and then Is_A_Global_Variable (V) and then not Is_Imported (E)
      then
         Global_Set_Metadata
           (+V, 0,
            DI_Create_Global_Variable_Expression
              (Debug_Compile_Unit, Name,
               (if Ext_Name = Name then "" else Ext_Name),
               Get_Debug_File_Node (Get_Source_File_Index (S)),
               Get_Physical_Line_Number (S), Type_Data, False, Empty_DI_Expr,
               No_Metadata_T, Get_Type_Alignment (GT)));
      end if;
   end Create_Global_Variable_Debug_Data;

   --------------------------------------
   -- Create_Local_Variable_Debug_Data --
   --------------------------------------

   procedure Create_Local_Variable_Debug_Data
     (E : Entity_Id; V : GL_Value; Arg_Num : Nat := 0)
   is
      GT        : constant GL_Type    := Related_Type (V);
      Type_Data : constant Metadata_T := Create_Type_Data (V);
      Name      : constant String     := Get_Name (E);
      Var_Data  : Metadata_T;

   begin
      if Emit_Full_Debug_Info and then Present (Type_Data) then
         if Arg_Num = 0 then
            Var_Data :=
              DI_Create_Auto_Variable
              (Current_Debug_Scope, Name,
               Get_Debug_File_Node (Get_Source_File_Index (Sloc (E))),
               Get_Physical_Line_Number (Sloc (E)), Type_Data, False,
               DI_Flag_Zero, Get_Type_Alignment (GT));
         else
            Var_Data :=
              DI_Create_Parameter_Variable
              (Current_Debug_Scope, Name, Arg_Num,
               Get_Debug_File_Node (Get_Source_File_Index (Sloc (E))),
               Get_Physical_Line_Number (Sloc (E)),
               Type_Data, False, DI_Flag_Zero);
         end if;

         --  If this is a reference, insert a dbg.declare call. Otherwise,
         --  a dbg.value call. ??? However, there sees to be an LLVM issue
         --  when we do this for a zero-length array.

         if Is_Data (V) then
            if Get_Type_Size (V) /= Nat (0) then
               Discard (DI_Builder_Insert_Dbg_Value_At_End
                          (V, Var_Data, Empty_DI_Expr,
                           Create_Location (E), Get_Insert_Block));
            end if;
         else
            Discard (DI_Builder_Insert_Declare_At_End
                       (V, Var_Data, Empty_DI_Expr,
                        Create_Location (E), Get_Insert_Block));
         end if;
      end if;
   end Create_Local_Variable_Debug_Data;

end GNATLLVM.DebugInfo;
