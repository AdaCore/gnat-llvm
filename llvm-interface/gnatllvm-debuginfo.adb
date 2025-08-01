------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2025, AdaCore                     --
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
with Sem_Util;   use Sem_Util;
with Sinput;     use Sinput;
with Stand;      use Stand;
with Table;      use Table;
with Uintp.LLVM; use Uintp.LLVM;
with Urealp;     use Urealp;

with GNATLLVM.Arrays;      use GNATLLVM.Arrays;
with GNATLLVM.Codegen;     use GNATLLVM.Codegen;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLType;      use GNATLLVM.GLType;
with GNATLLVM.Helper;      use GNATLLVM.Helper;
with GNATLLVM.MDType;      use GNATLLVM.MDType;
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
     (MD : Metadata_T; E : Entity_Id) return Metadata_T
   is
     (DI_Create_Pointer_Type (MD, ULL (Thin_Pointer_Size), Thin_Pointer_Size,
                              0, ""))
     with Pre  => Present (MD) and then Present (E),
          Post => Present (Create_Pointer_To'Result);
   --  Given MD, debug metadata for some type, create debug metadata for a
   --  pointer to that type. E and Suffix is used for naming the type.

   function Convert_Bound_To_Metadata
     (GT : GL_Type; Dimension : Nat; Lower_Bound : Boolean;
      Packed : Boolean; Bound_Type : GL_Type; E_Bound : Node_Id)
      return Metadata_T;
   --  Convert a type bound to metadata, handling some peculiarities.

   function Create_Array_Type (GT : GL_Type; Size : ULL; Align : Nat;
                               S : Source_Ptr) return Metadata_T
     with Pre => Present (GT),
          Post => Present (Create_Array_Type'Result);
   --  Create metadata corresponding to the array type GT.

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

   function Get_Scope_For (E : Entity_Id) return Metadata_T
     with Pre => Present (E);
   --  Return the LLVM metadata object that should be used for the
   --  scope of the entity.  Returns No_Metadata_T if it should be
   --  CU-scoped.

   function Get_Unqualified_Name (E : Entity_Id) return String
     with Pre => Present (E);
   --  Return the unqualified name of the entity.

   function Get_Possibly_Local_Name (E : Entity_Id) return String
     with Pre => Present (E);
   --  Return the correct DWARF name to use for the entity.  If the
   --  entity has CU scope, the full name is returned, but if the
   --  entity has a more local scope, just the unqualified name is
   --  returned.

   function Create_Global_Variable_Declaration (E : Entity_Id)
     return Metadata_T with Pre => Present (E);
   --  Return the LLVM declaration of a global variable representing
   --  E, creating it if necessary.

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

   -------------------
   -- Get_Scope_For --
   -------------------

   function Get_Scope_For (E : Entity_Id) return Metadata_T is
      S : Node_Id := Scope (E);
   begin
      if not Comes_From_Source (E) or not Types_Can_Have_Function_Scope then
         return No_Metadata_T;
      end if;
      while Present (S) loop
         --  For the time being only function scopes are considered.
         if Ekind (S) = E_Function or else Ekind (S) = E_Procedure then
            return Current_Debug_Scope;
         end if;
         S := Scope (S);
      end loop;
      return No_Metadata_T;
   end Get_Scope_For;

   --------------------------
   -- Get_Unqualified_Name --
   --------------------------

   function Get_Unqualified_Name (E : Entity_Id) return String is
      Buf : Bounded_String;
   begin
      Append_Unqualified (Buf, Chars (E));
      return +Buf;
   end Get_Unqualified_Name;

   -----------------------------
   -- Get_Possibly_Local_Name --
   -----------------------------

   function Get_Possibly_Local_Name (E : Entity_Id) return String is
      S : constant Metadata_T := Get_Scope_For (E);
   begin
      if S = No_Metadata_T or not Types_Can_Have_Function_Scope then
         return Get_Name (E);
      end if;
      return Get_Unqualified_Name (E);
   end Get_Possibly_Local_Name;

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
            Is_Main : constant Boolean := S_Ext_Name = "main";
            Function_Node : constant Metadata_T          :=
              DI_Create_Function
                (Dyn_Scope,
                 (if Is_Main then S_Ext_Name else S_Name),
                 (if S_Ext_Name = S_Name or else Is_Main
                  then ""
                  else S_Ext_Name),
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
            B_GT  : constant GL_Type := Array_Index_Sub_GT (GT, J);
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
        (No_Metadata_T, "",
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
        (No_Metadata_T, "",
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
        Create_Pointer_To (Bounds_MD, Full_Etype (GT));
      Comp_MD     : constant Metadata_T := Create_Type_Data (CT);
      P_Comp_MD   : Metadata_T;
      Field_MDs   : Metadata_Array (1 .. 2);
      Rec_Align   : Nat                 := Thin_Pointer_Size;
      Offset      : ULL                 := 0;
      Idx         : Nat                 := 1;
      Ranges      : Metadata_Array (0 .. 0);

   begin
      --  If we can't make data for the component type, we can't make
      --  data for the fat pointer.

      if No (Comp_MD) then
         return No_Metadata_T;
      end if;

      --  GDB expects P_ARRAY to have type pointer-to-array.  The
      --  bounds here do not matter.
      Ranges (0) := DI_Builder_Get_Or_Create_Subrange (DI_Builder, 0, 0);
      P_Comp_MD := Create_Array_Type_With_Name (DI_Builder, No_Metadata_T, "",
                                                No_Metadata_T, 0, 0, Align,
                                                Comp_MD, No_Metadata_T,
                                                Ranges);
      P_Comp_MD := Create_Pointer_To (P_Comp_MD, Full_Component_Type (DT));

      --  Add fields for pointers to bounds and component and create debug
      --  data for that structure.

      pragma Assert (Add_Field (Field_MDs, Idx, Offset, Rec_Align, "P_ARRAY",
                                S, Align, Size, MD => P_Comp_MD));
      pragma Assert (Add_Field (Field_MDs, Idx, Offset, Rec_Align, "P_BOUNDS",
                                S, Align, Size, MD => P_Bounds_MD));

      return DI_Create_Struct_Type
        (No_Metadata_T, "",
         Get_Debug_File_Node (Get_Source_File_Index (S)),
         Get_Physical_Line_Number (S), Offset, Align, DI_Flag_Zero,
         No_Metadata_T, Field_MDs, 0, No_Metadata_T, "");

   end Create_Fat_Pointer_Type_Data;

   -------------------------------
   -- Convert_Bound_To_Metadata --
   -------------------------------

   function Convert_Bound_To_Metadata
     (GT : GL_Type; Dimension : Nat; Lower_Bound : Boolean;
      Packed : Boolean; Bound_Type : GL_Type; E_Bound : Node_Id)
     return Metadata_T is
   begin
      if not Is_Nonnative_Type (Full_Etype (GT)) then
         declare
            Bound : constant GL_Value :=
              Get_Array_Bound (GT, Dimension, Lower_Bound, No_GL_Value,
                               For_Orig => Packed);
         begin
            if Is_Constant (Bound) then
               begin
                  --  Since LLVM only really has signed scalar types
                  --  (and then some unsigned operations), we may end
                  --  up with a type bound that is incorrectly (for
                  --  DWARF's purposes) sign-extended.  So for example
                  --  if an array uses Character as an index type, the
                  --  upper bound is 255, but this will show up as "i8
                  --  -1".  This function undoes the sign extension in
                  --  this scenario.
                  if Is_Unsigned_Type (Bound_Type) then
                     declare
                        Bitsize : constant Uint :=
                          UI_From_ULL (Get_Scalar_Bit_Size (Bound_Type));
                        Max     : constant Uint := 2**Bitsize;
                        Masked  : constant Uint
                           := UI_From_GL_Value (Bound) mod Max;
                     begin
                        return Constant_As_Metadata (Masked);
                     end;
                  else
                     return Value_As_Metadata (Bound);
                  end if;
               end;
            end if;
         end;
      end if;
      case Nkind (E_Bound) is
         when N_Identifier => Ident : declare
            MD : Metadata_T := Get_Debug_Metadata (Entity (E_Bound));
         begin
            if not Present (MD) then
               MD := Create_Global_Variable_Declaration (Entity (E_Bound));
            end if;
            return MD;
         end Ident;

         when N_Integer_Literal =>
            return Constant_As_Metadata (Intval (E_Bound));

         when others =>
            return No_Metadata_T;
      end case;
   end Convert_Bound_To_Metadata;

   -----------------------
   -- Create_Array_Type --
   -----------------------

   function Create_Array_Type (GT : GL_Type; Size : ULL; Align : Nat;
                               S : Source_Ptr) return Metadata_T
   is
      TE         : constant Void_Or_Type_Kind_Id := Full_Etype (GT);
      Is_Packed  : constant Boolean := Is_Packed_Array_Impl_Type (TE);
      Array_TE   : constant Type_Kind_Id :=
         (if Is_Packed
          then Full_Original_Array_Type (TE)
          else TE);
      Name       : constant String := Get_Possibly_Local_Name (Array_TE);
      Comp_TE    : constant Void_Or_Type_Kind_Id :=
         Get_Fullest_View (Component_Type (Array_TE));
      Comp_Ty    : constant GL_Type
         := Get_GL_Type (Comp_TE);
      Inner_Type : constant Metadata_T := Create_Type_Data (Comp_Ty);
      Ranges     : Metadata_Array (0 .. Number_Dimensions (Array_TE) - 1);
      Stride     : Metadata_T := No_Metadata_T;
      E_Index : Opt_N_Is_Index_Id := First_Index (Array_TE);
   begin
      if Is_Unconstrained_Array (Array_TE) then
         return DI_Create_Unspecified_Type (Name);
      end if;

      --  For arrays, get the component type's data.

      for J in Ranges'Range loop
         declare
            E_Range : constant N_Has_Bounds_Id :=
              Simplify_Range (E_Index);
            Index_Type : constant GL_Type :=
               (if Is_Packed
                then Original_Array_Index_GT (TE, J)
                else Array_Index_GT (Array_TE, J));
            Base_Type_Data : constant Metadata_T :=
               Create_Type_Data (Index_Type);
            Low_Exp : constant Metadata_T :=
              Convert_Bound_To_Metadata (GT, J, True, Is_Packed,
                                         Index_Type, Low_Bound (E_Range));
            High_Exp : constant Metadata_T :=
              Convert_Bound_To_Metadata (GT, J, False, Is_Packed,
                                         Index_Type, High_Bound (E_Range));
         begin
            if Low_Exp = No_Metadata_T or else High_Exp = No_Metadata_T then
               return DI_Create_Unspecified_Type (Name);
            end if;
            if J = 0 and then Component_Size (Array_TE) /= Esize (Comp_TE) then
               Stride := Const_64_As_Metadata (Component_Size (Array_TE));
            end if;
            Ranges (J) :=
              Create_Subrange_Type
                (DI_Builder, No_Metadata_T, "", No_Metadata_T,
                 No_Line_Number, 0, 0, DI_Flag_Zero,
                 Is_Unsigned_Type (Full_Etype (Index_Type)),
                 Base_Type_Data, Low_Exp, High_Exp, No_Metadata_T,
                 No_Metadata_T);

            Next_Index (E_Index);
         end;
      end loop;

      return Create_Array_Type_With_Name (DI_Builder, Get_Scope_For (Array_TE),
         Name, Get_Debug_File_Node (Get_Source_File_Index (S)),
         Get_Logical_Line_Number (S),
         Size, Align, Inner_Type, Stride, Ranges);
   end Create_Array_Type;

   ----------------------
   -- Create_Type_Data --
   ----------------------

   function Create_Type_Data (GT : GL_Type) return Metadata_T is
      TE          : constant Void_Or_Type_Kind_Id := Full_Etype (GT);
      Name        : constant String               := Get_Name (TE);
      T           : constant Type_T               := +Type_Of (GT);
      Size        : constant ULL                  :=
        (if Type_Is_Sized (T) then Get_Type_Size (T) else 0);
      Align       : constant Nat                  := Get_Type_Alignment (GT);
      S           : constant Source_Ptr           := Sloc (TE);
      Result      : Metadata_T                    := Get_Debug_Metadata (TE);

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

      elsif Is_Being_Elaborated (TE) then
         return DI_Create_Unspecified_Type (Name);

      elsif Is_Nonnative_Type (TE) then
         if Ekind (TE) in Record_Kind or
            else not Types_Can_Have_Dynamic_Offsets
         then
            return DI_Create_Unspecified_Type (Name);
         end if;
      end if;

      --  Mark as being elaborated and create debug information based on
      --  the kind of the type.

      Set_Is_Being_Elaborated (TE, True);
      case Ekind (TE) is

         when E_Enumeration_Subtype | E_Signed_Integer_Subtype
              | E_Modular_Integer_Subtype => Sub_Type :
            begin
               if Is_Packed_Array_Impl_Type (TE) then
                  Result := Create_Array_Type (GT, Size, Align, S);
               else
                  declare

                     Full_BT : constant Entity_Id :=
                        Get_Fullest_View (Base_Type (TE));
                     Full_Low : constant Uint
                        := Get_Uint_Value (Type_Low_Bound (Full_BT));
                     Full_High : constant Uint
                        := Get_Uint_Value (Type_High_Bound (Full_BT));

                     Base_Type_Data : constant Metadata_T :=
                        Create_Type_Data (Primitive_GL_Type (Full_BT));
                     Low : constant Uint
                        := Get_Uint_Value (Type_Low_Bound (TE));
                     Low_Cst : constant Metadata_T :=
                        (if Present (Low)
                         then Const_64_As_Metadata (Low)
                         else No_Metadata_T);
                     High : constant Uint
                        := Get_Uint_Value (Type_High_Bound (TE));
                     High_Cst : constant Metadata_T :=
                        (if Present (High)
                         then Const_64_As_Metadata (High)
                         else No_Metadata_T);

                  begin

                     if Present (Low) and then Present (Full_Low)
                        and then Present (High) and then Present (Full_High)
                        and then Low = Full_Low and then High = Full_High
                     then

                        --  If the underlying type is identical, just
                        --  return it rather than building a typedef.
                        if Name = Get_Name (Full_BT)
                        then
                           Result := Base_Type_Data;
                        else
                           Result :=
                             DI_Builder_Create_Typedef
                               (Base_Type_Data, Name,
                                Get_Debug_File_Node
                                  (Get_Source_File_Index
                                     (S)),
                                Get_Physical_Line_Number
                                  (S),
                                Get_Scope_For (TE), Align);
                        end if;

                     else

                        Result := Create_Subrange_Type
                          (DI_Builder, Get_Scope_For (TE),
                           Get_Possibly_Local_Name (TE),
                           Get_Debug_File_Node (Get_Source_File_Index (S)),
                           Get_Logical_Line_Number (S), Size, Align,
                           DI_Flag_Zero, Is_Unsigned_Type (TE),
                           Base_Type_Data, Low_Cst, High_Cst,
                           No_Metadata_T,
                           (if Has_Biased_Representation (TE)
                            then Low_Cst
                            else No_Metadata_T));

                     end if;
                  end;
               end if;
            end Sub_Type;

         --  For scalar, non-enumeration types, we create the corresponding
         --  debug type.

         when E_Signed_Integer_Type | E_Modular_Integer_Type =>
            Result := DI_Create_Basic_Type
              (Name, Size,
               (if Is_Unsigned_Type (TE) then DW_ATE_Unsigned
                else  DW_ATE_Signed),
               DI_Flag_Zero);

         when Fixed_Point_Kind => Fixed_Point : declare
            Small : constant Ureal := Small_Value (TE);
            Base : constant Int := Rbase (Small);
            Numer : constant Uint := Numerator (Small);
            Denom : constant Uint := Denominator (Small);
         begin
            if Numer = 1 and then (Base = 2 or Base = 10) then
               begin
                  if Base = 2 then
                     Result := Create_Binary_Fixed_Point_Type (DI_Builder,
                        Name, Size, Align, Is_Unsigned_Type (TE), -(+Denom));
                  else
                     Result := Create_Decimal_Fixed_Point_Type (DI_Builder,
                        Name, Size, Align, Is_Unsigned_Type (TE), -(+Denom));
                  end if;
               end;
            else
               Result := Create_Rational_Fixed_Point_Type (DI_Builder,
                  Name, Size, Align, Is_Unsigned_Type (TE),
                  Constant_As_Metadata (Norm_Num (Small)),
                  Constant_As_Metadata (Norm_Den (Small)));
            end if;
         end Fixed_Point;

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
                    DI_Create_Pointer_Type (Result, Size, Align, 0, "");

               end if;
            end if;

            --  GDB and GNAT have a convention that an access type is
            --  represented by a typedef in DWARF.
            if Present (Result) then
               Result := DI_Builder_Create_Typedef
                 (Result, Get_Possibly_Local_Name (TE),
                  Get_Debug_File_Node (Get_Source_File_Index (S)),
                  Get_Physical_Line_Number (S), Get_Scope_For (TE), Align);
            end if;

         when Array_Kind =>
            Result := Create_Array_Type (GT, Size, Align, S);

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

            Original_Type : constant Entity_Id :=
               (if Is_Packed (TE)
                then Etype (TE)
                elsif Ekind (TE) = E_Record_Subtype
                then Implementation_Base_Type (TE)
                else Get_Fullest_View (TE));

         begin
            F := First_Component_Or_Discriminant (TE);
            while Present (F) loop
               if Get_Fullest_View (Scope (Ancestor_Field (F)))
                  /= Original_Type
               then
                  --  Inherited component, so we can skip it here.
                  null;
               elsif Known_Static_Component_Bit_Offset (F)
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
                 (Get_Scope_For (TE), Get_Possibly_Local_Name (TE),
                  Get_Debug_File_Node (Get_Source_File_Index (S)),
                  Get_Physical_Line_Number (S), Size, Align, DI_Flag_Zero,
                  No_Metadata_T, Members, 0, No_Metadata_T, "");
            end;
         end Record_Type;

         --  For an enumeration type, make an enumerator metadata for each
         --  entry. The code below is a bit convoluted to avoid needing a
         --  UI_To_LLI function just for this purpose.

         when E_Enumeration_Type => Enumeration :
            begin
               --  Check the Etype here to handle a case like
               --  "type My_Character is new Character".
               if Etype (TE) = Standard_Character
                  or Etype (TE) = Standard_Wide_Character
                  or Etype (TE) = Standard_Wide_Wide_Character
               then
                  Result := DI_Create_Basic_Type
                    (Name, Size, DW_ATE_Unsigned_Char, DI_Flag_Zero);

               elsif TE = Standard_Boolean then
                  Result := DI_Create_Basic_Type
                    (Name, Size, DW_ATE_Boolean, DI_Flag_Zero);

               else
                  declare

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
                           UI  : constant Uint       :=
                             Enumeration_Rep (Member);
                           Val : constant LLI        :=
                             Const_Int_Get_S_Ext_Value
                                (Const_Int (Int_Ty (Uint_64), UI));
                           MD  : constant Metadata_T :=
                             DI_Create_Enumerator (Get_Name (Member), Val,
                                                   UI >= 0);

                        begin
                           Member_Table.Append (MD);
                           Next_Literal (Member);
                        end;
                     end loop;

                     declare
                        Members : Metadata_Array (1 .. Member_Table.Last);

                     begin
                        for J in Members'Range loop
                           Members (J) := Member_Table.Table (J);
                        end loop;

                        Result := DI_Create_Enumeration_Type
                          (Get_Scope_For (TE), Get_Possibly_Local_Name (TE),
                           Get_Debug_File_Node (Get_Source_File_Index (S)),
                           Get_Physical_Line_Number (S), Size, Align, Members,
                           No_Metadata_T);
                     end;
                  end;
               end if;
            end Enumeration;

         --  Use "unspecified" for every other kind

         when others =>
            Result := DI_Create_Unspecified_Type (Name);
      end case;

      --  Show no longer elaborating this type and save and return the result

      Set_Is_Being_Elaborated (TE, False);
      Set_Debug_Metadata (TE, Result);
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

      --  Handle bounds and bounds and data relationships, but don't deal
      --  with PATs because we can't easily get the bounds and the info
      --  wouldn't be correct anyway.

      elsif Base_R = Bounds and then not Is_Packed_Array_Impl_Type (GT) then
         return Create_Bounds_Type_Data (GT, Size);
      elsif Base_R = Bounds_And_Data
        and then not Is_Packed_Array_Impl_Type (GT)
      then
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
         declare
            MD : constant Metadata_T :=
              DI_Create_Global_Variable_Expression
                (Debug_Compile_Unit, Name,
                 (if Ext_Name = Name then "" else Ext_Name),
                 Get_Debug_File_Node (Get_Source_File_Index (S)),
                 Get_Physical_Line_Number (S), Type_Data, False, Empty_DI_Expr,
                 No_Metadata_T, Get_Type_Alignment (GT));
         begin
            Set_Debug_Metadata
              (E, DI_Global_Variable_Expression_Get_Variable (MD));
            Global_Set_Metadata (+V, 0, MD);
         end;
      end if;
   end Create_Global_Variable_Debug_Data;

   ----------------------------------------
   -- Create_Global_Variable_Declaration --
   ----------------------------------------

   function Create_Global_Variable_Declaration (E : Entity_Id)
     return Metadata_T
   is
      GT        : constant GL_Type    := Default_GL_Type (Etype (E));
      Type_Data : constant Metadata_T := Create_Type_Data (GT);
      Name      : constant String     := Get_Name     (E);
      Ext_Name  : constant String     := Get_Ext_Name (E);
      S         : constant Source_Ptr := Sloc         (E);
      MD : Metadata_T;

   begin
      pragma Assert (Emit_Debug_Info);
      pragma Assert (Present (Type_Data));

      MD := Create_Global_Variable_Declaration
        (DI_Builder, Debug_Compile_Unit, Name,
         (if Ext_Name = Name then "" else Ext_Name),
         Get_Debug_File_Node (Get_Source_File_Index (S)),
         Get_Physical_Line_Number (S), Type_Data, False, Empty_DI_Expr,
         No_Metadata_T, Get_Type_Alignment (GT));
      MD := DI_Global_Variable_Expression_Get_Variable (MD);
      Set_Debug_Metadata (E, MD);
      return MD;
   end Create_Global_Variable_Declaration;

   --------------------------------------
   -- Create_Local_Variable_Debug_Data --
   --------------------------------------

   procedure Create_Local_Variable_Debug_Data
     (E : Entity_Id; V : GL_Value; Arg_Num : Nat := 0)
   is
      GT        : constant GL_Type    := Related_Type (V);
      Type_Data : constant Metadata_T := Create_Type_Data (V);
      Name      : constant String     := Get_Unqualified_Name (E);
      Var_Data  : Metadata_T          := Get_Debug_Metadata (E);
      Flags     : constant DI_Flags_T :=
         (if Comes_From_Source (E)
          then DI_Flag_Zero
          else DI_Flag_Artificial);

   begin
      if Emit_Full_Debug_Info and then Present (Type_Data)
         and then not Is_A_Global_Variable (V)
      then
         if No (Var_Data) then
            if Arg_Num = 0 then
               Var_Data :=
                 DI_Create_Auto_Variable
                 (Current_Debug_Scope, Name,
                  Get_Debug_File_Node (Get_Source_File_Index (Sloc (E))),
                  Get_Physical_Line_Number (Sloc (E)), Type_Data, False,
                  Flags, Get_Type_Alignment (GT));

            else
               Var_Data :=
                 DI_Create_Parameter_Variable
                 (Current_Debug_Scope, Name, Arg_Num,
                  Get_Debug_File_Node (Get_Source_File_Index (Sloc (E))),
                  Get_Physical_Line_Number (Sloc (E)),
                  Type_Data, False, Flags);
            end if;
            Set_Debug_Metadata (E, Var_Data);
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

   --------------------------
   -- Add_Label_Debug_Info --
   --------------------------

   procedure Add_Label_Debug_Info (E : Entity_Id; B : Basic_Block_T) is
      Name      : constant String     := Get_Name (E);
      S         : constant Source_Ptr := Sloc         (E);
   begin
      if Emit_Debug_Info and then Comes_From_Source (E) then
         Create_And_Insert_Label
           (DI_Builder, Current_Debug_Scope, Name,
            Get_Debug_File_Node (Get_Source_File_Index (S)),
            Get_Physical_Line_Number (S),
            Create_Location (E), B);
      end if;
   end Add_Label_Debug_Info;

   procedure Import_Module (N : Node_Id) is
      E     : constant Entity_Id   := Entity (Name (N));
      S     : constant Source_Ptr  := Sloc (N);
      Name  : constant String      := Get_Name (E);
   begin
      if Emit_Debug_Info then
         Create_Import_Declarations
           (DI_Builder, Name, Debug_Compile_Unit,
            Get_Debug_File_Node (Get_Source_File_Index (S)),
            Get_Logical_Line_Number (S));
      end if;
   end Import_Module;

end GNATLLVM.DebugInfo;
