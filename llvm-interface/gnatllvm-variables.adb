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

with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Conversion;
with System.Storage_Elements;    use System.Storage_Elements;

with Errout;   use Errout;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Table;    use Table;

with LLVM.Core;  use LLVM.Core;

with GNATLLVM.Arrays;      use GNATLLVM.Arrays;
with GNATLLVM.Blocks;      use GNATLLVM.Blocks;
with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.Exprs;       use GNATLLVM.Exprs;
with GNATLLVM.Records;     use GNATLLVM.Records;
with GNATLLVM.Subprograms; use GNATLLVM.Subprograms;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package body GNATLLVM.Variables is

   --  We sometime have a case where we have a pragma Export on one entity
   --  and a pragma Import for the same global name on a different entity.
   --  As long as both aren't simultaneously visible, there's no problem,
   --  but if we try to elaborate both of them, LLVM will detect the
   --  duplicate name and "disambiguate" it, which isn't what we want
   --  because they are, indeed, meant to refer to the same variable.  So
   --  we record here which entities correspond to the same external name.
   --  We also have to record internally-defined names that are defined in
   --  the library with matching external names.
   --
   --  Identifiers with interface names are relatively rare and duplications
   --  are rarer still, so we don't need to be overly concerned here with
   --  execution efficiency and trade that off against memory utilization
   --  and avoiding the complexity of hash tables.

   --  Make a table to record each string used in an interface name.
   --  Record the first entity we encountered it with and the index in
   --  Global_Dup_Value, if we've made one.

   type One_Interface_Name is record
      S     : String_Id;
      --  The identifier of the string representing the name

      E     : Entity_Id;
      --  An entity we've seen (the first)

      Index : Global_Dup_Value_Id;
   end record;

   package Interface_Names is new Table.Table
     (Table_Component_Type => One_Interface_Name,
      Table_Index_Type     => Interface_Names_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 10,
      Table_Name           => "Interface_Names_Table");

   type Global_Dup_Entry is record
      E     : Entity_Id;
      --  One entity that's part of a duplicate set

      Index : Global_Dup_Value_Id;
      --  Ordinal saying which duplicate this is; if two entries have this
      --  same index, they represent the same duplicated interface name.
   end record
     with Predicate => Present (E) and then Present (Index);

   package Global_Dup is new Table.Table
     (Table_Component_Type => Global_Dup_Entry,
      Table_Index_Type     => Global_Dup_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 4,
      Table_Name           => "Global_Dup_Table");

   package Global_Dup_Value is new Table.Table
     (Table_Component_Type => GL_Value,
      Table_Index_Type     => Global_Dup_Value_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 1,
      Table_Name           => "Global_Dup_Value_Table");

   function Find_Dup_Entry (E : Entity_Id) return Global_Dup_Value_Id
     with Pre => Present (E) and then not Is_Type (E);
   --  If E is present in the above table, returns the value of Index
   --  for that entry or 0 if not present.

   function Find_Dup_Entry (S : String) return Global_Dup_Value_Id;
   --  Similar, but by string (for builtins)

   function Get_Dup_Global_Is_Defined (E : Entity_Id) return Boolean
     with Pre => Present (E) and then not Is_Type (E);
   --  Return True if E corresponds to a duplicated interface name and one
   --  occurence of that name in the extended main unit is defining it.

   function String_Equal (L : String_Id; R : String) return Boolean;
   --  Compare a string with an entry in the string table

   function Has_Global_Name (E : Entity_Id) return Boolean
     with Pre => Present (E) and then not Is_Type (E);
   --  Return True if E may have a global name that we need to check for dups

   function Make_Global_Variable (Def_Ident : Entity_Id) return GL_Value
     with Pre  => Present (Def_Ident) and then not Is_Type (Def_Ident),
          Post => Present (Make_Global_Variable'Result);
   --  Create a global variable for Def_Ident.  Definition is true if we
   --  are doing this for a declaration.

   function Is_Static_Location (N : Node_Id) return Boolean
     with Pre => Present (N);
   --  Return True if N represent an object with constant address

   function Is_Static_Conversion (In_TE, Out_TE : Entity_Id) return Boolean
     with Pre => Is_Type (In_TE) and then Is_Type (Out_TE);
   --  Return True if we can statically convert from In_TE to Out_TE

   function Initialized_Value (E : Entity_Id) return Node_Id
     with Pre => Present (E);
   --  If E is an E_Constant that has an initializing expression, return it

   function Hash_Value_T (Val : Value_T) return Hash_Type;
   --  Convert a Value_T to a hash

   package Const_Map_P is new Ada.Containers.Hashed_Maps
     (Key_Type => Value_T,
      Element_Type => Value_T,
      Hash => Hash_Value_T,
      Equivalent_Keys => "=");
   use Const_Map_P;
   Const_Map : Map;
   --  Map the Value_T for a constant to the Value_T for the global
   --  variable containing that constant.  We do this at the Value_T level
   --  rather than the GL_Value level because we may want to interpret the
   --  same constant differently if they have different bounds.

   ------------------
   -- String_Equal --
   ------------------

   function String_Equal (L : String_Id; R : String) return Boolean is
      Len : constant Nat := String_Length (L);

   begin
      if Len /= R'Length then
         return False;
      else
         for J in 1 .. Len loop
            if Get_Character (Get_String_Char (L, J)) /= R (Integer (J)) then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end String_Equal;

   ---------------------
   -- Has_Global_Name --
   ---------------------

   function Has_Global_Name (E : Entity_Id) return Boolean is
   begin
      --  If there's an address clause, there's no global name

      if Present (Address_Clause (E)) then
         return False;

      --  If there's an Interace_Name, that's a global name

      elsif Present (Interface_Name (E)) then
         return True;

      --  The other case is an Exception in Standard

      else
         return Ekind (E) = E_Exception and then Sloc (E) <= Standard_Location;
      end if;

   end Has_Global_Name;

   --------------------
   -- Find_Dup_Entry --
   --------------------

   function Find_Dup_Entry (E : Entity_Id) return Global_Dup_Value_Id is
   begin
      --  Don't even to search if this doesn't have a global name

      if not Has_Global_Name (E) then
         return Empty_Global_Dup_Value_Id;
      end if;

      for J in Global_Dup_Id range 1 .. Global_Dup.Last loop
         if Global_Dup.Table (J).E = E then
            return Global_Dup.Table (J).Index;
         end if;
      end loop;

      return Empty_Global_Dup_Value_Id;
   end Find_Dup_Entry;

   --------------------
   -- Find_Dup_Entry --
   --------------------

   function Find_Dup_Entry (S : String) return Global_Dup_Value_Id is
   begin
      for J in Interface_Names_Id range 1 .. Interface_Names.Last loop
         if String_Equal (Interface_Names.Table (J).S, S) then
            return Interface_Names.Table (J).Index;
         end if;
      end loop;

      return Empty_Global_Dup_Value_Id;
   end Find_Dup_Entry;

   --------------------------
   -- Get_Dup_Global_Value --
   --------------------------

   function Get_Dup_Global_Value (E : Entity_Id) return GL_Value is
      Idx : constant Global_Dup_Value_Id := Find_Dup_Entry (E);

   begin
      return (if No (Idx) then No_GL_Value else Global_Dup_Value.Table (Idx));
   end Get_Dup_Global_Value;

   --------------------------
   -- Set_Dup_Global_Value --
   --------------------------

   procedure Set_Dup_Global_Value (E : Entity_Id; V : GL_Value) is
      Idx : constant Global_Dup_Value_Id := Find_Dup_Entry (E);

   begin
      if Present (Idx) then
         Global_Dup_Value.Table (Idx) := V;
      end if;
   end Set_Dup_Global_Value;

   --------------------------
   -- Get_Dup_Global_Value --
   --------------------------

   function Get_Dup_Global_Value (S : String) return GL_Value is
      Idx : constant Global_Dup_Value_Id := Find_Dup_Entry (S);

   begin
      return (if No (Idx) then No_GL_Value else Global_Dup_Value.Table (Idx));
   end Get_Dup_Global_Value;

   --------------------------
   -- Set_Dup_Global_Value --
   --------------------------

   procedure Set_Dup_Global_Value (S : String; V : GL_Value) is
      Idx : constant Global_Dup_Value_Id := Find_Dup_Entry (S);

   begin
      if Present (Idx) then
         Global_Dup_Value.Table (Idx) := V;
      end if;
   end Set_Dup_Global_Value;

   -------------------------------
   -- Get_Dup_Global_Is_Defined --
   -------------------------------

   function Get_Dup_Global_Is_Defined (E : Entity_Id) return Boolean is
      Idx : constant Global_Dup_Value_Id := Find_Dup_Entry (E);

   begin
      --  If this is not in the table, we have no external definition

      if No (Idx) then
         return False;
      end if;

      --  Otherwise, search for an entity in our duplicate class that's in
      --  the extended main unit and are being exported.

      return (for some J in Global_Dup_Id range 1 .. Global_Dup.Last
                => Global_Dup.Table (J).Index = Idx
                   and then In_Extended_Main_Code_Unit (Global_Dup.Table (J).E)
                   and then Is_Exported (Global_Dup.Table (J).E));
   end Get_Dup_Global_Is_Defined;

   --------------------------
   -- Register_Global_Name --
   --------------------------

   procedure Register_Global_Name (S : String) is
   begin
      Start_String;
      Store_String_Chars (S);
      Interface_Names.Append ((End_String, Empty, Empty_Global_Dup_Value_Id));
   end Register_Global_Name;

   -----------------------------------
   -- Detect_Duplicate_Global_Names --
   -----------------------------------

   procedure Detect_Duplicate_Global_Names is

      procedure Scan_Library_Item (U : Node_Id);
      --  Scan one library item looking for pragma Import or Export

      procedure Scan_All_Units is
         new Sem.Walk_Library_Items (Action => Scan_Library_Item);

      function Scan_One_Node (N : Node_Id) return Traverse_Result;
      --  Scan a single node looking for pragma Import or Export

      procedure Scan is new Traverse_Proc (Scan_One_Node);
      --  Used to scan most of a library unit looking for pragma Import/Export

      -----------------------
      -- Scan_Library_Item --
      -----------------------

      procedure Scan_Library_Item (U : Node_Id) is
         N : Node_Id;

      begin
         --  Scan the declarations and then the unit itself

         if Present (Parent (U)) then
            N := First (Declarations (Aux_Decls_Node (Parent (U))));
            while Present (N) loop
               Scan (N);
               Next (N);
            end loop;
         end if;

         Scan (U);
      end Scan_Library_Item;

      -------------------
      -- Scan_One_Node --
      -------------------

      function Scan_One_Node (N : Node_Id) return Traverse_Result is
         This_Str : String_Id;

      begin
         --  If we run into a stub, we have to search inside it because
         --  Library_Unit is a semantic, not syntactic, field.

         if Nkind (N) in N_Body_Stub then
            Scan (Library_Unit (N));

         --  Otherwise, check for cases where we have an interface name
         --  and process each of them to check for duplicates.

         elsif Nkind (N) = N_Defining_Identifier
           and then Ekind_In (N, E_Constant, E_Variable, E_Exception,
                              E_Function, E_Procedure, E_Package)
           and then Has_Global_Name (N)
         then
            --  See if this name is already in our table.  If it
            --  isn't, add it and indicate which node it was from, but
            --  then we're done since it's not a duplicate yet.  Handle
            --  both Interface_Name and exceptions in Standard.

            if Present (Interface_Name (N)) then
               This_Str := Strval (Interface_Name (N));
            else
               --  Must be exception in Standard

               Start_String;
               Store_String_Chars (Get_Name_String (Chars (N)));
               This_Str := End_String;
            end if;

            for J in 1 .. Interface_Names.Last loop
               if String_Equal (Interface_Names.Table (J).S, This_Str) then

                  --  But if it is present, we do have a duplicate.  If we've
                  --  already built a Global_Dup_Value entry, all we have to
                  --  do is make a new Global_Dup entry recording our entity.

                  if Present (Interface_Names.Table (J).Index) then
                     Global_Dup.Append ((N, Interface_Names.Table (J).Index));
                  else
                     --  Otherwise, make a new Global_Dup_Value entry, make
                     --  a Global_Dup entry for both this and the previous
                     --  entry, and indicate the Global_Dup_Value entry.

                     Global_Dup_Value.Append (No_GL_Value);
                     Interface_Names.Table (J).Index := Global_Dup_Value.Last;
                     Global_Dup.Append ((N, Global_Dup_Value.Last));
                     if Present (Interface_Names.Table (J).E) then
                        Global_Dup.Append ((Interface_Names.Table (J).E,
                                            Global_Dup_Value.Last));
                     end if;
                  end if;

                  return OK;
               end if;
            end loop;

            Interface_Names.Append ((This_Str, N, Empty_Global_Dup_Value_Id));
         end if;

         return OK;
      end Scan_One_Node;

   begin
      --  Start of processing for Detect_Duplicate_Global_Names

      Scan_All_Units;
      Detected_Duplicates := True;
   end Detect_Duplicate_Global_Names;

   ------------------------
   -- Is_Static_Location --
   ------------------------

   function Is_Static_Location (N : Node_Id) return Boolean is
      Index : Entity_Id;
      Expr  : Node_Id;

   begin
      case Nkind (N) is
         when N_Identifier | N_Expanded_Name =>
            return Is_Static_Location (Entity (N));

         when N_Defining_Identifier =>

            --  If this is at top level and has an address clause, we'll
            --  still allocate the variable, but set the initial value to
            --  be the address, so we can't consider this a static location
            --  in that case.  If we're not at top level and we do that, we
            --  could, but it's not worth the trouble because the
            --  distinction between static and non-static isn't that
            --  important then.

            return No (Address_Clause (N))
              and then (No (Renamed_Object (N))
                          or else Is_Static_Location (Renamed_Object (N)))
              and then Ekind (N) /= E_Enumeration_Literal
              and then (Ekind (Full_Etype (N)) = E_Void
                          or else not Is_Dynamic_Size (Full_Etype (N)))
              and then (Library_Level or else Is_Statically_Allocated (N)
                          or else not In_Extended_Main_Code_Unit (N));

         when N_Selected_Component =>
            return Is_Static_Location (Prefix (N));

         when N_Unchecked_Type_Conversion
            | N_Type_Conversion
            | N_Qualified_Expression =>
            return Is_Static_Location (Expression (N));

         when N_Indexed_Component =>

            --  Not static if prefix not static, a lower bound isn't static,
            --  or an expression isn't static.  It's also possible that the
            --  type of the prefix isn't actually an array in the case where
            --  we have a packed array type.  But then we know that it
            --  is static.

            if not Is_Static_Location (Prefix (N)) then
               return False;
            elsif not Is_Array_Type (Full_Etype (Prefix (N))) then
               return True;
            end if;

            Index := First_Index (Full_Etype (Prefix (N)));
            Expr  := First (Expressions (N));
            while Present (Index) loop
               exit when not Is_Static_Expression
                 (Low_Bound (Get_Dim_Range (Index)))
                 or else not Is_Static_Expression (Expr);
               Next_Index (Index);
               Next (Expr);
            end loop;

            return No (Index);

         when N_Slice =>

            --  Static if prefix is static and the lower bound and
            --  expression are static.

            return not Is_Static_Location (Prefix (N))
              and then (Is_Static_Expression
                          (Low_Bound (Get_Dim_Range (Discrete_Range (N)))))
              and then (not Is_Array_Type (Full_Etype (Prefix (N)))
                          or else (Is_Static_Expression
                                     (Low_Bound
                                        (Get_Dim_Range
                                           (First_Index
                                              (Full_Etype
                                                 (Prefix (N))))))));
         when others =>
            return False;
      end case;
   end Is_Static_Location;

   --------------------------
   -- Is_Static_Conversion --
   --------------------------

   function Is_Static_Conversion (In_TE, Out_TE : Entity_Id) return Boolean is
   begin
      --  We can do the conversion statically if both types are elementary

      return (Is_Elementary_Type (In_TE) and then Is_Elementary_Type (Out_TE))

        --  Or if they're the same type

        or else In_TE = Out_TE

        --  Or fixed-size record types with identical layout

        or else (Is_Record_Type (In_TE) and then Is_Loadable_Type (In_TE)
                   and then Is_Record_Type (Out_TE)
                   and then Is_Loadable_Type (Out_TE)
                   and then Is_Layout_Identical (In_TE, Out_TE))

        --  Or if neither type is dynamic and the LLVM types are the same

        or else (not Is_Dynamic_Size (In_TE)
                 and then not Is_Dynamic_Size (Out_TE)
                 and then Create_Type (In_TE) = Create_Type (Out_TE));

   end Is_Static_Conversion;

   -----------------------
   -- Is_Static_Address --
   -----------------------

   function Is_Static_Address (N : Node_Id) return Boolean is
      TE : constant Entity_Id := Full_Etype (N);

   begin
      case Nkind (N) is
         when N_Unchecked_Type_Conversion
            | N_Type_Conversion
            | N_Qualified_Expression =>
            return Is_Static_Conversion (Full_Etype (Expression (N)), TE)
              and then Is_Static_Address (Expression (N));

         when N_Attribute_Reference =>
            return (Get_Attribute_Id (Attribute_Name (N))
                      in Attribute_Address | Attribute_Access |
                        Attribute_Unchecked_Access | Attribute_Code_Address |
                        Attribute_Unrestricted_Access)
              and then Is_Static_Location (Prefix (N));

         when others =>
            return Compile_Time_Known_Value (N);
      end case;
   end Is_Static_Address;

   -----------------------
   -- Initialized_Value --
   -----------------------

   function Initialized_Value (E : Entity_Id) return Node_Id is
      Full_E : constant Entity_Id :=
        (if   Ekind (E) = E_Constant and then Present (Full_View (E))
         then Full_View (E) else E);
      Decl   : constant Node_Id   := Declaration_Node (Full_E);
      CV     : Node_Id;

   begin
      if No (Decl) or else not Is_True_Constant (E)
        or else Is_Volatile (E)
        or else (Nkind (Decl) = N_Object_Declaration
                   and then No_Initialization (Decl))
      then
         return Empty;
      else
         CV := Constant_Value (E);
         return (if   Present (CV) and then Is_No_Elab_Needed (CV)
                 then CV else Empty);
      end if;
   end Initialized_Value;

   -----------------------
   -- Is_No_Elab_Needed --
   -----------------------

   function Is_No_Elab_Needed (N : Node_Id) return Boolean is
      TE   : constant Entity_Id := Full_Etype (N);
      Expr : Node_Id;

   begin
      case Nkind (N) is
         when N_Aggregate | N_Extension_Aggregate =>
            if Is_Array_Type (TE) then

               --  We don't support constant aggregates of multi-dimensional
               --  Fortran arrays because it's too complex.  And we also
               --  can't make a constant aggregate of a dynamic size type
               --  even if the array has constant elements because we can't
               --  form the resulting GL_Value.  But be careful not to
               --  be confused by having the unconstrained array as the type
               --  of inner aggregates of multi-dimensional arrays.

               if (Number_Dimensions (TE) > 1
                     and then Convention (TE) = Convention_Fortran)
                 or else (Is_Constrained (TE) and then Is_Dynamic_Size (TE))
               then
                  return False;
               end if;

               Expr := First (Expressions (N));
               while Present (Expr) loop
                  exit when not Is_No_Elab_Needed (Expr);
                  Next (Expr);
               end loop;

            elsif Is_Record_Type (TE) then
               Expr := First (Component_Associations (N));
               while Present (Expr) loop
                  exit when not Box_Present (Expr)
                    and then not Is_No_Elab_Needed (Expression (Expr));
                  Next (Expr);
               end loop;
            else
               return False;
            end if;

            return No (Expr);

         when N_Binary_Op =>
            return not Do_Overflow_Check (N)
              and then Is_No_Elab_Needed (Left_Opnd (N))
              and then Is_No_Elab_Needed (Right_Opnd (N));

         when N_Unary_Op =>
            return not Do_Overflow_Check (N)
              and then Is_No_Elab_Needed (Right_Opnd (N));

         when N_Unchecked_Type_Conversion
            | N_Type_Conversion
            | N_Qualified_Expression =>
            if Nkind (N) = N_Type_Conversion
              and then Do_Overflow_Check (N)
            then
               return False;
            end if;

            return Is_Static_Conversion (Full_Etype (Expression (N)), TE)
              and then Is_No_Elab_Needed (Expression (N));

         when N_Attribute_Reference =>
            case Get_Attribute_Id (Attribute_Name (N)) is
               when Attribute_Size | Attribute_Object_Size
                  | Attribute_Value_Size | Attribute_Component_Size
                  | Attribute_Max_Size_In_Storage_Elements
                  | Attribute_Alignment | Attribute_Descriptor_Size =>

                  if Is_Entity_Name (Prefix (N))
                    and then Is_Type (Entity (Prefix (N)))
                  then
                     return not Is_Dynamic_Size (Full_Entity (Prefix (N)));
                  else

                  --  We have to be careful here because even though we
                  --  don't usually need to evaluate the Prefix to get
                  --  its size, we are required to, so it must be static
                  --  as well.

                     return not Is_Dynamic_Size (Full_Etype (Prefix (N)))
                       and then Is_No_Elab_Needed (Prefix (N));
                  end if;

               when others =>
                  return Is_Static_Address (N);
            end case;

         when N_Identifier | N_Expanded_Name =>
            return Is_No_Elab_Needed (Entity (N));

         --  If Emit_Identifier would walk into a constant value, we do as well

         when N_Defining_Identifier =>
            if Ekind (N) = E_Enumeration_Literal then
               return True;
            end if;

            declare
               CV : constant Node_Id := Initialized_Value (N);

            begin
               if Ekind (N) = E_Constant and then Present (Full_View (N))
                 and then No (Address_Clause (N))
               then
                  return Is_No_Elab_Needed (Full_View (N))
                    and then Is_Static_Conversion (Full_Etype (Full_View (N)),
                                                   TE);
               else
                  return Ekind (N) = E_Constant and then Present (CV)
                    and then Is_No_Elab_Needed (CV)
                    and then Is_Static_Conversion (Full_Etype (CV), TE);
               end if;
            end;

         when others =>

            --  If this is a compile-time constant or an address that we
            --  can evaluate, we don't need any elaboration.

            return Compile_Time_Known_Value (N)
              or else Is_Static_Address (N);
      end case;

   end Is_No_Elab_Needed;

   --------------------------
   -- Maybe_Promote_Alloca --
   --------------------------

   function Maybe_Promote_Alloca (T : Type_T) return Basic_Block_T is
      Current_BB         : constant Basic_Block_T := Get_Insert_Block;
      Threshold_In_Words : constant               := 4;
      Max_Promoted_Size  : constant ULL :=
        Get_LLVM_Type_Size (Void_Ptr_Type) * Threshold_In_Words;

   begin
      --  If this is small, promote it to the entry block by setting our
      --  position into that block and returning our current position.
      --  Otherwise, nothing to do here.

      if Present (Entry_Block_Allocas)
        and then Get_LLVM_Type_Size (T) <= Max_Promoted_Size
      then
         Set_Current_Position (Entry_Block_Allocas);
         return Current_BB;
      else
         return No_BB_T;
      end if;
   end Maybe_Promote_Alloca;

   ---------------------------
   -- Done_Promoting_Alloca --
   ---------------------------

   procedure Done_Promoting_Alloca (Alloca : Value_T; BB : Basic_Block_T) is
   begin
      --  If we promoted this alloca, update the position for allocas
      --  to after this one and restore the saved position.
      --  Otherwise, indicate that a stack save may be needed in the
      --  current block.

      if Present (BB) then
         Entry_Block_Allocas.Instr := Alloca;
         Position_Builder_At_End (BB);
      else
         Save_Stack_Pointer;
      end if;
   end Done_Promoting_Alloca;

   ---------------------
   -- Emit_Decl_Lists --
   ---------------------

   procedure Emit_Decl_Lists
     (List1, List2 : List_Id;
      End_List     : Node_Id := Empty;
      Pass1        : Boolean := True;
      Pass2        : Boolean := True)
   is
      type List_Array is array (1 .. 2) of List_Id;
      Lists     : constant List_Array := (1 => List1, 2 => List2);
      Def_Ident : Entity_Id;
      N         : Node_Id;

   begin
      if Pass1 then
         for J in 1 .. 2 loop
            if Present (Lists (J)) then
               N := First (Lists (J));
               while Present (N) and then N /= End_List loop

                  --  For package specs, we recurse inside the declarations
                  --  thus taking the two pass approach inside the boundary.

                  if Nkind (N) = N_Package_Declaration
                    and then (Nkind (Specification (N)) =
                                N_Package_Specification)
                  then
                     Emit_Decl_Lists (Visible_Declarations (Specification (N)),
                                      Private_Declarations (Specification (N)),
                                      Empty, True, False);

                     --  Similarly for any declarations in the actions
                     --  of a freeze node.

                  elsif Nkind (N) = N_Freeze_Entity then
                     Process_Freeze_Entity (N);
                     Emit_Decl_Lists (Actions (N), No_List, Empty,
                                      True, False);

                  --  Package bodies with freeze nodes get their
                  --  elaboration deferred until the freeze node, but the
                  --  code must be placed in the right place.  ??? We don't
                  --  yet know how to do this, so don't try.

                  --  elsif Nkind (N) = N_Package_Body
                  --    and then Present (Freeze_Node (Corresponding_Spec (N)))
                  --  then
                  --  null;
                  --
                  --  elsif Nkind (N) = N_Package_Body_Stub
                  --    and then Present (Library_Unit (N))
                  --    and then Present (Freeze_Node
                  --                        (Corresponding_Spec
                  --                           (Proper_Body
                  --                             (Unit (Library_Unit (N))))))
                  --  then
                  --     null;

                  --  We defer most subprogram bodies to the second pass

                  elsif Nkind (N) = N_Subprogram_Body then
                     if Acts_As_Spec (N) then
                        Def_Ident := Defining_Entity (N);

                        if not Ekind_In (Def_Ident, E_Generic_Procedure,
                                         E_Generic_Function)
                        then
                           Discard (Emit_Subprogram_Decl (N));
                        end if;
                     end if;

                  --  For bodies and stubs that act as their own specs, the
                  --  entity itself must be elaborated in the first pass,
                  --  because it may be used in other declarations.

                  elsif Nkind (N) = N_Subprogram_Body_Stub then
                     Def_Ident := Defining_Entity (Specification (N));

                     if not Ekind_In (Def_Ident, E_Subprogram_Body,
                                      E_Generic_Procedure, E_Generic_Function)
                     then
                        Discard (Emit_Subprogram_Decl (Specification (N)));
                     end if;

                  --  Concurrent stubs stand for the corresponding
                  --  subprogram bodies, which are deferred like other
                  --  bodies.

                  elsif Nkind_In (N, N_Task_Body_Stub, N_Protected_Body_Stub)
                  then
                     null;

                  --  Renamed subprograms may not be elaborated yet at this
                  --  point since renamings do not trigger freezing.  Wait
                  --  for the second pass to take care of them.

                  elsif Nkind (N) = N_Subprogram_Renaming_Declaration then
                     null;

                  else
                     Emit (N);
                  end if;

                  Next (N);
               end loop;
            end if;
         end loop;
      end if;

      --  Here we elaborate everything we deferred above except for package
      --  bodies, which are elaborated at their freeze nodes.  Note that we
      --  must also go inside things (package specs and freeze nodes) the
      --  first pass did.  */

      if Pass2 then
         for J in 1 .. 2 loop
            if Present (Lists (J)) then
               N := First (Lists (J));
               while Present (N) and then N /= End_List loop
                  if Nkind_In (N, N_Subprogram_Body, N_Subprogram_Body_Stub,
                               N_Task_Body_Stub, N_Protected_Body_Stub)
                  then
                     Emit (N);

                  elsif Nkind (N) = N_Package_Declaration
                    and then (Nkind (Specification (N))
                                = N_Package_Specification)
                  then
                     Emit_Decl_Lists (Visible_Declarations (Specification (N)),
                                      Private_Declarations (Specification (N)),
                                      Empty, False, True);

                  elsif Nkind (N) = N_Freeze_Entity then
                     Emit_Decl_Lists (Actions (N), No_List, Empty,
                                      False, True);

                  elsif Nkind (N) = N_Subprogram_Renaming_Declaration then
                     Emit (N);
                  end if;

                  Next (N);
               end loop;
            end if;
         end loop;
      end if;
   end Emit_Decl_Lists;

   ------------------
   -- Hash_Value_T --
   ------------------

   function Hash_Value_T (Val : Value_T) return Hash_Type is
      function UC is new Ada.Unchecked_Conversion (Value_T, System.Address);
   begin
      return Hash_Type ((To_Integer (UC (Val)) / (Val'Size / 8))
                        rem Hash_Type'Modulus);
   end Hash_Value_T;

   --------------------------
   -- Make_Global_Constant --
   --------------------------

   function Make_Global_Constant (V : GL_Value) return GL_Value is
      TE      : constant Entity_Id := Related_Type (V);
      In_V    : GL_Value           := V;
      Out_Val : Value_T;

   begin
      --  If we're making a constant for a string literal, we want
      --  both the bounds and data.

      if Ekind (TE) = E_String_Literal_Subtype then
         In_V := Get (In_V, Bounds_And_Data);
      end if;

      --  If we haven't already seen this value, make a constant for it

      if not Const_Map.Contains (LLVM_Value (In_V)) then
         Out_Val := Add_Global  (Module, Type_Of (In_V), "for-ref");
         Set_Initializer        (Out_Val, LLVM_Value (In_V));
         Set_Linkage            (Out_Val, Private_Linkage);
         Set_Global_Constant    (Out_Val, True);
         Set_Unnamed_Addr       (Out_Val, True);
         Const_Map.Insert       (LLVM_Value (In_V),  Out_Val);
      end if;

      --  Now make a GL_Value.  We do this here since different constant
      --  literals may have different types (i.e., bounds).

      return G (Const_Map.Element (LLVM_Value (In_V)),
                TE, Ref (Relationship (In_V)));
   end Make_Global_Constant;

   --------------------------
   -- Make_Global_Variable --
   --------------------------

   function Make_Global_Variable (Def_Ident : Entity_Id) return GL_Value is
      TE       : constant Entity_Id   := Full_Etype (Def_Ident);
      LLVM_Var : GL_Value             := Get_Dup_Global_Value (Def_Ident);
      Addr_Expr    : constant Node_Id :=
        (if Present (Address_Clause (Def_Ident))
         then Expression (Address_Clause (Def_Ident)) else Empty);
      Is_Ref   : constant Boolean     :=
        Present (Addr_Expr) or else Is_Dynamic_Size (TE)
          or else Present (Renamed_Object (Def_Ident));

   begin

      --  If we have an Interface name, see if this is a duplicate of
      --  another entity for which we've already made a global.

      if Present (LLVM_Var) then

         --  We could do this if both the previous and our entities agree
         --  on whether the type's size is dynamic, rather than requiring
         --  that neither be, but it's not worth the trouble.

         if Is_Double_Reference (LLVM_Var) or else Is_Dynamic_Size (TE)
           or else Type_Needs_Bounds (TE)
         then
            Error_Msg_N
              ("All uses of same interface name must have static size",
               Def_Ident);
            LLVM_Var := Emit_Undef (TE);
         else
            LLVM_Var := Convert_Ref (LLVM_Var, TE);
         end if;

      --  Otherwise, see if this is a simple renaming

      elsif Present (Renamed_Object (Def_Ident))
        and then Is_Static_Location (Renamed_Object (Def_Ident))
      then
         LLVM_Var := Emit_LValue (Renamed_Object (Def_Ident));

      --  Otherwise, make one here and properly set its linkage
      --  information.  Note that we don't set External_Linkage since
      --  that's the default if there's no initializer.

      else
         LLVM_Var := Add_Global
           (TE, Get_Ext_Name (Def_Ident), Need_Reference => Is_Ref);
         Set_Thread_Local (LLVM_Var,
                           Has_Pragma_Thread_Local_Storage (Def_Ident));

         if not Is_Public (Def_Ident) then
            Set_Linkage (LLVM_Var, Internal_Linkage);
         end if;

         Set_Dup_Global_Value (Def_Ident, LLVM_Var);
      end if;

      --  Now save the value we've made for this variable

      Set_Value (Def_Ident, LLVM_Var);
      return LLVM_Var;

   end Make_Global_Variable;

   ----------------------
   -- Emit_Declaration --
   ----------------------

   procedure Emit_Declaration
     (N : Node_Id; For_Freeze_Entity : Boolean := False)
   is
      Def_Ident    : constant Node_Id   := Defining_Identifier (N);
      --  Identifier being defined

      Full_Ident   : constant Node_Id    :=
        (if   Ekind (Def_Ident) = E_Constant
           and then Present (Full_View (Def_Ident))
         then Full_View (Def_Ident) else Def_Ident);
      --  Identifier to use to find the initializing expression

      No_Init      : constant Boolean   :=
        Nkind (N) = N_Object_Declaration and then No_Initialization (N);
      --  True if we aren't to initialize this object (ignore expression)

      Expr         : Node_Id            :=
        (if No_Init then Empty else Expression (Parent (Full_Ident)));
      --  Initializing expression, if Present and we are to use one

      TE           : constant Entity_Id :=
        (if   Ekind (Etype (Def_Ident)) = E_Class_Wide_Type
              and then Present (Expr)
              and then Nkind (Expr) = N_Qualified_Expression
         then Full_Etype (Expression (Expr)) else Full_Etype (Def_Ident));
      --  Type to use for allocation.  Normally, the type of the identifier
      --  unless we have a qualified expression initializing a class wide
      --  type.

      Max_Size     : constant Boolean   := Is_Unconstrained_Record (TE);
      --  True if our allocation should be of the maximum size.

      Addr_Expr    : constant Node_Id   :=
        (if   Present (Address_Clause (Def_Ident))
         then Expression (Address_Clause (Def_Ident)) else Empty);
      --  Expression to use for the address, if Present

      Is_External  : constant Boolean   :=
        Is_Imported (Def_Ident) and then No (Addr_Expr)
          and then not Get_Dup_Global_Is_Defined (Def_Ident);
      --  True if variable is not defined in this unit

      Is_Ref       : constant Boolean   :=
        Present (Addr_Expr) or else Is_Dynamic_Size (TE);
      --  True if we need to use an indirection for this variable

      Value        : GL_Value           :=
        (if Present (Expr) then Get_Value (Expr) else No_GL_Value);
      --  Any value that we've previously saved for an initializing expression

      Addr         : GL_Value           :=
        (if Present (Addr_Expr) then Get_Value (Addr_Expr) else No_GL_Value);
      --  Likewise for address

      Copied       : Boolean            := False;
      --  True if we've copied the value to the variable

      Set_Init     : Boolean            := False;
      --  True if we've made an initializer for a static variable that we're
      --  defining.

      LLVM_Var     : GL_Value           := Get_Value (Def_Ident);
      --  The LLVM value for the variable

   begin

      --  Nothing to do if this is a debug renaming type

      if TE = Standard_Debug_Renaming_Type then
         return;
      end if;

      --  Object declarations are variables either allocated on the
      --  stack (local) or global.  For globals, we operate in two phases:
      --  first we allocate the global itself, which may include some static
      --  initialization, and then do any required dynamic operations,
      --  which may include setting an address, allocating memory from the
      --  heap, and/or actually setting an initializing value.  If we're at
      --  library level, we have to do the dynamic operations in an elab
      --  proc, but, if not (if we have something to be statically allocated),
      --  we do it inline.
      --
      --  Ignore deferred constant definitions without address Clause since
      --  they are processed fully in the front-end.  If No_Initialization
      --  is set, this is not a deferred constant but a constant whose
      --  value is built manually.  And constants that are renamings are
      --  handled like variables.

      if Full_Ident /= Def_Ident and then No (Addr_Expr) and then not No_Init
        and then No (Renamed_Object (Def_Ident))
      then
         return;

      --  If we already have a saved value, but are neither in an elab
      --  proc or at library level, it must be necause this is a Full_View
      --  of a previous deferred constant.

      elsif Present (LLVM_Var) and not In_Elab_Proc and not Library_Level then
         return;

      --  If this entity has a freeze node and we're not currently
      --  processing the freeze node, all we do is evaluate the
      --  initial expression, if there is one and it's not something
      --  we can evaluate statically.  Otherwise, see if we've already
      --  evaluated that expression and get the value.

      elsif Present (Freeze_Node (Def_Ident))
        and then not For_Freeze_Entity and then not In_Elab_Proc
      then

         --  If we have a Full_View, we may see that declaration
         --  before our freeze node, so set a dummy value for it so we
         --  can detect that.

         if Full_Ident /= Def_Ident then
            Set_Value (Full_Ident, Emit_Undef (TE));
         end if;

         if Present (Expr)
           and then (not Is_No_Elab_Needed (Expr)
                       or else not Is_Static_Conversion
                       (Full_Etype (Expr), TE))
         then
            if Library_Level then
               Add_To_Elab_Proc (Expr, For_Type => TE);
            else
               Set_Value (Expr, Emit_Convert_Value (Expr, TE));
            end if;
         end if;

         return;
      end if;

      --  Handle top-level declarations or ones that need to be treated
      --  that way unless if we've already made the item (e.g., if we're
      --  in the elab proc).

      if (Library_Level or else Is_Statically_Allocated (Def_Ident)
            or else Is_External)
        and then No (LLVM_Var)
      then
         pragma Assert (not In_Elab_Proc);

         LLVM_Var := Make_Global_Variable (Def_Ident);

         --  If there's an Address clause with a static address, we can
         --  convert it to a pointer to us and make it a static
         --  initializer.  Otherwise, we have to take care of this in the
         --  elaboration proc if at library level.

         if Present (Addr_Expr) then
            if Is_Static_Address (Addr_Expr) then
               if No (Addr) then
                  Addr := Emit_Expression (Addr_Expr);
               end if;

               Set_Initializer (LLVM_Var, Int_To_Ref (Addr,  TE));
               Set_Init := True;
            elsif Library_Level then
               Add_To_Elab_Proc (N);
            end if;
         end if;

         --  If this is an object of dynamic size, we have to take care
         --  of the allocation in the elab proc if at library level.

         if Library_Level and then not Is_External
           and then Is_Dynamic_Size (TE)
         then
            Add_To_Elab_Proc (N);
         end if;

         --  If we have an initial value, we can set an initializer if this
         --  is a compile-time known expression, we have the actual global,
         --  not a type-converted value, and the variable is not of a
         --  dynamic size or has an address clause.
         --
         --  Note that the code below relies on the fact that if we execute
         --  this case, we won't be added to the elab proc for any other
         --  reason because if we are, we may do both a static and run-time
         --  initialization.

         if Present (Expr) then
            if Is_No_Elab_Needed (Expr)
              and then Is_A_Global_Variable (LLVM_Var)
              and then not Is_Dynamic_Size (TE) and then No (Addr_Expr)
              and then Is_Static_Conversion (Full_Etype (Expr), TE)
            then
               if No (Value) then
                  Value := Emit_Convert_Value (Expr, TE);
               end if;

               if Type_Needs_Bounds (TE) then
                  Value := Get (Value, Bounds_And_Data);
               end if;

               Set_Initializer (LLVM_Var, Value);
               Set_Init := True;
               Copied   := True;
            elsif Library_Level then
               Add_To_Elab_Proc (N);
            end if;

         --  If this is a constrained subtype for a unconstrained
         --  actual array subtype of fixed size, this is aliased, and
         --  we have no expression.  In that case, we still have to
         --  initialize the bounds.

         elsif Type_Needs_Bounds (TE) and then not Is_Dynamic_Size (TE) then
            Set_Initializer (LLVM_Var, Get (Get_Undef_Relationship (TE, Data),
                                            Bounds_And_Data));
            Set_Init := True;
            Copied   := True;
         end if;

         --  If we haven't already set an initializing expression and
         --  this is not an external or something we've already defined,
         --  set one to null to indicate that this is being defined.

         if not Set_Init and then not Is_External
           and then Is_A_Global_Variable (LLVM_Var)
         then
            Set_Initializer (LLVM_Var,
                             (if Is_Ref then Const_Null_Ref (TE)
                              else Const_Null_Alloc (TE)));
         end if;
      end if;

      --  If we're at library level and not in an elab proc, we can't do
      --  anything else now.

      if Library_Level and then not In_Elab_Proc then
         return;
      end if;

      --  If we're making an object of classwide type, strip off any
      --  conversions, since we have to ignore its type when doing the
      --  initializing.

      if Present (Expr) and then Is_Class_Wide_Equivalent_Type (TE) then
         Expr := Strip_Conversions (Expr);
      end if;

      --  Evaluate any expression for the address clause

      if Present (Addr_Expr) then
         if No (Addr) then
            Addr := Emit_Expression (Addr_Expr);
         end if;

         --  We have a special case here: if this would normally be
         --  allocated with bounds, the address points to the actual data,
         --  not the bounds, so the bounds get stored at an address below
         --  the data.

         declare
            R : GL_Relationship := Relationship_For_Alloc (TE);

         begin
            if R = Reference_To_Bounds_And_Data then
               R := Thin_Pointer;
            end if;

            Addr := Int_To_Relationship (Addr, TE, R);
         end;
      end if;

      --  If we've already gotten a value for the address of this entity,
      --  fetch it.  If a non-constant address was specified, set the the
      --  address of the entity to that address.  Otherwise, if the
      --  variable is of dynamic size, do the allocation here, copying any
      --  initializing expression.

      if Present (LLVM_Var) then
         if Present (Addr) and then not Is_Static_Address (Addr_Expr) then
            Store (Addr, LLVM_Var);
         elsif Is_Dynamic_Size (TE) then
            Store (Get (Heap_Allocate_For_Type (TE, TE, Value, Expr,
                                                Max_Size => Max_Size),
                        Any_Reference),
                   LLVM_Var);
            Copied := True;
         end if;

      --  Otherwise, if we have an address, that's what we use

      elsif Present (Addr) then
         LLVM_Var := Addr;

      --  If this is a true constant, we can just use the expression that
      --  computed the constant as the value, once converted to the proper
      --  type.  If the value is a Reference, it may be to something that's
      --  not constant, so we actually have to allocate our entity and copy
      --  into it.

      elsif Is_True_Constant (Def_Ident) and then not Is_Volatile (Def_Ident)
        and then (Present (Expr) or else Present (Value))
      then
         if No (Value) then
            Value := Emit_Conversion (Expr, TE);
         end if;

         if Is_Elementary_Type (TE) and then not Is_Packed_Array_Impl_Type (TE)
         then
            LLVM_Var := Get (Value, Data);
            Copied := True;
         elsif Is_Data (Value) then
            LLVM_Var := Value;
            Copied := True;
         end if;

         --  If this is an unnamed operation, set its name to that of our
         --  variable to make the code easier to read.

         if Present (LLVM_Var) and then Get_Value_Name (LLVM_Var) = "" then
            Set_Value_Name (LLVM_Var, Get_Name (Def_Ident));
         end if;
      end if;

      --  Otherwise, if we still haven't made a variable, allocate it
      --  on the stack, copying in any value.

      if No (LLVM_Var) then
         LLVM_Var := Allocate_For_Type (TE, TE, Def_Ident, Value, Expr,
                                        Name     => Get_Name (Def_Ident),
                                        Max_Size => Max_Size);
         Copied := True;
      end if;

      --  If we haven't already set the value, set it now

      if not Has_Value (Def_Ident) then
         Set_Value (Def_Ident, LLVM_Var);
      end if;

      --  If we haven't already copied in any initializing expression, do
      --  that now.

      if not Copied and then (Present (Expr) or else Present (Value)) then
         Emit_Assignment (Get (LLVM_Var, Any_Reference), Expr, Value);
      end if;
   end Emit_Declaration;

   -------------------------------
   -- Emit_Renaming_Declaration --
   -------------------------------

   procedure Emit_Renaming_Declaration (N : Node_Id) is
      Def_Ident : constant Entity_Id := Defining_Identifier (N);
      TE        : constant Entity_Id := Full_Etype (Def_Ident);
      LLVM_Var  : GL_Value;

   begin
      --  If this is just a macro substitution by the front end, omit
      --  the declaration.  ??? We may be able to write debugging info
      --  for this object, but then again, maybe we can't (e.g. packed
      --  array slice case).

      if Is_Renaming_Of_Object (Def_Ident) then
         return;

      --  If we've already defined this object, it means that we must be
      --  in an elab proc seeing this for the second time, which means
      --  that we have to set its address.

      elsif Has_Value (Def_Ident) then
         pragma Assert (In_Elab_Proc);

         Store (Convert_Ref (Emit_LValue (Name (N)), TE),
                Get_Value (Def_Ident));

      --  If this is a constant, just use the value of the expression for
      --  this object.  Otherwise, get the LValue of the expression, but
      --  don't try to force it into memory since that would give us a
      --  copy, which isn't useful.  If this is not a constant, the front
      --  end will have verified that the renaming is an actual LValue.
      --  Don't do this at library level both because we want to
      --  materialize the value and because it may need run-time
      --  computation.

      elsif Is_True_Constant (Def_Ident)
        and then not Is_Volatile (Def_Ident)
        and then (not Library_Level
                    or else Is_No_Elab_Needed (Name (N)))
      then
         Set_Value (Def_Ident, Emit_Conversion (Name (N), TE,
                                               Empty, False, False));
      elsif Is_Static_Location (Name (N)) or else not Library_Level then
         Set_Value (Def_Ident, Convert_Ref (Emit_LValue (Name (N)), TE));

      else
         LLVM_Var := Add_Global (TE, Get_Ext_Name (Def_Ident),
                                 Need_Reference => True);
         Set_Value (Def_Ident, LLVM_Var);
         Set_Initializer (LLVM_Var, Const_Null_Ref (TE));
         Add_To_Elab_Proc (N);
      end if;
   end Emit_Renaming_Declaration;

   ---------------------
   -- Emit_Identifier --
   ---------------------

   function Emit_Identifier (N : Node_Id) return GL_Value is
      TE        : constant Entity_Id := Full_Etype (N);
      E         : constant Entity_Id :=
        (if Nkind (N) in N_Entity then N else Entity (N));
      Def_Ident : constant Entity_Id :=
        (if Ekind (E) = E_Constant and then Present (Full_View (E))
           and then No (Address_Clause (E)) then Full_View (E) else E);
      Expr      : constant Node_Id   := Initialized_Value          (Def_Ident);
      V_Act     : constant GL_Value  := Get_From_Activation_Record (Def_Ident);
      V         : GL_Value           := Get_Value                  (Def_Ident);

   begin
      --  See if this is an entity that's present in our
      --  activation record. Return it if so.

      if Present (V_Act) then
         return V_Act;

      --  If this entity has a known constant value, use it unless we're
      --  getting the address or an operation where we likely need an LValue,
      --  but if it's part of Standard, we have to use it even then.

      elsif Present (Expr) and then Is_No_Elab_Needed (Expr)
        and then not (Nkind_In (Parent (N), N_Attribute_Reference,
                                N_Selected_Component, N_Indexed_Component,
                                N_Slice)
                        and then Prefix (Parent (N)) = N
                        and then Sloc (Def_Ident) > Standard_Location)
      then
         return Emit_Conversion (Expr, TE);
      end if;

      --  Otherwise, see if we have any special cases

      case Ekind (Def_Ident) is

         when E_Enumeration_Literal =>
            --  N_Defining_Identifier nodes for enumeration literals are not
            --  stored in the environment. Handle them here.

            return Const_Int (TE, Enumeration_Rep (Def_Ident));

         when E_Label =>
            return Block_Address (Current_Func, Get_Label_BB (Def_Ident));

         when Subprogram_Kind =>
            return Emit_Subprogram_Identifier (Def_Ident, N, TE);

         when E_Discriminant =>
            --  If this is a bare discriminant, it's a reference to the
            --  discriminant of some record.
            return Use_Discriminant_For_Bound (Def_Ident);

         when others =>
            --  If we haven't seen this variable and it's not in our
            --  code unit, make a global for it.

            if No (V) and then not In_Extended_Main_Code_Unit (Def_Ident) then
               V := Make_Global_Variable (Def_Ident);
            end if;

            --  Now return what we got (if we didn't get anything by now,
            --  we have an internal error).  But avoid returning a double
            --  reference.

            return (if   Is_Double_Reference (V)
                    then Get (V, Any_Reference) else V);
      end case;
   end Emit_Identifier;

end GNATLLVM.Variables;
