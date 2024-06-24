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

with Einfo.Utils; use Einfo.Utils;
with Errout;      use Errout;
with Get_Targ;    use Get_Targ;
with Lib;         use Lib;
with Nlists;      use Nlists;
with Sem;         use Sem;
with Sem_Aux;     use Sem_Aux;
with Sem_Eval;    use Sem_Eval;
with Sem_Util;    use Sem_Util;
with Snames;      use Snames;
with Stand;       use Stand;
with Stringt;     use Stringt;
with Table;       use Table;

with GNATLLVM.Aliasing;     use GNATLLVM.Aliasing;
with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Blocks;       use GNATLLVM.Blocks;
with GNATLLVM.Codegen;      use GNATLLVM.Codegen;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Conversions;  use GNATLLVM.Conversions;
with GNATLLVM.DebugInfo;    use GNATLLVM.DebugInfo;
with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.Exprs;        use GNATLLVM.Exprs;
with GNATLLVM.GLType;       use GNATLLVM.GLType;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;
with GNATLLVM.Records;      use GNATLLVM.Records;
with GNATLLVM.Subprograms;  use GNATLLVM.Subprograms;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Types.Create; use GNATLLVM.Types.Create;
with GNATLLVM.Utils;        use GNATLLVM.Utils;

package body GNATLLVM.Variables is

   --  We sometime have a case where we have a pragma Export on one entity
   --  and a pragma Import for the same global name on a different entity.
   --  As long as both aren't simultaneously visible, there's no problem,
   --  but if we try to elaborate both of them, LLVM will detect the
   --  duplicate name and "disambiguate" it, which isn't what we want
   --  because they are, indeed, meant to refer to the same variable. So
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

      E     : Opt_Global_Name_Kind_Id;
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
      E     : Global_Name_Kind_Id;
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

   function Find_Dup_Entry
     (E : Global_Name_Kind_Id) return Global_Dup_Value_Id;
   --  If E is present in the above table, returns the value of Index
   --  for that entry or 0 if not present.

   function Find_Dup_Entry (S : String) return Global_Dup_Value_Id;
   --  Similar, but by string (for builtins)

   function Get_Dup_Global_Is_Defined (E : Global_Name_Kind_Id) return Boolean;
   --  Return True if E corresponds to a duplicated interface name and one
   --  occurence of that name in the extended main unit is defining it.

   function String_Equal (L : String_Id; R : String) return Boolean;
   --  Compare a string with an entry in the string table

   function Has_Global_Name (E : Global_Name_Kind_Id) return Boolean;
   --  Return True if E may have a global name that we need to check for dups

   function Variable_GL_Type
     (E : Exception_Or_Object_Kind_Id; Expr : Opt_N_Subexpr_Id) return GL_Type
     with Post => Present (Variable_GL_Type'Result);
   --  Determine the proper GL_Type to use for E. If Expr is Present, it's
   --  an initializing expression for E.

   function Variable_Alloc_GL_Type
     (E : Exception_Or_Object_Kind_Id; Expr : Opt_N_Subexpr_Id) return GL_Type
     with Post => Present (Variable_Alloc_GL_Type'Result);
   --  Likewise, but the proper GL_Type to use when allocating E.

   function Is_Volatile_Entity (E : Evaluable_Kind_Id) return Boolean is
     (Is_Volatile_Object (E) or else Treat_As_Volatile (E)
        or else (not Emit_C and then Address_Taken (E)));
   --  True iff E is an entity (a variable or constant) that we
   --  need to treat as volatile for any reason. If we take the address of
   --  an entity, we want to treat it as volatile for optimization purposes,
   --  but we don't need to do this when generating C because the optimizer
   --  can see the address being taken. Moreover, if we do this, we'll
   --  produce a warning on the assignment of the address in the case
   --  of 'Unrestricted_Access.

   function Make_Global_Variable
     (E          : Exception_Or_Object_Kind_Id;
      GT         : GL_Type;
      Definition : Boolean) return GL_Value
     with Pre => Present (GT), Post => Present (Make_Global_Variable'Result);
   --  Create a global variable for E. Definition is true if we
   --  are doing this for a declaration.

   function Alloca_Smaller_Than
     (T : Type_T; Elts : GL_Value; Max : ULL) return Boolean
     with Pre => Present (T);
   --  True iff the size of Elts occurrences of T is fixed and smaller
   --  than Max bits.

   function Convert_Constant (V : GL_Value; GT : GL_Type) return GL_Value is
     (if   Is_Aggregate_Type (V) or else Is_Aggregate_Type (GT)
      then Convert_Aggregate_Constant (V, GT) else Convert_GT (V, GT))
     with Pre  => Is_Data (V) and then Present (GT),
          Post => Related_Type (Convert_Constant'Result) = GT;
   --  Convert V, a constant for which Can_Convert_Constant is true, to GT

   function Can_Convert_Constant
     (V            : GL_Value;
      GT           : GL_Type;
      Not_Symbolic : Boolean := False) return Boolean
     with Pre => Present (V) and then Present (GT);
   --  Return True iff Convert_Constant can convert V to GT. If Not_Symbolic,
   --  the result must not be a symbolic constant

   function Is_Entity_Static_Address
     (E : Evaluable_Kind_Id; Not_Symbolic : Boolean := False) return Boolean;
   --  Return True if E represents an address that can computed statically.
   --  If Not_Symbolic is True, only return if this address is a constant
   --  integer (rare).

   function Is_No_Elab_For_Convert
     (N              : N_Subexpr_Id;
      GT             : GL_Type;
      Not_Symbolic   : Boolean := False;
      Restrict_Types : Boolean := False) return Boolean
     with Pre => Present (GT);
   --  See if can avoid an elaboration procedure when elaborating N and
   --  then converting it to GT.

   function Is_No_Elab_For_Convert_Entity
     (E              : Evaluable_Kind_Id;
      GT             : GL_Type;
      Not_Symbolic   : Boolean := False;
      Restrict_Types : Boolean := False) return Boolean
     with Pre => Present (GT);
   --  See if can avoid an elaboration procedure when elaborating E and
   --  then converting it to GT.

   function Emit_No_Error (N : N_Subexpr_Id) return GL_Value
     with Post => Present (Emit_No_Error'Result);
   --  Like Emit_Expression, but don't post an error if there's an
   --  overflow.

   function Is_Static_Location (N : N_Subexpr_Id) return Boolean;
   --  Return True if N represent an object with constant address

   function Is_Entity_Static_Location (E : Evaluable_Kind_Id) return Boolean;
   --  Return True if E represent an object with constant address

   function Initialized_Value (E : Evaluable_Kind_Id) return Opt_N_Subexpr_Id;
   --  If E is an E_Constant that has an initializing expression, return it

   Const_Map : Value_Value_Map_P.Map;
   --  Map the Value_T for a constant to the Value_T for the global
   --  variable containing that constant. We do this at the Value_T level
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

   function Has_Global_Name (E : Global_Name_Kind_Id) return Boolean is
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

   function Find_Dup_Entry (E : Global_Name_Kind_Id) return Global_Dup_Value_Id
   is
   begin
      --  Don't even to search if this doesn't have a global name

      if not Has_Global_Name (E) then
         return Empty_Global_Dup_Value_Id;
      end if;

      for J in 1 .. Global_Dup.Last loop
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
      for J in  1 .. Interface_Names.Last loop
         if String_Equal (Interface_Names.Table (J).S, S) then
            return Interface_Names.Table (J).Index;
         end if;
      end loop;

      return Empty_Global_Dup_Value_Id;
   end Find_Dup_Entry;

   --------------------------
   -- Get_Dup_Global_Value --
   --------------------------

   function Get_Dup_Global_Value (E : Global_Name_Kind_Id) return GL_Value is
      Idx : constant Global_Dup_Value_Id := Find_Dup_Entry (E);

   begin
      return (if No (Idx) then No_GL_Value else Global_Dup_Value.Table (Idx));
   end Get_Dup_Global_Value;

   --------------------------
   -- Set_Dup_Global_Value --
   --------------------------

   procedure Set_Dup_Global_Value (E : Global_Name_Kind_Id; V : GL_Value) is
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

   function Get_Dup_Global_Is_Defined (E : Global_Name_Kind_Id) return Boolean
   is
      Idx : constant Global_Dup_Value_Id := Find_Dup_Entry (E);

   begin
      --  If this is not in the table, we have no external definition

      if No (Idx) then
         return False;
      end if;

      --  Otherwise, search for an entity in our duplicate class that's in
      --  the extended main unit and is being exported.

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

      function Scan_One_Node (N : Node_Id) return Traverse_Result;
      --  Scan a single node looking for pragma Import or Export

      procedure Scan is new Traverse_Proc (Scan_One_Node);
      --  Used to scan most of a library unit looking for pragma Import/Export

      procedure Scan_Unit is new Scan_Library_Item (Scan => Scan);
      --  Scan one library item looking for pragma Import or Export

      procedure Scan_All_Units is
         new Sem.Walk_Library_Items (Action => Scan_Unit);

      -------------------
      -- Scan_One_Node --
      -------------------

      function Scan_One_Node (N : Node_Id) return Traverse_Result is
         This_Str : String_Id;

      begin
         --  If we run into a stub, we have to search inside it because
         --  Library_Unit is a semantic, not syntactic, field.

         if Nkind (N) in N_Body_Stub and then Present (Library_Unit (N)) then
            Scan (Library_Unit (N));

         --  Ignore if this is generic

         elsif Is_Generic_Item (N) then
            return Skip;

         --  Otherwise, check for cases where we have an interface name
         --  and process each of them to check for duplicates.

         elsif Nkind (N) = N_Defining_Identifier
           and then Ekind (N) in Global_Name_Kind
           and then Has_Global_Name (N)
         then
            --  See if this name is already in our table. If it isn't, add
            --  it and indicate which node it was from, but then we're done
            --  since it's not a duplicate yet. Handle both Interface_Name
            --  and exceptions in Standard.

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

                  --  But if it is present, we do have a duplicate. If we've
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

   -------------------
   -- Emit_No_Error --
   -------------------

   function Emit_No_Error (N : N_Subexpr_Id) return GL_Value is
      Result : GL_Value;

   begin
      Push_Suppress_Overflow;
      Result := Emit_Expression (N);
      Pop_Suppress_Overflow;
      return Result;
   end Emit_No_Error;

   ------------------------
   -- Is_Static_Location --
   ------------------------

   function Is_Static_Location (N : N_Subexpr_Id) return Boolean is
   begin
      case Nkind (N) is
         when N_Entity_Name =>
            return Is_Entity_Static_Location (Entity (N));

         when N_Selected_Component =>
            return Is_Static_Location (Prefix (N))
              and then not Is_Bitfield_By_Rep (Entity (Selector_Name (N)));

         when N_Unchecked_Type_Conversion
            | N_Type_Conversion
            | N_Qualified_Expression =>

            --  Conversions between scalar types may actually need to be
            --  done (even if UC), but conversions involving composite
            --  types are always pointer-punning in this context.

            return Is_Composite_Type (Etype (N))
              and then Is_Static_Location (Expression (N));

         when N_Indexed_Component => Indexed_Component : declare

            GT : constant GL_Type := Full_GL_Type (Prefix (N));
            Index : Opt_N_Is_Index_Id;
            Expr  : Opt_N_Subexpr_Id;

         begin
            --  Not static if prefix not static, a lower bound isn't
            --  static, or an expression isn't static. It's also possible
            --  that the type of the prefix isn't actually an array in the
            --  case where we have a packed array type. But then we know
            --  that it is static.

            if not Is_Static_Location (Prefix (N)) then
               return False;
            elsif not Is_Array_Type (GT)
              or else Ekind (GT) = E_String_Literal_Subtype
            then
               return True;
            end if;

            Index := First_Index (GT);
            Expr  := First (Expressions (N));
            while Present (Index) loop
               exit when not Is_No_Elab_Needed
                               (Low_Bound (Simplify_Range (Index)))
                 or else not Is_No_Elab_Needed (Expr);
               Next_Index (Index);
               Next (Expr);
            end loop;

            return No (Index);
         end Indexed_Component;

         when N_Slice =>

            --  Not static unless prefix is static and the lower bound of
            --  the node.

            if not Is_Static_Location (Prefix (N))
              or else not (Is_Static_Expression
                             (Low_Bound (Simplify_Range (Discrete_Range (N)))))
            then
               return False;
            end if;

            --  Now check the lower bound of the prefix. A string literal
            --  is always static.

            declare
               GT : constant GL_Type := Full_GL_Type (Prefix (N));

            begin
               return not Is_Array_Type (GT)
                 or else Ekind (GT) = E_String_Literal_Subtype
                 or else Is_Static_Expression
                           (Low_Bound (Simplify_Range (First_Index (GT))));
            end;

         --  If this is a dereference of a static address, it's at a
         --  static location.

         when N_Explicit_Dereference =>
            return Is_Static_Address (Prefix (N));

         when others =>
            return False;
      end case;
   end Is_Static_Location;

   -------------------------------
   -- Is_Entity_Static_Location --
   -------------------------------

   function Is_Entity_Static_Location (E : Evaluable_Kind_Id) return Boolean is
      Can_Have_Alias : constant Boolean :=
        Is_Overloadable (E) or else Ekind (E) = E_Subprogram_Type;

   begin
      --  If a deferred constant, look at the full object

      if Ekind (E) = E_Constant and then Present (Full_View (E)) then
         return Is_Entity_Static_Location (Full_View (E));

      --  If something with an alias, look at the alias

      elsif Can_Have_Alias and then Present (Alias (E)) then
         return Is_Entity_Static_Location (Alias (E));

      --  Likewise for a renamed object or exception

      elsif not Can_Have_Alias and then not Is_Object (E)
        and then Present (Renamed_Entity (E))
      then
         return Is_Entity_Static_Location (Renamed_Entity (E));
      elsif not Can_Have_Alias and then Is_Object (E)
        and then Present (Renamed_Object (E))
      then
         return Is_Static_Location (Renamed_Object (E));

      --  If we have an address clause, see if the address is static

      elsif Present (Address_Clause (E)) then
         return Is_Static_Address (Expression (Address_Clause (E)));

      --  If it's a subprogram with no activation record, it's always at a
      --  static location.

      elsif E in Subprogram_Kind_Id and then not Has_Activation_Record (E) then
         return True;
      end if;

      --  Otherwise, this is at a static location if it's a fixed-length
      --  object allocated statically.

      return Ekind (E) /= E_Enumeration_Literal
        and then (Ekind (Full_Etype (E)) = E_Void
                    or else not Is_Nonnative_Type (Full_GL_Type (E)))
        and then (Library_Level or else In_Elab_Proc
                    or else Is_Statically_Allocated (E)
                    or else Enclosing_Dynamic_Scope (E) = Standard_Standard);

   end Is_Entity_Static_Location;

   -----------------------
   -- Is_Static_Address --
   -----------------------

   function Is_Static_Address
     (N : N_Subexpr_Id; Not_Symbolic : Boolean := False) return Boolean is
   begin
      case Nkind (N) is
         when N_Unchecked_Type_Conversion
            | N_Type_Conversion
            | N_Qualified_Expression =>
            return Is_Static_Address (Expression (N), Not_Symbolic);

         when N_Attribute_Reference =>
            return not Not_Symbolic
              and then Get_Attribute_Id (Attribute_Name (N))
                         in Attribute_Address | Attribute_Access |
                         Attribute_Unchecked_Access | Attribute_Code_Address |
                         Attribute_Unrestricted_Access
              and then Is_Static_Location (Prefix (N));

         when N_Entity_Name =>
            return not Not_Symbolic
              and then Is_Entity_Static_Address (Entity (N));

         when others =>
            return Compile_Time_Known_Value (N)
              and then (not Not_Symbolic
                          or else Is_A_Constant_Int (Emit_No_Error (N))
                          or else Is_A_Constant_FP  (Emit_No_Error (N)));
      end case;
   end Is_Static_Address;

   --------------------
   -- Static_Address --
   --------------------

   function Static_Address (N : N_Subexpr_Id) return GL_Value is
   begin
      case Nkind (N) is
         when N_Unchecked_Type_Conversion
            | N_Type_Conversion
            | N_Qualified_Expression
         =>
            return Static_Address (Expression (N));

         when N_Integer_Literal =>

            --  Return the LLVM representation of the integer. We can't go
            --  through Emit because we exceptionally need the address
            --  represented as an integer here (as opposed to a void
            --  pointer); therefore, we generate the LLVM value directly
            --  with a suitable type.

            return Const_Int (Size_GL_Type, Intval (N));

         when others =>
            Error_Msg_N ("unsupported static address", N);
            return Get_Undef (Size_GL_Type);
      end case;
   end Static_Address;

   ------------------------------
   -- Is_Entity_Static_Address --
   ------------------------------

   function Is_Entity_Static_Address
     (E : Evaluable_Kind_Id; Not_Symbolic : Boolean := False) return Boolean
   is
      CV : constant Opt_N_Subexpr_Id := Initialized_Value (E);
   begin
      return Present (CV) and then Is_Static_Address (CV, Not_Symbolic);

   end Is_Entity_Static_Address;

   -----------------------
   -- Initialized_Value --
   -----------------------

   function Initialized_Value (E : Evaluable_Kind_Id) return Opt_N_Subexpr_Id
   is
      Full_E : constant Evaluable_Kind_Id :=
        (if   Ekind (E) = E_Constant and then Present (Full_View (E))
         then Full_View (E) else E);
      Decl   : constant Opt_N_Is_Decl_Id  := Declaration_Node (Full_E);
      CV     : Opt_N_Subexpr_Id;

   begin
      --  First check for cases where we don't have or can't use the
      --  initialized value. But if this is in Standard, we have to use
      --  the initialized value even if it appears to be volatile.

      if No (Decl) or else not Is_True_Constant (E)
        or else (Is_Volatile_Entity (E) and then Sloc (E) > Standard_Location)
        or else (Nkind (Decl) = N_Object_Declaration
                   and then No_Initialization (Decl))
      then
         return Empty;
      else
         CV := Constant_Value (E);

         --  ??? workaround for S112-007

         if Present (CV) and then No (Etype (CV)) then
            return Empty;
         end if;

         return (if   Present (CV) and then Is_No_Elab_Needed (CV)
                 then CV else Empty);
      end if;
   end Initialized_Value;

   --------------------------
   -- Can_Convert_Constant --
   --------------------------

   function Can_Convert_Constant
     (V            : GL_Value;
      GT           : GL_Type;
      Not_Symbolic : Boolean := False) return Boolean is

   begin
      --  If it's not data or not a constant, we can't

      if not Is_Data (V) or else not Is_Constant (V) or else Is_Undef (V) then
         return False;

      --  If one type is aggregate, see if we can convert it that way

      elsif Is_Aggregate_Type (V) or else Is_Aggregate_Type (GT) then
         return Can_Convert_Aggregate_Constant (V, GT);

      --  We can always convert scalar to scalar, but may need to check for
      --  a symbolic value.

      else
         return not Not_Symbolic or else Is_A_Constant_Int (V)
           or else Is_A_Constant_FP (V);
      end if;
   end Can_Convert_Constant;

   -----------------------
   -- Is_No_Elab_Needed --
   -----------------------

   function Is_No_Elab_Needed
     (N              : N_Subexpr_Id;
      Not_Symbolic   : Boolean := False;
      Restrict_Types : Boolean := False) return Boolean
   is
      GT   : constant GL_Type := Full_GL_Type (N);
      Expr : Node_Id;
      F    : Record_Field_Kind_Id;

   begin
      --  If this is an aggregate type, we don't want to worry about
      --  this when just doing back-annotations.

      if Decls_Only and then Is_Composite_Type (GT) then
         return False;

      --  If we aren't to allow access or wide types, test here.

      elsif Restrict_Types
        and then ((Is_Elementary_Type (GT)
                    and then Get_Type_Size (GT) > Get_Bits_Per_Word)
                  or else Is_Access_Type (GT))
      then
         return False;
      end if;

      case Nkind (N) is
         when N_Aggregate | N_Extension_Aggregate =>

            if Is_Array_Type (GT) then

               --  We don't support constant aggregates of
               --  multi-dimensional Fortran arrays because it's too
               --  complex. And we also can't make a constant aggregate of
               --  a dynamic size type even if the array has constant
               --  elements because we can't form the resulting
               --  GL_Value. But be careful not to be confused by having
               --  the unconstrained array as the type of inner aggregates
               --  of multi-dimensional arrays. We also can't support an
               --  aggregate where the component type is a unconstrained
               --  record.

               if (Number_Dimensions (GT) > 1
                     and then Convention (GT) = Convention_Fortran)
                 or else (Is_Constrained (GT) and then Is_Nonnative_Type (GT))
                 or else Is_Unconstrained_Record (Full_Component_Type (GT))
               then
                  return False;
               end if;

               Expr := First (Expressions (N));
               while Present (Expr) loop
                  exit when not Is_No_Elab_Needed (Expr, Not_Symbolic,
                                                   Restrict_Types);
                  Next (Expr);
               end loop;

            elsif Is_Record_Type (GT) then

               Expr := First (Component_Associations (N));
               while Present (Expr) loop
                  F := Entity (First (Choices (Expr)));

                  if Ekind (F) /= E_Discriminant
                    or else not Is_Unchecked_Union (Full_Scope (F))
                  then
                     Discard (Type_Of (Full_Scope (F)));
                     F := Find_Matching_Field (Full_Scope (F), F);
                     exit when not Box_Present (Expr)
                       and then (not Is_No_Elab_For_Convert (Expression (Expr),
                                                             Field_Type (F),
                                                             Not_Symbolic,
                                                             Restrict_Types)
                                   or else Is_Large_Array_Bitfield (F));
                  end if;

                  Next (Expr);
               end loop;
            else
               return False;
            end if;

            return No (Expr);

         when N_Binary_Op => Binary_Op : declare

            --  LLVM only allows adds and subtracts of symbols to be
            --  considered static, so we can only allow actual known values
            --  in that case with one exception, which is if we have a
            --  comparison involving symbolic operands and that comparison
            --  has a constant value. Note that having a comparison
            --  operation "clears" the flags because all we'll see is the
            --  result of the comparison. However, C doesn't allow the
            --  subtracting two addresses.

            Is_Cmp : constant Boolean := Nkind (N) in N_Op_Compare;
            Our_NS : constant Boolean :=
              (Not_Symbolic and then not Is_Cmp)
              or Nkind (N) not in N_Op_Add | N_Op_Subtract | N_Op_Compare;
            Our_RT : constant Boolean := Restrict_Types and not Is_Cmp;

         begin
            if not Is_No_Elab_Needed (Left_Opnd (N),
                                      Not_Symbolic   => Our_NS,
                                      Restrict_Types => Our_RT)
              or else not Is_No_Elab_Needed (Right_Opnd (N),
                                             Not_Symbolic => Our_NS or Emit_C,
                                             Restrict_Types => Our_RT)
              --  If either side needs an elab proc, we do

              or else (Tagged_Pointers and then Is_Address (GT))
              --  If this is arithmetic on tagged pointers, we need an elab
              --  proc to create a valid pointer result.

              or else (Do_Overflow_Check (N)
                         and then Overflowed (Emit_No_Error (N)))
              --  If we're to check for overflow, this needs an elab proc
              --  if the operation overflows.

            then
               return False;
            end if;

            --  Otherwise, we're OK if this isn't a comparison or if it
            --  is and the result is a constant.

            return Nkind (N) not in N_Op_Compare
              or else Is_A_Constant_Int (Emit_No_Error (N));
         end Binary_Op;

         when N_Unary_Op =>

            return Is_No_Elab_Needed (Right_Opnd (N), Not_Symbolic,
                                      Restrict_Types)
              and then (not Do_Overflow_Check (N)
                         or else not Overflowed (Emit_No_Error (N)));

         when N_Unchecked_Type_Conversion
            | N_Type_Conversion
            | N_Qualified_Expression =>

            return Is_No_Elab_For_Convert (Expression (N), GT, Not_Symbolic,
                                           Restrict_Types)
              --  Operand must not need elaboration

              and then (not Is_Scalar_Type (GT)
                          or else Type_Of (GT) = Address_T
                          or else Is_A_Constant_Int (Emit_No_Error
                                                       (Expression (N)))
                          or else Is_A_Constant_FP  (Emit_No_Error
                                                       (Expression (N))))

              --  If converting to a scalar type other than Size_Type,
              --  we can't have a relocatable expression.

              and then (Nkind (N) /= N_Type_Conversion
                          or else not Do_Overflow_Check (N)
                          or else not Overflowed (Emit_No_Error (N)));
            --  If testing for overflow, must not overflow

         when N_Attribute_Reference =>

            --  If we can compute this from annotations in the tree, we
            --  know it doesn't need elaboration.

            if Present (Get_Attribute_From_Annotation (N)) then
               return True;
            end if;

            case Get_Attribute_Id (Attribute_Name (N)) is
               when Attribute_Size | Attribute_Object_Size
                  | Attribute_Value_Size | Attribute_Component_Size
                  | Attribute_Max_Size_In_Storage_Elements
                  | Attribute_Alignment | Attribute_Descriptor_Size =>

                  if Is_Entity_Name (Prefix (N))
                    and then Is_Type (Entity (Prefix (N)))
                  then
                     return not Is_Nonnative_Type (Full_Entity (Prefix (N)));
                  else

                  --  We have to be careful here because even though we
                  --  don't usually need to evaluate the Prefix to get
                  --  its size, we are required to, so it must be static
                  --  as well.

                     return not Is_Nonnative_Type (Full_Etype (Prefix (N)))
                       and then Is_No_Elab_Needed (Prefix (N), Not_Symbolic,
                                                   Restrict_Types);
                  end if;

               when Attribute_Min | Attribute_Max =>

                  return Is_No_Elab_Needed (First (Expressions (N)),
                                            Not_Symbolic, Restrict_Types)
                    and then Is_No_Elab_Needed (Last (Expressions (N)),
                                                Not_Symbolic, Restrict_Types);

               when Attribute_Pos | Attribute_Val | Attribute_Succ
                  | Attribute_Pred | Attribute_Machine | Attribute_Model =>

                  return Is_No_Elab_Needed (First (Expressions (N)),
                                            Not_Symbolic, Restrict_Types);

               when Attribute_Access | Attribute_Unchecked_Access
                  | Attribute_Unrestricted_Access | Attribute_Address
                  | Attribute_Code_Address | Attribute_Pool_Address =>

                  return Is_Static_Address (N, Not_Symbolic);

               when Attribute_Passed_By_Reference
                  | Attribute_Mechanism_Code | Attribute_Null_Parameter =>

                  return True;

               when Attribute_First  | Attribute_Last
                 | Attribute_Length | Attribute_Range_Length =>

                  if Is_Scalar_Type (Full_Etype (Prefix (N))) then
                     Expr :=
                       Simplify_Range (Scalar_Range (Full_Etype (Prefix (N))));
                     return Is_No_Elab_Needed (Low_Bound (Expr),
                                               Not_Symbolic, Restrict_Types)
                       and then Is_No_Elab_Needed (High_Bound (Expr),
                                                   Not_Symbolic,
                                                   Restrict_Types);
                  else
                     return False;
                  end if;

               when others =>
                  return False;
            end case;

         when N_Entity_Name =>

            return Is_No_Elab_For_Convert_Entity (Entity (N), GT, Not_Symbolic,
                                                  Restrict_Types);

         when N_Selected_Component =>

            return not Is_Nonnative_Type (Full_GL_Type (Entity
                                                          (Selector_Name (N))))
              and then Is_No_Elab_Needed (Prefix (N), Not_Symbolic,
                                          Restrict_Types);

         --  If this is extracting constant offsets from something that we
         --  can elaborate statically, we can elaborate this statically.
         --  We need not check for constant bounds since we couldn't
         --  elaborate our Prefix if they were variable.

         when N_Indexed_Component =>

            if not Is_No_Elab_Needed (Prefix (N), Not_Symbolic, Restrict_Types)
            then
               return False;
            end if;

            Expr := First (Expressions (N));
            while Present (Expr) loop
               exit when not Is_No_Elab_Needed (Expr, Not_Symbolic,
                                                Restrict_Types);
               Next (Expr);
            end loop;

            return No (Expr);

         when N_String_Literal =>

            return True;

         when others =>

            --  An address that we can evaluate or other compile time
            --  known value, we don't need any elaboration.

            return Is_Static_Address (N, Not_Symbolic);
      end case;

   end Is_No_Elab_Needed;

   ----------------------------------
   -- Is_No_Elab_Needed_For_Entity --
   ----------------------------------

   function Is_No_Elab_Needed_For_Entity
     (E              : Evaluable_Kind_Id;
      Not_Symbolic   : Boolean := False;
      Restrict_Types : Boolean := False) return Boolean
   is
      GT : constant GL_Type          := Full_GL_Type (E);
      CV : constant Opt_N_Subexpr_Id := Initialized_Value (E);
      V  : constant GL_Value         := Get_Value (E);

   begin
      if Ekind (E) = E_Enumeration_Literal then
         return True;

      elsif Present (V) and then Is_Data (V)
        and then Can_Convert_Constant (V, GT, Not_Symbolic)
      then
         return True;

      elsif Ekind (E) = E_Constant and then Present (Full_View (E))
        and then No (Address_Clause (E))
      then
         return Is_No_Elab_For_Convert_Entity (Full_View (E), GT,
                                               Not_Symbolic, Restrict_Types);
      else
         return Ekind (E) = E_Constant and then Present (CV)
           and then Is_No_Elab_For_Convert (CV, GT, Not_Symbolic,
                                            Restrict_Types);
      end if;

   end Is_No_Elab_Needed_For_Entity;

   ----------------------------
   -- Is_No_Elab_For_Convert --
   ---------------------------

   function Is_No_Elab_For_Convert
     (N              : N_Subexpr_Id;
      GT             : GL_Type;
      Not_Symbolic   : Boolean := False;
      Restrict_Types : Boolean := False) return Boolean is
   begin
      --  If we're converting to a non-native type, this always needs
      --  elaboration code.

      if Is_Nonnative_Type (GT) then
         return False;

      --  We always require the input to not need elaboration, but we may
      --  have further restrictions.

      else
         declare
            In_GT  : constant GL_Type := Full_GL_Type (N);
            Our_NS : Boolean          := Not_Symbolic;
            Our_RT : Boolean          := Restrict_Types;

         begin
            --  If both types are elementary, the only restriction we may
            --  have is that if we're converting to a scalar type other
            --  than Size_Type, we can't have a relocatable expression.

            if Is_Elementary_Type (In_GT) and then Is_Elementary_Type (GT) then
               if Is_Scalar_Type (GT)
                 and then Type_Of (GT) /= Address_T
                 and then Type_Of (GT) /= Type_Of (In_GT)
               then
                  Our_NS := True;
               end if;

            --  Otherwise, we'll be using Convert_Aggregate_Constant, so
            --  both restrictions apply unless the layouts are the same.
            --  Also, we can't do this at all for types that are too large.

            elsif not Is_Layout_Identical (In_GT, GT) then
               Our_NS := True;
               Our_RT := True;

               if Get_Type_Size (GT) >= To_Bits (1024) then
                  return False;
               end if;
            end if;

            return Is_No_Elab_Needed (N, Not_Symbolic => Our_NS,
                                      Restrict_Types  => Our_RT);
         end;
      end if;

   end Is_No_Elab_For_Convert;

   -----------------------------------
   -- Is_No_Elab_For_Convert_Entity --
   -----------------------------------

   function Is_No_Elab_For_Convert_Entity
     (E              : Evaluable_Kind_Id;
      GT             : GL_Type;
      Not_Symbolic   : Boolean := False;
      Restrict_Types : Boolean := False) return Boolean is
   begin
      --  If we're converting to a non-native type, this always needs
      --  elaboration code.

      if Is_Nonnative_Type (GT) then
         return False;

      --  We always require the input to not need elaboration, but we may
      --  have further restrictions.

      else
         declare
            In_GT  : constant GL_Type := Full_GL_Type (E);
            Our_NS : Boolean          := Not_Symbolic;
            Our_RT : Boolean          := Restrict_Types;

         begin
            --  If both types are elementary, the only restriction we may
            --  have is that if we're converting to a scalar type other
            --  than Size_Type, we can't have a relocatable expression.

            if Is_Elementary_Type (In_GT) and then Is_Elementary_Type (GT) then
               if Is_Scalar_Type (GT)
                 and then Type_Of (GT) /= Size_T
                 and then Type_Of (GT) /= Type_Of (In_GT)
               then
                  Our_NS := True;
               end if;

            --  Otherwise, we'll be using Convert_Aggregate_Constant, so
            --  both restrictions apply unless the layouts are the same.
            --  Also, we can't do this at all for types that are too large.

            elsif not Is_Layout_Identical (In_GT, GT) then
               Our_NS := True;
               Our_RT := True;

               if Get_Type_Size (GT) >= To_Bits (1024) then
                  return False;
               end if;
            end if;

            return Is_No_Elab_Needed_For_Entity (E, Not_Symbolic => Our_NS,
                                                 Restrict_Types  => Our_RT);
         end;
      end if;

   end Is_No_Elab_For_Convert_Entity;

   -------------------------
   -- Alloca_Smaller_Than --
   -------------------------

   function Alloca_Smaller_Than
     (T : Type_T; Elts : GL_Value; Max : ULL) return Boolean
   is
      T_Size           : constant ULL          := Get_Type_Size (T);
      Var_Size         : constant Boolean      :=
        Present (Elts) and then not Is_A_Constant_Int (Elts);
      Elts_ULL         : constant ULL          :=
        (if Present (Elts) and then not Var_Size then +Elts else 1);

   begin
      --  We can't promote to the entry block unless this is of fixed size.
      --  If we're not generating C and it's larger than Max, also don't
      --  say we can promote.

      return not Var_Size and then (Emit_C or else T_Size * Elts_ULL < Max);
   end Alloca_Smaller_Than;

   --------------------------
   -- Maybe_Promote_Alloca --
   --------------------------

   function Maybe_Promote_Alloca
     (T : Type_T; Elts : GL_Value := No_GL_Value) return Basic_Block_T
   is
      Max_Promote_Size : constant               := 1_000_000_000;
      Current_BB       : constant Basic_Block_T := Get_Insert_Block;

   begin
      --  If this is of a size that allows promotion, promote it to the
      --  entry block by setting our position into that block and returning
      --  our current position. Otherwise, nothing to do here.

      if Present (Entry_Block_Allocas)
        and then Alloca_Smaller_Than (T, Elts, Max_Promote_Size)
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

   procedure Done_Promoting_Alloca
     (Alloca : GL_Value;
      BB     : Basic_Block_T;
      T      : Type_T;
      Elts   : GL_Value := No_GL_Value)
   is
      Threshold_In_Words : constant     := 20;
      Min_Lifetime_Size  : constant ULL :=
        ULL (Get_Bits_Per_Word) *  Threshold_In_Words;

   begin
      --  If we promoted this alloca, update the position for allocas to
      --  after this one and restore the saved position. Otherwise,
      --  indicate that a stack save may be needed in the current block.

      if Present (BB) then
         Entry_Block_Allocas.Instr := +Alloca;
         Position_Builder_At_End (BB);

         --  If this is a large enough object to be worthwhile, emit a call
         --  to indicate the start of the lifetime and set up to emit the
         --  end of the lifetime when the block ends. Note that we know
         --  this has a constant size because Maybe_Promote_Alloca promoted
         --  this.

         if not Alloca_Smaller_Than (T, Elts, Min_Lifetime_Size) then
            declare
               T_Size     : constant ULL := Get_Type_Size (T);
               Num_Elts   : constant ULL :=
                 (if Present (Elts) then +Elts else 1);
               Alloc_Size : constant ULL := T_Size * Num_Elts;

            begin
               Add_Lifetime_Entry (Alloca, Const_Int_64 (Alloc_Size));
            end;
         end if;
      else
         Save_Stack_Pointer;
      end if;
   end Done_Promoting_Alloca;

   ---------------------
   -- Emit_Decl_Lists --
   ---------------------

   procedure Emit_Decl_Lists
     (List1    : List_Id := No_List;
      List2    : List_Id := No_List;
      End_List : Node_Id := Empty;
      Pass1    : Boolean := True;
      Pass2    : Boolean := True)
   is
      type List_Array is array (1 .. 2) of List_Id;
      Lists : constant List_Array := (1 => List1, 2 => List2);
      E     : Entity_Id;
      N     : Node_Id;

   begin
      if Pass1 then
         for J in 1 .. 2 loop
            if Present (Lists (J)) then
               N := First (Lists (J));
               while Present (N) and then N /= End_List loop

                  --  For package specs, we recurse inside the declarations
                  --  thus taking the two pass approach inside the boundary.

                  if Nkind (N) = N_Package_Declaration
                    and then Nkind (Specification (N)) =
                               N_Package_Specification
                  then
                     Emit_Decl_Lists (Visible_Declarations (Specification (N)),
                                      Private_Declarations (Specification (N)),
                                      Pass2 => False);

                     --  Similarly for any declarations in the actions
                     --  of a freeze node.

                  elsif Nkind (N) = N_Freeze_Entity then
                     Process_Freeze_Entity (N);
                     Emit_Decl_Lists (Actions (N), Pass2 => False);

                  --  Package bodies with freeze nodes get their
                  --  elaboration deferred until the freeze node, but the
                  --  code must be placed in the right place

                  elsif Nkind (N) = N_Package_Body
                    and then Present (Freeze_Node (Corresponding_Spec (N)))
                  then
                     Record_Code_Position (Corresponding_Spec (N));

                  elsif Nkind (N) = N_Package_Body_Stub
                    and then Present (Library_Unit (N))
                    and then Nkind (Proper_Body (Unit (Library_Unit (N)))) =
                               N_Package_Body
                    and then Present (Freeze_Node
                                        (Corresponding_Spec
                                           (Proper_Body
                                              (Unit (Library_Unit (N))))))
                  then
                     Record_Code_Position (Corresponding_Spec
                                             (Proper_Body
                                                (Unit (Library_Unit (N)))));

                  --  We defer most subprogram bodies to the second pass

                  elsif Nkind (N) = N_Subprogram_Body then
                     if Acts_As_Spec (N) then
                        E := Defining_Entity (N);

                        if Ekind (E) not in E_Generic_Procedure |
                                            E_Generic_Function
                          and then not Is_Eliminated (E)
                        then
                           Discard (Emit_Subprogram_Decl (Specification (N)));
                        end if;
                     end if;

                  --  For bodies and stubs that act as their own specs, the
                  --  entity itself must be elaborated in the first pass,
                  --  because it may be used in other declarations.

                  elsif Nkind (N) = N_Subprogram_Body_Stub then
                     E := Defining_Entity (Specification (N));

                     if Ekind (E) not in E_Subprogram_Body   |
                                         E_Generic_Procedure |
                                         E_Generic_Function
                       and then not Is_Eliminated (E)
                     then
                        Discard (Emit_Subprogram_Decl (Specification (N)));
                     end if;

                  --  Concurrent stubs stand for the corresponding
                  --  subprogram bodies, which are deferred like other
                  --  bodies.

                  elsif Nkind (N) in N_Task_Body_Stub | N_Protected_Body_Stub
                  then
                     null;

                  --  Renamed subprograms may not be elaborated yet at this
                  --  point since renamings do not trigger freezing. Wait
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
      --  bodies, which are elaborated at their freeze nodes. Note that we
      --  must also go inside things (package specs and freeze nodes) the
      --  first pass did.

      if Pass2 then
         for J in 1 .. 2 loop
            if Present (Lists (J)) then
               N := First (Lists (J));
               while Present (N) and then N /= End_List loop
                  if Nkind (N) in N_Subprogram_Body | N_Subprogram_Body_Stub |
                                  N_Task_Body_Stub  | N_Protected_Body_Stub
                  then
                     Emit (N);

                  elsif Nkind (N) = N_Package_Declaration
                    and then Nkind (Specification (N))
                               = N_Package_Specification
                  then
                     Emit_Decl_Lists (Visible_Declarations (Specification (N)),
                                      Private_Declarations (Specification (N)),
                                      Pass1 => False);

                  elsif Nkind (N) = N_Freeze_Entity then
                     Emit_Decl_Lists (Actions (N), Pass1 => False);

                  elsif Nkind (N) = N_Subprogram_Renaming_Declaration then
                     Emit (N);
                  end if;

                  Next (N);
               end loop;
            end if;
         end loop;
      end if;
   end Emit_Decl_Lists;

   -----------------------
   --  Variable_GL_Type --
   -----------------------

   function Variable_GL_Type
     (E : Exception_Or_Object_Kind_Id; Expr : Opt_N_Subexpr_Id) return GL_Type
   is
      TE          : constant Void_Or_Type_Kind_Id :=
        (if   Ekind (Etype (E)) = E_Class_Wide_Type and then Present (Expr)
              and then Nkind (Expr) = N_Qualified_Expression
         then Full_Etype (Expression (Expr)) else Full_Etype (E));
      --  Type to use for allocation. Normally, the type of the identifier
      --  unless we have a qualified expression initializing a class wide
      --  type.

      GT           : GL_Type                       := Default_GL_Type (TE);
      Max_Size : constant Boolean                  :=
        Is_Unconstrained_Record (GT);
      In_Size      : constant GL_Value             :=
        (if   Is_Dynamic_Size (GT, Max_Size) then No_GL_Value
         else Get_Type_Size (GT, Max_Size => Max_Size));
      In_Align     : constant GL_Value             := Get_Type_Alignment (GT);
      In_Align_Nat : constant Nat                  := +In_Align;
      Size         : constant Uint                 :=
        (if not Known_Esize (E) then No_Uint
         else   Validate_Size (E, GT, Esize (E),
                             Zero_Allowed => Has_Size_Clause (E)));
      Align        : Uint                          :=
        (if not Known_Alignment (E) then No_Uint
         else   Validate_Alignment (E, Alignment (E), In_Align_Nat));
      Biased   : constant Boolean                  :=
        Has_Biased_Representation (E);

   begin
      --  If this is an object with no specified size and alignment, and if
      --  either it is atomic or we are not optimizing alignment for space
      --  and it is composite and not an exception, an Out parameter or a
      --  reference to another object, and the size of its type is a
      --  constant, adjust the alignment.
      --
      --  But don't do this when generating C. Doing so will make it
      --  harder to detect user-specified alignments and this really should
      --  be done by the underlying C compiler if it would benefit the target.

      if not Emit_C and then No (Size) and then No (Align)
        and then Present (In_Size)
        and then (Is_Full_Access_Object (E)
                    or else (Ekind (E) not in E_Exception     |
                                              E_Out_Parameter |
                                              E_Loop_Parameter
                               and then Is_Composite_Type (GT)
                               and then not Optimize_Alignment_Space (E)
                               and then
                                 not Is_Constr_Array_Subt_With_Bounds (GT)
                               and then not Is_Exported (E)
                               and then not Is_Imported (E)
                               and then No (Renamed_Object (E))
                               and then No (Address_Clause (E))))
      then
         declare
            In_Size_ULL : constant ULL := +In_Size;
            Our_Align   : Nat          := 0;

         begin
            --  Rather than trying to make this unnecessarily portable,
            --  we're going to hardwire the three cases that make sense.

            case In_Size_ULL is
               when 16 =>
                  Our_Align := 16;
               when 17 .. 32 =>
                  Our_Align := 32;
               when 33 .. 128 =>
                  Our_Align := 64;
               when others =>
                  null;
            end case;

            --  If this is larger than the previous alignment, use it

            if Our_Align > In_Align_Nat then
               Align := +Our_Align;
            end if;
         end;
      end if;

      --  Get the proper GT for this object given all of the above
      --  computations. But don't use a size that overflows.

      GT := Make_GT_Alternative
        (GT, E,
         Size          =>
           (if   Present (Size) and then UI_Is_In_Int_Range (Size)
            then Size else No_Uint),
         Align         => Align,
         For_Type      => False,
         For_Component => False,
         Max_Size      => Max_Size,
         Is_Biased     => Biased);

      return GT;

   end Variable_GL_Type;

   -----------------------------
   --  Variable_Alloc_GL_Type --
   -----------------------------

   function Variable_Alloc_GL_Type
     (E : Exception_Or_Object_Kind_Id; Expr : Opt_N_Subexpr_Id) return GL_Type
   is
      GT       : constant GL_Type := Variable_GL_Type (E, Expr);

   begin
      --  To avoid linker issues, pad a zero-size object to one byte, but
      --  don't get confused for cases where we need to store bounds. We
      --  needn't do this for non-file-level objects, but do so for
      --  compatibility with Gigi. But don't do this when generating C,
      --  since we want to delete zero-sized objects in that case.

      return (if    not Emit_C and then Is_Zero_Size (GT)
                    and then not Is_Constr_Array_Subt_With_Bounds (GT)
              then Make_GT_Alternative (GT, Empty, +BPU, No_Uint)
              else GT);
   end Variable_Alloc_GL_Type;

   --------------------------
   -- Make_Global_Constant --
   --------------------------

   function Make_Global_Constant (V : GL_Value) return GL_Value is
      GT        : constant GL_Type := Related_Type (V);
      Globalize : constant Boolean :=
        Emit_C and then Present (Current_Func)
        and then (Has_Inline_Attribute (Current_Func)
                  or else Has_Inline_Always_Attribute (Current_Func));
      In_V      : GL_Value         := V;
      Out_Val   : Value_T;

   begin
      --  If we're making a constant for an undef, use an undef address

      if Is_Undef (In_V) then
         return Get_Undef_Relationship (GT, Ref (In_V));

      --  If we're making a constant for a string literal, we want both the
      --  bounds and data unless what we're being passed is just bounds.

      elsif Ekind (GT) = E_String_Literal_Subtype
        and then Relationship (V) /= Bounds
      then
         In_V := Get (In_V, Bounds_And_Data);
      end if;

      --  If we haven't already seen this value, make a constant for it.
      --  If we're generating C and the current function is inline, we need
      --  to make this a global.

      if not Const_Map.Contains (+In_V) then
         Out_Val := Add_Global  (Module, Type_Of (In_V),
                                 Globalize_Name ("for.ref"));
         Set_Initializer        (Out_Val, +In_V);
         Set_Global_Constant    (Out_Val, True);
         Set_Unnamed_Addr       (Out_Val, True);
         Const_Map.Insert       (+In_V,  Out_Val);

         if not Globalize then
            Set_Linkage         (Out_Val, Private_Linkage);
         end if;
      else
         Out_Val := Const_Map.Element (+In_V);
         if Globalize then
            Set_Linkage (Out_Val, External_Linkage);
         end if;
      end if;

      --  Now make a GL_Value. We do this here since different constant
      --  literals may have different types (i.e., bounds).

      return G (Out_Val, GT, Ref (In_V),
                Alignment => Get_Type_Alignment (Default_GL_Type (GT)));
   end Make_Global_Constant;

   --------------------------
   -- Make_Global_Variable --
   --------------------------

   function Make_Global_Variable
     (E          : Exception_Or_Object_Kind_Id;
      GT         : GL_Type;
      Definition : Boolean) return GL_Value
   is
      LLVM_Var        : GL_Value                  := Get_Dup_Global_Value (E);
      Has_Addr        : constant Boolean          :=
        Present (Address_Clause (E));
      Addr_Expr       : constant Opt_N_Subexpr_Id :=
        (if Has_Addr then Expression (Address_Clause (E)) else Empty);
      Has_Static_Addr : constant Boolean          :=
        Has_Addr and then Is_Static_Address (Addr_Expr);
      Nonnative       : constant Boolean          := Is_Nonnative_Type (GT);
      Needs_Alloc     : constant Boolean          :=
        not Has_Addr and then Nonnative;
      Is_Ref          : constant Boolean          :=
          (Has_Addr and then not Has_Static_Addr) or else Needs_Alloc
          or else (Is_Object (E) and then Present (Renamed_Object (E))
                     and then Is_Name (Renamed_Object (E)));
      Is_Volatile     : constant Boolean          := Is_Volatile_Entity (E);
      Linker_Alias    : constant Opt_N_Pragma_Id  :=
        Get_Pragma (E, Pragma_Linker_Alias);

   begin

      --  If we have an Interface name, see if this is a duplicate of
      --  another entity for which we've already made a global.

      if Present (LLVM_Var) then

         --  We could do this if both the previous and our entities agree
         --  on whether the type is nonnative, rather than requiring
         --  that neither be, but it's not worth the trouble.

         if Is_Double_Reference (LLVM_Var) or else Is_Nonnative_Type (GT)
           or else Type_Needs_Bounds (GT)
         then
            Error_Msg_N
              ("all uses of same interface name must have static size", E);
            LLVM_Var := Get_Undef_Relationship
              (GT, (if Is_Ref then Reference_To_Reference else Reference));

         --  If the old and new types have the same layout, don't do the
         --  conversion. Doing so can prevent a delayed initialization
         --  when the types are slightly different from working.

         elsif not Is_Layout_Identical (Related_Type (LLVM_Var), GT) then
            LLVM_Var := Convert_Ref (LLVM_Var, GT);
         end if;

      --  Otherwise, see if this is a simple renaming

      elsif not Is_Object (E) and then Present (Renamed_Entity (E))
        and then Is_Entity_Static_Location (Renamed_Entity (E))
      then
         LLVM_Var := Emit_Entity (Renamed_Entity (E));
      elsif Is_Object (E) and then Present (Renamed_Object (E))
        and then Is_Static_Location (Renamed_Object (E))
      then
         LLVM_Var := Emit_Safe_LValue (Renamed_Object (E));

      --  Otherwise, if this is a linker alias and we're defining this
      --  variable, set that up if we find a matching entity.

      elsif Present (Linker_Alias) and then Definition and then not Is_Ref then
         declare
            Str_Id : constant String_Id              :=
              Strval (Expression (Last (Pragma_Argument_Associations
                                          (Linker_Alias))));
            Our_E  : Opt_Exception_Or_Object_Kind_Id := Empty;

         begin
            --  Look for a variable in this compilation that has its
            --  Interface_Name the same as the string, with the same
            --  type, and which has been defined.

            for J in 1 .. Interface_Names.Last loop
               if String_Equal (Str_Id, Interface_Names.Table (J).S)
                 and then Full_GL_Type (Interface_Names.Table (J).E) = GT
                 and then Present (Get_Value (Interface_Names.Table (J).E))
               then
                  Our_E := Interface_Names.Table (J).E;
               end if;
            end loop;

            if No (Our_E) then
               Error_Msg_NE ("no matching object found", Linker_Alias, Our_E);
               LLVM_Var := Get_Undef (GT);
            else
               LLVM_Var := G (Add_Alias_2 (Module, Type_Of (GT), 0,
                                           +Get_Value (Our_E),
                                           Get_Ext_Name (E)),
                              GT, Reference);
               Initialize_TBAA (LLVM_Var, Kind_From_Decl (E));
            end if;
         end;

      --  If we're emitting C and this is of zero size, use undef.

      elsif Emit_C and then Is_Zero_Size (GT)
        and then not Type_Needs_Bounds (GT)
      then
         LLVM_Var := Get_Undef (GT);

      --  Otherwise, make one here and properly set its linkage
      --  information. Note that we don't set External_Linkage since that's
      --  the default if there's no initializer.

      else
         LLVM_Var := Add_Global (GT, Get_Ext_Name (E),
                                 Need_Reference => Is_Ref);
         Mark_Volatile    (LLVM_Var, Is_Volatile);
         Mark_Atomic      (LLVM_Var, Is_Atomic (E) or else Is_Atomic (GT));
         Set_Thread_Local (LLVM_Var, Has_Pragma_Thread_Local_Storage (E));

         --  If this isn't to be a globally-visible "global", mark it as
         --  internal. But since it isn't globally-visible, we must be
         --  declaring it. If not, this will generate an undefined external
         --  when linking.

         if Tagged_Pointers and then Has_Static_Addr then
            Set_Absolute_Address
              (LLVM_Var, Static_Address (Addr_Expr));
            Set_Linkage (LLVM_Var, External_Linkage);
         elsif not Is_Public (E) and then not Is_Imported (E) then
            pragma Assert (Definition);
            Set_Linkage (LLVM_Var, Internal_Linkage);
         end if;

         Set_Dup_Global_Value (E, LLVM_Var);
         Set_Linker_Section   (LLVM_Var, E);
         Process_Pragmas      (E, LLVM_Var);
         Initialize_TBAA      (LLVM_Var, Kind_From_Decl (E));

         if not Tagged_Pointers or else not Has_Static_Addr then
            if not Is_Ref then
               Set_Alignment
                 (LLVM_Var, Set_Object_Align (LLVM_Var, GT, E));
            else
               Set_Alignment
                 (LLVM_Var,
                  Get_Type_Alignment (Type_Of (LLVM_Var)));
            end if;

            if not DSO_Preemptable then
               Set_DSO_Local (LLVM_Var);
            end if;
         end if;
      end if;

      --  Now save the value we've made for this variable

      Set_Value (E, LLVM_Var);
      C_Set_Entity (LLVM_Var, E);
      return LLVM_Var;

   end Make_Global_Variable;

   ----------------------
   -- Emit_Declaration --
   ----------------------

   procedure Emit_Declaration
     (N : N_Declaration_Id; For_Freeze_Entity : Boolean := False)
   is
      function Is_Matching_Unc_Array
        (GT : GL_Type; Addr : N_Subexpr_Id) return Boolean
        with Pre => Present (GT);
      --  Return True iff Addr is an expression that represents the address
      --  of an object that's an unconstrained array wih the same bounds
      --  size as GT.

      E               : constant Exception_Or_Object_Kind_Id :=
        Defining_Identifier (N);
      --  Identifier being defined

      Full_Ident      : constant Exception_Or_Object_Kind_Id :=
        (if   Ekind (E) = E_Constant and then Present (Full_View (E))
         then Full_View (E) else E);
      --  Identifier to use to find the initializing expression

      No_Init         : constant Boolean                     :=
        Nkind (N) = N_Object_Declaration and then No_Initialization (N);
      --  True if we aren't to initialize this object (ignore expression)

      Expr            : Opt_N_Subexpr_Id                     :=
        (if No_Init then Empty else Expression (N));
      --  Initializing expression, if Present and we are to use one

      GT              : constant GL_Type                     :=
        Variable_GL_Type (E, Expr);
      --  Type to use for E

      Alloc_GT        : constant GL_Type                     :=
        Variable_Alloc_GL_Type (E, Expr);
      --  Type to use for allocating E (may be different)

      Nonnative       : constant Boolean                     :=
        Is_Nonnative_Type (GT);
      --  True if the type to use for this variable isn't a native LLVM type

      Has_Addr        : constant Boolean                     :=
          Present (Address_Clause (E));
      --  True if variable has an address clause

      Addr_Expr       : constant Opt_N_Subexpr_Id            :=
        (if Has_Addr then Expression (Address_Clause (E)) else Empty);
      --  Expression to use for the address, if Present

      Is_External     : constant Boolean                    :=
        Is_Imported (E) and then not Has_Addr
          and then not Get_Dup_Global_Is_Defined (E);
      --  True if variable is not defined in this unit

      Has_Static_Addr : constant Boolean                    :=
        Has_Addr and then Is_Static_Address (Addr_Expr);
      --  True if variable has an address clause that's static

      Needs_Alloc     : constant Boolean                    :=
        not Has_Addr and then Nonnative;
      --  True if variable needs a dynamic allocation

      Is_Ref          : constant Boolean                    :=
        (Has_Addr and then not Has_Static_Addr) or else Needs_Alloc;
      --  True if we need to use an indirection for this variable

      Is_Volatile     : constant Boolean                    :=
        Is_Volatile_Entity (E);
      --  True if we need to consider E as volatile

      Value           : GL_Value                            :=
        (if Present (Expr) then Get_Value (Expr) else No_GL_Value);
      --  Any value that we've previously saved for an initializing expression

      Addr            : GL_Value                            :=
        (if Has_Addr then Get_Value (Addr_Expr) else No_GL_Value);
      --  Likewise for address

      Copied          : Boolean                             := False;
      --  True if we've copied the value to the variable

      Set_Init        : Boolean                             := False;
      --  True if we've made an initializer for a static variable that we're
      --  defining.

      LLVM_Var        : GL_Value                            := Get_Value (E);
      --  The LLVM value for the variable

      ---------------------------
      -- Is_Matching_Unc_Array --
      ---------------------------

      function Is_Matching_Unc_Array
        (GT : GL_Type; Addr : N_Subexpr_Id) return Boolean is
      begin
         --  If the address isn't a 'Address, this isn't a match

         if Nkind (Addr) /= N_Attribute_Reference
           or else Get_Attribute_Id (Attribute_Name (Addr))
                     /= Attribute_Address
         then
            return False;
         end if;

         --  Otherwise, get the info for the entity whose address is being
         --  taken and see if it matches.

         declare
            Other_Id : constant Exception_Or_Object_Kind_Id :=
              Entity (Prefix (Addr));
            Other_GT : constant GL_Type                     :=
              Full_GL_Type (Other_Id);

         begin
            return Is_Constr_Array_Subt_With_Bounds (Other_GT)
              and then Get_Bound_Size (GT) = Get_Bound_Size (Other_GT);
         end;
      end Is_Matching_Unc_Array;

   begin --  Start of processing for Emit_Declaration

      Check_Convention (E);

      --  Nothing to do if this is a debug renaming type

      if Full_Etype (GT) = Standard_Debug_Renaming_Type then
         return;

      --  If we're just elaborating decls, just set the variable to be
      --  and undef because we've elaborated the type (it may be that
      --  there may be types only used in an expression or address,
      --  but let's not worry about that possibility).

      elsif Decls_Only then
         if No (Get_Value (E)) then
            Set_Value (E, Emit_Undef (GT));
            Annotate_Object_Size_And_Alignment (E, GT);
         end if;

         return;

      --  If the object is to have atomic components, find the component
      --  type and validate it.

      elsif Has_Atomic_Components (E)
        or else (Is_Array_Type (GT) and then Has_Atomic_Components (GT))
      then
         Check_OK_For_Atomic_Type ((if   Is_Array_Type (GT)
                                    then Full_Component_GL_Type (GT) else GT),
                                   E, True);
      end if;

      --  Now check if the type of the object allows atomic access

      if Is_Full_Access (E) then
         Check_OK_For_Atomic_Type (GT, E);
      end if;

      --  If we're emitting C, we're going to ignore "atomic" and just treat
      --  it as volatile, so warn about it. But don't do this if it's atomic
      --  because the type is: we'll already have warned then.

      if Emit_C and then Is_Atomic (E) and then not Is_Atomic (GT) then
         Error_Msg_NE ("??atomic object & treated as volatile", E, E);
      elsif Emit_C and then Has_Atomic_Components (E) then
         Error_Msg_NE ("??object & with atomic components treated as", E, E);
         Error_Msg_N  ("\\having volatile components", E);
      end if;

      --  Object declarations are variables either allocated on the
      --  stack (local) or global. For globals, we operate in two phases:
      --  first we allocate the global itself, which may include some static
      --  initialization, and then do any required dynamic operations,
      --  which may include setting an address, allocating memory from the
      --  heap, and/or actually setting an initializing value. If we're at
      --  library level, we have to do the dynamic operations in an elab
      --  proc, but, if not (if we have something to be statically allocated),
      --  we do it inline.
      --
      --  Ignore deferred constant definitions without address Clause since
      --  they are processed fully in the front-end. If No_Initialization
      --  is set, this is not a deferred constant but a constant whose
      --  value is built manually. And constants that are renamings are
      --  handled like variables.

      if Is_Object (E) and then Full_Ident /= E and then not Has_Addr
        and then not No_Init and then No (Renamed_Object (E))
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
      --  we can evaluate statically. Otherwise, see if we've already
      --  evaluated that expression and get the value.

      elsif Present (Freeze_Node (E))
        and then not For_Freeze_Entity and then not In_Elab_Proc
      then
         --  If we have a Full_View, we may see that declaration
         --  before our freeze node, so set a dummy value for it so we
         --  can detect that.

         if Full_Ident /= E then
            Set_Value (Full_Ident, Emit_Undef (GT));
         end if;

         if Present (Expr) and then not Is_No_Elab_Needed (Expr) then
            if Library_Level then
               Add_To_Elab_Proc (Expr, For_GT => GT);
            else
               Set_Value (Expr, Emit_Convert_Value (Expr, GT));
            end if;
         end if;

         return;
      end if;

      --  If this is an aliased object with an unconstrained array nominal
      --  subtype, then it can overlay only another aliased object with an
      --  unconstrained array nominal subtype and compatible template.

      if Has_Addr and then Is_Constr_Array_Subt_With_Bounds (GT) then
         --  We make an exception for an absolute address but we warn
         --  that there's a descriptor at the start of the object.

         if Nkind (Strip_Conversions (Addr_Expr)) = N_Integer_Literal then
            Error_Msg_NE ("??aliased object& with unconstrained array " &
                          "nominal subtype", Address_Clause (E), E);
            Error_Msg_N  ("\\starts with a descriptor whose size is " &
                          "given by ''Descriptor_Size", Address_Clause (E));
         elsif not Is_Matching_Unc_Array (GT, Addr_Expr) then
            Error_Msg_NE ("aliased object& with unconstrained array " &
                          "nominal subtype", Address_Clause (E), E);
            Error_Msg_N  ("\\can overlay only aliased object with " &
                          "compatible subtype", Address_Clause (E));
         end if;
      end if;

      --  Handle top-level declarations or ones that need to be treated
      --  that way unless if we've already made the item (e.g., if we're
      --  in the elab proc).

      if (Library_Level or else Is_Statically_Allocated (E)
            or else Is_External)
        and then No (LLVM_Var)
      then
         pragma Assert (not In_Elab_Proc or else Is_Statically_Allocated (E));

         --  If we have a static address clause, we can convert it to a
         --  pointer to us and use that as our variable, except if we have
         --  tagged pointers, because then we can't create valid pointers
         --  out of thin air.

         if Has_Static_Addr
           and then (not Tagged_Pointers or else
                     not Compile_Time_Known_Value (Addr_Expr))
         then
            LLVM_Var :=
              Int_To_Ref ((if   Present (Addr) then Addr
                           else Emit_Expression (Addr_Expr)),
                          GT);
            Mark_Volatile (LLVM_Var, Is_Volatile);
            Mark_Atomic   (LLVM_Var, Is_Atomic (E) or else Is_Atomic (GT));
            Set_Init := True;
            Set_Value (E, LLVM_Var);
         else
            --  Otherwise, make a global variable. If we have an absolute
            --  address clause on a tagged-pointer target, skip
            --  initialization, just like we do for objects with address
            --  clauses in the absence of tagged pointers. Otherwise, if we
            --  have a non-static address expression, add it to the elab
            --  proc if at library level.

            LLVM_Var := Make_Global_Variable (E, GT, True);

            if Tagged_Pointers and then Has_Static_Addr then
               Set_Init := True;
            elsif Library_Level and then Has_Addr then
               Add_To_Elab_Proc (N);
            end if;
         end if;

         --  If this is an object of non-native type with no address
         --  clause, we have to take care of the allocation in the elab
         --  proc if at library level.

         if Library_Level and then not Is_External and then not Has_Addr
           and then Nonnative
         then
            Add_To_Elab_Proc (N);
         end if;

         --  If we have an initial value, we can set an initializer if this
         --  is a compile-time known expression, we have the actual global,
         --  not a type-converted value, and the variable is not of a
         --  non-native type or has an address clause.
         --
         --  Note that the code below relies on the fact that if we execute
         --  this case, we won't be added to the elab proc for any other
         --  reason because if we are, we may do both a static and run-time
         --  initialization.

         if Present (Expr) then
            if Is_No_Elab_For_Convert (Expr, GT)
              and then not Nonnative and then not Has_Addr
            then
               if No (Value) then
                  Value := Emit_Convert_Value (Expr, GT);
               end if;

               if Type_Needs_Bounds (GT) then
                  Value := Get (Value, Bounds_And_Data);
               end if;

               if not Is_Undef (LLVM_Var) then
                  Set_Initializer (LLVM_Var, Value);
               end if;

               Set_Init := True;
               Copied   := True;

               --  If this is a true constant whose address is not taken and
               --  this is not used via link name punning elsewhere, set it as
               --  a global constant.

               if Is_True_Constant (E) and then not Address_Taken (E)
               then
                  Set_Global_Constant (LLVM_Var);
               end if;

            elsif Library_Level then
               Add_To_Elab_Proc (N);
            end if;

         --  If this is a constrained subtype for a unconstrained
         --  actual array subtype of native type, this is aliased, and
         --  we have no expression. In that case, we still have to
         --  initialize the bounds.

         elsif Type_Needs_Bounds (GT) and then not Nonnative
           and then not Has_Addr
         then
            Set_Global_Constant
              (LLVM_Var, (Is_True_Constant (E)
                            and then not Address_Taken (E)));
            Set_Initializer (LLVM_Var, Get (Get_Undef_Relationship (GT, Data),
                                            Bounds_And_Data));
            Set_Init := True;
            Copied   := True;
         end if;

         --  If we haven't already set an initializing expression and
         --  this is not an external or something we've already defined,
         --  set one to null to indicate that this is being defined.

         if not Set_Init and then not Is_External
           and then Is_A_Global_Variable (LLVM_Var)
           and then (not Get_Dup_Global_Is_Defined (E) or Is_Exported (E))
         then
            Set_Initializer
              (LLVM_Var,
               (if   Is_Ref then Const_Null_Ref (GT)
                else Const_Null_Alloc (GT)));
         end if;

         --  Make debugging information for the globel variable

         Create_Global_Variable_Debug_Data (E, LLVM_Var);
      end if;

      Annotate_Object_Size_And_Alignment (E, Alloc_GT);

      --  If we're at library level and not in an elab proc, we can't do
      --  anything else now.

      if Library_Level and then not In_Elab_Proc then
         return;
      end if;

      --  If we're making an object of classwide type, strip off any
      --  conversions, since we have to ignore its type when doing the
      --  initializing.

      if Present (Expr) and then Is_Class_Wide_Equivalent_Type (GT) then
         Expr := Strip_Conversions (Expr);
      end if;

      --  Evaluate any expression for the address clause

      if Has_Addr then

         --  We have a special case here: if this would normally be
         --  allocated with bounds, the address points to the actual data,
         --  not the bounds, so the bounds get stored at an address below
         --  the data.

         declare
            R : constant GL_Relationship := Relationship_For_Alloc (GT);

         begin
            Addr :=
              Int_To_Relationship ((if   Present (Addr) then Addr
                                    else Emit_Expression (Addr_Expr)),
                                   GT,
                                   (if   R = Reference_To_Bounds_And_Data
                                    then Thin_Pointer else R));
         end;
      end if;

      --  If we've already gotten a value for the address of this entity,
      --  fetch it. If a non-constant address was specified, set the the
      --  address of the entity to that address. Otherwise, if the variable
      --  is of dynamic size, do the allocation here, copying any
      --  initializing expression.

      if Present (LLVM_Var)  then
         if Has_Addr and then not Has_Static_Addr then
            Store (Addr, LLVM_Var);
         elsif Nonnative and then not Is_External then
            Store (Get (Heap_Allocate_For_Type (GT, Alloc_GT,
                                                V        => Value,
                                                Expr     => Expr,
                                                N        => N,
                                                E        => E,
                                                Max_Size => Is_Max_Size (GT)),
                        Any_Reference),
                   LLVM_Var);
            Copied := True;
         end if;

      --  Otherwise, if we have an address, that's what we use. But it's
      --  erroneous if the address is not a multiple of the alignment, so
      --  show that we know this alignment.

      elsif Has_Addr then
         LLVM_Var := Addr;
         Initialize_Alignment (LLVM_Var);

      --  If this is a true constant, we can just use the expression that
      --  computed the constant as the value, once converted to the proper
      --  type. But don't do this if this comes from source and we're
      --  generating C since we'd prefer to materialize those variable. If
      --  the value is a Reference, it may be to something that's not
      --  constant, so we actually have to allocate our entity and copy
      --  into it. We assume here if something is marked "constant" at
      --  source level, we can't modify it even if its address is taken.

      elsif Is_True_Constant (E)
        and then (not Emit_C or else not Comes_From_Source (E)
                  or else Is_No_Elab_Needed_For_Entity (E))
        and then (not Is_Volatile or else Ekind (E) = E_Constant)
        and then (Present (Expr) or else Present (Value))
      then
         --  Evaluate the expression if needed. Normally, convert it to
         --  our type, but if our type is an unconstrained record, we need
         --  to preserve the source type to prevent too much data from
         --  being copied.

         if No (Value) then
            Value := (if   Is_Unconstrained_Record (GT)
                      then Emit_Expression (Expr)
                      else Emit_Conversion (Expr, GT));
         end if;

         --  If this is an elementary type, be sure it's a value. In any
         --  case, if it is a value, use that value for this variable.

         if Is_Elementary_Type (GT) then
            LLVM_Var := Get (Value, Data);
            Copied   := True;
         elsif Is_Data (Value) then
            LLVM_Var := Value;
            Copied   := True;
         end if;

         --  If this is an unnamed operation, set its name to that of our
         --  variable to make the code easier to read.

         if Present (LLVM_Var) and then Get_Value_Name (LLVM_Var) = "" then
            Set_Value_Name (LLVM_Var, Get_Name (E));
         end if;
      end if;

      --  If this is the object from an extended return and we have a
      --  return address, we can use that value for this variable. But we
      --  can't do this if finalization is needed since we need to do
      --  that separately for the variable and the object to be placed
      --  at the return address.
      --  ??? The setting of Set_Allocated_For_Return below looks bogus
      --  because we only check it when the return kind isn't
      --  Return_By_Parameter.

      if Is_Return_Object (E) and then Present (Return_Address_Param)
        and then not Needs_Finalization (GT)
      then
         LLVM_Var := Return_Address_Param;
         Set_Allocated_For_Return (E);

         --  If it's the object from an extended return, we don't have a return
         --  address but it's a variable-sized object and we know where
         --  the N_Simple_Return_Statement is, we can allocate the object
         --  here in the manner that the return statement otherwise would.

      elsif Is_Return_Object (E) and then Is_Nonnative_Type (GT)
        and then Present (Return_Statement (E))
      then
         LLVM_Var := Heap_Allocate_For_Type
           (Full_GL_Type (Current_Subp), Alloc_GT,
            Expr => Expr,
            N    => N,
            Proc => Procedure_To_Call (Return_Statement (E)),
            Pool => Storage_Pool (Return_Statement (E)));

      --  If we're emitting C and this is of zero size, use Undef unless
      --  it's aliased or we're taking its address.

      elsif Emit_C and then Is_Zero_Size (Alloc_GT) and then not Is_Aliased (E)
        and then not Address_Taken (E)
      then
         LLVM_Var := Get_Undef (Alloc_GT);

      --  Otherwise, if we still haven't made a variable, allocate it
      --  on the stack, copying in any value.

      elsif No (LLVM_Var) then
         LLVM_Var := Allocate_For_Type (GT, Alloc_GT, E, Value, Expr,
                                        E        => E,
                                        Max_Size => Is_Max_Size (Alloc_GT));
         Mark_Volatile (LLVM_Var, Is_Volatile);
         Mark_Atomic   (LLVM_Var, Is_Atomic (E) or else Is_Atomic (GT));
         Copied := True;

         --  If this is a constant, but we're not assigning it here, it'll
         --  be assigned in the declarative part of the block and be constant
         --  in the code for the block, so add an entry indicating that it's
         --  constant from that point on.

         if Is_True_Constant (E) and then No (Value) and then No (Expr) then
            Create_Invariant_Start (LLVM_Var,
                                    (if   Is_Dynamic_Size (Alloc_GT)
                                     then No_GL_Value
                                     else Get_Type_Size (Alloc_GT)));
         end if;
      end if;

      --  If this is a thin pointer, but not an actual constant, show
      --  that the bounds are constant.

      if Relationship (LLVM_Var) = Thin_Pointer and then not Library_Level then
         Create_Invariant_Start (Get (LLVM_Var, Reference_To_Bounds),
                                 Get_Bound_Size (GT));

      --  If this is a constrained record, the discriminant values are
      --  invariant once we've set them. If this is a constant, we'll
      --  be marking the whole thing constant.

      elsif Has_Discriminants (GT) and then Is_Constrained (GT)
        and then not Is_Unchecked_Union (GT) and then not Library_Level
        and then not Is_True_Constant (E)
      then
         declare
            Disc : Opt_E_Discriminant_Id := First_Stored_Discriminant (GT);

         begin
            while Present (Disc) loop
               if not Is_Bitfield (Ancestor_Field (Disc)) then
                  Create_Invariant_Start
                    (Record_Field_Offset (LLVM_Var, Disc),
                     Get_Type_Size (Field_Type (Ancestor_Field (Disc))));
               end if;

               Next_Stored_Discriminant (Disc);
            end loop;
         end;
      end if;

      --  If we haven't already set the value, set it now

      if No (Get_Value (E)) then
         Set_Value (E, LLVM_Var);
      end if;

      --  Show which variable this is

      C_Set_Entity (LLVM_Var, E);

      --  For a constant, show that we may need to update "at end"
      --  information.

      if Ekind (E) = E_Constant then
         Maybe_Update_At_End (E);
      end if;

      --  If we're emitting C, LLVM_Var might be an undef (if it's of zero
      --  size). In that case, just evaluate anything that needs to be
      --  evaluated in case there's a side-effect.

      if Is_Undef (LLVM_Var) and then Present (Expr) and then No (Value) then
         Discard (Emit_Expression (Expr));

      --  Otherwise, if we haven't already copied in any initializing
      --  expression, do that now.

      elsif not Copied and then (Present (Expr) or else Present (Value)) then
         Emit_Assignment (Get (LLVM_Var, Any_Reference), Expr, Value);
      end if;

      --  Generate debug information

      Create_Local_Variable_Debug_Data (E, LLVM_Var);

   end Emit_Declaration;

   -------------------------------
   -- Emit_Renaming_Declaration --
   -------------------------------

   procedure Emit_Renaming_Declaration (N : N_Renaming_Declaration_Id) is
      E           : constant Exception_Or_Object_Kind_Id :=
        Defining_Identifier (N);
      GT          : constant GL_Type                     :=
        Full_GL_Type (E);
      Is_Volatile : constant Boolean                     :=
        Is_Volatile_Entity (E);
      Use_LHS     : constant Boolean                     :=
        Is_Name (Name (N))
        and then (Nkind (Name (N)) not in N_Has_Entity
                    or else Ekind (Entity (Name (N))) /=
                              E_Enumeration_Literal);
      LLVM_Var  : GL_Value                               := Get_Value (E);
      V         : GL_Value;

   begin
      --  If this is just a macro substitution by the front end, omit
      --  the declaration. ??? We may be able to write debugging info
      --  for this object, but then again, maybe we can't (e.g. packed
      --  array slice case).

      if Is_Renaming_Of_Object (E) then
         return;

      --  If we've already defined this object, it means that we must be
      --  in an elab proc seeing this for the second time, which means
      --  that we have to set its address or value.

      elsif Present (LLVM_Var) then
         pragma Assert (In_Elab_Proc);

         if Use_LHS then
            Store (Get (Convert_Ref (Emit_LValue (Name (N)), GT), Reference),
                   LLVM_Var);
         else
            Emit_Assignment (LLVM_Var,
                             Value => Emit_Convert_Value (Name (N), GT));
         end if;

      --  If this is a constant, just use the value of the expression for
      --  this object. Otherwise, get the LValue of the expression, but
      --  don't try to force it into memory since that would give us a
      --  copy, which isn't useful. If this is not a constant, the front
      --  end will have verified that the renaming is an actual LValue.
      --  Don't do this at library level if it needs run-time computation.

      elsif Is_True_Constant (E) and then not Use_LHS
        and then not Is_Volatile
        and then (not Library_Level
                    or else Is_No_Elab_Needed (Name (N)))
      then
         V := Emit_Conversion (Name (N), GT, Empty, False, False);
         Set_Value (E, V);

         --  If we're at library level, materialize this

         if Library_Level then
            LLVM_Var := Add_Global (GT, Get_Ext_Name (E));
            Set_Initializer     (LLVM_Var, Get (V, Deref (LLVM_Var)));
            Set_Global_Constant (LLVM_Var, True);
            Set_Linker_Section  (LLVM_Var, E);
         end if;
      elsif Is_Static_Location (Name (N)) or else not Library_Level then
         LLVM_Var := Convert_Ref (Emit_LValue (Name (N)), GT);
         Set_Value (E, LLVM_Var);
         C_Set_Entity  (LLVM_Var, E);
      else
         --  If this is a constant, we're going to put the actual value there;
         --  otherwise, we'll put the address of the expression.

         LLVM_Var := Add_Global (GT, Get_Ext_Name (E),
                                 Need_Reference => Use_LHS);
         Mark_Volatile (LLVM_Var, Is_Volatile);
         Mark_Atomic   (LLVM_Var, Is_Atomic (E) or else Is_Atomic (GT));
         Set_Value     (E, LLVM_Var);
         C_Set_Entity  (LLVM_Var, E);
         Set_Initializer
           (LLVM_Var, Const_Null_Relationship (GT, Deref (LLVM_Var)));
         Add_To_Elab_Proc (N);
      end if;

      Annotate_Object_Size_And_Alignment (E, GT);

   end Emit_Renaming_Declaration;

   -----------------
   -- Emit_Entity --
   -----------------

   function Emit_Entity
     (E          : Evaluable_Kind_Id;
      N          : Opt_N_Has_Entity_Id := Empty;
      Prefer_LHS : Boolean             := False) return GL_Value
   is
      GT        : constant GL_Type          := Full_GL_Type (E);
      Full_E    : constant Entity_Id        :=
        (if   Ekind (E) = E_Constant and then Present (Full_View (E))
              and then No (Address_Clause (E))
         then Full_View (E) else E);
      Expr      : constant Opt_N_Subexpr_Id := Initialized_Value (Full_E);
      V_Act     : constant GL_Value         :=
        Get_From_Activation_Record (Full_E);
      V         : GL_Value                  := Get_Value (Full_E);

   begin
      --  See if this is an entity that's present in our
      --  activation record. Return it if so.

      if Present (V_Act) then
         return V_Act;

      --  If we have a value already computed, it's a constant, and we can
      --  convert the constant to GT, do so.

      elsif Present (V) and then Can_Convert_Constant (V, GT) then
         return Convert_Constant (V, GT);

      --  If this entity has a known constant value, use it unless we're
      --  getting the address or an operation where we likely need an LValue,
      --  but if it's part of Standard, we have to use it even then.

      elsif Present (Expr) and then Is_No_Elab_Needed (Expr)
        and then (not Prefer_LHS or else Sloc (E) <= Standard_Location)
      then
         --  If this entity is of an unconstrained record type, we want to
         --  use the type of the defining constant.

         return Emit_Conversion (Expr,
                                 (if   Is_Unconstrained_Record (GT)
                                  then Full_GL_Type (Expr) else GT));
      end if;

      --  Otherwise, see if we have any special cases

      case Ekind (Full_E) is

         when E_Enumeration_Literal =>
            --  N_Defining_Identifier nodes for enumeration literals are not
            --  stored in the environment. Handle them here.

            return From_Primitive (Const_Int (Primitive_GL_Type (GT),
                                              Enumeration_Rep (E)),
                                   GT);

         when E_Label =>
            return
              Block_Address (Current_Func,
                             Get_Label_BB (E, For_Address => True));

         when Subprogram_Kind =>
            return Emit_Subprogram_Identifier (Full_E, N, GT);

         when E_Discriminant =>

            --  If this is a bare discriminant, it's a reference to the
            --  discriminant of some record.

            return Use_Discriminant_For_Bound (Full_E);

         when others =>

            --  If this has an address expression that's statically
            --  elaborable, dereference that.

            if No (V) and then Present (Address_Clause (Full_E))
              and then Is_No_Elab_Needed (Expression (Address_Clause (Full_E)))
            then
               V := Int_To_Ref (Emit_Expression
                                  (Expression (Address_Clause (Full_E))),
                                GT);

            --  Otherwise, if we haven't seen this variable and it's
            --  a top level, make a global for it.

            elsif No (V)
              and then Enclosing_Dynamic_Scope (Full_E) = Standard_Standard
            then
               V :=
                 Make_Global_Variable (Full_E,
                                       Variable_Alloc_GL_Type (Full_E, Empty),
                                       False);

            --  If we still have nothing, but are just elaboration decls,
            --  make this an undef.

            elsif No (V) and then Decls_Only then
               V := Emit_Undef (GT);
            end if;

            --  If we're not looking for an LHS and this is a modular
            --  integer type that's a packed array implementation type, we
            --  need to mask to only the defined bits. It may be more
            --  efficient to do this when storing the data, but this is
            --  easier and safer.

            if not Prefer_LHS and then Is_Modular_Integer_Type (GT)
              and then Is_Packed_Array_Impl_Type (GT)
            then
               V := From_Primitive (To_Primitive (V), GT);
            end if;

            --  Now return what we got (if we didn't get anything by now,
            --  we have an internal error). But avoid returning a double
            --  reference.

            if No (V) then
               raise Program_Error;
            end if;

            return (if   Is_Double_Reference (V) then Get (V, Any_Reference)
                    else V);
      end case;
   end Emit_Entity;

end GNATLLVM.Variables;
