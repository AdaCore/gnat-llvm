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

with Errout;   use Errout;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Table;

with LLVM.Core;  use LLVM.Core;

with GNATLLVM.Blocks;      use GNATLLVM.Blocks;
with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.Environment; use GNATLLVM.Environment;
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
   --
   --  Identifiers with interface names are relatively rare and duplications
   --  are rarer still, so we don't need to be overly concerned here with
   --  execution efficiency and trade that off against memory utilization
   --  and avoiding the complexity of hash tables.
   --
   --  All we record once we finish our computation is a table consisting of
   --  the following record and a table that it indexes into.

   type Global_Dup_Entry is record
      E     : Entity_Id;
      --  One entity that's part of a duplicate set

      Index : Integer;
      --  Ordinal saying which duplicate this is; if two entries have this
      --  same index, they represent the same duplicated interface name.
      --  This is the index into the Global_Dup_Value table.
   end record;

   package Global_Dup is new Table.Table
     (Table_Component_Type => Global_Dup_Entry,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 4,
      Table_Name           => "Global_Dup_Table");

   package Global_Dup_Value is new Table.Table
     (Table_Component_Type => GL_Value,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 1,
      Table_Name           => "Global_Dup_Value_Table");

   function Find_Dup_Entry (E : Entity_Id) return Integer
     with Pre => Present (E) and then not Is_Type (E);
   --  If E is present in the above table, returns the value of Index
   --  for that entry or 0 if not present.

   function Get_Dup_Global_Value (E : Entity_Id) return GL_Value
     with Pre  => Present (E) and then not Is_Type (E);
   --  If E corresponds to a duplicated interface name and we've aready
   --  created a global for it, return that global.

   procedure Set_Dup_Global_Value (E : Entity_Id; V : GL_Value)
     with Pre  => Present (E) and then not Is_Type (E) and then Present (V);
   --  If E corresponds to a duplicated interface name, record that we've
   --  created a value for it.

   function Get_Dup_Global_Is_Defined (E : Entity_Id) return Boolean
     with Pre  => Present (E) and then not Is_Type (E);
   --  Return True if E corresponds to a duplicated interface name and one
   --  occurence of that name in the extended main unit is defining it.

   --------------------
   -- Find_Dup_Entry --
   --------------------

   function Find_Dup_Entry (E : Entity_Id) return Integer is
   begin
      --  Don't even to search if we don't have an Interface_Name or
      --  do have an address clause, because those can't be in the list.

      if No (Interface_Name (E)) or else Present (Address_Clause (E)) then
         return 0;
      end if;

      for J in 1 .. Global_Dup.Last loop
         if Global_Dup.Table (J).E = E then
            return Global_Dup.Table (J).Index;
         end if;
      end loop;

      return 0;
   end Find_Dup_Entry;

   --------------------------
   -- Get_Dup_Global_Value --
   --------------------------

   function Get_Dup_Global_Value (E : Entity_Id) return GL_Value is
      Idx : constant Integer := Find_Dup_Entry (E);
   begin
      return (if Idx = 0 then No_GL_Value else Global_Dup_Value.Table (Idx));
   end Get_Dup_Global_Value;

   --------------------------
   -- Set_Dup_Global_Value --
   --------------------------

   procedure Set_Dup_Global_Value (E : Entity_Id; V : GL_Value) is
      Idx : constant Integer := Find_Dup_Entry (E);
   begin
      if Idx /= 0 then
         Global_Dup_Value.Table (Idx) := V;
      end if;
   end Set_Dup_Global_Value;

   -------------------------------
   -- Get_Dup_Global_Is_Defined --
   -------------------------------

   function Get_Dup_Global_Is_Defined (E : Entity_Id) return Boolean is
      Idx : constant Integer := Find_Dup_Entry (E);
   begin
      --  If this is not in the table, we have no external definition

      if Idx = 0 then
         return False;
      end if;

      --  Otherwise, search for an entity in our duplicate class that's in
      --  the extended main unit and are being exported.

      for J in 1 .. Global_Dup.Last loop
         if Global_Dup.Table (J).Index = Idx
           and then In_Extended_Main_Code_Unit (Global_Dup.Table (J).E)
           and then Is_Exported (Global_Dup.Table (J).E)
         then
            return True;
         end if;
      end loop;

      return False;
   end Get_Dup_Global_Is_Defined;

   -----------------------------------
   -- Detect_Duplicate_Global_Names --
   -----------------------------------

   procedure Detect_Duplicate_Global_Names is

      --  Make a table to record each string used in an interface name.
      --  Record the first entity we encountered it with and the index in
      --  Global_Dup_Value, if we've made one.

      type One_Interface_Name is record
         S     : String_Id;
         --  The identifier of the string representing the name

         E     : Entity_Id;
         --  An entity we've seen (the first)

         Index : Integer;
         --  Index into Global_Dup_Value or 0 if none yet.
      end record;

      package Interface_Names is new Table.Table
        (Table_Component_Type => One_Interface_Name,
         Table_Index_Type     => Integer,
         Table_Low_Bound      => 1,
         Table_Initial        => 100,
         Table_Increment      => 10,
         Table_Name           => "Interface_Names_Table");

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
         --  Ignore Standard and ASCII packages

         if Sloc (U) <= Standard_Location then
            return;
         end if;

         --  Otherwise, scan the declarations and then the unit itself

         N := First (Declarations (Aux_Decls_Node (Parent (U))));
         while Present (N) loop
            Scan (N);
            Next (N);
         end loop;

         Scan (U);
      end Scan_Library_Item;

      -------------------
      -- Scan_One_Node --
      -------------------

      function Scan_One_Node (N : Node_Id) return Traverse_Result is
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
           and then Present (Interface_Name (N))
           and then No (Address_Clause (N)) -- see cd30005
         then
            --  See if this Interface_Name is already in our table.  If it
            --  isn't, add it and indicate which node it was from, but then
            --  we're done since it's not a duplicate yet.

            for J in 1 .. Interface_Names.Last loop
               if String_Equal (Interface_Names.Table (J).S,
                                Strval (Interface_Name (N)))
               then
                  --  But if it is present, we do have a duplicate.  If we've
                  --  already built a Global_Dup_Value entry, all we have to
                  --  do is make a new Global_Dup entry recording our entity.

                  if Interface_Names.Table (J).Index /= 0 then
                     Global_Dup.Append ((N, Interface_Names.Table (J).Index));
                  else
                     --  Otherwise, make a new Global_Dup_Value entry, make
                     --  a Global_Dup entry for both this and the previous
                     --  entry, and indicate the Global_Dup_Value entry.

                     Global_Dup_Value.Append (No_GL_Value);
                     Global_Dup.Append ((N, Global_Dup_Value.Last));
                     Global_Dup.Append ((Interface_Names.Table (J).E,
                                         Global_Dup_Value.Last));
                  end if;

                  return OK;
               end if;
            end loop;

            Interface_Names.Append ((Strval (Interface_Name (N)), N, 0));
         end if;

         return OK;
      end Scan_One_Node;

   begin
      --  Start of processing for Detect_Duplicate_Global_Names

      Scan_All_Units;
      Interface_Names.Free;
   end Detect_Duplicate_Global_Names;

   ----------------------
   -- Emit_Declaration --
   ----------------------

   procedure Emit_Declaration (N : Node_Id) is
      Def_Ident    : constant Node_Id   := Defining_Identifier (N);
      TE           : constant Entity_Id := Full_Etype (Def_Ident);
      Expr         : constant Node_Id   := Expression (N);
      Value        : GL_Value           := No_GL_Value;
      Copied       : Boolean            := False;
      Is_External  : Boolean            := False;
      LLVM_Var_Dup : GL_Value           := No_GL_Value;
      LLVM_Var     : GL_Value           := No_GL_Value;

   begin
      --  Object declarations are variables either allocated on the stack
      --  (local) or global.

      --  If we are processing only declarations, only declare the
      --  corresponding symbol at the LLVM level and add it to the
      --  environment.

      --  Nothing to do if this is a debug renaming type

      if TE = Standard_Debug_Renaming_Type then
         return;
      end if;

      --  Ignore deferred constant definitions without address Clause since
      --  they are processed fully in the front-end.  If No_Initialization
      --  is set, this is not a deferred constant but a constant whose
      --  value is built manually.  And constants that are renamings are
      --  handled like variables.

      if Ekind (Def_Ident) = E_Constant
        and then Present (Full_View (Def_Ident))
        and then No (Address_Clause (Def_Ident))
        and then not No_Initialization (N)
        and then No (Renamed_Object (Def_Ident))
      then
         return;
      end if;

      --  Handle top-level declarations or ones that need to be treated
      --  that way.  If we're processing elaboration code, we've already
      --  made the item and need do nothing special if it's to be
      --  statically allocated.

      if Library_Level
        or else (Is_Statically_Allocated (Def_Ident)
                   and then not Special_Elaboration_Code)
      then
         --  If we have an Interface name, see if this is a duplicate of
         --  another entity for which we've already made a global.

         LLVM_Var_Dup := Get_Dup_Global_Value (Def_Ident);
         if Present (LLVM_Var_Dup) then

            --  We could do this if both the previous and our entities
            --  agree on whether the type's size is dynamic, rather than
            --  requiring that neither be, but it's not worth the trouble.

            if Is_Double_Reference (LLVM_Var_Dup)
              or else Is_Dynamic_Size (TE)
            then
               Error_Msg_N
                 ("All uses of same interface name must have static size", N);
            else
               LLVM_Var := Convert_To_Access_To (LLVM_Var_Dup, TE);
            end if;
         end if;

         --  If we haven't previously allocated a global above, or we couldn't
         --  use the one we allocated, make one here and properly set its
         --  linkage information.

         if No (LLVM_Var) then
            LLVM_Var := Add_Global
              (TE, Get_Ext_Name (Def_Ident),
               Need_Reference => (Present (Address_Clause (Def_Ident))
                                    or else Is_Dynamic_Size (TE)));
            Set_Thread_Local (LLVM_Var,
                              Has_Pragma_Thread_Local_Storage (Def_Ident));

            if not Library_Level and then No (Interface_Name (Def_Ident)) then
               Set_Linkage (LLVM_Var, Internal_Linkage);
            elsif not In_Main_Unit
              or else (Is_Imported (Def_Ident)
                         and then not Get_Dup_Global_Is_Defined (Def_Ident))
            then
               Set_Linkage (LLVM_Var, External_Linkage);
               Is_External := True;
            end if;

            Set_Dup_Global_Value (Def_Ident, LLVM_Var);
         end if;

         Set_Value (Def_Ident, LLVM_Var);

         if In_Main_Unit then

            --  ??? This code is probably wrong, but is rare enough that
            --  we'll worry about it later.

            if Present (Address_Clause (Def_Ident)) then
               Set_Initializer
                 (LLVM_Var,
                  Emit_Expression (Expression (Address_Clause (Def_Ident))));
               --  ??? Should also take Expression (Node) into account

            elsif not Is_External then
               if Is_Dynamic_Size (TE) then
                  Elaboration_Table.Append (N);

                  --  Take Expression (Node) into account

               elsif Present (Expr)
                 and then not (Nkind (N) = N_Object_Declaration
                                 and then No_Initialization (N))
               then
                  --  We can set an initializer if this is a compile-time
                  --  known expression and we have the actual global,
                  --  not a type-converted value.

                  if Compile_Time_Known_Value (Expr)
                    and then (No (LLVM_Var_Dup)
                                or else LLVM_Var = LLVM_Var_Dup)
                  then
                     Set_Initializer
                       (LLVM_Var, Build_Type_Conversion (Expr, TE));
                  else
                     Elaboration_Table.Append (N);

                     if not Is_Imported (Def_Ident) then
                        Set_Initializer (LLVM_Var, Const_Null (TE));
                     end if;
                  end if;
               elsif not Is_Imported (Def_Ident) then
                  Set_Initializer (LLVM_Var, Const_Null (TE));
               end if;
            end if;
         end if;
      else
         if Present (Expr)
           and then not (Nkind (N) = N_Object_Declaration
                           and then No_Initialization (N))
         then
            Value := Emit_Expression (Expr);
         end if;

         if Special_Elaboration_Code then
            LLVM_Var := Get_Value (Def_Ident);

            if Is_Dynamic_Size (TE) then
               Store (Heap_Allocate_For_Type (TE, TE, Value), LLVM_Var);
               Copied := True;
            end if;

         elsif Present (Address_Clause (Def_Ident)) then
            LLVM_Var := Int_To_Ref
              (Emit_Expression
                 (Expression (Address_Clause (Def_Ident))), TE,
               Get_Name (Def_Ident));
         else
            LLVM_Var :=
              Allocate_For_Type (TE, TE, Value, Get_Name (Def_Ident));
            Copied := True;
         end if;

         Set_Value (Def_Ident, LLVM_Var);

         if not Copied and then Present (Value) then
            Emit_Assignment (LLVM_Var, Empty, Value, True, True);
         end if;
      end if;
   end Emit_Declaration;

   --------------------------------------
   -- Emit_Object_Renaming_Declaration --
   --------------------------------------

   procedure Emit_Object_Renaming_Declaration (N : Node_Id) is
   begin
      --  If this is a constant, just use the value of the expression for
      --  this object.  Otherwise, get the LValue of the expression, but
      --  don't try to force it into memory since that would give us a
      --  copy, which isn't useful.  If this is not a constant, the front
      --  end will have verified that the renaming is an actual LValue.
      --  Don't do this at library level both because we want to
      --  materialize the value and because it may need run-time
      --  computation.

      if Is_True_Constant (Defining_Identifier (N)) and not Library_Level then
         Set_Value (Defining_Identifier (N), Emit_Expression  (Name (N)));
      else
         Set_Value (Defining_Identifier (N), Emit_LValue (Name (N)));
      end if;
   end Emit_Object_Renaming_Declaration;

   -----------------------------
   --  Emit_Identifier_LValue --
   -----------------------------

   function Emit_Identifier_LValue (N : Node_Id) return GL_Value is
      Def_Ident : Entity_Id := Entity (N);
      TE        : Entity_Id := Full_Etype (Def_Ident);
      V         : GL_Value  := Get_Value (Def_Ident);
      V1        : GL_Value;

   begin
      --  If this is a deferred constant, look at the private version

      if Ekind (Def_Ident) = E_Constant
        and then Present (Full_View (Def_Ident))
        and then No (Address_Clause (Def_Ident))
      then
         Def_Ident := Full_View (Def_Ident);
         V         := Get_Value (Def_Ident);
      end if;

      --  See if this is an entity that's present in our activation
      --  record. Return it if so.

      V1 := Get_From_Activation_Record (Def_Ident);
      if Present (V1) then
         return V1;

         --  If this a label, we can use "blockaddress"

      elsif Ekind (Def_Ident) = E_Label then
         return Block_Address (Current_Func, Get_Label_BB (Def_Ident));

      elsif Ekind (Def_Ident) in Subprogram_Kind then

         --  If this has an Alias, use that.  However, it may be an
         --  Enumeration_Literal, in which case we can't get its LValue.

         if Present (Alias (Def_Ident)) then
            Def_Ident := Alias (Def_Ident);
            V         := Get_Value (Def_Ident);
            pragma Assert (Ekind (Def_Ident) in Subprogram_Kind);
         end if;

         --  If we are elaborating this for 'Access, we want the actual
         --  subprogram type here, not the type of the return value, which
         --  is what TE is set to.

         if Nkind (Parent (N)) = N_Attribute_Reference
           and then Is_Access_Type (Full_Etype (Parent (N)))
         then
            TE := Full_Designated_Type (Full_Etype (Parent (N)));
         end if;

         if not Needs_Activation_Record (TE) then
            if TE = Standard_Void_Type then
               return V;
            else
               return Convert_To_Access_To (V, TE);
            end if;
         else
            --  Return a callback, which is a pair: subprogram
            --  code pointer and static link argument.

            return Insert_Value
              (Insert_Value (Get_Undef_Ref (TE), Get_Static_Link (N), 1),
               Pointer_Cast (V, Standard_A_Char), 0);
         end if;

         --  Handle entities in Standard and ASCII on the fly

      elsif No (V) and then Sloc (Def_Ident) <= Standard_Location then
         V := Add_Global (TE, Get_Ext_Name (Def_Ident));
         Set_Linkage (V, External_Linkage);
         Set_Value (Def_Ident, V);
         return V;
      elsif Is_Double_Reference (V) then
         return Load (V);
      else
         return V;
      end if;
   end Emit_Identifier_LValue;

   ---------------------------
   -- Emit_Identifier_Value --
   ---------------------------

   function Emit_Identifier_Value (N : Node_Id) return GL_Value is
      TE : constant Entity_Id := Full_Etype (N);
      Def_Ident : Entity_Id := Entity (N);
      V         : GL_Value;
   begin
      --  N_Defining_Identifier nodes for enumeration literals are not
      --  stored in the environment. Handle them here.

      --  If this is a deferred constant, look at private version

      if Ekind (Def_Ident) = E_Constant
        and then Present (Full_View (Def_Ident))
        and then No (Address_Clause (Def_Ident))
      then
         Def_Ident := Full_View (Def_Ident);
      end if;

      --  See if this is an entity that's present in our
      --  activation record. Return it if so.

      V := Get_From_Activation_Record (Def_Ident);
      if Present (V) then
         return Need_Value (V, Full_Etype (Def_Ident));

      elsif Ekind (Def_Ident) = E_Enumeration_Literal then
         return Const_Int (TE, Enumeration_Rep (Def_Ident));

         --  If this entity has a known constant value, use it

      elsif Ekind (Def_Ident) = E_Constant
        and then Present (Constant_Value (Def_Ident))
        and then Compile_Time_Known_Value (Constant_Value (Def_Ident))
      then
         return Emit_Expression (Constant_Value (Def_Ident));

         --  Handle entities in Standard and ASCII on the fly

      elsif Sloc (Def_Ident) <= Standard_Location then
         declare
            Node : constant Node_Id := Get_Full_View (Def_Ident);
            Decl : constant Node_Id := Declaration_Node (Node);
            Expr : Node_Id := Empty;

         begin
            if Nkind (Decl) /= N_Object_Renaming_Declaration then
               Expr := Expression (Decl);
            end if;

            if Present (Expr)
              and then Nkind_In (Expr, N_Character_Literal, N_Expanded_Name,
                                 N_Integer_Literal, N_Real_Literal)
            then
               return Emit_Expression (Expr);

            elsif Present (Expr) and then Nkind (Expr) = N_Identifier
              and then Ekind (Entity (Expr)) = E_Enumeration_Literal
            then
               return
                 Const_Int (TE, Enumeration_Rep (Entity (Expr)));
            else
               return Emit_Expression (Node);
            end if;
         end;

      elsif Nkind (N) in N_Subexpr
        and then Is_Constant_Folded (Entity (N))
      then

         --  Replace constant references by the direct values, to avoid a
         --  level of indirection for e.g. private values and to allow
         --  generation of static values and static aggregates.

         declare
            Node : constant Node_Id := Get_Full_View (Entity (N));
            Decl : constant Node_Id := Declaration_Node (Node);
            Expr : Node_Id := Empty;

         begin
            if Nkind (Decl) /= N_Object_Renaming_Declaration then
               Expr := Expression (Decl);
            end if;

            if Present (Expr) then
               if Nkind_In (Expr, N_Character_Literal, N_Expanded_Name,
                            N_Integer_Literal, N_Real_Literal)
                 or else (Nkind (Expr) = N_Identifier
                            and then (Ekind (Entity (Expr)) =
                                        E_Enumeration_Literal))
               then
                  return Emit_Expression (Expr);
               end if;
            end if;
         end;
      end if;

      V := Get_Value (Def_Ident);
      if Is_Double_Reference (V) then
         V := Load (V);
      end if;

      return Need_Value (V, Full_Etype (Def_Ident));
   end Emit_Identifier_Value;

end GNATLLVM.Variables;
