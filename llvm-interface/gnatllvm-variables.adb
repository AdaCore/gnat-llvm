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

with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Stand;    use Stand;

with LLVM.Core;  use LLVM.Core;

with GNATLLVM.Blocks;      use GNATLLVM.Blocks;
with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.Subprograms; use GNATLLVM.Subprograms;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package body GNATLLVM.Variables is

   ----------------------
   -- Emit_Declaration --
   ----------------------

   procedure Emit_Declaration (N : Node_Id) is
      Def_Ident : constant Node_Id   := Defining_Identifier (N);
      TE        : constant Entity_Id := Full_Etype (Def_Ident);
      Expr      : constant Node_Id   := Expression (N);
      Value     : GL_Value           := No_GL_Value;
      Copied    : Boolean            := False;
      LLVM_Var  : GL_Value;

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
         LLVM_Var := Add_Global
           (TE, Get_Ext_Name (Def_Ident),
            Need_Reference => (Present (Address_Clause (Def_Ident))
                                 or else Is_Dynamic_Size (TE)));
         Set_Thread_Local (LLVM_Var,
                           Has_Pragma_Thread_Local_Storage (Def_Ident));

         if not Library_Level then
            Set_Linkage (LLVM_Var, Internal_Linkage);
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

            else
               if Is_Imported (Def_Ident) then
                  Set_Linkage (LLVM_Var, External_Linkage);

               elsif Is_Dynamic_Size (TE) then
                  Elaboration_Table.Append (N);

                  --  Take Expression (Node) into account

               elsif Present (Expr)
                 and then not (Nkind (N) = N_Object_Declaration
                                 and then No_Initialization (N))
               then
                  if Compile_Time_Known_Value (Expr) then
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
         else
            Set_Linkage (LLVM_Var, External_Linkage);
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

      elsif Sloc (Def_Ident) <= Standard_Location then
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
