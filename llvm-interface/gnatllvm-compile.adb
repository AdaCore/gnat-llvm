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

with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with System;

with Errout;   use Errout;
with Eval_Fat; use Eval_Fat;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Sem_Aggr; use Sem_Aggr;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Table;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

with LLVM.Core;     use LLVM.Core;
with LLVM.Types;    use LLVM.Types;

with GNATLLVM.Arrays;      use GNATLLVM.Arrays;
with GNATLLVM.DebugInfo;   use GNATLLVM.DebugInfo;
with GNATLLVM.GLValue;     use GNATLLVM.GLValue;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Subprograms; use GNATLLVM.Subprograms;
with GNATLLVM.Utils;       use GNATLLVM.Utils;

package body GNATLLVM.Compile is

   --  Note: in order to find the right LLVM instruction to generate,
   --  you can compare with what Clang generates on corresponding C or C++
   --  code. This can be done online via http://ellcc.org/demo/index.cgi

   --  See also DragonEgg sources for comparison on how GCC nodes are converted
   --  to LLVM nodes: http://llvm.org/svn/llvm-project/dragonegg/trunk

   function Build_Short_Circuit_Op
     (Left, Right : Node_Id; Op : Node_Kind) return GL_Value
     with Pre  => Present (Left) and then Present (Right),
          Post => Present (Build_Short_Circuit_Op'Result);
   --  Emit the LLVM IR for a short circuit operator ("or else", "and then")

   function Emit_Attribute_Reference
     (Node : Node_Id; LValue : Boolean) return GL_Value
     with Pre  => Nkind (Node) = N_Attribute_Reference,
          Post => Present (Emit_Attribute_Reference'Result);
   --  Helper for Emit_Expression: handle N_Attribute_Reference nodes

   function Is_Zero_Aggregate (Src_Node : Node_Id) return Boolean
     with Pre => Nkind (Src_Node) = N_Aggregate
                 and then Is_Others_Aggregate (Src_Node);
   --  Helper for Emit_Assignment: say whether this is an aggregate of all
   --  zeros.

   procedure Emit_Assignment
     (LValue                    : GL_Value;
      E                         : Node_Id;
      E_Value                   : GL_Value;
      Forwards_OK, Backwards_OK : Boolean)
     with Pre => Present (LValue) or else Present (E);
   --  Helper for Emit: Copy the value of the expression E to LValue
   --  with the specified destination and expression types.

   function Emit_Comparison
     (Kind : Node_Kind; LHS, RHS : Node_Id) return GL_Value
     with Pre  => Present (LHS) and then Present (RHS),
          Post => Present (Emit_Comparison'Result);
   --  Generate a result which is a comparison of two expressions

   procedure Emit_Comparison_And_Branch
     (Kind              : Node_Kind;
      LHS, RHS          : Node_Id;
      BB_True, BB_False : Basic_Block_T)
     with Pre => Present (LHS) and then Present (RHS)
                 and then Present (BB_True) and then Present (BB_False);
   --  Similar, but generate comparison and branch to one of the basic
   --  blocks depending on the result

   function Emit_Elementary_Comparison
     (Kind               : Node_Kind;
      Orig_LHS, Orig_RHS : GL_Value) return GL_Value
     with Pre  => Is_Elementary_Type (Orig_LHS)
                  and then Is_Elementary_Type (Orig_RHS),
          Post => Present (Emit_Elementary_Comparison'Result);
   --  Helpers for Emit_Expression: handle comparison operations for
   --  elementary types.  The second form only supports discrete or pointer
   --  types.

   procedure Emit_If (Node : Node_Id)
     with Pre => Nkind (Node) = N_If_Statement;
   --  Helper for Emit: handle if statements

   procedure Emit_If_Cond (Cond : Node_Id; BB_True, BB_False : Basic_Block_T)
     with Pre => Present (Cond)
                 and then Present (BB_True) and then Present (BB_False);
   --  Helper for Emit_If to generate branch to BB_True or BB_False
   --  depending on whether Node is true or false.

   function Emit_If_Expression (Node : Node_Id) return GL_Value
     with Pre  => Nkind (Node) = N_If_Expression,
          Post => Present (Emit_If_Expression'Result);
   --  Helper for Emit_Expression: handle if expressions

   procedure Emit_If_Range
     (Node              : Node_Id;
      LHS               : GL_Value;
      Low, High         : Uint;
      BB_True, BB_False : Basic_Block_T)
     with Pre => Present (Node) and then Present (LHS)
                 and then Present (BB_True) and then Present (BB_False);
   --  Emit code to branch to BB_True or BB_False depending on whether LHS,
   --  which is of type Operand_Type, is in the range from Low to High.  Node
   --  is used only for error messages.

   procedure Emit_Case (Node : Node_Id)
     with Pre => Nkind (Node) = N_Case_Statement;
   --  Handle case statements

   function Emit_Literal (Node : Node_Id) return GL_Value
     with Pre  => Present (Node),
          Post => Present (Emit_Literal'Result);

   function Emit_LValue_Internal (Node : Node_Id) return GL_Value
     with Pre  => Present (Node),
          Post => Present (Emit_LValue_Internal'Result);
   --  Called by Emit_LValue to walk the tree saving values

   function Emit_LValue_Main (Node : Node_Id) return GL_Value
     with Pre  => Present (Node),
          Post => Present (Emit_LValue_Main'Result);
   --  Called by Emit_LValue_Internal to do the work at each level

   function Emit_Min_Max
     (Exprs : List_Id; Compute_Max : Boolean) return GL_Value
     with Pre  => List_Length (Exprs) = 2
                 and then Is_Scalar_Type (Full_Etype (First (Exprs))),
          Post => Present (Emit_Min_Max'Result);
   --  Exprs must be a list of two scalar expressions with compatible types.
   --  Emit code to evaluate both expressions. If Compute_Max, return the
   --  maximum value and return the minimum otherwise.

   function Emit_Array_Aggregate
     (Node           : Node_Id;
      Dims_Left      : Pos;
      Indices_So_Far : Index_Array;
      Value_So_Far   : GL_Value) return GL_Value
     with Pre  => Nkind (Node) = N_Aggregate and then Present (Value_So_Far),
          Post => Present (Emit_Array_Aggregate'Result);
   --  Emit an N_Aggregate which is an array, returning the GL_Value that
   --  contains the data.  Value_So_Far is any of the array whose value
   --  we've accumulated so far.  Dims_Left says how many dimensions of the
   --  outer array type we still can recurse into.  Indices_So_Far are the
   --  indexes of any outer N_Aggregate expressions we went through.

   function Emit_Shift
     (Node                : Node_Id;
      LHS_Node, RHS_Node  : Node_Id) return GL_Value
     with Pre  => Nkind (Node) in N_Op_Shift and then Present (LHS_Node)
                  and then Present (RHS_Node),
          Post => Present (Emit_Shift'Result);
   --  Helper for Emit_Expression: handle shift and rotate operations

   function Get_Label_BB (E : Entity_Id) return Basic_Block_T
     with Pre  => Ekind (E) = E_Label,
          Post => Present (Get_Label_BB'Result);
   --  Lazily get the basic block associated with label E, creating it
   --  if we don't have it already.

   procedure Decode_Range (Rng : Node_Id; Low, High : out Uint)
     with Pre => Present (Rng);
   --  Decode the right operand of an N_In or N_Not_In or of a Choice in
   --  a case statement into the low and high bounds.  If either Low or High
   --  is No_Uint, it means that we have a nonstatic value, a non-discrete
   --  value, or we can't find the value.  This should not happen in switch
   --  statements.

   function Is_Constant_Folded (E : Entity_Id) return Boolean
   is (Ekind (E) = E_Constant
       and then Is_Scalar_Type (Get_Full_View (Full_Etype (E))))
     with Pre => Present (E);

   package Elaboration_Table is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 1024,
      Table_Increment      => 100,
      Table_Name           => "Elaboration_Table");
   --  Table of statements part of the current elaboration procedure

   --  We save pairs of GNAT type and LLVM Value_T for each level of
   --  processing of an Emit_LValue so we can find it if we have a
   --  self-referential item (a discriminated record).

   package LValue_Pair_Table is new Table.Table
     (Table_Component_Type => GL_Value,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "LValue_Pair_Table");
   --  Table of intermediate results for Emit_LValue

   -----------------------
   -- Emit_Library_Item --
   -----------------------

   procedure Emit_Library_Item (U : Node_Id) is
      procedure Emit_Aux (Compilation_Unit : Node_Id);
      --  Process any pragmas and declarations preceding the unit

      --------------
      -- Emit_Aux --
      --------------

      procedure Emit_Aux (Compilation_Unit : Node_Id) is
         Prag : Node_Id;
      begin
         Prag := First (Context_Items (Compilation_Unit));
         while Present (Prag) loop
            if Nkind (Prag) = N_Pragma then
               Emit (Prag);
            end if;

            Prag := Next (Prag);
         end loop;

         Emit_List (Declarations (Aux_Decls_Node (Compilation_Unit)));
      end Emit_Aux;

   begin
      --  Ignore Standard and ASCII packages

      if Sloc (U) <= Standard_Location then
         return;
      end if;

      --  Current_Unit := Get_Cunit_Unit_Number (Parent (U));
      --  Current_Source_File := Source_Index (Current_Unit);

      if In_Extended_Main_Code_Unit (U) then
         Env.In_Main_Unit := True;

         --  ??? Has_No_Elaboration_Code is supposed to be set by default
         --  on subprogram bodies, but this is apparently not the case,
         --  so force the flag here. Ditto for subprogram decls.

         if Nkind_In (U, N_Subprogram_Body, N_Subprogram_Declaration) then
            Set_Has_No_Elaboration_Code (Parent (U), True);
         end if;

         --  Process any pragmas and declarations preceding the unit

         Emit_Aux (Parent (U));

         --  Process the unit itself

         Emit (U);

      else
         --  Should we instead skip these units completely, and generate
         --  referenced items on the fly???

         Env.In_Main_Unit := False;
         Emit_Aux (Parent (U));
         Emit (U);
      end if;
   end Emit_Library_Item;

   ------------------
   -- Decode_Range --
   ------------------

   procedure Decode_Range (Rng : Node_Id; Low, High : out Uint) is
   begin
      case Nkind (Rng) is
         when N_Identifier =>

            --  An N_Identifier can either be a type, in which case we look
            --  at the range of the type, or a constant, in which case we
            --  look at the initializing expression.

            if Is_Type (Entity (Rng)) then
               Decode_Range (Scalar_Range (Full_Etype (Rng)), Low, High);
            else
               Low := Get_Uint_Value (Rng);
               High := Low;
            end if;

         when N_Subtype_Indication =>
            Decode_Range (Range_Expression (Constraint (Rng)), Low, High);

         when N_Range | N_Signed_Integer_Type_Definition =>
            Low := Get_Uint_Value (Low_Bound (Rng));
            High := Get_Uint_Value (High_Bound (Rng));

         when N_Character_Literal | N_Integer_Literal =>
            Low := Get_Uint_Value (Rng);
            High := Low;

         when others =>
            Error_Msg_N ("unknown range operand", Rng);
            Low := No_Uint;
            High := No_Uint;
      end case;
   end Decode_Range;

   ----------
   -- Emit --
   ----------

   procedure Emit (Node : Node_Id) is
   begin
      Set_Debug_Pos_At_Node (Node);
      if Library_Level
        and then (Nkind (Node) in N_Statement_Other_Than_Procedure_Call
                   or else Nkind (Node) in N_Subprogram_Call
                   or else Nkind (Node) = N_Handled_Sequence_Of_Statements
                   or else Nkind (Node) in N_Raise_xxx_Error
                   or else Nkind (Node) = N_Raise_Statement)
      then
         --  Append to list of statements to put in the elaboration procedure
         --  if in main unit, otherwise simply ignore the statement.

         if Env.In_Main_Unit then
            Elaboration_Table.Append (Node);
         end if;

         return;
      end if;

      case Nkind (Node) is
         when N_Abstract_Subprogram_Declaration =>
            null;

         when N_Compilation_Unit =>
            Emit_List (Context_Items (Node));
            Emit_List (Declarations (Aux_Decls_Node (Node)));
            Emit (Unit (Node));
            Emit_List (Actions (Aux_Decls_Node (Node)));
            Emit_List (Pragmas_After (Aux_Decls_Node (Node)));

         when N_With_Clause =>
            null;

         when N_Use_Package_Clause =>
            null;

         when N_Package_Declaration =>
            Push_Lexical_Debug_Scope (Node);
            Emit (Specification (Node));
            Pop_Debug_Scope;

         when N_Package_Specification =>
            Push_Lexical_Debug_Scope (Node);
            Emit_List (Visible_Declarations (Node));
            Emit_List (Private_Declarations (Node));
            Pop_Debug_Scope;

            --  Only generate elaboration procedures for library-level packages
            --  and when part of the main unit.

            if Env.In_Main_Unit
              and then Nkind (Parent (Parent (Node))) = N_Compilation_Unit
            then
               if Elaboration_Table.Last = 0 then
                  Set_Has_No_Elaboration_Code (Parent (Parent (Node)), True);
               else
                  declare
                     Unit      : Node_Id := Defining_Unit_Name (Node);
                     Elab_Type : constant Type_T :=
                       Fn_Ty ((1 .. 0 => <>), Void_Type_In_Context (Env.Ctx));
                     LLVM_Func : GL_Value;

                  begin
                     if Nkind (Unit) = N_Defining_Program_Unit_Name then
                        Unit := Defining_Identifier (Unit);
                     end if;

                     LLVM_Func :=
                       G (Add_Function
                            (Env.Mdl,
                             Get_Name_String (Chars (Unit)) & "___elabs",
                             Elab_Type),
                          Standard_Void_Type, Is_Reference => True);
                     Enter_Subp (LLVM_Func);
                     Push_Debug_Scope
                       (Create_Subprogram_Debug_Info
                          (LLVM_Func, Unit, Node,
                           Get_Name_String (Chars (Unit)),
                           Get_Name_String (Chars (Unit)) & "___elabs"));
                     Env.Special_Elaboration_Code := True;

                     for J in 1 .. Elaboration_Table.Last loop
                        Env.Current_Elab_Entity := Elaboration_Table.Table (J);
                        Emit (Elaboration_Table.Table (J));
                     end loop;

                     Elaboration_Table.Set_Last (0);
                     Env.Current_Elab_Entity := Empty;
                     Env.Special_Elaboration_Code := False;
                     Build_Ret_Void;
                     Pop_Debug_Scope;
                     Leave_Subp;
                  end;
               end if;
            end if;

         when N_Package_Body =>
            declare
               Def_Id : constant Entity_Id := Unique_Defining_Entity (Node);
            begin
               if Ekind (Def_Id) in Generic_Unit_Kind then
                  if Nkind (Parent (Node)) = N_Compilation_Unit then
                     Set_Has_No_Elaboration_Code (Parent (Node), True);
                  end if;
               else
                  Push_Lexical_Debug_Scope (Node);
                  Emit_List (Declarations (Node));

                  if not Env.In_Main_Unit then
                     Pop_Debug_Scope;
                     return;
                  end if;

                  --  Handle statements

                  declare
                     Stmts     : constant Node_Id :=
                                   Handled_Statement_Sequence (Node);
                     Has_Stmts : constant Boolean :=
                                   Present (Stmts)
                                     and then Has_Non_Null_Statements
                                                (Statements (Stmts));

                     Elab_Type : Type_T;
                     LLVM_Func : GL_Value;
                     Unit      : Node_Id;

                  begin
                     --  For packages inside subprograms, generate elaboration
                     --  code as standard code as part of the enclosing unit.

                     if not Library_Level then
                        if Has_Stmts then
                           Emit_List (Statements (Stmts));
                        end if;

                     elsif Nkind (Parent (Node)) /= N_Compilation_Unit then
                        if Has_Stmts then
                           Elaboration_Table.Append (Stmts);
                        end if;

                     elsif Elaboration_Table.Last = 0
                       and then not Has_Stmts
                     then
                        Set_Has_No_Elaboration_Code (Parent (Node), True);

                     --  Generate the elaboration code for this library level
                     --  package.

                     else
                        Unit := Defining_Unit_Name (Node);

                        if Nkind (Unit) = N_Defining_Program_Unit_Name then
                           Unit := Defining_Identifier (Unit);
                        end if;

                        Elab_Type := Fn_Ty
                          ((1 .. 0 => <>), Void_Type_In_Context (Env.Ctx));
                        LLVM_Func :=
                          G (Add_Function
                               (Env.Mdl,
                                Get_Name_String (Chars (Unit)) & "___elabb",
                                Elab_Type),
                             Standard_Void_Type, Is_Reference => True);
                        Enter_Subp (LLVM_Func);
                        Push_Debug_Scope
                          (Create_Subprogram_Debug_Info
                             (LLVM_Func, Unit, Node,
                              Get_Name_String (Chars (Unit)),
                              Get_Name_String (Chars (Unit)) & "___elabs"));
                        Env.Special_Elaboration_Code := True;

                        for J in 1 .. Elaboration_Table.Last loop
                           Env.Current_Elab_Entity :=
                             Elaboration_Table.Table (J);
                           Emit (Elaboration_Table.Table (J));
                        end loop;

                        Elaboration_Table.Set_Last (0);
                        Env.Current_Elab_Entity := Empty;
                        Env.Special_Elaboration_Code := False;

                        if Has_Stmts then
                           Emit_List (Statements (Stmts));
                        end if;

                        Build_Ret_Void;
                        Pop_Debug_Scope;
                        Leave_Subp;
                     end if;
                  end;

                  Pop_Debug_Scope;
               end if;
            end;

         when N_Subprogram_Body =>
            --  If we are processing only declarations, do not emit a
            --  subprogram body: just declare this subprogram and add it to
            --  the environment.

            if not Env.In_Main_Unit then
               Discard (Emit_Subprogram_Decl (Get_Acting_Spec (Node)));
               return;

            --  Skip generic subprograms

            elsif Present (Corresponding_Spec (Node))
              and then Ekind (Corresponding_Spec (Node)) in
                         Generic_Subprogram_Kind
            then
               return;
            end if;

            Emit_Subprogram_Body (Node);

         when N_Subprogram_Declaration =>
            declare
               Subp : constant Entity_Id := Unique_Defining_Entity (Node);

            begin
               --  Do not print intrinsic subprogram as calls to those will be
               --  expanded.

               if Convention (Subp) = Convention_Intrinsic
                 or else Is_Intrinsic_Subprogram (Subp)
               then
                  null;
               else
                  Discard (Emit_Subprogram_Decl (Specification (Node)));
               end if;
            end;

         when N_Raise_Statement =>
            Emit_LCH_Call (Node);

         when N_Raise_xxx_Error =>
            if Present (Condition (Node)) then
               declare
                  BB_Then    : Basic_Block_T;
                  BB_Next    : Basic_Block_T;
               begin
                  BB_Then := Create_Basic_Block ("raise");
                  BB_Next := Create_Basic_Block;
                  Build_Cond_Br (Emit_Expression (Condition (Node)),
                                 BB_Then, BB_Next);
                  Position_Builder_At_End (BB_Then);
                  Emit_LCH_Call (Node);
                  Build_Br (BB_Next);
                  Position_Builder_At_End (BB_Next);
               end;
            else
               Emit_LCH_Call (Node);
            end if;

         when N_Object_Declaration | N_Exception_Declaration =>
            --  Object declarations are variables either allocated on the stack
            --  (local) or global.

            --  If we are processing only declarations, only declare the
            --  corresponding symbol at the LLVM level and add it to the
            --  environment.

            declare
               Def_Ident : constant Node_Id := Defining_Identifier (Node);
               T         : constant Entity_Id := Full_Etype (Def_Ident);
               LLVM_Type : Type_T;
               LLVM_Var  : GL_Value;
               Expr      : GL_Value;

            begin
               --  Nothing to do if this is a debug renaming type

               if T = Standard_Debug_Renaming_Type then
                  return;
               end if;

               --  Ignore deferred constant definitions without address
               --  Clause Since They Are Processed Fully in The Front-end.
               --  If No_Initialization is set, this is not a deferred
               --  constant but a constant whose value is built manually.
               --  And constants that are renamings are handled like
               --  variables.

               if Ekind (Def_Ident) = E_Constant
                 and then Present (Full_View (Def_Ident))
                 and then No (Address_Clause (Def_Ident))
                 and then not No_Initialization (Node)
                 and then No (Renamed_Object (Def_Ident))
               then
                  return;
               end if;

               --  Handle top-level declarations

               if Library_Level then
                  --  ??? Will only work for objects of static sizes

                  LLVM_Type := Create_Type (T);

                  if Present (Address_Clause (Def_Ident)) then
                     LLVM_Type := Pointer_Type (LLVM_Type, 0);
                  end if;

                  LLVM_Var :=
                    G (Add_Global (Env.Mdl, LLVM_Type,
                                   Get_Ext_Name (Def_Ident)),
                       T, Is_Reference => True);
                  Set_Value (Def_Ident, LLVM_Var);

                  if Env.In_Main_Unit then
                     if Is_Statically_Allocated (Def_Ident) then
                        Set_Linkage (LLVM_Value (LLVM_Var), Internal_Linkage);
                     end if;

                     --  ??? This code is probably wrong, but is rare enough
                     --  that we'll worry about it later.

                     if Present (Address_Clause (Def_Ident)) then
                        Set_Initializer
                          (LLVM_Value (LLVM_Var),
                           LLVM_Value (Emit_Expression
                                         (Expression
                                            (Address_Clause (Def_Ident)))));
                        --  ??? Should also take Expression (Node) into account

                     else
                        if Is_Imported (Def_Ident) then
                           Set_Linkage (LLVM_Value (LLVM_Var),
                                        External_Linkage);
                        end if;

                        --  Take Expression (Node) into account

                        if Present (Expression (Node))
                          and then not
                            (Nkind (Node) = N_Object_Declaration
                             and then No_Initialization (Node))
                        then
                           if Compile_Time_Known_Value (Expression (Node)) then
                              Expr := Emit_Expression (Expression (Node));
                              Set_Initializer (LLVM_Value (LLVM_Var),
                                               LLVM_Value (Expr));
                           else
                              Elaboration_Table.Append (Node);

                              if not Is_Imported (Def_Ident) then
                                 Set_Initializer
                                   (LLVM_Value (LLVM_Var),
                                    Const_Null (LLVM_Type));
                              end if;
                           end if;
                        elsif not Is_Imported (Def_Ident) then
                           Set_Initializer (LLVM_Value (LLVM_Var),
                                            Const_Null (LLVM_Type));
                        end if;
                     end if;
                  else
                     Set_Linkage (LLVM_Value (LLVM_Var), External_Linkage);
                  end if;

               else
                  if Env.Special_Elaboration_Code then
                     LLVM_Var := Get_Value (Def_Ident);

                  elsif Present (Address_Clause (Def_Ident)) then
                        LLVM_Var := Int_To_Ref
                          (Emit_Expression
                             (Expression (Address_Clause (Def_Ident))),
                           T, Get_Name (Def_Ident));
                  else
                     LLVM_Var := Allocate_For_Type (T, Get_Name (Def_Ident));

                  end if;

                  Set_Value (Def_Ident, LLVM_Var);
                  if Present (Expression (Node))
                    and then not
                      (Nkind (Node) = N_Object_Declaration
                       and then No_Initialization (Node))
                  then
                     Emit_Assignment (LLVM_Var, Expression (Node),
                                      No_GL_Value, True, True);
                  end if;
               end if;
            end;

         when N_Use_Type_Clause =>
            null;

         when N_Object_Renaming_Declaration =>
            declare
               Def_Ident : constant Node_Id := Defining_Identifier (Node);
               LLVM_Var  : GL_Value;
            begin
               if Library_Level then
                  Set_Value (Def_Ident, Emit_LValue (Name (Node)));
                  return;
               end if;

               --  If the renamed object is already an l-value, keep it as-is.
               --  Otherwise, create one for it.

               LLVM_Var := Emit_LValue (Name (Node));
               if not Is_Reference (LLVM_Var) then
                  declare
                     Temp : constant GL_Value :=
                       Allocate_For_Type
                       (Full_Etype (Def_Ident), Get_Name (Def_Ident));
                  begin
                     Store (LLVM_Var, Temp);
                     LLVM_Var := Temp;
                  end;
               end if;

               Set_Value (Def_Ident, LLVM_Var);
            end;

         when N_Subprogram_Renaming_Declaration =>
            --  Nothing is needed except for debugging information.
            --  Skip it for now???
            --  Note that in any case, we should skip Intrinsic subprograms

            null;

         when N_Implicit_Label_Declaration =>
            --  Don't do anything here in case this label isn't actually
            --  used as a label.  In that case, the basic block we create
            --  here will be empty, which LLVM doesn't allow.  This can't
            --  occur for user-defined labels, but can occur with some
            --  labels placed by the front end.  Instead, lazily create
            --  the basic block where it's placed or when its the target
            --  of a goto.
            null;

         when N_Assignment_Statement =>
            Emit_Assignment (Emit_LValue (Name (Node)), Expression (Node),
                             No_GL_Value, Forwards_OK (Node),
                             Backwards_OK (Node));

         when N_Procedure_Call_Statement =>
            Discard (Emit_Call (Node));

         when N_Null_Statement =>
            null;

         when N_Label =>
            declare
               BB : constant Basic_Block_T :=
                 Get_Label_BB (Entity (Identifier (Node)));
            begin
               Build_Br (BB);
               Position_Builder_At_End (BB);
            end;

         when N_Goto_Statement =>
            Build_Br (Get_Label_BB (Entity (Name (Node))));
            Position_Builder_At_End (Create_Basic_Block ("after-goto"));

         when N_Exit_Statement =>
            declare
               Exit_Point : constant Basic_Block_T :=
                 (if Present (Name (Node))
                  then Get_Exit_Point (Entity (Name (Node)))
                  else Get_Exit_Point);
               Next_BB    : constant Basic_Block_T :=
                 Create_Basic_Block ("loop-after-exit");

            begin
               if Present (Condition (Node)) then
                  Build_Cond_Br (Emit_Expression (Condition (Node)),
                                 Exit_Point, Next_BB);

               else
                  Build_Br (Exit_Point);
               end if;

               Position_Builder_At_End (Next_BB);
            end;

         when N_Simple_Return_Statement =>
            if Present (Expression (Node)) then

               --  If we have a parameter giving the address to which to
               --  copy the return value, do that copy instead of returning
               --  it.

               if Present (Env.Return_Address_Param) then
                  Emit_Assignment (Env.Return_Address_Param, Expression (Node),
                                   No_GL_Value, True, True);

                  Build_Ret_Void;

               else
                  Build_Ret (Build_Type_Conversion
                               (Full_Etype (Node_Enclosing_Subprogram (Node)),
                                Expression (Node)));
               end if;

            else
               Build_Ret_Void;
            end if;

            Position_Builder_At_End (Create_Basic_Block ("unreachable"));

         when N_If_Statement =>
            Emit_If (Node);

         when N_Loop_Statement =>
            declare
               Loop_Identifier   : constant Entity_Id :=
                 (if Present (Identifier (Node))
                  then Entity (Identifier (Node))
                  else Empty);
               Iter_Scheme       : constant Node_Id :=
                 Iteration_Scheme (Node);
               Is_Mere_Loop      : constant Boolean :=
                 not Present (Iter_Scheme);
               Is_For_Loop       : constant Boolean :=
                 not Is_Mere_Loop
                 and then
                   Present (Loop_Parameter_Specification (Iter_Scheme));

               BB_Init, BB_Cond  : Basic_Block_T;
               BB_Stmts, BB_Iter : Basic_Block_T;
               BB_Next           : Basic_Block_T;
               Cond              : GL_Value;
            begin

               --  The general format for a loop is:
               --    INIT;
               --    while COND loop
               --       STMTS;
               --       ITER;
               --    end loop;
               --    NEXT:
               --  Each step has its own basic block. When a loop does not need
               --  one of these steps, just alias it with another one.

               --  If this loop has an identifier, and it has already its own
               --  entry (INIT) basic block. Create one otherwise.

               BB_Init :=
                 (if Present (Identifier (Node))
                    and then Has_BB (Entity (Identifier (Node)))
                  then Get_Basic_Block (Entity (Identifier (Node)))
                  else Create_Basic_Block);
               Build_Br (BB_Init);
               Position_Builder_At_End (BB_Init);

               --  If this is not a FOR loop, there is no initialization: alias
               --  it with the COND block.

               BB_Cond :=
                 (if not Is_For_Loop
                  then BB_Init else Create_Basic_Block ("loop-cond"));

               --  If this is a mere loop, there is even no condition block:
               --  alias it with the STMTS block.

               BB_Stmts :=
                 (if Is_Mere_Loop
                  then BB_Cond else Create_Basic_Block ("loop-stmts"));

               --  If this is not a FOR loop, there is no iteration: alias it
               --  with the COND block, so that at the end of every STMTS, jump
               --  on ITER or COND.

               BB_Iter :=
                 (if Is_For_Loop then Create_Basic_Block ("loop-iter")
                  else BB_Cond);

               --  The NEXT step contains no statement that comes from the
               --  loop: it is the exit point.

               BB_Next := Create_Basic_Block ("loop-exit");

               --  The front-end expansion can produce identifier-less loops,
               --  but exit statements can target them anyway, so register such
               --  loops.

               Push_Loop (Loop_Identifier, BB_Next);

               --  First compile the iterative part of the loop: evaluation of
               --  the exit condition, etc.

               if not Is_Mere_Loop then
                  if not Is_For_Loop then

                     --  This is a WHILE loop: jump to the loop-body if the
                     --  condition evaluates to True, jump to the loop-exit
                     --  otherwise.

                     Position_Builder_At_End (BB_Cond);
                     Cond := Emit_Expression (Condition (Iter_Scheme));
                     Build_Cond_Br (Cond, BB_Stmts, BB_Next);

                  else
                     --  This is a FOR loop

                     declare
                        Loop_Param_Spec : constant Node_Id :=
                          Loop_Parameter_Specification (Iter_Scheme);
                        Def_Ident       : constant Node_Id :=
                          Defining_Identifier (Loop_Param_Spec);
                        Reversed        : constant Boolean :=
                          Reverse_Present (Loop_Param_Spec);
                        Unsigned_Type   : constant Boolean :=
                          Is_Unsigned_Type (Full_Etype (Def_Ident));
                        Var_Type        : constant Entity_Id :=
                          Full_Etype (Def_Ident);
                        LLVM_Type       : Type_T;
                        LLVM_Var        : GL_Value;
                        Low, High       : GL_Value;

                     begin
                        --  Initialization block: create the loop variable and
                        --  initialize it.

                        Create_Discrete_Type (Var_Type, LLVM_Type, Low, High);
                        LLVM_Var := Allocate_For_Type
                          (Var_Type, Get_Name (Def_Ident));
                        Set_Value (Def_Ident, LLVM_Var);
                        Store
                          ((if Reversed then High else Low), LLVM_Var);

                        --  Then go to the condition block if the range isn't
                        --  empty.

                        Cond := I_Cmp
                          ((if Unsigned_Type then Int_ULE else Int_SLE),
                           Low, High,
                           "loop-entry-cond");
                        Build_Cond_Br (Cond, BB_Cond, BB_Next);

                        --  The FOR loop is special: the condition is evaluated
                        --  during the INIT step and right before the ITER
                        --  step, so there is nothing to check during the
                        --  COND step.

                        Position_Builder_At_End (BB_Cond);
                        Build_Br (BB_Stmts);

                        BB_Cond := Create_Basic_Block ("loop-cond-iter");
                        Position_Builder_At_End (BB_Cond);
                        Cond := I_Cmp
                          (Int_EQ, Load (LLVM_Var),
                           (if Reversed then Low else High),
                           "loop-iter-cond");
                        Build_Cond_Br (Cond, BB_Next, BB_Iter);

                        --  After STMTS, stop if the loop variable was equal to
                        --  the "exit" bound. Increment/decrement it otherwise.

                        Position_Builder_At_End (BB_Iter);

                        declare
                           Iter_Prev_Value : constant GL_Value :=
                             Load (LLVM_Var);
                           One             : constant GL_Value :=
                             Const_Int (Var_Type, 1, False);
                           Iter_Next_Value : constant GL_Value :=
                             (if Reversed
                              then NSW_Sub
                                (Iter_Prev_Value, One, "next-loop-var")
                              else NSW_Add
                                (Iter_Prev_Value, One, "next-loop-var"));
                        begin
                           Store (Iter_Next_Value, LLVM_Var);
                        end;

                        Build_Br (BB_Stmts);

                        --  The ITER step starts at this special COND step

                        BB_Iter := BB_Cond;
                     end;
                  end if;
               end if;

               Position_Builder_At_End (BB_Stmts);
               Emit_List (Statements (Node));
               Build_Br (BB_Iter);
               Pop_Loop;

               Position_Builder_At_End (BB_Next);
            end;

         when N_Block_Statement =>
            declare
               BE          : constant Entity_Id :=
                 (if Present (Identifier (Node))
                  then Entity (Identifier (Node))
                  else Empty);
               BB          : Basic_Block_T;
               Stack_State : Value_T;

            begin

               --  The frontend can generate basic blocks with identifiers
               --  that are not declared: try to get any existing basic block,
               --  create and register a new one if it does not exist yet.

               if Has_BB (BE) then
                  BB := Get_Basic_Block (BE);
               else
                  BB := Create_Basic_Block;

                  if Present (BE) then
                     Set_Basic_Block (BE, BB);
                  end if;
               end if;

               Build_Br (BB);
               Position_Builder_At_End (BB);
               Push_Lexical_Debug_Scope (Node);

               Stack_State := Call
                 (Env.Bld,
                  Env.Stack_Save_Fn, System.Null_Address, 0, "");

               Emit_List (Declarations (Node));
               Emit_List (Statements (Handled_Statement_Sequence (Node)));

               Discard
                 (Call
                    (Env.Bld,
                     Env.Stack_Restore_Fn, Stack_State'Address, 1, ""));
               Pop_Debug_Scope;
            end;

         when N_Full_Type_Declaration | N_Subtype_Declaration
            | N_Incomplete_Type_Declaration | N_Private_Type_Declaration
            | N_Private_Extension_Declaration
         =>
            Discard
              (GNAT_To_LLVM_Type (Defining_Identifier (Node), True));

         when N_Freeze_Entity =>
            --  ??? Need to process Node itself

            Emit_List (Actions (Node));

         when N_Pragma =>
            case Get_Pragma_Id (Node) is
               --  ??? While we aren't interested in most of the pragmas,
               --  there are some we should look at (see
               --  trans.c:Pragma_to_gnu). But still, the "others" case is
               --  necessary.

               when others => null;
            end case;

         when N_Case_Statement =>
            Emit_Case (Node);

         when N_Body_Stub =>
            if Nkind_In (Node, N_Protected_Body_Stub, N_Task_Body_Stub) then
               raise Program_Error;
            end if;

            --  No action if the separate unit is not available

            if No (Library_Unit (Node)) then
               Error_Msg_N ("separate unit not available", Node);
            else
               Emit (Get_Body_From_Stub (Node));
            end if;

         --  Nodes we actually want to ignore

         when N_Call_Marker
            | N_Empty
            | N_Enumeration_Representation_Clause
            | N_Enumeration_Type_Definition
            | N_Function_Instantiation
            | N_Freeze_Generic_Entity
            | N_Itype_Reference
            | N_Number_Declaration
            | N_Procedure_Instantiation
            | N_Validate_Unchecked_Conversion
            | N_Variable_Reference_Marker =>
            null;

         when N_Package_Instantiation
            | N_Package_Renaming_Declaration
            | N_Generic_Package_Declaration
            | N_Generic_Subprogram_Declaration
         =>
            if Nkind (Parent (Node)) = N_Compilation_Unit then
               Set_Has_No_Elaboration_Code (Parent (Node), True);
            end if;

         --  ??? Ignore for now

         when N_Push_Constraint_Error_Label .. N_Pop_Storage_Error_Label =>
            null;

         --  ??? Ignore for now

         when N_Exception_Handler =>
            Error_Msg_N ("exception handler ignored??", Node);

         when N_Exception_Renaming_Declaration =>
            Set_Value
              (Defining_Identifier (Node), Get_Value (Entity (Name (Node))));

         when N_Attribute_Definition_Clause =>
            --  The only interesting case left after expansion is for Address
            --  clauses. We only deal with 'Address if the object has a Freeze
            --  node.

            --  ??? For now keep it simple and deal with this case in
            --  N_Object_Declaration.

            if Get_Attribute_Id (Chars (Node)) = Attribute_Address
              and then Present (Freeze_Node (Entity (Name (Node))))
            then
               null;
            end if;

         when others =>
            Error_Msg_N
              ("unhandled statement kind: `" &
               Node_Kind'Image (Nkind (Node)) & "`", Node);
      end case;
   end Emit;

   -----------------
   -- Emit_LValue --
   -----------------

   function Emit_LValue (Node : Node_Id) return GL_Value is
   begin
      Set_Debug_Pos_At_Node (Node);
      LValue_Pair_Table.Set_Last (0);

      --  Each time we start a new recursive call, we free the entries
      --  from the last one.

      return Emit_LValue_Internal (Node);
   end Emit_LValue;

   --------------------------
   -- Emit_LValue_Internal --
   --------------------------

   function Emit_LValue_Internal (Node : Node_Id) return GL_Value
   is
      Typ   : constant Entity_Id := Full_Etype (Node);
      Value : constant GL_Value := Need_LValue (Emit_LValue_Main (Node), Typ);

   begin
      --  If the object is not of void type, save the result in the
      --  pair table under the base type of the fullest view.

      if not Is_Subprogram_Type (Value) and then Ekind (Value) /= E_Void then
         LValue_Pair_Table.Append (Value);
      end if;

      return Value;
   end Emit_LValue_Internal;

   ----------------------
   -- Emit_LValue_Main --
   ----------------------

   function Emit_LValue_Main (Node : Node_Id) return GL_Value is
   begin
      case Nkind (Node) is
         when N_Identifier | N_Expanded_Name | N_Operator_Symbol |
           N_Defining_Identifier | N_Defining_Operator_Symbol =>
            declare
               Def_Ident : Entity_Id := Entity (Node);
               Typ       : Entity_Id := Full_Etype (Def_Ident);

            begin
               --  If this is a deferred constant, look at the private
               --  version.

               if Ekind (Def_Ident) = E_Constant
                 and then Present (Full_View (Def_Ident))
                 and then No (Address_Clause (Def_Ident))
               then
                  Def_Ident := Full_View (Def_Ident);
               end if;

               if Ekind (Def_Ident) in Subprogram_Kind then

                  --  If we are elaborating this for 'Access, we want the
                  --  actual subprogram type here, not the type of the return
                  --  value, which is what Typ is set to.

                  if Nkind (Parent (Node)) = N_Attribute_Reference then
                     Typ := Full_Designated_Type (Full_Etype (Parent (Node)));
                  end if;

                  if not Needs_Activation_Record (Typ) then
                     return Convert_To_Access_To (Get_Value (Def_Ident), Typ);
                  else
                     --  Return a callback, which is a pair: subprogram
                     --  code pointer and static link argument.

                     declare
                        Func   : constant GL_Value := Get_Value (Def_Ident);
                        S_Link : constant GL_Value := Get_Static_Link (Node);
                        Callback_Type : constant Type_T :=
                          Build_Struct_Type
                          ((1 => Type_Of (S_Link), 2 => Type_Of (S_Link)));
                        Result : constant GL_Value :=
                          Get_Undef_Ref (Callback_Type, Typ);

                     begin
                        return Insert_Value
                          (Insert_Value (Result, S_Link, 1),
                           Pointer_Cast (Func, S_Link), 0);
                     end;
                  end if;
               else
                  return Get_Value (Def_Ident);
               end if;
            end;

         when N_Attribute_Reference =>
            return Emit_Attribute_Reference (Node, LValue => True);

         when N_Explicit_Dereference =>
            --  The result of evaluating Emit_Eexpression is the
            --  address of what we want and is an access type.  What
            --  we want here is a reference to our type, which should
            --  be the Designated_Type of Value.

            return Make_Reference (Emit_Expression (Prefix (Node)));

         when N_String_Literal =>
            declare
               T : constant Type_T := Create_Type (Full_Etype (Node));
               V : constant GL_Value :=
                 G (Add_Global (Env.Mdl, T, "str-lit"), Full_Etype (Node),
                    Is_Reference => True);

            begin
               Set_Value (Node, V);
               Set_Initializer (LLVM_Value (V),
                                LLVM_Value (Emit_Expression (Node)));
               Set_Linkage (LLVM_Value (V), Private_Linkage);
               Set_Global_Constant (LLVM_Value (V), True);
               return V;
            end;

         when N_Selected_Component =>
            declare
               Pfx_Ptr : constant GL_Value :=
                 Emit_LValue_Internal (Prefix (Node));
               Record_Component : constant Entity_Id :=
                 Original_Record_Component (Entity (Selector_Name (Node)));

            begin
               return G (Record_Field_Offset (LLVM_Value (Pfx_Ptr),
                                              Record_Component),
                         Full_Etype (Node), Is_Reference => True);
            end;

         when N_Indexed_Component =>
            return Get_Indexed_LValue (Expressions (Node),
                                       Emit_LValue_Internal (Prefix (Node)));

         when N_Slice =>
            return Get_Slice_LValue
              (Full_Etype (Node), Discrete_Range (Node),
               Emit_LValue_Internal (Prefix (Node)));

         when N_Unchecked_Type_Conversion | N_Type_Conversion =>
            --  We have to mark that this is now to be treated as a new type.
            --  This matters if, e.g., the bounds of an array subtype change
            --  (see C46042A).

            return Convert_To_Access_To
              (Emit_LValue_Internal (Expression (Node)), Full_Etype (Node));

         when others =>
            --  If we have an arbitrary expression, evaluate it.  If it
            --  turns out to be a reference (e.g., if the size of our type
            --  is dynamic, we have no more work to do.  Otherwise, our caller
            --  will take care of storing it into a temporary.

            return Emit_Expression (Node);
      end case;
   end Emit_LValue_Main;

   ------------------------
   -- Get_Matching_Value --
   ------------------------

   function Get_Matching_Value (T : Entity_Id) return GL_Value is
   begin
      for I in 1 .. LValue_Pair_Table.Last loop
         if Implementation_Base_Type (T) =
           Implementation_Base_Type (LValue_Pair_Table.Table (I).Typ)
         then
            return LValue_Pair_Table.Table (I);
         end if;
      end loop;

      --  Should never get here and postcondition verifies

      return No_GL_Value;
   end Get_Matching_Value;

   ----------------------------
   -- Build_Short_Circuit_Op --
   ----------------------------

   function Build_Short_Circuit_Op
     (Left, Right : Node_Id; Op : Node_Kind) return GL_Value
   is
      LHS, RHS             : GL_Value;
      --  We start evaluating the LHS in the current block, but we need to
      --  record which block it completes in, since it may not be the
      --  same block.

      Block_Left_Expr_End  : Basic_Block_T;
      --  Block which contains the evaluation of the right part
      --  expression of the operator and its end.

      Block_Right_Expr     : constant Basic_Block_T :=
        Create_Basic_Block ("scl-right-expr");
      Block_Right_Expr_End : Basic_Block_T;
      --  Block containing the exit code (the phi that selects that value)

      Block_Exit           : constant Basic_Block_T :=
        Create_Basic_Block ("scl-exit");

   begin
      --  In the case of And, evaluate the right expression when Left is
      --  true. In the case of Or, evaluate it when Left is false.

      LHS := Emit_Expression (Left);
      Block_Left_Expr_End := Get_Insert_Block (Env.Bld);

      if Op = N_And_Then then
         Build_Cond_Br (LHS, Block_Right_Expr, Block_Exit);
      else
         Build_Cond_Br (LHS, Block_Exit, Block_Right_Expr);
      end if;

      --  Emit code for the evaluation of the right part expression

      Position_Builder_At_End (Block_Right_Expr);
      RHS := Emit_Expression (Right);

      Block_Right_Expr_End := Get_Insert_Block (Env.Bld);
      Build_Br (Block_Exit);

      Position_Builder_At_End (Block_Exit);

      --  If we exited the entry block, it means that for AND, the result
      --  is false and for OR, it's true.  Otherwise, the result is the right.

      declare
         LHS_Const : constant unsigned_long_long :=
           (if Op = N_And_Then then 0 else 1);
      begin
         return Build_Phi
           ((1 => Const_Int (RHS, LHS_Const), 2 => RHS),
            (1 => Block_Left_Expr_End, 2 => Block_Right_Expr_End));
      end;
   end Build_Short_Circuit_Op;

   ---------------------
   -- Emit_Expression --
   ---------------------

   function Emit_Expression (Node : Node_Id) return GL_Value is
   begin
      Set_Debug_Pos_At_Node (Node);
      if Nkind (Node) in N_Binary_Op then

         --  Handle comparisons and shifts with helper functions, then
         --  the rest are by generating the appropriate LLVM IR entry.

         if Nkind (Node) in N_Op_Compare then
            return Emit_Comparison
              (Nkind (Node), Left_Opnd (Node), Right_Opnd (Node));

         elsif Nkind (Node) in N_Op_Shift then
            return Emit_Shift (Node, Left_Opnd (Node), Right_Opnd (Node));
         end if;

         declare
            type Opf is access function
              (LHS, RHS : GL_Value; Name : String := "") return GL_Value;

            Left_Type  : constant Entity_Id := Full_Etype (Left_Opnd (Node));
            Right_Type : constant Entity_Id := Full_Etype (Right_Opnd (Node));
            Left_BT    : constant Entity_Id :=
              Implementation_Base_Type (Left_Type);
            Right_BT   : constant Entity_Id :=
              Implementation_Base_Type (Right_Type);
            LVal       : constant GL_Value :=
              Build_Type_Conversion (Left_BT, Left_Opnd (Node));
            RVal       : constant GL_Value :=
              Build_Type_Conversion (Right_BT, Right_Opnd (Node));
            FP         : constant Boolean := Is_Floating_Point_Type (Left_BT);
            Unsign     : constant Boolean := Is_Unsigned_Type (Left_BT);
            Subp       : Opf := null;
            Result     : GL_Value;

         begin
            case Nkind (Node) is
               when N_Op_Add =>
                  Subp := (if FP then F_Add'Access else NSW_Add'Access);

               when N_Op_Subtract =>
                  Subp := (if FP then F_Sub'Access else NSW_Sub'Access);

               when N_Op_Multiply =>
                  Subp := (if FP then F_Mul'Access else NSW_Mul'Access);

               when N_Op_Divide =>
                  Subp :=
                    (if FP then F_Div'Access
                     elsif Unsign then U_Div'Access else S_Div'Access);

               when N_Op_Rem =>
                  Subp := (if Unsign then U_Rem'Access else S_Rem'Access);

               when N_Op_And =>
                  Subp := Build_And'Access;

               when N_Op_Or =>
                  Subp := Build_Or'Access;

               when N_Op_Xor =>
                  Subp := Build_Xor'Access;

               when N_Op_Mod =>
                  Subp := (if Unsign then U_Rem'Access else S_Rem'Access);

               when others =>
                  null;

            end case;

            Result := Subp (LVal, RVal);

            --  If this is a signed mod operation, we have to adjust the
            --  result, since what we did is a rem operation.  If the result
            --  is zero or the result and the RHS have the same sign, the
            --  result is correct.  Otherwise, we have to add the RHS to
            --  the result.  Two values have the same sign iff their xor
            --  is non-negative, which is the best code for the general case,
            --  but having a variable as the second operand of mod is quite
            --  rare, so it's best to do slightly less efficient code for
            --  then general case that will get constant-folded in the
            --  constant case.

            if not Unsign and Nkind (Node) = N_Op_Mod then
               declare
                  Add_Back      : constant GL_Value :=
                    NSW_Add (Result, RVal, "addback");
                  RHS_Neg       : constant GL_Value :=
                    I_Cmp (Int_SLT, RVal, Const_Null (RVal),
                           "RHS-neg");
                  Result_Nonpos : constant GL_Value :=
                    I_Cmp (Int_SLE, Result, Const_Null (Result),
                           "result-nonpos");
                  Result_Nonneg : constant GL_Value :=
                    I_Cmp (Int_SGE, Result, Const_Null (Result),
                           "result-nonneg");
                  Signs_Same    : constant GL_Value :=
                    Build_Select (RHS_Neg, Result_Nonpos, Result_Nonneg,
                                  "signs-same");
               begin
                  Result := Build_Select (Signs_Same, Result, Add_Back);
               end;

            --  If this is a division operation with Round_Result set, we
            --  have to do that rounding.  There are two different cases,
            --  one for signed and one for unsigned.

            elsif Nkind (Node) = N_Op_Divide
              and then Rounded_Result (Node) and then Unsign
            then
               declare

                  --  We compute the remainder.  If the remainder is greater
                  --  then half of the RHS (e.g., > (RHS + 1) / 2), we add
                  --  one to the result.

                  Remainder       : constant GL_Value :=
                    U_Rem (LVal, RVal);
                  Half_RHS        : constant GL_Value :=
                    L_Shr (NSW_Sub (RVal, Const_Int (RVal, 1)),
                           Const_Int (RVal, 1));
                  Result_Plus_One : constant GL_Value :=
                    NSW_Add (Result, Const_Int (RVal, 1));
                  Need_Adjust     : constant GL_Value :=
                    I_Cmp (Int_UGT, Remainder, Half_RHS);
               begin
                  Result := Build_Select
                    (Need_Adjust, Result_Plus_One, Result);
               end;

            elsif Nkind (Node) = N_Op_Divide
              and then Rounded_Result (Node) and then not Unsign
            then
               declare

                  --  We compute the remainder.  Then it gets more
                  --  complicated.  Like in the mod case, we optimize for
                  --  the case when rhs RHS is a constant.  If twice the
                  --  absolute value of the remainder is greater than the
                  --  RHS, we have to either add or subtract one from the
                  --  result, depending on whether the RHS is positive or
                  --  negative.

                  Remainder        : constant GL_Value := S_Rem (LVal, RVal);
                  Rem_Negative     : constant GL_Value :=
                    I_Cmp (Int_SLT, Remainder, Const_Null (Remainder));
                  Abs_Rem          : constant GL_Value :=
                    Build_Select (Rem_Negative, NSW_Neg (Remainder),
                                  Remainder);
                  RHS_Negative     : constant GL_Value :=
                    I_Cmp (Int_SLT, RVal, Const_Null (RVal));
                  Abs_RHS : constant GL_Value :=
                    Build_Select (RHS_Negative, NSW_Neg (RVal), RVal);
                  Need_Adjust      : constant GL_Value :=
                    I_Cmp (Int_UGE, Shl (Abs_Rem, Const_Int (RVal, 1)),
                           Abs_RHS);
                  Result_Plus_One  : constant GL_Value :=
                    NSW_Add (Result, Const_Int (RVal, 1));
                  Result_Minus_One : constant GL_Value :=
                    NSW_Sub (Result, Const_Int (RVal, 1));
                  Which_Adjust     : constant GL_Value :=
                    Build_Select (RHS_Negative, Result_Minus_One,
                                  Result_Plus_One);

               begin
                  Result := Build_Select (Need_Adjust, Which_Adjust, Result);
               end;
            end if;

            return Result;

         end;

      else
         case Nkind (Node) is
         when N_Expression_With_Actions =>
            Emit_List (Actions (Node));
            return Emit_Expression (Expression (Node));

         when N_Character_Literal | N_Numeric_Or_String_Literal =>
            return Emit_Literal (Node);

         when N_And_Then | N_Or_Else =>
            return Build_Short_Circuit_Op
              (Left_Opnd (Node), Right_Opnd (Node), Nkind (Node));

         when N_Op_Not =>
            return Build_Not (Emit_Expression (Right_Opnd (Node)));

         when N_Op_Abs =>

            --  Emit: X >= 0 ? X : -X;

            declare
               Expr      : constant GL_Value :=
                 Emit_Expression (Right_Opnd (Node));
               Zero      : constant GL_Value := Const_Null (Expr);
               Compare   : constant GL_Value :=
                 Emit_Elementary_Comparison (N_Op_Ge, Expr, Zero);
               Neg_Expr  : constant GL_Value :=
                 (if Is_Floating_Point_Type (Expr)
                  then F_Neg (Expr) else NSW_Neg (Expr));

            begin
               if Is_Unsigned_Type (Expr) then
                  return Expr;
               else
                  return Build_Select (Compare, Expr, Neg_Expr, "abs");
               end if;
            end;

         when N_Op_Plus =>
            return Emit_Expression (Right_Opnd (Node));

         when N_Op_Minus =>
            declare
               Expr : constant GL_Value := Emit_Expression (Right_Opnd (Node));

            begin
               if Is_Floating_Point_Type (Expr) then
                  return F_Neg (Expr);
               else
                  return NSW_Neg (Expr);
               end if;
            end;

         when N_Unchecked_Type_Conversion =>
            return Build_Unchecked_Conversion
              (Full_Etype (Node), Expression (Node));

         when N_Qualified_Expression =>
            --  We can simply strip the type qualifier

            return Emit_Expression (Expression (Node));

         when N_Type_Conversion =>

            return Build_Type_Conversion
              (Full_Etype (Node), Expression (Node));

         when N_Identifier | N_Expanded_Name | N_Operator_Symbol =>
            --  ?? What if Node is a formal parameter passed by reference?
            --  pragma Assert (not Is_Formal (Entity (Node)));

            --  N_Defining_Identifier nodes for enumeration literals are not
            --  stored in the environment. Handle them here.

            declare
               Def_Ident : Entity_Id := Entity (Node);

            begin
               --  If this is a deferred constant, look at the private
               --  version.

               if Ekind (Def_Ident) = E_Constant
                 and then Present (Full_View (Def_Ident))
                 and then No (Address_Clause (Def_Ident))
               then
                  Def_Ident := Full_View (Def_Ident);
               end if;

               if Ekind (Def_Ident) = E_Enumeration_Literal then
                  return Const_Int (Full_Etype (Node),
                                    Enumeration_Rep (Def_Ident));

               --  See if this is an entity that's present in our
               --  activation record.

               elsif Ekind_In (Def_Ident, E_Constant,
                               E_Discriminant,
                               E_In_Parameter,
                               E_In_Out_Parameter,
                               E_Loop_Parameter,
                               E_Out_Parameter,
                               E_Variable)
                 and then Present (Activation_Record_Component (Def_Ident))
                 and then Present (Env.Activation_Rec_Param)
                 and then Get_Value (Scope (Def_Ident)) /= Env.Func
               then
                  declare
                     Component         : constant Entity_Id :=
                       Activation_Record_Component (Def_Ident);
                     Activation_Record : constant GL_Value :=
                       Env.Activation_Rec_Param;
                     Pointer           : constant Value_T :=
                       Record_Field_Offset (LLVM_Value (Activation_Record),
                                            Component);
                     Value_Address     : constant Value_T :=
                       Load (Env.Bld, Pointer, "");
                     Typ               : constant Type_T :=
                       Pointer_Type (Create_Type
                                       (Full_Etype (Def_Ident)), 0);
                     Value_Ptr         : constant Value_T :=
                       Int_To_Ptr (Env.Bld, Value_Address, Typ, "");
                  begin
                     return G (Load (Env.Bld, Value_Ptr, ""),
                               Full_Etype (Def_Ident));
                  end;

               --  Handle entities in Standard and ASCII on the fly

               elsif Sloc (Def_Ident) <= Standard_Location then
                  declare
                     N    : constant Node_Id := Get_Full_View (Def_Ident);
                     Decl : constant Node_Id := Declaration_Node (N);
                     Expr : Node_Id := Empty;

                  begin
                     if Nkind (Decl) /= N_Object_Renaming_Declaration then
                        Expr := Expression (Decl);
                     end if;

                     if Present (Expr)
                       and then Nkind_In (Expr, N_Character_Literal,
                                                N_Expanded_Name,
                                                N_Integer_Literal,
                                                N_Real_Literal)
                     then
                        return Emit_Expression (Expr);

                     elsif Present (Expr)
                       and then Nkind (Expr) = N_Identifier
                       and then Ekind (Entity (Expr)) = E_Enumeration_Literal
                     then
                        return Const_Int (Full_Etype (Node),
                                          Enumeration_Rep (Entity (Expr)));
                     else
                        return Emit_Expression (N);
                     end if;
                  end;

               elsif Nkind (Node) in N_Subexpr
                 and then Is_Constant_Folded (Entity (Node))
               then
                  --  Replace constant references by the direct values, to
                  --  avoid a level of indirection for e.g. private values and
                  --  to allow generation of static values and static
                  --  aggregates.

                  declare
                     N    : constant Node_Id := Get_Full_View (Entity (Node));
                     Decl : constant Node_Id := Declaration_Node (N);
                     Expr : Node_Id := Empty;

                  begin
                     if Nkind (Decl) /= N_Object_Renaming_Declaration then
                        Expr := Expression (Decl);
                     end if;

                     if Present (Expr) then
                        if Nkind_In (Expr, N_Character_Literal,
                                           N_Expanded_Name,
                                           N_Integer_Literal,
                                           N_Real_Literal)
                          or else (Nkind (Expr) = N_Identifier
                                   and then Ekind (Entity (Expr)) =
                                     E_Enumeration_Literal)
                        then
                           return Emit_Expression (Expr);
                        end if;
                     end if;
                  end;
               end if;

               return Need_Value (Get_Value (Def_Ident),
                                  Full_Etype (Def_Ident));
            end;

         when N_Function_Call =>
            return Emit_Call (Node);

         when N_Explicit_Dereference =>
            return Need_Value
              (Make_Reference (Emit_Expression (Prefix (Node))),
               Full_Etype (Node));

         when N_Allocator =>
            if Present (Storage_Pool (Node)) then
               Error_Msg_N ("unsupported form of N_Allocator", Node);
               return Get_Undef (Full_Etype (Node));
            end if;

            declare
               Expr             : constant Node_Id := Expression (Node);
               Typ              : Entity_Id;
               Arg              : array (1 .. 1) of Value_T;
               Value            : GL_Value;
               Result_Type      : constant Entity_Id := Full_Etype (Node);
               Result           : GL_Value;

            begin
               --  There are two cases: the Expression operand can either be
               --  an N_Identifier or Expanded_Name, which must represent a
               --  type, or a N_Qualified_Expression, which contains both
               --  the object type and an initial value for the object.

               if Is_Entity_Name (Expr) then
                  Typ   := Entity (Expr);
                  Value := No_GL_Value;
               else
                  pragma Assert (Nkind (Expr) = N_Qualified_Expression);
                  Typ   := Full_Etype (Expression (Expr));
                  Value := Emit_Expression (Expression (Expr));
               end if;

               Arg := (1 => LLVM_Value
                         (Get_Type_Size (Typ, Value, For_Type => No (Value))));
               Result := G (Call
                              (Env.Bld, Env.Default_Alloc_Fn,
                               Arg'Address, 1, "alloc"),
                            Standard_A_Char);

               --  Convert to a pointer to the type that the thing is suppose
               --  to point to.

               Result := Ptr_To_Ref (Result, Typ);

               --  Now copy the data, if there is any, into the value

               if Nkind (Expr) = N_Qualified_Expression then
                  Emit_Assignment (Result, Empty, Value, True, True);
               end if;

               return Convert_To_Access_To
                 (Result, Full_Designated_Type (Result_Type));
            end;

         when N_Reference =>
            return Emit_LValue (Prefix (Node));

         when N_Attribute_Reference =>
            return Emit_Attribute_Reference (Node, LValue => False);

         when N_Selected_Component | N_Indexed_Component  | N_Slice =>
            return Need_Value (Emit_LValue (Node), Full_Etype (Node));

         when N_Aggregate =>
            if Null_Record_Present (Node) then
               return Const_Null (Full_Etype (Node));
            end if;

            declare
               Agg_Type   : constant Entity_Id := Full_Etype (Node);
               LLVM_Type  : constant Type_T := Create_Type (Agg_Type);
               Result     : Value_T := Get_Undef (LLVM_Type);
               Cur_Index  : Integer := 0;
               Ent        : Entity_Id;
               Expr       : Node_Id;
               Initial_Indices : Index_Array (1 .. 0);

            begin
               if Ekind (Agg_Type) in Record_Kind then

                  --  The GNAT expander will always put fields in the right
                  --  order, so we can ignore Choices (Expr).

                  Expr := First (Component_Associations (Node));
                  while Present (Expr) loop
                     Ent := Entity (First (Choices (Expr)));

                     --  ?? Ignore discriminants that have
                     --  Corresponding_Discriminants in tagged types since
                     --  we'll be setting those fields in the parent subtype.

                     if Ekind (Ent) = E_Discriminant
                       and then Present (Corresponding_Discriminant (Ent))
                       and then Is_Tagged_Type (Scope (Ent))
                     then
                        null;

                     --  Also ignore discriminants of Unchecked_Unions

                     elsif Ekind (Ent) = E_Discriminant
                       and then Is_Unchecked_Union (Agg_Type)
                     then
                        null;
                     else
                        Result := Insert_Value
                          (Env.Bld,
                           Result,
                           LLVM_Value (Emit_Expression (Expression (Expr))),
                           unsigned (Cur_Index),
                           "");
                        Cur_Index := Cur_Index + 1;
                     end if;

                     Expr := Next (Expr);
                  end loop;
               else
                  return Emit_Array_Aggregate
                    (Node, Number_Dimensions (Agg_Type), Initial_Indices,
                     Get_Undef (Agg_Type));
               end if;

               return G (Result, Full_Etype (Node));
            end;

         when N_If_Expression =>
            return Emit_If_Expression (Node);

         when N_Null =>
            return Const_Null (Full_Etype (Node));

         when N_Defining_Identifier | N_Defining_Operator_Symbol =>
            return Get_Value (Node);

         when N_In =>
            declare
               Rng   : Node_Id := Right_Opnd (Node);
               Left  : constant GL_Value :=
                 Emit_Expression (Left_Opnd (Node));

            begin
               pragma Assert (No (Alternatives (Node)));
               pragma Assert (Present (Rng));
               --  The front end guarantees the above

               if Nkind (Rng) = N_Identifier then
                  Rng := Scalar_Range (Full_Etype (Rng));
               end if;

               return Build_And (Emit_Elementary_Comparison
                                   (N_Op_Ge, Left,
                                    Emit_Expression (Low_Bound (Rng))),
                                 Emit_Elementary_Comparison
                                   (N_Op_Le, Left,
                                    Emit_Expression (High_Bound (Rng))));
            end;

         when N_Raise_Expression =>
            Emit_LCH_Call (Node);
            return Get_Undef (Full_Etype (Node));

         when N_Raise_xxx_Error =>
            pragma Assert (No (Condition (Node)));
            Emit_LCH_Call (Node);
            return Get_Undef (Full_Etype (Node));

         when others =>
            Error_Msg_N
              ("unsupported node kind: `" &
               Node_Kind'Image (Nkind (Node)) & "`", Node);
            return Get_Undef (Full_Etype (Node));
         end case;
      end if;
   end Emit_Expression;

   ---------------
   -- Emit_List --
   ---------------

   procedure Emit_List (List : List_Id) is
      N : Node_Id;

   begin
      if Present (List) then
         N := First (List);
         while Present (N) loop
            Emit (N);
            N := Next (N);
         end loop;
      end if;
   end Emit_List;

   -----------------------
   -- Is_Zero_Aggregate --
   -----------------------

   function Is_Zero_Aggregate (Src_Node : Node_Id) return Boolean is
      Inner    : Node_Id;
      Val      : Uint;

   begin
      Inner := Expression (First (Component_Associations (Src_Node)));
      while Nkind (Inner) = N_Aggregate and then Is_Others_Aggregate (Inner)
      loop
         Inner := Expression (First (Component_Associations (Inner)));
      end loop;

      Val := Get_Uint_Value (Inner);
      return Val = Uint_0;
   end Is_Zero_Aggregate;

   ---------------------
   -- Emit_Assignment --
   ---------------------

   procedure Emit_Assignment
     (LValue                    : GL_Value;
      E                         : Node_Id;
      E_Value                   : GL_Value;
      Forwards_OK, Backwards_OK : Boolean)
   is
      Dest      : GL_Value := LValue;
      Dest_Type : constant Entity_Id := Full_Designated_Type (LValue);
      Src       : GL_Value;

   begin

      --  See if we have the special case where we're assigning all zeros.
      --  ?? This should really be in Emit_Array_Aggregate, which should take
      --  an LHS.

      if Is_Array_Type (Full_Designated_Type (LValue)) and then Present (E)
        and then Nkind (E) = N_Aggregate
        and then Is_Others_Aggregate (E) and then Is_Zero_Aggregate (E)
      then
         declare
            Align : constant unsigned :=
              Get_Type_Alignment (Create_Type (Dest_Type));

         begin
            Call (Build_Intrinsic
                    (Memset, "llvm.memset.p0i8.i", Env.Size_Type),
                  (1 => Bit_Cast (Dest, Standard_A_Char),
                   2 => Const_Null (Standard_Short_Short_Integer),
                   3 => Get_Type_Size (Dest_Type, No_GL_Value),
                   4 => Const_Int_32 (unsigned_long_long (Align)),
                   5 => Const_False));  --  Is_Volatile
         end;

      --  We now have three case: where we're copying an object of an
      --  elementary type, where we're copying an object that's not
      --  elementary, but can be copied with a Store instruction, or where
      --  we're copying an object of variable size.

      elsif Is_Elementary_Type (Dest_Type) then

         --  The easy case: convert the source to the destination type and
         --  store it.

         Src := (if No (E_Value) then Emit_Expression (E) else E_Value);
         Store (Convert_To_Elementary_Type (Src, Dest_Type), Dest);

      elsif (Present (E) and then not Is_Dynamic_Size (Full_Etype (E)))
         or else (Present (E_Value) and then not Is_Reference (E_Value))
      then
         Src := (if No (E_Value) then Emit_Expression (E) else E_Value);

         --  Here, we have the situation where the source is of an LLVM
         --  value, but the destiation may or may not be a variable-sized
         --  type.  In that case, since we know the size and know the object
         --  to store, we can convert Dest to the type of the pointer to
         --  Src, which we know is fixed-size, and do the store.

         if Pointer_Type (Type_Of (Src),  0) /= Type_Of (Dest) then
            Dest := Ptr_To_Ref (Dest, Full_Etype (Src));
         end if;

         Store (Src, Dest);

      else
         Src := (if No (E_Value) then Emit_LValue (E) else E_Value);

         --  Otherwise, we have to do a variable-sized copy

         declare
            Size : constant GL_Value := Compute_Size
              (Dest_Type, Full_Designated_Type (Src), No_GL_Value, Src);
            Align : constant unsigned := Compute_Alignment
              (Dest_Type, Full_Designated_Type (Src));
            Func_Name : constant String :=
              (if Forwards_OK and then Backwards_OK
               then "memcpy" else "memmove");

         begin

            --  If this is an array type, we have to point the memcpy/memmove
            --  to the underlying data.  But be sure we've done this after
            --  we've used the fat pointer to compute the size above.

            if Is_Array_Type (Full_Designated_Type (Src)) then
               Dest := Array_Data (Dest);
               Src  := Array_Data (Src);
            end if;

            Call (Build_Intrinsic
                    (Memcpy, "llvm." & Func_Name & ".p0i8.p0i8.i",
                     Env.Size_Type),
                  (1 => Bit_Cast (Dest, Standard_A_Char),
                   2 => Bit_Cast (Src, Standard_A_Char),
                   3 => Size,
                   4 => Const_Int_32 (unsigned_long_long (Align)),
                   5 => Const_False)); -- Is_Volatile
         end;
      end if;
   end Emit_Assignment;

   ------------------
   -- Emit_Min_Max --
   ------------------

   function Emit_Min_Max
     (Exprs       : List_Id;
      Compute_Max : Boolean) return GL_Value
   is
      Left      : constant GL_Value := Emit_Expression (First (Exprs));
      Right     : constant GL_Value := Emit_Expression (Last (Exprs));
      Choose    : constant GL_Value :=
        Emit_Elementary_Comparison
        ((if Compute_Max then N_Op_Gt else N_Op_Lt), Left, Right);

   begin
      return Build_Select (Choose, Left, Right,
                           (if Compute_Max then "max" else "min"));
   end Emit_Min_Max;

   --------------------------
   -- Emit_Array_Aggregate --
   --------------------------

   function Emit_Array_Aggregate
     (Node           : Node_Id;
      Dims_Left      : Pos;
      Indices_So_Far : Index_Array;
      Value_So_Far   : GL_Value) return GL_Value
   is
      Cur_Index  : Integer := 0;
      Cur_Value  : GL_Value := Value_So_Far;
      Expr       : Node_Id;

   begin

      pragma Assert (not Is_Dynamic_Size
                       (Full_Component_Type (Full_Etype (Node))));
      --  The code below, by using Insert_Value, restricts itself to
      --  Components of fixed sizes.  But that's OK because the front end
      --  handles those cases.

      Expr := First (Expressions (Node));
      while Present (Expr) loop

         --  If this is a nested N_Aggregate and we have dimensions left
         --  in the outer array, use recursion to fill in the aggregate.

         if Nkind (Expr) = N_Aggregate and then Dims_Left > 1 then
            Cur_Value := Emit_Array_Aggregate
              (Expr, Dims_Left - 1, Indices_So_Far & (1 => Cur_Index),
               Cur_Value);

         else
            Cur_Value := Insert_Value
              (Cur_Value, Emit_Expression (Expr),
               Indices_So_Far & (1 => Cur_Index));
         end if;

         Cur_Index := Cur_Index + 1;
         Expr := Next (Expr);
      end loop;

      return Cur_Value;
   end Emit_Array_Aggregate;

   ------------------------------
   -- Emit_Attribute_Reference --
   ------------------------------

   function Emit_Attribute_Reference
     (Node : Node_Id; LValue : Boolean) return GL_Value
   is
      Attr : constant Attribute_Id := Get_Attribute_Id (Attribute_Name (Node));
      Typ  : constant Entity_Id := Full_Etype (Node);

   begin
      case Attr is
         when Attribute_Access
            | Attribute_Unchecked_Access
            | Attribute_Unrestricted_Access =>
            --  We store values as pointers, so, getting an access to an
            --  expression is the same thing as getting an LValue, and has
            --  the same constraints.  But we do have to be sure that it's
            --  of the right type.

            return Convert_To_Access_To (Emit_LValue (Prefix (Node)),
                                         Full_Designated_Type (Typ));

         when Attribute_Address =>
            if LValue then
               return Emit_LValue (Prefix (Node));
            else
               return Ptr_To_Int
                 (Emit_LValue (Prefix (Node)), Typ, "attr-address");
            end if;

         when Attribute_Deref =>
            declare
               Expr : constant Node_Id := First (Expressions (Node));
               pragma Assert (Is_Descendant_Of_Address (Full_Etype (Expr)));

               Val : constant GL_Value :=
                 Int_To_Ref (Emit_Expression (Expr), Typ, "attr-deref");

            begin
               if LValue then
                  return Val;
               else
                  return Need_Value (Val, Typ);
               end if;
            end;

         when Attribute_First
            | Attribute_Last
            | Attribute_Length =>

            declare
               Prefix_Type : constant Entity_Id := Full_Etype (Prefix (Node));
               Array_Descr : GL_Value;
               Result      : GL_Value;
               Dim         : constant Nat :=
                 (if Present (Expressions (Node))
                  then UI_To_Int (Intval (First (Expressions (Node)))) - 1
                  else 0);

            begin
               if Is_Scalar_Type (Prefix_Type) then
                  if Attr = Attribute_First then
                     Result := Emit_Expression (Type_Low_Bound (Prefix_Type));
                  elsif Attr = Attribute_Last then
                     Result := Emit_Expression (Type_High_Bound (Prefix_Type));
                  else
                     Error_Msg_N ("unsupported attribute", Node);
                     Result :=
                       Get_Undef (Typ);
                  end if;

               elsif Is_Array_Type (Prefix_Type) then

                  --  If what we're taking the prefix of is a type, we can't
                  --  evaluate it as an expression.

                  if Is_Entity_Name (Prefix (Node))
                    and then Is_Type (Entity (Prefix (Node)))
                  then
                     Array_Descr := No_GL_Value;
                  else
                     Array_Descr := Emit_LValue (Prefix (Node));
                  end if;

                  if Attr = Attribute_Length then
                     Result :=
                       Get_Array_Length (Prefix_Type, Dim, Array_Descr);
                  else
                     Result :=
                       Get_Array_Bound
                       (Prefix_Type, Dim, Attr = Attribute_First, Array_Descr);
                  end if;
               else
                  Error_Msg_N ("unsupported attribute", Node);
                  Result := Get_Undef (Typ);
               end if;

               return Convert_To_Elementary_Type (Result, Typ);
            end;

         when Attribute_Max
            | Attribute_Min =>
            return Emit_Min_Max (Expressions (Node), Attr = Attribute_Max);

         when Attribute_Pos
            | Attribute_Val =>
            pragma Assert (List_Length (Expressions (Node)) = 1);
            return Build_Type_Conversion (Typ, First (Expressions (Node)));

         when Attribute_Succ
            | Attribute_Pred =>
            declare
               Exprs : constant List_Id := Expressions (Node);
               pragma Assert (List_Length (Exprs) = 1);

               Base : constant GL_Value := Emit_Expression (First (Exprs));
               One  : constant GL_Value := Const_Int (Base, Uint_1);

            begin
               return
                 (if Attr = Attribute_Succ
                  then NSW_Add (Base, One, "attr-succ")
                  else NSW_Sub (Base, One, "attr-pred"));
            end;

         when Attribute_Machine =>

            --  ??? For now return the prefix itself. Would need to force a
            --  store in some cases.

            return Emit_Expression (First (Expressions (Node)));

         when Attribute_Alignment =>
            declare
               Pre   : constant Node_Id := Full_Etype (Prefix (Node));
               Align : constant unsigned :=
                 Get_Type_Alignment (Create_Type (Pre));

            begin
               return Const_Int (Typ, unsigned_long_long (Align),
                                 Sign_Extend => False);
            end;

         when Attribute_Size | Attribute_Object_Size | Attribute_Value_Size =>

            --  ?? These aren't quite the same thing, but they're close
            --  enough for quite a while.

            declare

               Pref_Typ   : constant Entity_Id := Full_Etype (Prefix (Node));
               Const_8    : constant GL_Value := Size_Const_Int (8);
               For_Type   : constant Boolean :=
                 (Is_Entity_Name (Prefix (Node))
                    and then Is_Type (Entity (Prefix (Node))));
               Value      : GL_Value := No_GL_Value;

            begin
               if not For_Type then
                  Value := Emit_LValue (Prefix (Node));
               end if;

               return Convert_To_Elementary_Type
                 (NSW_Mul (Get_Type_Size (Pref_Typ, Value, For_Type), Const_8),
                  Typ);
            end;

         when others =>
            Error_Msg_N
              ("unsupported attribute: `" &
               Attribute_Id'Image (Attr) & "`", Node);
            return Get_Undef (Typ);
      end case;
   end Emit_Attribute_Reference;

   ---------------------
   -- Emit_Comparison --
   ---------------------

   function Emit_Comparison
     (Kind : Node_Kind; LHS, RHS : Node_Id) return GL_Value
   is
      Operation    : constant Pred_Mapping := Get_Preds (Kind);
      Operand_Type : constant Entity_Id := Full_Etype (LHS);

   begin
      --  LLVM treats pointers as integers regarding comparison.  But we first
      --  have to see if the pointer has an activation record.  If so,
      --  we just compare the functions, not the activation record.

      if Is_Access_Type (Operand_Type)
        and then Needs_Activation_Record (Full_Designated_Type (Operand_Type))
      then
         return I_Cmp
           (Operation.Unsigned, Subp_Ptr (LHS), Subp_Ptr (RHS));

      elsif Is_Elementary_Type (Operand_Type) then
         return Emit_Elementary_Comparison (Kind, Emit_Expression (LHS),
                                            Emit_Expression (RHS));

      else
         pragma Assert (Is_Array_Type (Operand_Type)
                          and then Operation.Signed in Int_EQ | Int_NE);
         --  The front end expands record type comparisons and array
         --  comparisons for other than equality.

         --  We handle this case by creating two basic blocks and doing
         --  this as a test of the arrays, branching to each if true or false.
         --  We then have a merge block which has a Phi selecting true
         --  or false.

         declare
            False_Val    : constant GL_Value :=
              Const_Int (Standard_Boolean, 0, False);
            True_Val     : constant GL_Value :=
              Const_Int (Standard_Boolean, 1, False);
            BB_True      : constant Basic_Block_T :=
              Create_Basic_Block ("true");
            BB_False     : constant Basic_Block_T :=
              Create_Basic_Block ("false");
            BB_Merge     : constant Basic_Block_T :=
              Create_Basic_Block ("merge");
            Results      : constant GL_Value_Array (1 .. 2) :=
              (1 => (if Kind = N_Op_Eq then True_Val  else False_Val),
               2 => (if Kind = N_Op_Eq then False_Val else True_Val));
            Basic_Blocks : constant Basic_Block_Array (1 .. 2) :=
              (1 => BB_True, 2 => BB_False);

         begin
            --  First emit the comparison and branch to one of the two
            --  blocks.

            Emit_Comparison_And_Branch (N_Op_Eq, LHS, RHS, BB_True, BB_False);

            --  Now have each block branch to the merge point and create the
            --  Phi at the merge point.

            Position_Builder_At_End (BB_True);
            Build_Br (BB_Merge);
            Position_Builder_At_End (BB_False);
            Build_Br (BB_Merge);
            Position_Builder_At_End (BB_Merge);
            return Build_Phi (Results, Basic_Blocks);
         end;

      end if;
   end Emit_Comparison;

   --------------------------------
   -- Emit_Comparison_And_Branch --
   --------------------------------

   procedure Emit_Comparison_And_Branch
     (Kind              : Node_Kind;
      LHS, RHS          : Node_Id;
      BB_True, BB_False : Basic_Block_T)
   is
      Cond : GL_Value;

   begin
      --  Do the array case here, where we have labels, to simplify the
      --  logic and take advantage of the reality that almost all array
      --  comparisons are part of "if" statements.

      if  Is_Array_Type (Full_Etype (LHS)) then
         pragma Assert (Kind = N_Op_Eq or else Kind = N_Op_Ne);
         pragma Assert (Number_Dimensions (Full_Etype (LHS)) =
                          Number_Dimensions (Full_Etype (RHS)));

         declare
            Last_Dim       : constant Nat :=
              Number_Dimensions (Full_Etype (LHS)) - 1;
            LHS_Complexity : constant Natural :=
              Get_Array_Size_Complexity (Full_Etype (LHS));
            RHS_Complexity : constant Natural :=
              Get_Array_Size_Complexity (Full_Etype (LHS));
            Our_LHS        : constant Node_Id :=
              (if LHS_Complexity > RHS_Complexity then LHS else RHS);
            --  To simplify the code below, we arrange things so that the
            --  array with the most complex size is on the LHS.

            Our_RHS        : constant Node_Id :=
              (if LHS_Complexity > RHS_Complexity then RHS else LHS);
            BB_T           : constant Basic_Block_T :=
              (if Kind = N_Op_Eq then BB_True else BB_False);
            BB_F           : constant Basic_Block_T :=
              (if Kind = N_Op_Eq then BB_False else BB_True);
            LHS_Val        : constant GL_Value := Emit_LValue (Our_LHS);
            RHS_Val        : constant GL_Value := Emit_LValue (Our_RHS);
            BB_Next        : Basic_Block_T;
            LHS_Lengths    : GL_Value_Array (0 .. Last_Dim);
            RHS_Lengths    : GL_Value_Array (0 .. Last_Dim);

         begin
            --  There's an obscure case where if both arrays have a
            --  dimension that's zero (whether or not it's the same
            --  dimension in both), the arrays compare true.  This is
            --  tested in C45264A.  If this is a single-dimensional array,
            --  this falls through from the normal computation, but for
            --  multi-dimensional arrays, we have to actually do the test.

            --  Start by getting all the lengths

            for Dim in 0 .. Last_Dim loop
               LHS_Lengths (Dim) :=
                 Get_Array_Length (Full_Designated_Type (LHS_Val),
                                   Dim, LHS_Val);
               RHS_Lengths (Dim) :=
                 Get_Array_Length (Full_Designated_Type (RHS_Val),
                                   Dim, RHS_Val);
            end loop;

            if Last_Dim /= 1 then

               --  RHS is the least complex.  So check its dimensions.
               --  If any are zero, we need to check LHS.  If none are zero
               --  (and hopefully we'll know this at compile-time), we
               --  don't need to check LHS and can go to the next test.

               declare
                  BB_RHS_Has_Zero_Dim : constant Basic_Block_T :=
                    Create_Basic_Block ("rhs-has-0-dim");
                  BB_Continue         : constant Basic_Block_T :=
                    Create_Basic_Block ("normal-tests");
               begin
                  for Dim in 0 .. Last_Dim loop
                     BB_Next :=
                       (if Dim = Last_Dim then BB_Continue
                        else Create_Basic_Block);
                     Cond := Emit_Elementary_Comparison
                       (N_Op_Eq, RHS_Lengths (Dim),
                        Const_Null (RHS_Lengths (Dim)));
                     Build_Cond_Br (Cond, BB_RHS_Has_Zero_Dim, BB_Next);
                     Position_Builder_At_End (BB_Next);
                  end loop;

                  --  Now go to where we know that RHS has a zero dimension
                  --  and see if LHS does as well.

                  Position_Builder_At_End (BB_RHS_Has_Zero_Dim);
                  for Dim in 0 .. Last_Dim loop
                     BB_Next :=
                       (if Dim = Last_Dim then BB_Continue
                        else Create_Basic_Block);
                     Cond := Emit_Elementary_Comparison
                       (N_Op_Eq, LHS_Lengths (Dim),
                        Const_Null (LHS_Lengths (Dim)));
                     Build_Cond_Br (Cond, BB_T, BB_Next);
                     Position_Builder_At_End (BB_Next);
                  end loop;

                  Position_Builder_At_End (BB_Continue);
               end;
            end if;

            --  For each dimension, see if the lengths of the two arrays
            --  are different.  If so, the comparison is false.

            --  We need to be careful with types here: LHS and RHS are
            --  the actual array types, but, because we called Emit_LValue,
            --  LHS_Val and RHS_Val are actually references to the array,
            --  not the array.

            for Dim in 0 .. Number_Dimensions (Full_Etype (LHS)) - 1 loop
               BB_Next := Create_Basic_Block;
               Cond := Emit_Elementary_Comparison
                 (N_Op_Eq, LHS_Lengths (Dim), RHS_Lengths (Dim));
               Build_Cond_Br (Cond, BB_Next, BB_F);
               Position_Builder_At_End (BB_Next);
            end loop;

            declare

               --  Now we need to get the size of the array (in bytes)
               --  to do the memory comparison.  Memcmp is defined as
               --  returning zero for a zero size, so we don't need to worry
               --  about testing for that case.

               Size : constant GL_Value :=
                 Compute_Size (Full_Designated_Type (LHS_Val),
                               Full_Designated_Type (RHS_Val),
                               LHS_Val, RHS_Val);
               Void_Ptr_Type : constant Type_T := Pointer_Type (Int_Ty (8), 0);
               Memcmp_Args : constant Value_Array (1 .. 3) :=
                 (1 => Bit_Cast (Env.Bld,
                                 LLVM_Value (Array_Data (LHS_Val)),
                                 Void_Ptr_Type, ""),
                  2 => Bit_Cast (Env.Bld,
                                 LLVM_Value (Array_Data (RHS_Val)),
                                 Void_Ptr_Type, ""),
                  3 => LLVM_Value (Size));
               Memcmp      : constant Value_T := Call
                 (Env.Bld, Env.Memory_Cmp_Fn,
                  Memcmp_Args'Address, Memcmp_Args'Length, "");
               Cond : constant GL_Value :=
                 I_Cmp (Int_EQ, G (Memcmp, Env.Size_Type),
                        Const_Int (Standard_Integer, 0));

            begin
               Build_Cond_Br (Cond, BB_T, BB_F);
            end;
         end;

      --  And now we have the other cases.  We do have to be careful in how
      --  the tests work that we don't have infinite mutual recursion.

      else
         Cond := Emit_Comparison (Kind, LHS, RHS);
         Build_Cond_Br (Cond, BB_True, BB_False);
      end if;
   end Emit_Comparison_And_Branch;

   --------------------------------
   -- Emit_Elementary_Comparison --
   --------------------------------

   function Emit_Elementary_Comparison
     (Kind               : Node_Kind;
      Orig_LHS, Orig_RHS : GL_Value) return GL_Value
   is
      Operation    : constant Pred_Mapping := Get_Preds (Kind);
      LHS          : GL_Value := Orig_LHS;
      RHS          : GL_Value := Orig_RHS;

   begin
      --  If a scalar type (meaning both must be), convert each operand to
      --  its base type.

      if Is_Scalar_Type (LHS) then
         LHS := Convert_To_Elementary_Type (LHS,
                                            Implementation_Base_Type (LHS));
         RHS := Convert_To_Elementary_Type (RHS,
                                            Implementation_Base_Type (RHS));
      end if;

      --  If one is a fat pointer and one isn't, get a raw pointer for the
      --  one that isn't.

      if Is_Access_Unconstrained (LHS)
        and then not Is_Access_Unconstrained (RHS)
      then
         LHS := Array_Data (LHS);
      elsif Is_Access_Unconstrained (RHS)
        and then not Is_Access_Unconstrained (LHS)
      then
         RHS := Array_Data (RHS);
      end if;

      --  If these are fat pointers (because of the above, we know that if
      --  one is, both must be), they are equal iff their addresses are
      --  equal.  It's not possible for the addresses to be equal and not
      --  the bounds. We can't make a recursive call here or we'll try to
      --  do it again that time.

      if Is_Access_Unconstrained (LHS) then
         return I_Cmp (Operation.Unsigned, Array_Data (LHS), Array_Data (RHS));

      elsif Is_Floating_Point_Type (LHS) then
         return F_Cmp (Operation.Real, LHS, RHS);

      else

         --  The only case left is integer or normal access type.

         pragma Assert (Is_Discrete_Or_Fixed_Point_Type (LHS)
                          or else Is_Access_Type (LHS));

         --  At this point, if LHS is an access type, then RHS is too and
         --  we know the aren't pointers to unconstrained arrays.  It's
         --  possible that the two pointer types aren't the same, however.
         --  So in that case, convert one to the pointer of the other.
         --  ?? We do this at low-level since the pointer cast operations
         --  on GL_Value don't quite do exactly the right thing yet.

         if Is_Access_Type (LHS) and then Type_Of (RHS) /= Type_Of (LHS) then
            RHS.Value := Pointer_Cast (Env.Bld, LLVM_Value (RHS),
                                       Type_Of (LHS), "");
         end if;

         return I_Cmp
           ((if Is_Unsigned_Type (LHS) or else Is_Access_Type (LHS)
             then Operation.Unsigned
             else Operation.Signed),
            LHS, RHS);

      end if;
   end Emit_Elementary_Comparison;

   ---------------
   -- Emit_Case --
   ---------------

   procedure Emit_Case (Node : Node_Id) is

      function Count_Choices (Node : Node_Id) return Nat;
      --  Count the total number of choices in this case statement.

      -------------------
      -- Count_Choices --
      -------------------

      function Count_Choices (Node : Node_Id) return Nat is
         Num_Choices  : Nat := 0;
         Alt          : Node_Id;
         First_Choice : Node_Id;
      begin
         Alt := First (Alternatives (Node));
         while Present (Alt) loop

            --  We have a peculiarity in the "others" case of a case statement.
            --  The Alternative points to a list of choices of which the
            --  first choice is an N_Others_Choice.  So handle  that specially
            --  both here and when we compute our Choices below.

            First_Choice := First (Discrete_Choices (Alt));
            Num_Choices := Num_Choices +
              (if Nkind (First_Choice) = N_Others_Choice
               then List_Length (Others_Discrete_Choices (First_Choice))
               else List_Length (Discrete_Choices (Alt)));
            Alt := Next (Alt);
         end loop;

         return Num_Choices;
      end Count_Choices;

      --  We have data structures to record information about each choice
      --  and each alternative in the case statement.  For each choice, we
      --  record the bounds and costs.  The "if" cost is one if both bounds
      --  are the same, otherwise two.  The "switch" cost is the size of the
      --  range, if known and fits in an integer, otherwise a large number
      --  (we arbitrary use 1000).  For the alternative, we record the
      --  basic block in which we've emitted the relevant code, the basic
      --  block we'll use for the test (in the "if" case), the first and
      --  last choice, and the total costs for all the choices in this
      --  alternative.

      type One_Choice is record
         Low, High            : Uint;
         If_Cost, Switch_Cost : Nat;
      end record;

      type One_Alt is record
         BB                        : Basic_Block_T;
         First_Choice, Last_Choice : Nat;
         If_Cost, Switch_Cost      : Nat;
      end record;

      Num_Alts         : constant Nat := List_Length (Alternatives (Node));
      Alts             : array (1 .. Num_Alts) of One_Alt;
      Choices          : array (1 .. Count_Choices (Node)) of One_Choice;
      LHS              : constant GL_Value :=
        Emit_Expression (Expression (Node));
      Typ              : constant Type_T := Create_Type (Full_Etype (LHS));
      Start_BB         : constant Basic_Block_T := Get_Insert_Block (Env.Bld);
      Current_Alt      : Nat := 1;
      First_Choice     : Nat;
      Current_Choice   : Nat := 1;
      Alt, Choice      : Node_Id;
      Low, High        : Uint;
      If_Cost          : Nat;
      Switch_Cost      : Nat;
      BB               : Basic_Block_T;
      BB_End           : constant Basic_Block_T :=
        Create_Basic_Block ("switch-end");
      Switch           : Value_T;

      procedure Swap_Highest_Cost (Is_Switch : Boolean);
      --  Move the highest-cost alternative to the last entry.  Is_Switch
      --  says whether we look at the switch cost or the if cost.

      procedure Swap_Highest_Cost (Is_Switch : Boolean) is
         Temp_Alt         : One_Alt;
         Worst_Alt        : Nat;
         Worst_Cost       : Nat;
         Our_Cost         : Nat;
      begin
         Worst_Alt := Alts'Last;
         Worst_Cost := 0;
         for I in Alts'Range loop
            Our_Cost := (if Is_Switch then Alts (I).Switch_Cost
                         else Alts (I).If_Cost);
            if Our_Cost > Worst_Cost then
               Worst_Cost := Our_Cost;
               Worst_Alt := I;
            end if;
         end loop;

         Temp_Alt := Alts (Alts'Last);
         Alts (Alts'Last) := Alts (Worst_Alt);
         Alts (Worst_Alt) := Temp_Alt;
      end Swap_Highest_Cost;

   begin
      --  First we scan all the alternatives and choices and fill in most
      --  of the data.  We emit the code for each alternative as part of
      --  that process.

      Alt := First (Alternatives (Node));
      while Present (Alt) loop
         First_Choice := Current_Choice;
         BB := Create_Basic_Block ("case-alt");
         Position_Builder_At_End (BB);
         Emit_List (Statements (Alt));
         Build_Br (BB_End);

         Choice := First (Discrete_Choices (Alt));
         if Nkind (Choice) = N_Others_Choice then
            Choice := First (Others_Discrete_Choices (Choice));
         end if;

         while Present (Choice) loop
            Decode_Range (Choice, Low, High);

            --  When we compute the cost, set the cost of a null range
            --  to zero.  If the if cost is 0 or 1, that's the switch cost too,
            --  but if either of the bounds aren't in Int, we can't use
            --  switch at all.

            If_Cost := (if Low > High then 0 elsif Low = High then 1 else 2);

            Switch_Cost := (if not UI_Is_In_Int_Range (Low)
                              or else not UI_Is_In_Int_Range (High)
                            then 1000
                            elsif If_Cost <= 1 then If_Cost
                            elsif Integer (UI_To_Int (Low)) /= Integer'First
                              and then Integer (UI_To_Int (High)) /=
                                         Integer'Last
                              and then UI_To_Int (High) - UI_To_Int (Low) <
                                         1000
                            then UI_To_Int (High) - UI_To_Int (Low) + 1
                            else 1000);
            Choices (Current_Choice) := (Low => Low, High => High,
                                         If_Cost => If_Cost,
                                         Switch_Cost => Switch_Cost);
            Current_Choice := Current_Choice + 1;
            Choice := Next (Choice);
         end loop;

         If_Cost := 0;
         Switch_Cost := 0;

         --  Sum up the costs of all the choices in this alternative

         for I in First_Choice .. Current_Choice - 1 loop
            If_Cost := If_Cost + Choices (I).If_Cost;
            Switch_Cost := Switch_Cost + Choices (I).Switch_Cost;
         end loop;

         Alts (Current_Alt) := (BB => BB, First_Choice => First_Choice,
                                Last_Choice => Current_Choice - 1,
                                If_Cost => If_Cost,
                                Switch_Cost => Switch_Cost);
         Current_Alt := Current_Alt + 1;
         Alt := Next (Alt);
      end loop;

      --  We have two strategies: we can use an LLVM switch instruction if
      --  there aren't too many choices.  If not, we use "if".  First we
      --  find the alternative with the largest switch cost and make that
      --  the "others" option.  Then we see if the total cost of the remaining
      --  alternatives is low enough (we use 100).  If so, use that approach.

      Swap_Highest_Cost (True);
      Position_Builder_At_End (Start_BB);
      Switch_Cost := 0;
      for I in Alts'First .. Alts'Last - 1 loop
         Switch_Cost := Switch_Cost + Alts (I).Switch_Cost;
      end loop;

      if Switch_Cost < 100 then

         --  First we emit the actual "switch" statement, then we add
         --  the cases to it.  Here we collect all the basic blocks.

         declare
            BBs : array (Alts'Range) of Basic_Block_T;
         begin
            for I in BBs'Range loop
               BBs (I) := Alts (I).BB;
            end loop;

            Switch := Build_Switch (Env.Bld, LLVM_Value (LHS),
                                    BBs (BBs'Last), BBs'Length);
            for I in Alts'First .. Alts'Last - 1 loop
               for J in Alts (I).First_Choice .. Alts (I).Last_Choice loop
                  for K in UI_To_Int (Choices (J).Low) ..
                    UI_To_Int (Choices (J).High) loop
                     Add_Case (Switch,
                               Const_Int (Typ,
                                          unsigned_long_long (Integer (K)),
                                          Sign_Extend => True),
                               Alts (I).BB);
                  end loop;
               end loop;
            end loop;
         end;

      else
         --  Otherwise, we generate if/elsif/elsif/else

         Swap_Highest_Cost (False);
         for I in Alts'First .. Alts'Last - 1 loop
            for J in Alts (I).First_Choice .. Alts (I).Last_Choice loop

               --  Only do something if this is not a null range

               if Choices (J).If_Cost /= 0 then

                  --  If we're processing the very last choice, then
                  --  if the choice is not a match, we go to "others".
                  --  Otherwise, we go to a new basic block that's the
                  --  next choice.  Note that we can't simply test
                  --  against Choices'Last because we may have swapped
                  --  some other alternative with Alts'Last.

                  if I = Alts'Last - 1 and then J = Alts (I).Last_Choice then
                     BB := Alts (Alts'Last).BB;
                  else
                     BB := Create_Basic_Block ("case-when");
                  end if;

                  Emit_If_Range (Node, LHS, Choices (J).Low, Choices (J).High,
                                 Alts (I).BB, BB);
                  Position_Builder_At_End (BB);
               end if;
            end loop;
         end loop;
      end if;

      Position_Builder_At_End (BB_End);
   end Emit_Case;

   -------------
   -- Emit_If --
   -------------

   procedure Emit_If (Node : Node_Id) is

      --  Record information about each part of an "if" statement

      type If_Ent is record
         Cond     : Node_Id;         --  Expression to test.
         Stmts    : List_Id;         --  Statements to emit if true.
         BB_True  : Basic_Block_T;   --  Basic block to branch for true.
         BB_False : Basic_Block_T;   --  Basic block to branch for false.
      end record;

      If_Parts     : array (0 .. List_Length (Elsif_Parts (Node))) of If_Ent;

      BB_End       : Basic_Block_T;
      If_Parts_Pos : Nat := 1;
      Elsif_Part   : Node_Id;

   begin
      --  First go through all the parts of the "if" statement recording
      --  the expressions and statements.

      If_Parts (0) := (Cond => Condition (Node),
                       Stmts => Then_Statements (Node),
                       BB_True => Create_Basic_Block ("true"),
                       BB_False => Create_Basic_Block ("false"));

      if Present (Elsif_Parts (Node)) then
         Elsif_Part := First (Elsif_Parts (Node));
         while Present (Elsif_Part) loop
            If_Parts (If_Parts_Pos) := (Cond => Condition (Elsif_Part),
                                        Stmts => Then_Statements (Elsif_Part),
                                        BB_True => Create_Basic_Block ("true"),
                                        BB_False =>
                                          Create_Basic_Block ("false"));
            If_Parts_Pos := If_Parts_Pos + 1;
            Elsif_Part := Next (Elsif_Part);
         end loop;
      end if;

      --  When done, each part goes to the end of the statement.  If there's
      --  an "else" clause, it's a new basic block and the end; otherwise,
      --  it's the last False block.

      BB_End := (if Present (Else_Statements (Node))
                 then Create_Basic_Block ("end")
                 else If_Parts (If_Parts_Pos - 1).BB_False);

      --  Now process each entry that we made: test the condition and branch;
      --  emit the statements in the appropriate block; branch to the end;
      --  and set up the block for the next test, the "else", or next
      --  statement.

      for Part of If_Parts loop
         Emit_If_Cond (Part.Cond, Part.BB_True, Part.BB_False);
         Position_Builder_At_End (Part.BB_True);
         Emit_List (Part.Stmts);
         Build_Br (BB_End);
         Position_Builder_At_End (Part.BB_False);
      end loop;

      --  If there's an Else part, emit it and go into the "end" basic block

      if Present (Else_Statements (Node)) then
         Emit_List (Else_Statements (Node));
         Build_Br (BB_End);
         Position_Builder_At_End (BB_End);
      end if;

   end Emit_If;

   ------------------
   -- Emit_If_Cond --
   ------------------

   procedure Emit_If_Cond (Cond : Node_Id; BB_True, BB_False : Basic_Block_T)
   is
      BB_New : Basic_Block_T;

   begin
      case Nkind (Cond) is

         --  Process operations that we can handle in terms of different branch
         --  mechanisms, such as short-circuit operators.

         when N_Op_Not =>
            Emit_If_Cond (Right_Opnd (Cond), BB_False, BB_True);
            return;

         when N_And_Then | N_Or_Else =>
            --  Depending on the result of the the test of the left operand,
            --  we either go to a final basic block or to a new intermediate
            --  one where we test the right operand.

            BB_New := Create_Basic_Block ("short-circuit");
            Emit_If_Cond (Left_Opnd (Cond),
                          (if Nkind (Cond) = N_And_Then
                           then BB_New else BB_True),
                          (if Nkind (Cond) = N_And_Then
                           then BB_False else BB_New));
            Position_Builder_At_End (BB_New);
            Emit_If_Cond (Right_Opnd (Cond), BB_True, BB_False);
            return;

         when N_Op_Compare =>
            Emit_Comparison_And_Branch (Nkind (Cond),
                                        Left_Opnd (Cond), Right_Opnd (Cond),
                                        BB_True, BB_False);
            return;

         when N_In | N_Not_In =>
            --  If we can decode the range into Uint's, we can just do
            --  simple comparisons.

            declare
               Low, High     : Uint;
            begin
               Decode_Range (Right_Opnd (Cond), Low, High);
               if Low /= No_Uint and then High /= No_Uint then
                  Emit_If_Range
                    (Cond, Emit_Expression (Left_Opnd (Cond)),
                     Low, High,
                     (if Nkind (Cond) = N_In then BB_True else BB_False),
                     (if Nkind (Cond) = N_In then BB_False else BB_True));
                  return;
               end if;
            end;

         when others =>
            null;

      end case;

      --  If we haven't handled it via one of the special cases above,
      --  just evaluate the expression and do the branch.

      Build_Cond_Br (Emit_Expression (Cond), BB_True, BB_False);

   end Emit_If_Cond;

   -------------------
   -- Emit_If_Range --
   -------------------

   procedure Emit_If_Range
     (Node              : Node_Id;
      LHS               : GL_Value;
      Low, High         : Uint;
      BB_True, BB_False : Basic_Block_T)
   is
      Cond              : GL_Value;
      Inner_BB          : Basic_Block_T;

   begin
      --  For discrete types (all we handle here), handle ranges by testing
      --  against the high and the low and branching as appropriate.  We
      --  must be sure to evaluate the LHS only once.  But first check for
      --  a range of size one since that's only one comparison.

      if Low = High then
         Cond := Emit_Elementary_Comparison
           (N_Op_Eq, LHS, Const_Int (LHS, Low));
         Build_Cond_Br (Cond, BB_True, BB_False);
      else
         Inner_BB := Create_Basic_Block ("range-test");
         Cond := Emit_Elementary_Comparison (N_Op_Ge, LHS,
                                             Const_Int (LHS, Low));
         Build_Cond_Br (Cond, Inner_BB, BB_False);
         Position_Builder_At_End (Inner_BB);
         Cond := Emit_Elementary_Comparison (N_Op_Le, LHS,
                                             Const_Int (LHS, High));
         Build_Cond_Br (Cond, BB_True, BB_False);
      end if;
   end Emit_If_Range;

   ------------------------
   -- Emit_If_Expression --
   ------------------------

   function Emit_If_Expression (Node : Node_Id) return GL_Value
   is
      Condition  : constant Node_Id := First (Expressions (Node));
      Then_Expr  : constant Node_Id := Next (Condition);
      Else_Expr  : constant Node_Id := Next (Then_Expr);

      BB_Then, BB_Else, BB_Next : Basic_Block_T;
      --  BB_Then is the basic block we jump to if the condition is true.
      --  BB_Else is the basic block we jump to if the condition is false.
      --  BB_Next is the BB we jump to after the IF is executed.

      Then_Value, Else_Value : GL_Value;

   begin
      BB_Then := Create_Basic_Block ("if-then");
      BB_Else := Create_Basic_Block ("if-else");
      BB_Next := Create_Basic_Block ("if-next");
      Build_Cond_Br (Emit_Expression (Condition), BB_Then, BB_Else);

      --  Emit code for the THEN part

      Position_Builder_At_End (BB_Then);
      Then_Value := Emit_Expression (Then_Expr);

      --  The THEN part may be composed of multiple basic blocks. We want
      --  to get the one that jumps to the merge point to get the PHI node
      --  predecessor.

      BB_Then := Get_Insert_Block (Env.Bld);
      Build_Br (BB_Next);

      --  Emit code for the ELSE part

      Position_Builder_At_End (BB_Else);
      Else_Value := Emit_Expression (Else_Expr);
      Build_Br (BB_Next);

      --  We want to get the basic blocks that jumps to the merge point: see
      --  above.

      BB_Else := Get_Insert_Block (Env.Bld);

      --  Then prepare the instruction builder for the next
      --  statements/expressions and return a merged expression if needed.

      Position_Builder_At_End (BB_Next);
      return Build_Phi ((1 => Then_Value, 2 => Else_Value),
                        (1 => BB_Then, 2 => BB_Else));
   end Emit_If_Expression;

   ------------------
   -- Emit_Literal --
   ------------------

   function Emit_Literal (Node : Node_Id) return GL_Value is
   begin
      case Nkind (Node) is
         when N_Character_Literal =>

            --  If a Entity is present, it means that this was one of the
            --  literals in a user-defined character type.

            return Const_Int (Full_Etype (Node),
                              (if Present (Entity (Node))
                               then Enumeration_Rep (Entity (Node))
                               else Char_Literal_Value (Node)));

         when N_Integer_Literal =>
            return Const_Int (Full_Etype (Node), Intval (Node));

         when N_Real_Literal =>
            if Is_Fixed_Point_Type (Full_Etype (Node)) then
               return Const_Int (Full_Etype (Node),
                                 Corresponding_Integer_Value (Node));
            else
               declare
                  Real_Type        : constant Entity_Id := Full_Etype (Node);
                  Val              : Ureal := Realval (Node);
                  FP_Num, FP_Denom : double;

               begin
                  if UR_Is_Zero (Val) then
                     return Const_Real (Real_Type, 0.0);
                  end if;

                  --  First convert the value to a machine number if it isn't
                  --  already. That will force the base to 2 for non-zero
                  --  values and simplify the rest of the logic.

                  if not Is_Machine_Number (Node) then
                     Val := Machine
                       (Implementation_Base_Type (Full_Etype (Node)),
                        Val, Round_Even, Node);
                  end if;

                  pragma Assert (Rbase (Val) = 2);

                  --  ??? This code is not necessarily the most efficient,
                  --  may not give full precision in all cases, and may not
                  --  handle denormalized constants, but should work in enough
                  --  cases for now.

                  FP_Num :=
                    double (UI_To_Long_Long_Integer (Numerator (Val)));
                  if UR_Is_Negative (Val) then
                     FP_Num := -FP_Num;
                  end if;

                  FP_Denom :=
                    2.0 ** (Integer (-UI_To_Int (Denominator (Val))));
                  return Const_Real (Real_Type, FP_Num * FP_Denom);
               end;
            end if;

         when N_String_Literal =>
            declare
               String       : constant String_Id := Strval (Node);
               Array_Type   : constant Type_T :=
                 Create_Type (Full_Etype (Node));
               Element_Type : constant Type_T := Get_Element_Type (Array_Type);
               Length       : constant Interfaces.C.unsigned :=
                 Get_Array_Length (Array_Type);
               Elements     : array (1 .. Length) of Value_T;

            begin
               for J in Elements'Range loop
                  Elements (J) := Const_Int
                    (Element_Type,
                     unsigned_long_long
                       (Get_String_Char (String, Standard.Types.Int (J))),
                     Sign_Extend => False);
               end loop;

               return G (Const_Array (Element_Type, Elements'Address, Length),
                         Full_Etype (Node));
            end;

         when others =>
            Error_Msg_N ("unhandled literal node", Node);
            return Get_Undef (Full_Etype (Node));

      end case;
   end Emit_Literal;

   ----------------
   -- Emit_Shift --
   ----------------

   function Emit_Shift
     (Node                : Node_Id;
      LHS_Node, RHS_Node  : Node_Id) return GL_Value
   is
      To_Left, Rotate, Arithmetic : Boolean := False;

      LHS       : constant GL_Value := Emit_Expression (LHS_Node);
      RHS       : constant GL_Value := Emit_Expression (RHS_Node);
      Operation : constant Node_Kind := Nkind (Node);
      Result    : GL_Value := LHS;
      N         : constant GL_Value := Convert_To_Elementary_Type (RHS, LHS);
      LHS_Size  : constant GL_Value := Get_LLVM_Type_Size_In_Bits (LHS);
      LHS_Bits  : constant GL_Value :=
        Convert_To_Elementary_Type (LHS_Size, LHS);
      Saturated : GL_Value;

   begin
      --  Extract properties for the operation we are asked to generate code
      --  for.  We defaulted to a right shift above.

      case Operation is
         when N_Op_Shift_Left =>
            To_Left := True;
         when N_Op_Shift_Right_Arithmetic =>
            Arithmetic := True;
         when N_Op_Rotate_Left =>
            To_Left := True;
            Rotate := True;
         when N_Op_Rotate_Right =>
            Rotate := True;
         when others =>
            null;
      end case;

      if Rotate then

         --  LLVM instructions will return an undefined value for
         --  rotations with too many bits, so we must handle "multiple
         --  turns".  However, the front-end has already computed the modulus.

         declare
            --  There is no "rotate" instruction in LLVM, so we have to stick
            --  to shift instructions, just like in C. If we consider that we
            --  are rotating to the left:
            --
            --     Result := (Operand << Bits) | (Operand >> (Size - Bits));
            --               -----------------   --------------------------
            --                    Upper                   Lower
            --
            --  If we are rotating to the right, we switch the direction of the
            --  two shifts.

            Lower_Shift : constant GL_Value :=
              NSW_Sub (LHS_Bits, N, "lower-shift");
            Upper       : constant GL_Value :=
              (if To_Left
               then Shl (LHS, N, "rotate-upper")
               else L_Shr (LHS, N, "rotate-upper"));
            Lower       : constant GL_Value :=
              (if To_Left
               then L_Shr (LHS, Lower_Shift, "rotate-lower")
               else Shl (LHS, Lower_Shift, "rotate-lower"));

         begin
            return Build_Or (Upper, Lower, "rotate-result");
         end;

      else
         --  If the number of bits shifted is bigger or equal than the number
         --  of bits in LHS, the underlying LLVM instruction returns an
         --  undefined value, so build what we want ourselves (we call this
         --  a "saturated value").

         Saturated :=
           (if Arithmetic

            --  If we are performing an arithmetic shift, the saturated value
            --  is 0 if LHS is positive, -1 otherwise (in this context, LHS is
            --  always interpreted as a signed integer).

            then Build_Select
              (C_If   => I_Cmp
                 (Int_SLT, LHS, Const_Null (LHS), "is-lhs-negative"),
               C_Then => Const_Ones (LHS),
               C_Else => Const_Null (LHS),
               Name   => "saturated")

            else Const_Null (LHS));

         --  Now, compute the value using the underlying LLVM instruction

         Result :=
           (if To_Left
            then Shl (LHS, N)
            else
              (if Arithmetic
               then A_Shr (LHS, N) else L_Shr (LHS, N)));

         --  Now, we must decide at runtime if it is safe to rely on the
         --  underlying LLVM instruction. If so, use it, otherwise return
         --  the saturated value.

         return Build_Select
           (C_If   => I_Cmp (Int_UGE, N, LHS_Bits, "is-saturated"),
            C_Then => Saturated,
            C_Else => Result,
            Name   => "shift-rotate-result");
      end if;
   end Emit_Shift;

   ------------------
   -- Get_Label_BB --
   ------------------

   function Get_Label_BB (E : Entity_Id) return Basic_Block_T is
      BB : Basic_Block_T := Get_Basic_Block (E);

   begin
      if No (BB) then
         BB := Create_Basic_Block (Get_Name (E));
         Set_Basic_Block (E, BB);
      end if;

      return BB;
   end Get_Label_BB;

end GNATLLVM.Compile;
