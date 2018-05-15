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

with Errout;   use Errout;
with Exp_Code; use Exp_Code;
with Exp_Util; use Exp_Util;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Sem_Aggr; use Sem_Aggr;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Uintp;    use Uintp;

with LLVM.Core;     use LLVM.Core;

with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Blocks;       use GNATLLVM.Blocks;
with GNATLLVM.Conditionals; use GNATLLVM.Conditionals;
with GNATLLVM.DebugInfo;    use GNATLLVM.DebugInfo;
with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.Exprs;        use GNATLLVM.Exprs;
with GNATLLVM.Records;      use GNATLLVM.Records;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Subprograms;  use GNATLLVM.Subprograms;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Variables;    use GNATLLVM.Variables;

package body GNATLLVM.Compile is

   --  Note: in order to find the right LLVM instruction to generate,
   --  you can compare with what Clang generates on corresponding C or C++
   --  code. This can be done online via http://ellcc.org/demo/index.cgi

   --  See also DragonEgg sources for comparison on how GCC nodes are converted
   --  to LLVM nodes: http://llvm.org/svn/llvm-project/dragonegg/trunk

   procedure Emit_Code_Statement (N : Node_Id)
     with Pre => Nkind (N) = N_Code_Statement;
   --  Generate code for inline asm

   procedure Emit_Loop_Statement (N : Node_Id)
     with Pre => Nkind (N) = N_Loop_Statement;
   --  Generate code for a loop

   function Emit_LValue_Internal (N : Node_Id) return GL_Value
     with Pre => Present (N), Post => Present (Emit_LValue_Internal'Result);
   --  Called by Emit_LValue to walk the tree saving values

   function Emit_LValue_Main (N : Node_Id) return GL_Value
     with Pre => Present (N), Post => Present (Emit_LValue_Main'Result);
   --  Called by Emit_LValue_Internal to do the work at each level

   ----------
   -- Emit --
   ----------

   procedure Emit (N : Node_Id) is
   begin
      Set_Debug_Pos_At_Node (N);
      if Library_Level
        and then ((Nkind (N) in N_Statement_Other_Than_Procedure_Call
                     and then Nkind (N) /= N_Null_Statement)
                    or else Nkind (N) in N_Subprogram_Call
                    or else Nkind (N) in N_Raise_xxx_Error
                    or else Nkind_In (N, N_Raise_Statement,
                                      N_Handled_Sequence_Of_Statements))
      then
         --  Append to list of statements to put in the elaboration procedure

         Add_To_Elab_Proc (N);
         return;
      end if;

      --  If not at library level and in dead code, start a new basic block
      --  for any code we emit.

      if not Library_Level and then Are_In_Dead_Code then
         Position_Builder_At_End (Create_Basic_Block ("dead-code"));
      end if;

      case Nkind (N) is
         when N_Compilation_Unit =>

            --  We assume there won't be any elaboration code and
            --  clear that flag if we're wrong.

            Set_Has_No_Elaboration_Code (N, True);

            --  For a body, first process the spec if there is one

            if Nkind (Unit (N)) = N_Package_Body
              or else (Nkind (Unit (N)) = N_Subprogram_Body
                         and then not Acts_As_Spec (Unit (N)))
            then
               Emit (Library_Unit (N));
            end if;

            Emit (Context_Items (N));
            Emit_Decl_Lists (Declarations (Aux_Decls_Node (N)), No_List);
            Emit (Unit (N));
            Emit (Actions (Aux_Decls_Node (N)));
            Emit (Pragmas_After (Aux_Decls_Node (N)));

         when N_Subunit =>
            Emit (Proper_Body (N));

         when N_Package_Declaration =>
            Push_Lexical_Debug_Scope (N);
            Emit (Specification (N));
            Pop_Debug_Scope;

         when N_Package_Specification =>
            Push_Lexical_Debug_Scope (N);
            Emit_Decl_Lists (Visible_Declarations (N),
                             Private_Declarations (N));
            Pop_Debug_Scope;

            if Library_Level then
               Emit_Elab_Proc (N, Empty, Parent (Parent (N)), "s");
            end if;

         when N_Package_Body =>

            --  Skip generic packages

            if Ekind (Unique_Defining_Entity (N)) in Generic_Unit_Kind then
               return;
            end if;

            declare
               Stmts : constant Node_Id := Handled_Statement_Sequence (N);

            begin
               --  Always process declarations

               Push_Lexical_Debug_Scope (N);
               Emit_Decl_Lists (Declarations (N), No_List);

               --  If we're at library level and our parent is an
               --  N_Compilation_Unit, make an elab proc and put the
               --  statements there.  Otherwise, emit them, which may add
               --  them to the elaboration table (if we're not at library
               --  level).

               if Library_Level
                 and then Nkind (Parent (N)) = N_Compilation_Unit
               then
                  Emit_Elab_Proc (N, Stmts, Parent (N), "b");
               elsif Present (Stmts) then
                  Emit (Stmts);
               end if;

               Pop_Debug_Scope;
            end;

         when N_Subprogram_Body =>

            --  Skip generic subprograms

            if not Present (Corresponding_Spec (N))
              or else not (Ekind (Corresponding_Spec (N))
                         in Generic_Subprogram_Kind)
            then
               Emit_Subprogram_Body (N);
            end if;

         when N_Subprogram_Declaration =>

            --  Ignore intrinsic subprograms as calls to those will be
            --  expanded.

            if not Is_Intrinsic_Subprogram (Unique_Defining_Entity (N)) then
               Discard (Emit_Subprogram_Decl (Specification (N)));
            end if;

         when N_Free_Statement =>
            Heap_Deallocate
              (Emit_Expression (Expression (N)),
               Procedure_To_Call (N), Storage_Pool (N));

         when N_Code_Statement =>
            Emit_Code_Statement (N);

         when N_Handled_Sequence_Of_Statements =>

            --  If First_Real_Statement is Present, items in Statements
            --  prior to it are declarations and need to be treated as such.
            --  Otherwise, all are statements.

            if Present (First_Real_Statement (N)) then
               Emit_Decl_Lists (Statements (N), No_List,
                                End_List => First_Real_Statement (N));
            end if;

            Start_Block_Statements (At_End_Proc (N), Exception_Handlers (N));
            Emit (Statements (N), Starting_At => First_Real_Statement (N));

         when N_Raise_Statement =>
            Emit_LCH_Call (N);

         when N_Raise_xxx_Error =>

            --  See if this Raise is really a goto due to having a label on
            --  the appropriate stack.

            declare
               Label_Ent : constant Entity_Id :=
                 Get_Exception_Goto_Entry (Nkind (N));
               BB_Next   : Basic_Block_T;

            begin
               if Present (Label_Ent) then
                  if Present (Condition (N)) then
                     BB_Next := Create_Basic_Block;
                     Emit_If_Cond
                       (Condition (N), Get_Label_BB (Label_Ent), BB_Next);
                     Position_Builder_At_End (BB_Next);
                  else
                     Build_Br (Get_Label_BB (Label_Ent));
                  end if;

                  return;
               end if;
            end;

            if Present (Condition (N)) then
               declare
                  BB_Then : constant Basic_Block_T :=
                    Create_Basic_Block ("raise");
                  BB_Next : constant Basic_Block_T := Create_Basic_Block;

               begin
                  Emit_If_Cond (Condition (N), BB_Then, BB_Next);
                  Position_Builder_At_End (BB_Then);
                  Emit_LCH_Call (N);
                  Build_Br (BB_Next);
                  Position_Builder_At_End (BB_Next);
               end;
            else
               Emit_LCH_Call (N);
            end if;

         when N_Object_Declaration | N_Exception_Declaration =>
            Emit_Declaration (N);

         when N_Object_Renaming_Declaration =>
            Emit_Object_Renaming_Declaration (N);

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
            Emit_Assignment (Emit_LValue (Name (N)), Expression (N),
                             No_GL_Value, Forwards_OK (N), Backwards_OK (N));

         when N_Procedure_Call_Statement =>
            Discard (Emit_Call (N));

         when N_Null_Statement =>
            null;

         when N_Label =>
            Discard (Enter_Block_With_Node (N));

         when N_Goto_Statement =>
            Build_Br (Get_Label_BB (Entity (Name (N))));

         when N_Exit_Statement =>
            declare
               Exit_BB : constant Basic_Block_T := Get_Exit_Point (Name (N));
               Next_BB : Basic_Block_T;

            begin
               if Present (Condition (N)) then
                  Next_BB := Create_Basic_Block ("loop-after-exit");
                  Emit_If_Cond (Condition (N), Exit_BB, Next_BB);
                  Position_Builder_At_End (Next_BB);
               else
                  Build_Br (Exit_BB);
               end if;

            end;

         when N_Simple_Return_Statement =>
            Emit_Return_Statement (N);

         when N_If_Statement =>
            Emit_If (N);

         when N_Loop_Statement =>
            Emit_Loop_Statement (N);

         when N_Block_Statement =>
            Push_Lexical_Debug_Scope (N);
            Push_Block;
            Emit_Decl_Lists (Declarations (N), No_List);
            Emit (Handled_Statement_Sequence (N));
            Set_Debug_Pos_At_Node (N);
            Pop_Block;
            Pop_Debug_Scope;

         when N_Full_Type_Declaration
            | N_Incomplete_Type_Declaration
            | N_Private_Extension_Declaration
            | N_Private_Type_Declaration
            | N_Subtype_Declaration
            | N_Task_Type_Declaration
           =>
            declare
               TE  : constant Entity_Id := Defining_Identifier (N);
               T   : constant Type_T    := GNAT_To_LLVM_Type (TE, True);

            begin
               if Esize (TE) /= Uint_0
                 and then (Is_Dynamic_Size (TE)
                             or else (Nat (unsigned_long_long'
                                             (Get_LLVM_Type_Size_In_Bits (T)))
                                        > Esize (TE)))
               then
                  Error_Msg_Uint_1 := Esize (TE);
                  Error_Msg_NE ("?Type & does not fit into ^ bits", N, TE);
               end if;
            end;

         when N_Freeze_Entity =>
            --  ??? Need to process Node itself

            Emit_Decl_Lists (Actions (N), No_List);

         when N_Pragma =>
            case Get_Pragma_Id (N) is

               when Pragma_Reviewable =>
                  if not Emit_Debug_Info then
                     Error_Msg_N ("must specify -g", N);
                  end if;

               --  ??? These are the ones that Gigi supports and we
               --  should support as well at some point.

               when Pragma_Inspection_Point
                  | Pragma_Loop_Optimize
                  | Pragma_Optimize
                  | Pragma_Warning_As_Error
                  | Pragma_Warnings
                  =>
                  null;

               when others => null;
            end case;

         when N_Case_Statement =>
            Emit_Case (N);

         when N_Body_Stub =>

            --  If we have a "separate" (either subprogram or package), we
            --  compile that as part of this unit, so go into it.

            if Present (Library_Unit (N)) then
               Emit (Unit (Library_Unit (N)));
            end if;

         --  Nodes we actually want to ignore, in many cases because they
         --  represent things that are put elsewhere in the tree (e.g,
         --  rep clauses).

         when N_Abstract_Subprogram_Declaration
            | N_At_Clause
            | N_Call_Marker
            | N_Empty
            | N_Enumeration_Representation_Clause
            | N_Enumeration_Type_Definition
            | N_Function_Instantiation
            | N_Freeze_Generic_Entity
            | N_Generic_Function_Renaming_Declaration
            | N_Generic_Package_Renaming_Declaration
            | N_Generic_Procedure_Renaming_Declaration
            | N_Generic_Package_Declaration
            | N_Generic_Subprogram_Declaration
            | N_Itype_Reference
            | N_Number_Declaration
            | N_Package_Instantiation
            | N_Package_Renaming_Declaration
            | N_Procedure_Instantiation
            | N_Protected_Type_Declaration
            | N_Record_Representation_Clause
            | N_Validate_Unchecked_Conversion
            | N_Variable_Reference_Marker
            | N_Use_Package_Clause
            | N_Use_Type_Clause
            | N_With_Clause
           =>
            null;

         when N_Push_Constraint_Error_Label .. N_Pop_Storage_Error_Label =>
            Process_Push_Pop_xxx_Error_Label (N);

         when N_Exception_Renaming_Declaration =>
            Set_Value (Defining_Identifier (N), Emit_LValue (Name (N)));

         when N_Attribute_Definition_Clause =>

            --  The only interesting case left after expansion is for Address
            --  clauses. We only deal with 'Address if the object has a Freeze
            --  node.

            --  ??? For now keep it simple and deal with this case in
            --  N_Object_Declaration.

            if Get_Attribute_Id (Chars (N)) = Attribute_Address
              and then Present (Freeze_Node (Entity (Name (N))))
            then
               null;
            end if;

         when others =>
            Error_Msg_N
              ("unhandled statement kind: `" &
               Node_Kind'Image (Nkind (N)) & "`", N);
      end case;
   end Emit;

   -----------------
   -- Emit_LValue --
   -----------------

   function Emit_LValue
     (N : Node_Id; Clear : Boolean := True) return GL_Value is
   begin
      Set_Debug_Pos_At_Node (N);

      --  When we start a new recursive call, we usualy free the entries
      --  from the last one.
      if Clear then
         Clear_LValue_List;
      end if;

      return Emit_LValue_Internal (N);
   end Emit_LValue;

   --------------------------
   -- Emit_LValue_Internal --
   --------------------------

   function Emit_LValue_Internal (N : Node_Id) return GL_Value
   is
      Typ   : constant Entity_Id := Full_Etype (N);
      Value : constant GL_Value  := Need_LValue (Emit_LValue_Main (N), Typ);

   begin
      --  If the object is not of void type, save the result in the
      --  pair table under the base type of the fullest view.

      if Ekind (Related_Type (Value)) /= E_Void then
         Add_To_LValue_List (Value);
      end if;

      return Value;
   end Emit_LValue_Internal;

   ----------------------
   -- Emit_LValue_Main --
   ----------------------

   function Emit_LValue_Main (N : Node_Id) return GL_Value is
      TE : constant Entity_Id := Full_Etype (N);
      V  : GL_Value;

   begin
      case Nkind (N) is
         when N_Identifier
            | N_Expanded_Name
            | N_Operator_Symbol
            | N_Defining_Identifier
            | N_Defining_Operator_Symbol =>
            return Emit_Identifier_LValue (N);

         when N_Attribute_Reference =>
            return Emit_Attribute_Reference (N, LValue => True);

         when N_Explicit_Dereference =>

            --  The result of evaluating Emit_Expression is the
            --  address of what we want and is an access type.  What
            --  we want here is a reference to our type, which should
            --  be the Designated_Type of Value.

            return Make_Reference (Emit_Expression (Prefix (N)));

         when N_String_Literal =>
            V := Add_Global (TE, "str");
            Set_Value (N, V);
            Set_Initializer (V, Emit_Expression (N));
            Set_Linkage (V, Private_Linkage);
            Set_Global_Constant (LLVM_Value (V), True);
            return V;

         when N_Selected_Component =>
            return Record_Field_Offset (Emit_LValue_Internal (Prefix (N)),
                                        Entity (Selector_Name (N)));

         when N_Indexed_Component =>
            return Get_Indexed_LValue (Expressions (N),
                                       Emit_LValue_Internal (Prefix (N)));

         when N_Slice =>
            return Get_Slice_LValue
              (TE, Discrete_Range (N), Emit_LValue_Internal (Prefix (N)));

         when N_Unchecked_Type_Conversion
            | N_Type_Conversion
            | N_Qualified_Expression =>

            --  We have to mark that this is now to be treated as a new type.
            --  This matters if, e.g., the bounds of an array subtype change
            --  (see C46042A).

            return
              Convert_To_Access_To (Emit_LValue_Internal (Expression (N)), TE);

         when others =>
            --  If we have an arbitrary expression, evaluate it.  If it
            --  turns out to be a reference (e.g., if the size of our type
            --  is dynamic, we have no more work to do.  Otherwise, our caller
            --  will take care of storing it into a temporary.

            return Emit_Expression (N);
      end case;
   end Emit_LValue_Main;

   --------------------
   -- Emit_Safe_Expr --
   --------------------

   function Emit_Safe_Expr (N : Node_Id) return GL_Value is
      V : GL_Value;
   begin
      Push_LValue_List;
      V := Emit_Expression (N);
      Pop_LValue_List;
      return V;
   end Emit_Safe_Expr;

   ---------------------
   -- Emit_Expression --
   ---------------------

   function Emit_Expression (N : Node_Id) return GL_Value is
      TE : constant Entity_Id := Full_Etype (N);

   begin
      Set_Debug_Pos_At_Node (N);
      if Nkind (N) in N_Binary_Op then

         --  Use helper functions for comparisons and shifts; the rest by
         --  generating the appropriate LLVM IR directly.

         if Nkind (N) in N_Op_Compare then
            return Emit_Comparison (Nkind (N), Left_Opnd (N), Right_Opnd (N));
         elsif Nkind (N) in N_Op_Shift then
            return Emit_Shift (Nkind (N), Left_Opnd (N), Right_Opnd (N));
         else
            return Emit_Binop (N);
         end if;
      end if;

      case Nkind (N) is

         when N_Expression_With_Actions =>
            Emit (Actions (N));
            return Emit_Expression (Expression (N));

         when N_Character_Literal | N_Numeric_Or_String_Literal =>
            return Emit_Literal (N);

         when N_And_Then | N_Or_Else =>
            if Side_Effect_Free (Left_Opnd (N))
              and then Side_Effect_Free (Right_Opnd (N))
              and then Is_Simple_Conditional (N)
            then
               declare
                  LHS : constant GL_Value := Emit_Expression (Left_Opnd (N));
                  RHS : constant GL_Value := Emit_Expression (Right_Opnd (N));

               begin
                  return (if Nkind (N) = N_And_Then
                          then Build_And (LHS, RHS) else Build_Or  (LHS, RHS));
               end;
            else
               return Build_Short_Circuit_Op (Left_Opnd (N), Right_Opnd (N),
                                              Nkind (N));
            end if;

         when N_Op_Not =>
            return Build_Not (Emit_Expression (Right_Opnd (N)));

         when N_Op_Abs =>

            --  Emit: X >= 0 ? X : -X;

            declare
               Expr      : constant GL_Value :=
                 Emit_Expression (Right_Opnd (N));
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
            return Emit_Expression (Right_Opnd (N));

         when N_Op_Minus =>
            declare
               Expr : constant GL_Value  := Emit_Expression (Right_Opnd (N));
               Typ  : constant Entity_Id := Full_Etype (Expr);

            begin
               if Is_Floating_Point_Type (Expr) then
                  return F_Neg (Expr);
               elsif Do_Overflow_Check (N)
                 and then not Is_Unsigned_Type (Expr)
               then
                  declare
                     Func      : constant GL_Value := Build_Intrinsic
                       (Overflow, "llvm.ssub.with.overflow.i", Typ);
                     Fn_Ret    : constant GL_Value :=
                       Call (Func, Typ, (1 => Const_Null (Typ), 2 => Expr));
                     Overflow  : constant GL_Value :=
                       Extract_Value (Standard_Boolean, Fn_Ret, 1, "overflow");
                     Label_Ent : constant Entity_Id :=
                       Get_Exception_Goto_Entry (N_Raise_Constraint_Error);
                     BB_Next   : Basic_Block_T;

                  begin
                     if Present (Label_Ent) then
                        BB_Next := Create_Basic_Block;
                        Build_Cond_Br
                          (Overflow, Get_Label_BB (Label_Ent), BB_Next);
                        Position_Builder_At_End (BB_Next);
                     else
                        Emit_LCH_Call_If (Overflow, N);
                     end if;

                     return Extract_Value (Typ, Fn_Ret, 0);
                  end;
               else
                  return NSW_Neg (Expr);
               end if;
            end;

         when N_Unchecked_Type_Conversion =>
            return Build_Unchecked_Conversion (Expression (N), TE);

         when N_Type_Conversion | N_Qualified_Expression =>
            return Build_Type_Conversion (Expression (N), TE);

         when N_Identifier
            | N_Expanded_Name
            | N_Operator_Symbol
            | N_Defining_Identifier
            | N_Defining_Operator_Symbol
            =>
            return Emit_Identifier_Value (N);

         when N_Function_Call =>
            return Emit_Call (N);

         when N_Explicit_Dereference =>
            return Need_Value
              (Make_Reference (Emit_Expression (Prefix (N))), TE);

         when N_Allocator =>

            declare
               Expr   : constant Node_Id := Expression (N);
               Value  : GL_Value         := No_GL_Value;
               Typ    : Entity_Id;
               Result : GL_Value;

            begin
               --  There are two cases: the Expression operand can
               --  either be an N_Identifier or Expanded_Name, which
               --  must represent a type, or a N_Qualified_Expression,
               --  which contains both the object type and an initial
               --  value for the object.  We ignore the initial value
               --  if No_Initialization is set.

               if Is_Entity_Name (Expr) then
                  Typ   := Get_Fullest_View (Entity (Expr));
                  Value := No_GL_Value;
               else
                  pragma Assert (Nkind (Expr) = N_Qualified_Expression);
                  Typ   := Full_Etype (Expression (Expr));

                  if not No_Initialization (N) then
                     Value := Emit_Expression (Expression (Expr));
                  end if;
               end if;

               Result := Heap_Allocate_For_Type
                 (Full_Designated_Type (TE), Typ, Value,
                  Procedure_To_Call (N), Storage_Pool (N));
               return Convert_To_Elementary_Type (Result, TE);
            end;

         when N_Reference =>
            return Convert_To_Elementary_Type (Emit_LValue (Prefix (N)), TE);

         when N_Attribute_Reference =>
            return Emit_Attribute_Reference (N, LValue => False);

         when N_Selected_Component | N_Indexed_Component  | N_Slice =>
            return Need_Value (Emit_LValue (N), TE);

         when N_Aggregate | N_Extension_Aggregate =>

            if Null_Record_Present (N) and then not Is_Dynamic_Size (TE)
            then
               return Const_Null (TE);

            elsif Ekind (TE) in Record_Kind then
               return Emit_Record_Aggregate (N, Get_Undef (TE));

            else
               pragma Assert (Is_Array_Type (TE)
                                   and then not Is_Dynamic_Size (TE));
               --  The back-end supports exactly two types of array
               --  aggregates.  One, which we handle here, is for a
               --  fixed-size aggregate of fixed-size components.  The
               --  other are very special cases of Others that are
               --  tested for in Aggr_Assignment_OK_For_Backend in
               --  Exp_Aggr.  We handle them in Emit_Assignment.

               return Emit_Array_Aggregate
                 (N, Number_Dimensions (TE), (1 .. 0 => <>), Get_Undef (TE));
            end if;

         when N_If_Expression =>
            return Emit_If_Expression (N);

         when N_Null =>
            return Const_Null (TE);

         when N_In =>
            declare
               Rng  : Node_Id := Right_Opnd (N);
               Left : constant GL_Value := Emit_Expression (Left_Opnd (N));

            begin
               pragma Assert (No (Alternatives (N)));
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
            Emit_LCH_Call (N);
            return Emit_Undef (TE);

         when N_Raise_xxx_Error =>
            pragma Assert (No (Condition (N)));
            Emit_LCH_Call (N);
            return Emit_Undef (TE);

         when others =>
            Error_Msg_N
              ("unsupported node kind: `" &
                 Node_Kind'Image (Nkind (N)) & "`", N);
            return Emit_Undef (TE);
      end case;
   end Emit_Expression;

   ----------
   -- Emit --
   ----------

   procedure Emit (List : List_Id; Starting_At : Node_Id := Empty) is
      N : Node_Id;

   begin
      if Present (List) then
         N := (if Present (Starting_At) then Starting_At else First (List));
         while Present (N) loop
            Emit (N);
            Next (N);
         end loop;
      end if;
   end Emit;

   -------------------------
   -- Emit_Loop_Statement --
   -------------------------

   procedure Emit_Loop_Statement (N : Node_Id) is
      Loop_Identifier : constant Entity_Id :=
        (if Present (Identifier (N)) then Entity (Identifier (N)) else Empty);
      Iter_Scheme     : constant Node_Id   := Iteration_Scheme (N);
      Is_Mere_Loop    : constant Boolean   := No (Iter_Scheme);
      Is_For_Loop     : constant Boolean   :=
        not Is_Mere_Loop
        and then Present (Loop_Parameter_Specification (Iter_Scheme));

      --  The general format for a loop is:
      --    INIT;
      --    while COND loop
      --       STMTS;
      --       ITER;
      --    end loop;
      --    NEXT:
      --
      --  Each step has its own basic block. When a loop doesn't need one
      --  of these steps, just alias it with another one.

      BB_Cond : Basic_Block_T :=
        (if not Is_For_Loop then Enter_Block_With_Node (Empty)
         else Create_Basic_Block ("loop-cond"));
      --  If this is not a FOR loop, there is no initialization: alias
      --  it with the COND block.

      BB_Stmts : constant Basic_Block_T :=
        (if Is_Mere_Loop or else Is_For_Loop
         then BB_Cond else Create_Basic_Block ("loop-stmts"));
      --  If this is a mere loop or a For loop, there is no condition
      --  block: alias it with the STMTS block.

      BB_Iter : Basic_Block_T :=
        (if Is_For_Loop then Create_Basic_Block ("loop-iter") else BB_Cond);
      --  If this is not a FOR loop, there is no iteration: alias it with
      --  the COND block, so that at the end of every STMTS, jump on ITER
      --  or COND.

      BB_Next : constant Basic_Block_T := Create_Basic_Block ("loop-exit");
      --  The NEXT step contains no statement that comes from the loop: it
      --  is the exit point.

   begin
      --  First compile the iterative part of the loop: evaluation of the
      --  exit condition, etc.

      if not Is_Mere_Loop then
         if not Is_For_Loop then

            --  This is a WHILE loop: jump to the loop-body if the
            --  condition evaluates to True, jump to the loop-exit
            --  otherwise.

            Position_Builder_At_End (BB_Cond);
            Emit_If_Cond (Condition (Iter_Scheme), BB_Stmts, BB_Next);

         else
            --  This is a FOR loop

            declare
               Loop_Param_Spec : constant Node_Id   :=
                 Loop_Parameter_Specification (Iter_Scheme);
               Def_Ident       : constant Node_Id   :=
                 Defining_Identifier (Loop_Param_Spec);
               Reversed        : constant Boolean   :=
                 Reverse_Present (Loop_Param_Spec);
               Unsigned_Type   : constant Boolean   :=
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
                 (Var_Type, Var_Type, (if Reversed then High else Low),
                  Name => Get_Name (Def_Ident));
               Set_Value (Def_Ident, LLVM_Var);

               --  Then go to the condition block if the range isn't empty

               Build_Cond_Br
                 (I_Cmp ((if Unsigned_Type then Int_ULE else Int_SLE),
                   Low, High, "loop-entry-cond"),
                 BB_Cond, BB_Next);

               BB_Cond := Create_Basic_Block ("loop-cond-iter");
               Position_Builder_At_End (BB_Cond);
               Build_Cond_Br
                 (I_Cmp (Int_EQ, Load (LLVM_Var),
                         (if Reversed then Low else High), "loop-iter-cond"),
                 BB_Next, BB_Iter);

               --  After STMTS, stop if the loop variable was equal to the
               --  "exit" bound. Increment/decrement it otherwise.

               Position_Builder_At_End (BB_Iter);

               declare
                  Prev : constant GL_Value := Load (LLVM_Var);
                  One  : constant GL_Value := Const_Int (Var_Type, Uint_1);
                  Next : constant GL_Value :=
                    (if Reversed then NSW_Sub (Prev, One, "next-loop-var")
                     else NSW_Add (Prev, One, "next-loop-var"));

               begin
                  Store (Next, LLVM_Var);
               end;

               Build_Br (BB_Stmts);

               --  The ITER step starts at this special COND step

               BB_Iter := BB_Cond;
            end;
         end if;
      end if;

      --  Finally, emit the body of the loop.  Save and restore the stack
      --  around that code, so we free any variables allocated each iteration.

      Position_Builder_At_End (BB_Stmts);
      Push_Block;
      Start_Block_Statements (Empty, No_List);
      Push_Loop (Loop_Identifier, BB_Next);
      Emit (Statements (N));
      Set_Debug_Pos_At_Node (N);
      Pop_Block;
      Pop_Loop;

      Build_Br (BB_Iter);
      Position_Builder_At_End (BB_Next);

   end Emit_Loop_Statement;

   ---------------------
   -- Emit_Assignment --
   ---------------------

   procedure Emit_Assignment
     (LValue                    : GL_Value;
      Orig_E                    : Node_Id;
      E_Value                   : GL_Value;
      Forwards_OK, Backwards_OK : Boolean)
   is
      Dest_Type : constant Entity_Id := Full_Designated_Type (LValue);
      E         : constant Node_Id   := Strip_Complex_Conversions (Orig_E);
      Dest      : GL_Value           := LValue;
      Src       : GL_Value;

   begin
      --  The back-end supports exactly two types of array aggregates.
      --  One, handled in Emit_Array_Aggregate, is for a fixed-size
      --  aggregate of fixed-size components.  The other are special cases
      --  of Others that are tested for in Aggr_Assignment_OK_For_Backend
      --  in Exp_Aggr.  We have to handle them here because we want to
      --  store directly into the LHS.  The front end guarantees that any
      --  Others aggregate will always be the RHS of an assignment, so
      --  we'll see it here.

      if Is_Array_Type (Dest_Type) and then Present (E)
        and then Nkind_In (E, N_Aggregate, N_Extension_Aggregate)
        and then Is_Others_Aggregate (E)
      then
         Emit_Others_Aggregate (Dest, E);

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
         --  Src, which we know is fixed-size, and do the store.  If Dest
         --  is pointer to an array type, we need to get the actual array
         --  data.

         if Pointer_Type (Type_Of (Src),  0) /= Type_Of (Dest) then
            if Is_Array_Type (Full_Designated_Type (Dest)) then
               Dest := Array_Data (Dest);
            end if;

            Dest := Ptr_To_Ref (Dest, Full_Etype (Src));
         end if;

         Store (Src, Dest);

      else
         Src := (if No (E_Value) then Emit_LValue (E, Clear => False)
                 else E_Value);

         --  Otherwise, we have to do a variable-sized copy

         declare
            Size      : constant GL_Value := Compute_Size
              (Dest_Type, Full_Designated_Type (Src), Dest, Src);
            Align     : constant unsigned := Compute_Alignment
              (Dest_Type, Full_Designated_Type (Src));
            Func_Name : constant String   :=
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
                    (Memcpy, "llvm." & Func_Name & ".p0i8.p0i8.i", Size_Type),
                  (1 => Pointer_Cast (Dest, Standard_A_Char),
                   2 => Pointer_Cast (Src, Standard_A_Char),
                   3 => Size,
                   4 => Const_Int_32 (Align),
                   5 => Const_False)); -- Is_Volatile
         end;
      end if;
   end Emit_Assignment;

   -------------------------
   -- Emit_Code_Statement --
   ------------------------

   procedure Emit_Code_Statement (N : Node_Id) is
      Template_Strval   : constant String_Id := Strval (Asm_Template (N));
      Num_Inputs        : Integer            := 0;
      Constraint_Length : Integer            := 0;
      Output_Val        : GL_Value           := No_GL_Value;
      Output_Variable   : Node_Id;
      Output_Constraint : Node_Id;
      Input             : Node_Id;
      Clobber           : System.Address;

   begin
      --  LLVM only allows one output, so just get the information on
      --  it, if any, and give an error if there's a second one.

      Setup_Asm_Outputs (N);
      Output_Variable := Asm_Output_Variable;

      if Present (Output_Variable) then
         Output_Constraint := Asm_Output_Constraint;
         Constraint_Length :=
           Integer (String_Length (Strval (Output_Constraint)));
         Output_Val := Emit_LValue (Output_Variable);
         Next_Asm_Output;

         if Present (Asm_Output_Variable) then
            Error_Msg_N ("LLVM only allows one output", N);
         end if;
      end if;

      --  For inputs, just count the number of them and the total
      --  constraint length so we can allocate what we need later.

      Setup_Asm_Inputs (N);
      Input := Asm_Input_Value;

      while Present (Input) loop
         Num_Inputs        := Num_Inputs + 1;
         Constraint_Length := Constraint_Length +
           Integer (String_Length (Strval (Asm_Input_Constraint)));
         Next_Asm_Input;
         Input := Asm_Input_Value;
      end loop;

      --  Likewise for clobbers, but we only need the length of the
      --  constraints here.  Node that Clobber_Get_Next isn't very friendly
      --  for an Ada called, so we'll use fact that it's set Name_Buffer
      --  and Name_Len;

      Clobber_Setup (N);
      Clobber := Clobber_Get_Next;

      while not System."=" (Clobber, System.Null_Address) loop
         Constraint_Length := Constraint_Length + Name_Len + 4;
         Clobber := Clobber_Get_Next;
      end loop;

      declare
         Args           : GL_Value_Array (1 .. Nat (Num_Inputs));
         Constraints    : String (1 .. Num_Inputs + Constraint_Length + 3);
         Constraint_Pos : Integer := 0;
         Input_Pos      : Nat := 0;
         Need_Comma     : Boolean := False;
         Asm            : GL_Value;
         Template       : String (1 .. Integer (String_Length
                                                  (Template_Strval)));

         procedure Add_Char (C : Character);
         procedure Add_Constraint (N : Node_Id)
           with Pre => Nkind (N) = N_String_Literal;

         --------------
         -- Add_Char --
         --------------

         procedure Add_Char (C : Character) is
         begin
            Constraint_Pos := Constraint_Pos + 1;
            Constraints (Constraint_Pos) := C;
            Need_Comma := C /= ',';
         end Add_Char;

         --------------------
         -- Add_Constraint --
         --------------------

         procedure Add_Constraint (N : Node_Id) is
         begin
            if Need_Comma then
               Add_Char (',');
            end if;

            for J in 1 .. String_Length (Strval (N)) loop
               Add_Char (Get_Character (Get_String_Char (Strval (N), J)));
            end loop;
         end Add_Constraint;

      begin
         --  Output constraints come first

         if Present (Output_Variable) then
            Add_Constraint (Output_Constraint);
         end if;

         --  Now collect inputs and add their constraints

         Setup_Asm_Inputs (N);
         Input := Asm_Input_Value;
         while Present (Input) loop
            Input_Pos := Input_Pos + 1;
            Args (Input_Pos) :=
              Need_Value (Emit_Expression (Input), Full_Etype (Input));
            Add_Constraint (Asm_Input_Constraint);
            Next_Asm_Input;
            Input := Asm_Input_Value;
         end loop;

         --  Now add clobber constraints

         Clobber_Setup (N);
         Clobber := Clobber_Get_Next;
         while not System."=" (Clobber, System.Null_Address) loop
            if Need_Comma then
               Add_Char (',');
            end if;

            Add_Char ('~');
            Add_Char ('{');
            for J in 1 .. Name_Len loop
               Add_Char (Name_Buffer (J));
            end loop;

            Add_Char ('}');
            Clobber := Clobber_Get_Next;
         end loop;

         --  Finally, build the template

         for J in 1 .. String_Length (Template_Strval) loop
            Template (Integer (J)) :=
              Get_Character (Get_String_Char (Template_Strval, J));
         end loop;

         --  Now create the inline asm

         Asm := Inline_Asm (Args, Output_Variable, Template,
                            Constraints (1 .. Constraint_Pos),
                            Is_Asm_Volatile (N), False);

         --  If we have an output, generate the vall with an output and store
         --  the result.  Otherwise, just do the call.

         if Present (Output_Variable) then
            Store (Call (Asm, Full_Etype (Asm), Args), Output_Val);
         else
            Call (Asm, Args);
         end if;
      end;
   end Emit_Code_Statement;

end GNATLLVM.Compile;
