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
with Exp_Util; use Exp_Util;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
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
      --  If we're at library level and this node type generates code,
      --  append it to the elab proc.

      if Library_Level
        and then ((Nkind (N) in N_Statement_Other_Than_Procedure_Call
                     and then Nkind (N) /= N_Null_Statement)
                    or else Nkind (N) in N_Subprogram_Call
                    or else Nkind (N) in N_Raise_xxx_Error
                    or else Nkind_In (N, N_Raise_Statement,
                                      N_Handled_Sequence_Of_Statements))
      then
         Add_To_Elab_Proc (N);
         return;

      --  If not at library level and in dead code, start a new basic block
      --  for any code we emit.

      elsif not Library_Level and then Are_In_Dead_Code then
         Position_Builder_At_End (Create_Basic_Block ("dead-code"));
      end if;

      Set_Debug_Pos_At_Node (N);
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
               Stmts      : constant Node_Id := Handled_Statement_Sequence (N);

            begin
               --  Always process declarations

               Push_Lexical_Debug_Scope (N);
               if not Library_Level then
                  Push_Block;
               end if;

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

               if not Library_Level then
                  Pop_Block;
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
            Heap_Deallocate (Emit_Expression (Expression (N)),
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
            pragma Assert (Back_End_Exceptions);
            Emit_Reraise;

         when N_Raise_xxx_Error =>
            Emit_Raise (N);

         when N_Object_Declaration | N_Exception_Declaration =>
            Emit_Declaration (N);

         when N_Object_Renaming_Declaration
            | N_Exception_Renaming_Declaration =>
            Emit_Renaming_Declaration (N);

         when N_Subprogram_Renaming_Declaration =>

            --  Nothing is needed except for debugging information.
            --  ??? Skip it for now.  Note that in any case, we should
            --  skip Intrinsic subprograms

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
                            or else (Nat (ULL'(Get_LLVM_Type_Size_In_Bits (T)))
                                       > Esize (TE)))
               then
                  Error_Msg_Uint_1 := Esize (TE);
                  Error_Msg_NE ("?Type & does not fit into ^ bits", N, TE);
               end if;
            end;

         when N_Freeze_Entity =>

            Process_Freeze_Entity (N);
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
            Emit_Case_Statement (N);

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

         when N_Attribute_Definition_Clause =>

            --  The only interesting case left after expansion is for Address
            --  clauses. We only deal with 'Address if the object has a Freeze
            --  node.

            if Get_Attribute_Id (Chars (N)) = Attribute_Address
              and then Present (Freeze_Node (Entity (Name (N))))
            then
               declare
                  Expr : constant Node_Id   := Expression (N);

               begin
                  if Library_Level then
                     Add_To_Elab_Proc (Expr, For_Type => Full_Etype (Expr));
                  else
                     Set_Value (Expr, Emit_Expression (Expr));
                  end if;
               end;
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
      Value : constant GL_Value  := Get (Emit_LValue_Main (N), Any_Reference);

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

            return From_Access (Emit_Expression (Prefix (N)));

         when N_String_Literal =>
            V := Add_Global (TE, "str");
            Set_Initializer (V, Get (Emit_Expression (N), Bounds_And_Data));
            Set_Linkage (V, Private_Linkage);
            Set_Global_Constant (LLVM_Value (V), True);
            Set_Value (N, Get (V, Thin_Pointer));
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
      case Nkind (N) is

         when N_Binary_Op =>
            if Nkind (N) in N_Op_Compare then
               return Emit_Comparison (Nkind (N), Left_Opnd (N),
                                       Right_Opnd (N));
            elsif Nkind (N) in N_Op_Shift then
               return Emit_Shift (Nkind (N), Left_Opnd (N), Right_Opnd (N));
            else
               return Emit_Binary_Operation (N);
            end if;

         when N_Unary_Op =>
            return Emit_Unary_Operation (N);

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
            return Get (From_Access (Emit_Expression (Prefix (N))), Object);

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
               --  value for the object.

               if Is_Entity_Name (Expr) then
                  Typ   := Get_Fullest_View (Entity (Expr));
                  Value := No_GL_Value;
               else
                  pragma Assert (Nkind (Expr) = N_Qualified_Expression);
                  Typ   := Full_Etype (Expression (Expr));
                  Value := Emit_Expression (Expression (Expr));
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
            return Get (Emit_LValue (N), Object);

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

            --  If N is an N_Handled_Sequence_Of_Statements here, we know
            --  that it's not nested in a block.  It probably was from a
            --  package body at library level and ended in the elab proc.
            --  Make a block around it.

            if Nkind (N) = N_Handled_Sequence_Of_Statements then
               Push_Block;
               Emit (N);
               Pop_Block;
            else
               Emit (N);
            end if;

            Next (N);
         end loop;
      end if;
   end Emit;

   ---------------------------
   -- Process_Freeze_Entity --
   ---------------------------

   procedure Process_Freeze_Entity (N : Node_Id) is
      E    : constant Entity_Id := Entity (N);
      Decl : constant Node_Id   := Declaration_Node (E);

   begin
      if Nkind_In (Decl, N_Object_Declaration, N_Exception_Declaration) then
         Emit_Declaration (Decl, For_Freeze_Entity => True);
      end if;
   end Process_Freeze_Entity;

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
               Spec       : constant Node_Id   :=
                 Loop_Parameter_Specification (Iter_Scheme);
               Def_Ident  : constant Node_Id   := Defining_Identifier (Spec);
               Reversed   : constant Boolean   := Reverse_Present (Spec);
               Var_Type   : constant Entity_Id := Full_Etype (Def_Ident);
               Var_BT     : constant Entity_Id :=
                 Implementation_Base_Type (Var_Type);
               Uns_BT     : constant Boolean   := Is_Unsigned_Type (Var_BT);
               One        : constant GL_Value  := Const_Int (Var_Type, Uint_1);
               LLVM_Var   : GL_Value;
               Low, High  : GL_Value;
               Prev       : GL_Value;

            begin
               --  Initialization block: create the loop variable and
               --  initialize it.

               Bounds_From_Type (Var_Type, Low, High);
               LLVM_Var := Allocate_For_Type
                 (Var_Type, Var_Type, (if Reversed then High else Low),
                  Name => Get_Name (Def_Ident));
               Set_Value (Def_Ident, LLVM_Var);

               --  Then go to the condition block if the range isn't empty.
               --  Note that this comparison must be done in the base type.

               Build_Cond_Br
                 (I_Cmp ((if Uns_BT then Int_ULE else Int_SLE),
                         Convert_To_Elementary_Type (Low, Var_BT),
                         Convert_To_Elementary_Type (High, Var_BT),
                         "loop-entry-cond"),
                  BB_Cond, BB_Next);

               --  Stop if the loop variable was equal to the "exit"
               --  bound. Increment/decrement it otherwise.

               BB_Cond := Create_Basic_Block ("loop-cond-iter");
               Position_Builder_At_End (BB_Cond);
               Prev := Load (LLVM_Var);
               Build_Cond_Br
                 (I_Cmp (Int_EQ, Prev,
                         (if Reversed then Low else High), "loop-iter-cond"),
                 BB_Next, BB_Iter);

               Position_Builder_At_End (BB_Iter);
               Store ((if Reversed then NSW_Sub (Prev, One, "next-loop-var")
                       else NSW_Add (Prev, One, "next-loop-var")),
                      LLVM_Var);
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

end GNATLLVM.Compile;
