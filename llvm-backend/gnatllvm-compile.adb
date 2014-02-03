with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with System;

with Einfo;    use Einfo;
with Errout;   use Errout;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Sem_Util; use Sem_Util;
with Stringt;  use Stringt;
with Uintp;    use Uintp;

with LLVM.Analysis; use LLVM.Analysis;

with GNATLLVM.Types; use GNATLLVM.Types;
with GNATLLVM.Utils; use GNATLLVM.Utils;
with GNATLLVM.Builder; use GNATLLVM.Builder;
with Get_Targ; use Get_Targ;

package body GNATLLVM.Compile is

   procedure Emit_List
     (Env : Environ; List : List_Id);
   --  Call Compile on every element of List

   function Call
     (Env : Environ; Call_Node : Node_Id) return Value_T;

   function Emit_Subprogram_Decl
     (Env : Environ; Subp_Spec : Node_Id) return Value_T;
   --  Compile a subprogram declaration, save the corresponding LLVM value to
   --  the environment and return it.

   function Array_Size
     (Env : Environ; Array_Type : Entity_Id) return Value_T;

   function Record_Field_Offset
     (Env : Environ;
      Record_Ptr : Value_T;
      Record_Field : Node_Id) return Value_T;

   type Bound_T is (Low, High);

   function Array_Bound
     (Env : Environ; Array_Node : Node_Id;
      Bound : Bound_T; Dim : Natural := 1) return Value_T;
   --  Compute the bound for the array corresponding to Array_Node. Depending
   --  on whether the array is constrained or not, this will compute the bound
   --  statically or at runtime.

   function Array_Bound_Addr
     (Env : Environ; Array_Ptr : Value_T;
      Bound : Bound_T; Dim : Natural) return Value_T;
   --  Compute the bound for the array corresponding to Array_Ptr. The pointer
   --  must be a fat pointer (i.e. containing the bounds of the array).

   function Array_Bound
     (Env : Environ; Array_Ptr : Value_T;
      Bound : Bound_T; Dim : Natural) return Value_T;
   --  Wrapper around Array_Bound_Addr that returns the value of the bound,
   --  instead of the address of element at the bound.

   -----------------
   -- Array_Bound --
   -----------------

   function Array_Bound
     (Env : Environ; Array_Ptr : Value_T;
      Bound : Bound_T; Dim : Natural) return Value_T
   is
   begin
      return Env.Bld.Load
        (Array_Bound_Addr (Env, Array_Ptr, Bound, Dim),
         (if Bound = Low
          then "load-low-bound"
          else "load-high-bound"));
   end Array_Bound;

   ----------------------
   -- Array_Bound_Addr --
   ----------------------

   function Array_Bound_Addr
     (Env : Environ; Array_Ptr : Value_T;
      Bound : Bound_T; Dim : Natural) return Value_T
   is
      Bounds_Ptr             : constant Value_T :=
        Env.Bld.Struct_GEP (Array_Ptr, 1, "gep-bounds-array");
      --  Get a pointer to the structure that contains array bounds

      Bounds_Pair_Idx        : constant Natural := (Dim - 1) * 2;
      --  In such a structure, bounds are stored as a sequence of (lower bound,
      --  upper bound) pairs : get the offset of such a pair.

      Bound_Idx              : constant unsigned :=
        unsigned (Bounds_Pair_Idx) + (if Bound = Low then 0 else 1);
   begin
      return Env.Bld.Struct_GEP
        (Bounds_Ptr,
         Bound_Idx,
         (if Bound = Low
          then "gep-low-bound"
          else "gep-high-bound"));
   end Array_Bound_Addr;

   -----------------
   -- Array_Bound --
   -----------------

   function Array_Bound
     (Env : Environ; Array_Node : Node_Id;
      Bound : Bound_T; Dim : Natural := 1) return Value_T
   is
      T : constant Entity_Id := Etype (Array_Node);
      R : Node_Id;
   begin
      if Is_Constrained (T) then
         R := Pick (List_Containing (First_Index (T)), Nat (Dim));
         return Emit_Expression
           (Env,
            (if Bound = Low then Low_Bound (R) else High_Bound (R)));
      else
         return Array_Bound
           (Env, Emit_LValue (Env, Array_Node), Bound, Dim);
      end if;
   end Array_Bound;

   ----------------
   -- Array_Size --
   ----------------

   function Array_Size
     (Env : Environ; Array_Type : Entity_Id) return Value_T
   is
      CT       : constant Entity_Id := Component_Type (Array_Type);

      Size     : Value_T := No_Value_T;
      Cur_Size : Value_T;
      DSD      : Node_Id := First_Index (Array_Type);
      T        : constant Type_T :=
        Int_Type_In_Context (Env.Ctx, unsigned (Get_Pointer_Size));
      --  An array can be as big as the memory space, so use the appropriate
      --  type.
   begin

      --  Go through every array dimension

      while Present (DSD) loop

         case Nkind (DSD) is
            when N_Range =>

               --  Compute the size of the dimension from the range bounds

               Cur_Size := Env.Bld.Add
                 (Env.Bld.Sub
                    (Emit_Expression (Env, High_Bound (DSD)),
                     Emit_Expression (Env, Low_Bound (DSD)), ""),
                  Const_Int
                    (Create_Type (Env, Etype (High_Bound (DSD))), 1, True),
                  "");
               Cur_Size := Env.Bld.Z_Ext (Cur_Size, T, "");

               --  Accumulate the product of the sizes
               --  If it's the first dimension, initialize our result with it
               --  Else, multiply our result by it

               if Size = No_Value_T then
                  Size := Cur_Size;
               else
                  Size := Env.Bld.Mul (Size, Cur_Size, "");
               end if;

            pragma Annotate (Xcov, Exempt_On, "Defensive programming");
            when others =>
               raise Program_Error with "Not supported : " & Nkind (DSD)'Img;
            pragma Annotate (Xcov, Exempt_Off);

         end case;

         DSD := Next (DSD);

      end loop;

      --  If the component of the array is itself an array, then recursively
      --  compute the size of the component and return the product

      if Is_Array_Type (CT) then
         return Env.Bld.Mul (Size, Array_Size (Env, CT), "");
      else
         return Size;
      end if;

   end Array_Size;

   -------------------------
   -- Record_Field_Offset --
   -------------------------

   function Record_Field_Offset
     (Env : Environ;
      Record_Ptr : Value_T;
      Record_Field : Node_Id) return Value_T
   is
      Field_Id : constant Entity_Id := Defining_Identifier (Record_Field);
      Type_Id  : constant Entity_Id := Scope (Field_Id);
      R_Info   : constant Record_Info := Env.Get (Type_Id);
      F_Info   : constant Field_Info := R_Info.Fields.Element (Field_Id);
   begin
      if F_Info.Containing_Struct_Index > 1 then
         null;
      end if;
      return Env.Bld.Struct_GEP
        (Record_Ptr, unsigned (F_Info.Index_In_Struct), "field_access");
   end Record_Field_Offset;

   ---------------------------
   -- Emit_Compilation_Unit --
   ---------------------------

   procedure Emit_Compilation_Unit
     (Env : Environ; Node : Node_Id; Emit_Library_Unit : Boolean) is
   begin
      Env.Begin_Declarations;
      for With_Clause of Iterate (Context_Items (Node)) loop
         Emit (Env, With_Clause);
      end loop;
      Env.End_Declarations;

      if Emit_Library_Unit
        and then Present (Library_Unit (Node))
        and then Library_Unit (Node) /= Node
      then
         --  Library unit spec and body point to each other. Avoid infinite
         --  recursion.

         Emit_Compilation_Unit (Env, Library_Unit (Node), False);
      end if;
      Emit (Env, Unit (Node));
   end Emit_Compilation_Unit;

   ----------
   -- Emit --
   ----------

   procedure Emit
     (Env : Environ; Node : Node_Id) is
   begin
      case Nkind (Node) is

         when N_Compilation_Unit =>
            pragma Annotate (Xcov, Exempt_On, "Defensive programming");
            raise Program_Error with
              "N_Compilation_Unit node must be processed in"
              & " Emit_Compilation_Unit";
            pragma Annotate (Xcov, Exempt_Off);

         when N_With_Clause =>
            Emit_Compilation_Unit (Env, Library_Unit (Node), True);

         when N_Use_Package_Clause =>
            null;

         when N_Package_Declaration =>
            Emit (Env, Specification (Node));

         when N_Package_Specification =>
            Emit_List (Env, Visible_Declarations (Node));
            Emit_List (Env, Private_Declarations (Node));

         when N_Package_Body =>
            declare
               Def_Id : constant Entity_Id := Unique_Defining_Entity (Node);
            begin
               if Ekind (Def_Id) not in Generic_Unit_Kind then
                  Emit_List (Env, Declarations (Node));
                  --  TODO : Handle statements
               end if;
            end;

         when N_Subprogram_Declaration =>
            Discard (Emit_Subprogram_Decl (Env, Specification (Node)));

         when N_Subprogram_Body =>
            --  If we are processing only declarations, do not emit a
            --  subprogram body: just declare this subprogram and add it to
            --  the environment.

            if Env.In_Declarations then
               Discard (Emit_Subprogram_Decl (Env, Get_Acting_Spec (Node)));
               return;
            end if;

            declare
               Spec       : constant Node_Id := Get_Acting_Spec (Node);
               Func       : constant Value_T :=
                 Emit_Subprogram_Decl (Env, Spec);
               Subp       : constant Subp_Env := Env.Enter_Subp (Func);

               LLVM_Param : Value_T;
               LLVM_Var   : Value_T;
               Param      : Entity_Id;
               I          : Natural := 0;
            begin
               --  Register each parameter into a new scope
               Env.Push_Scope;

               for P of Iterate (Parameter_Specifications (Spec)) loop
                  LLVM_Param := Get_Param (Subp.Func, unsigned (I));
                  Param := Defining_Identifier (P);

                  --  Define a name for the parameter P (which is the I'th
                  --  parameter), and associate the corresponding LLVM value to
                  --  its entity.

                  --  Set the name of the llvm value

                  Set_Value_Name (LLVM_Param, Get_Name (Param));

                  --  Special case for structures passed by value, we want to
                  --  store a pointer to them on the stack, so do an alloca,
                  --  to be able to do GEP on them.

                  if Param_Needs_Ptr (Param)
                    and then not
                      (Get_Type_Kind (Type_Of (LLVM_Param)) = Struct_Type_Kind)
                  then
                     LLVM_Var := LLVM_Param;
                  else
                     LLVM_Var := Env.Bld.Alloca
                       (Type_Of (LLVM_Param), Get_Name (Param));
                     Env.Bld.Store (LLVM_Param, LLVM_Var);
                  end if;

                  --  Add the parameter to the environnment

                  Env.Set (Param, LLVM_Var);
                  if Spec_Entity (Param) /= 0
                    and then Spec_Entity (Param) /= Param
                  then
                     Env.Set (Spec_Entity (Param), LLVM_Var);
                  end if;

                  I := I + 1;
               end loop;

               Emit_List (Env, Declarations (Node));
               Emit_List
                 (Env, Statements (Handled_Statement_Sequence (Node)));

               --  This point should not be reached: a return must have
               --  already... returned!

               Discard (Env.Bld.Unreachable);

               Env.Pop_Scope;
               Env.Leave_Subp;

               if Verify_Function (Subp.Func, Print_Message_Action) then
                  pragma Annotate (Xcov, Exempt_On, "Defensive programming");
                  Error_Msg_N
                    ("The backend generated bad LLVM for this subprogram.",
                     Node);
                  Dump_LLVM_Module (Env.Mdl);
                  pragma Annotate (Xcov, Exempt_Off);
               end if;
            end;

         when N_Raise_Constraint_Error =>

            --  TODO??? When exceptions handling will be implemented, implement
            --  this.

            null;

         when N_Raise_Storage_Error =>

            --  TODO??? When exceptions handling will be implemented, implement
            --  this.

            null;

         when N_Object_Declaration =>

            --  Object declarations are local variables allocated on the stack

            declare
               Def_Ident      : constant Node_Id := Defining_Identifier (Node);
               Obj_Def        : constant Node_Id := Object_Definition (Node);
               T              : constant Entity_Id := Etype (Def_Ident);
               LLVM_Type      : Type_T;
               LLVM_Var, Expr : Value_T;
            begin

               --  Strip useless entities such as the ones generated for
               --  renaming encodings.

               if Nkind (Obj_Def) = N_Identifier
                 and then Ekind (Entity (Obj_Def)) in Discrete_Kind
                 and then Esize (Entity (Obj_Def)) = 0
               then
                  return;
               end if;

               if Is_Array_Type (T) then

                  --  Alloca arrays are handled as follows:
                  --  * The total size is computed with Compile_Array_Size.
                  --  * The type of the innermost component is computed with
                  --    Get_Innermost_Component type.
                  --  * The result of the alloca is bitcasted to the proper
                  --    array type, so that multidimensional LLVM GEP
                  --    operations work properly.

                  LLVM_Type := Create_Access_Type (Env, T);

                  LLVM_Var := Env.Bld.Bit_Cast
                     (Env.Bld.Array_Alloca
                        (Get_Innermost_Component_Type (Env, T),
                         Array_Size (Env, T), "array-alloca"),
                     LLVM_Type,
                     Get_Name (Def_Ident));
               else
                  LLVM_Type := Create_Type (Env, T);
                  LLVM_Var := Env.Bld.Alloca (LLVM_Type, Get_Name (Def_Ident));
               end if;

               Env.Set (Def_Ident, LLVM_Var);

               if Present (Expression (Node))
                 and then not No_Initialization (Node)
               then
                  --  TODO??? Handle the Do_Range_Check_Flag
                  Expr := Emit_Expression (Env, Expression (Node));
                  Env.Bld.Store (Expr, LLVM_Var);
               end if;
            end;

         when N_Use_Type_Clause =>
            null;

         when N_Renaming_Declaration =>

            if Nkind (Node) = N_Package_Renaming_Declaration then

               --  ??? Probably safe to ignore those, remove interrogation
               --  marks when confirmed

               return;
            end if;

            declare
               Def_Ident : constant Node_Id := Defining_Identifier (Node);
               LLVM_Var  : Value_T;
            begin

               --  If the renamed object is already an l-value, keep it as-is.
               --  Otherwise, create one for it.

               if Is_LValue (Name (Node)) then
                  LLVM_Var := Emit_LValue (Env, Name (Node));
               else
                  LLVM_Var := Env.Bld.Alloca
                    (Create_Type (Env, Etype (Def_Ident)),
                     Get_Name (Def_Ident));
                  Env.Bld.Store (Emit_Expression (Env, Name (Node)), LLVM_Var);
               end if;
               Env.Set (Def_Ident, LLVM_Var);
            end;

         when N_Implicit_Label_Declaration =>
            Env.Set
              (Defining_Identifier (Node),
               Create_Basic_Block
                 (Env, Get_Name (Defining_Identifier (Node))));

         when N_Assignment_Statement =>
            declare
               Val : constant Value_T :=
                 Emit_Expression (Env, Expression (Node));
               Dest : constant Value_T := Emit_LValue (Env, Name (Node));
            begin
               Env.Bld.Store (Val, Dest);
            end;

         when N_Procedure_Call_Statement =>
            Discard (Call (Env, Node));

         when N_Null_Statement =>
            null;

         when N_Label =>
            declare
               BB : constant Basic_Block_T :=
                 Env.Get (Entity (Identifier (Node)));
            begin
               Discard (Env.Bld.Br (BB));
               Env.Bld.Position_At_End (BB);
            end;

         when N_Goto_Statement =>
            Discard (Env.Bld.Br (Env.Get (Entity (Name (Node)))));
            Env.Bld.Position_At_End
              (Env.Create_Basic_Block ("after-goto"));

         when N_Exit_Statement =>
            declare
               Exit_Point : constant Basic_Block_T :=
                 (if Present (Name (Node))
                  then Env.Get_Exit_Point (Entity (Name (Node)))
                  else Env.Get_Exit_Point);
               Next_BB    : constant Basic_Block_T :=
                 Env.Create_Basic_Block ("loop-after-exit");
            begin
               if Present (Condition (Node)) then
                  Discard
                    (Env.Bld.Cond_Br
                       (Emit_Expression (Env, Condition (Node)),
                        Exit_Point,
                        Next_BB));
               else
                  Discard (Env.Bld.Br (Exit_Point));
               end if;
               Env.Bld.Position_At_End (Next_BB);
            end;

         when N_Simple_Return_Statement =>
            if Present (Expression (Node)) then
               Discard
                 (Env.Bld.Ret (Emit_Expression (Env, Expression (Node))));
            else
               Discard (Env.Bld.Ret_Void);
            end if;
            Env.Bld.Position_At_End
              (Env.Create_Basic_Block ("unreachable"));

         when N_If_Statement =>
            declare
               BB_Then, BB_Else, BB_Next : Basic_Block_T;
               Cond                      : constant Value_T :=
                 Emit_Expression (Env, Condition (Node));
            begin
               BB_Next := Create_Basic_Block (Env, "if-next");
               BB_Then := Create_Basic_Block (Env, "if-then");
               BB_Else :=
                 (if not Is_Empty_List (Else_Statements (Node))
                  then Create_Basic_Block (Env, "if-else")
                  else BB_Next);

               Discard (Env.Bld.Cond_Br (Cond, BB_Then, BB_Else));

               Env.Bld.Position_At_End (BB_Then);
               Emit_List (Env, Then_Statements (Node));
               Discard (Env.Bld.Br (BB_Next));

               if not Is_Empty_List (Else_Statements (Node)) then
                  Env.Bld.Position_At_End (BB_Else);
                  Emit_List (Env, Else_Statements (Node));
                  Discard (Env.Bld.Br (BB_Next));
               end if;

               Env.Bld.Position_At_End (BB_Next);
            end;

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
               Cond              : Value_T;
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
                  then Env.Get (Entity (Identifier (Node)))
                  else Create_Basic_Block (Env, ""));
               Discard (Env.Bld.Br (BB_Init));
               Env.Bld.Position_At_End (BB_Init);

               --  If this is not a FOR loop, there is no initialization: alias
               --  it with the COND block.
               BB_Cond :=
                 (if not Is_For_Loop
                  then BB_Init
                  else Env.Create_Basic_Block ("loop-cond"));

               --  If this is a mere loop, there is even no condition block:
               --  alias it with the STMTS block.
               BB_Stmts :=
                 (if Is_Mere_Loop
                  then BB_Cond
                  else Env.Create_Basic_Block ("loop-stmts"));

               --  If this is not a FOR loop, there is no iteration: alias it
               --  with the COND block, so that at the end of every STMTS, jump
               --  on ITER or COND.
               BB_Iter :=
                 (if Is_For_Loop then Env.Create_Basic_Block ("loop-iter")
                  else BB_Cond);

               --  The NEXT step contains no statement that comes from the
               --  loop: it is the exit point.
               BB_Next := Create_Basic_Block (Env, "loop-exit");

               if Present (Loop_Identifier) then
                  Env.Push_Loop (Loop_Identifier, BB_Next);
               end if;
               Env.Push_Scope;

               --  First compile the iterative part of the loop: evaluation of
               --  the exit condition, etc.
               if not Is_Mere_Loop then
                  if not Is_For_Loop then
                     --  This is a WHILE loop: jump to the loop-body if the
                     --  condition evaluates to True, jump to the loop-exit
                     --  otherwise.
                     Env.Bld.Position_At_End (BB_Cond);
                     Cond := Emit_Expression (Env, Condition (Iter_Scheme));
                     Discard (Env.Bld.Cond_Br (Cond, BB_Stmts, BB_Next));

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
                          Is_Unsigned_Type (Etype (Def_Ident));
                        LLVM_Type       : Type_T;
                        LLVM_Var        : Value_T;
                        Low, High       : Value_T;
                     begin
                        --  Initialization block: create the loop variable and
                        --  initialize it.
                        Create_Discrete_Type
                          (Env, Etype (Def_Ident), LLVM_Type, Low, High);
                        LLVM_Var := Env.Bld.Alloca
                          (LLVM_Type, Get_Name (Def_Ident));
                        Env.Set (Def_Ident, LLVM_Var);
                        Env.Bld.Store
                          ((if Reversed then High else Low), LLVM_Var);

                        --  Then go to the condition block if the range isn't
                        --  empty.
                        Cond := Env.Bld.I_Cmp
                          ((if Unsigned_Type then Int_ULE else Int_SLE),
                           Low, High,
                           "loop-entry-cond");
                        Discard (Env.Bld.Cond_Br (Cond, BB_Cond, BB_Next));

                        --  The FOR loop is special: the condition is evaluated
                        --  during the INIT step and right before the ITER
                        --  step, so there is nothing to check during the
                        --  COND step.
                        Env.Bld.Position_At_End (BB_Cond);
                        Discard (Env.Bld.Br (BB_Stmts));

                        BB_Cond := Env.Create_Basic_Block ("loop-cond-iter");
                        Env.Bld.Position_At_End (BB_Cond);
                        Cond := Env.Bld.I_Cmp
                           (Int_EQ, Env.Bld.Load (LLVM_Var, "loop-var"), High,
                            "loop-iter-cond");
                        Discard (Env.Bld.Cond_Br (Cond, BB_Next, BB_Iter));

                        --  After STMTS, stop if the loop variable was equal to
                        --  the "exit" bound. Increment/decrement it otherwise.
                        Env.Bld.Position_At_End (BB_Iter);
                        declare
                           Iter_Prev_Value : constant Value_T :=
                             Env.Bld.Load (LLVM_Var, "loop-var");
                           One             : constant Value_T :=
                             Const_Int
                               (LLVM_Type, 1, False);
                           Iter_Next_Value : constant Value_T :=
                             (if Reversed
                              then Env.Bld.Sub
                                (Iter_Prev_Value, One, "next-loop-var")
                              else Env.Bld.Add
                                (Iter_Prev_Value, One, "next-loop-var"));
                        begin
                           Env.Bld.Store (Iter_Next_Value, LLVM_Var);
                        end;
                        Discard (Env.Bld.Br (BB_Stmts));

                        --  The ITER step starts at this special COND step
                        BB_Iter := BB_Cond;
                     end;
                  end if;
               end if;

               Env.Bld.Position_At_End (BB_Stmts);
               Emit_List (Env, Statements (Node));
               Discard (Env.Bld.Br (BB_Iter));

               Env.Pop_Scope;
               Env.Pop_Loop;

               Env.Bld.Position_At_End (BB_Next);
            end;

         when N_Block_Statement =>
            declare
               BB          : Basic_Block_T;
               Stack_State : Value_T;
            begin
               BB :=
                 (if Present (Identifier (Node))
                  then Env.Get (Entity (Identifier (Node)))
                  else Create_Basic_Block (Env, "block"));
               Discard (Env.Bld.Br (BB));
               Env.Bld.Position_At_End (BB);

               Env.Push_Scope;
               Stack_State := Env.Bld.Call
                 (Get_Stack_Save (Env), System.Null_Address, 0, "");

               Emit_List (Env, Declarations (Node));
               Emit_List
                 (Env, Statements (Handled_Statement_Sequence (Node)));

               Discard
                 (Env.Bld.Call
                    (Get_Stack_Restore (Env), Stack_State'Address, 1, ""));

               Env.Pop_Scope;
            end;

         when N_Full_Type_Declaration | N_Subtype_Declaration
            | N_Incomplete_Type_Declaration | N_Private_Type_Declaration =>
            Env.Set (Defining_Identifier (Node),
                     Create_Type (Env, Defining_Identifier (Node)));

         when N_Freeze_Entity =>
            Emit_List (Env, Actions (Node));

         when N_Pragma =>
            case Get_Pragma_Id (Node) is
               --  TODO??? While we aren't interested in most of the pragmas,
               --  there are some we should look at. But still, the "others"
               --  case is necessary.
               when others => null;
            end case;

         --  Nodes we actually want to ignore
         when N_Empty | N_Procedure_Instantiation
            | N_Function_Instantiation
            | N_Validate_Unchecked_Conversion
            | N_Itype_Reference =>
            null;

         when N_Attribute_Definition_Clause =>
            if Get_Name (Node) = "alignment" then
               --  TODO??? Handle the alignment clause
               null;
            elsif Get_Name (Node) = "size" then
               --  TODO??? Handle size clauses
               null;
            else
               pragma Annotate (Xcov, Exempt_On, "Defensive programming");
               raise Program_Error
                 with "Unhandled attribute definition clause: "
                 & Get_Name (Node);
               pragma Annotate (Xcov, Exempt_Off);
            end if;

         when others =>
            pragma Annotate (Xcov, Exempt_On, "Defensive programming");
            raise Program_Error
              with "Unhandled statement node kind: "
              & Node_Kind'Image (Nkind (Node));
            pragma Annotate (Xcov, Exempt_Off);

      end case;
   end Emit;

   -----------------
   -- Emit_LValue --
   -----------------

   function Emit_LValue (Env : Environ; Node : Node_Id) return Value_T
   is
   begin
      case Nkind (Node) is
         when N_Identifier | N_Expanded_Name =>
            return Env.Get (Entity (Node));

         when N_Explicit_Dereference =>
            return Emit_Expression (Env, Prefix (Node));

         when N_Selected_Component =>
            declare
               Pfx_Ptr : constant Value_T :=
                 Emit_LValue (Env, Prefix (Node));
               Record_Component : constant Entity_Id :=
                 Parent (Entity (Selector_Name (Node)));
            begin
               return Record_Field_Offset (Env, Pfx_Ptr, Record_Component);
            end;

         when N_Indexed_Component =>
            declare
               Array_Ptr : Value_T :=
                 Emit_LValue (Env, Prefix (Node));

               Idxs    :
               Value_Array (1 .. List_Length (Expressions (Node)) + 1) :=
                 (1      => Const_Int (Intptr_T, 0, Sign_Extend => False),
                  others => <>);
               --  Operands for the GetElementPtr instruction: one for the
               --  pointer deference, and then one per array index.

               I       : Nat := 2;
               DSD     : Node_Id := First_Index (Etype (Prefix (Node)));
               LB      : Value_T;
               Constrained : constant Boolean :=
                 Is_Constrained (Etype (Prefix (Node)));
            begin

               for N of Iterate (Expressions (Node)) loop
                  Idxs (I) := Emit_Expression (Env, N);

                  if Nkind (DSD) /= N_Range then
                     pragma Annotate
                       (Xcov, Exempt_On, "Defensive programming");
                     raise Program_Error
                       with "Arrays indexed with" & Nkind (DSD)'Img
                       & " not supported";
                     pragma Annotate (Xcov, Exempt_Off);
                  end if;

                  --  Adjust the index according to the range lower bound
                  if Constrained then
                     LB := Emit_Expression (Env, Low_Bound (DSD));
                  else
                     LB := Array_Bound
                       (Env, Array_Ptr, Low, Integer (I - 1));
                  end if;

                  Idxs (I) := Env.Bld.Sub (Idxs (I), LB, "index");

                  I := I + 1;
                  DSD := Next (DSD);
               end loop;

               if not Constrained then
                  Array_Ptr := Env.Bld.Load
                    (Env.Bld.Struct_GEP (Array_Ptr, 0, ""), "");
               end if;

               return
                 Env.Bld.GEP (Array_Ptr, Idxs, "array-access");
            end;

         when N_Slice =>
            declare
               Array_Node  : constant Node_Id := Prefix (Node);
               Array_Ptr   : Value_T :=
                 Emit_LValue (Env, Array_Node);

               --  Compute how much we need to offset the array pointer. Slices
               --  can be built only on single-dimension arrays

               Index_Shift : constant Value_T :=
                 Env.Bld.Sub
                   (Emit_Expression (Env, Low_Bound (Discrete_Range (Node))),
                    Array_Bound (Env, Array_Node, Low),
                    "offset");
            begin
               if not Is_Constrained (Etype (Prefix (Node))) then
                  Array_Ptr :=
                    Env.Bld.Load
                      (Env.Bld.Struct_GEP (Array_Ptr, 0, "fat-to-thin"), "");
               end if;

               return Env.Bld.Bit_Cast
                 (Env.Bld.GEP
                    (Array_Ptr,
                     (Const_Int (Intptr_T, 0, Sign_Extend => False),
                      Index_Shift),
                     "array-shifted"),
                  Create_Access_Type (Env, Etype (Node)),
                  "slice");
            end;

         when others =>
            pragma Annotate (Xcov, Exempt_On, "Defensive programming");
            raise Program_Error
              with "Unhandled node kind: " & Node_Kind'Image (Nkind (Node));
            pragma Annotate (Xcov, Exempt_Off);
      end case;
   end Emit_LValue;

   ---------------------
   -- Emit_Expression --
   ---------------------

   function Emit_Expression
     (Env : Environ; Node : Node_Id) return Value_T is

      function Compile_Expr (Node : Node_Id) return Value_T is
        (Emit_Expression (Env, Node));

      type Scl_Op is (Op_Or, Op_And);

      function Build_Scl_Op (Op : Scl_Op) return Value_T;
      --  Emit the LLVM IR for a short circuit operator ("or else", "and then")

      function Build_Scl_Op (Op : Scl_Op) return Value_T is
      begin
         declare

            --  The left expression of a SCL op is always evaluated.

            Left : constant Value_T := Compile_Expr (Left_Opnd (Node));
            Result : constant Value_T :=
              Env.Bld.Alloca (Type_Of (Left), "scl-res-1");

            --  Block which contains the evaluation of the right part
            --  expression of the operator.

            Block_Right_Expr : constant Basic_Block_T :=
              Append_Basic_Block (Env.Current_Subp.Func, "scl-right-expr");

            --  Block containing the exit code (load the final cond value into
            --  Result

            Block_Exit : constant Basic_Block_T :=
              Append_Basic_Block (Env.Current_Subp.Func, "scl-exit");

         begin
            Env.Bld.Store (Left, Result);

            --  In the case of And, evaluate the right expression when Left is
            --  true. In the case of Or, evaluate it when Left is false.

            if Op = Op_And then
               Discard
                 (Env.Bld.Cond_Br (Left, Block_Right_Expr, Block_Exit));
            else
               Discard
                 (Env.Bld.Cond_Br (Left, Block_Exit, Block_Right_Expr));
            end if;

            --  Emit code for the evaluation of the right part expression

            Position_At_End (Env.Bld, Block_Right_Expr);

            declare
               Right : constant Value_T := Compile_Expr (Right_Opnd (Node));
               Left : constant Value_T := Env.Bld.Load (Result, "load-left");
               Res : Value_T;
            begin
               if Op = Op_And then
                  Res := Build_And (Env.Bld, Left, Right, "scl-and");
               else
                  Res := Build_Or (Env.Bld, Left, Right, "scl-or");
               end if;
               Env.Bld.Store (Res, Result);
               Discard (Env.Bld.Br (Block_Exit));
            end;

            Position_At_End (Env.Bld, Block_Exit);

            return Env.Bld.Load (Result, "scl-final-res");
         end;
      end Build_Scl_Op;

   begin
      if Is_Binary_Operator (Node) then
         declare
            LVal : constant Value_T :=
              Compile_Expr (Left_Opnd (Node));
            RVal : constant Value_T :=
              Compile_Expr (Right_Opnd (Node));
            Op : Value_T;

            function Emit_Cmp (N : Node_Id) return Value_T;
            --  Compilation of binary comparison operators

            function Emit_Cmp (N : Node_Id) return Value_T is
               PM : constant Pred_Mapping := Get_Preds (N);
               T : constant Entity_Id := Etype (Left_Opnd (Node));
            begin

               --  LLVM treats pointers as integers regarding comparison

               if Is_Integer_Type (T)
                 or else Is_Access_Type (T)
               then
                  return Env.Bld.I_Cmp
                    ((if Is_Unsigned_Type (T) then PM.Unsigned else PM.Signed),
                     LVal, RVal, "icmp");
               elsif Is_Floating_Point_Type (Etype (Left_Opnd (Node))) then
                  return Env.Bld.F_Cmp (PM.Real, LVal, RVal, "fcmp");
               else
                  raise Program_Error
                    with "EQ only for int and real types";
                  --  TODO : Equality for aggregate types
               end if;
            end Emit_Cmp;

         begin
            case Nkind (Node) is

            when N_Op_Add =>
               Op := Env.Bld.Add (LVal, RVal, "add");

            when N_Op_Subtract =>
               Op := Env.Bld.Sub (LVal, RVal, "sub");

            when N_Op_Multiply =>
               Op := Env.Bld.Mul (LVal, RVal, "mul");

            when N_Op_Divide =>
               declare
                  T : constant Entity_Id := Etype (Left_Opnd (Node));
               begin
                  if Is_Signed_Integer_Type (T) then
                     Op := Env.Bld.S_Div (LVal, RVal, "sdiv");
                  elsif Is_Floating_Point_Type (T) then
                     Op := Env.Bld.F_Div (LVal, RVal, "fdiv");
                  elsif Is_Unsigned_Type (T) then
                     return Env.Bld.U_Div (LVal, RVal, "udiv");
                  else
                     pragma Annotate
                       (Xcov, Exempt_On, "Defensive programming");
                     raise Program_Error
                       with "Not handled : Division with type " & T'Img;
                     pragma Annotate (Xcov, Exempt_Off);
                  end if;
               end;

            when N_Op_Gt | N_Op_Lt | N_Op_Le | N_Op_Ge | N_Op_Eq | N_Op_Ne =>
               return Emit_Cmp (Node);

            when others =>
               pragma Annotate (Xcov, Exempt_On, "Defensive programming");
               raise Program_Error
                 with "Unhandled node kind in expression: "
                 & Node_Kind'Image (Nkind (Node));
               pragma Annotate (Xcov, Exempt_Off);

            end case;

            --  We need to handle modulo manually for non binary modulus types.

            if Non_Binary_Modulus (Etype (Node)) then
               Op := Env.Bld.U_Rem
                 (Op,
                  Const_Int
                    (Create_Type (Env, Etype (Node)), Modulus (Etype (Node))),
                 "mod");
            end if;

            return Op;
         end;

      else

         case Nkind (Node) is

         when N_Expression_With_Actions =>
            --  TODO??? Compile the list of actions
--              pragma Assert (Is_Empty_List (Actions (Node)));
            return Compile_Expr (Expression (Node));

         when N_Character_Literal =>
            return Const_Int
              (Create_Type (Env, Etype (Node)),
               Char_Literal_Value (Node));

         when N_Integer_Literal =>
            return Const_Int
              (Create_Type (Env, Etype (Node)),
               Intval (Node));

         when N_String_Literal =>
            declare
               String       : constant String_Id := Strval (Node);
               Array_Type   : constant Type_T :=
                 Create_Type (Env, Etype (Node));
               Element_Type : constant Type_T := Get_Element_Type (Array_Type);
               Length       : constant Interfaces.C.unsigned :=
                 Get_Array_Length (Array_Type);
               Elements     : array (1 .. Length) of Value_T;
            begin
               for I in Elements'Range loop
                  Elements (I) := Const_Int
                    (Element_Type,
                     unsigned_long_long
                       (Get_String_Char (String, Standard.Types.Int (I))),
                     Sign_Extend => True);
               end loop;
               return Const_Array (Element_Type, Elements'Address, Length);
            end;

         when N_And_Then => return Build_Scl_Op (Op_And);
         when N_Or_Else => return Build_Scl_Op (Op_Or);

         when N_Op_Plus => return Compile_Expr (Right_Opnd (Node));
         when N_Op_Minus => return Env.Bld.Sub
              (Const_Int
                 (Create_Type (Env, Etype (Node)), 0, False),
               Compile_Expr (Right_Opnd (Node)),
               "minus");

         when N_Unchecked_Type_Conversion =>
            declare
               Val     : constant Value_T := Compile_Expr (Expression (Node));
               Val_Ty  : constant Type_T := LLVM_Type_Of (Val);
               Dest_Ty : constant Type_T := Create_Type (Env, Etype (Node));
               Val_Tk  : constant Type_Kind_T := Get_Type_Kind (Val_Ty);
               Dest_Tk : constant Type_Kind_T := Get_Type_Kind (Dest_Ty);
            begin
               if Val_Tk = Pointer_Type_Kind then
                  return Env.Bld.Pointer_Cast
                    (Val, Dest_Ty, "unchecked-conv");
               elsif
                 Val_Tk = Integer_Type_Kind
                 and then Dest_Tk = Integer_Type_Kind
               then
                  return Env.Bld.Int_Cast
                    (Val, Dest_Ty, "unchecked-conv");
               elsif Val_Tk = Integer_Type_Kind
                 and then Dest_Tk = Pointer_Type_Kind
               then
                  return Env.Bld.Int_To_Ptr (Val, Dest_Ty, "unchecked-conv");
               else
                  pragma Annotate (Xcov, Exempt_On, "Defensive programming");
                  raise Program_Error
                    with "Invalid conversion, should never happen";
                  pragma Annotate (Xcov, Exempt_Off);
               end if;
            end;

         when N_Type_Conversion =>
            declare
               Src_T : constant Entity_Id := Etype (Node);
               Dest_T : constant Entity_Id := Etype (Expression (Node));
            begin

               --  For the moment, we handle only the simple case of Integer to
               --  Integer conversions.

               if Is_Integer_Type (Get_Fullest_View (Src_T))
                 and then Is_Integer_Type (Get_Fullest_View (Dest_T))
               then
                  if Src_T = Dest_T then
                     return Compile_Expr (Expression (Node));
                  end if;

                  return Env.Bld.S_Ext
                    (Compile_Expr (Expression (Node)),
                     Create_Type (Env, Etype (Node)),
                     "int_conv");
               elsif Is_Descendent_Of_Address (Src_T)
                 and then Is_Descendent_Of_Address (Dest_T)
               then
                  return Env.Bld.Bit_Cast
                    (Compile_Expr (Expression (Node)),
                     Create_Type (Env, Etype (Node)), "address-conv");
               else
                  pragma Annotate (Xcov, Exempt_On, "Defensive programming");
                  raise Program_Error with "Unhandled type conv";
                  pragma Annotate (Xcov, Exempt_Off);
               end if;
            end;

         when N_Identifier | N_Expanded_Name =>
            --  N_Defining_Identifier nodes for enumeration literals are not
            --  store in the environment. Handle them here.

            if Ekind (Entity (Node)) = E_Enumeration_Literal then
               return Const_Int
                 (Create_Type (Env, Etype (Node)),
                  Enumeration_Rep (Entity (Node)), False);
            else
               --  LLVM functions are pointers that cannot be dereferenced. If
               --  Entity (Node) is a subprogram, return it as-is, the caller
               --  expects a pointer to a function anyway.

               declare
                  LValue : constant Value_T := Env.Get (Entity (Node));
                  Is_Subprogram : constant Boolean :=
                    (Ekind (Entity (Node)) = E_Function
                     or else Ekind (Entity (Node)) = E_Procedure);
               begin
                  return
                    (if Is_Subprogram then
                        LValue
                     else
                        Env.Bld.Load (Env.Get (Entity (Node)), ""));
               end;
            end if;

         when N_Function_Call =>
            return Call (Env, Node);

         when N_Explicit_Dereference =>
            --  Special handling for accesses to subprograms, see N_Identifier

            declare
               Access_Value : constant Value_T :=
                 Compile_Expr (Prefix (Node));
            begin
               return
                 (if Ekind (Etype (Node)) = E_Subprogram_Type
                  then Access_Value
                  else Env.Bld.Load (Access_Value, ""));
            end;

         when N_Allocator =>
            declare
               Arg : array (1 .. 1) of Value_T :=
                 (1 => Size_Of (Create_Type (Env, Etype (Expression (Node)))));
            begin
               if Nkind (Expression (Node)) = N_Identifier then
                  return Env.Bld.Bit_Cast
                    (Env.Bld.Call
                       (Env.Default_Alloc_Fn, Arg'Address, 1, "alloc"),
                     Create_Type (Env, Etype (Node)),
                     "alloc_bc");
               else
                  pragma Annotate (Xcov, Exempt_On, "Defensive programming");
                  raise Program_Error
                    with "Non handled form in N_Allocator";
                  pragma Annotate (Xcov, Exempt_Off);
               end if;
            end;

         when N_Attribute_Reference =>

            --  We store values as pointers, so, getting an access to an
            --  expression is the same thing as getting an LValue, and has
            --  the same constraints

            declare
               Attr_Name : constant String :=
                 Get_Name_String (Attribute_Name (Node));
            begin
               if Attr_Name = "access"
                 or else Attr_Name = "unchecked_access"
                 or else Attr_Name = "unrestricted_access"
               then
                  return Emit_LValue (Env, Prefix (Node));
               elsif Attr_Name = "address" then
                  return Env.Bld.Ptr_To_Int
                    (Emit_LValue
                       (Env, Prefix (Node)), Get_Address_Type, "to-address");
               elsif Attr_Name = "first" then
                  return Array_Bound (Env, Prefix (Node), Low, 1);
               elsif Attr_Name = "last" then
                  return Array_Bound
                    (Env, Prefix (Node), High, 1);
               end if;
            end;

            pragma Annotate (Xcov, Exempt_On, "Defensive programming");
            raise Program_Error
              with "Unhandled Attribute : "
              & Get_Name_String (Attribute_Name (Node));
            pragma Annotate (Xcov, Exempt_Off);

            when N_Selected_Component =>
               declare
                  Pfx_Val : constant Value_T :=
                    Emit_Expression (Env, Prefix (Node));
                  Pfx_Ptr : constant Value_T :=
                    Env.Bld.Alloca (Type_Of (Pfx_Val), "pfx_ptr");
                  Record_Component : constant Entity_Id :=
                    Parent (Entity (Selector_Name (Node)));
               begin
                  Env.Bld.Store (Pfx_Val, Pfx_Ptr);
                  return Env.Bld.Load
                    (Record_Field_Offset (Env, Pfx_Ptr, Record_Component), "");
               end;

            when N_Indexed_Component | N_Slice =>
               return Env.Bld.Load (Emit_LValue (Env, Node), "");

            when N_Aggregate =>
               declare
                  Agg_Type   : constant Entity_Id := Etype (Node);
                  LLVM_Type  : constant Type_T :=
                    Create_Type (Env, Agg_Type);
                  Result     : Value_T := Get_Undef (LLVM_Type);
                  Cur_Expr   : Value_T;
                  Cur_Index  : Integer;
               begin
                  if Ekind (Agg_Type) in Record_Kind then
                     for Assoc of Iterate (Component_Associations (Node)) loop
                        Cur_Expr := Compile_Expr (Expression (Assoc));
                        for Choice of Iterate (Choices (Assoc)) loop
                           Cur_Index := Index_In_List
                             (Parent (Entity (Choice)));
                           Result := Env.Bld.Insert_Value
                             (Result, Cur_Expr, unsigned (Cur_Index - 1), "");
                        end loop;
                     end loop;

                  --  Must be an array

                  else
                     Cur_Index := 0;
                     for Expr of Iterate (Expressions (Node)) loop
                        Cur_Expr := Compile_Expr (Expr);
                        Result := Env.Bld.Insert_Value
                          (Result, Cur_Expr, unsigned (Cur_Index), "");
                        Cur_Index := Cur_Index + 1;
                     end loop;

                  end if;

                  return Result;
               end;

            when N_Null =>
               return Const_Null (Create_Type (Env, Etype (Node)));

            when others =>
               pragma Annotate (Xcov, Exempt_On, "Defensive programming");
               raise Program_Error
                 with "Unhandled node kind: " & Node_Kind'Image (Nkind (Node));
               pragma Annotate (Xcov, Exempt_Off);
         end case;
      end if;
   end Emit_Expression;

   ---------------
   -- Emit_List --
   ---------------

   procedure Emit_List
     (Env : Environ; List : List_Id) is
   begin
      for N of Iterate (List) loop
         Emit (Env, N);
      end loop;
   end Emit_List;

   ----------
   -- Call --
   ----------

   function Call
     (Env : Environ; Call_Node : Node_Id) return Value_T
   is
      function Is_Formal (E : Entity_Id) return Boolean is
        (Ekind (E) in Formal_Kind);

      function Iterate is new Iterate_Entities
        (Get_First => First_Entity,
         Get_Next  => Next_Entity,
         Filter    => Is_Formal);

      Subp        : constant Node_Id := Name (Call_Node);
      Params      : constant Entity_Iterator :=
        (if Nkind (Subp) = N_Identifier
         or else Nkind (Subp) = N_Expanded_Name
         then Iterate (Entity (Subp))
         else Iterate (Etype (Subp)));
      Actual      : Node_Id;

      LLVM_Func   : Value_T;
      Args        : array (1 .. Params'Length) of Value_T;
      I           : Standard.Types.Int := 1;
      P_Type      : Entity_Id;

   begin
      LLVM_Func := Emit_Expression (Env, Name (Call_Node));

      Actual := First (Parameter_Associations (Call_Node));
      while Present (Actual) loop
         Args (Natural (I)) :=
           (if Param_Needs_Ptr (Params (I))
            then Emit_LValue (Env, Actual)
            else Emit_Expression (Env, Actual));

         --  At this point we need to handle the conversion from constrained
         --  arrays to unconstrained arrays

         P_Type := Etype (Params (I));
         if Is_Constrained (Etype (Actual))
           and then not Is_Constrained (P_Type)
         then
            declare
               Array_Access_Type : constant Type_T :=
                 Create_Access_Type (Env, P_Type);
               Array_Access, V : Value_T;
               Dim_Idx : Integer := 1;
            begin
               Array_Access :=
                 Env.Bld.Alloca (Array_Access_Type, "");

               --  Store the ptr into the access struct
               V := Env.Bld.Struct_GEP (Array_Access, 0, "");
               Env.Bld.Store
                 (Env.Bld.Bit_Cast
                    (Args (Natural (I)),
                     Pointer_Type
                       (Create_Type
                            (Env, Etype (Params (I))), 0), ""), V);

               --  Store the bounds into the access struct
               for Dim of
                 Iterate (List_Containing (First_Index (Etype (Actual))))
               loop
                  declare
                     R : constant Node_Id := Get_Dim_Range (Dim);
                  begin
                     V := Array_Bound_Addr (Env, Array_Access, Low, Dim_Idx);
                     Env.Bld.Store (Emit_Expression (Env, Low_Bound (R)), V);

                     V := Array_Bound_Addr (Env, Array_Access, High, Dim_Idx);
                     Env.Bld.Store (Emit_Expression (Env, High_Bound (R)), V);

                     Dim_Idx := Dim_Idx + 1;
                  end;
               end loop;

               --  Replace the simple pointer by the access struct
               Args (Natural (I)) := Env.Bld.Load (Array_Access, "");
            end;
         end if;

         I := I + 1;
         Actual := Next (Actual);
      end loop;

      return
        Env.Bld.Call
          (LLVM_Func, Args'Address, Args'Length,
           --  Assigning a name to a void value is not possible with LLVM
           (if Nkind (Call_Node) = N_Function_Call then "subpcall" else ""));
   end Call;

   --------------------------
   -- Emit_Subprogram_Decl --
   --------------------------

   function Emit_Subprogram_Decl
     (Env : Environ; Subp_Spec : Node_Id) return Value_T
   is
      Def_Ident : constant Node_Id := Defining_Unit_Name (Subp_Spec);
   begin
      --  If this subprogram specification has already been compiled, do
      --  nothing.

      if Env.Has_Value (Def_Ident) then
         return Env.Get (Def_Ident);

      else
         declare
            Subp_Type : constant Type_T :=
              Create_Subprogram_Type_From_Spec (Env, Subp_Spec);

            Subp_Base_Name : constant String :=
                Get_Name_String (Chars (Def_Ident));
            Subp_Name : constant String :=
              (if Scope_Depth_Value (Def_Ident) > 1
               then Subp_Base_Name
               else "_ada_" & Subp_Base_Name);

            LLVM_Func : constant Value_T :=
              Add_Function (Env.Mdl, Subp_Name, Subp_Type);
         begin
            --  Define the appropriate linkage

            if not Is_Public (Def_Ident) then
               Set_Linkage (LLVM_Func, Internal_Linkage);
            end if;

            Env.Set (Def_Ident, LLVM_Func);
            return LLVM_Func;
         end;
      end if;
   end Emit_Subprogram_Decl;

end GNATLLVM.Compile;
