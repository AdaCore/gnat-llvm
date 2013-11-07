with Interfaces.C;            use Interfaces.C;
with System;

with Atree;    use Atree;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Sinfo;    use Sinfo;
with Sem_Util; use Sem_Util;
with Uintp;    use Uintp;

with LLVM.Analysis; use LLVM.Analysis;

with Get_Targ;       use Get_Targ;
with GNATLLVM.Types; use GNATLLVM.Types;
with GNATLLVM.Utils; use GNATLLVM.Utils;

package body GNATLLVM.Compile is

   type Value_Array is array (Nat range <>) of Value_T;

   procedure Store (Env : Environ; Expr : Value_T; Ptr : Value_T);
   --  Helper for LLVM's Build_Store

   function GEP
     (Env : Environ; Ptr : Value_T; Indices : Value_Array; Name : String)
      return Value_T;
   --  Helper for LLVM's Build_GEP

   function Is_LValue (Node : Node_Id) return Boolean;
   --  Returns true if Node is an L value

   function Const_Bound (B : Integer) return Value_T;
   --  Return an LLVM value of the type of array bounds which value is B

   procedure Compile_List
     (Env : Environ; List : List_Id);
   --  Call Compile on every element of List

   function Compile_Call
     (Env : Environ; Call : Node_Id) return Value_T;

   function Compile_Subprogram_Decl
     (Env : Environ; Subp_Spec : Node_Id) return Value_T;
   --  Compile a subprogram declaration, save the corresponding LLVM value to
   --  the environment and return it.

   function Compile_Array_Size
     (Env : Environ; Array_Type : Entity_Id) return Value_T;

   -----------------
   -- Const_Bound --
   -----------------

   function Const_Bound (B : Integer) return Value_T
   is
      (Const_Int (Array_Bounds_Type, B));

   ---------
   -- GEP --
   ---------

   function GEP
     (Env : Environ; Ptr : Value_T; Indices : Value_Array; Name : String)
      return Value_T
   is
     (Build_GEP (Env.Bld, Ptr, Indices'Address, Indices'Length, Name));

   -----------
   -- Store --
   -----------

   procedure Store (Env : Environ; Expr : Value_T; Ptr : Value_T)
   is
   begin
      Discard (Build_Store (Env.Bld, Expr, Ptr));
   end Store;

   ------------------------
   -- Compile_Array_Size --
   ------------------------

   function Compile_Array_Size
     (Env : Environ; Array_Type : Entity_Id) return Value_T
   is
      CT       : constant Entity_Id := Component_Type (Array_Type);
      T        : constant Type_T :=
        Int_Type_In_Context (Env.Ctx, unsigned (Get_Pointer_Size));
      --  An array can be as big as the memory space, so use the appropriate
      --  type.

      Size     : Value_T := No_Value_T;
      Cur_Size : Value_T;
      DSD      : Node_Id := First_Index (Array_Type);
   begin

      --  Go through every array dimension

      while Present (DSD) loop

         case Nkind (DSD) is
            when N_Range =>

               --  Compute the size of the dimension from the range bounds

               Cur_Size := Build_Add
                 (Env.Bld,
                  Build_Sub
                    (Env.Bld, Compile_Expression (Env, High_Bound (DSD)),
                     Compile_Expression (Env, Low_Bound (DSD)), ""),
                  Const_Int
                    (Create_Type (Env, Etype (High_Bound (DSD))), 1), "");
               Cur_Size := Build_Z_Ext (Env.Bld, Cur_Size, T, "");

               --  Accumulate the product of the sizes
               --  If it's the first dimension, initialize our result with it
               --  Else, multiply our result by it

               if Size = No_Value_T then
                  Size := Cur_Size;
               else
                  Size := Build_Mul (Env.Bld, Size, Cur_Size, "");
               end if;

            when others =>
               raise Program_Error with "Not supported : " & Nkind (DSD)'Img;

         end case;

         DSD := Next (DSD);

      end loop;

      --  If the component of the array is itself an array, then recursively
      --  compute the size of the component and return the product

      if Is_Array_Type (CT) then
         return Build_Mul (Env.Bld, Size, Compile_Array_Size (Env, CT), "");
      else
         return Size;
      end if;

   end Compile_Array_Size;

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Env : Environ; Node : Node_Id) is
   begin
      case Nkind (Node) is

         when N_Package_Declaration =>
            Compile (Env, Specification (Node));

         when N_Package_Specification =>
            Compile_List (Env, Visible_Declarations (Node));

         when N_Package_Body =>
            declare
               Def_Id : constant Entity_Id := Unique_Defining_Entity (Node);
            begin
               if Ekind (Def_Id) not in Generic_Unit_Kind then
                  Compile_List (Env, Declarations (Node));
                  --  TODO : Handle statements
               end if;
            end;

         when N_Subprogram_Declaration =>
            Discard (Compile_Subprogram_Decl (Env, Specification (Node)));

         when N_Subprogram_Body =>
            declare
               Spec       : constant Node_Id :=
                 (if Acts_As_Spec (Node)
                  then Specification (Node)
                  else Parent (Corresponding_Spec (Node)));
               Func       : constant Value_T :=
                 Compile_Subprogram_Decl (Env, Spec);
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

                  if Param_Needs_Ptr (P)
                    and then not
                      (Get_Type_Kind (Type_Of (LLVM_Param)) = Struct_Type_Kind)
                  then
                     LLVM_Var := LLVM_Param;
                  else
                     LLVM_Var := Build_Alloca
                       (Env.Bld,
                        Type_Of (LLVM_Param),
                        Get_Name (Param));
                     Store (Env, LLVM_Param, LLVM_Var);
                  end if;

                  --  Add the parameter to the environnment

                  Env.Set (Param, LLVM_Var);
                  if Spec_Entity (Param) /= 0
                    and then Spec_Entity (Param) /= Param then
                     Env.Set (Spec_Entity (Param), LLVM_Var);
                  end if;

                  I := I + 1;
               end loop;

               Compile_List (Env, Declarations (Node));
               Compile_List
                 (Env, Statements (Handled_Statement_Sequence (Node)));

               --  This point should not be reached: a return must have
               --  already... returned!

               Discard (Build_Unreachable (Env.Bld));

               Env.Pop_Scope;
               Env.Leave_Subp;

               if Verify_Function (Subp.Func, Print_Message_Action) /= 0 then
                  Error_Msg_N
                    ("The backend generated bad LLVM for this subprogram.",
                     Node);
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
                 and then Esize (Entity (Obj_Def)) = 0 then
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

                  LLVM_Var := Build_Bit_Cast
                    (Env.Bld,
                     Build_Array_Alloca
                       (Env.Bld,
                        Get_Innermost_Component_Type (Env, T),
                        Compile_Array_Size (Env, T), "array-alloca"),
                     LLVM_Type,
                     Get_Name (Def_Ident));
               else
                  LLVM_Type := Create_Type (Env, T);
                  LLVM_Var := Build_Alloca
                    (Env.Bld, LLVM_Type, Get_Name (Def_Ident));
               end if;

               Env.Set (Def_Ident, LLVM_Var);

               if Present (Expression (Node))
                 and then not No_Initialization (Node)
               then
                  --  TODO??? Handle the Do_Range_Check_Flag
                  Expr := Compile_Expression (Env, Expression (Node));
                  Store (Env, Expr, LLVM_Var);
               end if;
            end;

         when N_Renaming_Declaration =>
            declare
               Def_Ident : constant Node_Id := Defining_Identifier (Node);
               LLVM_Var  : Value_T;
            begin

               --  If the renamed object is already an l-value, keep it as-is.
               --  Otherwise, create one for it.

               if Is_LValue (Name (Node)) then
                  LLVM_Var := Compile_LValue (Env, Name (Node));
               else
                  LLVM_Var := Build_Alloca
                    (Env.Bld,
                     Create_Type (Env, Etype (Def_Ident)),
                     Get_Name (Def_Ident));
                  Store (Env, Compile_Expression (Env, Name (Node)), LLVM_Var);
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
                 Compile_Expression (Env, Expression (Node));
               Dest : constant Value_T := Compile_LValue (Env, Name (Node));
            begin
               Store (Env, Val, Dest);
            end;

         when N_Procedure_Call_Statement =>
            Discard (Compile_Call (Env, Node));

         when N_Null_Statement =>
            null;

         when N_Label =>
            declare
               BB : constant Basic_Block_T :=
                 Env.Get (Entity (Identifier (Node)));
            begin
               Discard (Build_Br (Env.Bld, BB));
               Position_Builder_At_End (Env.Bld, BB);
            end;

         when N_Goto_Statement =>
            Discard (Build_Br (Env.Bld, Env.Get (Entity (Name (Node)))));
            Position_Builder_At_End
              (Env.Bld, Env.Create_Basic_Block ("after-goto"));

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
                  Discard (Build_Cond_Br
                           (Env.Bld,
                              Compile_Expression (Env, Condition (Node)),
                              Exit_Point,
                              Next_BB));
               else
                  Discard (Build_Br (Env.Bld, Exit_Point));
               end if;
               Position_Builder_At_End (Env.Bld, Next_BB);
            end;

         when N_Simple_Return_Statement =>
            if Present (Expression (Node)) then
               Discard (Build_Ret
                        (Env.Bld,
                           Compile_Expression
                             (Env, Expression (Node))));
            else
               Discard (Build_Ret_Void (Env.Bld));
            end if;
            Position_Builder_At_End
              (Env.Bld, Env.Create_Basic_Block ("unreachable"));

         when N_If_Statement =>
            declare
               BB_Then, BB_Else, BB_Next : Basic_Block_T;
               Cond                      : constant Value_T :=
                 Compile_Expression (Env, Condition (Node));
            begin
               BB_Next := Create_Basic_Block (Env, "if-next");
               BB_Then := Create_Basic_Block (Env, "if-then");
               BB_Else :=
                 (if not Is_Empty_List (Else_Statements (Node))
                  then Create_Basic_Block (Env, "if-else")
                  else BB_Next);

               Discard (Build_Cond_Br (Env.Bld, Cond, BB_Then, BB_Else));

               Position_Builder_At_End (Env.Bld, BB_Then);
               Compile_List (Env, Then_Statements (Node));
               Discard (Build_Br (Env.Bld, BB_Next));

               if not Is_Empty_List (Else_Statements (Node)) then
                  Position_Builder_At_End (Env.Bld, BB_Else);
                  Compile_List (Env, Else_Statements (Node));
                  Discard (Build_Br (Env.Bld, BB_Next));
               end if;

               Position_Builder_At_End (Env.Bld, BB_Next);
            end;

         when N_Loop_Statement =>
            declare
               Loop_Identifier   : Entity_Id;
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
               pragma Assert (Present (Identifier (Node)));
               Loop_Identifier := Entity (Identifier (Node));

               --  The general format for a loop is:
               --    INIT;
               --    while COND loop
               --       STMTS;
               --       ITER;
               --    end loop;
               --    NEXT:
               --  Each step has its own basic block. When a loop does not need
               --  one of these steps, just alias it with another one.

               --  Every loop has an identifier, and thus this loop has already
               --  its own entry (INIT) basic block.
               BB_Init := Env.Get (Loop_Identifier);
               Discard (Build_Br (Env.Bld, BB_Init));
               Position_Builder_At_End (Env.Bld, BB_Init);

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

               Env.Push_Loop (Loop_Identifier, BB_Next);
               Env.Push_Scope;

               --  First compile the iterative part of the loop: evaluation of
               --  the exit condition, etc.
               if not Is_Mere_Loop then
                  if not Is_For_Loop then
                     --  This is a WHILE loop: jump to the loop-body if the
                     --  condition evaluates to True, jump to the loop-exit
                     --  otherwise.
                     Position_Builder_At_End (Env.Bld, BB_Cond);
                     Cond := Compile_Expression (Env, Condition (Iter_Scheme));
                     Discard (Build_Cond_Br
                              (Env.Bld, Cond, BB_Stmts, BB_Next));

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
                        LLVM_Var := Build_Alloca
                          (Env.Bld, LLVM_Type, Get_Name (Def_Ident));
                        Env.Set (Def_Ident, LLVM_Var);
                        Store
                          (Env, (if Reversed then High else Low), LLVM_Var);

                        --  Then go to the condition block if the range isn't
                        --  empty.
                        Cond := Build_I_Cmp
                          (Env.Bld,
                           (if Unsigned_Type then Int_ULE else Int_SLE),
                           Low, High,
                           "loop-entry-cond");
                        Discard (Build_Cond_Br
                                 (Env.Bld, Cond, BB_Cond, BB_Next));

                        --  The FOR loop is special: the condition is evaluated
                        --  during the INIT step and right before the ITER
                        --  step, so there is nothing to check during the
                        --  COND step.
                        Position_Builder_At_End (Env.Bld, BB_Cond);
                        Discard (Build_Br (Env.Bld, BB_Stmts));

                        BB_Cond := Env.Create_Basic_Block ("loop-cond-iter");
                        Position_Builder_At_End (Env.Bld, BB_Cond);
                        Cond := Build_I_Cmp
                          (Env.Bld,
                           Int_EQ,
                           Build_Load (Env.Bld, LLVM_Var, "loop-var"), High,
                           "loop-iter-cond");
                        Discard (Build_Cond_Br
                                 (Env.Bld, Cond, BB_Next, BB_Iter));

                        --  After STMTS, stop if the loop variable was equal to
                        --  the "exit" bound. Increment/decrement it otherwise.
                        Position_Builder_At_End (Env.Bld, BB_Iter);
                        declare
                           Iter_Prev_Value : constant Value_T :=
                             Build_Load (Env.Bld, LLVM_Var, "loop-var");
                           One             : constant Value_T :=
                             Const_Int
                               (LLVM_Type, 1, False);
                           Iter_Next_Value : constant Value_T :=
                             (if Reversed
                              then Build_Sub
                                (Env.Bld, Iter_Prev_Value, One,
                                 "next-loop-var")
                              else Build_Add
                                (Env.Bld, Iter_Prev_Value, One,
                                 "next-loop-var"));
                        begin
                           Store (Env, Iter_Next_Value, LLVM_Var);
                        end;
                        Discard (Build_Br (Env.Bld, BB_Stmts));

                        --  The ITER step starts at this special COND step
                        BB_Iter := BB_Cond;
                     end;
                  end if;
               end if;

               Position_Builder_At_End (Env.Bld, BB_Stmts);
               Compile_List (Env, Statements (Node));
               Discard (Build_Br (Env.Bld, BB_Iter));

               Env.Pop_Scope;
               Env.Pop_Loop;

               Position_Builder_At_End (Env.Bld, BB_Next);
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
               Discard (Build_Br (Env.Bld, BB));
               Position_Builder_At_End (Env.Bld, BB);

               Env.Push_Scope;
               Stack_State := Build_Call
                 (Env.Bld,
                  Get_Stack_Save (Env),
                  System.Null_Address, 0,
                  "");

               Compile_List (Env, Declarations (Node));
               Compile_List
                 (Env, Statements (Handled_Statement_Sequence (Node)));

               Discard (Build_Call
                        (Env.Bld,
                           Get_Stack_Restore (Env),
                           Stack_State'Address, 1,
                           ""));
               Env.Pop_Scope;
            end;

         when N_Full_Type_Declaration | N_Subtype_Declaration
            | N_Incomplete_Type_Declaration | N_Private_Type_Declaration =>
            Env.Set (Defining_Identifier (Node),
                     Create_Type (Env, Defining_Identifier (Node)));

         when N_Freeze_Entity =>
            --  TODO ??? Implement N_Freeze_Entity. We just need a stub
            --  implementation for basic types atm
            null;

         when N_Pragma =>
            case Get_Pragma_Id (Node) is
               --  TODO??? While we aren't interested in most of the pragmas,
               --  there are some we should look at. But still, the "others"
               --  case is necessary.
               when others => null;
            end case;

         when N_Empty =>
            null;

         when others =>
            raise Program_Error
              with "Unhandled statement node kind : "
              & Node_Kind'Image (Nkind (Node));
      end case;
   end Compile;

   --------------------
   -- Compile_LValue --
   --------------------

   function Compile_LValue (Env : Environ; Node : Node_Id) return Value_T
   is
   begin
      case Nkind (Node) is
         when N_Identifier =>
            return Env.Get (Entity (Node));

         when N_Explicit_Dereference =>
            return Build_Load
              (Env.Bld, Compile_LValue (Env, Prefix (Node)), "");

         when N_Selected_Component =>
            declare
               Pfx_Ptr : constant Value_T :=
                 Compile_LValue (Env, Prefix (Node));
               Record_Component : constant Entity_Id :=
                 Parent (Entity (Selector_Name (Node)));
               Idx : constant unsigned :=
                 unsigned (Index_In_List (Record_Component)) - 1;
            begin
               return Build_Struct_GEP
                 (Env.Bld, Pfx_Ptr, Idx, "pfx_load");
            end;

         when N_Indexed_Component =>
            declare
               Array_Ptr : Value_T :=
                 Compile_LValue (Env, Prefix (Node));

               Idxs    :
               Value_Array (1 .. List_Length (Expressions (Node)) + 1)
                 :=
                   (1 => Const_Int (Create_Type (Env, Etype (Node)), 0),
                    others => <>);
               --  Operands for the GetElementPtr instruction: one for the
               --  pointer deference, and then one per array index.

               I       : Nat := 2;
               DSD     : Node_Id := First_Index (Etype (Prefix (Node)));
               LB      : Value_T;
               Constrained : constant Boolean :=
                 Is_Constrained (Etype (Prefix (Node)));

               Bounds_Idxs : Value_Array :=
                 (1 => Const_Bound (0), 2 => <>);

            begin

               for N of Iterate (Expressions (Node)) loop
                  Idxs (I) := Compile_Expression (Env, N);

                  if Nkind (DSD) /= N_Range then
                     raise Program_Error
                       with "Arrays indexed with" & Nkind (DSD)'Img
                        & " not supported";
                  end if;

                  --  Adjust the index according to the range lower bound
                  if Constrained then
                     LB := Compile_Expression (Env, Low_Bound (DSD));
                  else
                     Bounds_Idxs (2) :=
                       Const_Bound (Integer ((I - 2) * 2));
                     LB := Build_Struct_GEP (Env.Bld, Array_Ptr, 1,
                                             "gep-bounds-array");
                     LB := GEP
                       (Env, LB, Bounds_Idxs, "gep-low-bound");
                     LB := Build_Load (Env.Bld, LB, "load-low-bound");
                  end if;

                  Idxs (I) := Build_Sub (Env.Bld, Idxs (I), LB, "index");

                  I := I + 1;
                  DSD := Next (DSD);
               end loop;

               if not Constrained then
                  Array_Ptr := Build_Load
                    (Env.Bld,
                     Build_Struct_GEP (Env.Bld, Array_Ptr, 0, ""), "");
               end if;

               return
                 GEP (Env, Array_Ptr, Idxs, "array-access");
            end;

         when others =>
            raise Program_Error
              with "Unhandled node kind: " & Node_Kind'Image (Nkind (Node));
      end case;
   end Compile_LValue;

   ---------------
   -- Is_LValue --
   ---------------

   function Is_LValue (Node : Node_Id) return Boolean
   is
      N : Node_Id := Node;
   begin
      loop
         case Nkind (N) is
            when N_Explicit_Dereference =>
               return True;
            when N_Selected_Component | N_Indexed_Component =>
               N := Prefix (N);
            when N_Identifier =>
               N := Entity (N);
            when N_Defining_Identifier =>
               if Present (Renamed_Object (N)) then
                  N := Renamed_Object (N);
               else
                  return True;
               end if;
            when others =>
               return False;
         end case;
      end loop;
   end Is_LValue;

   ------------------------
   -- Compile_Expression --
   ------------------------

   function Compile_Expression
     (Env : Environ; Node : Node_Id) return Value_T is

      function Compile_Expr (Node : Node_Id) return Value_T is
        (Compile_Expression (Env, Node));

      type Scl_Op is (Op_Or, Op_And);

      function Build_Scl_Op (Op : Scl_Op) return Value_T;
      --  Emit the LLVM IR for a short circuit operator ("or else", "and then")

      function Build_Scl_Op (Op : Scl_Op) return Value_T is
      begin
         declare

            --  The left expression of a SCL op is always evaluated.

            Left : constant Value_T := Compile_Expr (Left_Opnd (Node));
            Result : constant Value_T :=
              Build_Alloca
                (Env.Bld,
                 Type_Of (Left),
                 "scl-res-1");

            --  Block which contains the evaluation of the right part
            --  expression of the operator.

            Block_Right_Expr : constant Basic_Block_T :=
              Append_Basic_Block (Env.Current_Subp.Func, "scl-right-expr");

            --  Block containing the exit code (load the final cond value into
            --  Result

            Block_Exit : constant Basic_Block_T :=
              Append_Basic_Block (Env.Current_Subp.Func, "scl-exit");

         begin
            Store (Env, Left, Result);

            --  In the case of And, evaluate the right expression when Left is
            --  true. In the case of Or, evaluate it when Left is false.

            if Op = Op_And then
               Discard
                 (Build_Cond_Br
                    (Env.Bld, Left, Block_Right_Expr, Block_Exit));
            else
               Discard
                 (Build_Cond_Br
                    (Env.Bld, Left, Block_Exit, Block_Right_Expr));
            end if;

            --  Emit code for the evaluation of the right part expression

            Position_Builder_At_End (Env.Bld, Block_Right_Expr);

            declare
               Right : constant Value_T := Compile_Expr (Right_Opnd (Node));
               Left : constant Value_T := Build_Load
                 (Env.Bld, Result, "load-left");
               Res : Value_T;
            begin
               if Op = Op_And then
                  Res := Build_And (Env.Bld, Left, Right, "scl-and");
               else
                  Res := Build_Or (Env.Bld, Left, Right, "scl-or");
               end if;
               Store (Env, Res, Result);
               Discard (Build_Br (Env.Bld, Block_Exit));
            end;

            Position_Builder_At_End (Env.Bld, Block_Exit);

            return Build_Load (Env.Bld, Result, "scl-final-res");
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

            function Compile_Cmp (N : Node_Id) return Value_T;
            --  Compilation of binary comparison operators

            function Compile_Cmp (N : Node_Id) return Value_T is
               PM : constant Pred_Mapping := Get_Preds (N);
               T : constant Entity_Id := Etype (Left_Opnd (Node));
            begin

               --  LLVM treats pointers as integers regarding comparison

               if Is_Integer_Type (T)
                 or else Is_Access_Type (T)
               then
                  return Build_I_Cmp
                    (Env.Bld,
                     (if Is_Unsigned_Type (T) then PM.Unsigned else PM.Signed),
                     LVal, RVal, "icmp");
               elsif Is_Floating_Point_Type (Etype (Left_Opnd (Node))) then
                  return Build_F_Cmp
                    (Env.Bld, PM.Real, LVal, RVal, "fcmp");
               else
                  raise Program_Error
                    with "EQ only for int and real types";
                  --  TODO : Equality for aggregate types
               end if;
            end Compile_Cmp;

         begin
            case Nkind (Node) is

            when N_Op_Add =>
               Op := Build_Add (Env.Bld, LVal, RVal, "add");

            when N_Op_Subtract =>
               Op := Build_Sub (Env.Bld, LVal, RVal, "sub");

            when N_Op_Multiply =>
               Op := Build_Mul (Env.Bld, LVal, RVal, "mul");

            when N_Op_Divide =>
               declare
                  T : constant Entity_Id := Etype (Left_Opnd (Node));
               begin
                  if Is_Signed_Integer_Type (T) then
                     Op := Build_S_Div (Env.Bld, LVal, RVal, "sdiv");
                  elsif Is_Floating_Point_Type (T) then
                     Op := Build_F_Div (Env.Bld, LVal, RVal, "fdiv");
                  elsif Is_Unsigned_Type (T) then
                     return Build_U_Div (Env.Bld, LVal, RVal, "udiv");
                  else
                     raise Program_Error
                       with "Not handled : Division with type " & T'Img;
                  end if;
               end;

            when N_Op_Gt | N_Op_Lt | N_Op_Le | N_Op_Ge | N_Op_Eq | N_Op_Ne =>
               return Compile_Cmp (Node);

            when others =>
               raise Program_Error
                 with "Unhandled node kind in expression: "
                 & Node_Kind'Image (Nkind (Node));

            end case;

            --  We need to handle modulo manually for non binary modulus types.

            if Non_Binary_Modulus (Etype (Node)) then
               Op := Build_U_Rem
                 (Env.Bld, Op,
                  Const_Int
                    (Create_Type (Env, Etype (Node)),
                     Modulus (Etype (Node))),
                 "mod");
            end if;

            return Op;
         end;

      else

         case Nkind (Node) is

         when N_Expression_With_Actions =>
            --  TODO??? Compile the list of actions
            pragma Assert (Is_Empty_List (Actions (Node)));
            return Compile_Expr (Expression (Node));

         when N_Character_Literal =>
            return Const_Int
              (Create_Type (Env, Etype (Node)),
               Char_Literal_Value (Node));

         when N_Integer_Literal =>
            return Const_Int
              (Create_Type (Env, Etype (Node)),
               Intval (Node));

         when N_And_Then => return Build_Scl_Op (Op_And);
         when N_Or_Else => return Build_Scl_Op (Op_Or);

         when N_Op_Plus => return Compile_Expr (Right_Opnd (Node));
         when N_Op_Minus => return Build_Sub
              (Env.Bld,
               Const_Int
                 (Create_Type (Env, Etype (Node)), 0, False),
               Compile_Expr (Right_Opnd (Node)),
               "minus");

         when N_Type_Conversion =>
            declare
               Src_T : constant Entity_Id := Etype (Node);
               Dest_T : constant Entity_Id := Etype (Expression (Node));
            begin

               --  For the moment, we handle only the simple case of Integer to
               --  Integer conversions.

               if Is_Integer_Type (Src_T) then
                  if Is_Integer_Type (Dest_T) then

                     if Src_T = Dest_T then
                        return Compile_Expr (Expression (Node));
                     end if;

                     return Build_S_Ext
                       (Env.Bld, Compile_Expr (Expression (Node)),
                        Create_Type (Env, Etype (Node)),
                        "int_conv");
                  else
                     raise Program_Error with "Unhandled type conv";
                  end if;
               else
                  raise Program_Error with "Unhandled type conv";
               end if;
            end;

         when N_Identifier =>
            --  N_Defining_Identifier nodes for enumeration literals are not
            --  store in the environment. Handle them here.

            if Ekind (Entity (Node)) = E_Enumeration_Literal then
               return Const_Int
                 (Create_Type (Env, Etype (Node)),
                  Enumeration_Rep (Entity (Node)), False);
            else
               return Build_Load (Env.Bld, Env.Get (Entity (Node)), "");
            end if;

         when N_Function_Call =>
            return Compile_Call (Env, Node);

         when N_Explicit_Dereference =>
            return Build_Load (Env.Bld, Compile_Expr (Prefix (Node)), "");

         when N_Allocator =>
            declare
               Arg : array (1 .. 1) of Value_T :=
                 (1 => Size_Of (Create_Type (Env, Etype (Expression (Node)))));
            begin
               if Nkind (Expression (Node)) = N_Identifier then
                  return Build_Bit_Cast
                    (Env.Bld,
                     Build_Call
                       (Env.Bld, Env.Default_Alloc_Fn, Arg'Address,
                        1, "alloc"),
                     Create_Type (Env, Etype (Node)),
                     "alloc_bc");
               else
                  raise Program_Error
                    with "Non handled form in N_Allocator";
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
                  return Compile_LValue (Env, Prefix (Node));
               end if;
            end;

            raise Program_Error
              with "Unhandled Attribute : "
              & Get_Name_String (Attribute_Name (Node));

            when N_Selected_Component =>
               declare
                  Pfx_Val : constant Value_T :=
                    Compile_Expression (Env, Prefix (Node));
                  Pfx_Ptr : constant Value_T :=
                    Build_Alloca (Env.Bld, Type_Of (Pfx_Val), "pfx_ptr");
                  Record_Component : constant Entity_Id :=
                    Parent (Entity (Selector_Name (Node)));
                  Idx : constant unsigned :=
                    unsigned (Index_In_List (Record_Component)) - 1;
               begin
                  Store (Env, Pfx_Val, Pfx_Ptr);
                  return Build_Load
                    (Env.Bld,
                     Build_Struct_GEP (Env.Bld, Pfx_Ptr, Idx, "pfx_load"), "");
               end;

            when N_Indexed_Component =>
               return Build_Load (Env.Bld, Compile_LValue (Env, Node), "");

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
                           Result := Build_Insert_Value
                             (Env.Bld,
                              Result,
                              Cur_Expr,
                              unsigned (Cur_Index - 1),
                              "");
                        end loop;
                     end loop;

                  --  Must be an array

                  else
                     Cur_Index := 0;
                     for Expr of Iterate (Expressions (Node)) loop
                        Cur_Expr := Compile_Expr (Expr);
                        Result := Build_Insert_Value
                          (Env.Bld,
                           Result, Cur_Expr, unsigned (Cur_Index), "");
                        Cur_Index := Cur_Index + 1;
                     end loop;

                  end if;

                  return Result;
               end;

            when N_Null =>
               return Const_Null (Create_Type (Env, Etype (Node)));

            when others =>
               raise Program_Error
                 with "Unhandled node kind: " & Node_Kind'Image (Nkind (Node));
         end case;
      end if;
   end Compile_Expression;

   --------------------------
   -- Compile_Declarations --
   --------------------------

   procedure Compile_List
     (Env : Environ; List : List_Id) is
   begin
      for N of Iterate (List) loop
         Compile (Env, N);
      end loop;
   end Compile_List;

   ------------------
   -- Compile_Call --
   ------------------

   function Compile_Call
     (Env : Environ; Call : Node_Id) return Value_T is
      Func_Ident  : constant Node_Id := Entity (Name (Call));
      Func_Params : constant List_Id :=
        Parameter_Specifications (Parent (Func_Ident));
      Param_Spec  : Node_Id;

      Params      : constant List_Id := Parameter_Associations (Call);
      LLVM_Func   : Value_T;
      Args        : array (1 .. List_Length (Params)) of Value_T;
      I           : Nat := 1;
      P_Type      : Entity_Id;
   begin
      --  Lazy compilation of function specs
      if not Env.Has_Value (Func_Ident) then
         declare
            BB : constant Basic_Block_T := Get_Insert_Block (Env.Bld);
         begin
            Compile (Env, Parent (Parent (Func_Ident)));
            Position_Builder_At_End (Env.Bld, BB);
         end;
      end if;

      LLVM_Func := Env.Get (Func_Ident);

      Param_Spec := First (Func_Params);

      for Param of Iterate (Params) loop
         Args (I) :=
           (if Param_Needs_Ptr (Param_Spec)
            then Compile_LValue (Env, Param)
            else Compile_Expression (Env, Param));

         --  At this point we need to handle the conversion from constrained
         --  arrays to unconstrained arrays

         P_Type := Etype (Parameter_Type (Param_Spec));
         if Is_Constrained (Etype (Param))
           and then not Is_Constrained (P_Type)
         then
            declare
               Access_To_Array_Type : constant Type_T :=
                 Create_Access_Type (Env, P_Type);
               Access_Struct, Bounds_Array, V : Value_T;
               J : Integer := 0;
            begin
               Access_Struct :=
                 Build_Alloca (Env.Bld, Access_To_Array_Type, "");

               --  Store the ptr into the access struct
               V := Build_Struct_GEP (Env.Bld, Access_Struct, 0, "");
               Store (Env, Args (I), V);

               --  Store the bounds into the access struct
               Bounds_Array :=
                 Build_Struct_GEP (Env.Bld, Access_Struct, 1, "");

               for Dim of
                 Iterate (List_Containing (First_Index (Etype (Param))))
               loop
                  V := GEP (Env, Bounds_Array,
                            (Const_Bound (0), Const_Bound (J)), "");
                  Store (Env, Compile_Expression (Env, Low_Bound (Dim)), V);

                  V := GEP (Env, Bounds_Array,
                            (Const_Bound (0), Const_Bound (J + 1)), "");
                  Store (Env, Compile_Expression (Env, High_Bound (Dim)), V);

                  J := J + 2;
               end loop;

               --  Replace the simple pointer by the access struct
               Args (I) := Build_Load (Env.Bld, Access_Struct, "");
            end;
         end if;

         I := I + 1;
         Param_Spec := Next (Param_Spec);
      end loop;

      return
        Build_Call
          (Env.Bld, LLVM_Func,
           Args'Address, Args'Length,
           --  Assigning a name to a void value is not possible with LLVM
           (if Nkind (Call) = N_Function_Call then "subpcall" else ""));
   end Compile_Call;

   ------------------------
   -- Compile_Subprogram --
   ------------------------

   function Compile_Subprogram_Decl
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
              Create_Subprogram_Type (Env, Subp_Spec);

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
   end Compile_Subprogram_Decl;

end GNATLLVM.Compile;
