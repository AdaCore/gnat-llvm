with Atree;    use Atree;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Sinfo;    use Sinfo;
with Sem_Util; use Sem_Util;
with Uintp;    use Uintp;

with GNATLLVM.Types; use GNATLLVM.Types;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with GNATLLVM.Utils; use GNATLLVM.Utils;

with LLVM.Analysis; use LLVM.Analysis;

package body GNATLLVM.Compile is

   procedure Compile_List
     (Env : Environ; List : List_Id);

   function Compile_Call
     (Env : Environ; Call : Node_Id) return Value_T;

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Env : Environ; Node : Node_Id) is
   begin
      case Nkind (Node) is
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
            declare
               Subp_Node : constant Node_Id := Specification (Node);
               Subp_Type : constant Type_T :=
                 Create_Subprogram_Type (Env, Subp_Node);
               Subp_Name : constant String :=
                 Get_Name_String (Chars (Defining_Unit_Name (Subp_Node)));
               LLVM_Func : constant Value_T :=
                 Add_Function (Env.Mdl, Subp_Name, Subp_Type);
            begin
               Env.Set
                 (Defining_Unit_Name (Specification (Node)), LLVM_Func);
            end;

         when N_Subprogram_Body =>
            declare
               Subp_Node : constant Node_Id := Specification (Node);
               Subp_Type : constant Type_T :=
                 Create_Subprogram_Type (Env, Subp_Node);
               Subp_Name : constant String :=
                 Get_Name_String (Chars (Defining_Unit_Name (Subp_Node)));
               Subp      : constant Subp_Env
                 := Env.Create_Subp (Subp_Name, Subp_Type);
               LLVM_Param : Value_T;
               LLVM_Var   : Value_T;
               Param : Entity_Id;
               I : Natural := 0;
            begin
               if Acts_As_Spec (Node) then
                  Env.Set
                    (Defining_Unit_Name (Specification (Node)), Subp.Func);
               else
                  Env.Set (Corresponding_Spec (Node), Subp.Func);
               end if;

               --  Register each parameter into a new scope
               Env.Push_Scope;

               for P of Iterate (Parameter_Specifications (Subp_Node)) loop
                  LLVM_Param := Get_Param (Subp.Func, unsigned (I));
                  Param := Defining_Identifier (P);
                  --  Define a name for the parameter P (which is the I'th
                  --  parameter), and associate the corresponding LLVM value to
                  --  its entity.

                  --  Set the name of the llvm value
                  Set_Value_Name (LLVM_Param, Get_Name (Param));

                  --  If this is an out parameter, the parameter is already an
                  --  l-value: keep it. Otherwise, allocate some memory for the
                  --  corresponding variable, and initialize it.
                  if Out_Present (P) then
                     LLVM_Var := LLVM_Param;
                  else
                     LLVM_Var := Build_Alloca
                       (Env.Bld,
                        Type_Of (LLVM_Param),
                        Get_Name (Param));
                     Discard (Build_Store (Env.Bld, LLVM_Param, LLVM_Var));
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

         when N_Object_Declaration =>
            declare
               Def_Ident : constant Node_Id := Defining_Identifier (Node);
               LLVM_Type : constant Type_T :=
                 Create_Type (Env, Etype (Def_Ident));
               LLVM_Var  : constant Value_T :=
                 Build_Alloca (Env.Bld, LLVM_Type, Get_Name (Def_Ident));
            begin
               Env.Set (Def_Ident, LLVM_Var);
               if Present (Expression (Node))
                 and then
                   not No_Initialization (Node)
               then
                  --  TODO??? Handle the Do_Range_Check_Flag
                  Discard (Build_Store
                           (Env.Bld,
                              Compile_Expression
                                (Env, Expression (Node)),
                              LLVM_Var));
               end if;
            end;

         when N_Implicit_Label_Declaration =>
            Env.Set
              (Defining_Identifier (Node),
               Create_Basic_Block
                 (Env, Get_Name (Defining_Identifier (Node))));

         when N_Assignment_Statement =>
            Discard (Build_Store
                     (Env.Bld,
                        Compile_Expression (Env, Expression (Node)),
                        Compile_LValue (Env, Name (Node))));

         when N_Procedure_Call_Statement =>
            Discard (Compile_Call (Env, Node));

         when N_Null_Statement =>
            null;

         when N_Exit_Statement =>
            declare
               Exit_Point : constant Basic_Block_T :=
                 (if Present (Name (Node)) then
                     Env.Get_Exit_Point (Entity (Name (Node)))
                  else
                     Env.Get_Exit_Point);
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
               Env.Set_Current_Basic_Block (Next_BB);
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
            Env.Set_Current_Basic_Block
              (Env.Create_Basic_Block ("unreachable"));

         when N_If_Statement =>
            declare
               BB_Then, BB_Else, BB_Next : Basic_Block_T;
               Cond                      : constant Value_T :=
                 Compile_Expression (Env, Condition (Node));
            begin
               BB_Next := Create_Basic_Block (Env, "if-next");
               BB_Then := Create_Basic_Block (Env, "if-then");
               BB_Else :=
                 (if not Is_Empty_List (Else_Statements (Node)) then
                     Create_Basic_Block (Env, "if-else")
                  else
                     BB_Next);
               Discard (Build_Cond_Br (Env.Bld, Cond, BB_Then, BB_Else));

               Env.Set_Current_Basic_Block (BB_Then);
               Compile_List (Env, Then_Statements (Node));
               Discard (Build_Br (Env.Bld, BB_Next));

               if not Is_Empty_List (Else_Statements (Node)) then
                  Env.Set_Current_Basic_Block (BB_Else);
                  Compile_List (Env, Else_Statements (Node));
                  Discard (Build_Br (Env.Bld, BB_Next));
               end if;

               Env.Set_Current_Basic_Block (BB_Next);
            end;

         when N_Loop_Statement =>
            declare
               Loop_Identifier           : Entity_Id;
               BB_Cond, BB_Body, BB_Next : Basic_Block_T;
               Iter_Scheme               : constant Node_Id :=
                 Iteration_Scheme (Node);
               Cond                      : Value_T;
            begin
               pragma Assert (Present (Identifier (Node)));
               Loop_Identifier := Entity (Identifier (Node));

               --  Create a basic block if none is already created for this
               --  identifier.
               BB_Cond := Env.Get (Loop_Identifier);
               Discard (Build_Br (Env.Bld, BB_Cond));

               --  If this is a mere loop, there is no need for a separate
               --  basic block.
               BB_Body :=
                 (if Present (Iter_Scheme) then
                     Create_Basic_Block (Env, "loop-body")
                  else
                     BB_Cond);

               --  Create a basic block to jump to when leaving the loop
               BB_Next := Create_Basic_Block (Env, "loop-exit");

               Env.Push_Loop (Loop_Identifier, BB_Next);

               if Present (Iter_Scheme) then
                  Env.Set_Current_Basic_Block (BB_Cond);
                  if Present (Condition (Iter_Scheme)) then
                     --  This is a WHILE loop: jump to the loop-body if the
                     --  condition evaluates to True, jump to the loop-exit
                     --  otherwise.
                     Cond := Compile_Expression (Env, Condition (Iter_Scheme));
                     Discard (Build_Cond_Br (Env.Bld, Cond, BB_Body, BB_Next));

                  else
                     --  This is a FOR loop: TODO???
                     raise Program_Error with "FOR loops are not handled";
                  end if;
               end if;

               Env.Set_Current_Basic_Block (BB_Body);
               Compile_List (Env, Statements (Node));
               Discard (Build_Br (Env.Bld, BB_Cond));

               Env.Pop_Loop;

               Env.Set_Current_Basic_Block (BB_Next);
            end;

         when N_Block_Statement =>
            declare
               BB : Basic_Block_T;
            begin
                  BB :=
                    (if Present (Identifier (Node)) then
                        Env.Get (Entity (Identifier (Node)))
                     else
                        Create_Basic_Block (Env, "block"));
                  Discard (Build_Br (Env.Bld, BB));
                  Set_Current_Basic_Block (Env, BB);

                  Env.Push_Scope;
                  Compile_List (Env, Declarations (Node));
                  Compile_List
                    (Env, Statements (Handled_Statement_Sequence (Node)));
                  Env.Pop_Scope;
            end;

         when N_Full_Type_Declaration =>
            Env.Set (Defining_Identifier (Node),
                     Create_Type (Env, Type_Definition (Node)));

         when N_Freeze_Entity =>
            --  TODO ??? Implement N_Freeze_Entity. We just need a stub
            --  implementation for basic types atm
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

   function Compile_LValue (Env : Environ; Node : Node_Id) return Value_T is
   begin
      case Nkind (Node) is
         when N_Identifier =>
            return Env.Get (Entity (Node));

         when N_Explicit_Dereference =>
            return Build_Load (Env.Bld, Env.Get (Entity (Prefix (Node))), "");

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

         when others =>
            raise Program_Error
              with "Unhandled node kind: " & Node_Kind'Image (Nkind (Node));
      end case;
   end Compile_LValue;

   ------------------------
   -- Compile_Expression --
   ------------------------

   function Compile_Expression
     (Env : Environ; Node : Node_Id) return Value_T is

      function Compile_Expr (Node : Node_Id) return Value_T is
        (Compile_Expression (Env, Node));

      type Scl_Op is (Op_Or, Op_And);

      function Build_Scl_Op (Op : Scl_Op) return Value_T;
      function Build_Scl_Op (Op : Scl_Op) return Value_T is
      begin
         declare
            Left : constant Value_T := Compile_Expr (Left_Opnd (Node));
            Result : constant Value_T :=
              Build_Alloca
                (Env.Bld,
                 Type_Of (Left),
                 "scl-res-1");
            Block_Left_True : constant Basic_Block_T :=
              Append_Basic_Block (Env.Current_Subp.Func, "scl-pass");
            Block_Left_Exit : constant Basic_Block_T :=
              Append_Basic_Block (Env.Current_Subp.Func, "scl-exit");
         begin
            Discard (Build_Store (Env.Bld, Left, Result));

            if Op = Op_And then
               Discard
                 (Build_Cond_Br
                    (Env.Bld, Left, Block_Left_True, Block_Left_Exit));
            else
               Discard
                 (Build_Cond_Br
                    (Env.Bld, Left, Block_Left_Exit, Block_Left_True));
            end if;

            Position_Builder_At_End (Env.Bld, Block_Left_True);
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
               Discard (Build_Store (Env.Bld, Res, Result));
               Discard (Build_Br (Env.Bld, Block_Left_Exit));
            end;
            Position_Builder_At_End (Env.Bld, Block_Left_Exit);
            Env.Current_Subp.Current_Block := Block_Left_Exit;
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

            function Compile_Cmp (N : Node_Id) return Value_T;
            function Compile_Cmp (N : Node_Id) return Value_T is
               PM : constant Pred_Mapping := Get_Preds (N);
               T : constant Entity_Id := Etype (Left_Opnd (Node));
            begin
               if Is_Integer_Type (T) then
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
               return Build_Add
                 (Env.Bld, LVal, RVal, "add");

            when N_Op_Subtract =>
               return Build_Sub
                 (Env.Bld, LVal, RVal, "sub");

            when N_Op_Multiply =>
               return Build_Mul
                 (Env.Bld, LVal, RVal, "mul");

            when N_Op_Divide =>
               declare
                  T : constant Entity_Id := Etype (Left_Opnd (Node));
               begin
                  if Is_Signed_Integer_Type (T) then
                     return Build_S_Div (Env.Bld, LVal, RVal, "sdiv");
                  elsif Is_Floating_Point_Type (T) then
                     return Build_F_Div (Env.Bld, LVal, RVal, "fdiv");
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
         end;
      else

         case Nkind (Node) is

         when N_Expression_With_Actions =>
            --  TODO??? Compile the list of actions
            pragma Assert (Is_Empty_List (Actions (Node)));
            return Compile_Expr (Expression (Node));

         when N_Integer_Literal =>
            return Const_Int
              (Create_Type (Env, Etype (Node)),
               unsigned_long_long (UI_To_Int (Intval (Node))),
               Sign_Extend => Boolean'Pos (True));

         when N_And_Then => return Build_Scl_Op (Op_And);
         when N_Or_Else => return Build_Scl_Op (Op_Or);

         when N_Type_Conversion =>
            --  TODO : Implement N_Type_Conversion
            return Compile_Expr (Expression (Node));

         when N_Identifier =>
            return Build_Load (Env.Bld, Env.Get (Entity (Node)), "");

         when N_Function_Call =>
            return Compile_Call (Env, Node);

         when N_Explicit_Dereference =>
            return Build_Load (Env.Bld, Compile_Expr (Prefix (Node)), "");

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
                    Build_Alloca
                      (Env.Bld,
                       Type_Of (Pfx_Val),
                       "pfx_ptr");
                  Record_Component : constant Entity_Id :=
                    Parent (Entity (Selector_Name (Node)));
                  Idx : constant unsigned :=
                    unsigned (Index_In_List (Record_Component)) - 1;
               begin
                  Discard (Build_Store (Env.Bld, Pfx_Val, Pfx_Ptr));
                  return Build_Load
                    (Env.Bld,
                     Build_Struct_GEP
                       (Env.Bld, Pfx_Ptr, Idx, "pfx_load"),
                     "");
               end;

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
   begin
      --  Lazy compilation of function specs
      if not Env.Has_Value (Entity (Name (Call))) then
         Compile (Env, Parent (Parent (Entity (Name (Call)))));
      end if;

      LLVM_Func := Env.Get (Entity (Name (Call)));

      Param_Spec := First (Func_Params);

      for Param of Iterate (Params) loop
         Args (I) :=
           (if Out_Present (Param_Spec) then
                 Compile_LValue (Env, Param)
            else
               Compile_Expression (Env, Param));
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

end GNATLLVM.Compile;
