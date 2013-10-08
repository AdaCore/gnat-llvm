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
with GNATLLVM.Id_Generator; use GNATLLVM.Id_Generator;

package body GNATLLVM.Compile is

   procedure Compile_List
     (Env : Environ; List : List_Id);

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

                  --  Allocate some memory for the corresponding variable, and
                  --  initialize it.
                  LLVM_Var := Build_Alloca
                    (Env.Bld,
                     Type_Of (LLVM_Param),
                     Id (Get_Name (Param)));
                  Discard (Build_Store (Env.Bld, LLVM_Param, LLVM_Var));

                  --  Add the parameter to the environnment
                  Env.Set (Param, LLVM_Var);

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
                  --  TODO??? Display the crash message, or something like this
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

         when N_Null_Statement =>
            null;

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
               BB_Next := Create_Basic_Block (Env, Id ("if-next"));
               BB_Then := Create_Basic_Block (Env, Id ("if-then"));
               BB_Else :=
                 (if not Is_Empty_List (Else_Statements (Node)) then
                     Create_Basic_Block (Env, Id ("if-else"))
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
               BB_Cond, BB_Body, BB_Next : Basic_Block_T;
               Iter_Scheme               : constant Node_Id :=
                 Iteration_Scheme (Node);
               Cond                      : Value_T;
            begin
               --  Create a basic block if none is already created for this
               --  identifier.
               BB_Cond :=
                 (if Present (Identifier (Node)) then
                     Env.Get (Entity (Identifier (Node)))
                  else
                     Create_Basic_Block (Env, Id ("loop")));
               Discard (Build_Br (Env.Bld, BB_Cond));

               --  If this is a mere loop, there is no need for a separate
               --  basic block.
               BB_Body :=
                 (if Present (Iter_Scheme) then
                     Create_Basic_Block (Env, Id ("loop-body"))
                  else
                     BB_Cond);

               --  Create a basic block to jump to when leaving the loop
               BB_Next := Create_Basic_Block (Env, Id ("loop-exit"));

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

               Env.Set_Current_Basic_Block (BB_Next);
            end;

         when others =>
            raise Program_Error
              with "Unhandled statement node kind : "
              & Node_Kind'Image (Nkind (Node));
      end case;
   end Compile;

   --------------------
   -- Compile_LValue --
   --------------------

   --------------------
   -- Compile_LValue --
   --------------------

   function Compile_LValue (Env : Environ; Node : Node_Id) return Value_T is
   begin
      case Nkind (Node) is
         when N_Identifier =>
            return Env.Get (Entity (Node));
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
   begin
      if Is_Binary_Operator (Node) then
         declare
            LVal : constant Value_T :=
              Compile_Expr (Left_Opnd (Node));
            RVal : constant Value_T :=
              Compile_Expr (Right_Opnd (Node));
         begin
            case Nkind (Node) is
            when N_Op_Add =>
               return Build_Add
                 (Env.Bld, LVal, RVal, Id ("add"));
            when N_Op_Subtract =>
               return Build_Sub
                 (Env.Bld, LVal, RVal, Id ("sub"));
            when N_Op_Multiply =>
               return Build_Mul
                 (Env.Bld, LVal, RVal, Id ("mul"));
            when N_Op_Divide =>
               declare
                  T : constant Entity_Id := Etype (Left_Opnd (Node));
               begin
                  if Is_Signed_Integer_Type (T) then
                     return Build_S_Div (Env.Bld, LVal, RVal, Id ("sdiv"));
                  elsif Is_Floating_Point_Type (T) then
                     return Build_F_Div (Env.Bld, LVal, RVal, Id ("fdiv"));
                  elsif Is_Unsigned_Type (T) then
                     return Build_U_Div (Env.Bld, LVal, RVal, Id ("sdiv"));
                  else
                     raise Program_Error
                       with "Not handled : Division with type " & T'Img;
                  end if;
               end;
            when N_Op_Eq | N_Op_Ne | N_Op_Lt | N_Op_Le | N_Op_Gt | N_Op_Ge =>
               if Is_Integer_Type (Etype (Left_Opnd (Node))) then
                  declare
                     Unsigned_Opnds : constant Boolean :=
                       Is_Unsigned_Type (Etype (Left_Opnd (Node)));
                     Pred : constant Int_Predicate_T :=
                       (case Nkind (Node) is
                           when N_Op_Eq => Int_EQ,
                           when N_Op_Ne => Int_NE,
                           when N_Op_Gt =>
                          (if Unsigned_Opnds then Int_UGT else Int_SGT),
                           when N_Op_Ge =>
                          (if Unsigned_Opnds then Int_UGE else Int_SGE),
                           when N_Op_Lt =>
                          (if Unsigned_Opnds then Int_ULT else Int_SLT),
                           when N_Op_Le =>
                          (if Unsigned_Opnds then Int_ULE else Int_SLE),
                           when others  => raise Program_Error);
                  begin
                     return Build_I_Cmp
                       (Env.Bld, Pred, LVal, RVal, Id ("icmp"));
                  end;
               elsif Is_Floating_Point_Type (Etype (Left_Opnd (Node))) then
                  declare
                     Pred : constant Real_Predicate_T :=
                       (case Nkind (Node) is
                           when N_Op_Eq => Real_OEQ,
                           when N_Op_Ne => Real_ONE,
                           when N_Op_Gt => Real_OGT,
                           when N_Op_Ge => Real_OGE,
                           when N_Op_Lt => Real_OLT,
                           when N_Op_Le => Real_OLE,
                           when others  => raise Program_Error);
                  begin
                     return Build_F_Cmp
                       (Env.Bld, Pred, LVal, RVal, Id ("fcmp"));
                  end;
               else
                  raise Program_Error
                    with "EQ only for int and real types";
                  --  TODO : Equality for aggregate types
               end if;
            when others =>
               raise Program_Error
                 with "Unhandled node kind: " & Node_Kind'Image (Nkind (Node));
            end case;
         end;
      else
         case Nkind (Node) is
         when N_Integer_Literal =>
            return Const_Int
              (Create_Type (Env, Etype (Node)),
               unsigned_long_long (UI_To_Int (Intval (Node))),
               Sign_Extend => Boolean'Pos (True));
         when N_Type_Conversion =>
            --  TODO : Implement N_Type_Conversion
            return Compile_Expr (Expression (Node));
         when N_Identifier =>
            return Build_Load (Env.Bld, Env.Get (Entity (Node)), "");
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

end GNATLLVM.Compile;
