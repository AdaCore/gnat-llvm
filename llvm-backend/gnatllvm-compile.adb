with Atree;    use Atree;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Namet;    use Namet;
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
                  LLVM_Var := Build_Alloca (Env.Bld, Type_Of (LLVM_Param), "");
                  Discard (Build_Store (Env.Bld, LLVM_Param, LLVM_Var));

                  --  Add the parameter to the environnment
                  Env.Set (Param, LLVM_Var);

                  I := I + 1;
               end loop;

               Compile_List (Env, Declarations (Node));
               Compile_List
                 (Env, Statements (Handled_Statement_Sequence (Node)));

               Env.Pop_Scope;

               if Verify_Function (Subp.Func, Print_Message_Action) /= 0 then
                  --  TODO??? Display the crash message, or something like this
                  Error_Msg_N
                    ("The backend generated bad LLVM for this subprogram.",
                     Node);
               end if;
            end;

         when N_Null_Statement =>
            null;

         when N_Simple_Return_Statement =>
            if Present (Expression (Node)) then
               Discard (Build_Ret
                        (Env.Bld,
                           Compile_Expression (Env, Expression (Node))));
            else
               Discard (Build_Ret_Void (Env.Bld));
            end if;

         when others =>
            raise Program_Error
              with "Unhandled statement node kind : "
              & Node_Kind'Image (Nkind (Node));
      end case;
   end Compile;

   -------------
   -- Compile --
   -------------

   function Compile_Expression
     (Env : Environ; Node : Node_Id) return Value_T is
   begin
      if Is_Binary_Operator (Node) then
         declare
            LVal : constant Value_T :=
              Compile_Expression (Env, Left_Opnd (Node));
            RVal : constant Value_T :=
              Compile_Expression (Env, Right_Opnd (Node));
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
            when N_Op_Eq =>
               if Is_Integer_Type (Etype (Left_Opnd (Node))) then
                  return Build_I_Cmp
                    (Env.Bld, Int_EQ, LVal, RVal, Id ("eq"));
               elsif Is_Floating_Point_Type (Etype (Left_Opnd (Node))) then
                  return Build_F_Cmp
                    (Env.Bld, Real_OEQ, LVal, RVal, Id ("eq"));
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
            return Compile_Expression (Env, Expression (Node));
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
