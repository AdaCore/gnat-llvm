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

                  --  Add the parameter to the environnment
                  Env.Set (Param, LLVM_Param);

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

         when N_Simple_Return_Statement =>
            if Present (Expression (Node)) then
               Discard (Build_Ret
                        (Env.Bld,
                           Compile_Expression (Env, Expression (Node))));
            else
               Discard (Build_Ret_Void (Env.Bld));
            end if;

         when others => null;
      end case;
   end Compile;

   -------------
   -- Compile --
   -------------

   function Compile_Expression
     (Env : Environ; Node : Node_Id) return Value_T is
   begin
      case Nkind (Node) is
         when N_Integer_Literal =>
            return Const_Int
              (Create_Type (Env, Etype (Node)),
               unsigned_long_long (UI_To_Int (Intval (Node))),
               Sign_Extend => Boolean'Pos (True));
         when others =>
            raise Program_Error
              with "Unhandled node kind: " & Node_Kind'Image (Nkind (Node));
      end case;
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
