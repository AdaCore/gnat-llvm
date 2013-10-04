
with Atree;    use Atree;
with Einfo;    use Einfo;
with Namet;    use Namet;
with Sinfo;    use Sinfo;
with Sem_Util; use Sem_Util;

with GNATLLVM.Types; use GNATLLVM.Types;
with Interfaces.C; use Interfaces.C;
with GNATLLVM.Utils; use GNATLLVM.Utils;
with System; use System;

package body GNATLLVM.Compile is

   procedure Compile_Declarations
     (Env : Environ; Declarations : List_Id);

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
                  Compile_Declarations (Env, Declarations (Node));
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

               Env.Pop_Scope;
            end;

         when others => null;
      end case;
   end Compile;

   -------------
   -- Compile --
   -------------

   function Compile_Expression
     (Env : Environ; Node : Node_Id) return Value_T is
      pragma Unreferenced (Env);
   begin
      case Nkind (Node) is
         when others => return Value_T (Null_Address);
      end case;
   end Compile_Expression;

   --------------------------
   -- Compile_Declarations --
   --------------------------

   procedure Compile_Declarations
     (Env : Environ; Declarations : List_Id) is
   begin
      for N of Iterate (Declarations) loop
         Compile (Env, N);
      end loop;
   end Compile_Declarations;

end GNATLLVM.Compile;
