with Atree;    use Atree;
with Sinfo;    use Sinfo;
with Sem_Util; use Sem_Util;
with Einfo;    use Einfo;
with Nlists;   use Nlists;

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
         when others => null;
      end case;
   end Compile;

   --------------------------
   -- Compile_Declarations --
   --------------------------

   procedure Compile_Declarations
     (Env : Environ; Declarations : List_Id) is
      Node : Node_Id := First (Declarations);
   begin
      while Present (Node) loop
         Compile (Env, Node);
         Node := Next (Node);
      end loop;
   end Compile_Declarations;

end GNATLLVM.Compile;
