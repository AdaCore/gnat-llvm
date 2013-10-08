with Sinfo; use Sinfo;
with Namet; use Namet;
with Nlists;   use Nlists;
with Atree;    use Atree;

package body GNATLLVM.Utils is

   ------------------------
   -- Is_Binary_Operator --
   ------------------------

   function Is_Binary_Operator (Node : Node_Id) return Boolean is
     (case Nkind (Node) is
         when N_Op_Add | N_Op_Subtract | N_Op_Multiply | N_Op_Divide
           | N_Op_Eq | N_Op_Ne | N_Op_Ge | N_Op_Gt | N_Op_Le | N_Op_Lt => True,
         when others => False);

   -------------
   -- Discard --
   -------------

   procedure Discard (V : Value_T) is
      pragma Unreferenced (V);
   begin
      null;
   end Discard;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (E : Entity_Id) return String is
   begin
      return Get_Name_String (Chars (E));
   end Get_Name;

   -------------
   -- Iterate --
   -------------

   function Iterate (L : List_Id) return List_Iterator
   is
      Len : constant Nat := List_Length (L);
      A : List_Iterator (1 .. Len);
      N : Node_Id := First (L);
      I : Nat := 1;
   begin
      while Present (N) loop
         A (I) := N;
         I := I + 1;
         N := Next (N);
      end loop;
      return A;
   end Iterate;

end GNATLLVM.Utils;
