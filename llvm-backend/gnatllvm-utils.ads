with Types; use Types;
with LLVM.Core; use LLVM.Core;

package GNATLLVM.Utils is

   type List_Iterator is array (Nat range <>) of Node_Id;

   function Iterate (L : List_Id) return List_Iterator;
   --  Return an iterator on list L

   function Get_Name (E : Entity_Id) return String;
   --  Return the name of an entity: Get_Name_String (Chars (E))

   procedure Discard (V : Value_T);

   function Is_Binary_Operator (Node : Node_Id) return Boolean;

end GNATLLVM.Utils;
