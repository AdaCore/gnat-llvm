with Types; use Types;

package GNATLLVM.Utils is

   type List_Iterator is array (Nat range <>) of Node_Id;

   function Iterate (L : List_Id) return List_Iterator;
   --  Return an iterator on list L

   function Get_Name (E : Entity_Id) return String;
   --  Return the name of an entity: Get_Name_String (Chars (E))

end GNATLLVM.Utils;
