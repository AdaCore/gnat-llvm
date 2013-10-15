package Linked_Lists is

   type List_Record;
   type List is access List_Record;

   type List_Record is record
      Value : Integer := 0;
      Next  : List    := null;
   end record;

   function Length (L : List) return Natural;

end Linked_Lists;
