package Stack is
   type Stack is private;
   function New_Stack return Stack;
   procedure Append (S : in out Stack; A : Natural);
   function Pop (S : in out Stack) return Natural;
private
   type Stack_Element_Record;
   type Stack_Element is access Stack_Element_Record;

   type Stack is record
      First_Stack_El : Stack_Element;
   end record;

   type Stack_Element_Record is record
      El : Natural;
      Next : Stack_Element;
   end record;
end Stack;
