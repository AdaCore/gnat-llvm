package body Stack is

   function New_Stack return Stack is
      S : Stack;
   begin
      S.First_Stack_El := null;
      return S;
   end New_Stack;

   procedure Init_Stack (S : out Stack) is
   begin
      S := New_Stack;
   end Init_Stack;

   procedure Append (S: in out Stack; A : Natural) is
      SE : Stack_Element := S.First_Stack_El;
   begin
      S.First_Stack_El := new Stack_Element_Record;
      S.First_Stack_El.El := A;
      S.First_Stack_El.Next := SE;
   end Append;

   function Pop (S : in out Stack) return Natural is
      SE : Stack_Element := S.First_Stack_El;
   begin
      if SE = null then
         return 0;
      end if;

      S.First_Stack_El := SE.Next;

      return SE.El;
   end;

end Stack;
