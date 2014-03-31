function Foo (Should_Be_Equal : Boolean) return Boolean
is
   type Int_Array is array (Natural range <>) of Integer;

   function Is_Equal (A, B : Int_Array) return Boolean is
   begin
      return A = B;
   end Is_Equal;

   A1 : Int_Array := (1, 2, 3, 4, 5);
   A2 : Int_Array (A1'First .. A1'Last);
begin
   for I in 0 .. A1'Length - 1 loop
      A2 (I) := 6 - A1 (A1'Last - I);
   end loop;

   --  Here, A1 and A2 should be equal

   if not Should_Be_Equal then
      A2 (A2'Last) := 0;
   end if;

   return Is_Equal (A1, A2);
end Foo;
