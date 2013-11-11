function Foo return Boolean
is
   type Int_Array is array (Natural range <>) of Integer;

   A1 : Int_Array := (1, 2, 3, 4, 5);
   A2 : Int_Array (A1'First .. A1'Last);
begin
   for I in A1'Range loop
      A2 (I) := 6 - A1 (A1'First + A1'Last - I);
   end loop;

   return A1 = A2;
end Foo;
