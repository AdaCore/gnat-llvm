function Foo return Integer is
   type Int_Array is array (Integer range <>) of Integer;
   A : Int_Array := (1 .. 10 => 0);
begin
   A := (1 => 0, 2 => 1, 3 => 2, others => 3);
   return A (2);
end Foo;
