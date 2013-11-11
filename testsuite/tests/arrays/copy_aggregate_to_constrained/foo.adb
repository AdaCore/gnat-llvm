function Foo return Integer is
   type Int_Array is array (1 .. 1024) of Integer;
   A : Int_Array;
begin
   A := (1 => 0, 2 => 1, 3 => 2, others => 3);
   return A (2);
end Foo;
