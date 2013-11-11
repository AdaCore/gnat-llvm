function Foo return Integer is
   type Int_Array is array (1 .. 1024) of Integer;
   A : Int_Array := (others => 0);
begin
   A (2 .. 4) := (1, 2, 3);
   return A (3);
end Foo;
