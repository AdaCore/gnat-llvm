function Foo return Integer is
   type Int_UC_Array is array (Natural range <>) of Integer;

   A1 : Int_UC_Array := (1000 .. 3000 => 0);
   A2 : Int_UC_Array := A1;
begin
   for I in A1'Range loop
      A1 (I) := I - A1'First + 1;
   end loop;
   A2 := A1 (A1'First .. A2'Last);
   return A2 (A2'First + 2);
end Foo;
