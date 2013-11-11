function Foo return Integer is
   type Int_UC_Array is array (Natural range <>) of Integer;
   subtype Int_C_Array is Int_UC_Array (1 .. 1024);

   A1 : Int_UC_Array := (1000 .. 3000 => 0);
   A2 : Int_C_Array := (others => 0);
begin
   for I in A1'Range loop
      A1 (I) := I - A1'First;
   end loop;
   A2 := A1 (1001 .. 2024);
   return A2 (3);
end Foo;
