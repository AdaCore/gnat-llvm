function Runtime_Size (Size : Natural) return Integer is
   type Int_Array is array (1 .. Size) of Integer;
   A : Int_Array;
begin
   A (12) := 15;
   return A (12);
end;
