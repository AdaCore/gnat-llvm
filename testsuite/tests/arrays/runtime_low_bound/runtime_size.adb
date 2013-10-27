function Runtime_Size (Size : Natural) return Integer is
   type Int_Array is
      array (Natural'Last - Size .. Natural'Last - 1)
      of Integer;
   A : Int_Array;
begin
   A (Natural'Last - 1) := 15;
   return A (Natural'Last - 1);
end;
