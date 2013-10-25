function Bidim return Integer is
   type Int_Array is array (10 .. 16, 5 .. 16) of Integer;
   A : Int_Array;
begin
   A (12, 12) := 15;
   return A (12, 12);
end;
