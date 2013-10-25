function Bidim return Integer is
   type Int_Array is array (1 .. 16, 1 .. 16) of Integer;
   A : Int_Array;
begin
   A (12, 12) := 15;
   return A (12, 12);
end;
