function Bidim return Integer is
   type Int_Array is array (Boolean, 5 .. 16) of Integer;
   A : Int_Array;
begin
   A (True, 12) := 15;
   return A (True, 12);
end;
