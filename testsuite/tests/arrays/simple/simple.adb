function Simple return Integer is
   type Int_Array is array (1 .. 1024) of Integer;
   A : Int_Array;
begin
   A (12) := 15;
   return A (12);
end;
