function Dynamic (Size : Natural) return Integer is
   type Int_Array is array (1 .. Size) of Integer;
   type Int_Array_Access is access all Int_Array;
   A : Int_Array_Access := new Int_Array;
begin
   A (12) := 15;
   return A (12);
end;
