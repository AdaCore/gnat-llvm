function Sum (A : Natural) return Integer is 
   type Int_Array is array (1 .. A) of Integer;
   Arr : Int_Array;
   Result : Integer := 0;
begin
   for I in 1 .. A loop
      Arr (I) := I;
   end loop;

   for I in 1 .. A loop
      Result := Result + Arr (I);
   end loop;

   return Result;
end;
