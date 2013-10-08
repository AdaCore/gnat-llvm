function Fact (N : Natural) return Natural is
   Result : Natural := 1;
begin
   for I in 2 .. N loop
      Result := Result * N;
   end loop;
   return Result;
end Fact;
