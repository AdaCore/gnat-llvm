function Fact (N : Natural) return Natural is
   Result : Natural := 1;
begin
   for I in -N .. -2 loop
      Result := Result * (-I);
   end loop;
   return Result;
end Fact;
