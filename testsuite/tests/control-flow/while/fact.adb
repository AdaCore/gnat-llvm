function Fact (N : Natural) return Natural is
   I      : Natural := N;
   Result : Natural := 1;
begin
   while I > 1 loop
      Result := Result * I;
      I := I - 1;
   end loop;
   return Result;
end Fact;
