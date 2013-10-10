procedure Fact (N : Natural; Result : out Natural) is
begin
   if N <= 1 then
      Result := 1;
   else
      Fact (N - 1, Result);
      Result := N * Result;
   end if;
end Fact;
