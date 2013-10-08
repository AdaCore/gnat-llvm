function Absolute (I : Integer) return Integer is
   Result : Integer := I;
begin
   if Result < 0 then
      Result := 0 - Result;
   end if;
   return Result;
end Absolute;
