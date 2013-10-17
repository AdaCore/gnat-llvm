function Absolute (I : Integer) return Integer is
   Result : Integer;
begin
   Result := I;

   if Result >= 0 then
      goto Return_Result;
   end if;

   Result := 0 - Result;

   <<Return_Result>>
   return Result;
end Absolute;
