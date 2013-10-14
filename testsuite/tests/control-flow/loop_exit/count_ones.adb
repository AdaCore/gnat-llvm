function Count_Ones (N : Natural) return Natural is
   T      : Natural := N;
   Result : Natural := 0;
begin
   loop
      declare
         Next_Digits : constant Natural := T / 10;
         Digit       : constant Natural := T - Next_Digits * T;
      begin
         if Digit = 1 then
            Result := Result + 1;
         end if;
         exit when Next_Digits = 0;
         T := Next_Digits;
      end;
   end loop;
   return Result;
end Count_Ones;
