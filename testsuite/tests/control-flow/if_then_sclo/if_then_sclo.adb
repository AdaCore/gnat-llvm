function If_Then_SCLO (I : Integer) return Integer is
begin
   if (I > 0 and then I < 40) then
      return I * 2;
   end if;
   return I;
end If_Then_SCLO;
