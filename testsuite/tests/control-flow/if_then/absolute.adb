function Absolute (I : Integer) return Integer is
begin
   if I < 0 then
      return 0 - I;
   end if;
   return I;
end Absolute;
