procedure Absolute (I : in out Integer) is
begin
   if I < 0 then
      I := 0 - I;
   end if;
end Absolute;
