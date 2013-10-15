procedure Silly (N : in out Natural) is
begin
   loop
      N := N + 1;
      exit;
      N := N - 1;
   end loop;
end Silly;
