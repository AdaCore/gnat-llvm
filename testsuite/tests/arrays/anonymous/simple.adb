function Simple return Integer is
   A : array (1 .. 1024) of Integer;
begin
   A (12) := 15;
   return A (12);
end;
