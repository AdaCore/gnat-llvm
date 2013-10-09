function Fibo (A : Integer) return Integer is
begin
   if A <= 1 then
      return A;
   else
      return Fibo (A - 1) + Fibo (A - 2);
   end if;
end Fibo;
