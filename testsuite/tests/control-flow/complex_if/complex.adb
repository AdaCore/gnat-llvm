function Complex (I : Integer) return Integer is
   R : Integer;
begin
   if I > 5 and then I < 20 then
      R := 77;
   elsif I < 30 or else I > 200 then
      R := 66;
   elsif I = 42 then
      R := 55;
   else
      R := 44;
   end if;
   
   return R;
end Complex;
