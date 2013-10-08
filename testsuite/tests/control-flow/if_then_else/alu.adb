function ALU (Add : Boolean; A, B : Natural) return Natural is
begin
   if Add then
      return A + B;
   else
      return A * B;
   end if;
end ALU;
