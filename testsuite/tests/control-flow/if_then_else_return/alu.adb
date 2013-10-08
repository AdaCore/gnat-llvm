function ALU (Add : Boolean; A, B : Natural) return Natural is
   Result : Natural;
begin
   if Add then
      Result := A + B;
   else
      Result := A * B;
   end if;
   return Result;
end ALU;
