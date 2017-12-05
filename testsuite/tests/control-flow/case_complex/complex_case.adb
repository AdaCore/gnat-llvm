function Complex_Case (I : Integer) return Integer is
   Result : Integer;

   subtype Index is Integer range 1 .. 10;

begin
   case I is
      when 0 =>
         Result := 1;
      when Index =>
         Result := 2;
      when 20 .. 29 =>
         Result := 3;
      when 30 | 32 | 34 =>
         Result := 4;
      when others =>
         return 5;
   end case;

   return Result;
end Complex_Case;
