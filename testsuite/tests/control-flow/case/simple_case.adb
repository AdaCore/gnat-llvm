function Simple_Case (I : Integer) return Integer is
   Result : Integer;
begin
   case I is
      when 0 =>
         Result := 1;
      when 1 =>
         Result := 2;
      when 2 =>
         Result := 3;
      when 4 =>
         Result := 5;
      when others =>
         Result := 0;
   end case;

   return Result;
end Simple_Case;
