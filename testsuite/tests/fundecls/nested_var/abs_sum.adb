function Abs_Sum (I, J : Integer) return Integer is

   Result : Integer := 0;

   procedure Add_Abs (I : Integer);

   -------------
   -- Add_Abs --
   -------------

   procedure Add_Abs (I : Integer) is
   begin
      if I < 0 then
         Result := Result - I;
      else
         Result := Result + I;
      end if;
   end Add_Abs;

begin
   Add_Abs (I);
   Add_Abs (J);
   return Result;
end Abs_Sum;
