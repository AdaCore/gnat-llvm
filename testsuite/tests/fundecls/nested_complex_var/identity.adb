function Identity (N : Natural) return Natural is
   Ones : array (1 .. N) of Natural := (others => 1);
   I    : Natural;
begin
   I := Ones'First;
   declare
      Result : Natural := 0;

      procedure Add_Current;

      -----------------
      -- Add_Current --
      -----------------

      procedure Add_Current is
      begin
         Result := Result + Ones (I);
      end Add_Current;

   begin
      if Ones'Length > 0 then
         loop
            Add_Current;
            exit when I = Ones'Last;
            I := I + 1;
         end loop;
      end if;
      return Result;
   end;
end Identity;
