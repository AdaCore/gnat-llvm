function PGCD (A, B : Natural) return Natural is
    X : Natural := A;
    Y : Natural := B;
begin
   loop
      declare
         Q : constant Natural := X / Y;
         R : constant Natural := X - Q * Y;
      begin
         if R = 0 then
            return Y;
         end if;
         X := Y;
         Y := R;
      end;
   end loop;
end PGCD;
