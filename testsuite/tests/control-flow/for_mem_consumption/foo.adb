with System; use System;

function Foo (N : Natural) return Boolean is
   First          : Boolean := True;
   Reference_Addr : System.Address;

begin
   for I in 1 .. N loop
      for J in 1 .. N loop
         declare
            A : array (1 .. I, 1 .. J) of Natural :=
               (others => (others => 1));
         begin
            A (I / 2 + 1, J / 2 + 1) := 0;
            if First then
               Reference_Addr := A'Address;
               First := False;
            end if;
            if Reference_Addr /= A'Address then
               --  A should lie at the same address for all iterations
               return False;
            end if;
         end;
      end loop;
   end loop;
   return True;
end Foo;
