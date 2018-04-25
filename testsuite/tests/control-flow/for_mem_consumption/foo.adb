with System; use System;
with Ada.Unchecked_Conversion;

function Foo (N : Natural) return Boolean is
   First          : Boolean := True;
   Reference_Addr_Up, Reference_Addr_Down : Address;
   function Uc is new Ada.Unchecked_Conversion
     (Source => Address, Target => Long_Long_Integer);

begin
   for I in 1 .. N loop
      for J in 1 .. N loop
         declare
            A : array (1 .. I, 1 .. J) of Natural;
         begin
            A (I / 2 + 1, J / 2 + 1) := 0;
            if First then
               Reference_Addr_Up   := A'Address;
               Reference_Addr_Down := A(I,J)'Address;
               First := False;
            end if;
            if abs (UC (Reference_Addr_Down) - UC (A(I,J)' Address)) > 16
              and then abs (UC (Reference_Addr_Up) -  UC (A'Address)) > 16
            then
               --  A should lie at the same address for all iterations, but
               --  the stack may grow up or down and there may be an
               --  alignment difference.
               return False;
            end if;
         end;
      end loop;
   end loop;
   return True;
end Foo;
