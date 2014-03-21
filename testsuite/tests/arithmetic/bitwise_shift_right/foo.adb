with Interfaces; use Interfaces;

function Foo (A : Unsigned_32; N : Natural) return Unsigned_32 is
begin
   return Shift_Right (A, N);
end Foo;
