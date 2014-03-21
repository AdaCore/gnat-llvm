with Interfaces; use Interfaces;

function Foo (A : Unsigned_32; N : Natural) return Unsigned_32 is
begin
   return Rotate_Left (A, N);
end Foo;
