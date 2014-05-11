with Interfaces; use Interfaces;

function Foo (A : Integer_64) return Integer_32 is
begin
   return Integer_32 (A);
end Foo;
