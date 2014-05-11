with Interfaces; use Interfaces;

function Foo (A : Integer_32) return Integer_64 is
begin
   return Integer_64 (A);
end Foo;
