function Foo (I : Integer) return Integer
is
   J : Integer := I;
   K : Integer renames J;
begin
   K := K + 1;
   return J;
end Foo;
