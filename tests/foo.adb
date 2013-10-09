function Foo (A : Integer; B : Integer; C : Integer) return Integer is
   D : Integer;
begin
   D := A / B * C + 42;
   return D;
end;
