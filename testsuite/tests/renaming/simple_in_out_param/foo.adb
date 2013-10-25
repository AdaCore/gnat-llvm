procedure Foo (I : in out Integer)
is
   J : Integer renames I;
begin
   J := J + 1;
end Foo;
