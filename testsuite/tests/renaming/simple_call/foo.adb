function Foo (I : Integer) return Integer
is

   function One return Integer is
   begin
      return 1;
   end One;

   J : Integer renames One;
begin
   return I + J;
end Foo;
