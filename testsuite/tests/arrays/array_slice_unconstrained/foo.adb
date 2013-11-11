package body Foo is

   function Bar (A : Int_Array) return Integer is
   begin
      return A (3);
   end Bar;

   function Baz return Integer is
      A : Int_Array (1 .. 10) := (others => 2);
   begin
      A (3) := 42;
      return Bar (A (2 .. 5));
   end Baz;

end Foo;
