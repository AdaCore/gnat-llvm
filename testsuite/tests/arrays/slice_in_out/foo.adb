package body Foo is

   procedure Bar (A : in out Int_Array) is
   begin
      A (3) := 67;
   end Bar;

   function Bal (A : in out Int_Array) return Integer is
   begin
      A (3) := 42;
      Bar (A (2 .. 5));
      return A (3);
   end Bal;

   function Baz return Integer is
      A : Int_Array (1 .. 10) := (others => 2);
   begin
      return Bal (A);
   end Baz;

end Foo;
