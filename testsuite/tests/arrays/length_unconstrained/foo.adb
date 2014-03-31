function Foo (I : Integer) return Integer is
   type Array_Type is array (Integer range <>) of Integer;

   function Length (A : Array_Type) return Integer is
   begin
      return A'Length;
   end Length;

   A : constant Array_Type (-1 .. I - 2) := (others => 0);

begin
   return Length (A);
end;
