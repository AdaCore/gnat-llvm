function Foo (Should_Be_Equal : Boolean) return Boolean
is
   type Rec_Type is record
      B : Boolean;
      --  We should have padding, here.

      I : Integer;
   end record;
   type Rec_Array is array (Natural range <>) of Rec_Type;

   function Is_Equal (A, B : Rec_Array) return Boolean is
   begin
      return A = B;
   end Is_Equal;

   A1 : Rec_Array (1 .. 6);
   A2 : Rec_Array (A1'First .. A1'Last);
begin
   for I in A1'Range loop
      A1 (I) := (B => True, I => 2);
      A2 (I) := (B => Should_Be_Equal, I => 2);
   end loop;

   return Is_Equal (A1, A2);
end Foo;
