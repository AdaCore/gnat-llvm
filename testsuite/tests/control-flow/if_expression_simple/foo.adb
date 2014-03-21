function Foo (A : Integer; Cond : Boolean) return Integer is

   function Then_Return return Integer is
   begin
      return A;
   end Then_Return;

   function Else_Return return Integer is
   begin
      return -A;
   end Else_Return;

begin
   return (if Cond then Then_Return else Else_Return);
end Foo;
