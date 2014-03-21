function Foo (A : Integer; Cond1, Cond2 : Boolean) return Integer is

   function Then_Return return Integer is
   begin
      return A;
   end Then_Return;

   function Elsif_Return return Integer is
   begin
      return 2 * A;
   end Elsif_Return;

   function Else_Return return Integer is
   begin
      return 3 * A;
   end Else_Return;

begin
   return
     (if Cond1    then Then_Return
      elsif Cond2 then Elsif_Return
      else             Else_Return);
end Foo;
