function Foo (A : Integer) return Integer is

   function Return_Lazy (L : access function return Integer) return Integer is
   begin
      return L.all;
   end Return_Lazy;

   function Return_A return Integer is
   begin
      return A;
   end Return_A;

   function Compute return Integer is

      function Compute_Helper return Integer is
      begin
         return Return_Lazy (Return_A'Access);
      end Compute_Helper;

   begin
      return Compute_Helper;
   end Compute;

begin
   return Compute;
end Foo;
