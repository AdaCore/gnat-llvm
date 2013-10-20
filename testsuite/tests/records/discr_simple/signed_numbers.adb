package body Signed_Numbers is

   ------------
   -- Create --
   ------------

   function Create (N : Natural; Positive : Boolean) return Signed_Number is
   begin
      return (Positive, N);
   end Create;

   ---------
   -- Add --
   ---------

   function Add (Left, Right : Signed_Number) return Signed_Number is
      N        : Natural;
      Positive : Boolean;
   begin
      if Left.Positive = Right.Positive then
         N := Left.N + Right.N;
         Positive := Left.Positive;

      elsif Left.N >= Right.N then
         N := Left.N - Right.N;
         Positive := Left.Positive;

      else
         N := Right.N - Left.N;
         Positive := Right.Positive;

      end if;

      return (Positive, N);
   end Add;

end Signed_Numbers;
