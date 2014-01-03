package body Signed_Numbers is

   procedure Init (S : in out Signed_Number) is
   begin
      null;
   end Init;

   ------------
   -- Create --
   ------------

   function Create (N : Natural; Positive : Boolean) return Signed_Number is
      A : Signed_Number := (Positive, N);
   begin
      Init (A);
      return A;
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

   procedure Test is
      A : Signed_Number := Create (124, True);
      B : Signed_Number := Create (124, True);
      C : Signed_Number := Add (A, B);
   begin
      null;
   end Test;

end Signed_Numbers;
