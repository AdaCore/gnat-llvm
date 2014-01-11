with A;

package body B is

   function Call_A (I : Integer) return Integer is
   begin
      return A.Called_By_B (I);
   end Call_A;

   function Called_By_A (N : Natural) return Natural is
   begin
      return N * 2;
   end Called_By_A;

end B;
