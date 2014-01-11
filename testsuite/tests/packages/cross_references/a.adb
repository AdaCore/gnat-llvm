with B;

package body A is

   function Call_B (N : Natural) return Natural is
   begin
      return B.Called_By_A (N);
   end Call_B;

   function Called_By_B (I : Integer) return Integer is
   begin
      return I + 1;
   end Called_By_B;

end A;
