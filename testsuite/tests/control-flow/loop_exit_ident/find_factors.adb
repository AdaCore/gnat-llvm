procedure Find_Factors
  (N : Natural;
   F1, F2 : out Natural) is
   --  Find F1 and F2 such as N = F1 * F2 and F2 <= F1
begin
   Left : for I in 1 .. 10 loop
      Right : for J in 1 .. 10 loop

         if I * J = N then
            F1 := I;
            F2 := J;
            exit Left;
         end if;

         exit Right when J >= I;

      end loop Right;
   end loop Left;
end Find_Factors;
