with QuickSort; use QuickSort;

function Driver return Boolean
is
   A        : Int_Array          := (5, 2, 3, 6, 1, 4);
   Expected : constant Int_Array := (1, 2, 3, 4, 5, 6);
begin
   --  Sort A.
   Sort (A);

   --  Return if it is actually sorted
   for I in A'Range loop
      if A (I) /= Expected (I) then
         return False;
      end if;
   end loop;
   return True;
end Driver;
