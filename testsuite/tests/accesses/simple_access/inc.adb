package body Inc is

   procedure Inc (A : Int_Access) is
   begin
      A.all := A.all + 1;
   end Inc;

end Inc;
