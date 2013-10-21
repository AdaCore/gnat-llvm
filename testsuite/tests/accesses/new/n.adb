function N return Natural is
   type Nat_Access is access all Natural;
   A : Nat_Access := new Natural;
begin
   A.all := 123;
   return A.all;
end;
