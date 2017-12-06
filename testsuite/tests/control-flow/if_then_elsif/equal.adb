function Equal (X, Y : Integer) return Integer is
     Rec_Limit : constant Integer range 1..100 := 3;  -- Recursion limit.
     Z : Integer;                  -- Result.
begin
     if X > Rec_Limit then
          Z := Equal (Rec_Limit, Y-X+Rec_Limit);
     elsif X > 0 then
          Z := Equal (X-1, Y-1);
     elsif Y = 0 then
        Z := 1;
     else
        Z := 0;
     end if;

     return Z;
end Equal;
