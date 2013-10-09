function Boolean_Ops (A : Integer; B : Integer) return Boolean is
begin
   return ((A < B) and then ((A > 0) or else (B < 0)));
end Boolean_Ops;
