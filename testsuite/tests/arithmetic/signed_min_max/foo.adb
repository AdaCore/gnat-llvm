function Foo (Compute_Max : Boolean; X, Y : Integer) return Integer is
begin
   if Compute_Max then
      return Integer'Max (X, Y);
   else
      return Integer'Min (X, Y);
   end if;
end Foo;
