function Foo (Compute_Max : Boolean; X, Y : Float) return Float is
begin
   if Compute_Max then
      return Float'Max (X, Y);
   else
      return Float'Min (X, Y);
   end if;
end Foo;
