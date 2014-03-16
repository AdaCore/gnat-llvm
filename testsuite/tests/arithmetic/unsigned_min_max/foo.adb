with Interfaces; use Interfaces;

function Foo (Compute_Max : Boolean; X, Y : Unsigned_32) return Unsigned_32 is
begin
   if Compute_Max then
      return Unsigned_32'Max (X, Y);
   else
      return Unsigned_32'Min (X, Y);
   end if;
end Foo;
