function Seconds_Per_Days (Days : Natural) return Natural
is
   Hours   : constant Natural := Days * 24;
   Minutes : constant Natural := Hours * 60 + 1;
   Seconds : Natural := Minutes * 60;
begin
   Seconds := Seconds - 60;
   return Seconds;
end Seconds_Per_Days;
