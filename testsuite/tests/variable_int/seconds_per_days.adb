function Seconds_Per_Days (Days : Natural) return Natural
is
   Hours   : constant Natural := Days * 24;
   Minutes : constant Natural := Hours * 60;
   Seconds : constant Natural := Minutes * 60;
begin
   return Seconds;
end Seconds_Per_Days;
