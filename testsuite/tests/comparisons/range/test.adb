package body Test is

   function Ret (I : Integer) return Integer is
   begin
      Counter := Counter + 1;
      return I;
   end Ret;

   function Test (I : Integer) return Integer is
      Result : Integer := 0;
   begin
      Counter := 0;
 
      if Ret (I) in 10 .. 20 then
         Result := 10;
      end if;
      
      return Counter + Result;
   end Test;
end Test;
