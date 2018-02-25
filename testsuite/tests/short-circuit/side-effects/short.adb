package body Short is

   function Ret0 return Boolean is
   begin
      Counter := Counter + 1;
      return False;
   end Ret0;

   function Test return Integer is
   begin
      Counter := 0;
      if Ret0 and then Ret0 then
         null;
      end if;

      return Counter;
   end Test;
end Short;
