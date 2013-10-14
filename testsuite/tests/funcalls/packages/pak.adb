with Pak2; use Pak2;

package body Pak is
   function Quadruple (I : Integer) return Integer is
   begin
      return Double (I) * 2;
   end Quadruple;
end Pak;
