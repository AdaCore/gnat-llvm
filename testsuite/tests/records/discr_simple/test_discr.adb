with Signed_Numbers; use Signed_Numbers;
with Text_IO; use Text_IO;

procedure Test_Discr is
   A : Signed_Number := Create (124, True);
   B : Signed_Number := Create (124, True);
   C : Signed_Number := Add (A, B);
   D : Signed_Number := Add (Create (1, True), Create (3, False));
begin
   if C /= (True, 248) then
      Put_Line ("error 1");
   end if;

   if D /= (False, 2) then
      Put_Line ("error 2");
   end if;
end Test_Discr;
