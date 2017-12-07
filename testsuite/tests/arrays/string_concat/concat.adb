with Text_IO; use Text_IO;

procedure Concat is
   procedure Cat (S1, S2 : String) is
   begin
      Put (S1);
      Put (S2);
      New_Line;
      Put_Line (S1 & S2);
   end Cat;
begin
   Cat ("Hello ", "World");
end Concat;
