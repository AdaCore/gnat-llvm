package body GNATLLVM.Id_Generator is
   Next_Id : Natural := 0;

   function Id (Label : String := "") return String is
      Id : constant String := Next_Id'Img;
   begin
      Next_Id := Next_Id + 1;
      return Label & "_" & Id (2 .. Id'Last);
   end Id;

end GNATLLVM.Id_Generator;
