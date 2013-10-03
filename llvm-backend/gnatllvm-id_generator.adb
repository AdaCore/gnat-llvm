package body GNATLLVM.Id_Generator is
   Next_Id : Integer := 0;

   function Generate_Id (Label : String := "") return String is
   begin
      Next_Id := Next_Id + 1;
      return Label & "_" & Next_Id'Img;
   end Generate_Id;

end GNATLLVM.Id_Generator;
