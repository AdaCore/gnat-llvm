package body GNATLLVM.Id_Generator is
   Next_Id : Natural := 0;

   function Id (Label : String := "") return String is
      Id : constant String := Next_Id'Img;
   begin
      --  Using "-" as a separator between the radix and the suffix is
      --  completely safe since such names will not clash with Ada
      --  identifiers nor with expansed tree temporaries.
      Next_Id := Next_Id + 1;
      return Label & "-" & Id (2 .. Id'Last);
   end Id;

end GNATLLVM.Id_Generator;
