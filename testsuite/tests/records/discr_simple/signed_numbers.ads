package Signed_Numbers is

   type Signed_Number (Positive : Boolean) is record
      N : Natural;
   end record;

   function Create (N : Natural; Positive : Boolean) return Signed_Number;
   function Add (Left, Right : Signed_Number) return Signed_Number;

end Signed_Numbers;
