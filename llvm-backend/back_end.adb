with LLVM_Drive;
with Adabkend;
with Elists;
with Stringt;
with Namet;

package body Back_End is

   package GNAT2LLVM is new Adabkend
     (Product_Name       => "GNAT for LLVM",
      Copyright_Years    => "2013-2013",
      Driver             => LLVM_Drive.GNAT_To_LLVM,
      Is_Back_End_Switch => LLVM_Drive.Is_Back_End_Switch);

   procedure Scan_Compiler_Arguments renames GNAT2LLVM.Scan_Compiler_Arguments;

   -------------------
   -- Call_Back_End --
   -------------------

   procedure Call_Back_End (Mode : Back_End_Mode_Type) is
      pragma Unreferenced (Mode); -- Mode not referenced

   begin
      --  Since the back end is called with all tables locked,
      --  first unlock any tables that we need to change.

      Stringt.Unlock;
      Namet.Unlock;
      Elists.Unlock;

      GNAT2LLVM.Call_Back_End;

      --  Make sure to lock any unlocked tables again before returning

      Elists.Lock;
      Namet.Lock;
      Stringt.Lock;
   end Call_Back_End;

   -------------------------------
   -- Gen_Or_Update_Object_File --
   -------------------------------

   procedure Gen_Or_Update_Object_File is
   begin
      null;
   end Gen_Or_Update_Object_File;

end Back_End;
