------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Table;

with GNATLLVM.Codegen; use GNATLLVM.Codegen;

package body CCG.Target is

   --  We have a table of parameters, which are (for now at least) either
   --  booleans or integers. We create a record type to describe each
   --  parameter.

   type    Access_Boolean is access all Boolean;
   type    Access_Integer is access all Integer;
   type    Param_Type     is (Bool, Int);
   subtype Str_Len        is Integer range 1 .. 20;

   type Parameter_Desc (PT : Param_Type := Int; SL : Str_Len := 20) is record
      Name : String (1 .. SL);
      case PT is
         when Bool =>
            Bool_Ptr : Access_Boolean;
         when Int =>
            Int_Ptr  : Access_Integer;
      end case;
   end record;

   package Parameter_Table is new Table.Table
     (Table_Component_Type => Parameter_Desc,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Parameter_Table");

   procedure Add_Param
     (Name     : String;
      PT       : Param_Type;
      Bool_Ptr : Access_Boolean := null;
      Int_Ptr  : Access_Integer := null)
     with Pre => (if   PT = Bool then Bool_Ptr /= null and then Int_Ptr = null
                  else Bool_Ptr = null and then Int_Ptr /= null);
   --  Add a table entry for the specified parameter

   procedure Set_C_Parameter (Name : String; Value : String);

   ---------------
   -- Add_Param --
   ---------------

   procedure Add_Param
     (Name     : String;
      PT       : Param_Type;
      Bool_Ptr : Access_Boolean := null;
      Int_Ptr  : Access_Integer := null)
   is
   begin
      case PT is
         when Bool =>
            Parameter_Table.Append ((Bool, Name'Length, Name, Bool_Ptr));
         when Int =>
            Parameter_Table.Append ((Int,  Name'Length, Name, Int_Ptr));
      end case;
   end Add_Param;

   ---------------------
   -- Set_C_Parameter --
   ---------------------

   procedure Set_C_Parameter (Name : String; Value : String) is
      Bool_Val    : Boolean;

   begin
      --  First handle boolean values

      if Name = "warns-parens" or else Name = "always-brace"
        or else Name = "have-includes"
      then
         if Value = "yes" or else Value = "y"
           or else Value = "true" or else Value = "t" or else Value = "on"
         then
            Bool_Val := True;
         elsif Value = "no" or else Value = "n"
           or else Value = "false" or else Value = "f" or else Value = "off"
         then
            Bool_Val := False;
         else
            Early_Error ("illegal value '" & Value & "' for parameter '"
                           & Name & "'");
            return;
         end if;

         if Name = "warns-parens" then
            Warns_Parens := Bool_Val;
         elsif Name = "always-brace" then
            Always_Brace := Bool_Val;
         else  -- Name = "have-includes"
            Have_Includes := Bool_Val;
         end if;

      --  Now handle integer values. Make a block here to catch an exception

      elsif Name = "indent" or else Name = "max-depth" or else Name = "version"
      then
         declare
            Value_First : Integer := Value'First;
            Int_Val     : Integer;

         begin
            --  For "version", we allow a leading "C", which we ignore

            if Value (Value_First) = 'C' or else Value (Value_First) = 'c' then
               Value_First := Value_First + 1;
            end if;

            Int_Val := Integer'Value (Value (Value_First .. Value'Last));
            if Name = "indent" then
               C_Indent := Int_Val;
            elsif Name = "max-depth" then
               Max_Depth := Int_Val;
            else --  Name = "version"
               if Int_Val < 80 then
                  Int_Val := Int_Val + 2000;
               elsif Int_Val < 1900 then
                  Int_Val := Int_Val + 1900;
               end if;

               Version := Int_Val;
            end if;
         end;

      else
         Early_Error ("unknown C parameter '" & Name & "'");
      end if;

   exception
      when Constraint_Error =>
         Early_Error ("illegal value '" & Value & "' for parameter '"
                        & Name & "'");
   end Set_C_Parameter;

   ----------------------
   --  Set_C_Parameter --
   ----------------------

   procedure Set_C_Parameter (S : String) is
      Equal_Pos : Integer := S'First - 1;

   begin
      for J in S'Range loop
         if S (J) = '=' then
            Equal_Pos := J;
         end if;
      end loop;

      if Equal_Pos = S'First - 1 then
         Early_Error ("Missing equal sign in parameter specification");
      elsif Equal_Pos = S'Last then
         Early_Error ("Missing value in parameter specification");
      else
         Set_C_Parameter (S (S'First .. Equal_Pos - 1),
                          S (Equal_Pos + 1 .. S'Last));
      end if;
   end Set_C_Parameter;

begin
   Add_Param ("C_Indent",      Int,  Int_Ptr  => C_Indent'Access);
   Add_Param ("Warns_Parens",  Bool, Bool_Ptr => Warns_Parens'Access);
   Add_Param ("Always_Brace",  Bool, Bool_Ptr => Always_Brace'Access);
   Add_Param ("Max_Depth",     Int,  Int_Ptr  => Max_Depth'Access);
   Add_Param ("Have_Includes", Bool, Bool_Ptr => Have_Includes'Access);
   Add_Param ("Version",       Int,  Int_Ptr  => Version'Access);

end CCG.Target;
