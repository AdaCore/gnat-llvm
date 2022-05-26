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

with Osint;   use Osint;
with Osint.C; use Osint.C;
with Output;  use Output;
with Table;

with GNATLLVM.Codegen; use GNATLLVM.Codegen;

package body CCG.Target is

   --  We have a table of parameters, which are (for now at least) either
   --  booleans or integers. We create a record type to describe each
   --  parameter.

   type    Access_Boolean is access all Boolean;
   type    Access_Integer is access all Integer;
   type    Param_Type     is (Bool, Num);
   subtype Str_Len        is Integer range 1 .. 20;

   type Parameter_Desc (PT : Param_Type := Num; SL : Str_Len := 20) is record
      Is_Version : Boolean;
      Name       : String (1 .. SL);
      case PT is
         when Bool =>
            Bool_Ptr : Access_Boolean;
         when Num =>
            Int_Ptr  : Access_Integer;
      end case;
   end record;

   package Parameters is new Table.Table
     (Table_Component_Type => Parameter_Desc,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Parameters");

   procedure Add_Param
     (Name       : String;
      PT         : Param_Type;
      Is_Version : Boolean        := False;
      Bool_Ptr   : Access_Boolean := null;
      Int_Ptr    : Access_Integer := null)
     with Pre => (if   PT = Bool then Bool_Ptr /= null and then Int_Ptr = null
                  else Bool_Ptr = null and then Int_Ptr /= null);
   --  Add a table entry for the specified parameter

   procedure Set_One_Parameter (PD : Parameter_Desc; Value : String);
   --  Set the parameter denoted by PD to Value

   ---------------
   -- Add_Param --
   ---------------

   procedure Add_Param
     (Name       : String;
      PT         : Param_Type;
      Is_Version : Boolean        := False;
      Bool_Ptr   : Access_Boolean := null;
      Int_Ptr    : Access_Integer := null)
   is
   begin
      case PT is
         when Bool =>
            Parameters.Append ((Bool, Name'Length, Is_Version, Name,
                                Bool_Ptr));
         when Num =>
            Parameters.Append ((Num,  Name'Length, Is_Version, Name,
                                Int_Ptr));
      end case;
   end Add_Param;

   ---------------------
   -- Set_One_Parameter --
   ---------------------

   procedure Set_One_Parameter (PD : Parameter_Desc; Value : String) is
   begin
      case PD.PT is
         when Bool =>
            if Value = "yes" or else Value = "y" or else Value = "true"
              or else Value = "t" or else Value = "on"
            then
               PD.Bool_Ptr.all := True;
            elsif Value = "no" or else Value = "n" or else Value = "false"
              or else Value = "f" or else Value = "off"
            then
               PD.Bool_Ptr.all := False;
            else
               Early_Error ("illegal value '" & Value & "' for parameter '" &
                            PD.Name & "'");
            end if;

         when Num =>

            --  Make a block here to catch an exception

            declare
               Value_First : Integer := Value'First;
               Int_Val     : Integer;

            begin

               --  For "version", we allow a leading "C", which we ignore

               if PD.Is_Version and then Value (Value_First) in 'c' | 'C' then
                  Value_First := Value_First + 1;
               end if;

               Int_Val := Integer'Value (Value (Value_First .. Value'Last));

               --  For "version", handle either full or partial year

               if PD.Is_Version then
                  if Int_Val < 80 then
                     Int_Val := Int_Val + 2000;
                  elsif Int_Val < 1900 then
                     Int_Val := Int_Val + 1900;
                  end if;
               end if;

               PD.Int_Ptr.all := Int_Val;

            exception
               when Constraint_Error =>
                  Early_Error ("illegal value '" & Value &
                               "' for parameter '" & PD.Name & "'");
            end;
      end case;
   end Set_One_Parameter;

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
         for J in 1 .. Parameters.Last loop
            if S (S'First .. Equal_Pos - 1) = Parameters.Table (J).Name then
               Set_One_Parameter (Parameters.Table (J),
                                  S (Equal_Pos + 1 .. S'Last));
               return;
            end if;
         end loop;

         Early_Error
           ("unknown C parameter '" & S (S'First .. Equal_Pos - 1) & "'");
      end if;

   end Set_C_Parameter;

   -------------------------
   -- Output_C_Parameters --
   -------------------------

   procedure Output_C_Parameters is
   begin
      Push_Output;
      if C_Parameter_File /= null then
         Namet.Unlock;
         Create_List_File (C_Parameter_File.all);
         Set_Output (Output_FD);
         Namet.Lock;
      end if;

      for J in 1 .. Parameters.Last loop
         declare
            PD : constant Parameter_Desc := Parameters.Table (J);

         begin
            Write_Str (PD.Name);
            Write_Str ("=");
            case PD.PT is
               when Bool =>
                  Write_Str ((if PD.Bool_Ptr.all then "True" else "False"));
               when Num =>
                  Write_Int (Int (PD.Int_Ptr.all));
            end case;

            Write_Eol;
         end;
      end loop;

      Pop_Output;
   end Output_C_Parameters;

begin
   Add_Param ("c-indent",      Num,  Int_Ptr  => C_Indent'Access);
   Add_Param ("warns-parens",  Bool, Bool_Ptr => Warns_Parens'Access);
   Add_Param ("always-brace",  Bool, Bool_Ptr => Always_Brace'Access);
   Add_Param ("max-depth",     Num,  Int_Ptr  => Max_Depth'Access);
   Add_Param ("have-includes", Bool, Bool_Ptr => Have_Includes'Access);
   Add_Param ("version",       Num,  Int_Ptr  => Version'Access,
                                     Is_Version => True);

end CCG.Target;
