------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with Atree;    use Atree;
with Namet;    use Namet;
with Sinput;   use Sinput;

with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;
with LLVM_Drive;       use LLVM_Drive;

package body GNATLLVM.DebugInfo is

   -------------------------
   -- Get_Debug_File_Node --
   -------------------------

   function Get_Debug_File_Node
     (Bld  : DI_Builder_T;
      File : Source_File_Index) return Metadata_T is
   begin
      if DI_Cache = null then
         DI_Cache :=
           new DI_File_Cache'(1 .. Last_Source_File => No_Metadata_T);
      end if;

      if DI_Cache (File) /= No_Metadata_T then
         return DI_Cache (File);
      end if;

      declare
         Full_Name : constant String :=
           Get_Name_String (Full_Debug_Name (File));
         Name      : constant String :=
           Get_Name_String (Debug_Source_Name (File));
         DIFile    : constant Metadata_T :=
           Create_Debug_File (Bld, Name,
                              Full_Name (1 .. Full_Name'Length - Name'Length));
      begin
         DI_Cache (File) := DIFile;
         return DIFile;
      end;
   end Get_Debug_File_Node;

   ---------------------------
   -- Set_Debug_Pos_At_Node --
   ---------------------------

   procedure Set_Debug_Pos_At_Node (Env : Environ; N : Node_Id) is
   begin
      if Emit_Debug_Info then
         Set_Debug_Loc (Env.Bld, Env.Func_Debug_Info,
                        Integer (Get_Logical_Line_Number (Sloc (N))),
                        Integer (Get_Column_Number (Sloc (N))));
      end if;
   end Set_Debug_Pos_At_Node;

end GNATLLVM.DebugInfo;
