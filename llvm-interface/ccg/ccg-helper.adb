------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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

with stddef_h;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body CCG.Helper is

   ---------------------------
   -- Const_Real_Get_Double --
   ---------------------------

   function Const_Real_Get_Double
     (V : Value_T; Loses_Info : out Boolean) return Double
   is
      C_Loses_Info : aliased Bool_T;
      Result       : constant Double :=
        Const_Real_Get_Double (V, C_Loses_Info'Access);

   begin
      Loses_Info := C_Loses_Info /= 0;
      return Result;
   end Const_Real_Get_Double;

   ---------------------
   -- Get_Opcode_Name --
   ---------------------

   function Get_Opcode_Name (Opc : Opcode_T) return String is
      function Get_Opcode_Name_C (Opc : Opcode_T) return chars_ptr
        with Import, Convention => C, External_Name => "Get_Opcode_Name";
   begin
      return Value (Get_Opcode_Name_C (Opc));
   end Get_Opcode_Name;

   -------------------
   -- Get_As_String --
   -------------------

   function Get_As_String (V : Value_T) return String is
      Length : aliased stddef_h.size_t;
      S      : constant String := Get_As_String (V, Length'Access);
   begin
      return S;
   end Get_As_String;

   ----------------------------
   -- Get_Debug_Loc_Filename --
   ----------------------------

   function Get_Debug_Loc_Filename (V : Value_T) return String is
      Length : aliased unsigned;
      Str    : constant String := Get_Debug_Loc_Filename (V, Length'Access);

   begin
      return Str;
   end Get_Debug_Loc_Filename;

   -----------------------------
   -- Get_Debug_Loc_Directory --
   -----------------------------

   function Get_Debug_Loc_Directory (V : Value_T) return String is
      Length : aliased unsigned;
      Str    : constant String := Get_Debug_Loc_Directory (V, Length'Access);

   begin
      return Str;
   end Get_Debug_Loc_Directory;

   ------------------------
   -- Get_Debug_Loc_Line --
   ------------------------

   function Get_Debug_Loc_Line (V : Value_T) return Physical_Line_Number is
      Line : constant unsigned := Get_Debug_Loc_Line (V);
   begin
      return (if Line = 0 then 1 else Physical_Line_Number (Line));
   end Get_Debug_Loc_Line;

   -------------------
   -- Set_Successor --
   -------------------

   procedure Set_Successor (V : Value_T; J : Nat; BB : Basic_Block_T) is
   begin
      Set_Successor (V, unsigned (J), BB);
   end Set_Successor;

end CCG.Helper;
