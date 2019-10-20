------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

with GNATLLVM.Codegen;     use GNATLLVM.Codegen;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.GLType;      use GNATLLVM.GLType;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Utils;       use GNATLLVM.Utils;
with GNATLLVM.Wrapper;     use GNATLLVM.Wrapper;

package body GNATLLVM.Aliasing is

   TBAA_Root : Metadata_T;
   --  Root of tree for Type-Based alias Analysis (TBAA) metadata

   function Create_TBAA_For_Type (TE : Entity_Id) return Metadata_T
     with Pre => Is_Type_Or_Void (TE);
   --  Create a TBAA type entry for the specified GNAT type

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      TBAA_Root := Create_TBAA_Root (MD_Builder);
   end Initialize;

   --------------------------
   -- Create_TBAA_For_Type --
   --------------------------

   function Create_TBAA_For_Type (TE : Entity_Id) return Metadata_T is
      BT   : constant Entity_Id  := Full_Base_Type (TE);
      TBAA : constant Metadata_T := Get_TBAA (BT);

   begin
      --  If we have -fno-strict-aliasing, don't create a TBAA

      if Flag_No_Strict_Aliasing then
         return No_Metadata_T;

      --  If the base type has a TBAA, use it for us.  If it doesn't, it's
      --  probably because this is the base type, in which case, make a
      --  new entry for it.  If it's a type that we don't currently make
      --  TBAA information for, return none.

      elsif Present (TBAA) then
         return TBAA;
      elsif Is_Scalar_Type (BT) then
         return Create_TBAA_Scalar_Type_Node (MD_Builder, Get_Name (BT),
                                              TBAA_Root);
      else
         return No_Metadata_T;
      end if;

   end Create_TBAA_For_Type;

   --------------------------
   -- Record_TBAA_For_Type --
   --------------------------

   procedure Record_TBAA_For_Type (TE : Entity_Id) is
      TBAA : constant Metadata_T := Create_TBAA_For_Type (TE);
   begin
      if Present (TBAA) then
         Set_TBAA (TE, TBAA);
      end if;
   end Record_TBAA_For_Type;

   ---------------------------------
   -- Add_Aliasing_To_Instruction --
   ---------------------------------

   procedure Add_Aliasing_To_Instruction (Inst : Value_T; V : GL_Value) is
      GT           : constant GL_Type    := Related_Type (V);
      TBAA         : constant Metadata_T := Get_TBAA (Full_Etype (GT));

   begin
      if Present (TBAA) and then not Universal_Aliasing (GT) then
         Add_TBAA_Access
           (Inst, Create_TBAA_Access_Tag (MD_Builder, TBAA, TBAA, 0));
      end if;
   end Add_Aliasing_To_Instruction;

end GNATLLVM.Aliasing;
