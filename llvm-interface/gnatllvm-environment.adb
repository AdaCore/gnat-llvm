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

with Errout; use Errout;

with LLVM.Core; use LLVM.Core;

package body GNATLLVM.Environment is

   --------------
   -- Has_Type --
   --------------

   function Has_Type (Env : Environ; TE : Entity_Id) return Boolean is
   begin
      return Get_Type (Env, TE) /= No_Type_T;
   end Has_Type;

   --------------
   -- Has_TBAA --
   --------------

   function Has_TBAA (Env : Environ; TE : Entity_Id) return Boolean is
   begin
      return Get_TBAA (Env, TE) /= No_Metadata_T;
   end Has_TBAA;

   ---------------
   -- Has_Value --
   ---------------

   function Has_Value (Env : Environ; VE : Entity_Id) return Boolean is
   begin
      return Get_Value (Env, VE) /= No_Value_T;
   end Has_Value;

   ------------
   -- Has_BB --
   ------------

   function Has_BB (Env : Environ; BE : Entity_Id) return Boolean is
   begin
      return Get_Basic_Block (Env, BE) /= No_BB_T;
   end Has_BB;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Env : Environ; TE : Entity_Id) return Type_T is
   begin
      if Env.LLVM_Info (TE) = Empty_LLVM_Info_Id then
         return No_Type_T;
      else
         return LLVM_Info_Table.Table (Env.LLVM_Info (TE)).Typ;
      end if;
   end Get_Type;

   --------------
   -- Get_TBAA --
   --------------

   function Get_TBAA (Env : Environ; TE : Entity_Id) return Metadata_T is
   begin
      if Env.LLVM_Info (TE) = Empty_LLVM_Info_Id then
         return No_Metadata_T;
      else
         return LLVM_Info_Table.Table (Env.LLVM_Info (TE)).TBAA;
      end if;
   end Get_TBAA;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Env : Environ; VE : Entity_Id) return Value_T is
   begin
      if Env.LLVM_Info (VE) = Empty_LLVM_Info_Id then
         return No_Value_T;
      else
         return LLVM_Info_Table.Table (Env.LLVM_Info (VE)).Value;
      end if;
   end Get_Value;

   ---------------------
   -- Get_Basic_Block --
   ---------------------

   function Get_Basic_Block
     (Env : Environ; BE : Entity_Id) return Basic_Block_T
   is
   begin
      if Env.LLVM_Info (BE) = Empty_LLVM_Info_Id then
         return No_BB_T;
      else
         return LLVM_Info_Table.Table (Env.LLVM_Info (BE)).Basic_Block;
      end if;
   end Get_Basic_Block;

   ---------------------
   -- Get_Record_Info --
   ---------------------

   function Get_Record_Info
     (Env : Environ; RI : Entity_Id) return Record_Info
   is
   begin
      return LLVM_Info_Table.Table (Env.LLVM_Info (RI)).Record_Inf;
   end Get_Record_Info;

   function Get_LLVM_Info_Id (Env : Environ; N : Node_Id) return LLVM_Info_Id;
   --  Helper for below to allocate LLVM_Info_Table entry if needed.

   ----------------------
   -- Get_LLVM_Info_Id --
   ----------------------

   function Get_LLVM_Info_Id
     (Env : Environ; N : Node_Id) return LLVM_Info_Id
   is
      Id : LLVM_Info_Id := Env.LLVM_Info (N);
   begin
      if Id /= Empty_LLVM_Info_Id then
         return Id;
      else
         LLVM_Info_Table.Append ((Value => No_Value_T, Typ => No_Type_T,
                                  TBAA => No_Metadata_T,
                                  Basic_Block => No_BB_T, others => <>));
         Id := LLVM_Info_Table.Last;
         Env.LLVM_Info (N) := Id;
         return Id;
      end if;
   end Get_LLVM_Info_Id;

   --------------------
   -- Copy_Type_Info --
   --------------------

   procedure Copy_Type_Info (Env : Environ; Old_T, New_T : Entity_Id) is
      Id : constant LLVM_Info_Id := Env.LLVM_Info (Old_T);
   begin
      pragma Assert (Id /= Empty_LLVM_Info_Id);
      pragma Assert (Env.LLVM_Info (New_T) = Empty_LLVM_Info_Id);
      --  We know this is a type and one for which we don't have any
      --  data, so we shouldn't have allocated anything for it.

      LLVM_Info_Table.Append (LLVM_Info_Table.Table (Id));
      Env.LLVM_Info (New_T) := LLVM_Info_Table.Last;
   end Copy_Type_Info;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type (Env : Environ; TE : Entity_Id; TL : Type_T) is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id (Env, TE);
   begin
      LLVM_Info_Table.Table (Id).Typ :=  TL;
   end Set_Type;

   --------------
   -- Set_TBAA --
   --------------

   procedure Set_TBAA (Env : Environ; TE : Entity_Id; TBAA : Metadata_T) is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id (Env, TE);
   begin
      LLVM_Info_Table.Table (Id).TBAA := TBAA;
   end Set_TBAA;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Env : Environ; VE : Entity_Id; VL : Value_T) is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id (Env, VE);
   begin
      LLVM_Info_Table.Table (Id).Value :=  VL;
   end Set_Value;

   ---------------------
   -- Set_Basic_Block --
   ---------------------

   procedure Set_Basic_Block
     (Env : Environ; BE : Entity_Id; BL : Basic_Block_T)
   is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id (Env, BE);
   begin
      LLVM_Info_Table.Table (Id).Basic_Block := BL;
   end Set_Basic_Block;

   ---------------------
   -- Set_Record_Info --
   ---------------------

   procedure Set_Record_Info
     (Env : Environ; TE : Entity_Id; RI : Record_Info)
   is
      Id : constant LLVM_Info_Id := Get_LLVM_Info_Id (Env, TE);
   begin
      LLVM_Info_Table.Table (Id).Record_Inf := RI;
   end Set_Record_Info;

   ---------------
   -- Push_Loop --
   ---------------

   procedure Push_Loop (LE : Entity_Id; Exit_Point : Basic_Block_T) is
   begin
      Exit_Point_Table.Append ((LE, Exit_Point));
   end Push_Loop;

   --------------
   -- Pop_Loop --
   --------------

   procedure Pop_Loop is
   begin
      Exit_Point_Table.Decrement_Last;
   end Pop_Loop;

   --------------------
   -- Get_Exit_Point --
   --------------------

   function Get_Exit_Point (LE : Entity_Id) return Basic_Block_T is
   begin
      for I in Exit_Point_Low_Bound .. Exit_Point_Table.Last loop
         if Exit_Point_Table.Table (I).Label_Entity = LE then
            return Exit_Point_Table.Table (I).Exit_BB;
         end if;
      end loop;

      --  If the loop label isn't registered, then we just met an exit
      --  statement with no corresponding loop: should not happen.

      Error_Msg_N ("unknown loop identifier", LE);
      raise Program_Error;
   end Get_Exit_Point;

   --------------------
   -- Get_Exit_Point --
   --------------------

   function Get_Exit_Point return Basic_Block_T is
   begin
      return Exit_Point_Table.Table (Exit_Point_Table.Last).Exit_BB;
   end Get_Exit_Point;

   ----------------
   -- Enter_Subp --
   ----------------

   procedure Enter_Subp (Env : Environ; Func : Value_T) is
   begin
      Env.Func := Func;
      Env.Activation_Rec_Param := No_Value_T;
      Position_Builder_At_End (Env.Bld, Create_Basic_Block (Env, "entry"));
   end Enter_Subp;

   ----------------
   -- Leave_Subp --
   ----------------

   procedure Leave_Subp (Env  : Environ) is
   begin
      Env.Func := No_Value_T;
   end Leave_Subp;

   -------------------
   -- Library_Level --
   -------------------

   function Library_Level (Env : Environ) return Boolean is
     (Env.Func = No_Value_T);

   ------------------------
   -- Create_Basic_Block --
   ------------------------

   function Create_Basic_Block
     (Env : Environ; Name : String) return Basic_Block_T
   is
   begin
      return Append_Basic_Block_In_Context (Env.Ctx, Env.Func, Name);
   end Create_Basic_Block;

end GNATLLVM.Environment;
