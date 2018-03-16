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

with GNATLLVM.Utils; use GNATLLVM.Utils;

package body GNATLLVM.Environment is

   use Ada.Containers;

   --------------
   -- Has_Type --
   --------------

   function Has_Type
     (Env : access Environ_Record; TE : Entity_Id) return Boolean
   is
   begin
      return Get (Env, TE) /= No_Type_T;
   end Has_Type;

   ---------------
   -- Has_Value --
   ---------------

   function Has_Value
     (Env : access Environ_Record; VE : Entity_Id) return Boolean
   is
   begin
      return Get (Env, VE) /= No_Value_T;
   end Has_Value;

   ------------
   -- Has_BB --
   ------------

   function Has_BB
     (Env : access Environ_Record; BE : Entity_Id) return Boolean
   is
   begin
      return Get (Env, BE) /= No_BB_T;
   end Has_BB;

   ---------
   -- Get --
   ---------

   function Get (Env : access Environ_Record; VE : Entity_Id) return Value_T is
   begin
      return Env.LLVM_Info (VE).Value;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Env : access Environ_Record; TE : Entity_Id)
     return Type_T
   is
      E : constant Entity_Id := GNATLLVM.Utils.Get_Fullest_View (TE);
   begin
      return Env.LLVM_Info (E).Typ;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Env : access Environ_Record; RI : Entity_Id) return Record_Info
   is
      E : constant Entity_Id := GNATLLVM.Utils.Get_Fullest_View (RI);
   begin
      return Env.LLVM_Info (E).Record_Inf;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Env : access Environ_Record; BE : Entity_Id) return Basic_Block_T is
   begin
      return Env.LLVM_Info (BE).Basic_Block;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (Env : access Environ_Record; TE : Entity_Id; TL : Type_T) is
   begin
      Env.LLVM_Info (TE).Typ :=  TL;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Env : access Environ_Record; TE : Entity_Id; RI : Record_Info) is
   begin
      Env.LLVM_Info (TE).Record_Inf := RI;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Env : access Environ_Record; VE : Entity_Id; VL : Value_T) is
   begin
      Env.LLVM_Info (VE).Value :=  VL;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Env : access Environ_Record; BE : Entity_Id; BL : Basic_Block_T) is
   begin
      Env.LLVM_Info (BE).Basic_Block := BL;
   end Set;

   ---------------
   -- Push_Loop --
   ---------------

   procedure Push_Loop
     (Env : access Environ_Record;
      LE : Entity_Id;
      Exit_Point : Basic_Block_T) is
   begin
      Env.Exit_Points.Append ((LE, Exit_Point));
   end Push_Loop;

   --------------
   -- Pop_Loop --
   --------------

   procedure Pop_Loop (Env : access Environ_Record) is
   begin
      Env.Exit_Points.Delete_Last;
   end Pop_Loop;

   --------------------
   -- Get_Exit_Point --
   --------------------

   function Get_Exit_Point
     (Env : access Environ_Record; LE : Entity_Id) return Basic_Block_T is
   begin
      for Exit_Point of Env.Exit_Points loop
         if Exit_Point.Label_Entity = LE then
            return Exit_Point.Exit_BB;
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

   function Get_Exit_Point
     (Env : access Environ_Record) return Basic_Block_T is
   begin
      return Env.Exit_Points.Last_Element.Exit_BB;
   end Get_Exit_Point;

   ----------------
   -- Enter_Subp --
   ----------------

   function Enter_Subp
     (Env       : access Environ_Record;
      Func      : Value_T) return Subp_Env
   is
      Subp : constant Subp_Env := new Subp_Env_Record'
        (Env                    => Environ (Env),
         Func                   => Func,
         Saved_Builder_Position => Get_Insert_Block (Env.Bld),
         Activation_Rec_Param   => No_Value_T);

   begin
      Env.Current_Subps.Append (Subp);
      Position_Builder_At_End (Env.Bld, Create_Basic_Block (Env, "entry"));
      return Subp;
   end Enter_Subp;

   ----------------
   -- Leave_Subp --
   ----------------

   procedure Leave_Subp (Env  : access Environ_Record) is
   begin
      --  There is no builder position to restore if no subprogram translation
      --  was interrupted in order to translate the current subprogram.

      if Env.Current_Subps.Length > 1 then
         Position_Builder_At_End
           (Env.Bld, Env.Current_Subps.Last_Element.Saved_Builder_Position);
      end if;

      Env.Current_Subps.Delete_Last;
   end Leave_Subp;

   -------------------
   -- Library_Level --
   -------------------

   function Library_Level (Env : access Environ_Record) return Boolean is
     (Env.Current_Subps.Length = 0);

   ------------------
   -- Current_Subp --
   ------------------

   function Current_Subp (Env : access Environ_Record) return Subp_Env is
   begin
      return Env.Current_Subps.Last_Element;
   end Current_Subp;

   ------------------------
   -- Create_Basic_Block --
   ------------------------

   function Create_Basic_Block
     (Env : access Environ_Record; Name : String) return Basic_Block_T is
   begin
      return Append_Basic_Block_In_Context
        (Env.Ctx, Current_Subp (Env).Func, Name);
   end Create_Basic_Block;

end GNATLLVM.Environment;
