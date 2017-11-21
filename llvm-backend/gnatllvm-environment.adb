------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2017, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with System;

with Errout; use Errout;
with Sinfo; use Sinfo;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Utils; use GNATLLVM.Utils;

package body GNATLLVM.Environment is

   use Ada.Containers;

   function Get
     (Env : access Environ_Record; TE : Entity_Id)
      return Type_Maps.Cursor;

   function Get
     (Env : access Environ_Record; VE : Entity_Id)
      return Value_Maps.Cursor;

   function Get
     (Env : access Environ_Record; RI : Entity_Id)
      return Record_Info_Maps.Cursor;

   ----------------
   -- Push_Scope --
   ----------------

   procedure Push_Scope (Env : access Environ_Record) is
   begin
      Env.Scopes.Append (new Scope_Type'(others => <>));
   end Push_Scope;

   ---------------
   -- Pop_Scope --
   ---------------

   procedure Pop_Scope (Env : access Environ_Record) is
      procedure Free is new Ada.Unchecked_Deallocation (Scope_Type, Scope_Acc);
      Last_Scope : Scope_Acc := Env.Scopes.Last_Element;
   begin
      Free (Last_Scope);
      Env.Scopes.Delete_Last;
   end Pop_Scope;

   ----------------------
   -- Set_In_Main_Unit --
   ----------------------

   procedure Set_In_Main_Unit
     (Env : access Environ_Record; In_Main_Unit : Boolean := True) is
   begin
      Env.In_Main_Unit := In_Main_Unit;
   end Set_In_Main_Unit;

   --------------
   -- Has_Type --
   --------------

   function Has_Type
     (Env : access Environ_Record; TE : Entity_Id) return Boolean
   is
      use Type_Maps;
   begin
      return Get (Env, TE) /= No_Element;
   end Has_Type;

   ---------------
   -- Has_Value --
   ---------------

   function Has_Value
     (Env : access Environ_Record; VE : Entity_Id) return Boolean
   is
      use Value_Maps;
   begin
      return Get (Env, VE) /= No_Element;
   end Has_Value;

   ------------
   -- Has_BB --
   ------------

   function Has_BB
     (Env : access Environ_Record; BE : Entity_Id) return Boolean
   is
      use Value_Maps;
   begin
      return Get (Env, BE) /= No_Element;
   end Has_BB;

   ---------
   -- Get --
   ---------

   function Get
     (Env : access Environ_Record; TE : Entity_Id)
      return Type_Maps.Cursor is
      use Type_Maps;
   begin
      for S of reverse Env.Scopes loop
         declare
            C : constant Cursor := S.Types.Find (TE);
         begin
            if C /= No_Element then
               return C;
            end if;
         end;
      end loop;
      return No_Element;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Env : access Environ_Record; VE : Entity_Id)
      return Value_Maps.Cursor is
      use Value_Maps;
   begin
      for S of reverse Env.Scopes loop
         declare
            C : constant Cursor := S.Values.Find (VE);
         begin
            if C /= No_Element then
               return C;
            end if;
         end;
      end loop;
      return No_Element;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Env : access Environ_Record; RI : Entity_Id)
      return Record_Info_Maps.Cursor is
      use Record_Info_Maps;
      E : constant Entity_Id := GNATLLVM.Utils.Get_Fullest_View (RI);
   begin
      for S of reverse Env.Scopes loop
         declare
            C : constant Cursor := S.Records_Infos.Find (E);
         begin
            if C /= No_Element then
               return C;
            end if;
         end;
      end loop;
      return No_Element;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Env : access Environ_Record; TE : Entity_Id) return Type_T is
      use Type_Maps;
      Cur : constant Cursor := Get (Env, TE);
   begin
      if Cur /= No_Element then
         return Element (Cur);
      else
         Error_Msg_N ("cannot find matching LLVM type", TE);
         raise No_Such_Type;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Env : access Environ_Record; VE : Entity_Id) return Value_T is
      use Value_Maps;
      Cur : constant Cursor := Get (Env, VE);
   begin
      if Cur /= No_Element then
         return Element (Cur);
      else
         Error_Msg_N ("cannot find matching LLVM value", VE);
         raise No_Such_Value;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Env : access Environ_Record; RI : Entity_Id) return Record_Info
   is
      use Record_Info_Maps;
      Cur : constant Cursor := Get (Env, RI);
   begin
      if Cur /= No_Element then
         return Element (Cur);
      else
         Error_Msg_N ("cannot find matching LLVM value", RI);
         raise No_Such_Value;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Env : access Environ_Record; BE : Entity_Id) return Basic_Block_T is
      use Value_Maps;
      Cur : constant Cursor := Get (Env, BE);
   begin
      if Cur /= No_Element then
         return Value_As_Basic_Block (Env.Get (BE));
      else
         Error_Msg_N ("cannot find matching LLVM basic block", BE);
         raise No_Such_Basic_Block;
      end if;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (Env : access Environ_Record; TE : Entity_Id; TL : Type_T) is
   begin
      Env.Scopes.Last_Element.Types.Include (TE, TL);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Env : access Environ_Record; TE : Entity_Id; RI : Record_Info) is
   begin
      Env.Scopes.Last_Element.Records_Infos.Include (TE, RI);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Env : access Environ_Record; VE : Entity_Id; VL : Value_T) is
   begin
      Env.Scopes.Last_Element.Values.Insert (VE, VL);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Env : access Environ_Record; BE : Entity_Id; BL : Basic_Block_T) is
   begin
      Env.Set (BE, Basic_Block_As_Value (BL));
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

   ------------------
   -- Takes_S_Link --
   ------------------

   function Takes_S_Link
     (Env  : access Environ_Record;
      Subp : Entity_Id) return Boolean
   is
      use Static_Link_Descriptor_Maps;

      S_Link_Cur : constant Cursor := Env.S_Links.Find (Subp);
   begin
      return S_Link_Cur /= No_Element
        and then Element (S_Link_Cur).Parent /= null;
   end Takes_S_Link;

   ----------------
   -- Get_S_Link --
   ----------------

   function Get_S_Link
     (Env  : access Environ_Record;
      Subp : Entity_Id) return Static_Link_Descriptor is
   begin
      return Env.S_Links.Element (Subp);
   end Get_S_Link;

   ----------------
   -- Enter_Subp --
   ----------------

   function Enter_Subp
     (Env       : access Environ_Record;
      Subp_Body : Node_Id;
      Func      : Value_T) return Subp_Env
   is
      Subp_Ent : constant Entity_Id :=
        Defining_Unit_Name (Get_Acting_Spec (Subp_Body));
      Subp     : constant Subp_Env := new Subp_Env_Record'
        (Env                    => Environ (Env),
         Func                   => Func,
         Saved_Builder_Position => Get_Insert_Block (Env.Bld),
         S_Link_Descr           => null,
         S_Link                 => Value_T (System.Null_Address));
   begin
      Subp.S_Link_Descr := Env.S_Links.Element (Subp_Ent);
      Env.Subprograms.Append (Subp);
      Env.Current_Subps.Append (Subp);
      Position_Builder_At_End (Env.Bld, Env.Create_Basic_Block ("entry"));
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
