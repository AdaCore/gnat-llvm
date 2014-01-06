with Ada.Unchecked_Deallocation;

with Sinfo; use Sinfo;

with GNATLLVM.Utils; use GNATLLVM.Utils;

package body GNATLLVM.Environment is

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

   ------------------------
   -- Begin_Declarations --
   ------------------------

   procedure Begin_Declarations (Env : access Environ_Record) is
   begin
      Env.Declarations_Level := Env.Declarations_Level + 1;
   end Begin_Declarations;

   ----------------------
   -- End_Declarations --
   ----------------------

   procedure End_Declarations (Env : access Environ_Record) is
   begin
      Env.Declarations_Level := Env.Declarations_Level - 1;
   end End_Declarations;

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
         pragma Annotate (Xcov, Exempt_On, "Defensive programming");
         raise No_Such_Type
           with "Cannot find a LLVM type for Entity #" & Entity_Id'Image (TE);
         pragma Annotate (Xcov, Exempt_Off);
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
         pragma Annotate (Xcov, Exempt_On, "Defensive programming");
         raise No_Such_Value
           with "Cannot find a LLVM value for Entity #" & Entity_Id'Image (VE);
         pragma Annotate (Xcov, Exempt_Off);
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
         pragma Annotate (Xcov, Exempt_On, "Defensive programming");
         raise No_Such_Value
           with "Cannot find a LLVM value for Entity #" & Entity_Id'Image (RI);
         pragma Annotate (Xcov, Exempt_Off);
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
         pragma Annotate (Xcov, Exempt_On, "Defensive programming");
         raise No_Such_Basic_Block
           with "Cannot find a LLVM basic block for Entity #"
           & Entity_Id'Image (BE);
         pragma Annotate (Xcov, Exempt_Off);
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

      pragma Annotate (Xcov, Exempt_On, "Defensive programming");
      raise Program_Error with "Unknown loop identifier";
      pragma Annotate (Xcov, Exempt_Off);
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
     (Env       : access Environ_Record;
      Subp_Spec : Node_Id) return Static_Link_Descriptor is
   begin
      return Env.S_Links.Element (Defining_Unit_Name (Subp_Spec));
   end Get_S_Link;

   --------------------
   -- Enter_Function --
   --------------------

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
         Saved_Builder_Position => Env.Bld.Get_Insert_Block,
         S_Link_Descr           => null);
   begin
      Subp.S_Link_Descr := Env.S_Links.Element (Subp_Ent);
      Env.Subprograms.Append (Subp);
      Env.Current_Subps.Append (Subp);
      Env.Bld.Position_At_End (Env.Create_Basic_Block ("entry"));
      return Subp;
   end Enter_Subp;

   ----------------
   -- Leave_Subp --
   ----------------

   procedure Leave_Subp (Env  : access Environ_Record) is
   begin
      Env.Bld.Position_At_End
        (Env.Current_Subps.Last_Element.Saved_Builder_Position);
      Env.Current_Subps.Delete_Last;
   end Leave_Subp;

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
