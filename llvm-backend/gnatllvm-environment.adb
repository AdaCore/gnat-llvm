with Ada.Unchecked_Deallocation;
with GNATLLVM.Id_Generator; use GNATLLVM.Id_Generator;

package body GNATLLVM.Environment is

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

   ---------
   -- Get --
   ---------

   function Get (Env : access Environ_Record; TE : Entity_Id) return Type_T is
      use Type_Maps;
   begin
      for S of reverse Env.Scopes loop
         declare
            C : constant Cursor := S.Types.Find (TE);
         begin
            if C /= No_Element then
               return Element (C);
            end if;
         end;
      end loop;
      raise Program_Error
        with "Cannot find a LLVM type for Entity #" & Entity_Id'Image (TE);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Env : access Environ_Record; VE : Entity_Id) return Value_T is
      use Value_Maps;
   begin
      for S of reverse Env.Scopes loop
         declare
            C : constant Cursor := S.Values.Find (VE);
         begin
            if C /= No_Element then
               return Element (C);
            end if;
         end;
      end loop;
      raise Program_Error
        with "Cannot find a LLVM value for Entity #" & Entity_Id'Image (VE);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Env : access Environ_Record; BE : Entity_Id) return Basic_Block_T is
   begin
      return Value_As_Basic_Block (Env.Get (BE));
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

   ---------------------
   -- Create_Function --
   ---------------------

   function Create_Subp
     (Env : access Environ_Record;
      Name : String; Typ : Type_T) return Subp_Env
   is
      Func : constant Value_T := Add_Function (Env.Mdl, Name, Typ);
      Subp : constant Subp_Env := new Subp_Env_Record'
        (Env           => Environ (Env),
         Func          => Func,
         Current_Block =>
           Append_Basic_Block_In_Context
             (Env.Ctx, Func, Id ("entry")));
   begin
      Env.Subprograms.Append (Subp);
      Env.Current_Subps.Append (Subp);
      Position_Builder_At_End (Env.Bld, Subp.Current_Block);
      return Subp;
   end Create_Subp;

   ----------------
   -- Leave_Subp --
   ----------------

   procedure Leave_Subp (Env  : access Environ_Record) is
   begin
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

   -----------------------------
   -- Set_Current_Basic_Block --
   -----------------------------

   procedure Set_Current_Basic_Block
     (Env : access Environ_Record; BB : Basic_Block_T) is
   begin
      Position_Builder_At_End (Env.Bld, BB);
      Env.Current_Subp.Current_Block := BB;
   end Set_Current_Basic_Block;

end GNATLLVM.Environment;
