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
   -- Set --
   ---------

   procedure Set (Env : access Environ_Record; TE : Entity_Id; TL : Type_T) is
   begin
      Env.Scopes.Last_Element.Types.Insert (TE, TL);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Env : access Environ_Record; VE : Entity_Id; VL : Value_T) is
   begin
      Env.Scopes.Last_Element.Values.Insert (VE, VL);
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
          (Func => Func,
           Current_Block =>
             Append_Basic_Block_In_Context
               (Env.Ctx, Func, Generate_Id ("entry")),
           Env => Environ (Env));
   begin
      Env.Subprograms.Append (Subp);
      Position_Builder_At_End (Env.Bld, Subp.Current_Block);
      return Subp;
   end Create_Subp;

end GNATLLVM.Environment;
