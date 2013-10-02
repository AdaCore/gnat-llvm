with Ada.Unchecked_Deallocation;

package body GNATLLVM.Environment is

   ----------------
   -- Push_Scope --
   ----------------

   procedure Push_Scope (Env : in out Environ) is
   begin
      Env.Scopes.Append (new Scope_Type'(others => <>));
   end Push_Scope;

   ---------------
   -- Pop_Scope --
   ---------------

   procedure Pop_Scope (Env : in out Environ) is
      procedure Free is new Ada.Unchecked_Deallocation (Scope_Type, Scope_Acc);
      Last_Scope : Scope_Acc := Env.Scopes.Last_Element;
   begin
      Free (Last_Scope);
      Env.Scopes.Delete_Last;
   end Pop_Scope;

   ---------
   -- Get --
   ---------

   function Get (Env : Environ; TE : Entity_Id) return Type_T is
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

   function Get (Env : Environ; VE : Entity_Id) return Value_T is
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

   procedure Set (Env : Environ; TE : Entity_Id; TL : Type_T) is
   begin
      Env.Scopes.Last_Element.Types.Insert (TE, TL);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Env : Environ; VE : Entity_Id; VL : Value_T) is
   begin
      Env.Scopes.Last_Element.Values.Insert (VE, VL);
   end Set;

end GNATLLVM.Environment;
