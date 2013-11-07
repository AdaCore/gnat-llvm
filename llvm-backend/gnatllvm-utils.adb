with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Namet;  use Namet;
with Nlists; use Nlists;
with Einfo; use Einfo;

package body GNATLLVM.Utils is

   ---------------------
   -- Param_Needs_Ptr --
   ---------------------

   function Param_Needs_Ptr (Arg : Node_Id) return Boolean is
      PT : constant Node_Id := Parameter_Type (Arg);
   begin
      return (Nkind (PT) /= N_Access_Definition
              and then not Is_Access_Type (Etype (PT))
              and then
                (Out_Present (Arg)
                 or else not Is_Constrained (Etype (PT))));
   end Param_Needs_Ptr;

   -------------------
   -- Index_In_List --
   -------------------

   function Index_In_List (N : Node_Id) return Natural is
      L : constant List_Id := List_Containing (N);
      Cur_N : Node_Id := First (L);
      I : Natural := 1;
   begin

      while Present (Cur_N) loop
         exit when Cur_N = N;
         I := I + 1;
         Cur_N := Next (Cur_N);
      end loop;

      return I;
   end Index_In_List;

   ------------------------
   -- Is_Binary_Operator --
   ------------------------

   function Is_Binary_Operator (Node : Node_Id) return Boolean is
     (case Nkind (Node) is
         when N_Op_Add | N_Op_Eq | N_Op_Subtract |
              N_Op_Divide | N_Op_Multiply | N_Op_Gt | N_Op_Lt |
              N_Op_Le | N_Op_Ge | N_Op_Ne => True,
         when others => False);

   -------------
   -- Discard --
   -------------

   procedure Discard (V : Value_T) is
      pragma Unreferenced (V);
   begin
      null;
   end Discard;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (E : Entity_Id) return String is
   begin
      return Get_Name_String (Chars (E));
   end Get_Name;

   -------------
   -- Iterate --
   -------------

   function Iterate (L : List_Id) return List_Iterator
   is
      Len : constant Nat := List_Length (L);
      A : List_Iterator (1 .. Len);
      N : Node_Id := First (L);
      I : Nat := 1;
   begin
      while Present (N) loop
         A (I) := N;
         I := I + 1;
         N := Next (N);
      end loop;
      return A;
   end Iterate;

   ----------------------
   -- Iterate_Entities --
   ----------------------

   function Iterate_Entities (Root : Entity_Id) return Entity_Iterator
   is
      Len : Nat := 0;
      Cur : Entity_Id := Get_First (Root);
   begin
      while Cur /= Empty loop
         Cur := Get_Next (Cur);
         Len := Len + 1;
      end loop;

      declare
         A : Entity_Iterator (1 .. Len);
         I : Nat := 1;
      begin
         Cur := Get_First (Root);
         while Cur /= Empty loop
            A (I) := Cur;
            Cur := Get_Next (Cur);
            I := I + 1;
         end loop;
         return A;
      end;
   end Iterate_Entities;

   --------------------
   -- Get_Stack_Save --
   --------------------

   function Get_Stack_Save (Env : Environ) return Value_T is
      Result_Type : constant Type_T :=
        Pointer_Type (Int8_Type_In_Context (Env.Ctx), 0);
   begin
      return Add_Function
        (Env.Mdl,
         "llvm.stacksave",
         Function_Type
           (Result_Type,
            Null_Address, 0,
            Boolean'Pos (False)));
   end Get_Stack_Save;

   -----------------------
   -- Get_Stack_Restore --
   -----------------------

   function Get_Stack_Restore (Env : Environ) return Value_T is
      Param_Type : constant Type_T :=
        Pointer_Type (Int8_Type_In_Context (Env.Ctx), 0);
   begin
      return Add_Function
        (Env.Mdl,
         "llvm.stackrestore",
         Function_Type
           (Void_Type_In_Context (Env.Ctx),
            Param_Type'Address, 1,
            Boolean'Pos (False)));
   end Get_Stack_Restore;

   ---------------------
   -- Dump_LLVM_Value --
   ---------------------

   procedure Dump_LLVM_Value (V : Value_T) is
   begin
      Dump_Value (V);
   end Dump_LLVM_Value;

   ----------------------
   -- Dump_LLVM_Module --
   ----------------------

   procedure Dump_LLVM_Module (M : Module_T) is
   begin
      Dump_Module (M);
   end Dump_LLVM_Module;

   --------------------
   -- Dump_LLVM_Type --
   --------------------

   procedure Dump_LLVM_Type (T : Type_T) is

      --  LLVM::Type::dump is not bound in the C API and thus is not available
      --  in the Ada bindings. Hack instead.

      type Type_Class is limited null record;
      pragma Import (CPP, Type_Class);
      type Type_Class_Ptr is access Type_Class;

      procedure Dump (This : Type_Class_Ptr);
      pragma Import (CPP, Dump, "_ZNK4llvm4Type4dumpEv");

      function Unwrap is new Ada.Unchecked_Conversion
        (Type_T, Type_Class_Ptr);

   begin
      Dump (Unwrap (T));
      New_Line (Current_Error);
   end Dump_LLVM_Type;

end GNATLLVM.Utils;
