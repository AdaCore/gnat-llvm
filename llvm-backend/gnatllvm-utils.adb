with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Namet;  use Namet;
with Nlists; use Nlists;

package body GNATLLVM.Utils is

   ---------
   -- Img --
   ---------

   function Img (I : Nat) return String is
      Str : constant String := I'Img;
   begin
      return Str (Str'First + 1 .. Str'Last);
   end Img;

   ---------------------
   -- Get_Param_Types --
   ---------------------

   function Get_Param_Types (Fn_Ty : Type_T) return Type_Array is
      Fn_Ty_Real : constant Type_T :=
        (if Get_Type_Kind (Fn_Ty) = Pointer_Type_Kind
         then Get_Element_Type (Fn_Ty)
         else Fn_Ty);
      Params_Types : Type_Array (1 .. Nat (Count_Param_Types (Fn_Ty_Real)));
   begin
      Get_Param_Types (Fn_Ty_Real, Params_Types'Address);
      return Params_Types;
   end Get_Param_Types;

   -----------------------------
   -- UI_To_Long_Long_Integer --
   -----------------------------

   function UI_To_Long_Long_Integer (U : Uint) return Long_Long_Integer is
   begin
      UI_Image (U, Decimal);
      declare
         Img   : constant String := UI_Image_Buffer (1 .. UI_Image_Length);
         Img_W : constant String := (if Img (1) = '-' then "" else " ") & Img;
      begin
         return Long_Long_Integer'Value (Img_W);
      end;
   end UI_To_Long_Long_Integer;

   --------------------
   -- Get_Type_Range --
   --------------------

   function Get_Dim_Range (N : Node_Id) return Node_Id is
   begin
      case Nkind (N) is
         when N_Range =>
            return N;
         when N_Identifier =>
            return Scalar_Range (Entity (N));

         when N_Subtype_Indication =>
            declare
               Constr : constant Node_Id := Constraint (N);
            begin
               if Present (Constr) then
                  case Nkind (Constr) is
                     when N_Range_Constraint =>
                        return Range_Expression (Constr);

                     when N_Index_Or_Discriminant_Constraint =>
                        --  TODO
                        raise Program_Error
                          with "Composite constraints are unhandled,"
                          & " right now";

                     when N_Digits_Constraint | N_Delta_Constraint =>
                        raise Program_Error
                          with "Unhandled subtype indication constraint"
                          & " (no fixed point arithmetics): "
                          & Node_Kind'Image (Nkind (N));

                     when others =>
                        pragma Annotate
                          (Xcov, Exempt_On, "Defensive programming");
                        raise Program_Error
                          with "Invalid subtype indication constraint: "
                          & Node_Kind'Image (Nkind (N));
                        pragma Annotate
                          (Xcov, Exempt_Off, "Defensive programming");
                  end case;

               else
                  return Scalar_Range (Entity (Subtype_Mark (N)));
               end if;
            end;

         when others =>
            pragma Annotate (Xcov, Exempt_On, "Defensive programming");
            raise Program_Error
              with "Invalid node kind in context: "
              & Node_Kind'Image (Nkind (N));
            pragma Annotate (Xcov, Exempt_Off);
      end case;
   end Get_Dim_Range;

   ---------------
   -- Is_LValue --
   ---------------

   function Is_LValue (Node : Node_Id) return Boolean
   is
      N : Node_Id := Node;
   begin
      loop
         case Nkind (N) is
            when N_Explicit_Dereference =>
               return True;
            when N_Selected_Component | N_Indexed_Component | N_Slice =>
               N := Prefix (N);
            when N_Identifier =>
               N := Entity (N);
            when N_Defining_Identifier =>
               if Present (Renamed_Object (N)) then
                  N := Renamed_Object (N);
               else
                  return True;
               end if;
            when others =>
               return False;
         end case;
      end loop;
   end Is_LValue;

   ---------------------
   -- Param_Needs_Ptr --
   ---------------------

   function Param_Needs_Ptr (Param : Entity_Id) return Boolean is
      PT : constant Entity_Id := Etype (Param);
   begin
      return

        --  Out/InOut parameters are passed by reference and thus need a
        --  pointer.

        (Ekind (Param) = E_In_Out_Parameter
         or else Ekind (Param) = E_Out_Parameter

         --  At the moment, let's consider that all arrays need to be
         --  passed as pointers: constrained arrays can have dynamic
         --  bounds.

         or else Ekind (PT) in Array_Kind);
   end Param_Needs_Ptr;

   ----------------------------
   -- Return_Needs_Sec_Stack --
   ----------------------------

   function Return_Needs_Sec_Stack (Arg : Node_Id) return Boolean is
      pragma Unreferenced (Arg);
   begin
      return False;
   end Return_Needs_Sec_Stack;

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
         when N_Op_Add | N_Op_Subtract | N_Op_Divide | N_Op_Multiply
           | N_Op_Rem | N_Op_Mod
           | N_Op_Eq | N_Op_Ne
           | N_Op_Gt | N_Op_Lt | N_Op_Le | N_Op_Ge
           | N_Op_Shift_Left | N_Op_Shift_Right | N_Op_Shift_Right_Arithmetic
           | N_Op_Rotate_Left | N_Op_Rotate_Right
           | N_Op_And | N_Op_Or | N_Op_Xor => True,
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

   ---------------------
   -- Get_Acting_Spec --
   ---------------------

   function Get_Acting_Spec (Subp_Body : Node_Id) return Node_Id is
   begin
      if Acts_As_Spec (Subp_Body) then
         return Specification (Subp_Body);
      else
         return Parent (Corresponding_Spec (Subp_Body));
      end if;
   end Get_Acting_Spec;

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
            if Filter (Cur) then
               A (I) := Cur;
               I := I + 1;
            end if;
            Cur := Get_Next (Cur);
         end loop;
         return A (1 .. I - 1);
      end;
   end Iterate_Entities;

   ----------------
   -- Get_Params --
   ----------------

   function Get_Params (Subp : Entity_Id) return Entity_Iterator is
      function Is_Formal (E : Entity_Id) return Boolean is
        (Ekind (E) in Formal_Kind);

      function Iterate is new Iterate_Entities
        (Get_First => First_Entity,
         Get_Next  => Next_Entity,
         Filter    => Is_Formal);

      function Iterate_Extra_Formals is new Iterate_Entities
        (Get_First => Extra_Formals,
         Get_Next  => Next_Entity,
         Filter    => Is_Formal);
   begin
      return Iterate (Subp) & Iterate_Extra_Formals (Subp);
   end Get_Params;

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
         Function_Type (Result_Type, Null_Address, 0, False));
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
           (Void_Type_In_Context (Env.Ctx), Param_Type'Address, 1, False));
   end Get_Stack_Restore;

   pragma Annotate (Xcov, Exempt_On, "Debug helpers");

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

   pragma Annotate (Xcov, Exempt_Off, "Debug helpers");

end GNATLLVM.Utils;
