with Interfaces.C;

with Atree;    use Atree;
with Get_Targ; use Get_Targ;
with Nlists;   use Nlists;
with Sinfo;    use Sinfo;
with Stand;    use Stand;

package body GNATLLVM.Types is

   ----------------------------
   -- Register_Builtin_Types --
   ----------------------------

   procedure Register_Builtin_Types (Env : Environ) is
      use Interfaces.C;
      Int_Size : constant unsigned := unsigned (Get_Int_Size);
   begin
      Env.Set (Universal_Integer, Int_Type_In_Context (Env.Ctx, Int_Size));
      Env.Set (Standard_Integer, Int_Type_In_Context (Env.Ctx, Int_Size));

      Env.Set (Standard_Natural, Int_Type_In_Context (Env.Ctx, Int_Size));

      --  TODO??? add other builtin types!
   end Register_Builtin_Types;

   ----------------------------
   -- Create_Subprogram_Type --
   ----------------------------

   function Create_Subprogram_Type
     (Env : Environ; Subp_Spec : Node_Id) return Type_T
   is
      Param_Specs : constant List_Id := Parameter_Specifications (Subp_Spec);
      Param_Types : array (1 .. List_Length (Param_Specs)) of Type_T;
      Return_Type : Type_T;
      Arg         : Node_Id := First (Param_Specs);
   begin
      --  Set the

      for I in Param_Types'Range loop
         Param_Types (I) := Create_Type (Env, Parameter_Type (Arg));
         Arg := Next (Arg);
      end loop;

      --  Set the LLVM function return type

      case Nkind (Subp_Spec) is
         when N_Procedure_Specification =>
            Return_Type := Void_Type_In_Context (Env.Ctx);
         when N_Function_Specification =>
            Return_Type := Create_Type (Env, Result_Definition (Subp_Spec));
         when others =>
            raise Program_Error
              with "Invalid node: " & Node_Kind'Image (Nkind (Subp_Spec));
      end case;

      return Function_Type
        (Return_Type,
         Param_Types'Address,
         Param_Types'Length,
         Is_Var_Arg => Boolean'Pos (False));
   end Create_Subprogram_Type;

   function Create_Type (Env : Environ; Type_Node : Node_Id) return Type_T is
   begin
      case Nkind (Type_Node) is
         when N_Defining_Identifier =>
            return Env.Get (Type_Node);
         when N_Identifier =>
            return Env.Get (Entity (Type_Node));
         when others =>
            raise Program_Error
              with "Unhandled node: " & Node_Kind'Image (Nkind (Type_Node));
      end case;
   end Create_Type;

end GNATLLVM.Types;
