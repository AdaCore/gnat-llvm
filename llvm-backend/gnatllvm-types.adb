with Interfaces.C;

with Atree;    use Atree;
with Get_Targ; use Get_Targ;
with Nlists;   use Nlists;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with GNATLLVM.Utils; use GNATLLVM.Utils;
with GNATLLVM.Compile;

package body GNATLLVM.Types is

   ----------------------------
   -- Register_Builtin_Types --
   ----------------------------

   procedure Register_Builtin_Types (Env : Environ) is

      procedure Set_Rec (E : Entity_Id; T : Type_T);
      procedure Set_Rec (E : Entity_Id; T : Type_T) is
      begin
         Env.Set (E, T);
         if Etype (E) /= E then
            Set_Rec (Etype (E), T);
         end if;
      end Set_Rec;

      use Interfaces.C;
      Int_Size : constant unsigned := unsigned (Get_Int_Size);
   begin
      Set_Rec (Universal_Integer, Int_Type_In_Context (Env.Ctx, Int_Size));
      Set_Rec (Standard_Integer, Int_Type_In_Context (Env.Ctx, Int_Size));
      Set_Rec (Standard_Boolean, Int_Type_In_Context (Env.Ctx, 1));
      Set_Rec (Standard_Natural, Int_Type_In_Context (Env.Ctx, Int_Size));

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
      --  Associate an LLVM type for each argument

      for I in Param_Types'Range loop
         Param_Types (I) := Create_Type (Env, Parameter_Type (Arg));
         --  If this is an OUT parameter, take a pointer to the actual
         --  parameter.
         if Out_Present (Arg) then
            Param_Types (I) := Pointer_Type (Param_Types (I), 0);
         end if;
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

   -----------------
   -- Create_Type --
   -----------------

   function Create_Type (Env : Environ; Type_Node : Node_Id) return Type_T is
   begin
      case Nkind (Type_Node) is
         when N_Defining_Identifier =>
            return Env.Get (Type_Node);

         when N_Identifier =>
            --  If the type does not yet exist in the environnment, it may have
            --  been drawn from another file (adb's own spec file or another),
            --  so we lazily compile the full type declaration.
            if not Env.Has_Type (Entity (Type_Node)) then
               GNATLLVM.Compile.Compile (Env, Parent (Entity (Type_Node)));
            end if;
            return Env.Get (Entity (Type_Node));

         when N_Access_Definition =>
            return Pointer_Type
              (Create_Type (Env, Subtype_Mark (Type_Node)), 0);

         when N_Access_To_Object_Definition =>
            return Pointer_Type
              (Create_Type (Env, Subtype_Indication (Type_Node)), 0);

         when N_Record_Definition =>
            declare
               Struct_Type : constant Type_T := Struct_Create_Named
                 (Env.Ctx,
                  Get_Name (Defining_Identifier (Parent (Type_Node))));
               Comp_List : constant List_Id :=
                 Component_Items (Component_List (Type_Node));
               LLVM_Comps : array (1 .. List_Length (Comp_List)) of Type_T;
               I : Int := 1;
            begin
               for El of
                 Iterate (Component_Items (Component_List (Type_Node)))
               loop
                  LLVM_Comps (I) :=
                    Create_Type
                      (Env, Subtype_Indication (Component_Definition (El)));
                  I := I + 1;
               end loop;
               Struct_Set_Body
                 (Struct_Type, LLVM_Comps'Address, LLVM_Comps'Length, 0);
               return Struct_Type;
            end;

         when others =>
            raise Program_Error
              with "Unhandled node: " & Node_Kind'Image (Nkind (Type_Node));
      end case;
   end Create_Type;

end GNATLLVM.Types;
