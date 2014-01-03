with Interfaces.C;
with Get_Targ; use Get_Targ;
with Nlists;   use Nlists;
with Sem_Eval; use Sem_Eval;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Uintp; use Uintp;
with Ttypes;

with GNATLLVM.Compile;
with GNATLLVM.Utils; use GNATLLVM.Utils;

package body GNATLLVM.Types is

   ----------------------
   -- Get_Address_Type --
   ----------------------

   function Get_Address_Type return Type_T
   is
     (Int_Ty (Natural (Ttypes.System_Address_Size)));

   ----------------------------------
   -- Get_Innermost_Component_Type --
   ----------------------------------

   function Get_Innermost_Component_Type
     (Env : Environ; N : Entity_Id) return Type_T
   is
     (if Is_Array_Type (N)
      then Get_Innermost_Component_Type (Env, Component_Type (N))
      else Create_Type (Env, N));

   ------------
   -- Int_Ty --
   ------------

   function Int_Ty (Num_Bits : Natural) return Type_T
   is
      (Int_Type (Interfaces.C.unsigned (Num_Bits)));

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

   -----------
   -- Fn_Ty --
   -----------

   function Fn_Ty (Param_Ty : Type_Array; Ret_Ty : Type_T) return Type_T
   is
     (Function_Type (Ret_Ty, Param_Ty'Address, Param_Ty'Length, False));

   -----------------------
   -- Array_Bounds_Type --
   -----------------------

   function Array_Bounds_Type return Type_T
   is
      (Int_Type (Interfaces.C.unsigned (Get_Pointer_Size)));

   -----------------------------
   -- Array_Bounds_Array_Type --
   -----------------------------

   function Array_Bounds_Array_Type (Nb_Dims : Nat) return Type_T is
      Bounds_Type : constant Type_T := Array_Bounds_Type;
      use Interfaces.C;
      Ret : constant Type_T :=
        Array_Type (Bounds_Type, unsigned (Nb_Dims * 2));
   begin
      return Ret;
   end Array_Bounds_Array_Type;

   -----------------
   -- Access_Type --
   -----------------

   function Create_Access_Type
     (Env : Environ; Type_Node : Node_Id) return Type_T
   is
      T : constant Type_T := Create_Type (Env, Type_Node);
      TT : constant Entity_Id :=
        (if Nkind (Type_Node) in N_Entity and then Is_Type (Type_Node)
         then Entity_Id (Type_Node)
         else Etype (Type_Node));
   begin
      if Get_Type_Kind (T) = Array_Type_Kind
        and then not Is_Constrained (TT)
      then
         declare
            Nb_Dimensions : constant Nat :=
              List_Length (List_Containing (First_Index (TT)));
            St_Els : Type_Array (1 .. 2) :=
              (Pointer_Type (T, 0),
               Array_Bounds_Array_Type (Nb_Dimensions));
         begin
            return Struct_Type (St_Els'Address, St_Els'Length, False);
         end;
      else
         return Pointer_Type (T, 0);
      end if;
   end Create_Access_Type;

   ----------------------------
   -- Create_Subprogram_Type --
   ----------------------------

   function Create_Subprogram_Type
     (Env : Environ; Subp_Spec : Node_Id) return Type_T
   is
      Param_Specs : constant List_Id := Parameter_Specifications (Subp_Spec);
      Param_Types : Type_Array (1 .. List_Length (Param_Specs));
      Return_Type : Type_T;
      Arg         : Node_Id := First (Param_Specs);
   begin

      --  Associate an LLVM type for each argument
      for Param of Param_Types loop

         --  If this is an out parameter, or a parameter whose type is
         --  unconstrained, take a pointer to the actual parameter.

         Param :=
           (if Param_Needs_Ptr (Arg)
            then Create_Access_Type (Env, Parameter_Type (Arg))
            else Create_Type (Env, Parameter_Type (Arg)));

         Arg := Next (Arg);
      end loop;

      --  Set the LLVM function return type

      case Nkind (Subp_Spec) is
         when N_Procedure_Specification =>
            Return_Type := Void_Type_In_Context (Env.Ctx);
         when N_Function_Specification =>
            if Return_Needs_Sec_Stack (Subp_Spec) then
               Return_Type := Create_Access_Type
                 (Env, Result_Definition (Subp_Spec));
            else
               Return_Type := Create_Type
                 (Env, Result_Definition (Subp_Spec));
            end if;
         when others =>
            raise Program_Error
              with "Invalid node: " & Node_Kind'Image (Nkind (Subp_Spec));
      end case;

      return Fn_Ty (Param_Types, Return_Type);

   end Create_Subprogram_Type;

   -----------------
   -- Create_Type --
   -----------------

   function Create_Type (Env : Environ; Type_Node : Node_Id) return Type_T is
      Def_Ident : Entity_Id;
   begin
      --  First, look for this type in the environment, and return it if it's
      --  there.

      case Nkind (Type_Node) is
         when N_Defining_Identifier =>
            if Env.Has_Type (Type_Node) then
               return Env.Get (Type_Node);
            end if;

         when N_Identifier =>
            return Create_Type (Env, Entity (Type_Node));

         --  Some types can be anonymous (so they don't have any defining
         --  identifier).

         when N_Access_Definition =>
            return Create_Access_Type
              (Env, Subtype_Mark (Type_Node));

         when N_Access_To_Object_Definition =>
            return Create_Access_Type
              (Env, Subtype_Indication (Type_Node));

         when N_Expanded_Name =>
            return Create_Type (Env, Associated_Node (Type_Node));

         when others =>
            raise Program_Error with "Unhandled type node kind: "
              & Node_Kind'Image (Nkind (Type_Node));
      end case;

      --  If this type is unknown, build it from its entity information

      --  Always get the fullest view

      Def_Ident := Get_Fullest_View (Type_Node);

      --  The full view may already be in the environment

      if Env.Has_Type (Def_Ident) then
         return Env.Get (Def_Ident);
      end if;

      case Ekind (Def_Ident) is

         when Discrete_Kind =>

            if Is_Modular_Integer_Type (Def_Ident) then
               return Int_Type_In_Context
                 (Env.Ctx,
                  Interfaces.C.unsigned (UI_To_Int (RM_Size (Def_Ident))));
            end if;

            return Int_Type_In_Context
              (Env.Ctx, Interfaces.C.unsigned (UI_To_Int (Esize (Def_Ident))));

         when E_Access_Type .. E_General_Access_Type =>
            return Create_Access_Type
              (Env, Designated_Type (Def_Ident));

         when Record_Kind =>
            declare
               function Rec_Comp_Filter (E : Entity_Id) return Boolean
               is (Ekind (E) in E_Component | E_Discriminant);

               function Iterate is new Iterate_Entities
                 (Get_First => First_Entity,
                  Get_Next  => Next_Entity,
                  Filter    => Rec_Comp_Filter);

               Struct_Type   : Type_T;
               Comps         : constant Entity_Iterator := Iterate (Def_Ident);
               LLVM_Comps    : array (1 .. Comps'Length) of Type_T;
               I             : Natural := 1;
               Struct_Num    : Nat := 1;
               Num_Fields    : Natural := 1;
               Info          : Record_Info;
               use Interfaces.C;
            begin
               Struct_Type := Struct_Create_Named
                 (Env.Ctx, Get_Name (Def_Ident));
               Info.Structs.Append (Struct_Type);

               --  Records enable some "type recursivity", so store this one in
               --  the environment so that there is no infinite recursion when
               --  nested components reference it.

               Env.Set (Def_Ident, Struct_Type);

               for Comp of Comps loop
                  LLVM_Comps (I) := Create_Type (Env, Etype (Comp));
                  Info.Fields.Include (Comp, (Struct_Num, Nat (I - 1)));
                  I := I + 1;
                  Num_Fields := Num_Fields + 1;

                  --  If we are on a component which sizes depends on a
                  --  discriminant, we create a new struct type for the
                  --  following components.

                  if Size_Depends_On_Discriminant (Etype (Comp)) then
                     Struct_Set_Body
                       (Struct_Type, LLVM_Comps'Address,
                        unsigned (I - 1), False);
                     I := 1;
                     Struct_Num := Struct_Num + 1;

                     --  Only create a new struct if we have remaining fields
                     --  after this one

                     if Num_Fields < Comps'Length then
                        Struct_Type := Struct_Create_Named
                          (Env.Ctx, Get_Name (Def_Ident) & Struct_Num'Img);
                        Info.Structs.Append (Struct_Type);
                     end if;
                  end if;
               end loop;

               --  If there are components remaining, set them to be the
               --  current struct body

               if I > 1 then
                  Struct_Set_Body
                    (Struct_Type, LLVM_Comps'Address, unsigned (I - 1), False);
               end if;

               Env.Set (Def_Ident, Info);
               return Env.Get (Def_Ident);
            end;

         when Array_Kind =>
            declare
               Result     : Type_T :=
                 Create_Type (Env, Component_Type (Def_Ident));
               LB, HB     : Node_Id;
               Range_Size : Long_Long_Integer := 0;

               function Iterate is new Iterate_Entities
                 (Get_First => First_Index,
                  Get_Next  => Next_Index);
            begin
               --  Wrap each "nested type" into an array using the previous
               --  index.

               for Index of reverse Iterate (Def_Ident) loop
                  declare
                     --  Sometimes, the frontends leaves an identifier that
                     --  references an integer subtype instead of a range.

                     Idx_Range : constant Node_Id := Get_Dim_Range (Index);

                  begin
                     LB := Low_Bound (Idx_Range);
                     HB := High_Bound (Idx_Range);
                  end;

                  --  Compute the size of this range if possible, otherwise
                  --  keep 0 for "unknown".

                  if Is_Constrained (Type_Node)
                    and then Compile_Time_Known_Value (LB)
                    and then Compile_Time_Known_Value (HB)
                  then
                     Range_Size := Long_Long_Integer
                       (UI_To_Long_Long_Integer (Expr_Value (HB))
                        - UI_To_Long_Long_Integer (Expr_Value (LB)) + 1);
                  end if;

                  Result := Array_Type
                    (Result, Interfaces.C.unsigned (Range_Size));
               end loop;
               return Result;
            end;

         when others =>
            raise Program_Error
              with "Unhandled type kind: "
              & Entity_Kind'Image (Ekind (Def_Ident));
      end case;
   end Create_Type;

   procedure Create_Discrete_Type
     (Env       : Environ;
      TE        : Entity_Id;
      TL        : out Type_T;
      Low, High : out Value_T) is
      SRange : Node_Id;
   begin
      --  Delegate LLVM Type creation to Create_Type

      TL := Create_Type (Env, TE);

      --  Compute ourselves the bounds

      case Ekind (TE) is
         when E_Enumeration_Type | E_Enumeration_Subtype
            | E_Signed_Integer_Type | E_Signed_Integer_Subtype
            | E_Modular_Integer_Type | E_Modular_Integer_Subtype =>

            SRange := Scalar_Range (TE);
            case Nkind (SRange) is
               when N_Range =>
                  Low := GNATLLVM.Compile.Emit_Expression
                    (Env, Low_Bound (SRange));
                  High := GNATLLVM.Compile.Emit_Expression
                    (Env, High_Bound (SRange));
               when others => raise Program_Error
                    with "Invalid scalar range: "
                    & Node_Kind'Image (Nkind (SRange));
            end case;

         when others =>
            raise Program_Error
              with "Invalid discrete type: " & Entity_Kind'Image (Ekind (TE));
      end case;
   end Create_Discrete_Type;

end GNATLLVM.Types;
