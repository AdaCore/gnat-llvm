with Interfaces.C;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

with Atree;    use Atree;
with Get_Targ; use Get_Targ;
with Einfo;    use Einfo;
with Nlists;   use Nlists;
with Sinfo;    use Sinfo;
with Stand;    use Stand;

with GNATLLVM.Compile;
with GNATLLVM.Utils; use GNATLLVM.Utils;
with Uintp; use Uintp;

package body GNATLLVM.Types is

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
     (Function_Type
        (Ret_Ty, Param_Ty'Address, Param_Ty'Length, Boolean'Pos (False)));

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

      return Fn_Ty (Param_Types, Return_Type);

   end Create_Subprogram_Type;

   -----------------
   -- Create_Type --
   -----------------

   function Create_Type (Env : Environ; Type_Node : Node_Id) return Type_T is
   begin

      case Nkind (Type_Node) is
         when N_Defining_Identifier =>

            if not Env.Has_Type (Type_Node) then
               if Nkind (Parent (Type_Node)) /= N_Empty then
                  GNATLLVM.Compile.Compile (Env, Parent (Type_Node));
               else
                  return Create_Type (Env, Etype (Type_Node));
               end if;
            end if;

            return Env.Get (Type_Node);

         when N_Identifier =>

            --  If the type does not yet exist in the environnment, it may have
            --  been drawn from another file (adb's own spec file or another),
            --  so we compile the full type declaration.

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

         when N_Derived_Type_Definition =>

            --  Machine representation of a derived type is the same as its
            --  parent type

            return Create_Type
              (Env, Subtype_Mark (Subtype_Indication (Type_Node)));

         when N_Enumeration_Type_Definition =>
            declare
               Rep, Rep_Max : Natural := 0;
            begin
               --  An enumeration type is represented by an integer. Just get
               --  here the size needed to represent all possible values.

               --  Note that we do not store literals in the environment,
               --  since the environment requires l-values. Enumeration
               --  literals are a special cases for N_Identifier nodes
               --  in Compile_Expression.

               for Lit of Iterate (Literals (Type_Node)) loop
                  Rep := Natural (UI_To_Int (Enumeration_Rep (Lit)));
                  if Rep > Rep_Max then
                     Rep_Max := Rep;
                  end if;
               end loop;
               return Int_Type_In_Context
                 (Env.Ctx, Interfaces.C.unsigned (Get_Binary_Size (Rep_Max)));
            end;

         when N_Record_Definition =>
            declare
               DI : constant Node_Id :=
                 Defining_Identifier (Parent (Type_Node));

               --  When declaring a record, we want to check if the environment
               --  already contains a type for the given entity. If it does,
               --  rather than creating a new type, we'll want to alter the
               --  existing LLVM struct

               Full : constant Boolean := Env.Has_Type (DI);
               Struct_Type : constant Type_T :=
                 (if Full
                  then Env.Get (DI)
                  else Struct_Create_Named
                    (Env.Ctx,
                     Get_Name (Defining_Identifier (Parent (Type_Node)))));

               Comp_List : constant List_Id :=
                 Component_Items (Component_List (Type_Node));
               LLVM_Comps : array (1 .. List_Length (Comp_List)) of Type_T;
               I : Int := 1;
            begin

               --  Create a LLVM type recursively for every component of the
               --  record, and set the resulting array of types as the struct's
               --  body

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

         when N_Modular_Type_Definition =>
            declare
               E : constant Entity_Id :=
                 Defining_Identifier (Parent (Type_Node));
               use Interfaces.C;
            begin
               if Non_Binary_Modulus (E) then

                  --  Use the biggest integer type for non binary mod types
                  --  TODO : Use a smaller type when possible. Check what
                  --  GNATGCC does on 32 bits platforms

                  return Int64_Type;
               else

                  --  Since LLVM behavior regarding unsigned int types is what
                  --  we expect for mod types, just use a rightly sized integer
                  --  for binary mod types

                  return Int_Type
                    (unsigned (Log (Float (UI_To_Int (Modulus (E))), 2.0)));
               end if;
            end;

            pragma Warnings (Off);

         when N_Constrained_Array_Definition =>
            declare
               function Array_From_Type
                 (T : Type_T; DSD : Node_Id) return Type_T;

               function Array_From_Type
                 (T : Type_T; DSD : Node_Id) return Type_T
               is
               begin
                  case Nkind (DSD) is
                     when N_Range =>
                        declare
                           LB : Node_Id := Low_Bound (DSD);
                           HB : Node_Id := High_Bound (DSD);
                           Array_Size : Natural := 0;
                        begin

                           if
                             Is_Static_Expression (LB)
                             and then Is_Static_Expression (HB)
                           then
                              Array_Size := Natural
                                (UI_To_Int (Intval (HB)) -
                                  UI_To_Int (Intval (LB)) + 1);
                           end if;

                           return Array_Type
                             (T, Interfaces.C.unsigned (Array_Size));
                        end;

                     when others =>
                        raise Program_Error
                          with "Node not handled as DSD : " & Nkind (DSD)'Img;
                  end case;
               end Array_From_Type;

               DSD : constant List_Id :=
                 Discrete_Subtype_Definitions (Type_Node);
               El_Type : Type_T;
            begin
               El_Type := Create_Type
                 (Env, Subtype_Indication (Component_Definition (Type_Node)));

               for DSD of Iterate (Discrete_Subtype_Definitions (Type_Node))
               loop
                  El_Type := Array_From_Type (El_Type, DSD);
               end loop;

               return El_Type;
            end;

         when others =>
            raise Program_Error
              with "Unhandled node: " & Node_Kind'Image (Nkind (Type_Node));
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
                  Low := GNATLLVM.Compile.Compile_Expression
                    (Env, Low_Bound (SRange));
                  High := GNATLLVM.Compile.Compile_Expression
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

   function Get_Binary_Size (N : Natural) return Natural
   is
      Greater : Natural := 1;
      Result  : Natural := 1;
   begin
      while Greater < N loop
         Greater := 2 * Greater;
         Result := Result + 1;
      end loop;
      return Result;
   end Get_Binary_Size;

end GNATLLVM.Types;
