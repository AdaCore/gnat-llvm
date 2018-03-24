------------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Errout;   use Errout;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Uintp;    use Uintp;
with Ttypes;

with LLVM.Target; use LLVM.Target;

with GNATLLVM.Arrays; use GNATLLVM.Arrays;
with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;
with GNATLLVM.Compile;

package body GNATLLVM.Types is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Create_Subprogram_Type
     (Env           : Environ;
      Params        : Entity_Iterator;
      Return_Type   : Entity_Id;
      Takes_S_Link  : Boolean) return Type_T
     with Pre  => Env /= null,
          Post => Create_Subprogram_Type'Result /= No_Type_T;
   --  Helper for public Create_Subprogram_Type functions: the public ones
   --  harmonize input and this one actually creates the LLVM type for
   --  subprograms.  Return_Type can be Empty if this is a procedure.

   function Create_Subprogram_Access_Type
     (Env       : Environ;
      Subp_Type : Type_T) return Type_T
     with Pre  => Env /= null and then Subp_Type /= No_Type_T,
          Post => Create_Subprogram_Access_Type'Result /= No_Type_T;
   --  Return a structure type that embeds Subp_Type and a static link pointer

   function Dynamic_Size_Array (T : Entity_Id) return Boolean
     with Pre => Is_Type (T);
   --  Return True if T denotees an array with a dynamic size

   function Rec_Comp_Filter (E : Entity_Id) return Boolean is
     (Ekind (E) in E_Component | E_Discriminant);

   function Iterate_Components is new Iterate_Entities
     (Get_First => First_Entity,
      Get_Next  => Next_Entity,
      Filter    => Rec_Comp_Filter);
   --  Iterate over all components of a given record type

   ----------------------
   -- Get_Address_Type --
   ----------------------

   function Get_Address_Type return Type_T is
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

   function Int_Ty (Num_Bits : Natural) return Type_T is
      (Int_Type (Interfaces.C.unsigned (Num_Bits)));

   -----------
   -- Fn_Ty --
   -----------

   function Fn_Ty (Param_Ty : Type_Array; Ret_Ty : Type_T) return Type_T is
     (Function_Type
       (Ret_Ty, Param_Ty'Address, Param_Ty'Length, False));

   ------------------------
   -- Create_Access_Type --
   ------------------------

   function Create_Access_Type
     (Env : Environ; TE : Entity_Id) return Type_T
   is
      T : constant Type_T := Create_Type (Env, TE);
   begin
      if Get_Type_Kind (T) = Array_Type_Kind
        and then not Is_Constrained (TE)
      then
         return Create_Array_Fat_Pointer_Type (Env, TE);
      else
         return Pointer_Type (T, 0);
      end if;
   end Create_Access_Type;

   -----------------------
   -- GNAT_To_LLVM_Type --
   -----------------------

   function GNAT_To_LLVM_Type
     (Env : Environ; TE : Entity_Id; Definition : Boolean) return Type_T
   is
      Def_Ident : Entity_Id;
      Typ       : Type_T := No_Type_T;
      TBAA      : Metadata_T := No_Metadata_T;
      Discard   : Type_T;
      pragma Unreferenced (Discard);
      pragma Unreferenced (Definition);
   begin
      --  See if we already have a type.  If so, we must not be defining
      --  this type.  ??? But we can't add that test just yet.

      Typ := Get_Type (Env, TE);
      if Typ /= No_Type_T then
         --  pragma Assert (not Definition);
         return Typ;
      end if;

      --  See if we can get the type from the fullest view.
      --  ??? This isn't quite right in the case where we're not
      --  defining the type, or where there's a Freeze_Node, but add this
      --  logic later.
      Def_Ident := Get_Fullest_View (TE);
      if Def_Ident /= TE then
         Typ := GNAT_To_LLVM_Type (Env, Def_Ident, False);
         if Typ /= No_Type_T then
            Copy_Type_Info (Env, Def_Ident, TE);
            return Typ;
         end if;
      end if;

      --  ??? This probably needs to be cleaned up, but before we do anything,
      --  see if this isn't a base type and process that if so.
      if Base_Type (Def_Ident) /= Def_Ident then
         Discard := GNAT_To_LLVM_Type (Env, Base_Type (Def_Ident), False);
      end if;

      case Ekind (Def_Ident) is
         when Discrete_Kind =>
            --  LLVM is expecting boolean expressions to be of size 1
            --  ??? will not work properly if there is a size clause

            if Is_Boolean_Type (Def_Ident) then
               Typ := Int_Type_In_Context (Env.Ctx, 1);
            elsif Is_Modular_Integer_Type (Def_Ident) then
               Typ := Int_Type_In_Context
                 (Env.Ctx,
                  Interfaces.C.unsigned
                    (UI_To_Int (RM_Size (Def_Ident))));

            else
               Typ := Int_Type_In_Context
                 (Env.Ctx,
                  Interfaces.C.unsigned
                    (UI_To_Int (Esize (Def_Ident))));
            end if;

         when E_Floating_Point_Type | E_Floating_Point_Subtype =>
            declare
               Float_Type : constant Node_Id := Etype (Etype (Def_Ident));
               --  Use Full_Type as in gnat2il-gnat_utils.adb???

               Size       : constant Uint := Esize (Float_Type);

            begin
               case Float_Rep (Float_Type) is
                  when IEEE_Binary =>
                     if Size = Uint_32 then
                        Typ := Float_Type_In_Context (Env.Ctx);
                     elsif Size = Uint_64 then
                        Typ := Double_Type_In_Context (Env.Ctx);
                     elsif Size = Uint_128 then
                        --  Extended precision; not IEEE_128
                        Typ := X86_F_P80_Type_In_Context (Env.Ctx);
                     else
                        pragma Assert (UI_Is_In_Int_Range (Size));

                        case UI_To_Int (Size) is
                           when 80 | 96 =>
                              Typ := X86_F_P80_Type_In_Context (Env.Ctx);
                           when others =>
                              --  ??? Double check that
                              Typ := F_P128_Type_In_Context (Env.Ctx);
                        end case;
                     end if;

                  when AAMP =>
                     --  Not supported
                     Error_Msg_N ("unsupported floating point type", TE);
                     Typ := Void_Type_In_Context (Env.Ctx);
               end case;
            end;

         when E_Access_Type .. E_General_Access_Type
            | E_Anonymous_Access_Type
            | E_Access_Subprogram_Type =>
            Typ := Create_Access_Type
              (Env, Designated_Type (Def_Ident));

         when E_Anonymous_Access_Subprogram_Type =>
            Typ := Create_Subprogram_Access_Type
              (Env, Create_Type (Env, Designated_Type (Def_Ident)));

         when Record_Kind =>
            declare
               Struct_Type   : Type_T;
               Comps         : constant Entity_Iterator :=
                 Iterate_Components (Def_Ident);
               LLVM_Comps    : array (1 .. Comps'Length) of Type_T;
               I             : Natural := 1;
               Struct_Num    : Nat := 1;
               Num_Fields    : Natural := 0;
               Info          : Record_Info;
               Fields        : Field_Info_Vectors.Vector;
               Current_Field : Field_Info;
               use Interfaces.C;

               function New_Struct_Info return Struct_Info is
                 ((LLVM_Type => Struct_Type, Preceding_Fields => Fields));

            begin
               Struct_Type := Struct_Create_Named
                 (Env.Ctx, Get_Name (Def_Ident));
               Info.Structs.Append (New_Struct_Info);

               --  Records enable some "type recursivity", so store this one in
               --  the environment so that there is no infinite recursion when
               --  nested components reference it.

               Set_Type (Env, Def_Ident, Struct_Type);

               for Comp of Comps loop
                  LLVM_Comps (I) := Create_Type (Env, Etype (Comp));
                  Current_Field :=
                    (Struct_Num, Nat (I - 1), Comp, LLVM_Comps (I));
                  Fields.Append (Current_Field);
                  Info.Fields.Include (Comp, Current_Field);
                  I := I + 1;
                  Num_Fields := Num_Fields + 1;

                  --  If we are on a component with a dynamic size,
                  --  we create a new struct type for the following components.

                  if Dynamic_Size_Array (Etype (Comp)) then
                     Info.Dynamic_Size := True;
                     Struct_Set_Body
                       (Struct_Type, LLVM_Comps'Address,
                        unsigned (I - 1), False);
                     I := 1;
                     Struct_Num := Struct_Num + 1;

                     Struct_Type := Struct_Create_Named
                       (Env.Ctx, Get_Name (Def_Ident) & Img (Struct_Num));
                     Info.Structs.Append (New_Struct_Info);
                  end if;
               end loop;

               Struct_Set_Body
                 (Struct_Type, LLVM_Comps'Address, unsigned (I - 1), False);
               Set_Record_Info (Env, Def_Ident, Info);
               Typ := Get_Type (Env, Def_Ident);
            end;

         when Array_Kind =>
            --  Handle packed arrays.
            if Present (Packed_Array_Impl_Type (Def_Ident)) then
               Typ := Create_Type (Env, Packed_Array_Impl_Type (Def_Ident));
            else
               Typ := Create_Array_Type (Env, Def_Ident);
            end if;

         when E_Subprogram_Type =>
            --  An anonymous access to a subprogram can point to any subprogram
            --  (nested or not), so it must accept a static link.

            Typ := Create_Subprogram_Type_From_Entity
              (Env, Def_Ident,
               Takes_S_Link => (Nkind (Associated_Node_For_Itype (TE))
                                  /= N_Full_Type_Declaration));

         when Fixed_Point_Kind =>
            Typ := Int_Type_In_Context
              (Env.Ctx, Interfaces.C.unsigned (UI_To_Int (Esize (Def_Ident))));

         when E_Incomplete_Type =>
            --  This is a taft amendment type, return a dummy type

            Typ := Void_Type_In_Context (Env.Ctx);

         when E_Private_Type
            | E_Private_Subtype
            | E_Limited_Private_Type
            | E_Limited_Private_Subtype
         =>
            Typ := Create_Type (Env, Etype (Def_Ident));

         when others =>
            Error_Msg_N
              ("unsupported type kind: `"
               & Ekind (Def_Ident)'Image & "`", Def_Ident);
            raise Program_Error;
      end case;

      --  Now save the result, if we have one, and compute any TBAA
      --  information.
      if Typ /= No_Type_T then
         Set_Type (Env, TE, Typ);
         TBAA := Create_TBAA (Env, TE);
         if TBAA /= No_Metadata_T then
            Set_TBAA (Env, TE, TBAA);
         end if;
      end if;

      return Typ;
   end GNAT_To_LLVM_Type;

   -----------------
   -- Create_TBAA --
   -----------------

   function Create_TBAA (Env : Environ; TE : Entity_Id) return Metadata_T is
   begin
      if Ekind (TE) in E_Signed_Integer_Type  |
                       E_Modular_Integer_Type |
                       E_Floating_Point_Type
      then
         return Create_TBAA_Scalar_Type_Node
           (Env.MDBld, Get_Name (TE), Env.TBAA_Root);
      else
         return No_Metadata_T;
      end if;
   end Create_TBAA;

   --------------------------
   -- Create_Discrete_Type --
   --------------------------

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
               when others =>
                  pragma Annotate (Xcov, Exempt_On, "Defensive programming");
                  raise Program_Error
                    with "Invalid scalar range: "
                    & Node_Kind'Image (Nkind (SRange));
                  pragma Annotate (Xcov, Exempt_Off);
            end case;

         when others =>
            pragma Annotate (Xcov, Exempt_On, "Defensive programming");
            raise Program_Error
              with "Invalid discrete type: " & Entity_Kind'Image (Ekind (TE));
            pragma Annotate (Xcov, Exempt_Off);
      end case;
   end Create_Discrete_Type;

   --------------------------------------
   -- Create_Subprogram_Type_From_Spec --
   --------------------------------------

   function Create_Subprogram_Type_From_Spec
     (Env : Environ; Subp_Spec : Node_Id) return Type_T
   is
      Def_Ident : constant Entity_Id := Defining_Entity (Subp_Spec);
      Params    : constant Entity_Iterator := Get_Params (Def_Ident);
      Result    : Node_Id := Empty;

   begin
      case Nkind (Subp_Spec) is
         when N_Procedure_Specification =>
            null;
         when N_Function_Specification =>
            Result := Entity (Result_Definition (Subp_Spec));
         when others =>
            Error_Msg_N
              ("invalid node kind: `" & Node_Kind'Image (Nkind (Subp_Spec)),
               Subp_Spec);
            raise Program_Error;
      end case;

      return Create_Subprogram_Type (Env, Params, Result, False);
   end Create_Subprogram_Type_From_Spec;

   ----------------------------------------
   -- Create_Subprogram_Type_From_Entity --
   ----------------------------------------

   function Create_Subprogram_Type_From_Entity
     (Env           : Environ;
      Subp_Type_Ent : Entity_Id;
      Takes_S_Link  : Boolean) return Type_T is
   begin
      return Create_Subprogram_Type
        (Env,
         Get_Params (Subp_Type_Ent),
         (if Etype (Subp_Type_Ent) = Standard_Void_Type
          then Empty
          else Etype (Subp_Type_Ent)),
         Takes_S_Link);
   end Create_Subprogram_Type_From_Entity;

   ----------------------------
   -- Create_Subprogram_Type --
   ----------------------------

   function Create_Subprogram_Type
     (Env           : Environ;
      Params        : Entity_Iterator;
      Return_Type   : Entity_Id;
      Takes_S_Link  : Boolean) return Type_T
   is
      Args_Count : constant Int :=
        Params'Length + (if Takes_S_Link then 1 else 0);
      Arg_Types  : Type_Array (1 .. Args_Count);

   begin
      --  First, Associate an LLVM type for each Ada subprogram parameter

      for J in Params'Range loop
         declare
            Param_Ent  : constant Entity_Id := Params (J);
            Param_Type : constant Node_Id := Etype (Param_Ent);
         begin
            --  If this is an out parameter, or a parameter whose type is
            --  unconstrained, take a pointer to the actual parameter.

            Arg_Types (J) :=
              (if Param_Needs_Ptr (Param_Ent)
               then Create_Access_Type (Env, Param_Type)
               else Create_Type (Env, Param_Type));
         end;
      end loop;

      --  Set the argument for the static link, if any

      if Takes_S_Link then
         Arg_Types (Arg_Types'Last) :=
           Pointer_Type (Int8_Type_In_Context (Env.Ctx), 0);
      end if;

      return Fn_Ty
        (Arg_Types,
         (if Present (Return_Type)
          then Create_Type (Env, Return_Type)
          else Void_Type_In_Context (Env.Ctx)));
   end Create_Subprogram_Type;

   -----------------------------------
   -- Create_Subprogram_Access_Type --
   -----------------------------------

   function Create_Subprogram_Access_Type
     (Env       : Environ;
      Subp_Type : Type_T) return Type_T
   is
      pragma Unreferenced (Subp_Type);

      Void_Ptr : constant Type_T :=
        Pointer_Type (Int8_Type_In_Context (Env.Ctx), 0);
      Couple : constant Type_Array (1 .. 2) := (Void_Ptr, Void_Ptr);

   begin
      return Struct_Type_In_Context
        (Env.Ctx,
         Couple'Address, Couple'Length,
         Packed => False);
   end Create_Subprogram_Access_Type;

   ------------------------
   -- Get_Type_Alignment --
   ------------------------

   function Get_Type_Alignment
     (Env : Environ;
      T   : Type_T) return Interfaces.C.unsigned
   is
   begin
      return ABI_Alignment_Of_Type (Env.Module_Data_Layout, T);
   end Get_Type_Alignment;

   -------------------
   -- Get_Type_Size --
   -------------------

   function Get_Type_Size
     (Env : Environ;
      T   : Type_T) return Value_T
   is
   begin
      return Const_Int
        (Int_Ptr_Type,
         Size_Of_Type_In_Bits (Env.Module_Data_Layout, T) / 8,
         Sign_Extend => False);
   end Get_Type_Size;

   function Get_Type_Size_In_Bits
     (Env : Environ;
      T   : Type_T) return unsigned_long_long
   is
   begin
      return Size_Of_Type_In_Bits (Env.Module_Data_Layout, T);
   end Get_Type_Size_In_Bits;

   --------------------
   -- Emit_Type_Size --
   --------------------

   function Emit_Type_Size
     (Env                   : Environ;
      T                     : Entity_Id;
      Array_Descr           : Value_T;
      Containing_Record_Ptr : Value_T) return Value_T
   is
      LLVM_Type      : constant Type_T := Create_Type (Env, T);
      Size           : Value_T;
      Dynamic_Fields : Boolean := False;

   begin
      if Is_Scalar_Type (T)
        or else Is_Access_Type (T)
      then
         return Get_Type_Size (Env, LLVM_Type);
      elsif Is_Array_Type (T) then
         if Esize (Component_Type (T)) = Uint_1 then
            return Array_Size (Env, Array_Descr, T, Containing_Record_Ptr);
         else
            return Mul
              (Env.Bld,
               Emit_Type_Size
                 (Env, Component_Type (T), No_Value_T, Containing_Record_Ptr),
               Array_Size
                 (Env, Array_Descr, T, Containing_Record_Ptr),
               "array-size");
         end if;
      elsif Is_Record_Type (T) then
         Size := Get_Type_Size (Env, LLVM_Type);

         if Record_With_Dynamic_Size (Env, T) then
            for Comp of Iterate_Components (T) loop
               if Dynamic_Size_Array (Etype (Comp)) then
                  Dynamic_Fields := True;
               end if;

               --  Compute size of all fields once we've found a dynamic
               --  component.

               if Dynamic_Fields then
                  Size := Add
                    (Env.Bld,
                     Size,
                     Emit_Type_Size
                       (Env,
                        Etype (Comp),
                        No_Value_T,
                        No_Value_T),
                     "record-size");
               end if;
            end loop;
         end if;

         return Size;
      else
         Error_Msg_N ("unimplemented case for emit type size", T);
         raise Program_Error;
      end if;
   end Emit_Type_Size;

   -------------------------
   -- Record_Field_Offset --
   -------------------------

   function Record_Field_Offset
     (Env          : Environ;
      Record_Ptr   : Value_T;
      Record_Field : Node_Id) return Value_T
   is
      use Interfaces.C;

      Type_Id    : constant Entity_Id :=
        Get_Fullest_View (Scope (Record_Field));
      R_Info     : constant Record_Info := Get_Record_Info (Env, Type_Id);
      F_Info     : constant Field_Info := R_Info.Fields.Element (Record_Field);
      Struct_Ptr : Value_T := Record_Ptr;

   begin
      if F_Info.Containing_Struct_Index > 1 then
         declare
            Int_Struct_Address : Value_T := Ptr_To_Int
              (Env.Bld,
               Record_Ptr, Int_Ptr_Type, "offset-calc");
            S_Info : constant Struct_Info :=
              R_Info.Structs (F_Info.Containing_Struct_Index);

         begin
            --  Accumulate the size of every field
            for Preceding_Field of S_Info.Preceding_Fields loop
               Int_Struct_Address := Add
                 (Env.Bld,
                  Int_Struct_Address,
                  Emit_Type_Size
                    (Env,
                     Etype (Preceding_Field.Entity),
                     No_Value_T,
                     Record_Ptr),
                  "offset-calc");
            end loop;

            Struct_Ptr := Int_To_Ptr
              (Env.Bld,
               Int_Struct_Address, Pointer_Type (S_Info.LLVM_Type, 0), "back");
         end;
      end if;

      return Struct_GEP
        (Env.Bld,
         Struct_Ptr, unsigned (F_Info.Index_In_Struct), "field_access");
   end Record_Field_Offset;

   ------------------------------
   -- Record_With_Dynamic_Size --
   ------------------------------

   function Record_With_Dynamic_Size
     (Env : Environ; T : Entity_Id) return Boolean
   is
      Full_View : constant Entity_Id := Get_Fullest_View (T);
      Unused    : Type_T;
   begin
      if Is_Record_Type (Full_View) then
         --  First ensure the type is created
         Unused := Create_Type (Env, Full_View);
         return Get_Record_Info (Env, Full_View).Dynamic_Size;
      else
         return False;
      end if;
   end Record_With_Dynamic_Size;

   ------------------------
   -- Dynamic_Size_Array --
   ------------------------

   function Dynamic_Size_Array (T : Entity_Id) return Boolean is
      E    : constant Entity_Id := Get_Fullest_View (T);
      Indx : Node_Id;
      Ityp : Entity_Id;

   begin
      if not Is_Array_Type (E) then
         return False;
      end if;

      --  Loop to process array indexes

      Indx := First_Index (E);
      while Present (Indx) loop
         Ityp := Etype (Indx);

         --  If an index of the array is a generic formal type then there is
         --  no point in determining a size for the array type.

         if Is_Generic_Type (Ityp) then
            return False;
         end if;

         if not Compile_Time_Known_Value (Type_Low_Bound (Ityp))
           or else not Compile_Time_Known_Value (Type_High_Bound (Ityp))
         then
            return True;
         end if;

         Next_Index (Indx);
      end loop;

      return False;
   end Dynamic_Size_Array;

end GNATLLVM.Types;
