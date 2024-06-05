-----------------------------------------------------------------------------
--                             G N A T - L L V M                            --
--                                                                          --
--                     Copyright (C) 2013-2023, AdaCore                     --
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

with Einfo.Utils; use Einfo.Utils;
with Errout;      use Errout;
with Exp_Unst;    use Exp_Unst;
with Exp_Util;    use Exp_Util;
with Get_Targ;    use Get_Targ;
with Lib;         use Lib;
with Nlists;      use Nlists;
with Restrict;    use Restrict;
with Rident;      use Rident;
with Sem_Aux;     use Sem_Aux;
with Sem_Mech;    use Sem_Mech;
with Sem_Util;    use Sem_Util;
with Sinput;      use Sinput;
with Snames;      use Snames;
with Table;       use Table;
with Targparm;    use Targparm;

with GNATLLVM.Aliasing;     use GNATLLVM.Aliasing;
with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Blocks;       use GNATLLVM.Blocks;
with GNATLLVM.Builtins;     use GNATLLVM.Builtins;
with GNATLLVM.Codegen;      use GNATLLVM.Codegen;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Conversions;  use GNATLLVM.Conversions;
with GNATLLVM.DebugInfo;    use GNATLLVM.DebugInfo;
with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.Exprs;        use GNATLLVM.Exprs;
with GNATLLVM.GLType;       use GNATLLVM.GLType;
with GNATLLVM.Helper;       use GNATLLVM.Helper;
with GNATLLVM.Records;      use GNATLLVM.Records;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Types.Create; use GNATLLVM.Types.Create;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Variables;    use GNATLLVM.Variables;

with CCG; use CCG;

package body GNATLLVM.Subprograms is

   --  We define enum types corresponding to information about subprograms
   --  and their parameters that we use consistently within this file to
   --  process those subprograms and parameters.

   type Param_Kind is
     (PK_By_Ref_In,
      --  This parameter is passed by reference and is only read

      PK_By_Ref_Out,
      --  This parameter is passed by reference and is only written

      PK_By_Ref_In_Out,
      --  This parameter is passed by reference and is read and written

      Foreign_By_Ref,
      --  Similar to By_Reference, but for a parameter with foreign convention

      Foreign_By_Component_Ref,
      --  Similar to Foreign_By_Ref, but for an array, where we use a
      --  pointer to the component.

      Activation_Record,
      --  This parameter is a pointer to an activation record

      In_Value,
      --  This parameter is passed by value, but used only as an input

      In_Value_By_Int,
      --  Similar to In_Value, but for a parameter with foreign convention
      --  that we have to pass as an integer to avoid issues with LLVM's
      --  implementation of the x86-64 calling convention. We don't
      --  try to conditionalize this on the calling convention because
      --  in all other cases, the integer and record is passed the same.
      --  We also don't do this for large records even though there'd
      --  be a difference in the ABI. We do this compromise since this
      --  case is rare and all known instances are of small size.

      Zero_Size,
      --  This parameter is of zero-size and we're going to ignore it
      --  (only used when emitting C).

      Out_Value,
      --  This parameter is passed by value, but is only an output

      In_Out_Value);
      --  This parameter is passed by value and is both an input and output

   function PK_Is_Reference (PK : Param_Kind) return Boolean is
     (PK in PK_By_Ref_In | PK_By_Ref_Out | PK_By_Ref_In_Out | Foreign_By_Ref |
            Foreign_By_Component_Ref);
   --  True if this parameter kind represents a value passed by reference

   function PK_Is_In_Or_Ref (PK : Param_Kind) return Boolean is
     (PK not in Out_Value | Zero_Size);
   --  True if this parameter kind corresponds to an input parameter to
   --  the subprogram in the C sense.

   function PK_Is_Out (PK : Param_Kind) return Boolean is
     (PK in Out_Value | In_Out_Value);
   --  True if this parameter kind is returned from the subprogram

   function Is_Initialized
     (GT : GL_Type; Recurse : Boolean := True) return Boolean
     with Pre => Present (GT);
   --  Returns True if GT's type has a subcomponent with an implicit
   --  initial value. See RM 6.4.1(14) and RM 3.1.1. If Recurse is true,
   --  check component types as above, but otherwise just check GT itself.

   function Create_Subprogram_Type_Internal
     (E : Subprogram_Type_Or_Kind_Id) return Type_T
     with Post => Present (Create_Subprogram_Type_Internal'Result);

   function Get_Param_Kind (Param : Formal_Kind_Id) return Param_Kind;
   --  Return the parameter kind for Param

   function Get_Param_GL_Type (Param : Formal_Kind_Id) return GL_Type
     with Post => Present (Get_Param_GL_Type'Result);
   --  Return the GL_Type to use for a parameter, taking into account
   --  any initialized types used as an Out parameter.

   function Relationship_For_PK
     (PK : Param_Kind; GT : GL_Type) return GL_Relationship
     with Pre => Present (GT);
   --  Return the Relationship for a parameter of type GT and kind PK

   function Make_Trampoline
     (GT : GL_Type; Fn, Static_Link : GL_Value; N : Node_Id) return GL_Value
     with Pre  => Present (GT) and then Present (Fn)
                  and then Present (Static_Link) and then Present (N),
          Post => Present (Make_Trampoline'Result);
   --  Given the type of a function, a pointer to it, a static link and the
   --  location of the reference, make a trampoline that combines the
   --  static link and function.

   --  Elaboration entries can be either nodes to be emitted as statements
   --  or expressions to be saved.

   type Elaboration_Entry is record
      N         : Node_Id;
      --  Node to elaborate, possibly as an expression

      For_GT    : GL_Type;
      --  If Present, compute N as a value, convert it to this type, and
      --  save the result as the value corresponding to it.
   end record;
   type Elaboration_Entry_Array is array (Nat range <>) of Elaboration_Entry;

   package Elaborations is new Table.Table
     (Table_Component_Type => Elaboration_Entry,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 1024,
      Table_Increment      => 100,
      Table_Name           => "Elaborations");
   --  Table of statements part of the current elaboration procedure

   package Nested_Functions is new Table.Table
     (Table_Component_Type => N_Subprogram_Body_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Nested_Functions");
   --  Table of nested functions to elaborate

   --  Tables for recording global constructors and global destructors

   package Global_Constructors is new Table.Table
     (Table_Component_Type => Subprogram_Kind_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 5,
      Table_Name           => "Global_Constructors");

   package Global_Destructors is new Table.Table
     (Table_Component_Type => Subprogram_Kind_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 5,
      Table_Name           => "Global_Destructors");

   package Created_Subprograms is new Table.Table
     (Table_Component_Type => Subprogram_Kind_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 50,
      Table_Name           => "Created_Subprograms");

   --  We maintain a map within a subprogram for variables that need to be
   --  extracted from the activation record. The LLVM optimizer will do this
   --  for us, so we don't have to do it, but lowering the amount of work the
   --  optimizer has to do is worthwhile and this will also make the code
   --  much easier to read.

   function Hash_Entity_Id (E : Entity_Id) return Hash_Type is
      (Hash_Type'Mod (E))
      with Pre => not Is_Type (E);
   --  Convert an Entity_Id to a hash

   package Activation_Record_Map_P is new Ada.Containers.Hashed_Maps
     (Key_Type        => Object_Kind_Id,
      Element_Type    => GL_Value,
      Hash            => Hash_Entity_Id,
      Equivalent_Keys => "=");

   Activation_Var_Map : Activation_Record_Map_P.Map;

   function Get_Activation_Record_Ptr
     (V : GL_Value; E : E_Component_Id) return GL_Value
     with Pre  => Is_Reference (V) and then Is_Record_Type (Related_Type (V)),
          Post => Is_Reference (Get_Activation_Record_Ptr'Result)
                  and then Is_Record_Type (Related_Type
                                        (Get_Activation_Record_Ptr'Result));
   --  We need field E from an activation record. V is the activation record
   --  pointer passed to the current subprogram. Return a pointer to the
   --  proper activation record, which is either V or an up-level pointer.

   function Is_Binder_Elab_Proc (Name : String) return Boolean;
   --  Return True if Name is the name of the elab proc for Ada_Main

   First_Body_Elab_Idx    : Nat                    := 0;
   --  Indicates the first entry in Elaborations that represents
   --  an elab entry for the body of a package. If zero, then all entries
   --  are for the spec.

   Ada_Main_Elabb         : GL_Value               := No_GL_Value;
   Ada_Main_Elabb_Ident   : Opt_Subprogram_Kind_Id := Empty;
   --  We sometimes need an elab proc for Ada_Main and this can cause
   --  confusion with global names. So if we made it as part of the
   --  processing of a declaration, save it.

   Subprogram_Access_Type : Type_T                 := No_Type_T;
   --  A type used for a subprogram access

   ----------------------
   -- Number_In_Params --
   ----------------------

   function Number_In_Params (E : Subprogram_Type_Or_Kind_Id) return Nat is
      Param : Opt_Formal_Kind_Id := First_Formal_With_Extras (E);

   begin
      return Cnt : Nat := 0 do
         while Present (Param) loop
            if PK_Is_In_Or_Ref (Get_Param_Kind (Param)) then
               Cnt := Cnt + 1;
            end if;

            Next_Formal_With_Extras (Param);
         end loop;
      end return;
   end Number_In_Params;

   -----------------------
   -- Number_Out_Params --
   -----------------------

   function Number_Out_Params (E : Subprogram_Type_Or_Kind_Id) return Nat is
      Param : Opt_Formal_Kind_Id := First_Formal_With_Extras (E);

   begin
      return Cnt : Nat := 0 do
         while Present (Param) loop
            if PK_Is_Out (Get_Param_Kind (Param)) then
               Cnt := Cnt + 1;
            end if;

            Next_Formal_With_Extras (Param);
         end loop;
      end return;
   end Number_Out_Params;

   --------------------
   -- First_In_Param --
   --------------------

   function First_In_Param
     (E : Subprogram_Type_Or_Kind_Id) return Opt_Formal_Kind_Id
   is
   begin
      return Param : Opt_Formal_Kind_Id := First_Formal_With_Extras (E) do
         while Present (Param) loop
            exit when PK_Is_In_Or_Ref (Get_Param_Kind (Param));
            Next_Formal_With_Extras (Param);
         end loop;
      end return;
   end First_In_Param;

   -------------------
   -- Next_In_Param --
   -------------------

   function Next_In_Param (E : Formal_Kind_Id) return Opt_Formal_Kind_Id is

   begin
      return Param : Opt_Formal_Kind_Id := Next_Formal_With_Extras (E) do
         while Present (Param) loop
            exit when PK_Is_In_Or_Ref (Get_Param_Kind (Param));
            Next_Formal_With_Extras (Param);
         end loop;
      end return;
   end Next_In_Param;

   -------------------
   -- Next_In_Param --
   -------------------

   procedure Next_In_Param (E : in out Opt_Formal_Kind_Id) is
   begin
      E := Next_In_Param (E);
   end Next_In_Param;

   ---------------------
   -- First_Out_Param --
   ---------------------

   function First_Out_Param
     (E : Subprogram_Type_Or_Kind_Id) return Opt_Formal_Kind_Id
   is
   begin
      return Param : Opt_Formal_Kind_Id := First_Formal_With_Extras (E) do
         while Present (Param) loop
            exit when PK_Is_Out (Get_Param_Kind (Param));
            Next_Formal_With_Extras (Param);
         end loop;
      end return;
   end First_Out_Param;

   --------------------
   -- Next_Out_Param --
   --------------------

   function Next_Out_Param (E : Formal_Kind_Id) return Opt_Formal_Kind_Id is
   begin
      return Param : Opt_Formal_Kind_Id := Next_Formal_With_Extras (E) do
         while Present (Param) loop
            exit when PK_Is_Out (Get_Param_Kind (Param));
            Next_Formal_With_Extras (Param);
         end loop;
      end return;
   end Next_Out_Param;

   --------------------
   -- Next_Out_Param --
   --------------------

   procedure Next_Out_Param (E : in out Opt_Formal_Kind_Id) is
   begin
      E := Next_Out_Param (E);
   end Next_Out_Param;

   -------------------------
   --  Param_Is_Reference --
   -------------------------

   function Param_Is_Reference (E : Formal_Kind_Id) return Boolean is
     (PK_Is_Reference (Get_Param_Kind (E)));

   ---------------------------
   -- Get_Param_By_Ref_Mech --
   ---------------------------

   function Get_Param_By_Ref_Mech (GT : GL_Type) return Param_By_Ref_Mech is
      Ptr_Size : constant ULL := Get_Type_Size (Void_Ptr_T);

   begin
      if Is_By_Reference_Type (GT) or else Is_Nonnative_Type (GT) then
         return Must;
      elsif not Decls_Only and then Get_Type_Size (Type_Of (GT)) > 2 * Ptr_Size
      then
         return Default_By_Ref;
      else
         return Default_By_Copy;
      end if;
   end Get_Param_By_Ref_Mech;

   --------------------
   -- Is_Initialized --
   --------------------

   function Is_Initialized
     (GT : GL_Type; Recurse : Boolean := True) return Boolean
   is
      F : Opt_Record_Field_Kind_Id;
   begin
      if Is_Access_Type (GT)
        or else (Is_Scalar_Type (GT)
                   and then Present (Default_Aspect_Value (GT)))
        or else (Has_Discriminants (GT)
                   and not Is_Unchecked_Union (GT))
      then
         return True;

      --  We have to be careful in how we test for array types. This could
      --  be a normal array type or it could be a packed array
      --  implementation type. Check for both separately since both are
      --  arrays.

      elsif Is_Array_Type (GT) then
         return Recurse and then Is_Initialized (Full_Component_GL_Type (GT));

      elsif Recurse and then Is_Packed_Array_Impl_Type (GT) then
         return Is_Initialized (Default_GL_Type (Full_Component_Type
                                                   (Full_Original_Array_Type
                                                      (Full_Etype (GT)))));

      elsif Is_Record_Type (GT) then
         F := First_Component_Or_Discriminant (Full_Base_Type (GT));
         while Present (F) loop
            exit when Present (Parent (F))
              and then Present (Expression (Parent (F)));
            exit when Recurse and then Is_Initialized (Default_GL_Type
                                                         (Full_Base_Type
                                                            (Full_Etype (F))));
            Next_Component_Or_Discriminant (F);
         end loop;

         return Present (F);
      else
         return False;
      end if;
   end Is_Initialized;

   -----------------------
   -- Get_Param_GL_Type --
   -----------------------

   function Get_Param_GL_Type (Param : Formal_Kind_Id) return GL_Type is
      GT : constant GL_Type := Full_GL_Type (Param);

   begin
      return (if   Ekind (Param) = E_Out_Parameter
                   and then Is_Initialized (GT, Recurse => False)
              then Base_GL_Type (GT) else GT);
   end Get_Param_GL_Type;

   --------------------
   -- Get_Param_Kind --
   --------------------

   function Get_Param_Kind (Param : Formal_Kind_Id) return Param_Kind is
      GT           : constant GL_Type                    :=
        Get_Param_GL_Type (Param);
      T            : constant Type_T                     := Type_Of (GT);
      Size         : constant ULL                        :=
        (if   Decls_Only or else Is_Nonnative_Type (GT)
         then ULL (Get_Bits_Per_Word) * 2 else Get_Type_Size (T));
      Subp         : constant Subprogram_Type_Or_Kind_Id := Scope (Param);
      By_Ref_Mech  : constant Param_By_Ref_Mech          :=
          Get_Param_By_Ref_Mech (GT);
      Foreign      : constant Boolean                    :=
        Has_Foreign_Convention (Param) or else Has_Foreign_Convention (Subp);
      Param_Mode   : Entity_Kind                         := Ekind (Param);
      Mech         : Int                                 := Mechanism (Param);
      By_Copy_Kind : Param_Kind;
      By_Ref_Kind  : Param_Kind;

   begin
      --  If we're emitting C and this parameter has zero size, omit it

      if Emit_C and then Is_Zero_Size (GT) then
         return Zero_Size;

      --  If this is the first parameter of a Valued Procedure, it needs to be
      --  an out parameter and is treated as an out parameter passed by value.

      elsif Param = First_Formal_With_Extras (Subp)
        and then Ekind (Subp) = E_Procedure and then Is_Valued_Procedure (Subp)
      then
         return Out_Value;

      --  There are some case where an out parameter needs to be
      --  viewed as in out. These are detailed at 6.4.1(12).

      elsif Param_Mode = E_Out_Parameter and then Is_Initialized (GT) then
         Param_Mode := E_In_Out_Parameter;
      end if;

      --  If the Mechanism is a positive number, whether it's By_Copy or
      --  By_Reference depends on the size of the type.

      if Mech > 0 then
         if Is_Nonnative_Type (GT) then
            Mech := By_Reference;
         elsif Foreign and then C_Pass_By_Copy (GT) then
            Mech := By_Copy;
         else
            Mech := (if   +Get_Type_Size (GT) > LLI (Mech)
                     then By_Reference else By_Copy);
         end if;
      elsif Mech not in Default_Mechanism | By_Reference | By_Copy then
         Error_Msg_N ("unsupported mechanism for&", Param);
      end if;

      --  Set the return value if this ends up being by-copy or by-ref

      By_Copy_Kind := (if    Param_Mode = E_In_Parameter  then In_Value
                       elsif Param_Mode = E_Out_Parameter then Out_Value
                       else  In_Out_Value);
      By_Ref_Kind :=  (if    Param_Mode = E_In_Parameter  then PK_By_Ref_In
                       elsif Param_Mode = E_Out_Parameter then PK_By_Ref_Out
                       else  PK_By_Ref_In_Out);

      --  Handle the easy case of an activation record

      if Param_Mode = E_In_Parameter
        and then Is_Activation_Record (Param)
      then
         return Activation_Record;

      --  For foreign convention, the only time we pass by value is an
      --  elementary type that's an In parameter and Mechanism isn't
      --  By_Reference or any type with a Mechanism of By_Copy. Handle them.

      elsif Foreign then
         return (if   Param_Mode = E_In_Parameter and then By_Ref_Mech /= Must
                      and then (Mech = By_Copy
                                  or else (Is_Elementary_Type (GT)
                                               and then Mech /= By_Reference))
                 then (if   Is_Record_Type (GT)
                            and then Size <= ULL (Get_Bits_Per_Word)
                       then In_Value_By_Int else In_Value)
                 elsif Is_Array_Type (GT) then Foreign_By_Component_Ref
                 else  Foreign_By_Ref);

      --  Force by-reference and dynamic-sized types to be passed by reference

      elsif By_Ref_Mech = Must then
         return By_Ref_Kind;

      --  If the mechanism is specified, use it

      elsif Mech = By_Reference then
         return By_Ref_Kind;

      elsif Mech = By_Copy then
         return By_Copy_Kind;

      --  For the default case, return by reference if it's larger than
      --  two pointers.

      else
         return (if   By_Ref_Mech = Default_By_Ref then By_Ref_Kind
                 else By_Copy_Kind);
      end if;
   end Get_Param_Kind;

   ---------------------
   -- Get_Return_Kind --
   ---------------------

   function Get_Return_Kind (E : Subprogram_Type_Or_Kind_Id) return Return_Kind
   is
      TE       : constant Void_Or_Type_Kind_Id := Etype (E);
      GT       : constant GL_Type              := Full_GL_Type (E);
      T        : constant Type_T               :=
        (if Ekind (GT) /= E_Void then Type_Of (GT) else No_Type_T);
      Ptr_Size : constant ULL                  := Get_Type_Size (Void_Ptr_T);

   begin
      --  If there's no return type, that's simple. If we're emitting C
      --  and the return kind is a zero-sized type, we don't have a return
      --  either.

      if Ekind (GT) = E_Void or else (Emit_C and then Is_Zero_Size (GT)) then
         return None;

      --  Otherwise, we return by reference if we're required to or if we
      --  need the secondary stack. We need to use the GNAT type of E
      --  for this and not the GL_Type, as often would, because we care
      --  about properties of the original, not implementation, type,
      --  such as if it's an E_Class_Wide_Type.

      elsif Returns_By_Ref (E) or else Needs_Secondary_Stack (TE)
        or else Is_Secondary_Stack_Thunk (E)
      then
         return RK_By_Reference;

      --  Otherwise, return by parameter for by-reference type

      elsif Is_By_Reference_Type (TE) then
         return Return_By_Parameter;

      --  If this is not an unconstrained array, but is either of dynamic
      --  size or a Convention Ada subprogram with a large return, we
      --  return the value via an extra parameter.

      elsif not Is_Unconstrained_Array (GT)
        and then (Is_Nonnative_Type (GT)
                    or else (not Has_Foreign_Convention (E)
                               and then not Decls_Only
                               and then Get_Type_Size (T) > 5 * Ptr_Size))
      then
         return Return_By_Parameter;

      --  Otherwise, return by value

      else
         return Value_Return;
      end if;

   end Get_Return_Kind;

   --------------------
   -- Get_L_Ret_Kind --
   --------------------

   function Get_L_Ret_Kind (E : Subprogram_Type_Or_Kind_Id) return L_Ret_Kind
   is
      RK        : constant Return_Kind        := Get_Return_Kind  (E);
      Num_Out   : constant Nat                := Number_Out_Params (E);
      Out_Param : constant Opt_Formal_Kind_Id := First_Out_Param (E);
      Has_Ret   : constant Boolean            :=
        RK not in None | Return_By_Parameter;

   begin
      if not Has_Ret and then Num_Out = 0 then
         return Void;
      elsif Num_Out = 0 then
         return Subprog_Return;
      elsif not Has_Ret then

         --  If this has no return has one or more out parameters, we return
         --  then. If more than one or if emitting C, exactly one, and that
         --  has an array type, use a struct form.

         return (if   Num_Out = 1
                   and then not (Emit_C
                                   and then Is_Constrained_Array
                                   (Full_Etype (Out_Param)))
                 then Out_Return else Struct_Out);
      else
         return Struct_Out_Subprog;
      end if;
   end Get_L_Ret_Kind;

   ------------------------
   -- Get_Mechanism_Code --
   ------------------------

   function Get_Mechanism_Code
     (E : Subprogram_Kind_Id; Exprs : List_Id) return Uint
   is
      P_Num : Int;
      Param : Opt_Formal_Kind_Id;

   begin
      --  If there's no Exprs, then we're asking about the function return
      --  mechanism; otherwise, we're asking about a parameter.

      if No (Exprs) then
         if Get_Return_Kind (E) in RK_By_Reference | Return_By_Parameter then
            return Uint_2;
         else
            return Uint_1;
         end if;
      end if;

      P_Num := +Intval (First (Exprs));
      Param := First_Formal (E);
      for J in 2 .. P_Num loop
         Next_Formal (Param);
      end loop;

      if PK_Is_Reference (Get_Param_Kind (Param)) then
         return Uint_2;
      else
         return Uint_1;
      end if;
   end Get_Mechanism_Code;

   -------------------------
   -- Relationship_For_PK --
   -------------------------

   function Relationship_For_PK
     (PK : Param_Kind; GT : GL_Type) return GL_Relationship is

   begin
      if PK_Is_Reference (PK) then
         return (if  Is_Unconstrained_Array (GT) and then PK /= Foreign_By_Ref
                     and then PK /= Foreign_By_Component_Ref
                 then Fat_Pointer else Reference);
      else
         return Data;
      end if;
   end Relationship_For_PK;

   ----------------------------
   -- Create_Subprogram_Type --
   ----------------------------

   function Create_Subprogram_Type
     (E : Subprogram_Type_Or_Kind_Id) return Type_T
   is
      T : Type_T := Get_Subprogram_Type (E);

   begin
      if not Present (T) then
         T := Create_Subprogram_Type_Internal (E);
         Set_Subprogram_Type (E, T);
      end if;

      return T;
   end Create_Subprogram_Type;

   -------------------------------------
   -- Create_Subprogram_Type_Internal --
   -------------------------------------

   function Create_Subprogram_Type_Internal
     (E : Subprogram_Type_Or_Kind_Id) return Type_T
   is
      Return_GT       : constant GL_Type     := Full_GL_Type    (E);
      RK              : constant Return_Kind := Get_Return_Kind (E);
      LRK             : constant L_Ret_Kind  := Get_L_Ret_Kind  (E);
      Has_S_Link      : constant Boolean     := Has_Activation_Record (E);
      Foreign         : constant Boolean     := Has_Foreign_Convention (E);
      Adds_S_Link     : constant Boolean     :=
        (Is_Type (E) and then not Foreign)
        or else (not Has_S_Link and then Force_Activation_Record_Parameter
                   and then not Foreign);
      LLVM_Ret_Typ    : Type_T               :=
        (if    RK in None | Return_By_Parameter then Void_Type
         elsif RK = RK_By_Reference then Create_Access_Type_To (Return_GT)
         else  Type_Of (Return_GT));
      In_Args_Count   : constant Nat         :=
        Number_In_Params (E) + (if Adds_S_Link then 1 else 0) +
          (if RK = Return_By_Parameter then 1 else 0);
      Out_Args_Count  : constant Nat         :=
        Number_Out_Params (E) +
          (if LRK = Struct_Out_Subprog then 1 else 0);
      In_Arg_Types    : Type_Array    (1 .. In_Args_Count);
      Out_Arg_Types   : Type_Array    (1 .. Out_Args_Count);
      Out_Arg_Names   : Name_Id_Array (1 .. Out_Args_Count);
      Param_Ent       : Opt_Formal_Kind_Id   := First_Formal_With_Extras (E);
      J               : Nat                  := 1;
      LLVM_Result_Typ : Type_T               := LLVM_Ret_Typ;

   begin
      --  If the return type has dynamic size, we need to add a parameter
      --  to which we pass the address for the return to be placed in.

      if RK = Return_By_Parameter then
         In_Arg_Types (1) := Create_Access_Type_To (Return_GT);
         LLVM_Ret_Typ     := Void_Type;
         J                := 2;
      end if;

      --  Back-annotate the return mechanism

      if Ekind (E) = E_Function then
         Set_Mechanism (E, (if   RK =  Value_Return then By_Copy
                            else By_Reference));
      end if;

      --  Associate an LLVM type with each Ada subprogram parameter

      while Present (Param_Ent) loop
         declare
            GT        : constant GL_Type    := Get_Param_GL_Type (Param_Ent);
            T         : constant Type_T     := Type_Of (GT);
            PK        : constant Param_Kind := Get_Param_Kind (Param_Ent);
            PK_By_Ref : constant Boolean    := PK_Is_Reference (PK);
            Size      : constant ULL        :=
              (if   Decls_Only or else Is_Nonnative_Type (GT) then 0
               else Get_Type_Size (T));

         begin
            --  Save whether the original mechanism used was by-ref before
            --  we change it. We need this to warn on misaligned actuals.

            Set_Orig_By_Ref_Mech (Param_Ent,
                                  Mechanism (Param_Ent) = By_Reference);

            --  If the mechanism requested was by-copy and we use by-ref,
            --  give a warning. If the mechanism was defaulted, set
            --  what we used.

            if Mechanism (Param_Ent) = By_Copy and then PK_By_Ref then
               Error_Msg_N ("??cannot pass & by copy", Param_Ent);
            elsif Mechanism (Param_Ent) = Default_Mechanism then
               Set_Mechanism (Param_Ent,
                              (if PK_By_Ref then By_Reference else By_Copy));
            end if;

            --  If this is an input or reference, set the type for the
            --  param. For foreign convention, we pass small record (less
            --  than a word) as an integer to avoid issues with the x86-64
            --  ABI.

            if PK_Is_In_Or_Ref (PK) then
               In_Arg_Types (J) :=
                 (if PK = Foreign_By_Ref then
                    Pointer_Type (Type_Of (GT), Address_Space)
                  elsif PK = Foreign_By_Component_Ref then
                    Pointer_Type
                      (Type_Of (Full_Component_GL_Type (GT)),
                       Address_Space)
                  elsif PK = In_Value_By_Int then Int_Ty (Size)
                  elsif PK_By_Ref then
                    Create_Access_Type_To (GT)
                  else Type_Of (GT));
               J := J + 1;
            end if;
         end;

         Next_Formal_With_Extras (Param_Ent);
      end loop;

      --  Set the argument for the static link, if any

      if Adds_S_Link then
         In_Arg_Types (J) := Void_Ptr_T;
      end if;

      --  Now deal with the result type. We've already set if it it's
      --  simply the return type.

      case LRK is
         when Void | Subprog_Return =>
            null;

         when Out_Return =>
            LLVM_Result_Typ :=
              Type_Of (Get_Param_GL_Type (First_Out_Param (E)));

         when Struct_Out | Struct_Out_Subprog =>
            J := 1;

            if LRK = Struct_Out_Subprog then
               Out_Arg_Types (J) := LLVM_Ret_Typ;
               Out_Arg_Names (J) := Name_Find ("RESULT");
               J                 := J + 1;
            end if;

            Param_Ent := First_Out_Param (E);
            while Present (Param_Ent) loop
               Out_Arg_Types (J) := Type_Of (Get_Param_GL_Type (Param_Ent));
               Out_Arg_Names (J) := Get_Ext_Name (Param_Ent);
               J                 := J + 1;
               Next_Out_Param (Param_Ent);
            end loop;

            LLVM_Result_Typ :=
              Build_Struct_Type (Out_Arg_Types,
                                 Name        => Get_Ext_Name (E, "_RET"),
                                 Field_Names => Out_Arg_Names);
      end case;

      return Fn_Ty (In_Arg_Types, LLVM_Result_Typ);
   end Create_Subprogram_Type_Internal;

   -----------------------------------
   -- Create_Subprogram_Access_Type --
   -----------------------------------

   function Create_Subprogram_Access_Type return Type_T is
   begin
      --  It would be nice to have the field of this struct to be a pointer
      --  to the subprogram type, but it can't be because the signature of
      --  an access type doesn't include the possibility of an activation
      --  record while the actual subprogram might have one. So we use a
      --  generic pointer for it and cast at the actual call.

      if No (Subprogram_Access_Type) then
         Subprogram_Access_Type :=
           Build_Struct_Type ((1 => Void_Ptr_T, 2 => Void_Ptr_T),
                              Name => Name_Find ("SUBPROGRAM_FP"),
                              Field_Names => (1 => Name_Find ("SUBP"),
                                              2 => Name_Find ("STATIC_LINK")));
      end if;

      return Subprogram_Access_Type;
   end Create_Subprogram_Access_Type;

   -------------------------
   -- Add_Global_Function --
   -------------------------

   function Add_Global_Function
     (S          : String;
      Subp_Type  : Type_T;
      GT         : GL_Type;
      Can_Throw  : Boolean := False;
      Can_Return : Boolean := True) return GL_Value
   is
      Func : GL_Value := Get_Dup_Global_Value (S);

   begin
      --  If we've already built a function for this, return it, after
      --  being sure it's in the same type that we need.

      if Present (Func) then
         return
           G_From
             (Pointer_Cast
                (IR_Builder, +Func,
                 Pointer_Type (Subp_Type, Address_Space), S),
              Func);
      else
         Func := Add_Function (S, Subp_Type, GT, Is_Builtin => True);

         if not Can_Throw then
            Set_Does_Not_Throw (Func);
         end if;

         if not Can_Return then
            Set_Does_Not_Return (Func);
         end if;

         if Can_Throw and not Can_Return then
            Add_Cold_Attribute (Func);
         end if;

         if not DSO_Preemptable then
            Set_DSO_Local (Func);
         end if;

         Set_Dup_Global_Value (S, Func);
         return Func;
      end if;

   end Add_Global_Function;

   ---------------------
   -- Make_Trampoline --
   ---------------------

   function Make_Trampoline
     (GT : GL_Type; Fn, Static_Link : GL_Value; N : Node_Id) return GL_Value
   is
      Tramp  : constant GL_Value :=
        Array_Alloca (SSI_GL_Type, Size_Const_Int (ULL (72)),
                      Name => "TRAMP");
      Cvt_Fn : constant GL_Value := Pointer_Cast (Fn, A_Char_GL_Type);

   begin
      --  We have to initialize the trampoline and then adjust it and return
      --  that result.

      Check_Implicit_Dynamic_Code_Allowed (N);
      Call (Get_Tramp_Init_Fn, (1 => Tramp, 2 => Cvt_Fn, 3 => Static_Link));

      if Enable_Execute_Stack then
         Call (Get_Enable_Execute_Stack_Fn, (1 => Tramp));
      end if;

      return G_Is_Relationship
        (Call (Get_Tramp_Adjust_Fn, (1 => Tramp)), GT, Trampoline);

   end Make_Trampoline;

   -------------------------------
   -- Get_Activation_Record_Ptr --
   -------------------------------

   function Get_Activation_Record_Ptr
     (V : GL_Value; E : E_Component_Id) return GL_Value
   is
      Need_Type   : constant Record_Kind_Id           := Full_Scope (E);
      Have_Type   : constant Record_Kind_Id           := Full_Etype (V);
      First_Field : constant Opt_Record_Field_Kind_Id :=
        First_Component_Or_Discriminant (Have_Type);

   begin
      --  If V points to an object that E is a component of, return it.
      --  Otherwise, go up the chain by getting the first field of V's
      --  record and dereferencing it.

      if Need_Type = Have_Type then
         return V;
      else
         pragma Assert (Present (First_Field)
                          and then Is_Access_Type (Full_Etype (First_Field)));
         return Get_Activation_Record_Ptr
           (From_Access (Load (Get (Record_Field_Offset (V, First_Field),
                                    Reference))),
            E);
      end if;
   end Get_Activation_Record_Ptr;

   --------------------------------
   -- Get_From_Activation_Record --
   --------------------------------

   function Get_From_Activation_Record (E : Evaluable_Kind_Id) return GL_Value
   is
      BB : Basic_Block_T;

   begin
      --  See if this is a type of object that's passed in activation
      --  records, if this object is allocated space in an activation
      --  record, if we have an activation record as a parameter of this
      --  subprogram, this isn't a reference to the variable in its own
      --  subprogram, and if this isn't a constant. If so, get the object
      --  from the activation record. We return the address from the
      --  record so we can either give an LValue or an expression.

      if Ekind (E) in E_Constant | E_Variable | Formal_Kind | E_Loop_Parameter
        and then Present (Activation_Record_Component (E))
        and then Present (Activation_Rec_Param)
        and then Get_Value (Enclosing_Subprogram (E)) /= Current_Func
        and then not Is_No_Elab_Needed_For_Entity (E)
      then
         --  If we've already recorded this in our map, return that

         if Activation_Var_Map.Contains (E) then
            return Activation_Var_Map.Element (E);
         end if;

         --  Otherwise, do the computations to fetch it. We must do this
         --  in the entry block so that it dominates every possible use.

         BB := Get_Insert_Block;
         Set_Current_Position (Entry_Block_Allocas);

         declare
            GT             : constant GL_Type        := Full_GL_Type (E);
            Component      : constant E_Component_Id :=
              Activation_Record_Component (E);
            Activation_Rec : constant GL_Value       :=
              Get_Activation_Record_Ptr (Activation_Rec_Param, Component);
            Pointer        : constant GL_Value       :=
              Get (Record_Field_Offset (Activation_Rec, Component), Reference);
            V              : constant GL_Value       := Get_Value (E);
            Result         : GL_Value                := Load (Pointer);

         begin
            --  If GT is unconstrained, we have an access type, which is a
            --  fat pointer. Convert it to a reference to the underlying
            --  type. Otherwise, this is a System.Address which needs to
            --  be converted to a pointer.

            if Is_Unconstrained_Array (GT) then
               Result := From_Access (Result);
            else
               Result := Int_To_Ref (Result, GT);
               Set_Alignment
                 (Result,
                  (if   Relationship (Result) = Relationship (V)
                        and then Related_Type (Result) = Related_Type (V)
                   then Alignment (V) else Get_Type_Alignment (V)));
            end if;

            --  If we have the same relationship and type as that of the
            --  underlying variable, the aliasing information for our
            --  reference is the same as as if we were accessing it
            --  directly in the scope in which it's defined.

            if Relationship (Result) = Relationship (V)
              and then Related_Type (Result) = Related_Type (V)
            then
               Set_TBAA_Type   (Result, TBAA_Type   (V));
               Set_TBAA_Offset (Result, TBAA_Offset (V));
               Set_Aliases_All (Result, Aliases_All (V));
            end if;

            --  Set the name, store this in our map, save the current
            --  position, and restore the saved position.

            Set_Value_Name (Result, Get_Name (E));
            Activation_Var_Map.Insert (E, Result);
            Entry_Block_Allocas := Get_Current_Position;
            Position_Builder_At_End (BB);

            return Result;
         end;
      else
         return No_GL_Value;
      end if;
   end Get_From_Activation_Record;

   -------------------
   -- Emit_One_Body --
   -------------------

   procedure Emit_One_Body
     (N : N_Subprogram_Body_Id; For_Inline : Boolean := False)
   is
      Spec       : constant Node_Id            := Acting_Spec (N);
      Func       : constant GL_Value           := Emit_Subprogram_Decl (Spec);
      E          : constant Subprogram_Kind_Id := Defining_Entity (Spec);
      Return_GT  : constant GL_Type            := Full_GL_Type    (E);
      RK         : constant Return_Kind        := Get_Return_Kind (E);
      LRK        : constant L_Ret_Kind         := Get_L_Ret_Kind  (E);
      Param_Num  : Nat                         := 0;
      LLVM_Param : GL_Value;
      Param      : Opt_Formal_Kind_Id;

   begin
      --  If we're compiling this for inline, set the proper linkage, which
      --  depends on whether the function is marked Public or not.

      if For_Inline then
         Set_Linkage (Func,
                      (if   Is_Public (E) then Available_Externally_Linkage
                       else Internal_Linkage));

      --  If the linkage was weak external, change to weak

      elsif Get_Linkage (Func) = External_Weak_Linkage then
         Set_Linkage (Func, Weak_Any_Linkage);
      end if;

      --  Now set up to process this subprogram. The frontend takes care of
      --  generating unique names where necessary, so we can allow LLVM
      --  deduplication for the rest.

      Current_Subp := E;
      Add_Function_To_Module (Func, Allow_Deduplication => True);
      Set_Added_To_Module (E);
      Enter_Subp (Func);
      Push_Debug_Scope
        (Get_Source_File_Index (Sloc (N)),
         Create_Subprogram_Debug_Info (Func, N, E));
      Set_Debug_Pos_At_Node (N);

      --  If the return type has dynamic size, we've added a parameter
      --  that's passed the address to which we want to copy our return
      --  value.

      if RK = Return_By_Parameter then
         LLVM_Param := Get_Param (Func, Param_Num, Return_GT,
                                  (if Is_Unconstrained_Array (Return_GT)
                                   then Fat_Pointer else Reference),
                                  Is_Pristine => True);
         Initialize_Alignment (LLVM_Param);
         Initialize_TBAA      (LLVM_Param);
         Set_Value_Name       (LLVM_Param, "_return");
         Return_Address_Param := LLVM_Param;
         Param_Num := Param_Num + 1;
      end if;

      Entry_Block_Allocas := Get_Current_Position;
      Push_Block
        (At_End_Proc     => At_End_Proc (N),
         Catch_Unhandled =>
           SEH
           and then not
           (No_Exception_Handlers_Set
            or else No_Exception_Propagation_Active)
           and then Get_Value_Name (Func) = "main");
      Param := First_Formal_With_Extras (E);
      while Present (Param) loop
         declare
            type String_Access is access constant String;

            PK     : constant Param_Kind      := Get_Param_Kind    (Param);
            GT     : constant GL_Type         := Get_Param_GL_Type (Param);
            R      : constant GL_Relationship := Relationship_For_PK (PK, GT);
            V      : constant GL_Value        :=
              (if   PK_Is_In_Or_Ref (PK)
               then Get_Param (Func, Param_Num, GT, R) else No_GL_Value);
            P_Name : aliased constant String  := Get_Name (Param);
            A_Name : aliased constant String  := Get_Name (Param, ".out");
            P_Num  : Nat                      := Param_Num + 1;
            Name   : String_Access            := P_Name'Access;

         begin
            Set_Debug_Pos_At_Node (Param);

            if Present (V) then
               Set_Value_Name (V, P_Name);
               Name := A_Name'Access;
               C_Set_Entity (V, Param);
            end if;

            --  If this is an out parameter, we have to make a variable for
            --  it, possibly initialized to our parameter value if this is
            --  also an in parameter. Otherwise, we can use the parameter
            --  unchanged. If we make a variable for it, indicate that the
            --  variable is no longer a parameter.

            if PK_Is_Out (PK) then
               P_Num      := 0;
               LLVM_Param := Allocate_For_Type
                 (GT,
                  N    => Param,
                  V    => V,
                  E    => Param,
                  Name => Name.all);
               C_Set_Entity (LLVM_Param, Param);

            --  Handle the case where we have to pun the object from
            --  integer to its actual type.

            elsif PK = In_Value_By_Int then
               declare
                  Size  : constant ULL      := Get_Type_Size (Type_Of (GT));
                  T     : constant Type_T   := Int_Ty (Size);
                  Val   : GL_Value;

               begin
                  LLVM_Param := Allocate_For_Type
                    (GT,
                     N    => Param,
                     E    => Param,
                     Name => Name.all);

                  Val :=
                    Ptr_To_Relationship
                      (LLVM_Param,
                       Pointer_Type (T, Address_Space),
                       Reference_To_Unknown);
                  Set_Unknown_T (Val, T);
                  Store (V, Val);
                  C_Set_Entity  (LLVM_Param, Param);
               end;

            --  If this is an array passed to a foreign convention, we
            --  pass it as a reference to the component type. So we have
            --  to convert to a pointer to the array type. If it's an
            --  unconstrained array, we have to materialize bounds for a
            --  fat pointer.

            elsif PK = Foreign_By_Component_Ref then
               LLVM_Param := Ptr_To_Ref (V, GT);

               if not Is_Constrained (GT) then
                  declare
                     Fat_Ptr   : constant GL_Value  :=
                       Get_Undef_Relationship (GT, Fat_Pointer);
                     Bound_Val : GL_Value           :=
                       Get_Undef_Relationship (GT, Bounds);

                  begin
                     for J in 0 .. Number_Dimensions (GT) - 1 loop
                        declare
                           IGT : constant GL_Type      :=
                             Array_Index_GT (GT, J);
                           LB  : constant GL_Value     :=
                             Const_Int (IGT, Uint_0);
                           HB  : constant N_Subexpr_Id :=
                             Type_High_Bound (IGT);

                        begin
                           Bound_Val := Insert_Value
                             (Insert_Value (Bound_Val, LB, unsigned (J * 2)),
                              Emit_Expression (HB), unsigned (J * 2 + 1));
                        end;
                     end loop;

                     LLVM_Param := Insert_Value
                       (Insert_Value (Fat_Ptr, LLVM_Param, 0),
                        Get (Bound_Val, Reference_To_Bounds), 1);
                  end;

                  Set_Value_Name (LLVM_Param, Name.all);
               end if;

            --  If this is a zero-size parameter (which we return onlywhen
            --  emitting C), use an Undef.

            elsif PK = Zero_Size then
               LLVM_Param := Get_Undef (GT);
            --  Otherwise we have the simple case

            else
               LLVM_Param := V;
            end if;

            --  Save the location of an activation record parameter

            if PK = Activation_Record then
               Activation_Rec_Param := From_Access (LLVM_Param);
            end if;

            --  If we have a reference to an unconstrained array, mark that
            --  the bounds can't change unless the bound itself is in the
            --  fat pointer. If the array is an In parameter (but not if
            --  its an access type), the data can't change as well. Don't
            --  do any of this when generating C.

            if not Emit_C
              and then Relationship (LLVM_Param) in Fat_Pointer | Thin_Pointer
            then
               if not Has_Bounds_In_Fat_Pointer (GT) then
                  Create_Invariant_Start
                    (Get (LLVM_Param, Reference_To_Bounds),
                     Get_Bound_Size (GT));
               end if;

               if Ekind (Param) = E_In_Parameter then
                  Create_Invariant_Start (Get (LLVM_Param, Reference));
               end if;
            end if;

            --  Finally, set up the resulting GL_Value, back-annotate,
            --  create debug info, set the value, and move to the next param.

            Initialize_Alignment (LLVM_Param);
            Initialize_TBAA      (LLVM_Param);
            Annotate_Object_Size_And_Alignment (Param, GT, Want_Max => False);
            Create_Local_Variable_Debug_Data (Param, LLVM_Param, P_Num);
            Set_Value (Param, LLVM_Param);
            Next_Formal_With_Extras (Param);

            if PK_Is_In_Or_Ref (PK) then
               Param_Num := Param_Num + 1;
            end if;
         end;
      end loop;

      Emit_Decl_Lists (Declarations (N), No_List);
      Emit (Handled_Statement_Sequence (N));

      --  If the last instrution isn't a terminator, add a return, but
      --  use an access type if this is a dynamic type or a return by ref.

      if not Are_In_Dead_Code then
         case LRK is
            when Void =>
               Build_Ret_Void;

            when Subprog_Return =>
               if RK = RK_By_Reference then
                  Build_Ret (Get_Undef_Ref (Return_GT));
               else
                  Build_Ret (Get_Undef (Return_GT));
               end if;

            when Out_Return =>
               Build_Ret (Get_Undef (Get_Param_GL_Type
                                       (First_Out_Param (Current_Subp))));

            when Struct_Out | Struct_Out_Subprog =>
               Build_Ret (Get_Undef_Fn_Ret (Current_Func));
         end case;
      end if;

      Pop_Block;
      Maybe_Build_Unreachable;
      Pop_Debug_Scope;
      Leave_Subp;
      Reset_Block_Tables;
      Activation_Var_Map.Clear;
      Current_Subp := Empty;
   end Emit_One_Body;

   ----------------------
   -- Add_To_Elab_Proc --
   ----------------------

   procedure Add_To_Elab_Proc (N : Node_Id; For_GT : GL_Type := No_GL_Type) is
   begin
      if Elaborations.Last = 0 or else No (N)
        or else Elaborations.Table (Elaborations.Last).N /= N
      then
         Elaborations.Append ((N => N, For_GT => For_GT));
      end if;
   end Add_To_Elab_Proc;

   --------------------
   -- Mark_Body_Elab --
   --------------------

   procedure Mark_Body_Elab is
   begin
      First_Body_Elab_Idx := Elaborations.Last + 1;
   end Mark_Body_Elab;

   -------------------------
   -- Is_Binder_Elab_Proc --
   -------------------------

   function Is_Binder_Elab_Proc (Name : String) return Boolean is
   begin
      return Name'Length >= 13
        and then Name (Name'Last - 7 .. Name'Last) = "___elabb"
        and then (Name (Name'First .. Name'First + 8) = "ada_main_"
                  or else Name (Name'Last - 11 .. Name'Last - 8) = "main");
   end Is_Binder_Elab_Proc;

   -----------------------
   -- Get_Elab_Position --
   -----------------------

   function Get_Elab_Position return Nat is
     (Elaborations.Last);

   ------------------------
   -- Reorder_Elab_Table --
   ------------------------

   procedure Reorder_Elab_Table (Old_Pos, New_Start : Nat) is
      Elabs : Elaboration_Entry_Array (1 .. Elaborations.Last);

      procedure Append_Range (Start, Last : Nat) with Inline;
      --  Append the range Start .. Last from Elabs above into the
      --  elaboration table.

      ------------------
      -- Append_Range --
      ------------------

      procedure Append_Range (Start, Last : Nat) is
      begin
         for J in Start .. Last loop
            Elaborations.Append (Elabs (J));
         end loop;
      end Append_Range;

   begin
      --  Copy entries from table to our array, reset the table, then copy
      --  then back in the desired order.

      for J in Elabs'Range loop
         Elabs (J) := Elaborations.Table (J);
      end loop;

      Elaborations.Set_Last (0);
      Append_Range (1, Old_Pos);
      Append_Range (New_Start + 1, Elabs'Last);
      Append_Range (Old_Pos + 1, New_Start);

      --  If we added to the elaboration of the spec, adjust the boundary
      --  between spec and body.

      if First_Body_Elab_Idx /= 0 and then First_Body_Elab_Idx > Old_Pos then
         First_Body_Elab_Idx :=
           First_Body_Elab_Idx + Elaborations.Last - New_Start;
      end if;
   end Reorder_Elab_Table;

   --------------------
   -- Emit_Elab_Proc --
   --------------------

   procedure Emit_Elab_Proc
     (N        : Node_Id;
      Stmts    : Opt_N_Handled_Sequence_Of_Statements_Id;
      CU       : N_Compilation_Unit_Id;
      For_Body : Boolean := False)
   is
      Nest_Table_First : constant Nat      := Nested_Functions.Last + 1;
      U                : constant Node_Id  := Defining_Unit_Name (N);
      Unit             : constant Node_Id  :=
        (if   Nkind (U) = N_Defining_Program_Unit_Name
         then Defining_Identifier (U) else U);
      S_List           : constant List_Id  :=
        (if No (Stmts) then No_List else Statements (Stmts));
      Suffix           : constant String   := (if For_Body then "b" else "s");
      Name             : constant String   :=
        Get_Name_String (Chars (Unit)) & "___elab" & Suffix;
      First_Idx        : constant Nat      :=
        (if For_Body then First_Body_Elab_Idx else 1);
      Last_Idx         : constant Nat      :=
        (if   For_Body or else First_Body_Elab_Idx = 0
         then Elaborations.Last else First_Body_Elab_Idx - 1);
      Work_To_Do       : constant Boolean  :=
        (for some J in First_Idx .. Last_Idx =>
         Present (Elaborations.Table (J).N))
        or else Has_Non_Null_Statements (S_List);
      Contains_Work    : Boolean           := False;
      BB               : Basic_Block_T;
      Last_Instr       : Value_T;
      LLVM_Func        : GL_Value;

   begin
      --  If we already know that we have nothing to elaborate, do nothing

      if not Work_To_Do then
         return;
      end if;

      --  If this is the elaboration for the body of the main unit that was
      --  already generated, use it. Otherwise make a new function.

      if Present (Ada_Main_Elabb)
        and then Get_Ext_Name (Ada_Main_Elabb_Ident) = Name
      then
         LLVM_Func := Ada_Main_Elabb;
      else
         LLVM_Func := Add_Function (Name, Fn_Ty ((1 .. 0 => <>), Void_Type),
                                    Void_GL_Type,
                                    Is_Builtin => True);
      end if;

      --  Do the mechanics of setting up the elab procedure

      Enter_Subp (LLVM_Func);
      Push_Debug_Scope
        (Get_Source_File_Index (Sloc (N)),
         Create_Subprogram_Debug_Info
           (LLVM_Func, N,
            Name     => Get_Name_String (Chars (Unit)),
            Ext_Name => Name));
      Set_Debug_Pos_At_Node (N);
      Entry_Block_Allocas := Get_Current_Position;
      Push_Block (EH_List => (if   No (Stmts) then No_List
                              else Exception_Handlers (Stmts)));
      In_Elab_Proc := True;
      C_Set_Elab_Proc (LLVM_Func, For_Body);

      --  Do through the elaboration table and process each entry

      for J in First_Idx .. Last_Idx loop
         declare
            Stmt : constant Node_Id := Elaborations.Table (J).N;
            GT   : constant GL_Type := Elaborations.Table (J).For_GT;

         begin
            if Present (GT) then
               Set_Value (Stmt, Emit_Convert_Value (Stmt, GT));
            elsif Present (Stmt)
              and then Nkind (Stmt) = N_Handled_Sequence_Of_Statements
            then
               --  If Stmt is an N_Handled_Sequence_Of_Statements, it must
               --  have come from a package body. Make a block around it
               --  so exceptions will work properly, if needed.

               Push_Block;
               Emit (Stmt);
               Pop_Block;
            elsif Present (Stmt) then
               Emit (Stmt);
            end if;
         end;
      end loop;

      --  Emit the statements after clearing the special code flag since
      --  we want to handle them normally: this will be the first time we
      --  see them, unlike any that were previously partially processed
      --  as declarations.

      In_Elab_Proc       := False;
      In_Elab_Proc_Stmts := True;
      Emit (S_List);
      Pop_Block;
      Build_Ret_Void;
      In_Elab_Proc_Stmts := False;
      Pop_Debug_Scope;
      Leave_Subp;
      Activation_Var_Map.Clear;

      --  Check if the function that we made actually does something. It
      --  does if any basic block has an instruction other than a branch or
      --  return.

      BB := Get_First_Basic_Block (+LLVM_Func);
      while Present (BB) loop
         Last_Instr := Get_Last_Instruction (BB);

         if Last_Instr /= Get_First_Instruction (BB)
           or else (not Present (Is_A_Return_Inst (Last_Instr))
                      and then not Present (Is_A_Branch_Inst (Last_Instr)))
         then
            Contains_Work := True;
            exit;
         end if;

         BB := Get_Next_Basic_Block (BB);
      end loop;

      --  If the function we made has no work to do delete, it. Otherwise,
      --  show there will be elaboration code

      if not Contains_Work then
         Delete_Function (+LLVM_Func);
      else
         Set_Has_No_Elaboration_Code (CU, False);

         --  Now elaborate any subprograms that were nested inside us

         for J in Nest_Table_First .. Nested_Functions.Last loop
            Emit_Subprogram_Body (Nested_Functions.Table (J));
         end loop;

         Nested_Functions.Set_Last (Nest_Table_First);
      end if;
   end Emit_Elab_Proc;

   --------------------------
   -- Emit_Subprogram_Body --
   --------------------------

   procedure Emit_Subprogram_Body
     (N : N_Subprogram_Body_Id; For_Inline : Boolean := False)
   is
      Nest_Table_First : constant Nat     := Nested_Functions.Last + 1;
      Spec             : constant Node_Id := Acting_Spec (N);

   begin
      --  Do nothing if this is an eliminated subprogram

      if Is_Eliminated (Defining_Entity (Spec)) then
         return;

      --  If we're not at library level, this a nested function. Defer it
      --  until we complete elaboration of the enclosing function. But do
      --  ensure that the spec has been elaborated.

      elsif not Library_Level then
         Discard (Emit_Subprogram_Decl (Acting_Spec (N)));
         Nested_Functions.Append (N);
         return;
      end if;

      --  Otherwise, elaborate this function and then any nested functions
      --  within in.

      Emit_One_Body (N, For_Inline => For_Inline);

      for J in Nest_Table_First .. Nested_Functions.Last loop
         Emit_Subprogram_Body (Nested_Functions.Table (J),
                               For_Inline => For_Inline);
      end loop;

      Nested_Functions.Set_Last (Nest_Table_First);
   end Emit_Subprogram_Body;

   ---------------------------
   -- Emit_Return_Statement --
   ---------------------------

   procedure Emit_Return_Statement (N : N_Simple_Return_Statement_Id) is
      GT   : constant GL_Type          := Full_GL_Type (Current_Subp);
      RK   : constant Return_Kind      := Get_Return_Kind (Current_Subp);
      LRK  : constant L_Ret_Kind       := Get_L_Ret_Kind (Current_Subp);
      Expr : constant Opt_N_Subexpr_Id := Expression (N);
      E    : constant Entity_Id        :=
        (if   Present (Expr) and then Nkind (Expr) = N_Identifier
         then Entity (Expr) else Empty);
      V    : GL_Value                  := No_GL_Value;

   begin
      --  Start by handling our expression, if any. Note that we ignore
      --  the expression if we're emitting C and this is a function that
      --  returns a zero-sized object.

      if Present (Expression (N))
        and then not (Emit_C and then Is_Zero_Size (GT))
      then
         pragma Assert (RK /= None);

         --  If there's a parameter for the address to which to copy the
         --  return value, do the copy instead of returning the value.

         if RK = Return_By_Parameter then
            Emit_Assignment (Return_Address_Param, Expr);

         --  If we return by reference do just that. We don't do this if a
         --  Storage_Pool is specified unless this is the variable from
         --  an extended return statement, in which case we've already
         --  done the allocation.

         elsif RK = RK_By_Reference
           and then (No (Storage_Pool (N))
                      or else (Present (E)
                                and then Is_Return_Object (E)
                                and then Get_Allocated_For_Return (E)))
         then
            V := Convert_Ref (Emit_LValue (Expr), GT);

         --  If this function returns unconstrained, allocate memory for
         --  the return value, copy the data to be returned to there, and
         --  return an access (fat pointer) to the value. If this is a
         --  return-by-reference function, return a reference to this
         --  value. However, if a return-by-reference function has a
         --  Storage_Pool, that means we must allocate memory in that pool,
         --  copy the return value to it, and return that address.

         elsif Is_Unconstrained_Array (GT)
           or else (RK = RK_By_Reference and then Present (Storage_Pool (N)))
         then
            V := Get (Heap_Allocate_For_Type
                        (GT, Full_Alloc_GL_Type (Expr),
                         Expr => Expression (N),
                         N    => N,
                         Proc => Procedure_To_Call (N),
                         Pool => Storage_Pool (N)),
                      Relationship_For_Ref (GT));

         --  Otherwise, we just return data

         else
            V := Get (Emit_Conversion (Expression (N), GT), Data);
         end if;
      else
         pragma Assert (RK = None);
      end if;

      --  Now generate any neded fixups for this return

      Emit_Fixups_For_Return;

      --  Now see what the actual return value of this LLVM function should be

      case LRK is
         when Void =>
            Build_Ret_Void;

         when Subprog_Return =>
            Build_Ret (V);

         when Out_Return =>
            Build_Ret (Get (Get_Value (First_Out_Param (Current_Subp)), Data));

         when Struct_Out | Struct_Out_Subprog => Struct : declare

            Retval : GL_Value           := Get_Undef_Fn_Ret (Current_Func);
            Param  : Opt_Formal_Kind_Id := First_Out_Param  (Current_Subp);
            J      : unsigned           := 0;

         begin
            if not Decls_Only then
               if LRK = Struct_Out_Subprog then
                  Retval := Insert_Value (Retval, V, J);
                  J      := J + 1;
               end if;

               while Present (Param) loop
                  Retval :=
                    Insert_Value (Retval, Get (Get_Value (Param), Data), J);
                  J      := J + 1;
                  Next_Out_Param (Param);
               end loop;
            end if;

            Build_Ret (Retval);
         end Struct;
      end case;

   end Emit_Return_Statement;

   ---------------------------------
   -- Actual_Subprogram_Base_Type --
   ---------------------------------

   function Actual_Subprogram_Base_Type
     (E : Entity_Id) return Opt_Type_Kind_Id
   is
      TE : Opt_Type_Kind_Id := Empty;

   begin
      --  If there's no entity or it's not a subprogram, we don't have a
      --  type to return.

      if No (E) or else Ekind (E) not in Subprogram_Kind then
         return Empty;
      end if;

      --  Otherwise, it depends on the LLVM return kind

      case Get_L_Ret_Kind (E) is
         when Subprog_Return =>
            TE := Etype (E);

         when Out_Return =>
            TE := Etype (First_Out_Param (E));

         when others =>
            null;
      end case;

      --  We want the full base type

      return (if Present (TE) then Full_Base_Type (TE) else Empty);
   end Actual_Subprogram_Base_Type;

   ---------------------
   -- Get_Static_Link --
   ---------------------

   function Get_Static_Link (Subp : Subprogram_Kind_Id) return GL_Value is
      Parent      : constant Subprogram_Kind_Id := Enclosing_Subprogram (Subp);
      Ent_Caller  : Subp_Entry;
      Ent         : Subp_Entry;
      Result      : GL_Value;

   begin

      if Present (Parent) then
         Ent        := Subps.Table (Subp_Index (Parent));
         Ent_Caller := Subps.Table (Subp_Index (Current_Subp));

         if Parent = Current_Subp then
            Result := (if   Present (Ent.ARECnP)
                       then Get (Get_Value (Ent.ARECnP), Data)
                       else Get_Undef (A_Char_GL_Type));
         elsif No (Ent_Caller.ARECnF) then
            return Get_Undef (A_Char_GL_Type);
         else
            Result := Get_Value (Ent_Caller.ARECnF);

            --  Go levels up via the ARECnU field if needed. Each time,
            --  get the new type from the first field of the record that
            --  it points to.

            for J in 1 .. Ent_Caller.Lev - Ent.Lev - 1 loop
               Result := Load
                 (Get (GEP (Full_GL_Type (First_Component_Or_Discriminant
                                          (Full_Designated_Type (Result))),
                            Result, (1 => Const_Null_32, 2 => Const_Null_32),
                            "ARECnF.all.ARECnU"), Reference));
            end loop;
         end if;

         return Pointer_Cast (Result, A_Char_GL_Type, "static.link");
      else
         return Get_Undef (A_Char_GL_Type);
      end if;
   end Get_Static_Link;

   ---------------------
   -- Add_Static_Link --
   ---------------------

   function Add_Static_Link
     (Proc : Opt_Subprogram_Kind_Id;
      Args : GL_Value_Array) return GL_Value_Array
   is
      S_Link         : GL_Value := No_GL_Value;
      Args_With_Link : GL_Value_Array (Args'First .. Args'Last + 1);

   begin
      --  First see if we need an activation record parameter, either a real
      --  one or a dummy one.

      if Present (Proc) and then Has_Activation_Record (Proc)
        and then Present (Subps.Table (Subp_Index (Proc)).ARECnF)
      then
         S_Link := Pointer_Cast (Get_Static_Link (Proc),
                                 Full_GL_Type (Extra_Formals (Proc)));
      elsif Force_Activation_Record_Parameter then
         S_Link := Get_Undef (A_Char_GL_Type);
      end if;

      --  If we did, add it to the argument list

      if Present (S_Link) then
         Args_With_Link (Args'Range) := Args;
         Args_With_Link (Args_With_Link'Last) := S_Link;
         return Args_With_Link;
      else
         return Args;
      end if;
   end Add_Static_Link;

   ----------------
   -- Call_Alloc --
   ----------------

   function Call_Alloc
     (Proc : E_Procedure_Id;
      N    : Node_Id;
      Args : GL_Value_Array) return GL_Value is
   begin
      --  See if this is a call to __builtin_return_slot and handle if so.
      --  If something is wrong with the call, we'll likely get an ICE.
      --  It's theoretically possible, but very hard, for a user to produce
      --  a call to that here.

      if Is_Intrinsic_Subprogram (Proc)
        and then Get_Ext_Name (Proc) = "__builtin_return_slot"
      then
         declare
            Our_Size : constant GL_Value := Args (Args'First);
            Ret_GT   : constant GL_Type  :=
              Related_Type (Return_Address_Param);
            Ret_Size : constant GL_Value :=
              To_Bytes (Get_Type_Size (Ret_GT, Max_Size =>
                                         Is_Unconstrained_Record (Ret_GT)));

         begin
            --  If both sizes are constant, our size must be no larger than
            --  the size of the return area. Otherwise, emit a runtime
            --  test to verify that.

            if Is_A_Constant_Int (Our_Size)
              and then Is_A_Constant_Int (Ret_Size)
            then
               pragma Assert (Ret_Size >= Our_Size);
            else
               Emit_Raise_Call_If (I_Cmp (Int_SGT, Our_Size, Ret_Size),
                                   (if Present (N) then N else Proc),
                                   PE_Explicit_Raise);
            end if;

            --  Finally, return the address of the return parameter

            return Ptr_To_Address_Type (Return_Address_Param);
         end;

         --  Otherwise, call the procedure with our size. We treat this as
         --  a function because we know that it's been converted into a
         --  function whose return is the parameter which has "out" mode,
         --  but we have to make sure the type is correct. We have to
         --  handle both the integral and record pointer cases.

      else
         declare
            Result : constant GL_Value  :=
              Call (Emit_Entity (Proc), Add_Static_Link (Proc, Args));
            Formal : Opt_Formal_Kind_Id := First_Formal_With_Extras (Proc);
            GT     : GL_Type;

         begin
            while Present (Formal) loop
               if Ekind (Formal) = E_Out_Parameter then
                  GT := Default_GL_Type (Full_Etype (Formal));
                  return (if Is_Descendant_Of_Address (GT)
                          then G_Is (Result, GT) else G_Is_Ref (Result, GT));
               end if;

               Next_Formal_With_Extras (Formal);
            end loop;

            pragma Assert (False);
         end;
      end if;
   end Call_Alloc;

   ------------------
   -- Call_Dealloc --
   ------------------

   procedure Call_Dealloc (Proc : E_Procedure_Id; Args : GL_Value_Array) is
   begin
      Call (Emit_Entity (Proc), Add_Static_Link (Proc, Args));
   end Call_Dealloc;

   ---------------------------
   -- Has_Activation_Record --
   ---------------------------

   function Has_Activation_Record
     (E : Subprogram_Type_Or_Kind_Id) return Boolean
   is
      Formal : Opt_Formal_Kind_Id := First_Formal_With_Extras (E);

   begin
      --  In the type case, we don't consider it as having an activation
      --  record for foreign conventions because we will have had to make
      --  a trampoline directly at the 'Access or 'Address.

      if Is_Type (E) and then Has_Foreign_Convention (E) then
         return False;
      end if;

      --  Otherwise, see if any parameter is an activation record.

      while Present (Formal) loop
         exit when  Ekind (Formal) = E_In_Parameter
           and then Is_Activation_Record (Formal);
         Next_Formal_With_Extras (Formal);
      end loop;

      return Present (Formal);
   end Has_Activation_Record;

   --------------------------------
   -- Emit_Subprogram_Identifier --
   --------------------------------

   function Emit_Subprogram_Identifier
     (E  : Subprogram_Kind_Id;
      N  : Opt_N_Has_Entity_Id;
      GT : GL_Type) return GL_Value
   is
      V  : GL_Value := Get_Value (E);

   begin
      --  If this has an Alias, use that. But make sure that it's the proper
      --  type since we may have extension records or slightly different
      --  subtypes for some parameters.

      if Present (Alias (E)) then
         declare
            T : constant Type_T :=
              Pointer_Type (Create_Subprogram_Type (E), Address_Space);

         begin
            V := Emit_Entity (Alias (E));
            return (if   Type_Of (V) /= T
                    then Ptr_To_Relationship (V, T, Reference_To_Subprogram)
                    else V);
         end;
      end if;

      --  If we haven't gotten one yet, make it. Otherwise, see if we need
      --  to dereference it.

      if No (V) then
         V := Create_Subprogram (E);
      elsif Is_Double_Reference (V) then
         V := Load (V);
      end if;

      --  If we're elaborating this for 'Access or 'Address, we want the
      --  actual subprogram type here, not the type of the return value,
      --  which is what GT is set to. We also may have to make a
      --  trampoline or Fat_Reference_To_Subprogram here since it's too
      --  late to make it in Get because it doesn't know what subprogram it
      --  was for.

      if Present (N) and then  Nkind (Parent (N)) = N_Attribute_Reference then
         declare
            Ref    : constant N_Attribute_Reference_Id := Parent (N);
            Ref_GT : constant GL_Type                  := Full_GL_Type (Ref);
            S_Link : constant GL_Value                 :=
              (if   Has_Activation_Record (E) then Get_Static_Link (E)
               else Get_Undef (A_Char_GL_Type));
            Attr   : constant Attribute_Id             :=
              Get_Attribute_Id (Attribute_Name (Ref));

         begin
            if Is_Access_Type (Ref_GT) then
               declare
                  DT : constant GL_Type := Full_Designated_GL_Type (Ref_GT);

               begin
                  if Has_Foreign_Convention (Ref_GT)
                    or else not Can_Use_Internal_Rep (Ref_GT)
                  then
                     return (if   Has_Activation_Record (E)
                             then Make_Trampoline (DT, V, S_Link, E)
                             else G_Is_Relationship (V, DT, Trampoline));
                  else
                     return Insert_Value
                       (Insert_Value (Get_Undef_Relationship
                                        (DT, Fat_Reference_To_Subprogram),
                                      S_Link, 1),
                        Pointer_Cast (V, A_Char_GL_Type), 0);
                  end if;
               end;
            elsif Attr = Attribute_Address then
               return (if   Has_Activation_Record (E)
                       then Make_Trampoline (GT, V, S_Link, E) else V);
            end if;
         end;
      end if;

      return V;

   end Emit_Subprogram_Identifier;

   ---------------
   -- Emit_Call --
   ---------------

   function Emit_Call
     (N         : N_Subprogram_Call_Id;
      Outer_LHS : GL_Value := No_GL_Value) return GL_Value
   is
      procedure Write_Back
        (In_LHS  : GL_Value;
         F       : Opt_Record_Field_Kind_Id;
         In_Idxs : Access_GL_Value_Array;
         In_RHS  : GL_Value;
         LVs     : in out Access_GL_Value_Array;
         VFA     : Boolean)
        with Pre => Present (In_RHS) and then Is_Reference (In_LHS);
      --  Write the value in In_RHS to the location In_LHS. F, if Present,
      --  is a field into In_LHS to write. LVs are the list of LValues
      --  pushed by the argument's evaluation.

      function Misaligned_Copy_Required
        (V        : GL_Value;
         Actual   : N_Subexpr_Id;
         Formal   : Formal_Kind_Id;
         Bitfield : Boolean) return Boolean
        with Pre => Present (V) and then Present (Param);
      --  Return whether V, when passed to a by-reference Formal, must be
      --  copied due to a misalignment. If Bitfield is True, we know that
      --  we have a bitfield. Also produce any required errors or
      --  warnings.

      type WB is record
         LHS, RHS : GL_Value;
         Field    : Opt_Record_Field_Kind_Id;
         Idxs     : Access_GL_Value_Array;
         LVs      : Access_GL_Value_Array;
         VFA      : Boolean;
      end record;
      --   A writeback entry for an by-ref type that's In or In Out

      type WB_Array     is array (Nat range <>) of WB;
      type Entity_Array is array (Nat range <>) of Opt_Record_Field_Kind_Id;

      Subp             : constant N_Subexpr_Id := Name (N);
      Our_Return_GT    : constant GL_Type      := Full_GL_Type (N);
      Direct_Call      : constant Boolean      := Is_Entity_Name (Subp);
      Subp_Typ         : constant Subprogram_Type_Or_Kind_Id :=
        (if Direct_Call then Entity (Subp) else Full_Etype (Subp));
      Fn_Ty            : constant Type_T       :=
        Create_Subprogram_Type (Subp_Typ);
      RK               : constant Return_Kind  := Get_Return_Kind   (Subp_Typ);
      LRK              : constant L_Ret_Kind   := Get_L_Ret_Kind    (Subp_Typ);
      Return_GT        : constant GL_Type      := Full_GL_Type      (Subp_Typ);
      Orig_Arg_Count   : constant Nat          := Number_In_Params  (Subp_Typ);
      Out_Arg_Count    : constant Nat          := Number_Out_Params (Subp_Typ);
      Out_Param        : Opt_Formal_Kind_Id    := First_Out_Param   (Subp_Typ);
      No_Adjust_LV     : constant Boolean      := Contains_Discriminant (N);
      In_Idx           : Nat                   := 1;
      Out_Idx          : Nat                   := 1;
      Ret_Idx          : Nat                   := 0;
      Result           : GL_Value              := No_GL_Value;
      Foreign          : constant Boolean      :=
        Has_Foreign_Convention (Subp_Typ);
      Has_S_Link       : constant Boolean      :=
        Has_Activation_Record (Subp_Typ);
      This_Adds_S_Link : constant Boolean      :=
        (not Direct_Call and not Foreign)
        or else (not Has_S_Link and then Force_Activation_Record_Parameter
                 and then not Foreign);
      Arg_Count        : constant Nat          :=
        Orig_Arg_Count + (if This_Adds_S_Link then 1 else 0) +
          (if RK = Return_By_Parameter then 1 else 0);
      Args             : GL_Value_Array (1 .. Arg_Count);
      WBs              : WB_Array       (1 .. Arg_Count) :=
        (others => (No_GL_Value, No_GL_Value, Empty, null, null, False));
      Out_LHSs         : GL_Value_Array (1 .. Out_Arg_Count);
      Out_Flds         : Entity_Array   (1 .. Out_Arg_Count);
      Out_VFAs         : array (1 .. Out_Arg_Count) of Boolean;
      Out_Idxs         : array (1 .. Out_Arg_Count) of Access_GL_Value_Array;
      Out_LVs          : array (1 .. Out_Arg_Count) of Access_GL_Value_Array;
      Actual_Return    : GL_Value;
      S_Link           : GL_Value;
      LLVM_Func        : GL_Value;
      Param            : Opt_Formal_Kind_Id;
      Actual           : Opt_N_Subexpr_Id;

      ----------------
      -- Write_Back --
      ----------------

      procedure Write_Back
        (In_LHS  : GL_Value;
         F       : Opt_Record_Field_Kind_Id;
         In_Idxs : Access_GL_Value_Array;
         In_RHS  : GL_Value;
         LVs     : in out Access_GL_Value_Array;
         VFA     : Boolean)
      is
         LHS      : GL_Value              := In_LHS;
         RHS      : GL_Value              := Get (In_RHS, Object);
         Idxs     : Access_GL_Value_Array := In_Idxs;
         LHS_GT   : constant GL_Type      :=
           (if    Present (F) then Field_Type (F)
            elsif Idxs /= null then Full_Component_GL_Type (Related_Type (LHS))
            else  Related_Type (LHS));
         RHS_GT   : constant GL_Type      := Related_Type (RHS);

      begin
         --  Handle the case of an undef as our arg. See below in Emit_Call.

         if Is_Undef (LHS) then
            return;
         end if;

         --  We've looked through any conversions in the actual and
         --  evaluated the actual LHS to be assigned before the call. We
         --  wouldn't be here if this were a dynamic-sized type, and we
         --  know that the types of LHS and RHS are similar, but it may be
         --  a small record and the types on both sides may differ.
         --
         --  If we're dealing with elementary types, convert the RHS to the
         --  type of the LHS. Otherwise, convert the type of the LHS to be
         --  a reference to the type of the RHS unless this is a field
         --  or indexed store.

         if Is_Elementary_Type (LHS_GT)
           and then not Is_Packed_Array_Impl_Type (LHS_GT)
         then
            RHS := Convert (RHS, LHS_GT);
         elsif No (F) and then Idxs = null then
            LHS := Convert_Ref (LHS, RHS_GT);
         end if;

         --  Now do the assignment, either directly or as a bitfield or
         --  indexed store.
         --  ??? This may be somewhat problematic because we've lost the
         --  list of LValues used to compute the size so may not be able
         --  to copy it back if it's self-referential.

         if LVs /= null then
            Put_LValue_List (LVs);
         end if;

         if Present (F) then
            Build_Field_Store (LHS, F, RHS, VFA);
         elsif Idxs /= null then
            Build_Indexed_Store (LHS, Idxs.all, RHS, VFA);
            Free (Idxs);
         else
            Emit_Assignment (LHS, Value => RHS);
         end if;

         --  Clear any LValues that we pushed above

         if not No_Adjust_LV then
            Clear_LValue_List;
         end if;
      end Write_Back;

      ------------------------------
      -- Misaligned_Copy_Required --
      ------------------------------

      function Misaligned_Copy_Required
        (V        : GL_Value;
         Actual   : N_Subexpr_Id;
         Formal   : Formal_Kind_Id;
         Bitfield : Boolean) return Boolean
      is
         GT           : constant GL_Type := Full_GL_Type (Formal);
         Our_Bitfield : Boolean          := Bitfield;
         N            : N_Subexpr_Id     := Actual;

      begin
         --  If this is a fat pointer type, we don't need a copy. Don't
         --  give error if just analyzing decls. And if this is an undef,
         --  we already know that we have an error

         if Relationship (V) in Fat_Pointer | Fat_Reference_To_Subprogram
           or else Decls_Only or else Is_Undef (V)
         then
            return False;
         end if;

         --  Otherwise see if Actual involves a bitfield reference

         while not Our_Bitfield loop
            case Nkind (N) is
               when N_Selected_Component =>
                  if Is_Bitfield_By_Rep (Entity (Selector_Name (N))) then
                     Our_Bitfield := True;
                  end if;

                  N := Prefix (N);

               when N_Indexed_Component =>
                  N := Prefix (N);

               when others =>
                  exit;
            end case;
         end loop;

         --  If the alignment is at least as strict as that for the type and
         --  we don't have a bitfield, we don't have an issue.

         if not Our_Bitfield and then Alignment (V) >= Get_Type_Alignment (GT)
         then
            return False;

         --  If the formal is passed by reference, a copy is not allowed

         elsif Is_Aliased (Formal) or else Is_By_Reference_Type (GT) then
            Error_Msg_N ("misaligned actual cannot be passed by reference",
                         Actual);

         --  If the mechanism was forced to by-ref, a copy is not allowed
         --  but we issue only a warning because this case is not strict
         --  Ada.

         elsif Get_Orig_By_Ref_Mech (Formal) then
            Error_Msg_N ("??misaligned actual cannot be passed by reference",
                         Actual);
         end if;

         return True;
      end Misaligned_Copy_Required;

   begin  -- Start of processing for Emit_Call

      --  See if this is an instrinsic subprogram that we handle. We're
      --  done if so.

      if Direct_Call and then Is_Intrinsic_Subprogram (Entity (Subp)) then
         Result := Emit_Intrinsic_Call (N, Entity (Subp));

         if Present (Result) then
            return Result;
         end if;
      end if;

      --  Get the suprogram to call. If we have a static link, extract
      --  it. Then, unless the subprogram address is already a reference to
      --  a subprogram, get it as a reference.

      LLVM_Func := Emit_Safe_LValue (Subp);

      if This_Adds_S_Link then
         S_Link := Get (LLVM_Func, Reference_To_Activation_Record);
      end if;

      if Relationship (LLVM_Func) /= Reference_To_Subprogram then
         LLVM_Func := Get (LLVM_Func, Reference);
      end if;

      --  Add a pointer to the location of the return value if the return
      --  type is of dynamic size.

      if RK = Return_By_Parameter then
         Args (In_Idx) :=
           (if   Present (Outer_LHS)
                 and then (Is_Safe_From (Outer_LHS, N)
                           or else (Is_By_Reference_Type (Return_GT)
                                    and then Nkind (N) = N_Function_Call))
            then Convert_Ref (Outer_LHS, Return_GT)
            else Get (Allocate_For_Type (Return_GT,
                                         N        => Subp,
                                         Name     => "RETURN",
                                         Max_Size =>
                                           Is_Unconstrained_Record
                                           (Return_GT)),
                      Relationship_For_Ref (Return_GT)));
         In_Idx := In_Idx + 1;
      end if;

      --  Normally, we want to handle any LValues in each argument separately.
      --  However, this may be a call that converts a discriminant Rep to a Pos
      --  in which case, we don't want to change the LValue lists.

      if not No_Adjust_LV then
         Push_LValue_List;
      end if;

      Param  := First_Formal_With_Extras (Subp_Typ);
      Actual := First_Actual (N);
      while Present (Actual) loop

         declare
            GT   : constant GL_Type         := Get_Param_GL_Type (Param);
            PK   : constant Param_Kind      := Get_Param_Kind    (Param);
            R    : constant GL_Relationship := Relationship_For_PK (PK, GT);
            F    : Opt_Record_Field_Kind_Id := Empty;
            Idxs : Access_GL_Value_Array    := null;
            LHS  : GL_Value                 := No_GL_Value;
            Arg  : GL_Value;

         begin
            --  We don't want to get confused between LValues from
            --  a previous parameter when getting positions and
            --  sizes of another, so clear the list.

            if not No_Adjust_LV then
               Clear_LValue_List;
            end if;

            if PK_Is_In_Or_Ref (PK) then

               --  If the param isn't passed by reference, convert the
               --  value to the parameter's type. If it is, convert the
               --  pointer to being a pointer to the parameter's type.

               if PK_Is_Reference (PK) then

                  --  If this is an Out or In Out parameter, we need to check
                  --  if this is a bitfield reference.

                  if Ekind (Param) = E_In_Parameter then
                     LHS := Emit_LValue (Actual);
                  else
                     LHS_And_Component_For_Assignment (Actual, LHS, F, Idxs,
                                                       For_LHS       => True,
                                                       Only_Bitfield => True);
                  end if;

                  --  If this is a bitfield reference, we need to create
                  --  a temporary for this reference, initialize it to
                  --  the field load, and set up for a writeback.

                  pragma Assert (Idxs = null);

                  if Present (F)
                    and then Misaligned_Copy_Required (LHS, Actual, Param,
                                                       True)
                  then
                     Arg := Allocate_For_Type (GT,
                                               N => Actual,
                                               V => Build_Field_Load (LHS, F));
                     WBs (In_Idx) := (LHS   => LHS,
                                      RHS   => Arg,
                                      Field => F,
                                      Idxs  => Idxs,
                                      LVs   => Get_LValue_List,
                                      VFA   => Is_VFA_Ref (Actual));

                     Arg := Get (Arg, R);

                  else
                     --  If we have to make a copy, do so. Also set up a
                     --  writeback if we have to.

                     if Misaligned_Copy_Required (LHS, Actual, Param, False)
                       or else Has_SM_Copy_From (LHS)
                       or else (Ekind (Param) /= E_In_Parameter
                                  and then Has_SM_Copy_To (LHS))
                     then
                        Arg  := Allocate_For_Type (Related_Type (LHS),
                                                   N => Actual,
                                                   V => LHS);

                        if Ekind (Param) /= E_In_Parameter then
                           WBs (In_Idx) := (LHS   => LHS,
                                            RHS   => Arg,
                                            Field => Empty,
                                            Idxs  => null,
                                            LVs   => Get_LValue_List,
                                            VFA   => Is_VFA_Ref (Actual));
                        end if;
                     else
                        Arg := LHS;
                     end if;

                     --  Now convert to a pointer to the proper type

                     Arg := (if    PK = Foreign_By_Ref
                             then  Ptr_To_Relationship (Get (Arg, R), GT, R)
                             elsif PK = Foreign_By_Component_Ref
                             then  Ptr_To_Relationship
                                     (Get (Arg, R),
                                       Full_Component_GL_Type (GT), R)
                             else  Get (Convert_Ref (Arg, GT), R));
                  end if;

               else
                  Arg := Emit_Conversion (Actual, GT);

                  --  Handle the case where this is a parameter passed by
                  --  integer. In that case, we have to store the parameter
                  --  in memory, pun that memory to a pointer to the integer,
                  --  and load the integer.

                  if PK = In_Value_By_Int then
                     declare
                        Size : constant ULL    :=
                          Get_Type_Size (Type_Of (Related_Type (Arg)));
                        T    : constant Type_T := Int_Ty (Size);

                     begin
                        Arg := Get (Arg, Reference);
                        Arg :=
                          Ptr_To_Relationship
                            (Get (Arg, Reference),
                             Pointer_Type
                               (T, Address_Space),
                             Reference_To_Unknown);
                        Set_Unknown_T (Arg, T);
                        Arg := Load (Arg);
                     end;
                  else
                     Arg := Get (Arg, Data);
                  end if;
               end if;

               Args (In_Idx) := Arg;
               In_Idx        := In_Idx + 1;
            end if;

            --  For out and in out parameters, we need to evaluate the
            --  expression before the call (see, e.g., c64107a) into an
            --  LValue and use that after the return. We look through any
            --  conversions here. However, if the input is an undef, it
            --  really isn't an LValue, so we don't want to write anything
            --  back. Use an undef instead and check for it when we do the
            --  writeback.

            if PK_Is_Out (PK) then
               if PK_Is_In_Or_Ref (PK) and then Is_Undef (Arg) then
                  LHS := Get_Undef_Ref (GT);
               elsif No (LHS) or else Actual /= Strip_Conversions (Actual) then
                  LHS_And_Component_For_Assignment (Strip_Conversions (Actual),
                                                    LHS, F, Idxs,
                                                    For_LHS => True);
               end if;

               --  If LHS represents the full parameter (i.e., no field
               --  reference or index), ensure it's of the same type as the
               --  returned value.

               Out_LHSs (Out_Idx) :=
                 (if   No (F) and then Idxs = null then Convert_Ref (LHS, GT)
                  else LHS);
               Out_Flds (Out_Idx) := F;
               Out_Idxs (Out_Idx) := Idxs;
               Out_LVs  (Out_Idx) := Get_LValue_List;
               Out_VFAs (Out_Idx) := Is_VFA_Ref (Actual);
               Out_Idx            := Out_Idx + 1;
            end if;
         end;

         Next_Actual (Actual);
         Next_Formal_With_Extras (Param);
         pragma Assert (No (Actual) = No (Param));
      end loop;

      --  Pop the LValue list if we pushed it

      if not No_Adjust_LV then
         Pop_LValue_List;
      end if;

      --  Set the argument for the static link, if any

      if This_Adds_S_Link then
         Args (In_Idx) := S_Link;
      end if;

      --  Now we handle the result. It may be the return type of a function,
      --  in which case we return it, one or more out parameters, or both.
      --  We may also have used the first parameter to pass an address where
      --  our value was returned.

      case LRK is
         when Void =>
            Call (LLVM_Func, Fn_Ty, Args);

            --  If we're emitting C and the return type is of zero size,
            --  we may get here, in which case our result is undef.

            if Emit_C and then Is_Zero_Size (Return_GT) then
               Result := Get_Undef (Return_GT);
            end if;

         when Subprog_Return =>
            if RK = RK_By_Reference then
               Result := Call_Ref (LLVM_Func, Fn_Ty, Args);
            else
               Result := Call (LLVM_Func, Fn_Ty, Args);
            end if;

         when Out_Return =>

            --  Write back the single out parameter to our saved LHS

            Write_Back
              (Out_LHSs (1), Out_Flds (1), Out_Idxs (1),
               Call (LLVM_Func, Fn_Ty, Args, Get_Param_GL_Type (Out_Param)),
               LVs => Out_LVs (1),
               VFA => Out_VFAs (1));

         when Struct_Out | Struct_Out_Subprog =>
            Actual_Return :=
              Call_Relationship (LLVM_Func, Fn_Ty, Args, Unknown);

            --  First extract the return value (possibly returned by-ref)

            if LRK = Struct_Out_Subprog then
               if RK = RK_By_Reference then
                  Result := Extract_Value_To_Ref (Return_GT, Actual_Return,
                                                  unsigned (Ret_Idx));
               else
                  Result := Extract_Value (Return_GT, Actual_Return,
                                           unsigned (Ret_Idx));
               end if;

               Ret_Idx := Ret_Idx + 1;
            end if;

            --  Now write back any out parameters

            Out_Idx := 1;
            while Present (Out_Param) loop
               Write_Back (Out_LHSs (Out_Idx), Out_Flds (Out_Idx),
                           Out_Idxs (Out_Idx),
                           Extract_Value (Get_Param_GL_Type (Out_Param),
                                          Actual_Return, unsigned (Ret_Idx)),
                           LVs => Out_LVs (Out_Idx),
                           VFA => Out_VFAs (Out_Idx));
               Ret_Idx := Ret_Idx + 1;
               Out_Idx := Out_Idx + 1;
               Next_Out_Param (Out_Param);
            end loop;
      end case;

      --  Do writebacks for by-reference types that reference bitfields

      for J in WBs'Range loop
         if Present (WBs (J).LHS) then
            Write_Back (WBs (J).LHS, WBs (J).Field, WBs (J).Idxs, WBs (J).RHS,
                        WBs (J).LVs, WBs (J).VFA);
         end if;
      end loop;

      --  Perform any needed write-backs in case any of the above had an
      --  LHS involving a bitfield internally.

      Perform_Writebacks;

      if RK = Return_By_Parameter then
         return Convert_Ref (Args (1), Our_Return_GT);
      else
         return Result;
      end if;

   end Emit_Call;

   --------------------------
   -- Emit_Subprogram_Decl --
   --------------------------

   function Emit_Subprogram_Decl
     (N      : N_Subprogram_Specification_Id;
      Frozen : Boolean := True) return GL_Value
   is
      E           : constant Entity_Id := Defining_Entity (N);
      V           : GL_Value           := Get_Value       (E);
      Addr_Clause : constant Opt_N_Attribute_Definition_Clause_Id :=
        Address_Clause (E);

   begin
      --  If we have a freeze node and we're not sure that it's already
      --  frozen, do nothing.

      if Present (Freeze_Node (E)) and then not Frozen
        and then not In_Elab_Proc
      then
         return No_GL_Value;

      --  Otherwise, if we have an address clause, we make a pointer
      --  to the subprogram and either initialize it to the address
      --  clause if it's static, assign it if we're not at library
      --  level, or set up for our elab proc to do the initialization.

      elsif Present (Addr_Clause) then
         declare
            Subp_T    : constant Type_T          :=
              Pointer_Type (Create_Subprogram_Type (E), Address_Space);
            GT        : constant GL_Type         := Full_GL_Type (E);
            Addr_Expr : constant N_Subexpr_Id    := Expression (Addr_Clause);
            R         : constant GL_Relationship := Reference_To_Subprogram;
            Addr      : GL_Value                 := Get_Value (Addr_Expr);

            function Int_To_Subp (V : GL_Value) return GL_Value is
              (GM (Int_To_Ptr (IR_Builder, +Addr, Subp_T, ""), GT, R, V))
              with Pre => Present (V), Post => Present (Int_To_Subp'Result);

         begin
            if Library_Level or else In_Elab_Proc then
               if No (V) then
                  V := G (Add_Global (Module, Subp_T, Get_Ext_Name (E)),
                          GT, Ref (R));
                  Set_Value (E, V);
               end if;

               --  If we have a static address, we can put it in the
               --  initializer.

               if Is_Static_Address (Addr_Expr) then
                  if No (Addr) then
                     Addr := Emit_Expression (Addr_Expr);
                  end if;

                  Set_Initializer (V, Int_To_Subp (Addr));

               --  If we're in an elab proc, we already have the global
               --  variable for the function's address and have just
               --  previously evaluate the address. So just convert and
               --  store it.

               elsif In_Elab_Proc then
                  Store (Int_To_Subp (Addr), V);

               --  Otherwise, initialize this to null and add to elab proc

               else
                  Set_Initializer  (V, G (Const_Null (Subp_T), GT, R));
                  Add_To_Elab_Proc (N);
               end if;

            --  If inside a subprogram, convert the address to the proper
            --  type, give it our name, and set it as our value.

            else
               if No (Addr) then
                  Addr := Emit_Expression (Addr_Expr);
               end if;

               V := Int_To_Subp (Addr);
               Set_Value_Name (V, Get_Ext_Name (E));
               Set_Value      (E, V);
            end if;
         end;

      --  Otherwise, if we haven't already made this subprogram,
      --  make it.

      elsif No (Get_Value (E)) then
         Discard (Create_Subprogram (E));
      end if;

      return Get_Value (E);
   end Emit_Subprogram_Decl;

   ------------------------
   --  Create_Subprogram --
   ------------------------

   function Create_Subprogram (E : Subprogram_Kind_Id) return GL_Value is
      Subp_Type   : constant Type_T      := Create_Subprogram_Type (E);
      Subp_Name   : constant String      := Get_Ext_Name (E);
      Is_Imported : constant Boolean     :=
        Present (Interface_Name (E)) and then No (Address_Clause (E));
      Actual_Name : constant String      :=
        (if   Is_Compilation_Unit (E) and then No (Interface_Name (E))
         then "_ada_" & Subp_Name else Subp_Name);
      RK          : constant Return_Kind := Get_Return_Kind      (E);
      Return_GT   : constant GL_Type     := Full_GL_Type         (E);
      Return_DT   : constant GL_Type     :=
          (if   Is_Access_Type (Return_GT)
           then Full_Designated_GL_Type (Return_GT) else No_GL_Type);
      Pure_Func   : constant Boolean     := Has_Pragma_Pure_Function (E);
      LLVM_Func   : GL_Value             := Get_Dup_Global_Value (E);
      Param_Num   : Natural              := 0;
      Readonly    : Boolean              :=
          Pure_Func or else (Is_Pure (E) and then not Is_Imported);
      UID         : constant Unique_Id   := New_Unique_Id;
      Formal      : Opt_Formal_Kind_Id;

   begin
      Check_Convention (E);

      --  If we've already seen this function name before, verify that we
      --  have the same type. Convert it to it if not.

      if Present (LLVM_Func)
        and then Type_Of (LLVM_Func) /=
          Pointer_Type (Subp_Type, Address_Space)
      then
         LLVM_Func :=
           G_From
             (Pointer_Cast
                (IR_Builder, +LLVM_Func,
                 Pointer_Type (Subp_Type, Address_Space), Subp_Name),
              LLVM_Func);
      elsif No (LLVM_Func) then
         LLVM_Func := Add_Function (Actual_Name, Subp_Type, Full_GL_Type (E));

         --  Define the appropriate linkage

         if not In_Extended_Main_Code_Unit (E) then
            Set_Linkage (LLVM_Func, External_Linkage);
         elsif not Is_Public (E) and then not Is_Imported then
            Set_Linkage (LLVM_Func, Internal_Linkage);
         end if;

         Set_Linker_Section   (LLVM_Func, E);
         Process_Pragmas      (E, LLVM_Func);
         Set_Dup_Global_Value (E, LLVM_Func);

         --  Add function to the table of subprograms that we've created,
         --  so we can add it to the module.

         Created_Subprograms.Append (E);

         --  Now deal with function and parameter attributes.
         --  ??? We don't handle some return value attributes yet.

         if CPU.all /= "generic" then
            Add_Named_Attribute (LLVM_Func, "target-cpu", CPU.all);
         end if;

         if Features.all /= "" then
            Add_Named_Attribute (LLVM_Func, "target-features", Features.all);
         end if;

         Add_Inline_Attribute (LLVM_Func, E);

         if No_Tail_Calls then
            Add_Named_Attribute (LLVM_Func, "disable-tail-calls", "true");
         end if;

         if No_Return (E) then
            Set_Does_Not_Return (LLVM_Func);
            Readonly := False;
         end if;

         if RK = Return_By_Parameter then
            Add_Dereferenceable_Attribute (LLVM_Func, Param_Num, Return_GT);
            Add_Noalias_Attribute         (LLVM_Func, Param_Num);
            Add_Nocapture_Attribute       (LLVM_Func, Param_Num);
            Readonly  := False;
            Param_Num := Param_Num + 1;
         elsif Get_L_Ret_Kind (E) = Subprog_Return then
            if RK = Value_Return and then Is_Access_Type (Return_GT)
              and then not Is_Unconstrained_Array (Return_DT)
              and then Ekind (Return_DT) /= E_Subprogram_Type
            then
               if Can_Never_Be_Null (Return_GT) then
                  Add_Dereferenceable_Attribute (LLVM_Func, Return_DT);
               else
                  Add_Dereferenceable_Or_Null_Attribute (LLVM_Func, Return_DT);
               end if;
            elsif RK = RK_By_Reference then
                  Add_Dereferenceable_Attribute (LLVM_Func, Return_GT);
            end if;
         end if;

         if Enable_Fuzzer then
            Add_Opt_For_Fuzzing_Attribute (LLVM_Func);
         end if;

         if Enable_Address_Sanitizer then
            Add_Sanitize_Address_Attribute (LLVM_Func);
         end if;

         if No_Implicit_Float then
            Add_No_Implicit_Float_Attribute (LLVM_Func);
         end if;

         Formal := First_Formal_With_Extras (E);
         while Present (Formal) loop
            declare
               PK : constant Param_Kind := Get_Param_Kind    (Formal);
               GT : constant GL_Type    := Get_Param_GL_Type (Formal);
               DT : constant GL_Type    :=
                 (if   Is_Access_Type (GT) then Full_Designated_GL_Type (GT)
                  else No_GL_Type);

            begin
               --  If we can write a reference parameter, the function is not
               --  Readonly. Likewise if this is an In parameter of type
               --  Address since we may use it to write through. But allow
               --  Address if we also have Pure_Function.

               if (PK_Is_Reference (PK) and then PK /= PK_By_Ref_In)
                 or else (PK = In_Value and then Is_Descendant_Of_Address (GT)
                            and then not Pure_Func)
               then
                  Readonly := False;
               end if;

               --  Verify that the convention is valid and compute
               --  any parameter attributes.

               Check_Convention (Formal);

               if PK = Activation_Record then
                  Add_Dereferenceable_Attribute (LLVM_Func, Param_Num, DT);
                  Add_Noalias_Attribute         (LLVM_Func, Param_Num);
                  Add_Nocapture_Attribute       (LLVM_Func, Param_Num);
                  Add_Readonly_Attribute        (LLVM_Func, Param_Num);

                  if not Restrictions_On_Target.Set (No_Implicit_Dynamic_Code)
                  then
                     Add_Nest_Attribute         (LLVM_Func, Param_Num);
                  end if;
               elsif PK_Is_Reference (PK)
                 and then (not Is_Unconstrained_Array (GT)
                             or else PK in Foreign_By_Ref |
                                           Foreign_By_Component_Ref)
               then
                  --  Technically, we can take 'Address of a parameter
                  --  and put the address someplace, but that's undefined,
                  --  so we can set this as nocapture.

                  Add_Dereferenceable_Attribute (LLVM_Func, Param_Num, GT);
                  Add_Nocapture_Attribute       (LLVM_Func, Param_Num);

                  if Ekind (Formal) = E_In_Parameter then
                     Add_Readonly_Attribute     (LLVM_Func, Param_Num);
                  end if;

                  --  See RM 6.2(12) for a discussion of when parameters
                  --  can alias. If the mechanism is specified as being by
                  --  reference, they can alias. Likewise if they're
                  --  explicitly marked as aliased.

                  if not Is_Aliased (Formal)
                    and then not Is_By_Reference_Type (GT)
                  then
                     Add_Noalias_Attribute         (LLVM_Func, Param_Num);
                  end if;

               elsif PK_Is_In_Or_Ref (PK) and then Is_Access_Type (GT)
                 and then not Is_Unconstrained_Array (DT)
                 and then Ekind (DT) /= E_Subprogram_Type
               then
                  Readonly := False;

                  if Can_Never_Be_Null (GT) then
                     Add_Dereferenceable_Attribute (LLVM_Func, Param_Num, DT);
                  else
                     Add_Dereferenceable_Or_Null_Attribute (LLVM_Func,
                                                            Param_Num, DT);
                  end if;

                  if Is_Access_Constant (GT) then
                     Add_Readonly_Attribute (LLVM_Func, Param_Num);
                  end if;
               end if;

               if PK_Is_In_Or_Ref (PK) then
                  C_Set_Parameter (UID, Nat (Param_Num), Formal);
                  Param_Num := Param_Num + 1;
               end if;

               Next_Formal_With_Extras (Formal);
            end;
         end loop;

      end if;

      --  Save the value for this subprogram and possibly set it readonly

      Set_Value (E, LLVM_Func);
      C_Set_Function (UID, LLVM_Func);
      C_Set_Entity   (LLVM_Func, E);

      if Readonly then
         Add_Readonly_Attribute (LLVM_Func);
      end if;

      --  Deal with our subprogram name being that of an elab proc

      if No (Ada_Main_Elabb) and then Is_Binder_Elab_Proc (Subp_Name) then
         Ada_Main_Elabb       := LLVM_Func;
         Ada_Main_Elabb_Ident := E;
      end if;

      --  See if we're to make a global constructor or destructor entry for
      --  this subprogram.

      if Present (Get_Pragma (E, Pragma_Linker_Constructor)) then
         Global_Constructors.Append (E);
      elsif Present (Get_Pragma (E, Pragma_Linker_Destructor)) then
         Global_Destructors.Append (E);
      end if;

      --  If this is a CUDA kernel, add the appropriate annotation.

      if Is_CUDA_Kernel (E) then
         Add_Named_Metadata_Operand
           ("nvvm.annotations",
            MD_Node ((1 => Value_As_Metadata (LLVM_Func),
                      2 => MD_String ("kernel"),
                      3 => Const_32_As_Metadata (Uint_1))));
      end if;

      return LLVM_Func;
   end Create_Subprogram;

   --------------
   -- Subp_Ptr --
   --------------

   function Subp_Ptr (N : N_Subexpr_Id) return GL_Value is
   begin
      if Nkind (N) = N_Null then
         return Const_Null (A_Char_GL_Type);
      else
         return Load
           (Get (GEP (A_Char_GL_Type, Emit_LValue (N),
                      (1 => Const_Null_32, 2 => Const_Null_32)),
                 Reference));

      end if;
   end Subp_Ptr;

   ----------------
   -- Enter_Subp --
   ----------------

   procedure Enter_Subp (Func : GL_Value) is
   begin
      Current_Func         := Func;
      Activation_Rec_Param := No_GL_Value;
      Return_Address_Param := No_GL_Value;
      Entry_Block_Allocas  := No_Position_T;
      Position_Builder_At_End (Create_Basic_Block ("entry"));
   end Enter_Subp;

   ----------------
   -- Leave_Subp --
   ----------------

   procedure Leave_Subp is
   begin
      Current_Func := No_GL_Value;
   end Leave_Subp;

   -------------------
   -- Library_Level --
   -------------------

   function Library_Level return Boolean is
     (Current_Func = No_GL_Value);

   ------------------------
   -- Create_Basic_Block --
   ------------------------

   function Create_Basic_Block (Name : String := "") return Basic_Block_T is
   begin
      return Append_Basic_Block (+Current_Func, Name);
   end Create_Basic_Block;

   --------------------------------------------
   -- Output_Global_Constructors_Destructors --
   --------------------------------------------

   procedure Output_Global_Constructors_Destructors is
      Constructors : GL_Value_Array (1 .. Global_Constructors.Last);
      Destructors  : GL_Value_Array (1 .. Global_Destructors.Last);
      Val          : GL_Value;
      Var          : Value_T;

   begin
      if Global_Constructors.Last > 0 then
         for J in 1 .. Global_Constructors.Last loop
            Constructors (J) :=
              Const_Struct ((1 => Const_Int_32 (Uint_1),
                             2 => Get_Value (Global_Constructors.Table (J)),
                             3 => Const_Null (A_Char_GL_Type)),
                            Any_Array_GL_Type, False);
         end loop;

         Val := Const_Array  (Constructors, Any_Array_GL_Type);
         Var := Add_Global   (Module, Type_Of (Val), "llvm.global_ctors");
         Set_Initializer     (Var, +Val);
         Set_Linkage         (Var, Appending_Linkage);
         Set_Global_Constant (Var, True);
      end if;

      if Global_Destructors.Last > 0 then
         for J in 1 .. Global_Destructors.Last loop
            Destructors (J) :=
              Const_Struct ((1 => Const_Int_32 (Uint_1),
                             2 => Get_Value (Global_Destructors.Table (J)),
                             3 => Const_Null (A_Char_GL_Type)),
                            Any_Array_GL_Type, False);
         end loop;

         Val := Const_Array  (Destructors, Any_Array_GL_Type);
         Var := Add_Global   (Module, Type_Of (Val), "llvm.global_dtors");
         Set_Initializer     (Var, +Val);
         Set_Linkage         (Var, Appending_Linkage);
         Set_Global_Constant (Var, True);
      end if;

   end Output_Global_Constructors_Destructors;

   -----------------------------
   -- Add_Functions_To_Module --
   -----------------------------

   procedure Add_Functions_To_Module is
   begin
      for J in 1 .. Created_Subprograms.Last loop
         if not Get_Added_To_Module (Created_Subprograms.Table (J)) then

            --  The functions that we add here don't have a body, so they're
            --  imported. We generally don't want LLVM deduplication for
            --  imported functions because it yields unresolved symbols, but in
            --  Decls_Only mode we have to allow it because the frontend then
            --  doesn't deduplicate Ada subprogram names for us.

            Add_Function_To_Module
              (Get_Value (Created_Subprograms.Table (J)),
               Allow_Deduplication => Decls_Only);
         end if;
      end loop;
   end Add_Functions_To_Module;

end GNATLLVM.Subprograms;
