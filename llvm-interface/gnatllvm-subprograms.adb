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

with Exp_Unst; use Exp_Unst;
with Lib;      use Lib;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Table;    use Table;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Blocks;      use GNATLLVM.Blocks;
with GNATLLVM.Compile;     use GNATLLVM.Compile;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with GNATLLVM.Exprs;       use GNATLLVM.Exprs;
with GNATLLVM.DebugInfo;   use GNATLLVM.DebugInfo;
with GNATLLVM.Records;     use GNATLLVM.Records;
with GNATLLVM.Types;       use GNATLLVM.Types;
with GNATLLVM.Utils;       use GNATLLVM.Utils;
with GNATLLVM.Variables;   use GNATLLVM.Variables;

package body GNATLLVM.Subprograms is

   --  Elaboration entries can be either nodes to be emitted as statements
   --  or expressions to be saved.

   type Elaboration_Entry is record
      N         : Node_Id;
      --  Note to elaborate, possibly as an exppression

      For_Type  : Entity_Id;
      --  If Present, compute N as a value, convert it to this type, and
      --  save the result as the value corresponding to it.
   end record;

   package Elaboration_Table is new Table.Table
     (Table_Component_Type => Elaboration_Entry,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 1024,
      Table_Increment      => 100,
      Table_Name           => "Elaboration_Table");
   --  Table of statements part of the current elaboration procedure

   package Nested_Functions_Table is new Table.Table
     (Table_Component_Type => Node_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Nested_Function_Table");
   --  Table of nested functions to elaborate

   type Intrinsic is record
      Name  : access String;
      Width : Uint;
      Func  : GL_Value;
   end record;
   --  A description of an intrinsic function that we've created

   --  Since we aren't going to be creating all that many different
   --  intrinsic functions, a simple list that we search should be
   --  fast enough.

   package Intrinsic_Functions_Table is new Table.Table
     (Table_Component_Type => Intrinsic,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 5,
      Table_Name           => "Intrinsic_Function_Table");

   Default_Alloc_Fn  : GL_Value := No_GL_Value;
   --  Default memory allocation function

   Default_Free_Fn   : GL_Value := No_GL_Value;
   --  Default memory deallocation function

   Memory_Compare_Fn : GL_Value := No_GL_Value;
   --  Function to compare memory

   Stack_Save_Fn     : GL_Value := No_GL_Value;
   Stack_Restore_Fn  : GL_Value := No_GL_Value;
   --  Functions to save and restore the stack pointer

   function Is_Dynamic_Return (TE : Entity_Id) return Boolean is
     (Ekind (TE) /= E_Void and then Is_Dynamic_Size (TE)
        and then not Is_Unconstrained_Array (TE))
     with Pre => Is_Type_Or_Void (TE);
   --  TE is the return type of a function.  This predicate is true is the
   --  function returns a dynamic sized type.  If TE is an unconstrained array,
   --  the return type of the function will have been changed to an access
   --  to that array, so this must return false.

   function Name_To_RMW_Op
     (S           : String;
      Index       : Integer;
      End_Index   : out Integer;
      Op          : out Atomic_RMW_Bin_Op_T) return Boolean;
   --  See if the string S starting at position Index is the name of
   --  a supported LLVM atomicrmw instruction.  If so, set End_Index
   --  to after the name and Op to the code for the operation and return True.

   function Emit_Sync_Call (N : Node_Id; S : String) return GL_Value
     with Pre  => Nkind (N) in N_Subprogram_Call;
   --  If S is a valid __sync name, emit the LLVM for it and return the
   --  result.  Otherwise, return No_GL_Value.

   function Emit_Intrinsic_Call (N : Node_Id; Subp : Entity_Id) return GL_Value
     with Pre  => Nkind (N) in N_Subprogram_Call;
   --  If Subp is an intrinsic that we know how to handle, emit the LLVM
   --  for it and return the result.  Otherwise, No_GL_Value.

   Ada_Main_Elabb : GL_Value := No_GL_Value;
   --  ???  This a kludge.  We sometimes need an elab proc for Ada_Main and
   --  this can cause confusion with global names.  So if we made it as
   --  part of the processing of a declaration, save it.

   ------------------
   -- Count_Params --
   ------------------

   function Count_Params (E : Entity_Id) return Nat is
      Param : Entity_Id := First_Formal_With_Extras (E);

   begin
      return Cnt : Nat := 0 do
         while Present (Param) loop
            Cnt := Cnt + 1;
            Param := Next_Formal_With_Extras (Param);
         end loop;
      end return;
   end Count_Params;

   ----------------------------
   -- Create_Subprogram_Type --
   ----------------------------

   function Create_Subprogram_Type (Def_Ident   : Entity_Id) return Type_T is
      Return_Type     : constant Entity_Id := Full_Etype (Def_Ident);
      Foreign         : constant Boolean   :=
        Has_Foreign_Convention (Def_Ident);
      Ret_By_Ref      : constant Boolean   := Returns_By_Ref (Def_Ident);
      Takes_S_Link    : constant Boolean   :=
        Needs_Activation_Record (Def_Ident) or else Is_Type (Def_Ident);
      Unc_Return      : constant Boolean   :=
        (Ekind (Return_Type) /= E_Void
           and then Is_Unconstrained_Array (Return_Type));
      LLVM_Return_Typ : Type_T             :=
        (if Ekind (Return_Type) = E_Void
         then Void_Type elsif Unc_Return or else Ret_By_Ref
         then Create_Access_Type (Return_Type)
         else Create_Type (Return_Type));
      Dynamic_Return  : constant Boolean   :=
        not Ret_By_Ref and then Is_Dynamic_Return (Return_Type);
      Orig_Arg_Count  : constant Nat       := Count_Params (Def_Ident);
      Args_Count      : constant Nat       :=
        Orig_Arg_Count + (if Takes_S_Link then 1 else 0) +
          (if Dynamic_Return then 1 else 0);
      Arg_Types       : Type_Array (1 .. Args_Count);
      Param_Ent       : Entity_Id          :=
          First_Formal_With_Extras (Def_Ident);
      J               : Nat                := 1;

   begin
      --  First, Associate an LLVM type for each Ada subprogram parameter

      while Present (Param_Ent) loop
         declare
            Param_Type : constant Node_Id := Full_Etype (Param_Ent);
         begin
            --  If this is an out parameter, or a parameter whose type is
            --  unconstrained, take a pointer to the actual parameter.
            --  If it's a foreign convention, force to an actual pointer
            --  to the type./

            Arg_Types (J) :=
              (if Param_Needs_Ptr (Param_Ent)
               then (if Foreign
                     then Pointer_Type (Create_Type (Param_Type), 0)
                     else Create_Access_Type (Param_Type))
               else Create_Type (Param_Type));
         end;

         J := J + 1;
         Next_Formal_With_Extras (Param_Ent);
      end loop;

      --  Set the argument for the static link, if any

      if Takes_S_Link then
         Arg_Types (Orig_Arg_Count + 1) := Void_Ptr_Type;
      end if;

      --  If the return type has dynamic size, we need to add a parameter
      --  to which we pass the address for the return to be placed in.

      if Dynamic_Return then
         Arg_Types (Arg_Types'Last) := Create_Access_Type (Return_Type);
         LLVM_Return_Typ := Void_Type;
      end if;

      return Fn_Ty (Arg_Types, LLVM_Return_Typ);
   end Create_Subprogram_Type;

   -----------------------------------
   -- Create_Subprogram_Access_Type --
   -----------------------------------

   function Create_Subprogram_Access_Type return Type_T is
   begin
      --  ??? Should we really always use char * for both of these?

      return Build_Struct_Type ((1 => Void_Ptr_Type, 2 => Void_Ptr_Type));
   end Create_Subprogram_Access_Type;

   ---------------------
   -- Build_Intrinsic --
   ---------------------

   function Build_Intrinsic
     (Kind : Overloaded_Intrinsic_Kind;
      Name : String;
      TE   : Entity_Id) return GL_Value
   is
      Width         : constant Uint   := Esize (TE);
      Full_Name     : constant String := Name & UI_Image (Width);
      LLVM_Typ      : constant Type_T := Create_Type (TE);
      Return_TE     : Entity_Id := TE;
      Fun_Ty        : Type_T;
      Result        : GL_Value;

   begin
      for J in 1 .. Intrinsic_Functions_Table.Last loop
         if Intrinsic_Functions_Table.Table (J).Name.all = Name
           and then Intrinsic_Functions_Table.Table (J).Width = Width
         then
            return Intrinsic_Functions_Table.Table (J).Func;
         end if;
      end loop;

      case Kind is
         when Unary =>
            Fun_Ty := Fn_Ty ((1 => LLVM_Typ), LLVM_Typ);

         when Binary =>
            Fun_Ty := Fn_Ty ((1 => LLVM_Typ, 2 => LLVM_Typ), LLVM_Typ);

         when Overflow =>
            Fun_Ty := Fn_Ty
              ((1 => LLVM_Typ, 2 => LLVM_Typ),
               Build_Struct_Type ((1 => LLVM_Typ, 2 => Int_Ty (1))));

         when Memcpy =>
            Return_TE := Standard_Void_Type;
            Fun_Ty := Fn_Ty
              ((1 => Void_Ptr_Type, 2 => Void_Ptr_Type,
                3 => LLVM_Size_Type, 4 => Int_Ty (32), 5 => Int_Ty (1)),
               Void_Type);

         when Memset =>
            Return_TE := Standard_Void_Type;
            Fun_Ty := Fn_Ty
              ((1 => Void_Ptr_Type, 2 => Int_Ty (8), 3 => LLVM_Size_Type,
                4 => Int_Ty (32), 5 => Int_Ty (1)),
               Void_Type);
      end case;

      Result := Add_Function (Full_Name, Fun_Ty, Return_TE);
      Set_Does_Not_Throw (Result);
      Intrinsic_Functions_Table.Append ((new String'(Name), Width, Result));
      return Result;
   end Build_Intrinsic;

   -------------------------
   -- Add_Global_Function --
   -------------------------

   function Add_Global_Function
     (S          : String;
      Subp_Type  : Type_T;
      TE         : Entity_Id;
      Can_Throw  : Boolean := False;
      Can_Return : Boolean := True) return GL_Value
   is
      Func : GL_Value := Get_Dup_Global_Value (S);

   begin
      --  If we've already built a function for this, return it, after
      --  being sure it's in the same type that we need.

      if Present (Func) then
         return G_From (Pointer_Cast (IR_Builder,
                                      LLVM_Value (Func),
                                      Pointer_Type (Subp_Type, 0), S),
                        Func);
      else
         Func := Add_Function (S, Subp_Type, TE);
         if not Can_Throw then
            Set_Does_Not_Throw (Func);
         end if;

         if not Can_Return then
            Set_Does_Not_Return (Func);
         end if;

         Set_Dup_Global_Value (S, Func);
         return Func;
      end if;

   end Add_Global_Function;

   --------------------------
   -- Get_Default_Alloc_Fn --
   --------------------------

   function Get_Default_Alloc_Fn return GL_Value is
   begin
      if No (Default_Alloc_Fn) then
         Default_Alloc_Fn :=
           Add_Global_Function ("__gnat_malloc", Fn_Ty ((1 => LLVM_Size_Type),
                                                        Void_Ptr_Type),
                                Standard_A_Char);
      end if;

      return Default_Alloc_Fn;
   end Get_Default_Alloc_Fn;

   -------------------------
   -- Get_Default_Free_Fn --
   -------------------------

   function Get_Default_Free_Fn return GL_Value is
   begin
      if No (Default_Free_Fn) then
         Default_Free_Fn :=
           Add_Global_Function ("__gnat_free",
                                Fn_Ty ((1 => Void_Ptr_Type), Void_Type),
                                Standard_Void_Type);
      end if;

      return Default_Free_Fn;
   end Get_Default_Free_Fn;

   ---------------------------
   -- Get_Memory_Compare_Fn --
   ---------------------------

   function Get_Memory_Compare_Fn return GL_Value is
   begin
      if No (Memory_Compare_Fn) then
         Memory_Compare_Fn := Add_Global_Function
           ("memcmp",
            Fn_Ty ((1 => Void_Ptr_Type, 2 => Void_Ptr_Type,
                    3 => LLVM_Size_Type),
                   Create_Type (Standard_Integer)),
            Standard_Integer);
      end if;

      return Memory_Compare_Fn;
   end Get_Memory_Compare_Fn;

   -----------------------
   -- Get_Stack_Save_Fn --
   -----------------------

   function Get_Stack_Save_Fn return GL_Value is
   begin
      if No (Stack_Save_Fn) then
         Stack_Save_Fn := Add_Function
           ("llvm.stacksave", Fn_Ty ((1 .. 0 => <>), Void_Ptr_Type),
            Standard_A_Char);
         Set_Does_Not_Throw (Stack_Save_Fn);
      end if;

      return Stack_Save_Fn;
   end Get_Stack_Save_Fn;

   --------------------------
   -- Get_Stack_Restore_Fn --
   --------------------------

   function Get_Stack_Restore_Fn return GL_Value is
   begin
      if No (Stack_Restore_Fn) then
         Stack_Restore_Fn := Add_Function
           ("llvm.stackrestore",
            Fn_Ty ((1 => Void_Ptr_Type), Void_Type), Standard_Void_Type);
         Set_Does_Not_Throw (Stack_Restore_Fn);
      end if;

      return Stack_Restore_Fn;
   end Get_Stack_Restore_Fn;

   --------------------------------
   -- Get_From_Activation_Record --
   --------------------------------

   function Get_From_Activation_Record (E : Entity_Id) return GL_Value is
   begin
      --  See if this is a type of object that's passed in activation
      --  records, if this object is allocated space in an activation
      --  record, if we have an activation record as a parameter of this
      --  subprogram, and if this isn't a reference to the variable
      --  in its own subprogram.  If so, get the object from the activation
      --  record.  We return the address from the record so we can either
      --  give an LValue or an expression.  ???  Note that we only handle
      --  one level: there's no code here to go up multiple levels or
      --  even detect that we need to.

      if Ekind_In (E, E_Constant, E_Variable, E_In_Parameter, E_Out_Parameter,
                   E_In_Out_Parameter, E_Loop_Parameter)
        and then Present (Activation_Record_Component (E))
        and then Present (Activation_Rec_Param)
        and then Get_Value (Enclosing_Subprogram (E)) /= Current_Func
      then
         declare
            TE            : constant Entity_Id := Full_Etype (E);
            Component     : constant Entity_Id :=
              Activation_Record_Component (E);
            Pointer       : constant GL_Value  :=
              Record_Field_Offset (Activation_Rec_Param, Component);
            Value         : constant GL_Value  := Load (Pointer);

         begin
            --  If TE is unconstrained, we have an access type, which is a
            --  fat pointer.  Convert it to a reference to the underlying
            --  type.  Otherwise, this is a System.Address which needs to
            --  be converted to a pointer.

            if Is_Unconstrained_Array (TE) then
               return From_Access (Value);
            else
               return Int_To_Ref (Value, TE);
            end if;
         end;
      else
         return No_GL_Value;
      end if;
   end Get_From_Activation_Record;

   -------------------
   -- Emit_One_Body --
   -------------------

   procedure Emit_One_Body (N : Node_Id) is
      Spec            : constant Node_Id   := Get_Acting_Spec (N);
      Func            : constant GL_Value  := Emit_Subprogram_Decl (Spec);
      Def_Ident       : constant Entity_Id := Defining_Entity (Spec);
      Return_Typ      : constant Entity_Id := Full_Etype (Def_Ident);
      Void_Return_Typ : constant Boolean   := Ekind (Return_Typ) = E_Void;
      Ret_By_Ref      : constant Boolean   := Returns_By_Ref (Def_Ident);
      Dyn_Return      : constant Boolean   :=
        not Ret_By_Ref and then Is_Dynamic_Return (Return_Typ);
      Void_Return     : constant Boolean   := Void_Return_Typ or Dyn_Return;
      Param_Num       : Natural            := 0;
      Param           : Entity_Id;
      LLVM_Param      : GL_Value;

   begin
      Current_Subp := Def_Ident;
      Enter_Subp (Func);
      Push_Debug_Scope
        (Create_Subprogram_Debug_Info (Func, Def_Ident, N,
                                       Get_Name_String (Chars (Def_Ident)),
                                       Get_Ext_Name (Def_Ident)));

      Push_Block;
      Param := First_Formal_With_Extras (Def_Ident);
      while Present (Param) loop
         declare
            Is_Ref : constant Boolean         := Param_Needs_Ptr (Param);
            TE     : constant Entity_Id       := Full_Etype (Param);
            R      : constant GL_Relationship :=
              (if Is_Ref
               then (if Is_Unconstrained_Array (TE) then Fat_Pointer
                     else Reference)
               else Data);
         begin
            LLVM_Param :=
              G (Get_Param (LLVM_Value (Func), unsigned (Param_Num)), TE, R);

            --  Define a name for the parameter Param (which is the
            --  Param_Num'th parameter), and associate the corresponding
            --  LLVM value to its entity.

            Set_Value_Name (LLVM_Value (LLVM_Param), Get_Name (Param));

            --  Add the parameter to the environnment

            Set_Value (Param, LLVM_Param);

            if Ekind (Param) = E_In_Parameter
              and then Is_Activation_Record (Param)
            then
               Activation_Rec_Param := LLVM_Param;
            end if;

            Param_Num := Param_Num + 1;
            Next_Formal_With_Extras (Param);
         end;
      end loop;

      --  If the return type has dynamic size, we've added a parameter
      --  that's passed the address to which we want to copy our return
      --  value.

      if Dyn_Return then
         LLVM_Param := G (Get_Param (LLVM_Value (Func),
                                     unsigned (Param_Num)),
                          Return_Typ,
                          (if Is_Unconstrained_Array (Return_Typ)
                           then Fat_Pointer else Reference));
         Set_Value_Name (LLVM_Value (LLVM_Param), "return");
         Return_Address_Param := LLVM_Param;
      end if;

      Emit_Decl_Lists (Declarations (N), No_List);
      Emit (Handled_Statement_Sequence (N));

      --  If the last instrution isn't a terminator, add a return, but
      --  use an access type if this is a dynamic type or a return by ref.

      if not Are_In_Dead_Code then
         if Void_Return or Dyn_Return then
            Build_Ret_Void;
         elsif Ret_By_Ref or else Is_Dynamic_Size (Return_Typ) then
            Build_Ret (Get_Undef_Ref (Return_Typ));
         else
            Build_Ret (Get_Undef (Return_Typ));
         end if;
      end if;

      Pop_Block;

      --  If we're in dead code here, it means that we made a label for the
      --  end of a block, but didn't do anything with it.  So mark it as
      --  unreachable.  ??? We need to sort this out sometime.

      if not Are_In_Dead_Code then
         Build_Unreachable;
      end if;

      Pop_Debug_Scope;
      Leave_Subp;
      Current_Subp := Empty;
   end Emit_One_Body;

   ----------------------
   -- Add_To_Elab_Proc --
   ----------------------

   procedure Add_To_Elab_Proc (N : Node_Id; For_Type : Entity_Id := Empty) is
   begin
      if Elaboration_Table.Last = 0
        or else Elaboration_Table.Table (Elaboration_Table.Last).N /= N
      then
         Elaboration_Table.Append ((N => N, For_Type => For_Type));
      end if;
   end Add_To_Elab_Proc;

   --------------------
   -- Emit_Elab_Proc --
   --------------------

   procedure Emit_Elab_Proc
     (N : Node_Id; Stmts : Node_Id; CU : Node_Id; Suffix : String) is
      U          : constant Node_Id  := Defining_Unit_Name (N);
      Unit       : constant Node_Id  :=
        (if Nkind (U) = N_Defining_Program_Unit_Name
         then Defining_Identifier (U) else U);
      S_List     : constant List_Id  :=
        (if No (Stmts) then No_List else Statements (Stmts));
      Name       : constant String   :=
        Get_Name_String (Chars (Unit)) & "___elab" & Suffix;
      Work_To_Do : constant Boolean  :=
        Elaboration_Table.Last /= 0 or else Has_Non_Null_Statements (S_List);
      Elab_Type  : constant Type_T   := Fn_Ty ((1 .. 0 => <>), Void_Type);
      LLVM_Func  : GL_Value;

   begin
      --  If nothing to elaborate, do nothing

      if Nkind (CU) /= N_Compilation_Unit or else not Work_To_Do then
         return;
      end if;

      --  Otherwise, show there will be elaboration code and emit it

      if Nkind (CU) = N_Compilation_Unit then
         Set_Has_No_Elaboration_Code (CU, False);
      end if;

      if Name = "ada_main___elabb" and Present (Ada_Main_Elabb) then
         LLVM_Func := Ada_Main_Elabb;
      else
         LLVM_Func := Add_Function (Name, Elab_Type, Standard_Void_Type);
      end if;

      Enter_Subp (LLVM_Func);
      Push_Debug_Scope
        (Create_Subprogram_Debug_Info
           (LLVM_Func, Unit, N, Get_Name_String (Chars (Unit)), Name));
      Set_Debug_Pos_At_Node (N);
      Push_Block;
      In_Elab_Proc := True;

      for J in 1 .. Elaboration_Table.Last loop
         declare
            Stmt : constant Node_Id   := Elaboration_Table.Table (J).N;
            Typ  : constant Entity_Id := Elaboration_Table.Table (J).For_Type;

         begin
            if Present (Typ) then
               Set_Value (Stmt, Build_Type_Conversion (Stmt, Typ));
            else
               --  If Stmt is an N_Handled_Sequence_Of_Statements, it
               --  must have come from a package body.  Make a block around
               --  it so exceptions will work properly, if needed.

               if Nkind (Stmt) = N_Handled_Sequence_Of_Statements then
                  Push_Block;
                  Emit (Stmt);
                  Pop_Block;
               else
                  Emit (Stmt);
               end if;
            end if;
         end;
      end loop;

      --  Emit the statements after clearing the special code flag since
      --  we want to handle them normally: this will be the first time we
      --  see them, unlike any that were previously partially processed
      --  as declarations.

      In_Elab_Proc := False;
      Elaboration_Table.Set_Last (0);
      Start_Block_Statements (Empty, No_List);
      Emit (S_List);
      Build_Ret_Void;
      Pop_Block;
      Pop_Debug_Scope;
      Leave_Subp;
   end Emit_Elab_Proc;

   --------------------------
   -- Emit_Subprogram_Body --
   --------------------------

   procedure Emit_Subprogram_Body (N : Node_Id) is
      Nest_Table_First : constant Nat := Nested_Functions_Table.Last + 1;

   begin
      --  If we're not at library level, this a nested function.  Defer it
      --  until we complete elaboration of the enclosing function.  But do
      --  ensure that the spec has been elaborated.

      if not Library_Level then
         Discard (Emit_Subprogram_Decl (Get_Acting_Spec (N)));
         Nested_Functions_Table.Append (N);
         return;
      end if;

      --  Otherwise, elaborate this function and then any nested functions
      --  within in.

      Emit_One_Body (N);

      for J in Nest_Table_First .. Nested_Functions_Table.Last loop
         Emit_Subprogram_Body (Nested_Functions_Table.Table (J));
      end loop;

      Nested_Functions_Table.Set_Last (Nest_Table_First);
   end Emit_Subprogram_Body;

   ---------------------------
   -- Emit_Return_Statement --
   ---------------------------

   procedure Emit_Return_Statement (N : Node_Id) is
   begin
      --  First, generate any neded fixups for this.  Then see what kind of
      --  return we're doing.

      Emit_Fixups_For_Return;
      if Present (Expression (N)) then
         declare
            Expr : constant Node_Id :=
              Strip_Complex_Conversions (Expression (N));
            TE   : constant Entity_Id := Full_Etype (Current_Subp);

         begin
            --  If there's a parameter for the address to which to copy the
            --  return value, do the copy instead of returning the value.

            if Present (Return_Address_Param) then
               Emit_Assignment (Return_Address_Param, Expr,
                                No_GL_Value, True, True);
               Build_Ret_Void;

               --  If this function returns unconstrained, allocate memory
               --  for the return value, copy the data to be returned to
               --  there, and return an access (fat pointer) to the value.
               --  Is this is a return-by-reference function, do that.

            elsif Returns_By_Ref (Current_Subp) then
               Build_Ret (Convert_To_Access_To (Emit_LValue (Expr), TE));

            elsif Is_Unconstrained_Array (TE) then
               Build_Ret
                 (Heap_Allocate_For_Type
                    (TE, Full_Etype (Expr), Emit_Expression (Expr),
                     Procedure_To_Call (N), Storage_Pool (N)));
            else
               Build_Ret (Build_Type_Conversion (Expr, TE));
            end if;
         end;
      else
         Build_Ret_Void;
      end if;
   end Emit_Return_Statement;

   ---------------------
   -- Get_Static_Link --
   ---------------------

   function Get_Static_Link (N : Node_Id) return GL_Value is
      Subp        : constant Entity_Id  :=
        (if Nkind (N) in N_Entity then N else Entity (N));
      Parent      : constant Entity_Id  := Enclosing_Subprogram (Subp);
      Ent_Caller  : Subp_Entry;
      Ent         : Subp_Entry;
      Result      : GL_Value;

   begin

      if Present (Parent) then
         Ent        := Subps.Table (Subp_Index (Parent));
         Ent_Caller := Subps.Table (Subp_Index (Current_Subp));

         if Parent = Current_Subp then
            Result := (if Present (Ent.ARECnP)
                       then Get (Get_Value (Ent.ARECnP), Data)
                       else Get_Undef (Standard_A_Char));
         elsif No (Ent_Caller.ARECnF) then
            return Get_Undef (Standard_A_Char);
         else
            Result := Get_Value (Ent_Caller.ARECnF);

            --  Go levels up via the ARECnU field if needed.  Each time,
            --  get the new type from the first field of the record that
            --  it points to.

            for J in 1 .. Ent_Caller.Lev - Ent.Lev - 1 loop
               Result :=
                 Load (GEP (Full_Etype (First_Component_Or_Discriminant
                                          (Full_Designated_Type (Result))),
                            Result, (1 => Const_Null_32, 2 => Const_Null_32),
                            "ARECnF.all.ARECnU"));
            end loop;
         end if;

         return Pointer_Cast (Result, Standard_A_Char, "static-link");
      else
         return Get_Undef (Standard_A_Char);
      end if;
   end Get_Static_Link;

   ------------------------
   -- Call_Alloc_Dealloc --
   ------------------------

   procedure Call_Alloc_Dealloc (Proc : Entity_Id; Args : GL_Value_Array) is
      Func           : constant GL_Value := Emit_LValue (Proc, Clear => False);
      Args_With_Link : GL_Value_Array (Args'First .. Args'Last + 1);
      S_Link         : GL_Value;
   begin
      if Subps_Index (Proc) /= Uint_0
        and then Present (Subps.Table (Subp_Index (Proc)).ARECnF)
      then
         --  This needs a static link.  Get it, convert it to the precise
         --  needed type, and then create the new argument list.

         S_Link := Pointer_Cast (Get_Static_Link (Proc),
                                 Full_Etype (Extra_Formals (Proc)));
         Args_With_Link (Args'Range) := Args;
         Args_With_Link (Args_With_Link'Last) := S_Link;
         Call (Func, Args_With_Link);
      else
         Call (Func, Args);
      end if;
   end Call_Alloc_Dealloc;

   --------------------
   -- Name_To_RMW_Op --
   --------------------

   function Name_To_RMW_Op
     (S           : String;
      Index       : Integer;
      End_Index   : out Integer;
      Op          : out Atomic_RMW_Bin_Op_T) return Boolean
   is
      type RMW_Op is record
         Length : Integer;
         Name   : String (1 .. 4);
         Op     : Atomic_RMW_Bin_Op_T;
      end record;
      type RMW_Op_Array is array (Integer range <>) of RMW_Op;

      Len : Integer;
      Ops : constant RMW_Op_Array :=
        ((4, "xchg", Atomic_RMW_Bin_Op_Xchg),
         (3, "add ", Atomic_RMW_Bin_Op_Add),
         (3, "sub ", Atomic_RMW_Bin_Op_Sub),
         (3, "and ", Atomic_RMW_Bin_Op_And),
         (4, "nand", Atomic_RMW_Bin_Op_Nand),
         (2, "or  ", Atomic_RMW_Bin_Op_Or),
         (3, "xor ", Atomic_RMW_Bin_Op_Xor),
         (3, "max ", Atomic_RMW_Bin_Op_Max),
         (3, "min ", Atomic_RMW_Bin_Op_Min),
         (4, "umax", Atomic_RMW_Bin_Op_U_Max),
         (4, "umin", Atomic_RMW_Bin_Op_U_Min));

   begin
      for J in Ops'Range loop
         Len := Ops (J).Length;
         if S'Last > Index + Len - 1
           and then S (Index .. Index + Len - 1) = Ops (J).Name (1 .. Len)
         then
            End_Index := Index + Len;
            Op        := Ops (J).Op;
            return True;
         end if;
      end loop;

      return False;
   end Name_To_RMW_Op;

   --------------------
   -- Emit_Sync_Call --
   --------------------

   function Emit_Sync_Call (N : Node_Id; S : String) return GL_Value is
      Ptr        : Node_Id;
      Val        : Node_Id;
      Op         : Atomic_RMW_Bin_Op_T;
      Op_Back    : Boolean;
      Index      : Integer := S'First + 7;
      New_Index  : Integer;
      TE, BT, PT : Entity_Id;
      Type_Size  : ULL;
      Value      : GL_Value;
      Result     : GL_Value;

   begin
      --  This is supposedly a __sync builtin.  Parse it to see what it
      --  tells us to do.  If anything is wrong with the builtin or its
      --  operands, just return No_GL_Value and a normal call will result,
      --  which will produce a link error.  ???  We could produce warnings
      --  here.
      --
      --  We need to have "Op_and_fetch" or "fetch_and_Op".

      if Name_To_RMW_Op (S, Index, New_Index, Op)
        and then S'Last > New_Index + 9
        and then S (New_Index .. New_Index + 9) = "_and_fetch"
      then
         Op_Back := True;
         Index   := New_Index + 10;
      elsif S'Last > Index + 9 and then S (Index .. Index + 9) = "fetch_and_"
        and then Name_To_RMW_Op (S, Index + 10, New_Index, Op)
      then
         Op_Back := False;
         Index   := New_Index;
      else
         return No_GL_Value;
      end if;

      --  There must be exactly two actuals with the second an elementary
      --  type and the first an access type to it.

      Ptr := First_Actual (N);
      if No (Ptr) then
         return No_GL_Value;
      end if;

      Val := Next_Actual (Ptr);
      if No (Val) or else Present (Next_Actual (Val)) then
         return No_GL_Value;
      end if;

      TE  := Full_Etype (Val);
      PT  := Full_Etype (Ptr);
      BT  := Implementation_Base_Type (TE);
      if not Is_Elementary_Type (TE)
        or else not Is_Access_Type (PT)
        or else Implementation_Base_Type (Full_Designated_Type (PT)) /= BT
      then
         return No_GL_Value;
      end if;

      --  Finally, verify that the size of the type matches the builtin name
      if S'Last < Index + 1 then
         return No_GL_Value;
      end if;

      Type_Size := Get_LLVM_Type_Size (Create_Type (TE));
      if not (S (Index .. Index + 1) = "_1" and then Type_Size = 1)
        and then not (S (Index .. Index + 1) = "_2" and then Type_Size = 2)
        and then not (S (Index .. Index + 1) = "_4" and then Type_Size = 4)
        and then not (S (Index .. Index + 1) = "_8" and then Type_Size = 8)
      then
         return No_GL_Value;
      end if;

      --  Now we can emit the operation

      Value  := Emit_Expression (Val);
      Result := Atomic_RMW (Op, Emit_Expression (Ptr), Value);

      --  If we want the value before the operation, we're done.  Otherwise,
      --  we have to do the operation.

      if not Op_Back then
         return Result;
      end if;

      case Op is
         when Atomic_RMW_Bin_Op_Xchg =>
            return Result;

         when Atomic_RMW_Bin_Op_Add =>
            return NSW_Add (Result, Value);

         when Atomic_RMW_Bin_Op_Sub =>
            return NSW_Sub (Result, Value);

         when Atomic_RMW_Bin_Op_And =>
            return Build_And (Result, Value);

         when Atomic_RMW_Bin_Op_Nand =>
            return Build_Not (Build_And (Result, Value));

         when Atomic_RMW_Bin_Op_Or =>
            return Build_Or (Result, Value);

         when Atomic_RMW_Bin_Op_Xor =>
            return Build_Xor (Result, Value);

         when Atomic_RMW_Bin_Op_Max | Atomic_RMW_Bin_Op_U_Max =>
            return Build_Max (Result, Value);

         when Atomic_RMW_Bin_Op_Min | Atomic_RMW_Bin_Op_U_Min =>
            return Build_Min (Result, Value);
      end case;

   end Emit_Sync_Call;

   -------------------------
   -- Emit_Intrinsic_Call --
   -------------------------

   function Emit_Intrinsic_Call (N : Node_Id; Subp : Entity_Id) return GL_Value
   is
      Fn_Name : constant String := Get_Ext_Name (Subp);

   begin
      --  First see if this is a __sync class of subprogram

      if Fn_Name'Length > 7 and then Fn_Name (1 .. 7) = "__sync_" then
         return Emit_Sync_Call (N, Fn_Name);
      end if;

      --  ??? That's all we support for the moment

      return No_GL_Value;
   end Emit_Intrinsic_Call;

   ---------------
   -- Emit_Call --
   ---------------

   function Emit_Call (N : Node_Id) return GL_Value is
      Subp           : Node_Id            := Name (N);
      Our_Return_Typ : constant Entity_Id := Full_Etype (N);
      Direct_Call    : constant Boolean   :=
        Nkind (Subp) /= N_Explicit_Dereference;
      Subp_Typ       : constant Entity_Id :=
        (if Direct_Call then Entity (Subp) else Full_Etype (Subp));
      Return_Typ     : constant Entity_Id := Full_Etype (Subp_Typ);
      Foreign        : constant Boolean   := Has_Foreign_Convention (Subp_Typ);
      Ret_By_Ref     : constant Boolean   :=
        Returns_By_Ref ((if Direct_Call then Entity (Subp) else Subp_Typ));
      Dynamic_Return : constant Boolean   :=
        not Ret_By_Ref and then Is_Dynamic_Return (Return_Typ);
      Param          : Node_Id;
      P_Type         : Entity_Id;
      Actual         : Node_Id;

      --  If it's not an identifier, it must be an access to a subprogram and
      --  in such a case, it must accept a static link.

      This_Takes_S_Link : constant Boolean := not Direct_Call;
      Orig_Arg_Count : constant Nat        := Count_Params (Subp_Typ);
      Args_Count     : constant Nat        :=
        Orig_Arg_Count + (if This_Takes_S_Link then 1 else 0) +
          (if Dynamic_Return then 1 else 0);
      Args           : GL_Value_Array (1 .. Args_Count);
      Idx            : Nat                 := 1;
      S_Link         : GL_Value;
      LLVM_Func      : GL_Value;
      Arg            : GL_Value;
      Result         : GL_Value;

   begin

      if Direct_Call then
         Subp := Entity (Subp);
      end if;

      --  See if this is an instrinsic subprogram that we handle.  We're
      --  done if so.

      if Direct_Call and then Is_Intrinsic_Subprogram (Subp) then
         Result := Emit_Intrinsic_Call (N, Subp);
         if Present (Result) then
            return Result;
         end if;
      end if;

      LLVM_Func := Emit_LValue (Subp);
      if This_Takes_S_Link then
         S_Link    := Get (LLVM_Func, Reference_To_Activation_Record);
         LLVM_Func := Get (LLVM_Func, Reference);
      end if;

      Param  := First_Formal_With_Extras (Subp_Typ);
      Actual := First_Actual (N);
      while Present (Actual) loop
         P_Type := Full_Etype (Param);

         --  We have two cases: if the param isn't passed indirectly, convert
         --  the value to the parameter's type.  If it is, then convert the
         --  pointer to being a pointer to the parameter's type.  The way we
         --  do this depends on whether the subprogram has foreign convension
         --  or not.

         if Param_Needs_Ptr (Param) then
            Arg := Emit_LValue (Actual);
            if Foreign then
               Arg := Ptr_To_Ref (Get (Arg, Reference), P_Type);
            else
               Arg := Convert_To_Access_To (Arg, P_Type);
            end if;

            Args (Idx) := Arg;
         else
            Args (Idx) := Build_Type_Conversion (Actual, P_Type);
         end if;

         Idx := Idx + 1;
         Next_Actual (Actual);
         Next_Formal_With_Extras (Param);
         pragma Assert (No (Actual) = No (Param));
      end loop;

      --  Set the argument for the static link, if any

      if This_Takes_S_Link then
         Args (Orig_Arg_Count + 1) := S_Link;
      end if;

      --  Add a pointer to the location of the return value if the return
      --  type is of dynamic size.

      if Dynamic_Return then
         Args (Args'Last) :=
           Allocate_For_Type (Return_Typ, Return_Typ, Name => "call-return");
      end if;

      --  If the return type is of dynamic size, call as a procedure and
      --  return the address we set as the last parameter.

      if Dynamic_Return then
         Call (LLVM_Func, Args);
         return Convert_To_Access_To (Args (Args'Last), Our_Return_Typ);
      elsif Ret_By_Ref or else Is_Unconstrained_Array (Return_Typ) then
         return Call_Ref (LLVM_Func, Return_Typ, Args);
      elsif Ekind (Return_Typ) = E_Void then
         Call (LLVM_Func, Args);
         return No_GL_Value;
      else
         return Call (LLVM_Func, Return_Typ, Args);
      end if;
   end Emit_Call;

   --------------------------
   -- Emit_Subprogram_Decl --
   --------------------------

   function Emit_Subprogram_Decl (N : Node_Id) return GL_Value is
      Def_Ident : constant Entity_Id := Defining_Entity (N);
   begin
      if not Has_Value (Def_Ident) then
         Set_Value (Def_Ident, Create_Subprogram (Def_Ident));
      end if;

      return Get_Value (Def_Ident);
   end Emit_Subprogram_Decl;

   ------------------------
   --  Create_Subprogram --
   ------------------------

   function Create_Subprogram (Def_Ident : Entity_Id) return GL_Value is
      Subp_Type   : constant Type_T   := Create_Subprogram_Type (Def_Ident);
      Subp_Name   : constant String   := Get_Ext_Name (Def_Ident);
      Actual_Name : constant String   :=
        (if Is_Compilation_Unit (Def_Ident)
           and then No (Interface_Name (Def_Ident))
         then "_ada_" & Subp_Name else Subp_Name);
      LLVM_Func   : GL_Value          := Get_Dup_Global_Value (Def_Ident);

   begin
      --  If we've already seen this function name before, verify that we
      --  have the same type.  Convert it to it if not.

      if Present (LLVM_Func)
        and then Type_Of (LLVM_Func) /= Pointer_Type (Subp_Type, 0)
      then
         LLVM_Func := G_From (Pointer_Cast (IR_Builder,
                                            LLVM_Value (LLVM_Func),
                                            Pointer_Type (Subp_Type, 0),
                                            Subp_Name),
                              LLVM_Func);
      elsif No (LLVM_Func) then
         LLVM_Func := Add_Function (Actual_Name,
                                    Subp_Type, Full_Etype (Def_Ident));
         --  Define the appropriate linkage

         if not In_Extended_Main_Code_Unit (Def_Ident) then
            Set_Linkage (LLVM_Func, External_Linkage);
         elsif not Is_Public (Def_Ident) then
            Set_Linkage (LLVM_Value (LLVM_Func), Internal_Linkage);
         end if;

         Set_Dup_Global_Value (Def_Ident, LLVM_Func);
      end if;

      Set_Value (Def_Ident, LLVM_Func);

      if Subp_Name = "ada_main___elabb" then
         Ada_Main_Elabb := LLVM_Func;
      end if;

      return LLVM_Func;
   end Create_Subprogram;

   --------------
   -- Subp_Ptr --
   --------------

   function Subp_Ptr (N : Node_Id) return GL_Value is
   begin
      if Nkind (N) = N_Null then
         return Const_Null (Standard_A_Char);
      else
         return Load
           (GEP (Standard_A_Char, Emit_LValue (N),
                 (1 => Const_Null_32, 2 => Const_Null_32)));
      end if;
   end Subp_Ptr;

   ----------------
   -- Enter_Subp --
   ----------------

   procedure Enter_Subp (Func : GL_Value) is
   begin
      Current_Func := Func;
      Activation_Rec_Param := No_GL_Value;
      Return_Address_Param := No_GL_Value;
      Position_Builder_At_End (IR_Builder, Create_Basic_Block ("entry"));
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
      return Append_Basic_Block_In_Context
        (LLVM_Context, LLVM_Value (Current_Func), Name);
   end Create_Basic_Block;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register_Global_Name ("__gnat_free");
      Register_Global_Name ("__gnat_malloc");
      Register_Global_Name ("memcmp");
   end Initialize;

end GNATLLVM.Subprograms;
