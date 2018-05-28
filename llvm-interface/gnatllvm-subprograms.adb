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

with Interfaces.C;            use Interfaces.C;

with Exp_Unst; use Exp_Unst;
with Lib;      use Lib;
with Sem_Util; use Sem_Util;
with Sinput;   use Sinput;
with Stand;    use Stand;
with Table;    use Table;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Blocks;      use GNATLLVM.Blocks;
with GNATLLVM.Compile;     use GNATLLVM.Compile;
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

   LCH_Fn            : GL_Value := No_GL_Value;
   --  Last-chance handler

   function Is_Dynamic_Return (TE : Entity_Id) return Boolean is
     (Ekind (TE) /= E_Void and then Is_Dynamic_Size (TE)
        and then not Is_Unconstrained_Array (TE))
     with Pre => Is_Type_Or_Void (TE);
   --  TE is the return type of a function.  This predicate is true is the
   --  function returns a dynamic sized type.  If TE is an unconstrained array,
   --  the return type of the function will have been changed to an access
   --  to that array, so this must return false.

   Ada_Main_Elabb : GL_Value := No_GL_Value;
   --  ???  This a kludge.  We sometimes need an elab proc for Ada_Main and
   --  this can cause confusion with global names.  So if we made it as
   --  part of the processing of a declaration, save it.

   ------------------
   -- Count_Params --
   ------------------

   function Count_Params (E : Entity_Id) return Nat is
      Cnt   : Nat := 0;
      Param : Entity_Id := First_Formal_With_Extras (E);

   begin
      while Present (Param) loop
         Cnt := Cnt + 1;
         Param := Next_Formal_With_Extras (Param);
      end loop;

      return Cnt;
   end Count_Params;

   ----------------------------
   -- Create_Subprogram_Type --
   ----------------------------

   function Create_Subprogram_Type (Def_Ident   : Entity_Id) return Type_T is
      Return_Type     : constant Entity_Id := Full_Etype (Def_Ident);
      Ret_By_Ref      : constant Boolean   := Returns_By_Ref (Def_Ident);
      Takes_S_Link    : constant Boolean   :=
        Needs_Activation_Record (Def_Ident);
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

            Arg_Types (J) :=
              (if Param_Needs_Ptr (Param_Ent)
               then Create_Access_Type (Param_Type)
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

   --------------------------
   -- Get_Default_Alloc_Fn --
   --------------------------

   function Get_Default_Alloc_Fn return GL_Value is
   begin
      if No (Default_Alloc_Fn) then
         Default_Alloc_Fn :=
           Add_Function ("malloc", Fn_Ty ((1 => LLVM_Size_Type),
                                          Void_Ptr_Type),
                         Standard_A_Char);
         Set_Does_Not_Throw (Default_Alloc_Fn);
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
           Add_Function ("free",
                         Fn_Ty ((1 => Void_Ptr_Type, 2 => LLVM_Size_Type),
                                Void_Type),
                         Standard_Void_Type);
         Set_Does_Not_Throw (Default_Free_Fn);
      end if;

      return Default_Free_Fn;
   end Get_Default_Free_Fn;

   ---------------------------
   -- Get_Memory_Compare_Fn --
   ---------------------------

   function Get_Memory_Compare_Fn return GL_Value is
   begin
      if No (Memory_Compare_Fn) then
         Memory_Compare_Fn := Add_Function
           ("memcmp",
            Fn_Ty ((1 => Void_Ptr_Type, 2 => Void_Ptr_Type,
                    3 => LLVM_Size_Type),
                   Create_Type (Standard_Integer)),
            Standard_Integer);
         Set_Does_Not_Throw (Memory_Compare_Fn);
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

   ----------------
   -- Get_LCH_Fn --
   ----------------

   function Get_LCH_Fn return GL_Value is
   begin
      if No (LCH_Fn) then
         LCH_Fn := Add_Function
           ("__gnat_last_chance_handler",
            Fn_Ty ((1 => Void_Ptr_Type,
                    2 => Create_Type (Standard_Integer)),
                   Void_Type), Standard_Void_Type);
      end if;

      return LCH_Fn;
   end Get_LCH_Fn;

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

         begin
            pragma Assert (not Is_Unconstrained_Array (TE));
            --  This can't work for unconstrained types since we lose bounds

            return Int_To_Ref (Load (Pointer), TE);
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
            Result := Get (Get_Value (Ent.ARECnP), Data);
         elsif No (Ent_Caller.ARECnF) then
            return Const_Null (Standard_A_Char);
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
         return Const_Null (Standard_A_Char);
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

   ----------------------
   -- Emit_LCH_Call_If --
   ----------------------

   procedure Emit_LCH_Call_If (V : GL_Value; N : Node_Id) is
      BB_Then  : constant Basic_Block_T := Create_Basic_Block ("raise");
      BB_Next  : constant Basic_Block_T := Create_Basic_Block;

   begin
      Build_Cond_Br (V, BB_Then, BB_Next);
      Position_Builder_At_End (BB_Then);
      Emit_LCH_Call (N);
      Build_Br (BB_Next);
      Position_Builder_At_End (BB_Next);
   end Emit_LCH_Call_If;

   -------------------
   -- Emit_LCH_Call --
   -------------------

   procedure Emit_LCH_Call (N : Node_Id) is
      Int_Type      : constant Type_T := Create_Type (Standard_Integer);
      Args          : Value_Array (1 .. 2);

      File : constant String :=
        Get_Name_String (Reference_Name (Get_Source_File_Index (Sloc (N))));

      Element_Type : constant Type_T := Int_Ty (8);
      Array_Type   : constant Type_T :=
        LLVM.Core.Array_Type (Element_Type, File'Length + 1);
      Elements     : array (1 .. File'Length + 1) of Value_T;
      V            : constant Value_T :=
                       Add_Global (LLVM_Module, Array_Type, "str-lit");

   begin
      --  Build a call to __gnat_last_chance_handler (FILE, LINE)

      --  First build a string literal for FILE

      for J in File'Range loop
         Elements (J) := Const_Int
           (Element_Type, ULL (Character'Pos (File (J))),
            Sign_Extend => False);
      end loop;

      --  Append NUL character

      Elements (Elements'Last) :=
        Const_Int (Element_Type, 0, Sign_Extend => False);

      Set_Initializer
        (V, Const_Array (Element_Type, Elements'Address, Elements'Length));
      Set_Linkage (V, Private_Linkage);
      Set_Global_Constant (V, True);

      Args (1) := Bit_Cast
        (IR_Builder,
         GEP
           (IR_Builder,
            V,
            (Const_Int (LLVM_Size_Type, 0, Sign_Extend => False),
             Const_Int (Create_Type (Standard_Positive),
                        0, Sign_Extend => False)),
            ""),
         Void_Ptr_Type,
         "");

      --  Then provide the line number

      Args (2) := Const_Int
        (Int_Type, ULL (Get_Logical_Line_Number (Sloc (N))),
         Sign_Extend => False);
      Discard (Call (IR_Builder, LLVM_Value (Get_LCH_Fn),
                     Args'Address, Args'Length, ""));
   end Emit_LCH_Call;

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
      Ret_By_Ref     : constant Boolean   :=
        Returns_By_Ref ((if Direct_Call then Entity (Subp) else Subp_Typ));
      Dynamic_Return : constant Boolean   :=
        not Ret_By_Ref and then Is_Dynamic_Return (Return_Typ);
      Param          : Node_Id;
      P_Type         : Entity_Id;
      Actual         : Node_Id;

      --  If it's not an identifier, it must be an access to a subprogram and
      --  in such a case, it must accept a static link.

      This_Takes_S_Link : constant Boolean := not Direct_Call
          and then Needs_Activation_Record (Subp_Typ);
      S_Link         : GL_Value;
      LLVM_Func      : GL_Value;
      Orig_Arg_Count : constant Nat := Count_Params (Subp_Typ);
      Args_Count     : constant Nat :=
        Orig_Arg_Count + (if This_Takes_S_Link then 1 else 0) +
          (if Dynamic_Return then 1 else 0);
      Args           : GL_Value_Array (1 .. Args_Count);
      Idx            : Nat := 1;

   begin

      if Direct_Call then
         Subp := Entity (Subp);
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
         --  pointer to being a pointer to the parameter's type.

         if Param_Needs_Ptr (Param) then
            Args (Idx) := Convert_To_Access_To (Emit_LValue (Actual), P_Type);
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
        (if Is_Compilation_Unit (Def_Ident) then "_ada_" & Subp_Name
         else Subp_Name);
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

end GNATLLVM.Subprograms;
