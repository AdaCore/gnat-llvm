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
with Interfaces.C.Extensions; use Interfaces.C.Extensions;

with Einfo;    use Einfo;
with Exp_Unst; use Exp_Unst;
with Namet;    use Namet;
with Sem_Util; use Sem_Util;
with Sinput;   use Sinput;
with Stand;    use Stand;

with LLVM.Core;  use LLVM.Core;
with LLVM.Types; use LLVM.Types;

with GNATLLVM.Compile;   use GNATLLVM.Compile;
with GNATLLVM.DebugInfo; use GNATLLVM.DebugInfo;
with GNATLLVM.GLValue;   use GNATLLVM.GLValue;
with GNATLLVM.Types;     use GNATLLVM.Types;
with GNATLLVM.Utils;     use GNATLLVM.Utils;
with GNATLLVM.Wrapper;   use GNATLLVM.Wrapper;
with LLVM_Drive;         use LLVM_Drive;

package body GNATLLVM.Subprograms is

   -------------------
   -- Emit_One_Body --
   -------------------

   procedure Emit_One_Body (Env : Environ; Node : Node_Id) is
      Spec       : constant Node_Id := Get_Acting_Spec (Node);
      Func       : constant GL_Value := Emit_Subprogram_Decl (Env, Spec);
      Def_Ident  : constant Entity_Id := Defining_Entity (Spec);
      Return_Typ : constant Entity_Id := Full_Etype (Def_Ident);
      Param      : Entity_Id;
      LLVM_Param : GL_Value;
      Param_Num  : Natural := 0;

   begin
      Enter_Subp (Env, Func);

      if Emit_Debug_Info then
         Env.Func_Debug_Info :=
           Create_Debug_Subprogram
             (Env.DIBld,
              LLVM_Value (Func),
              Get_Debug_File_Node (Env, Get_Source_File_Index (Sloc (Node))),
              Get_Ext_Name (Def_Ident),
              Integer (Get_Logical_Line_Number (Sloc (Node))));
      end if;

      Param := First_Formal_With_Extras (Def_Ident);
      while Present (Param) loop
         LLVM_Param := G (Get_Param (LLVM_Value (Func), unsigned (Param_Num)),
                          Full_Etype (Param),
                          Is_Reference => Param_Needs_Ptr (Param));

         --  Define a name for the parameter Param (which is the
         --  Param_Num'th parameter), and associate the corresponding
         --  LLVM value to its entity.

         Set_Value_Name (LLVM_Value (LLVM_Param), Get_Name (Param));

         --  Special case for structures passed by value, we want to
         --  store a pointer to them on the stack, so do an alloca,
         --  to be able to do GEP on them.

         --  Add the parameter to the environnment

         Set_Value (Env, Param, LLVM_Param);

         if Ekind (Param) = E_In_Parameter
           and then Is_Activation_Record (Param)
         then
            Env.Activation_Rec_Param := LLVM_Param;
         end if;

         Param_Num := Param_Num + 1;
         Param := Next_Formal_With_Extras (Param);
      end loop;

      --  If the return type has dynamic size, we've added a parameter
      --  that's passed the address to which we want to copy our return
      --  value.

      if Ekind (Return_Typ) /= E_Void
        and then Is_Dynamic_Size (Env, Return_Typ)
      then
         LLVM_Param := G (Get_Param (LLVM_Value (Func), unsigned (Param_Num)),
                          Return_Typ, Is_Reference => True);
         Set_Value_Name (LLVM_Value (LLVM_Param), "return");
         Env.Return_Address_Param := LLVM_Param;
      end if;

      Emit_List (Env, Declarations (Node));
      Emit_List (Env, Statements (Handled_Statement_Sequence (Node)));

      --  This point should not be reached: a return must have
      --  already... returned!

      Build_Unreachable (Env);
      Leave_Subp (Env);
   end Emit_One_Body;

   --------------------------
   -- Emit_Subprogram_Body --
   --------------------------

   procedure Emit_Subprogram_Body (Env : Environ; Node : Node_Id) is
      Nest_Table_First : constant Nat := Nested_Functions_Table.Last + 1;

   begin
      --  If we're not at library level, this a nested function.  Defer it
      --  until we complete elaboration of the enclosing function.  But do
      --  ensure that the spec has been elaborated.

      if not Library_Level (Env) then
         Discard (Emit_Subprogram_Decl (Env, Get_Acting_Spec (Node)));
         Nested_Functions_Table.Append (Node);
         return;
      end if;

      --  Otherwise, elaborate this function and then any nested functions
      --  within in.

      Emit_One_Body (Env, Node);

      for I in Nest_Table_First .. Nested_Functions_Table.Last loop
         Emit_Subprogram_Body (Env, Nested_Functions_Table.Table (I));
      end loop;

      Nested_Functions_Table.Set_Last (Nest_Table_First);
   end Emit_Subprogram_Body;

   ---------------------
   -- Get_Static_Link --
   ---------------------

   function Get_Static_Link (Env : Environ; Node : Entity_Id) return GL_Value
   is
      Subp        : constant Entity_Id := Entity (Node);
      Result      : GL_Value;
      Result_Type : constant Type_T := Pointer_Type (Int_Ty (8), 0);
      Parent : constant Entity_Id := Enclosing_Subprogram (Subp);
      Caller : Node_Id;

   begin
      --  ?? This routine needs a major rewrite

      if Present (Parent) then
         Caller := Node_Enclosing_Subprogram (Node);

         declare
            Ent        : constant Subp_Entry :=
              Subps.Table (Subp_Index (Parent));
            Ent_Caller : constant Subp_Entry :=
              Subps.Table (Subp_Index (Caller));

         begin
            if Parent = Caller then
               Result := Need_Value (Env, Get_Value (Env, Ent.ARECnP),
                                     Ent.ARECnPT);
            else
               Result := Get_Value (Env, Ent_Caller.ARECnF);

               --  Go levels up via the ARECnU field if needed

               for J in 1 .. Ent_Caller.Lev - Ent.Lev - 1 loop
                  Result := Load (Env, GEP
                                    (Env, Env.Size_Type, Result,
                                     (1 => Const_Int_32 (Env, 0),
                                      2 => Const_Int_32 (Env, 0)),
                                     "ARECnF.all.ARECnU"));
               end loop;
            end if;

            return Ptr_To_Ref
              (Env, Result, Standard_Short_Short_Integer, "static-link");
         end;
      else
         return G (Const_Null (Result_Type),
                   Standard_Short_Short_Integer, Is_Reference => True);
      end if;
   end Get_Static_Link;

   -------------------
   -- Emit_LCH_Call --
   -------------------

   procedure Emit_LCH_Call (Env : Environ; Node : Node_Id) is
      Void_Ptr_Type : constant Type_T := Pointer_Type (Int_Ty (8), 0);
      Int_Type      : constant Type_T := Create_Type (Env, Standard_Integer);
      Args          : Value_Array (1 .. 2);

      File : constant String :=
        Get_Name_String (Reference_Name (Get_Source_File_Index (Sloc (Node))));

      Element_Type : constant Type_T :=
        Int_Type_In_Context (Env.Ctx, 8);
      Array_Type   : constant Type_T :=
        LLVM.Core.Array_Type (Element_Type, File'Length + 1);
      Elements     : array (1 .. File'Length + 1) of Value_T;
      V            : constant Value_T :=
                       Add_Global (Env.Mdl, Array_Type, "str-lit");

   begin
      --  Build a call to __gnat_last_chance_handler (FILE, LINE)

      --  First build a string literal for FILE

      for J in File'Range loop
         Elements (J) := Const_Int
           (Element_Type,
            unsigned_long_long (Character'Pos (File (J))),
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
        (Env.Bld,
         GEP
           (Env.Bld,
            V,
            (Const_Int (Env.LLVM_Size_Type, 0, Sign_Extend => False),
             Const_Int (Create_Type (Env, Standard_Positive),
                        0, Sign_Extend => False)),
            ""),
         Void_Ptr_Type,
         "");

      --  Then provide the line number

      Args (2) := Const_Int
        (Int_Type,
         unsigned_long_long (Get_Logical_Line_Number (Sloc (Node))),
         Sign_Extend => False);
      Discard (Call (Env.Bld, Env.LCH_Fn, Args'Address, Args'Length, ""));
   end Emit_LCH_Call;

   ---------------
   -- Emit_Call --
   ---------------

   function Emit_Call (Env : Environ; Call_Node : Node_Id) return GL_Value is
      Subp           : Node_Id := Name (Call_Node);
      Return_Typ     : constant Entity_Id := Full_Etype (Call_Node);
      Void_Return    : constant Boolean := Ekind (Return_Typ) = E_Void;
      Dynamic_Return : constant Boolean :=
        not Void_Return and then Is_Dynamic_Size (Env, Return_Typ);
      Direct_Call    : constant Boolean :=
        Nkind (Subp) /= N_Explicit_Dereference;
      Subp_Typ       : constant Entity_Id :=
        (if Direct_Call then Entity (Subp) else Full_Etype (Subp));
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

      LLVM_Func := Emit_Expression (Env, Subp);
      if This_Takes_S_Link then
         S_Link := Extract_Value_To_Ref (Env, Standard_Short_Short_Integer,
                                         LLVM_Func, 1, "static-link");
         LLVM_Func := Ptr_To_Ref (Env,
                                  Extract_Value_To_Ref
                                    (Env, Standard_Short_Short_Integer,
                                     LLVM_Func, 0, "callback"),
                                  Full_Designated_Type
                                    (Full_Etype (Prefix (Subp))));
      end if;

      Param  := First_Formal_With_Extras (Subp_Typ);
      Actual := First_Actual (Call_Node);
      while Present (Actual) loop
         P_Type := Full_Etype (Param);

         --  We have two cases: if the param isn't passed indirectly, convert
         --  the value to the parameter's type.  If it is, then convert the
         --  pointer to being a pointer to the parameter's type.

         if Param_Needs_Ptr (Param) then
            Args (Idx) := Convert_To_Access_To
              (Env, Emit_LValue (Env, Actual), P_Type);
         else
            Args (Idx) := Build_Type_Conversion (Env, P_Type, Actual);
         end if;

         Idx := Idx + 1;
         Actual := Next_Actual (Actual);
         Param := Next_Formal_With_Extras (Param);
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
           Allocate_For_Type (Env, Return_Typ, "call-return");
      end if;

      --  If the return type is of dynamic size, call as a procedure and
      --  return the address we set as the last parameter.

      if Dynamic_Return then
         Discard (Call (Env, LLVM_Func, Standard_Void_Type, Args));
         return Args (Args'Last);
      else
         return Call (Env, LLVM_Func, Return_Typ, Args);
      end if;
   end Emit_Call;

   --------------------------
   -- Emit_Subprogram_Decl --
   --------------------------

   function Emit_Subprogram_Decl
     (Env : Environ; Subp_Spec : Node_Id) return GL_Value
   is
      Def_Ident : constant Node_Id := Defining_Entity (Subp_Spec);

   begin
      --  If this subprogram specification has already been compiled, do
      --  nothing.

      if Has_Value (Env, Def_Ident) then
         return Get_Value (Env, Def_Ident);
      else
         declare
            Subp_Type : constant Type_T :=
              Create_Subprogram_Type_From_Spec (Env, Subp_Spec);

            Subp_Base_Name : constant String := Get_Ext_Name (Def_Ident);
            LLVM_Func      : GL_Value;

         begin
            --  ??? Special case __gnat_last_chance_handler which is
            --  already defined as Env.LCH_Fn

            if Subp_Base_Name = "__gnat_last_chance_handler" then
               return G (Env.LCH_Fn, Standard_Void_Type,
                         Is_Reference => True, Is_Intermediate_Type => True);
            end if;

            --  ?? We have a tricky issue here.  We need to indicate that this
            --  object is a subprogram, but we don't have a GNAT type
            --  corresponding to the subprogram type unless there's an access
            --  type.  So we'll use the the return type and flag it as
            --  both a reference and intermediate type.  There doesn't seem
            --  to be better way to handle this right now.

            LLVM_Func :=
              G (Add_Function
                   (Env.Mdl,
                    (if Is_Compilation_Unit (Def_Ident)
                       then "_ada_" & Subp_Base_Name
                       else Subp_Base_Name),
                         Subp_Type),
                 Full_Etype (Def_Ident),
                 Is_Reference => True, Is_Intermediate_Type => True);
            --  Define the appropriate linkage

            if not Is_Public (Def_Ident) then
               Set_Linkage (LLVM_Value (LLVM_Func), Internal_Linkage);
            end if;

            Set_Value (Env, Def_Ident, LLVM_Func);
            return LLVM_Func;
         end;
      end if;
   end Emit_Subprogram_Decl;

   -------------------------------
   -- Node_Enclosing_Subprogram --
   -------------------------------

   function Node_Enclosing_Subprogram (Node : Node_Id) return Node_Id is
      N : Node_Id := Node;

   begin
      while Present (N) loop
         if Nkind (N) = N_Subprogram_Body then
            N := Defining_Unit_Name (Specification (N));
            if Nkind (N) = N_Defining_Program_Unit_Name then
               N := Defining_Identifier (N);
            end if;

            return N;
         end if;

         N := Parent (N);
      end loop;

      return N;
   end Node_Enclosing_Subprogram;

end GNATLLVM.Subprograms;
