------------------------------------------------------------------------------
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
with Exp_Util;    use Exp_Util;
with Nlists;      use Nlists;
with Opt;         use Opt;
with Restrict;    use Restrict;
with Sem_Util;    use Sem_Util;
with Set_Targ;    use Set_Targ;
with Sinput;      use Sinput;
with Snames;      use Snames;
with Stand;       use Stand;
with Stringt;     use Stringt;
with Table;       use Table;

with GNATLLVM.Aliasing;     use GNATLLVM.Aliasing;
with GNATLLVM.Arrays;       use GNATLLVM.Arrays;
with GNATLLVM.Blocks;       use GNATLLVM.Blocks;
with GNATLLVM.Builtins;     use GNATLLVM.Builtins;
with GNATLLVM.Codegen;      use GNATLLVM.Codegen;
with GNATLLVM.Conditionals; use GNATLLVM.Conditionals;
with GNATLLVM.Conversions;  use GNATLLVM.Conversions;
with GNATLLVM.DebugInfo;    use GNATLLVM.DebugInfo;
with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.Exprs;        use GNATLLVM.Exprs;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;
with GNATLLVM.Records;      use GNATLLVM.Records;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Types.Create; use GNATLLVM.Types.Create;
with GNATLLVM.Subprograms;  use GNATLLVM.Subprograms;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Variables;    use GNATLLVM.Variables;

with CCG; use CCG;

package body GNATLLVM.Compile is

   function Emit_Internal
     (N          : N_Subexpr_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False;
      Prefer_LHS : Boolean  := False) return GL_Value
     with Post => Present (Emit_Internal'Result);
   --  Same as Emit, but push result into LValue list

   Suppress_Overflow_Depth : Int := 0;
   --  The depth of Push/Pop_Suppress_Overflow

   ------------------
   -- GNAT_To_LLVM --
   ------------------

   procedure GNAT_To_LLVM (GNAT_Root : N_Compilation_Unit_Id) is
      function Stand_Type (Size : Nat) return Opt_Signed_Integer_Kind_Id;
      --  Find a standard integer type with the specified size. If none,
      --  return Empty.

      function Stand_Type (Size : Nat) return Opt_Signed_Integer_Kind_Id is
      begin
         if Long_Long_Long_Size = Size then
            return Standard_Long_Long_Long_Integer;
         elsif Long_Long_Size = Size then
            return Standard_Long_Long_Integer;
         elsif Long_Size = Size then
            return Standard_Long_Integer;
         elsif Int_Size = Size then
            return Standard_Integer;
         elsif Short_Size = Size then
            return Standard_Short_Integer;
         elsif Char_Size = Size then
            return Standard_Short_Short_Integer;
         else
            return Empty;
         end if;
      end Stand_Type;

      Size_Type   : Opt_Signed_Integer_Kind_Id;
      Int_32_Type : Opt_Signed_Integer_Kind_Id;
      Int_64_Type : Opt_Signed_Integer_Kind_Id;

   begin
      --  If we're going to generate C code (or LLVM IR as if we were to
      --  generate C code), initialize that subsystem.

      if Emit_C then
         C_Initialize_Output;
      end if;

      --  Initialize the environment and get the sizes of fat and thin
      --  pointers and make some types. We must do this after initializing
      --  the target info since Pointer_Size requires it.

      Initialize_Environment;
      Thin_Pointer_Size := Set_Targ.Pointer_Size;
      Fat_Pointer_Size  := Thin_Pointer_Size * 2;
      Size_Type         := Stand_Type (Set_Targ.Bits_Per_Word);
      Int_32_Type       := Stand_Type (32);
      Int_64_Type       := Stand_Type (64);

      --  Get single bit and single byte values and types, max alignmen
      --  and maximum integer size.

      BPU          := Bits_Per_Unit;
      UBPU         := ULL (BPU);
      Bit_T        := Int_Ty (Nat (1));
      Byte_T       := Int_Ty (BPU);
      Max_Align    := Maximum_Alignment * BPU;
      Max_Int_Size := (if   Enable_128bit_Types then +Long_Long_Long_Size
                       else +Long_Long_Size);

      --  We want to be able to support overaligned values, but we still need
      --  to have a maximum possible alignment to start with. The maximum
      --  alignment in bytes supported by LLVM is actually 2 ** 29, but if
      --  we convert to an alignment in bits, which is the way we store
      --  alignments, that will overflow, so we restrict it to a value
      --  that won't overflow and then a further power of two to be safe.

      Max_Valid_Align := Nat ((ULL (Nat'Last) + 1) / UBPU / 2);

      --  We have to initialize aliasing before we create any types

      GNATLLVM.Aliasing.Initialize;

      --  We create Name_Id values for struct names

      Namet.Unlock;

      --  We must elaborate Size_Type first because its needed to elaborate
      --  all other types and we need to have a kludge here to set the sizes
      --  of the GL_Type only when the below variables have been set.

      Size_GL_Type := Primitive_GL_Type (Size_Type);
      Size_T       := Type_Of (Size_Type);
      Update_GL_Type (Size_GL_Type, Size_T, False);
      Update_GL_Type (Base_GL_Type (Size_Type), Size_T, False);

      --  Now create the 32-bit and 64-bit integer types, allowing for the
      --  possibility that we don't have a 64-bit type.

      Int_32_GL_Type := Primitive_GL_Type (Int_32_Type);
      Int_32_T       := Type_Of (Int_32_GL_Type);

      if Present (Int_64_Type) then
         Int_64_GL_Type := Primitive_GL_Type (Int_64_Type);
         Int_64_T       := Type_Of (Int_64_GL_Type);
      end if;

      --  Create GL_Types for builtin types. Create Boolean first because
      --  we use it internally to make boolean constants in the evaluation
      --  of expressions used in elaborating other types.

      Boolean_GL_Type   := Primitive_GL_Type (Standard_Boolean);
      A_Char_GL_Type    := Primitive_GL_Type (Standard_A_Char);
      SSI_GL_Type       := Primitive_GL_Type (Standard_Short_Short_Integer);
      SI_GL_Type        := Primitive_GL_Type (Standard_Short_Integer);
      Integer_GL_Type   := Primitive_GL_Type (Standard_Integer);
      LI_GL_Type        := Primitive_GL_Type (Standard_Long_Integer);
      LLI_GL_Type       := Primitive_GL_Type (Standard_Long_Long_Integer);
      Void_GL_Type      := Primitive_GL_Type (Standard_Void_Type);
      Any_Array_GL_Type := Primitive_GL_Type (Any_Array);

      --  Create a "void" pointer, which is i8* in LLVM

      Void_Ptr_T        := Type_Of (A_Char_GL_Type);

      --  In most cases, addresses can be represented as Size_T (which
      --  usually is an integer type of pointer width), but on
      --  architectures with tagged pointers we would lose information by
      --  doing so; use a void pointer on those architectures instead. We
      --  don't want to use void pointers unconditionally because it puts a
      --  burden on address arithmetic, which now requires conversions to
      --  and from a suitable integer representation of the address.

      Address_T       := (if Tagged_Pointers then Void_Ptr_T else Size_T);
      Address_GL_Type := Primitive_GL_Type (Standard_Address);

      --  The size of a pointer is specified in both the LLVM data layout
      --  string (usually from a --target specification) and the target
      --  parameter file. Make sure they agree.

      if Get_Scalar_Bit_Size (Void_Ptr_T) /= ULL (Thin_Pointer_Size) then
         Early_Error
           ("Pointer size mismatch between target and target parameters");
      end if;
      --  Initialize modules and handle duplicate globals

      --  Get the actual main source file index. If -gnatDG,
      --  Main_Source_File_Index is wrong.

      Our_Source_File := Get_Source_File_Index (Sloc (GNAT_Root));

      Stringt.Unlock;
      GNATLLVM.Blocks.Initialize;
      GNATLLVM.Builtins.Initialize;
      GNATLLVM.DebugInfo.Initialize;
      Detect_Duplicate_Global_Names;
      Stringt.Lock;

      --  Actually translate

      Emit (GNAT_Root);

      --   Now finalize things and generate code

      C_Protect_Source_Order;
      Output_Global_Constructors_Destructors;
      Add_Functions_To_Module;
      Finalize_Debugging;
      Generate_Code (GNAT_Root);
      Namet.Lock;

   end GNAT_To_LLVM;

   ---------------------------
   -- Push_Supress_Overflow --
   ---------------------------

   procedure Push_Suppress_Overflow is
   begin
      Suppress_Overflow_Depth := Suppress_Overflow_Depth + 1;
   end Push_Suppress_Overflow;

   ---------------------------
   -- Push_Supress_Overflow --
   ---------------------------

   procedure Pop_Suppress_Overflow is
   begin
      Suppress_Overflow_Depth := Suppress_Overflow_Depth - 1;
   end Pop_Suppress_Overflow;

   ----------
   -- Emit --
   ----------

   procedure Emit (N : Node_Id) is
      Start_Position : Position_T;

   begin
      --  If we're at library level and this node type generates code,
      --  append it to the elab proc.

      if Library_Level
        and then ((Nkind (N) in N_Statement_Other_Than_Procedure_Call
                     and then Nkind (N) /= N_Null_Statement)
                    or else Nkind (N) in N_Subprogram_Call | N_Raise_xxx_Error
                    or else (Nkind (N) = N_Handled_Sequence_Of_Statements
                               and then Has_Non_Null_Statements
                                          (Statements (N))))
      then
         Add_To_Elab_Proc (N);
         return;

      --  If not at library level and in dead code, start a new basic block
      --  for any code we emit.

      elsif not Library_Level and then Are_In_Dead_Code then
         Position_Builder_At_End (Create_Basic_Block ("dead.code"));
      end if;

      --  If we're in an elab proc, save our current position to see if we've
      --  generated any code.

      if In_Elab_Proc or else In_Elab_Proc_Stmts then
         Start_Position := Get_Current_Position;
      end if;

      --  Set our location for debuging, clear any pending LValues, and
      --  then generate code for this node.

      Set_Debug_Pos_At_Node (N);
      Clear_LValue_List;
      case Nkind (N) is

         when N_Compilation_Unit => Compilation_Unit : declare
            U         : constant Node_Id := Unit (N);
            Subp      : Opt_Subprogram_Kind_Id;
            Subp_Body : Node_Id;

         begin
            --  We assume there won't be any elaboration code and
            --  clear that flag if we're wrong.

            Set_Has_No_Elaboration_Code (N, True);

            --  For a body, first process the spec if there is one

            if (Nkind (U) = N_Subprogram_Body and then not Acts_As_Spec (U))
              or else Nkind (U) = N_Package_Body
            then
               Emit (Library_Unit (N));
            end if;

            Emit (Context_Items (N));
            Emit_Decl_Lists (Declarations (Aux_Decls_Node (N)));
            Emit (U);

            --  Generate code for all the inlined subprograms

            Subp := First_Inlined_Subprogram (N);
            while Present (Subp) loop
               Subp_Body := Parent (Declaration_Node (Subp));

               --  If inlining is disabled, process only the required
               --  subprograms. Likewise if optimization is disabled, unless
               --  we're generating C.

               if (Has_Pragma_Inline_Always (Subp)
                   or else (not No_Inlining
                                and then (Code_Opt_Level > 0
                                          or else C_Can_Cross_Inline)))
                 --  The set of inlined subprograms is computed from data
                 --  recorded early during expansion and it can be a strict
                 --  superset of the final set computed after semantic
                 --  analysis, for example if a call to such a subprogram
                 --  occurs in a pragma Assert and assertions are disabled.
                 --  In that case, semantic analysis resets Is_Public to
                 --  false but the entry for the subprogram in the inlining
                 --  tables is stalled.

                 and then Is_Public (Subp)
               then
                  if Nkind (Subp_Body) = N_Subprogram_Declaration
                    and then Present (Corresponding_Body (Subp_Body))
                  then
                     Subp_Body := Parent (Declaration_Node
                                            (Corresponding_Body (Subp_Body)));
                  end if;

                  if Nkind (Subp_Body) = N_Subprogram_Body then
                     Emit_Subprogram_Body (Subp_Body, For_Inline => True);
                  end if;
               end if;

               Next_Inlined_Subprogram (Subp);
            end loop;

            Emit (Actions (Aux_Decls_Node (N)));
            Emit (Pragmas_After (Aux_Decls_Node (N)));
         end Compilation_Unit;

         when N_Subunit =>
            Emit (Proper_Body (N));

         when N_Package_Declaration =>
            Push_Lexical_Debug_Scope (N);
            Emit (Specification (N));
            Pop_Debug_Scope;

         when N_Package_Specification =>
            Push_Lexical_Debug_Scope (N);
            Emit_Decl_Lists (Visible_Declarations (N),
                             Private_Declarations (N));
            Pop_Debug_Scope;

            if Nkind (Parent (Parent (N))) = N_Compilation_Unit
              and then No (Corresponding_Body (Parent (N)))
            then
               Emit_Elab_Proc (N, Empty, Parent (Parent (N)), False);
            end if;

         when N_Package_Body =>

            --  Skip generic packages

            if Ekind (Unique_Defining_Entity (N)) in Generic_Unit_Kind then
               return;
            end if;

            declare
               Stmts : constant Opt_N_Handled_Sequence_Of_Statements_Id :=
                 Handled_Statement_Sequence (N);

            begin
               --  If this is the uppermost compilation unit, show any
               --  elaborations are now for the body

               if Nkind (Parent (N)) = N_Compilation_Unit then
                  Mark_Body_Elab;
               end if;

               --  If we have an At_End_Proc, we need to make a block for it

               if Present (At_End_Proc (N)) then
                  Push_Block (At_End_Proc => At_End_Proc (N));
               end if;

               --  Always process declarations, but they do not provide
               --  a scope, since those declarations are part of what
               --  encloses us, if anything.

               Emit_Decl_Lists (Declarations (N));

               --  If we're at library level and our parent is an
               --  N_Compilation_Unit, make an elab proc and put the
               --  statements there. Otherwise, emit them, which may add
               --  them to the elaboration table (if we're not at library
               --  level).

               Push_Lexical_Debug_Scope (N);
               if Nkind (Parent (N)) = N_Compilation_Unit then
                  if Present (Corresponding_Spec (N)) then
                     declare
                        Spec : constant E_Package_Id := Corresponding_Spec (N);
                        Decl : constant N_Is_Decl_Id :=
                          Declaration_Node (Spec);

                     begin
                        Emit_Elab_Proc (Decl, Empty, Parent (Parent (Decl)));
                     end;
                  end if;

                  Emit_Elab_Proc (N, Stmts, Parent (N), For_Body => True);
               elsif Present (Stmts) then
                  if not Library_Level and then No (At_End_Proc (N)) then
                     Push_Block;
                  end if;

                  Emit (Stmts);

                  if not Library_Level and then No (At_End_Proc (N)) then
                     Pop_Block;
                  end if;
               end if;

               Pop_Debug_Scope;

               if Present (At_End_Proc (N)) then
                  Pop_Block;
               end if;
            end;

         when N_Subprogram_Body =>

            --  Skip generic subprograms

            if not Present (Corresponding_Spec (N))
              or else not (Ekind (Corresponding_Spec (N))
                         in Generic_Subprogram_Kind)
            then
               Emit_Subprogram_Body (N);

               if Library_Level then
                  C_Add_To_Source_Order (N);
               end if;
            end if;

         when N_Subprogram_Declaration =>

            --  Do not process functions that return arrays because they have
            --  been rewritten as procedures.

            if Ekind (Unique_Defining_Entity (N)) /= E_Function
              or else not Rewritten_For_C (Unique_Defining_Entity (N))
            then
               Emit (Specification (N));

               if Library_Level then
                  C_Add_To_Source_Order (N);
               end if;
            end if;

         when N_Function_Specification | N_Procedure_Specification =>

            --  Ignore intrinsic subprograms as calls to those will be
            --  expanded. Also ignore eliminated subprograms.

            if not Is_Intrinsic_Subprogram (Unique_Defining_Entity (N))
              and then not Is_Eliminated (Unique_Defining_Entity (N))
            then
               Discard (Emit_Subprogram_Decl (N, Frozen => False));
            end if;

         when N_Free_Statement =>
            Heap_Deallocate
              (Emit_Expression (Expression (N)),
               (if   Present (Actual_Designated_Subtype (N))
                then Default_GL_Type (Get_Fullest_View
                                        (Actual_Designated_Subtype (N)))
                else No_GL_Type),
               Procedure_To_Call (N), Storage_Pool (N));

         when N_Code_Statement =>
            Emit_Code_Statement (N);

         when N_Handled_Sequence_Of_Statements =>
            declare
               Need_Block : constant Boolean :=
                 Present (At_End_Proc (N))
                 or else Present (Exception_Handlers (N));

            begin
               --  If this block doesn't contain any statements, ignore it

               if not Has_Non_Null_Statements (Statements (N)) then
                  return;
               end if;

               --  If we need a block, make it

               if Need_Block then
                  Push_Block (At_End_Proc (N), Exception_Handlers (N));
               end if;

               Emit (Statements (N));

               if Need_Block then
                  Pop_Block;
               end if;
            end;

         when N_Raise_Statement =>
            Emit_Reraise;

         when N_Raise_xxx_Error =>
            Emit_Raise (N);

         when N_Object_Declaration | N_Exception_Declaration =>
            Emit_Declaration (N);

            if Library_Level then
               C_Add_To_Source_Order (N);
            end if;

         when N_Object_Renaming_Declaration
            | N_Exception_Renaming_Declaration =>

            Emit_Renaming_Declaration (N);

            if Library_Level then
               C_Add_To_Source_Order (N);
            end if;

         when N_Subprogram_Renaming_Declaration =>

            --  Nothing is needed except for debugging information.
            --  ??? Skip it for now. Note that in any case, we should
            --  skip Intrinsic subprograms

            null;

         when N_Implicit_Label_Declaration =>

            --  Don't do anything here in case this label isn't actually
            --  used in an N_Label or N_Goto_Statement operation. If it
            --  were unused, the basic block we create here would be empty,
            --  which LLVM doesn't allow. This can't occur for user-defined
            --  labels, but can occur with some labels placed by the front
            --  end. Instead, lazily create the basic block where it's
            --  placed or when its the target of a goto.

            null;

         when N_Assignment_Statement =>
            Emit_Assignment_Statement (N);

         when N_Procedure_Call_Statement =>

            --  If we're only elaborating decls, we may have a call to a
            --  function whose Name is an N_Selected_Component. This is an
            --  unexpanded tasking-related call. Skip it and hope there are
            --  no types only in that call.

            if not Decls_Only or else Nkind (Name (N)) /= N_Selected_Component
            then
               Discard (Emit_Call (N));
            end if;

         when N_Null_Statement =>
            null;

         when N_Label =>
            Discard (Enter_Block_With_Node (N));

         when N_Goto_Statement =>
            Build_Br (Get_Label_BB (Entity (Name (N))));

         when N_Exit_Statement =>
            declare
               Exit_BB : constant Basic_Block_T := Get_Exit_Point (Name (N));
               Next_BB : Basic_Block_T;
            begin
               if Present (Condition (N)) then
                  Next_BB := Create_Basic_Block;
                  Emit_If_Cond (Condition (N), Exit_BB, Next_BB);
                  Position_Builder_At_End (Next_BB);
               else
                  Build_Br (Exit_BB);
               end if;
            end;

         when N_Simple_Return_Statement =>
            Emit_Return_Statement (N);

         when N_If_Statement =>
            Emit_If (N);

         when N_Loop_Statement =>
            Emit_Loop_Statement (N);

         when N_Block_Statement =>
            Push_Lexical_Debug_Scope (N);
            Push_Block (At_End_Proc => At_End_Proc (N));
            Emit_Decl_Lists (Declarations (N));
            Emit (Handled_Statement_Sequence (N));
            Set_Debug_Pos_At_Node (N);
            Pop_Block;
            Pop_Debug_Scope;

         when N_Incomplete_Type_Declaration
            | N_Private_Extension_Declaration
            | N_Private_Type_Declaration =>

            --  Ignore incomplete type declarations since we'll either
            --  elaborate the type when we see the full declaration or
            --  lazily elaborate the it either when we need it.

            null;

         when N_Full_Type_Declaration | N_Subtype_Declaration
            | N_Task_Type_Declaration
         =>
         Declaration : declare

            TE : Type_Kind_Id          := Defining_Identifier (N);
            FT : constant Type_Kind_Id := Get_Fullest_View (TE);

         begin
            --  Start by elaborating this type via its fullest view

            Discard (Type_Of (FT));

            --  If the fullest view isn't the same as the type being
            --  declared, copy the annotations from the full type to the
            --  type being declared and any intermediate types.

            while TE /= FT loop
               Copy_Annotations (FT, TE);
               TE := Get_Fullest_View (TE, Recurse => False);
            end loop;

            --  If this is an enumeration type, notify CCG

            if Ekind (TE) = E_Enumeration_Type then
               C_Note_Enum (TE);
            end if;
         end Declaration;

         when N_Freeze_Entity =>
            Process_Freeze_Entity (N);
            Emit_Decl_Lists (Actions (N));

         when N_Pragma =>
            Emit_Pragma (N);

         when N_Case_Statement =>
            Emit_Case_Statement (N);

         when N_Body_Stub =>

            --  If we have a "separate" (either subprogram or package), we
            --  compile that as part of this unit, so go into it.

            if Present (Library_Unit (N)) then
               Emit (Unit (Library_Unit (N)));
            end if;

         --  Nodes we actually want to ignore, in many cases because they
         --  represent things that are put elsewhere in the tree (e.g,
         --  rep clauses).

         when N_Abstract_Subprogram_Declaration
            | N_At_Clause
            | N_Call_Marker
            | N_Empty
            | N_Enumeration_Representation_Clause
            | N_Enumeration_Type_Definition
            | N_Function_Instantiation
            | N_Freeze_Generic_Entity
            | N_Generic_Function_Renaming_Declaration
            | N_Generic_Package_Renaming_Declaration
            | N_Generic_Procedure_Renaming_Declaration
            | N_Generic_Package_Declaration
            | N_Generic_Subprogram_Declaration
            | N_Itype_Reference
            | N_Number_Declaration
            | N_Package_Instantiation
            | N_Package_Renaming_Declaration
            | N_Procedure_Instantiation
            | N_Protected_Type_Declaration
            | N_Record_Representation_Clause
            | N_Validate_Unchecked_Conversion
            | N_Variable_Reference_Marker
            | N_Use_Package_Clause
            | N_Use_Type_Clause
            | N_With_Clause
           =>
            null;

         when N_Push_Constraint_Error_Label .. N_Pop_Storage_Error_Label =>
            Process_Push_Pop_xxx_Error_Label (N);

         when N_Attribute_Definition_Clause =>

            --  The only interesting case left after expansion is for Address
            --  clauses. We only deal with 'Address if the object has a Freeze
            --  node.

            if Get_Attribute_Id (Chars (N)) = Attribute_Address
              and then Present (Freeze_Node (Entity (Name (N))))
            then
               declare
                  Expr : constant N_Subexpr_Id := Expression (N);

               begin
                  if Library_Level and then not Is_Static_Address (Expr) then
                     Add_To_Elab_Proc (Expr, For_GT => Full_GL_Type (Expr));
                  else
                     Set_Value (Expr, Emit_Expression (Expr));
                  end if;
               end;
            end if;

         when others =>
            pragma Assert (Decls_Only);
      end case;

      --  If we're in the elaboration procedure, check if we're violating a
      --  No_Elaboration_Code restriction by having generated code.

      if (In_Elab_Proc or else In_Elab_Proc_Stmts)
        and then not Is_Equivalent_Position (Start_Position,
                                             Get_Current_Position)
      then
         Check_Elaboration_Code_Allowed (N);
      end if;

   end Emit;

   --------------------
   -- Emit_Safe_Expr --
   --------------------

   function Emit_Safe_Expr
     (N : N_Subexpr_Id; LHS : GL_Value := No_GL_Value) return GL_Value is
   begin
      return V : GL_Value do
         Push_LValue_List;
         Push_Debug_Freeze_Pos;
         V := Emit_Expression (N, LHS => LHS);
         Pop_Debug_Freeze_Pos;
         Pop_LValue_List;
      end return;
   end Emit_Safe_Expr;

   -----------------
   -- Emit_LValue --
   -----------------

   function Emit_LValue
     (N          : N_Subexpr_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False) return GL_Value
   is
      Result : GL_Value;
   begin
      --  We have an important special case here. If N represents an entity
      --  and its value is a Reference, always return that reference in
      --  preference to returning its value and forcing it into memory.
      --  But don't do this for subprograms since they may need static
      --  links and avoid variables that are in activation records.

      if Is_Entity_Name (N) and then Ekind (Entity (N)) not in Subprogram_Kind
        and then No (Get_From_Activation_Record (Entity (N)))
        and then Present (Get_Value (Entity (N)))
        and then Is_Single_Reference (Get_Value (Entity (N)))
      then
         Result := Get_Value (Entity (N));
      else
         Result :=
           Emit (N, LHS, For_LHS => For_LHS, Prefer_LHS => True);

         --  If what we've got now is a Reference_To_Bounds_And_Data let's
         --  return it instead of turning it into Any_Reference. Doing so
         --  would throw away the pointer to the bounds; we can recompute
         --  it (because the bounds are in front of the data in memory) but
         --  for some targets this isn't possible in an expression context
         --  (e.g., on Morello, where address arithmetic requires calls to
         --  intrinsics).

         if Relationship (Result) /= Reference_To_Bounds_And_Data then
            Result := Get (Result, Any_Reference);
         end if;
      end if;

      return Result;
   end Emit_LValue;

   ----------------------
   -- Emit_Safe_LValue --
   ----------------------

   function Emit_Safe_LValue
     (N          : N_Subexpr_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False) return GL_Value is
   begin
      return V : GL_Value do
         Push_LValue_List;
         Push_Debug_Freeze_Pos;
         V := Emit_LValue (N, LHS => LHS, For_LHS => For_LHS);
         Pop_Debug_Freeze_Pos;
         Pop_LValue_List;
      end return;
   end Emit_Safe_LValue;

   ----------
   -- Emit --
   ----------

   function Emit
     (N          : N_Subexpr_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False;
      Prefer_LHS : Boolean  := False) return GL_Value
   is
      Is_Volatile : constant Boolean :=
        (Nkind (N) /= N_Defining_Identifier
           and then Is_Volatile_Reference (N));
      Is_Atomic   : constant Boolean :=
        Is_Atomic_Object (N)
        or else (Nkind (N) in N_Identifier | N_Expanded_Name
                              | N_Explicit_Dereference | N_Indexed_Component
                              | N_Selected_Component
                   and then Atomic_Sync_Required (N));
      Result      : GL_Value         :=
        Emit_Internal (N, LHS, For_LHS => For_LHS, Prefer_LHS => Prefer_LHS);

   begin
      --  If we have an overflow, convert it to an undef. Unless we're to
      --  suppress the error, also give an error and emit a raise.

      if Overflowed (Result) then
         Result := Get_Undef_Relationship (Related_Type (Result),
                                           Relationship (Result));
         if Suppress_Overflow_Depth = 0 then
            Error_Msg_N ("??Constraint_Error will be raised at run time",
                         N);
            Emit_Raise_Call (N, CE_Overflow_Check_Failed);
         end if;
      end if;

      --  Now mark the result as volatile or atomic as needed, maybe add
      --  it to the LValue list, and return it.

      Mark_Volatile (Result, Is_Volatile or else Is_Atomic);
      Mark_Atomic   (Result, Is_Atomic);
      return Add_To_LValue_List (Result);
   end Emit;

   -------------------------
   -- Simple_Value_Action --
   -------------------------

   function Simple_Value_Action
     (N : N_Expression_With_Actions_Id; Has_All : out Boolean)
     return Opt_N_Subexpr_Id
   is
      Action : Node_Id      := First (Actions (N));
      Expr   : N_Subexpr_Id := Expression (N);
      Freeze : Node_Id      := First (Actions (N));

   begin
      --  Skip any non-executable nodes

      while Nkind (Action) in N_Call_Marker | N_Null_Statement |
                              N_Full_Type_Declaration | N_Subtype_Declaration
        or else (Nkind (Action) = N_Freeze_Entity
                   and then No (Actions (Action)))
      loop
         Next (Action);
      end loop;

      --  If the expression of this node is an N_Explicit_Dereference, note
      --  it and get the inner expression.

      Has_All := Nkind (Expr) = N_Explicit_Dereference;

      if Has_All then
         Expr := Prefix (Expr);
      end if;

      --  If the next action isn't the last or isn't a declaration of the
      --  identifier in Expression, this is not a case we handle.

      if Nkind (Action) /= N_Object_Declaration
        or else Present (Next (Action))
        or else Nkind (Expr) /= N_Identifier
        or else Defining_Identifier (Action) /= Entity (Expr)
      then
         return Empty;
      end if;

      --  Process any freeze nodes we may have skipped

      while Present (Freeze) loop
         if Nkind (Freeze) = N_Freeze_Entity then
            Process_Freeze_Entity (Freeze);
         end if;

         Next (Freeze);
      end loop;

      --  If we have an N_Explicit_Dereference and Action's expression is
      --  an N_Reference, use the inner expression.

      return Init : Opt_N_Subexpr_Id := Expression (Action) do
         if  Has_All and then Nkind (Init) = N_Reference then
            Has_All := False;
            Init    := Prefix (Init);
         end if;
      end return;

   end Simple_Value_Action;

   --------------------
   --  Emit_Internal --
   --------------------

   function Emit_Internal
     (N          : N_Subexpr_Id;
      LHS        : GL_Value := No_GL_Value;
      For_LHS    : Boolean  := False;
      Prefer_LHS : Boolean  := False) return GL_Value
   is
      GT     : constant GL_Type := Full_GL_Type (N);
      Result : GL_Value;

   begin
      Set_Debug_Pos_At_Node (N);

      case Nkind (N) is
         when N_Binary_Op =>
            pragma Assert (not For_LHS);

            if Nkind (N) in N_Op_Compare then
               return Emit_Comparison (Nkind (N), Left_Opnd (N),
                                       Right_Opnd (N));
            elsif Nkind (N) in N_Op_Shift then
               return Emit_Shift (Nkind (N), Left_Opnd (N), Right_Opnd (N));
            elsif Nkind (N) in N_Op_And | N_Op_Or | N_Op_Xor
              and then Is_Boolean_Type (GT)
            then
               return
                 Emit_And_Or_Xor (Nkind (N), Left_Opnd (N), Right_Opnd (N));
            else
               return Emit_Binary_Operation (N);
            end if;

         when N_Unary_Op =>
            pragma Assert (not For_LHS);

            --  Check for the special case of taking the NOT of a comparison,
            --  in which case we can just emit a different comparison.

            if Nkind (N) = N_Op_Not
              and then Nkind (Right_Opnd (N)) in N_Op_Compare
            then
               return Emit_Comparison ((case Nkind (Right_Opnd (N)) is
                                          when N_Op_Eq => N_Op_Ne,
                                          when N_Op_Ne => N_Op_Eq,
                                          when N_Op_Lt => N_Op_Ge,
                                          when N_Op_Le => N_Op_Gt,
                                          when N_Op_Gt => N_Op_Le,
                                          when N_Op_Ge => N_Op_Lt,
                                          when others  => N_Op_Not),
                                        Left_Opnd  (Right_Opnd (N)),
                                        Right_Opnd (Right_Opnd (N)));
            else
               return Emit_Unary_Operation (N);
            end if;

         when N_Expression_With_Actions => Expression_With_Actions : declare
            Has_All : Boolean;
            Expr    : constant Opt_N_Subexpr_Id :=
              Simple_Value_Action (N, Has_All);

         begin
            --  If this is just defining the value that is to be its
            --  result, just expand the initializer.

            if Present (Expr) then
               Result := Emit (Expr,
                               LHS        => LHS,
                               For_LHS    => For_LHS,
                               Prefer_LHS => Prefer_LHS);
               if Has_All then
                  Result := From_Access (Result);
               end if;

               return Result;
            end if;

            --  Otherwise do each action and evaluate our expression

            Push_LValue_List;
            Emit (Actions (N));
            Pop_LValue_List;
            return Emit (Expression (N),
                         LHS        => LHS,
                         For_LHS    => For_LHS,
                         Prefer_LHS => Prefer_LHS);
         end Expression_With_Actions;

         when N_Character_Literal | N_Numeric_Or_String_Literal =>
            pragma Assert (not For_LHS);

            return Emit_Literal (N);

         when N_And_Then | N_Or_Else =>
            pragma Assert (not For_LHS);

            if Safe_For_Short_Circuit (Left_Opnd (N))
              and then Safe_For_Short_Circuit (Right_Opnd (N))
              and then Is_Simple_Conditional (N)
            then
               return Emit_And_Or_Xor ((if   Nkind (N) = N_And_Then
                                        then N_Op_And else N_Op_Or),
                                       Left_Opnd (N), Right_Opnd (N));
            else
               return Build_Short_Circuit_Op (Left_Opnd (N), Right_Opnd (N),
                                              Nkind (N));
            end if;

         when N_Unchecked_Type_Conversion => Unchecked_Conversion : declare
            Expr   : constant N_Subexpr_Id := Expression (N);
            BT     : constant Type_Kind_Id := Full_Base_Type (GT);

         begin
            --  The result can't have overflowed (this is unchecked), but
            --  if this is not just converting between subtypes of the same
            --  base type, it must be marked as aliasing everything.

            Result := Emit_Conversion (Expr, GT, N,
                                       Is_Unchecked  => True,
                                       No_Truncation => No_Truncation (N));
            Clear_Overflowed (Result);

            if Full_Base_Type (Full_Etype (Expr)) /= BT then
               Set_Aliases_All (Result);
            end if;

            return Result;
         end Unchecked_Conversion;

         when N_Type_Conversion =>
            return Emit_Conversion
              (Expression (N), GT, N,
               Need_Overflow_Check => Do_Overflow_Check (N),
               Float_Truncate      => Float_Truncate (N));

         when N_Qualified_Expression =>
            return Emit_Conversion (Expression (N), GT, N);

         when N_Entity_Name =>
            return Emit_Entity (Entity (N), N, Prefer_LHS => Prefer_LHS);

         when N_Function_Call =>

            --  If we're only elaborating decls, we may have a call to a
            --  function whose Name is an N_Selected_Component. This is
            --  an unexpanded tasking-related call. Skip it and hope there
            --  are no types only in that call.

            if Decls_Only and then Nkind (Name (N)) = N_Selected_Component then
               return Emit_Undef (GT);
            else
               pragma Assert (Decls_Only or else not For_LHS);
               return Emit_Call (N, Outer_LHS => LHS);
            end if;

         when N_Explicit_Dereference =>

            --  If we have a .all of a 'Reference, we can just evaluate
            --  the inner expression. This allows us to pass our LHS info.

            if Nkind (Prefix (N)) = N_Reference then
               return Emit (Prefix (Prefix (N)),
                            LHS        => LHS,
                            For_LHS    => For_LHS,
                            Prefer_LHS => Prefer_LHS);
            else
               --  Get a reference to our prefix

               Result := From_Access (Emit_Expression (Prefix (N)));
            end if;

            --  If we have a reference to a global constant, we can
            --  use the value instead. But we can't do this if we're
            --  pointing to a variable-sized type because we've lost the
            --  type of what this points to. We also can't do this if pointer
            --  punning has been done so that the type of the global variable
            --  doesn't agree with the type we want.
            --
            --  ??? We could optimize this in some cases, such as
            --  where padding is involved, but it's not worth it.

            if Is_Reference (Result) and then Is_A_Global_Variable (Result)
              and then Is_Global_Constant (Result)
              and then not Is_Nonnative_Type (Result)
              and then Type_Of (Related_Type (Result)) =
                       Global_Get_Value_Type (Result)
            then
               Result := Get_Initializer (Result);
            end if;

            --  Finally see if we have an Actual_Designated_Subtype that we
            --  have to convert to.

            if Present (Actual_Designated_Subtype (N)) then
               Result := Convert_Ref (Get (Result, Reference),
                                      Default_GL_Type
                                        (Get_Fullest_View
                                           (Actual_Designated_Subtype (N))));
            end if;

            return Result;

         when N_Allocator =>

            --  If we're just analying decls, don't go any further since
            --  Expression may not be set properly.

            if Decls_Only then
               return Get_Undef (GT);
            end if;

            declare
               Expr  : constant N_Subexpr_Id := Expression (N);
               Value : GL_Value              := No_GL_Value;
               A_GT  : GL_Type;

            begin
               --  There are two cases: the Expression operand can either
               --  be an N_Identifier or Expanded_Name, which must
               --  represent a type, or a N_Qualified_Expression, which
               --  contains both the object type and an initial value for
               --  the object.

               pragma Assert (not For_LHS);

               if Is_Entity_Name (Expr) then
                  A_GT  := Default_GL_Type (Get_Fullest_View (Entity (Expr)));
                  Value := No_GL_Value;
               else
                  pragma Assert (Nkind (Expr) = N_Qualified_Expression);
                  A_GT  := Full_GL_Type (Expression (Expr));
                  Value := Emit_Expression (Expression (Expr));
               end if;

               --  If GT's designated type is a record with discriminants
               --  and there's no Value, we're usually passed a subtype as
               --  A_GT. But in some cases (such as where it's limited), we
               --  aren't.

               Result := Heap_Allocate_For_Type
                 (Full_Designated_GL_Type (GT), A_GT,
                  V         => Value,
                  N         => N,
                  Access_GT => Full_GL_Type (N),
                  Proc      => Procedure_To_Call (N),
                  Pool      => Storage_Pool (N),
                  Max_Size  => (Is_Unconstrained_Record (A_GT)
                                  and then No (Value)));
               return Convert_To_Access (Result, GT);
            end;

         when N_Reference =>

            --  If we have a 'Reference of a .all, we can just evaluate
            --  the inner expression. This allows us to pass our LHS info.

            if Nkind (Prefix (N)) = N_Explicit_Dereference then
               return Emit (Prefix (Prefix (N)),
                            LHS        => LHS,
                            For_LHS    => For_LHS,
                            Prefer_LHS => Prefer_LHS);
            else
               --  It's tempting to mark the call below as For_LHS, but we
               --  do allow taking 'Reference of something that's not an
               --  LValue (though an assignment to it will fail in that
               --  case).

               return Convert_To_Access (Emit_LValue (Prefix (N)), GT);
            end if;

         when N_Attribute_Reference =>
            return Emit_Attribute_Reference (N);

         when N_Selected_Component =>

            --  If we're just processing declarations, make sure we've
            --  elaborated the type of the prefix and do nothing more.

            if Decls_Only then
               Discard (Full_GL_Type (Prefix (N)));
               return Emit_Undef (GT);
            else
               return Maybe_Convert_GT
                 (Build_Field_Load (Emit (Prefix (N),
                                          For_LHS    => For_LHS,
                                          Prefer_LHS => Prefer_LHS),
                                    Entity (Selector_Name (N)),
                                    LHS        => LHS,
                                    For_LHS    => For_LHS,
                                    Prefer_LHS => Prefer_LHS,
                                    VFA        =>
                                      Has_Full_Access (Prefix (N))),
                  GT);
            end if;

         when N_Indexed_Component | N_Slice =>
            Result := Emit (Prefix (N),
                            For_LHS    => For_LHS,
                            Prefer_LHS => Prefer_LHS);

            --  If we're just processing decls, the above is all we have to do

            if Decls_Only then
               return Emit_Undef (GT);

            --  This can be an integer type if it's the implementation type
            --  of a packed array type. In that case, convert it to the
            --  result type.

            elsif Is_Integer_Type (Related_Type (Result))
              and then Is_Packed_Array_Impl_Type (Related_Type (Result))
            then
               --  Evaluate any expressions in case they have side-effects

               declare
                  Expr : Opt_N_Subexpr_Id := First (Expressions (N));

               begin
                  while Present (Expr) loop
                     if not Is_No_Elab_Needed (Expr) then
                        Discard (Emit (Expr));
                     end if;

                     Next (Expr);
                  end loop;
               end;

               return (if   Is_Reference (Result) then Convert_Ref (Result, GT)
                       else Convert (Result, GT));

            elsif Nkind (N) = N_Indexed_Component then
               return Maybe_Convert_GT
                 (Build_Indexed_Load (Result,
                                      Get_Indices (Expressions (N), Result),
                                      For_LHS    => For_LHS,
                                      Prefer_LHS => Prefer_LHS,
                                      VFA        =>
                                        Has_Full_Access (Prefix (N))),
                  GT);

            else
               return Get_Slice_LValue (GT, Get (Result, Any_Reference));
            end if;

         when N_Aggregate | N_Extension_Aggregate =>
            pragma Assert (not For_LHS);

            if Null_Record_Present (N) and then not Is_Nonnative_Type (GT) then
               return Const_Null (GT);

            elsif Ekind (GT) in Record_Kind then
               return Convert_GT
                 (Emit_Record_Aggregate
                    (N, (if   Present (LHS) and then Is_Safe_From (LHS, N)
                           then LHS else No_GL_Value)),
                  GT);

            elsif not Is_Array_Type (GT) then
               pragma Assert (Decls_Only);
               return Emit_Undef (GT);
            else
               --  The back-end supports exactly two types of array
               --  aggregates. One is for a fixed-size aggregate. The other
               --  are very special cases of Others that are tested for in
               --  Aggr_Assignment_OK_For_Backend in Exp_Aggr.

               return Emit_Array_Aggregate
                 (N, Number_Dimensions (GT), (1 .. 0 => <>),
                  (if   Present (LHS) and then Is_Safe_From (LHS, N) then LHS
                   else No_GL_Value));
            end if;

         when N_If_Expression =>
            pragma Assert (not For_LHS);

            return Emit_If_Expression (N, LHS => LHS);

         when N_Null =>
            pragma Assert (not For_LHS);

            return Const_Null (GT);

         when N_In => In_Expr : declare
            Left       : constant GL_Value := Emit_Expression (Left_Opnd (N));
            Rng        : N_Is_Index_Id     := Right_Opnd (N);
            Compare_LB : GL_Value;
            Compare_HB : GL_Value;

         begin
            if Decls_Only then
               return Get (Get_Undef (Boolean_GL_Type), Boolean_Data);
            end if;

            pragma Assert (not For_LHS);
            pragma Assert (No (Alternatives (N)));

            if Is_Entity_Name (Rng) then
               Rng := Simplify_Range (Scalar_Range (Full_Etype (Rng)));
            end if;

            Compare_LB := Build_Elementary_Comparison
              (N_Op_Ge, Left, Emit_Expression (Low_Bound (Rng)));
            Compare_HB := Build_Elementary_Comparison
              (N_Op_Le, Left, Emit_Expression (High_Bound (Rng)));
            return Build_And (Compare_LB, Compare_HB);
         end In_Expr;

         when N_Raise_xxx_Error =>
            pragma Assert (No (Condition (N)));

            Emit_Raise (N);
            return Emit_Undef (GT);

         when others =>
            pragma Assert (Decls_Only);

            return Emit_Undef (GT);
      end case;
   end Emit_Internal;

   ----------
   -- Emit --
   ----------

   procedure Emit (List : List_Id) is
      N : Node_Id;

   begin
      if Present (List) then
         N := First (List);
         while Present (N) loop

            --  If N is an N_Handled_Sequence_Of_Statements here, we know
            --  that it's not nested in a block. It probably was from a
            --  package body at library level and ended in the elab proc.
            --  Make a block around it.

            if Nkind (N) = N_Handled_Sequence_Of_Statements then
               Push_Block;
               Emit (N);
               Pop_Block;

            else
               Emit (N);
            end if;

            Next (N);
         end loop;
      end if;
   end Emit;

   --  Freeze nodes for package bodies are relatively rare, so we can store
   --  information about them in a table that we search for the relevant
   --  entity. We need to handle the case where we're at library level (so
   --  what we have to save is the position into the elab table) or in
   --  code, where we need to save a pointer to a branch we add to a new
   --  basic block that we made. Note that in the latter case, we can't use
   --  Get_Current_Position / Set_Current_Position because those are
   --  intended for adding individual instructions within a basic block but
   --  here we need to insert large amounts of code, including basic
   --  blocks.

   type Code_Position (Library : Boolean := False) is record
      E : E_Package_Id;
      case Library is
         when True =>
            Elab_Ptr : Nat;

         when False =>
            Branch_Inst : Value_T;
      end case;
   end record;

   package Code_Positions is new Table.Table
     (Table_Component_Type => Code_Position,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Code_Positions");

   --------------------------
   -- Record_Code_Position --
   --------------------------

   procedure Record_Code_Position (E : E_Package_Id) is
   begin
      if Library_Level then

         --  Add a dummy entry into the elab list so that multiple
         --  consecutive calls will have different positions and record our
         --  position in the list.

         Add_To_Elab_Proc (Empty);
         Code_Positions.Append ((True, E, Get_Elab_Position));

      else
         --  Create a new basic block and branch to it. Later, we'll
         --  replace the branch we made to a branch to our new code and
         --  then branch to that new block.

         declare
            BB : constant Basic_Block_T := Create_Basic_Block;

         begin
            Code_Positions.Append ((False, E, Build_Br (IR_Builder, BB)));
            Position_Builder_At_End (BB);
         end;
      end if;

   end Record_Code_Position;

   ---------------------
   -- Insert_Code_For --
   ---------------------

   procedure Insert_Code_For (E : E_Package_Id) is
      Pkg_Body     : constant Opt_E_Package_Body_Id :=
        Corresponding_Body (Parent (Declaration_Node (E)));
      Code_To_Emit : constant Opt_N_Package_Body_Id :=
        (if Present (Pkg_Body) then Parent (Pkg_Body) else Empty);

   begin
      if No (Code_To_Emit) then
         return;
      end if;

      for J in 1 .. Code_Positions.Last loop
         declare
            RCP  : constant Code_Position := Code_Positions.Table (J);
         begin
            if RCP.E = E then
               pragma Assert (Library_Level = RCP.Library);

               if RCP.Library then

                  --  Get the elab pointer that we saved and the first one
                  --  that we'll generate.

                  declare
                     Prev_Ptr  : constant Nat := RCP.Elab_Ptr;
                     Start_Ptr : constant Nat := Get_Elab_Position;
                     Last_Ptr  : Nat;

                  begin
                     --  Then emit our code and reorder the elab entries

                     Emit (Code_To_Emit);
                     Last_Ptr := Get_Elab_Position;
                     Reorder_Elab_Table (Prev_Ptr, Start_Ptr);

                     --  Now look for other pointers that have been stored
                     --  in the code position table and are behind us and
                     --  update them to account for our length.

                     for K in 1 .. Code_Positions.Last loop
                        declare
                           RCP2 : Code_Position
                             renames Code_Positions.Table (K);

                        begin
                           if RCP2.Library and then RCP2.Elab_Ptr > Prev_Ptr
                           then
                              RCP2.Elab_Ptr
                                := RCP2.Elab_Ptr + Last_Ptr - Start_Ptr;
                           end if;
                        end;
                     end loop;
                  end;
               else
                  --  Make a new block and and get pointers to all the
                  --  relevant blocks. Then rewrite the branch to point to
                  --  our code, emit our code, and branch to the new block
                  --  that we used to branch to.

                  declare
                     Our_BB  : constant Basic_Block_T := Get_Insert_Block;
                     New_BB  : constant Basic_Block_T := Create_Basic_Block;
                     Inst    : constant Value_T       := RCP.Branch_Inst;
                     Old_BB  : constant Basic_Block_T :=
                       Get_Instruction_Parent (Inst);
                     Targ_BB : constant Basic_Block_T :=
                       Value_As_Basic_Block (Get_Operand (Inst, 0));

                  begin
                     Instruction_Erase_From_Parent (Inst);
                     Position_Builder_At_End (Old_BB);
                     Move_To_BB (New_BB);
                     Emit (Code_To_Emit);
                     Build_Br (Targ_BB);
                     Position_Builder_At_End (Our_BB);
                  end;
               end if;
            end if;
         end;
      end loop;
   end Insert_Code_For;

   ---------------------------
   -- Process_Freeze_Entity --
   ---------------------------

   procedure Process_Freeze_Entity (N : N_Freeze_Entity_Id) is
      E      : constant Entity_Id        := Entity (N);
      Full_E : constant Entity_Id        :=
        (if Is_Type (E) then Get_Fullest_View (E) else E);
      Decl   : constant Opt_N_Is_Decl_Id :=
        (if   Is_Ignored_Ghost_Entity (Full_E) then Empty
         else Declaration_Node (Full_E));

   begin
      --  If there's no declaration node, we have nothing to do

      if No (Decl) then
         return;
      end if;

      --  Otherwise, see what type of declaration this is

      case Nkind (Decl) is
         when N_Object_Declaration | N_Exception_Declaration =>

            --  For objects, perform the object declaration

            Emit_Declaration (Decl, For_Freeze_Entity => True);

         when N_Procedure_Specification | N_Function_Specification =>

            --  For subprograms, the decl node points to the subprogram
            --  specification. We only want to consider "normal"
            --  subprograms that aren't intrinsic, so we not only test for
            --  intrinsic but for an N_Subprogram_Declaration, as opposed
            --  to, for example an N_Abstract_Subprogram_Declaration, which
            --  we don't process. We also have to test for protected
            --  subprograms and finally ignore functions that return arrays
            --  because they have been rewritten as procedures.

            if not Is_Intrinsic_Subprogram (E)
              and then Nkind (Parent (Decl)) = N_Subprogram_Declaration
              and then Convention (E) /= Convention_Protected
              and then No (Protected_Body_Subprogram (E))
              and then not Is_Eliminated (E)
              and then not
                (Ekind (Defining_Unit_Name (Decl)) = E_Function
                  and then Rewritten_For_C (Defining_Unit_Name (Decl)))
            then
               Discard (Emit_Subprogram_Decl (Decl));
            end if;

         when N_Package_Specification =>

            --  Write out the code for this specification at the point of the
            --  initial declaration.

            Insert_Code_For (E);

         when others =>
            null;
      end case;
   end Process_Freeze_Entity;

end GNATLLVM.Compile;
