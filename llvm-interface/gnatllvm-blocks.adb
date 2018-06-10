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
with Exp_Ch11; use Exp_Ch11;
with Exp_Unst; use Exp_Unst;
with Nlists;   use Nlists;
with Restrict; use Restrict;
with Sinput;   use Sinput;
with Stand;    use Stand;
with Table;    use Table;
with Uintp;    use Uintp;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Conditionals; use GNATLLVM.Conditionals;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Variables;    use GNATLLVM.Variables;

package body GNATLLVM.Blocks is

   --  This data structure records the information about each block that
   --  we're in and we construct a table to act as a block stack.

   type Block_Info is record
      In_Stmts           : Boolean;
      --  True if we are in the statement section of the current block

      Unprotected        : Boolean;
      --  True if we've reached the pop of the block (in the end handler
      --  or its fixup) where calls aren't protected by exceptions or
      --  At_End handlers in this block.

      Stack_Save         : GL_Value;
      --  Value of the stack pointer at entry to the block

      At_End_Proc        : GL_Value;
      --  Procedure to be called at normal or abnormal exit of the block

      At_End_Parameter   : GL_Value;
      --  A parameter to pass to the At_End_Proc, for example an
      --  activation record.

      Landing_Pad        : Basic_Block_T;
      --  Basic block containing the landing pad for this block, if any.

      EH_List            : List_Id;
      --  List of exception handlers

      Dispatch_BB        : Basic_Block_T;
      --  BB created by an inner handler to branch to our dispatch code

      Reraise_BB         : Basic_Block_T;
      --  BB created by a reraise for the fallthrough of our dispatch code

   end record;

   type Block_Stack_Level is new Integer;
   --  Type to record depth of block stack

   package Block_Stack is new Table.Table
     (Table_Component_Type => Block_Info,
      Table_Index_Type     => Block_Stack_Level,
      Table_Low_Bound      => 1,
      Table_Initial        => 15,
      Table_Increment      => 5,
      Table_Name           => "Block_Stack");
   --  Stack of blocks that we're in.

   --  We need to jump from the end of one piece dispatch code to the start
   --  of the outer dispatch code when we have nested handlers.  To do
   --  this, we need a Phi for the exception information (or a variable,
   --  but finding a scope for that variable is non-trivial).  We record
   --  that in this table, which contains the starting BB for dispatch code
   --  (Dispatch_BB from the above) and the BB that jumps to it as well as
   --  the exception data location (for the Phi).  We don't try to remove
   --  these from the table when used because it's not worth the
   --  complexity: there aren't that many nested handlers.

   type D_D_Info is record
      Dispatch_BB  : Basic_Block_T;
      From_BB      : Basic_Block_T;
      From_EH_Data : GL_Value;
   end record
     with Predicate => Present (Dispatch_BB) and then Present (From_BB)
                       and then Present (From_EH_Data);

   package Dispatch_Info is new Table.Table
     (Table_Component_Type => D_D_Info,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 10,
      Table_Name           => "Dispatch_Info");
   --  Stack of labels for constraint error

   --  These tables implement local exception handling, where a
   --  language-defined check within a block jumps directly to a label
   --  associated with the actions for that exception.

   package Constraint_Error_Stack is new Table.Table
     (Table_Component_Type => Entity_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 1,
      Table_Name           => "Constraint_Error_Stack");
   --  Stack of labels for constraint error

   package Storage_Error_Stack is new Table.Table
     (Table_Component_Type => Entity_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 1,
      Table_Name           => "Storage_Error_Stack");
   --  Stack of labels for storage error

   package Program_Error_Stack is new Table.Table
     (Table_Component_Type => Entity_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 1,
      Table_Name           => "Program_Error_Stack");
   --  Stack of labels for program error

   type Exit_Point is record
      Label_Entity : Entity_Id;
      Exit_BB      : Basic_Block_T;
      Block_Depth  : Block_Stack_Level;
   end record;

   Exit_Point_Low_Bound : constant := 1;

   package Exit_Point_Table is new Table.Table
     (Table_Component_Type => Exit_Point,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => Exit_Point_Low_Bound,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Exit_Point_Table");
   --  Table of scoped loop exit points. Last inserted exit point correspond
   --  to the innermost loop.

   procedure Emit_One_Fixup
     (Blk : Block_Stack_Level; Do_At_End, Do_Stack : Boolean);
   --  Do one fixup when exiting Blk, saying whether to run "at end handler
   --  and whether to restore the stack pointer.

   procedure Emit_Handlers (Block : Block_Stack_Level);
   --  Generate the parts of a block at level Block used for exception
   --  handling, including at end procs and exception handlers.  This
   --  includes the landingpad instruction, the exception handlers, code to
   --  dispatch to the handlers, and code to handle falling through.  If a
   --  landingpad was requested, the block will start with a landingpad
   --  instruction starting that basic block.  Otherwise, there may not be
   --  a need for a landing pad.  Return the actual landing pad
   --  instruction, if any.

   function Get_File_Name_Address
     (Index : Source_File_Index) return GL_Value
     with Post => Type_Of (Get_File_Name_Address'Result) = LLVM_Size_Type;
   --  Return a GL_Value giving the address of a string corresponding to
   --  the name of the file with the specified file index.

   function Get_Raise_Fn (Kind : RT_Exception_Code) return GL_Value
     with Post => Present (Get_Raise_Fn'Result);
   --  Get function for raising a builtin exception of Kind

   function Get_Set_EH_Param_Fn (Exc_Type : Entity_Id) return GL_Value
     with Pre  => Is_Type (Exc_Type),
          Post => Present (Get_Set_EH_Param_Fn'Result);
   --  Get (and create if needed) the function that sets the exception
   --  parameter.  This can only be called once we have an exception parameter
   --  since we can't easily find the exception type before that.

   procedure Emit_Raise_Call (N : Node_Id; Kind : RT_Exception_Code)
     with Pre => Present (N);
   --  Generate a call to __gnat_last_chance_handler

   procedure Initialize_Predefines;
   --  Initialize the predefined functions and variables below

   Predefines_Set   : Boolean  := False;
   --  True when all of the below have been initialized

   Personality_Fn   : GL_Value;
   --  The definition of the personality function

   EH_Slot_Id_Fn    : GL_Value;
   --  The LLVM builtin that converts an exception into a slot number

   Begin_Handler_Fn : GL_Value;
   End_Handler_Fn   : GL_Value;
   --  Begin and end functions for handlers

   Others_Value     : GL_Value;
   All_Others_Value : GL_Value;
   --  "Exception" address for "others" and special "all others"

   Set_Exception_Param_Fn : GL_Value := No_GL_Value;
   --  Declaration for __gnat_set_exception_parameter.  This can't be
   --  initialized with the ones above since we need its type.

   LCH_Fn            : GL_Value := No_GL_Value;
   --  Last-chance handler.  We only initialize this if needed

   type File_GL_Value_Array is array (Source_File_Index range <>) of GL_Value;
   File_Name_Strings : access File_GL_Value_Array := null;
   --  Array of GL_Values corresponding to 'Address of the string literal
   --  representing the name of the file.

   type String_Access is access String;
   type Rcheck_Name_Array is array (RT_Exception_Code) of String_Access;

   Rcheck_Names      : Rcheck_Name_Array;
   --  Array of pointers to strings giving the names of the functions for
   --  raising builtin exceptions of various kinds.

   Rcheck_FNs        : array (RT_Exception_Code'Range) of GL_Value :=
     (others => No_GL_Value);
   --  Array of functions to call for raising builtin exceptions of
   --  various kinds.

   ----------------
   -- Push_Block --
   ----------------

   procedure Push_Block is
      Stack_Save : constant GL_Value :=
        (if Block_Stack.Last < 1 then No_GL_Value
         else Call (Get_Stack_Save_Fn, Standard_A_Char, (1 .. 0 => <>)));
   begin
      Block_Stack.Append ((Stack_Save       => Stack_Save,
                           At_End_Proc      => No_GL_Value,
                           At_End_Parameter => No_GL_Value,
                           Landing_Pad      => No_BB_T,
                           Dispatch_BB      => No_BB_T,
                           Reraise_BB       => No_BB_T,
                           EH_List          => No_List,
                           In_Stmts         => False,
                           Unprotected      => False));

   end Push_Block;

   -----------------------------
   --  Start_Block_Statements --
   -----------------------------

   procedure Start_Block_Statements
     (At_End_Proc : Entity_Id; EH_List : List_Id) is

   begin
      pragma Assert (not Block_Stack.Table (Block_Stack.Last).In_Stmts);

      Block_Stack.Table (Block_Stack.Last).EH_List  := EH_List;
      Block_Stack.Table (Block_Stack.Last).In_Stmts := True;

      if Present (At_End_Proc) then

         --  Save both the end proc and the value of the static link.
         --  Since we'll be generating the call directly, we have to convert
         --  the static link to the proper pointer type for the activation
         --  record.  There may not be a static link, however, if there re
         --  no uplevel references.

         Block_Stack.Table (Block_Stack.Last).At_End_Proc :=
           Emit_LValue (At_End_Proc);
         if Subps_Index (Entity (At_End_Proc)) /= Uint_0
           and then Present (Subps.Table (Subp_Index
                                            (Entity (At_End_Proc))).ARECnF)
         then
            Block_Stack.Table (Block_Stack.Last).At_End_Parameter :=
              Pointer_Cast (Get_Static_Link (At_End_Proc),
                            Full_Etype (Extra_Formals (Entity (At_End_Proc))));
         end if;
      end if;

   end Start_Block_Statements;

   ---------------------
   -- Get_Landing_Pad --
   ---------------------

   function Get_Landing_Pad return Basic_Block_T is
      BI : Block_Info;

   begin
      --  If we're in the Statements part of a block that has nexceptions,
      --  see if we've made a block for the landing-pad.  If not, make one.

      for J in reverse 1 .. Block_Stack.Last loop
         BI := Block_Stack.Table (J);
         if (Present (BI.EH_List) and then BI.In_Stmts)
           or else (Present (BI.At_End_Proc) and then not BI.Unprotected)
         then
            if No (BI.Landing_Pad) then
               Block_Stack.Table (J).Landing_Pad :=
                 Create_Basic_Block ("Lpad");
            end if;

            return Block_Stack.Table (J).Landing_Pad;
         end if;
      end loop;

      return No_BB_T;
   end Get_Landing_Pad;

   --------------------
   -- Emit_One_Fixup --
   --------------------

   procedure Emit_One_Fixup
     (Blk : Block_Stack_Level; Do_At_End, Do_Stack : Boolean)
   is
      Block_Inf : constant Block_Info := Block_Stack.Table (Blk);

   begin
      --  First call the "at end" handler before any variables get
      --  deallocated.

      if Do_At_End and then Present (Block_Inf.At_End_Proc) then
         if Present (Block_Inf.At_End_Parameter) then
            Call (Block_Inf.At_End_Proc, (1 => Block_Inf.At_End_Parameter));
         else
            Call (Block_Inf.At_End_Proc, (1 .. 0 => <>));
         end if;
      end if;

      --  Then deallocate variables

      if Do_Stack and then Present (Block_Inf.Stack_Save) then
         Call (Get_Stack_Restore_Fn, (1 => Block_Inf.Stack_Save));
      end if;
   end Emit_One_Fixup;

   ---------------------------
   -- Initialize_Predefines --
   ---------------------------

   procedure Initialize_Predefines is
   begin
      if Predefines_Set then
         return;
      end if;

      Personality_Fn  :=
        Add_Global_Function ("__gnat_personality_v0",
                             Fn_Ty ((1 .. 0 => <>), Int_Ty (32), True),
                             Standard_Void_Type);

      Begin_Handler_Fn :=
        Add_Global_Function ("__gnat_begin_handler",
                             Fn_Ty ((1 => Void_Ptr_Type), Void_Type),
                             Standard_Void_Type);

      End_Handler_Fn   :=
        Add_Global_Function ("__gnat_end_handler",
                             Fn_Ty ((1 => Void_Ptr_Type), Void_Type),
                             Standard_Void_Type);

      EH_Slot_Id_Fn    :=
        Add_Function ("llvm.eh.typeid.for",
                      Fn_Ty ((1 => Void_Ptr_Type), Int_Ty (32)), Int_32_Type);
      Set_Does_Not_Throw (EH_Slot_Id_Fn);

      Others_Value     := Add_Global (Standard_Short_Short_Integer,
                                      "__gnat_others_value");
      All_Others_Value := Add_Global (Standard_Short_Short_Integer,
                                      "__gnat_all_others_value");

      Predefines_Set   := True;

   end Initialize_Predefines;

   --------------------------
   -- Get_Set_EH_Param_Fn --
   --------------------------

   function Get_Set_EH_Param_Fn (Exc_Type : Entity_Id) return GL_Value is
   begin
      --  If we haven't already made the function to set the
      --  choice parameter, make it now that we have the type.

      if No (Set_Exception_Param_Fn) then
         Set_Exception_Param_Fn := Add_Global_Function
           ("__gnat_set_exception_parameter",
            Fn_Ty ((1 => Create_Access_Type (Exc_Type), 2 => Void_Ptr_Type),
                   Void_Type),
            Standard_Void_Type);
      end if;

      return Set_Exception_Param_Fn;
   end Get_Set_EH_Param_Fn;

   ------------------
   -- Get_Raise_Fn --
   ------------------

   function Get_Raise_Fn (Kind : RT_Exception_Code) return GL_Value is
      Fun_Type : constant Type_T :=
        Fn_Ty ((1 => LLVM_Size_Type, 2 => Create_Type (Standard_Integer)),
               Void_Type);

   begin
      if No_Exception_Handlers_Set then
         if No (LCH_Fn) then
            LCH_Fn := Add_Global_Function
              ("__gnat_last_chance_handler", Fun_Type,
               Standard_Void_Type, Can_Throw => True, Can_Return => False);
         end if;

         return LCH_Fn;
      else
         if No (Rcheck_FNs (Kind)) then
            Rcheck_FNs (Kind) := Add_Global_Function
              (Rcheck_Names (Kind).all, Fun_Type,
               Standard_Void_Type, Can_Throw => True, Can_Return => False);
         end if;

         return Rcheck_FNs (Kind);
      end if;
   end Get_Raise_Fn;

   ---------------------------
   -- Emit_Overflow_Call_If --
   ---------------------------

   procedure Emit_Overflow_Call_If (V : GL_Value; N : Node_Id) is
      BB_Then  : constant Basic_Block_T := Create_Basic_Block ("raise");
      BB_Next  : constant Basic_Block_T := Create_Basic_Block;

   begin
      Build_Cond_Br (V, BB_Then, BB_Next);
      Position_Builder_At_End (BB_Then);
      Emit_Raise_Call (N, CE_Overflow_Check_Failed);
      Move_To_BB (BB_Next);
   end Emit_Overflow_Call_If;

   ---------------------------
   -- Get_File_Name_Address --
   ---------------------------

   function Get_File_Name_Address
     (Index : Source_File_Index) return GL_Value is
   begin
      if File_Name_Strings = null then
         File_Name_Strings :=
           new File_GL_Value_Array'(1 .. Last_Source_File => No_GL_Value);
      end if;

      if No (File_Name_Strings (Index)) then
         declare
            File     : constant String
              := Get_Name_String (Reference_Name (Index));
            Elements : GL_Value_Array (1 .. File'Length + 1);
            V        : GL_Value;
            Str      : GL_Value;

         begin
            --  First build a string literal for FILE

            for J in File'Range loop
               Elements (Nat (J)) :=
                 Const_Int (Standard_Short_Short_Integer,
                            ULL (Character'Pos (File (J))));
            end loop;

            --  Append NUL character

            Elements (Elements'Last)
              := Const_Null (Standard_Short_Short_Integer);

            Str := Const_Array (Elements, Any_Array);
            V   := G_Ref (Add_Global (LLVM_Module, Type_Of (Str), "fname"),
                          Any_Array);
            Set_Initializer (V, Str);
            Set_Linkage (V, Private_Linkage);
            Set_Global_Constant (LLVM_Value (V), True);
            File_Name_Strings (Index) := Ptr_To_Int (V, Size_Type);
         end;
      end if;

      return File_Name_Strings (Index);
   end Get_File_Name_Address;

   ---------------------
   -- Emit_Raise_Call --
   ---------------------

   procedure Emit_Raise_Call (N : Node_Id; Kind : RT_Exception_Code) is
      File : constant GL_Value :=
        Get_File_Name_Address (Get_Source_File_Index (Sloc (N)));
      Line : constant GL_Value :=
        Const_Int (Standard_Integer, ULL (Get_Logical_Line_Number (Sloc (N))));

   begin
      --  Build a call to __gnat_xx (FILE, LINE)

      Call (Get_Raise_Fn (Kind), (1 => File, 2 => Line));
   end Emit_Raise_Call;

   -------------------
   -- Emit_Handlers --
   -------------------

   procedure Emit_Handlers (Block : Block_Stack_Level) is
      BI                : Block_Info renames Block_Stack.Table (Block);
      Lpad              : constant Basic_Block_T := BI.Landing_Pad;
      EH_List           : constant List_Id       := BI.EH_List;
      At_End_Proc       : constant GL_Value      := BI.At_End_Proc;
      At_End_Parameter  : constant GL_Value      := BI.At_End_Parameter;
      LP_Type           : constant Type_T        :=
        Build_Struct_Type ((1 => Void_Ptr_Type, 2 => Int_Ty (32)));
      Have_Cleanup      : constant Boolean       :=
        (for some J in 1 .. Block =>
           Present (Block_Stack.Table (J).At_End_Proc));
      LP_Inst           : GL_Value               := No_GL_Value;
      N_Dispatch_Froms  : Nat                    :=
        (if Present (Lpad) then 1 else 0);
      Next_BB           : Basic_Block_T;
      DDT               : D_D_Info;
      BB                : Basic_Block_T;
      Handler, Choice   : Node_Id;
      EH_Data           : GL_Value;
      Selector, Exc_Ptr : GL_Value;
      Exc               : GL_Value;

      function Choice_To_Exc (Choice : Node_Id) return GL_Value
        with Pre => Present (Choice), Post => Present (Choice_To_Exc'Result);
      --  Given a Choice from an exception alternative, return a GL_Value
      --  corresponding to that choice, taking into account the special
      --  values use for "others".

      -------------------
      -- Choice_To_Exc --
      -------------------

      function Choice_To_Exc (Choice : Node_Id) return GL_Value is
      begin
         if Nkind (Choice) = N_Others_Choice then
            return (if All_Others (Choice)
                    then All_Others_Value else Others_Value);
         else
            return Emit_LValue (Choice);
         end if;
      end Choice_To_Exc;

      type One_Clause is record
         BB    : Basic_Block_T;
         --  Basic block containing the actions for this exception

         Exc   : GL_Value;
         --  The address of the exception caught by this handler

         Param : Entity_Id;
         --  The value of Choice_Parameter, if any

         Stmts : List_Id;
         --  The statements in the handler
      end record;

      package Clauses is new Table.Table
        (Table_Component_Type => One_Clause,
         Table_Index_Type     => Nat,
         Table_Low_Bound      => 1,
         Table_Initial        => 15,
         Table_Increment      => 5,
         Table_Name           => "Clauses");

      package Exceptions_Seen is new Table.Table
        (Table_Component_Type => GL_Value,
         Table_Index_Type     => Nat,
         Table_Low_Bound      => 1,
         Table_Initial        => 20,
         Table_Increment      => 5,
         Table_Name           => "Exceptions_Seen");

   begin
      --  The exception handling information has four parts: the landingpad
      --  instruction itself, the exception handlers themselves, code to
      --  dispatch to the proper exception handler, and the code that's
      --  executed if none of the handlers in our block are executed.
      --
      --  The landing pad must take into account the requirements of any
      --  block above our own as well as this block.   If this or any upper
      --  block has an At_End proc, we must catch all exceptions, so the
      --  landingpad must contain a cleanup and hence need not list any
      --  exceptions other than ones we catch (??? the documentation isn't
      --  clear that this is required, but what sense does allowing clauses
      --  with a cleanup mean if it isn't).  Otherwise, we need to list all
      --  exceptions that we or any parent blocks catch.
      --
      --  The dispatcher may be entered either from the landingpad or by
      --  falling through from the dispatchers of inner blocks.  In the
      --  latter case, we need to generate a Phi to track the location of
      --  the exception data.

      Initialize_Predefines;

      --  If somebody asked for landing pad, make one and set it as
      --  needing cleanup if it does.

      if Present (Lpad) then
         Position_Builder_At_End (Lpad);
         LP_Inst := Landing_Pad (LP_Type, Personality_Fn);
         if Have_Cleanup then
            Set_Cleanup (LP_Inst);
         end if;
      end if;

      --  If we have handlers, we have to record them in the table of
      --  clauses so we can dispatch to them.  If we have a landingpad
      --  instruction, add them to it.  Also record what we've seen here
      --  for the following code.

      if Present (EH_List) then
         Handler := First_Non_Pragma (EH_List);
         while Present (Handler) loop
            BB     := Create_Basic_Block;
            Choice := First (Exception_Choices (Handler));
            while Present (Choice) loop
               Exc := Choice_To_Exc (Choice);
               Exceptions_Seen.Append (Exc);
               Clauses.Append ((BB    => BB,
                                Exc   => Convert_To_Access (Exc,
                                                            Standard_A_Char),
                                Param => Choice_Parameter (Handler),
                                Stmts => Statements (Handler)));
               if Present (LP_Inst) then
                  Add_Clause (LP_Inst, Exc);
               end if;

               Next (Choice);
            end loop;

            Next_Non_Pragma (Handler);
         end loop;
      end if;

      --  If we don't have a landingpad, we need to add clauses to it to
      --  cover any uplevel exceptions that we haven't added yet.  One
      --  might think this isn't neccessary, but it is in order to
      --  get the proper values for the exception slot.  The LLVM exception
      --  documentation is very weak on this point.

      if Present (LP_Inst) then
         for J in reverse 1 .. Block - 1 loop
            if Present (Block_Stack.Table (J).EH_List) then
               Handler := First_Non_Pragma (Block_Stack.Table (J).EH_List);
               while Present (Handler) loop
                  Choice := First (Exception_Choices (Handler));
                  while Present (Choice) loop
                     Exc := Choice_To_Exc (Choice);
                     if not (for some K in 1 .. Exceptions_Seen.Last =>
                               Exceptions_Seen.Table (K) = Exc)
                     then
                        Exceptions_Seen.Append (Exc);
                        Add_Clause (LP_Inst, Exc);
                     end if;

                     Next (Choice);
                  end loop;

                  Next_Non_Pragma (Handler);
               end loop;
            end if;
         end loop;
      end if;

      --  Now generate the dispatch code to branch to each exception
      --  handler, if we have any.  If no inner block set up a BB for us to
      --  use, we can emit this inline and the exception data is in the
      --  landing-pad instruction.  Otherwise, we have to branch to the
      --  dispatch code location and add a Phi to collect the values.

      if Present (BI.Dispatch_BB) then
         for J in 1 .. Dispatch_Info.Last loop
            if Dispatch_Info.Table (J).Dispatch_BB = BI.Dispatch_BB then
               N_Dispatch_Froms := N_Dispatch_Froms + 1;
            end if;
         end loop;

         declare
            From_BBs  : Basic_Block_Array (1 .. N_Dispatch_Froms);
            From_Vals : GL_Value_Array (1 .. N_Dispatch_Froms);
            From_Idx  : Nat := 1;

         begin
            if Present (Lpad) then
               From_Vals (1) := LP_Inst;
               From_BBs  (1) := Get_Insert_Block;
               From_Idx      := 2;
            end if;

            for J in 1 .. Dispatch_Info.Last loop
               DDT := Dispatch_Info.Table (J);
               if DDT.Dispatch_BB = BI.Dispatch_BB then
                  From_Vals (From_Idx) := DDT.From_EH_Data;
                  From_BBs  (From_Idx) := DDT.From_BB;
                  From_Idx             := From_Idx + 1;
               end if;
            end loop;

            pragma Assert (From_Idx = From_BBs'Last + 1);
            Move_To_BB (BI.Dispatch_BB);
            EH_Data := Build_Phi (From_Vals, From_BBs);
            BB      := BI.Dispatch_BB;
         end;
      else
         --  This is the simple case: nobody goes to us

         EH_Data := LP_Inst;
         BB      := Lpad;
      end if;

      --  If handlers, generate them and the code to dispatch to them

      Next_BB := Create_Basic_Block;
      if Present (EH_List) then

         --  Extract the selector and the exception pointer

         Exc_Ptr  := Extract_Value (Standard_A_Char,  EH_Data, 0);
         Selector := Extract_Value (Standard_Integer, EH_Data, 1);

         --  Generate code for the handlers, taking into account that we
         --  have duplicate BB's in the table.

         for J in 1 .. Clauses.Last loop
            if No (Get_Last_Instruction (Clauses.Table (J).BB)) then
               Position_Builder_At_End (Clauses.Table (J).BB);
               Push_Block;
               Call (Begin_Handler_Fn, (1 => Exc_Ptr));
               if Present (Clauses.Table (J).Param) then
                  declare
                     Param   : constant Entity_Id := Clauses.Table (J).Param;
                     Typ     : constant Entity_Id := Full_Etype (Param);
                     V       : constant GL_Value  :=
                       Allocate_For_Type (Typ, Typ, No_GL_Value,
                                          Get_Name (Param));
                     Cvt_Ptr : constant GL_Value  :=
                       Convert_To_Access (Exc_Ptr, Standard_A_Char);

                  begin
                     Call (Get_Set_EH_Param_Fn (Typ), (1 => V, 2 => Cvt_Ptr));
                     Set_Value (Param, V);
                  end;
               end if;

               Emit (Clauses.Table (J). Stmts);

               --  If the above code branched out or returned, don't call the
               --  end handler code.  ???  TBD to make a block and make that
               --  the fixup.

               if not Are_In_Dead_Code then
                  Call (End_Handler_Fn, (1 => Exc_Ptr));
                  Build_Br (Next_BB);
               end if;

               Pop_Block;
            end if;
         end loop;

         --  Now generate the dispatch table.  We left off in BB.  Handle
         --  the end case where somebody needs a reraise branch point.

         for J in 1 .. Clauses.Last loop
            Position_Builder_At_End (BB);
            BB := (if J /= Clauses.Last or else No (BI.Reraise_BB)
                   then Create_Basic_Block else BI.Reraise_BB);
            Build_Cond_Br (I_Cmp (Int_EQ, Selector,
                                  Call (EH_Slot_Id_Fn, Int_32_Type,
                                        (1 => Clauses.Table (J).Exc))),
                           Clauses.Table (J).BB, BB);
         end loop;
      end if;

      --  At this point, we've fallen out of the dispatch code (if there was
      --  any) and didn't find a matching exception.  That may be because
      --  we're an "at end" block, not an exception block, in which case
      --  we call that proceduce

      Position_Builder_At_End (BB);
      if Present (At_End_Proc) then
         pragma Assert (No (BI.Reraise_BB));
         if Present (At_End_Parameter) then
            Call (At_End_Proc, (1 => At_End_Parameter));
         else
            Call (At_End_Proc, (1 .. 0 => <>));
         end if;
      end if;

      --  Finally, see if there's an outer block that has an "at end" or
      --  exception handlers.  Ignore any block that's no longer
      --  "protected", meaning that we're generating code for the handlers.
      --  If code in those handlers gets an exception, that should propagate
      --  to the next outer block, not the one with the handlers.  Find the
      --  innermost such.  If so, we branch to that dispatch table
      --  (possibly making a BB for it, ??? but not doing any fixups) and
      --  indicate the data needed for that block's Phi.

      for J in reverse 1 .. Block - 1 loop
         declare
            BI : Block_Info renames Block_Stack.Table (J);

         begin
            if (Present (BI.At_End_Proc) or else Present (BI.EH_List))
              and then not BI.Unprotected
            then
               if No (BI.Dispatch_BB) then
                  BI.Dispatch_BB := Create_Basic_Block ("dispatch");
               end if;

               Dispatch_Info.Append ((BI.Dispatch_BB, Get_Insert_Block,
                                      EH_Data));
               Build_Br (BI.Dispatch_BB);
               Position_Builder_At_End (Next_BB);
               return;
            end if;
         end;
      end loop;

      --  If we reach here, there's no outer block with a handlers or "at end",
      --  so propagate the exception to an enclosing subprogram.

      Build_Resume (EH_Data);
      Position_Builder_At_End (Next_BB);
   end Emit_Handlers;

   ---------------
   -- Pop_Block --
   ---------------

   procedure Pop_Block is
      BI         : Block_Info renames Block_Stack.Table (Block_Stack.Last);
      At_Dead    : constant Boolean       := Are_In_Dead_Code;
      EH_Work    : constant Boolean       :=
        Present (BI.Landing_Pad) or else Present (BI.Dispatch_BB);
      Next_BB    : constant Basic_Block_T :=
        (if EH_Work and then not At_Dead then Create_Basic_Block else No_BB_T);

   begin
      --  If we're not in dead code, we have to fixup the block and the branch
      --  around any landingpad.  But that code is not protected by any
      --  exception handlers in the block and this code isn't protected by
      --  any At_End handler.

      BI.In_Stmts    := False;
      BI.Unprotected := True;
      if not At_Dead then
         Emit_One_Fixup (Block_Stack.Last,
                         Do_At_End => True, Do_Stack => True);
         Maybe_Build_Br (Next_BB);
      end if;

      --  Output the landing pad, handlers, and related exception data and
      --  code if we either have exception handlers or an "at end" proc.

      if EH_Work then
         Emit_Handlers (Block_Stack.Last);
      end if;

      --  ??? Clean this up later.  Too many branches here

      Move_To_BB (Next_BB);
      Block_Stack.Decrement_Last;
   end Pop_Block;

   ------------------
   -- Emit_Reraise --
   ------------------

   procedure Emit_Reraise is
   begin
      --  Find the innermost block that has exception handlers.  If a
      --  basic block has already been created for the reraise point,
      --  use it, otherwise make one.  Jump to that basic block.  It will
      --  be used when that block is popped.  ??? Handle fixups of the
      --  outer blocks.

      for J in reverse 1 .. Block_Stack.Last loop
         declare
            BI : Block_Info renames Block_Stack.Table (J);

         begin
            if Present (BI.EH_List) then
               if No (BI.Reraise_BB) then
                  BI.Reraise_BB := Create_Basic_Block ("reraise");
               end if;

               Build_Br (BI.Reraise_BB);
               return;
            end if;
         end;
      end loop;

      --  We should have found such a block.

      pragma Assert (False);
   end Emit_Reraise;

   --------------------------------------
   -- Process_Push_Pop_xxx_Error_Label --
   --------------------------------------

   procedure Process_Push_Pop_xxx_Error_Label (N : Node_Id) is begin
      case Nkind (N) is
         when N_Push_Constraint_Error_Label =>
            Constraint_Error_Stack.Append (Exception_Label (N));

         when N_Push_Storage_Error_Label =>
            Storage_Error_Stack.Append (Exception_Label (N));

         when N_Push_Program_Error_Label =>
            Program_Error_Stack.Append (Exception_Label (N));

         when N_Pop_Constraint_Error_Label =>
            Constraint_Error_Stack.Decrement_Last;

         when N_Pop_Storage_Error_Label =>
            Storage_Error_Stack.Decrement_Last;

         when N_Pop_Program_Error_Label =>
            Program_Error_Stack.Decrement_Last;

         when others =>
            pragma Assert (False);
      end case;
   end Process_Push_Pop_xxx_Error_Label;

   ------------------------------
   -- Get_Exception_Goto_Entry --
   ------------------------------

   function Get_Exception_Goto_Entry (Kind : Node_Kind) return Entity_Id is
   begin
      if Kind = N_Raise_Constraint_Error
        and then Constraint_Error_Stack.Last /= 0
        and then Present (Constraint_Error_Stack.Table
                            (Constraint_Error_Stack.Last))
      then
         return Constraint_Error_Stack.Table (Constraint_Error_Stack.Last);

      elsif Kind = N_Raise_Program_Error
        and then Program_Error_Stack.Last /= 0
        and then Present (Program_Error_Stack.Table
                            (Program_Error_Stack.Last))
      then
         return Program_Error_Stack.Table (Program_Error_Stack.Last);

      elsif Kind = N_Raise_Storage_Error
        and then Storage_Error_Stack.Last /= 0
        and then Present (Storage_Error_Stack.Table
                            (Storage_Error_Stack.Last))
      then
         return Storage_Error_Stack.Table (Storage_Error_Stack.Last);
      else
         return Empty;
      end if;
   end Get_Exception_Goto_Entry;

   ------------------
   -- Get_Label_BB --
   ------------------

   function Get_Label_BB (E : Entity_Id) return Basic_Block_T is
      BB : Basic_Block_T := Get_Basic_Block (E);

   begin
      if No (BB) then
         BB := Create_Basic_Block (Get_Name (E));
         Set_Basic_Block (E, BB);
      end if;

      return BB;
   end Get_Label_BB;

   ---------------------------
   -- Enter_Block_With_Node --
   ---------------------------

   function Enter_Block_With_Node (Node : Node_Id) return Basic_Block_T
   is
      E         : constant Entity_Id     :=
        (if Present (Node) and then Present (Identifier (Node))
         then Entity (Identifier (Node)) else Empty);
      This_BB   : constant Basic_Block_T := Get_Insert_Block;
      Last_Inst : constant Value_T       := Get_Last_Instruction (This_BB);
      Entry_BB  : constant Basic_Block_T :=
        Get_Entry_Basic_Block (LLVM_Value (Current_Func));
      BB        : constant Basic_Block_T :=
          (if Present (E) and then Has_BB (E) then Get_Basic_Block (E)
           elsif No (Last_Inst) and then This_BB /= Entry_BB
           then This_BB else Create_Basic_Block);
      --  If we have an identifier and it has a basic block already set,
      --  that's the one that we have to use.  If we've just started a
      --  basic block with no instructions in it, that basic block will do,
      --  unless it's the entry BB since we're going to branch to it.
      --  Otherwise, get a new one.

   begin
      --  Now, unless this is our basic block, jump to it and position there

      if BB /= This_BB then
         Move_To_BB (BB);
      end if;

      --  If we have an entity to point to the block, make that linkage.

      if Present (E) then
         Set_Basic_Block (E, BB);
      end if;

      return BB;
   end Enter_Block_With_Node;

   ---------------
   -- Push_Loop --
   ---------------

   procedure Push_Loop (LE : Entity_Id; Exit_Point : Basic_Block_T) is
   begin
      Exit_Point_Table.Append ((LE, Exit_Point, Block_Stack.Last));
   end Push_Loop;

   --------------
   -- Pop_Loop --
   --------------

   procedure Pop_Loop is
   begin
      Exit_Point_Table.Decrement_Last;
   end Pop_Loop;

   --------------------
   -- Get_Exit_Point --
   --------------------

   function Get_Exit_Point (N : Node_Id) return Basic_Block_T is
   begin
      --  If no exit label was specified, use the last one

      if No (N) then
         return Exit_Point_Table.Table (Exit_Point_Table.Last).Exit_BB;
      end if;

      --  Otherwise search for a match

      for I in Exit_Point_Low_Bound .. Exit_Point_Table.Last loop
         if Exit_Point_Table.Table (I).Label_Entity = Entity (N) then
            return Exit_Point_Table.Table (I).Exit_BB;
         end if;
      end loop;

      --  If the loop label isn't registered, then we just met an exit
      --  statement with no corresponding loop: should not happen.

      Error_Msg_N ("unknown loop identifier", N);
      raise Program_Error;
   end Get_Exit_Point;

   ----------------
   -- Emit_Raise --
   ----------------

   procedure Emit_Raise (N : Node_Id) is
      Label   : constant Entity_Id     := Get_Exception_Goto_Entry (Nkind (N));
      Cond    : constant Node_Id       := Condition (N);
      BB_Then : constant Basic_Block_T :=
        (if Present (Label) then Get_Label_BB (Label)
         elsif No (Cond) then No_BB_T else Create_Basic_Block ("raise"));
      BB_Next : constant Basic_Block_T :=
        (if Present (Cond) then Create_Basic_Block else No_BB_T);

   begin
      --  If there's a condition, test it.  If we have the label case,
      --  that's all we have to do since it's one of two branches.

      if Present (Cond) then
         Emit_If_Cond (Cond, BB_Then, BB_Next);
      elsif Present (Label) then
         Build_Br (BB_Then);
      end if;

      --  If this isn't the branch case, we have to raise the exception,
      --  possibly only if the condition above failed.

      if No (Label) then
         if Present (BB_Then) then
            Position_Builder_At_End (BB_Then);
         end if;

         Emit_Raise_Call (N, RT_Exception_Code'Val (UI_To_Int (Reason (N))));
         Maybe_Build_Br (BB_Next);
      end if;

      --  If we've needed to make one, now define the label past the condition

      if Present (BB_Next) then
         Position_Builder_At_End (BB_Next);
      end if;
   end Emit_Raise;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register_Global_Name ("__gnat_all_others_value");
      Register_Global_Name ("__gnat_begin_handler");
      Register_Global_Name ("__gnat_end_handler");
      Register_Global_Name ("__gnat_others_value");
      Register_Global_Name ("__gnat_personality_v0");
      Register_Global_Name ("__gnat_set_exception_parameter");

      if No_Exception_Handlers_Set then
         Register_Global_Name ("__gnat_last_chance_handler");
      else
         for Kind in RT_Exception_Code'Range loop
            Name_Len := 0;
            Add_Str_To_Name_Buffer ("__gnat_rcheck_");
            Get_RT_Exception_Name (Kind);
            Rcheck_Names (Kind) := new String'(Name_Buffer (1 .. Name_Len));
            Register_Global_Name (Rcheck_Names (Kind).all);
         end loop;

         Register_Global_Name ("__gnat_rcheck_CE_Access_Check_ext");
         Register_Global_Name ("__gnat_rcheck_CE_Index_Check_ext");
         Register_Global_Name ("__gnat_rcheck_CE_Invalid_Data_ext");
         Register_Global_Name ("__gnat_rcheck_CE_Range_Check_ext");
      end if;
   end Initialize;

end GNATLLVM.Blocks;
