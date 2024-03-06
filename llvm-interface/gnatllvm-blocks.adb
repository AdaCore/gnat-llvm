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

with Debug;       use Debug;
with Einfo.Utils; use Einfo.Utils;
with Errout;      use Errout;
with Exp_Ch11;    use Exp_Ch11;
with Exp_Unst;    use Exp_Unst;
with Nlists;      use Nlists;
with Opt;         use Opt;
with Restrict;    use Restrict;
with Sem_Util;    use Sem_Util;
with Sinput;      use Sinput;
with Stand;       use Stand;
with Table;       use Table;

with GNATLLVM.Builtins;     use GNATLLVM.Builtins;
with GNATLLVM.Codegen;      use GNATLLVM.Codegen;
with GNATLLVM.Conditionals; use GNATLLVM.Conditionals;
with GNATLLVM.Conversions;  use GNATLLVM.Conversions;
with GNATLLVM.Compile;      use GNATLLVM.Compile;
with GNATLLVM.Environment;  use GNATLLVM.Environment;
with GNATLLVM.GLType;       use GNATLLVM.GLType;
with GNATLLVM.Instructions; use GNATLLVM.Instructions;
with GNATLLVM.Types;        use GNATLLVM.Types;
with GNATLLVM.Utils;        use GNATLLVM.Utils;
with GNATLLVM.Variables;    use GNATLLVM.Variables;
with GNATLLVM.Wrapper;      use GNATLLVM.Wrapper;

package body GNATLLVM.Blocks is

   --  We record a list of variables and sizes that we must mark as
   --  having their lifetime ended at the end of a block.

   type Lifetime_Data;
   type A_Lifetime_Data is access Lifetime_Data;
   type Lifetime_Data is record
     Next   : A_Lifetime_Data;
     Memory : GL_Value;
     Size   : GL_Value;
   end record;

   --  This data structure records the information about each block that
   --  we're in and we construct a table to act as a block stack.

   type Block_Info is record
      Unprotected        : Boolean;
      --  True if we've reached the pop of the block (in the end handler
      --  or its fixup) where calls aren't protected by exceptions or
      --  At_End handlers in this block.

      At_Entry_Start     : Boolean;
      --  True if this block start at the start of the entry block

      Stack_Save         : GL_Value;
      --  Value of the stack pointer at entry to the block, if saved

      Starting_Position  : Position_T;
      --  The position at the start of the block

      At_End_Subp        : Opt_Subprogram_Kind_Id;
      At_End_Proc        : GL_Value;
      --  Procedure to be called at normal or abnormal exit of the block

      At_End_Parameter   : GL_Value;
      --  If Present, a parameter to pass to the At_End_Proc, for example an
      --  activation record.

      At_End_Parameter_2 : GL_Value;
      --  If Present, a second parameter to pass to the At_End_Proc. This is
      --  currently only used for calling the End_Handler procedure.

      At_End_Pass_Excptr : Boolean;
      --  If True, pass the exception pointer as a third parameter to the
      --  At_End_Proc.

      Landing_Pad        : Basic_Block_T;
      --  Basic block containing the landing pad for this block, if any.

      EH_List            : List_Id;
      --  List of exception handlers

      Dispatch_BB        : Basic_Block_T;
      --  BB created by an inner handler to branch to our dispatch code

      Exc_Ptr            : GL_Value;
      --  The exception pointer for the block

      Lifetime_List      : A_Lifetime_Data;
      --  List of memory locations whose lifetimes end at the end of this
      --  block.

      Catch_Unhandled    : Boolean;
      --  True if this block catches unhandled exceptions. This is used for
      --  the main subprogram on SEH targets.

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
   --  of the outer dispatch code when we have nested handlers. To do this,
   --  we need a Phi for the exception information (or a variable, but
   --  finding a scope for that variable is non-trivial). We record that in
   --  this table, which contains the starting BB for dispatch code
   --  (Dispatch_BB from the above) and the BB that jumps to it as well as
   --  the exception data location (for the Phi). We don't try to remove
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
     (Table_Component_Type => E_Label_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 1,
      Table_Name           => "Constraint_Error_Stack");
   --  Stack of labels for constraint error

   package Storage_Error_Stack is new Table.Table
     (Table_Component_Type => E_Label_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 1,
      Table_Name           => "Storage_Error_Stack");
   --  Stack of labels for storage error

   package Program_Error_Stack is new Table.Table
     (Table_Component_Type => E_Label_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 1,
      Table_Name           => "Program_Error_Stack");
   --  Stack of labels for program error

   type Exit_Point is record
      Label_Entity : E_Loop_Id;
      --  The Identifier of the block, used to find which block to exit

      Orig_BB      : Basic_Block_T;
      --  The basic block to jump to in order to exit the block

      Exit_BB      : Basic_Block_T;
      --  A basic block to jump to, which includes any needed fixup code

      Block_Depth  : Block_Stack_Level;
      --  The block depth of Orig_BB

      From_Block   : Block_Stack_Level;
      --  The starting block depth of Exit_BB
   end record;

   type Exit_Point_Level is new Integer;

   Exit_Point_Low_Bound : constant Exit_Point_Level := 1;

   package Exit_Points is new Table.Table
     (Table_Component_Type => Exit_Point,
      Table_Index_Type     => Exit_Point_Level,
      Table_Low_Bound      => Exit_Point_Low_Bound,
      Table_Initial        => 10,
      Table_Increment      => 5,
      Table_Name           => "Exit_Points");
   --  Table of scoped loop exit points. Last inserted exit point correspond
   --  to the innermost loop.

   --  We maintain two tables to handles gotos and labels. We need to
   --  generate fixups when gotos branch outside of blocks, but have both
   --  the case where we've previously seen the label and where we haven't.
   --
   --  For each label, we record, in Label_Info, which is linked to the
   --  GNAT label object, the basic block that corresponds to the label.
   --  When we've defined the label, we store the stack depth at which it's
   --  defined.
   --
   --  If we're branching to a label at a known location, we generate a
   --  second block that contains the fixup code followed by the actual
   --  branch to the label's block. We use Label_Info as a one-element
   --  cache to save the location of the fixup code in case we see another
   --  branch to that same label from the same depth. However, this cache
   --  needs to be invalidated once we leave that block since a new block
   --  at the same depth will have a different fixup.
   --
   --  If where we don't know the label's location when branching to it, we
   --  create an additional label, this time to collect the needed fixups.
   --  We use the Open_Branches table for this. Each time we exit a block,
   --  its fixup is added to each Open_Branch. When the label is finally
   --  defined, we close the Open_Branch block (which may be a different
   --  block from the one originally created if the fixup started a new
   --  block) by doing the branch to the actual label (we knew this label
   --  earlier, so could have ended the block with it, but the complexity
   --  isn't worth it).
   --
   --  Note that this algorithm is quadratic in the number of labels and
   --  can generate duplicate copies of fixups if there are many gotos to
   --  the same label from different depths in the block stack. Luckily,
   --  there are very few labels and gotos in real programs, so this
   --  implementation works well. The same approach would work with more
   --  labels, but we'd need more complex data structures that cache fixups
   --  from more levels and avoid searching the entire list for open
   --  branches.

   type Label_Data is record
      Orig_BB         : Basic_Block_T;
      --  The basic block directly corresponding to the label

      Fixup_BB        : Basic_Block_T;
      --  A basic block to jump to, which includes any needed fixup code

      Block_Depth     : Block_Stack_Level;
      --  The block depth of Orig_BB, or -1 if not known

      From_Block      : Block_Stack_Level;
      --  The depth containing the current fixup label for this label, if any

      Has_Open_Branch : Boolean;
      --  True if we've made an entry in the Open_Branches table for this
   end record;

   package Label_Info is new Table.Table
     (Table_Component_Type => Label_Data,
      Table_Index_Type     => Label_Info_Id,
      Table_Low_Bound      => Label_Info_Low_Bound,
      Table_Initial        => 100,
      Table_Increment      => 10,
      Table_Name           => "Label_Info");
   --  Information about labels we encounter

   type Open_Branch is record
      Orig_BB     : Basic_Block_T;
      --  The actual label that we're trying to branch to

      Made_BB     : Basic_Block_T;
      --  The basic block we created to contain the needed fixups.

      From_Block  : Block_Stack_Level;
      --  The block depth at which this open branch has been fixed up to,
      --  initially the block that the branch is being made from.
   end record;

   package Open_Branches is new Table.Table
     (Table_Component_Type => Open_Branch,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 100,
      Table_Increment      => 10,
      Table_Name           => "Open_Branches");
   --  Information needed to fixup branches to labels we haven't defined yet

   Global_LP_Type : Type_T := No_Type_T;
   --  Type for the "landing pad" used in exception handling

   function Get_LP_Type return Type_T
     with Post => Get_Type_Kind (Get_LP_Type'Result) = Struct_Type_Kind;
   --  Get (and create, if necessary, the type for an EH Landing Pad

   function Find_Exit_Point (N : Opt_N_Identifier_Id) return Exit_Point_Level;
   --  Find the index into the exit point table for node N, if Present

   procedure Call_At_End
     (Block : Block_Stack_Level; For_Exception : Boolean := False);
   --  Call the At_End procedure of Block, if any. If For_Exception is True,
   --  this is the exception case. This only matters when At_End_Pass_Excptr
   --  is set, which is only for the End_Handler.

   procedure Build_Fixups_From_To (From, To : Block_Stack_Level);
   --  We're currently in block From and going to block To. Call any
   --  "at end" procedures in between and restore the stack, if needed.

   procedure Emit_Handlers (Block : Block_Stack_Level);
   --  Generate the parts of a block at level Block used for exception
   --  handling, including at end procs and exception handlers. This
   --  includes the landingpad instruction, the exception handlers, code to
   --  dispatch to the handlers, and code to handle falling through. If a
   --  landingpad was requested, the block will start with a landingpad
   --  instruction starting that basic block  Otherwise, there may not be
   --  a need for a landing pad. Return the actual landing pad
   --  instruction, if any.

   function Get_File_Name_Address (Index : Source_File_Index) return GL_Value
     with Post => Type_Of (Get_File_Name_Address'Result) = Address_T;
   --  Return a GL_Value giving the address of a string corresponding to
   --  the name of the file with the specified file index.

   function Get_Raise_Fn
     (Kind : RT_Exception_Code; Ext : Boolean := False) return GL_Value
     with Post => Present (Get_Raise_Fn'Result);
   --  Get function for raising a builtin exception of Kind. Ext is True if
   --  we want the "extended" (-gnateE) versions of the exception functions.

   function Emit_Raise_Call_With_Extra_Info
     (N    : N_Raise_xxx_Error_Id;
      Kind : RT_Exception_Code;
      Cond : N_Subexpr_Id) return Boolean;
   --  Like Emit_Raise_Call, but generate extra info, if possible, about
   --  the bad value and the range that it's supposed to be within. If if
   --  returns True, it was able to get the info and emit the call.
   --  Otherwise, it couldn't and the caller should make a normal call.

   function Get_Set_EH_Param_Fn (Exc_GT : GL_Type) return GL_Value
     with Pre  => Present (Exc_GT),
          Post => Present (Get_Set_EH_Param_Fn'Result);
   --  Get (and create if needed) the function that sets the exception
   --  parameter. This can only be called once we have an exception parameter
   --  since we can't easily find the exception type before that.

   function Get_EH_Slot (Exc : GL_Value) return GL_Value
     with Pre  => Is_Access_Type (Exc),
          Post => Present (Get_EH_Slot'Result);
   --  Emit code that obtains the type info index for the exception

   procedure Initialize_Predefines;
   --  Initialize the predefined functions and variables below

   Predefines_Set   : Boolean        := False;
   --  True when all of the below have been initialized

   Personality_Fn   : GL_Value;
   --  The definition of the personality function

   EH_Slot_Id_Fn    : GL_Value;
   --  The LLVM builtin that converts an exception into a slot number

   Begin_Handler_Fn : GL_Value;
   End_Handler_Fn   : GL_Value;
   --  Begin and end functions for handlers

   Unhandler_Fn     : GL_Value;
   --  Handler for unhandled exceptions

   Reraise_Fn       : GL_Value;
   --  Function to reraise an exception

   Others_Value           : GL_Value;
   All_Others_Value       : GL_Value;
   Unhandled_Others_Value : GL_Value;
   --  "Exception" address for "others" and special "all others" and
   --  "unhandled others".

   Set_Exception_Param_Fn : GL_Value := No_GL_Value;
   --  Declaration for __gnat_set_exception_parameter. This can't be
   --  initialized with the ones above since we need its type.

   LCH_Fn            : GL_Value      := No_GL_Value;
   --  Last-chance handler. We only initialize this if needed

   type File_GL_Value_Array is
     array (Source_File_Index range <>, Boolean range <>) of GL_Value;
   type File_GL_Value_Array_Access is access all File_GL_Value_Array;
   File_Name_Strings : File_GL_Value_Array_Access := null;
   --  Array of GL_Values corresponding to 'Address of the string literal
   --  representing the name of the file, with one entry when we need a
   --  global for the name and one where we don't.

   type Rcheck_Name_Array is array (RT_Exception_Code) of String_Access;

   Rcheck_Names      : Rcheck_Name_Array;
   --  Array of pointers to strings giving the names of the functions for
   --  raising builtin exceptions of various kinds.

   Rcheck_FNs        : array (RT_Exception_Code'Range) of GL_Value :=
     (others => No_GL_Value);
   Rcheck_Ext_FNs    : array (RT_Exception_Code'Range) of GL_Value :=
     (others => No_GL_Value);
   --  Array of functions to call for raising builtin exceptions of
   --  various kinds.

   ----------------
   -- Push_Block --
   ----------------

   procedure Push_Block
     (At_End_Proc     : Opt_N_Subexpr_Id := Empty;
      EH_List         : List_Id          := No_List;
      Catch_Unhandled : Boolean          := False)
   is
      End_Subp      : Opt_Subprogram_Kind_Id := Empty;
      End_Proc      : GL_Value               := No_GL_Value;
      End_Parameter : GL_Value               := No_GL_Value;

   begin
      if Present (At_End_Proc) then

         --  Save both the end proc and the value of the static link.
         --  Since we'll be generating the call directly, we have to
         --  convert the static link to the proper pointer type for the
         --  activation record. There may not be a static link, however,
         --  if there are no uplevel references.

         End_Subp := Entity (At_End_Proc);
         End_Proc := Emit_LValue (At_End_Proc);

         if Has_Activation_Record (End_Subp)
           and then Present (Subps.Table (Subp_Index (End_Subp)).ARECnF)
         then
            --  We have to defer doing this if the parent of Subp is
            --  us and we haven't elaborated our ARECnP variable yet.

            if Enclosing_Subprogram (End_Subp) /= Current_Subp
              or else Present (Get_Value
                                 (Subps.Table
                                    (Subp_Index (Current_Subp)).ARECnP))
            then
               End_Parameter :=
                 Pointer_Cast (Get_Static_Link (End_Subp),
                               Full_GL_Type (Extra_Formals (End_Subp)));
            end if;
         end if;
      end if;

      Block_Stack.Append ((Stack_Save         => No_GL_Value,
                           Starting_Position  => Get_Current_Position,
                           At_End_Subp        => End_Subp,
                           At_End_Proc        => End_Proc,
                           At_End_Parameter   => End_Parameter,
                           At_End_Parameter_2 => No_GL_Value,
                           At_End_Pass_Excptr => False,
                           Landing_Pad        => No_BB_T,
                           Dispatch_BB        => No_BB_T,
                           Exc_Ptr            => No_GL_Value,
                           EH_List            => EH_List,
                           Unprotected        => False,
                           At_Entry_Start     =>
                             Get_Current_Position = Entry_Block_Allocas,
                           Lifetime_List      => null,
                           Catch_Unhandled    => Catch_Unhandled));

   end Push_Block;

   -------------------------
   -- Maybe_Update_At_End --
   -------------------------

   procedure Maybe_Update_At_End (E : E_Constant_Id) is
      BI : Block_Info renames Block_Stack.Table (Block_Stack.Last);

   begin
      --  See if this is the ARECnP variable for this subprogram. We have
      --  nothing to do if not. Likewise if there's no At_End_Proc
      --  for this block.

      if Present (Current_Subp)
        and then Has_Nested_Subprogram (Current_Subp)
        and then E = Subps.Table (Subp_Index (Current_Subp)).ARECnP
        and then Present (BI.At_End_Subp) and then No (BI.At_End_Parameter)
        and then Has_Activation_Record (BI.At_End_Subp)
      then
         BI.At_End_Parameter :=
           Pointer_Cast (Get_Static_Link (BI.At_End_Subp),
                         Full_GL_Type (Extra_Formals (BI.At_End_Subp)));
      end if;
   end Maybe_Update_At_End;

   ------------------------
   -- Save_Stack_Pointer --
   ------------------------

   procedure Save_Stack_Pointer is
      BI : Block_Info renames Block_Stack.Table (Block_Stack.Last);
      Our_BB : constant Basic_Block_T := Get_Insert_Block;

   begin
      --  If we're not in the top-level block, we haven't already saved the
      --  stack, and we're not generating C, produce a stack save at the
      --  start of the block.

      if No (BI.Stack_Save) and then Block_Stack.Last > 1 and then not Emit_C
      then
         Set_Current_Position (BI.Starting_Position);
         BI.Stack_Save := Call (Get_Stack_Save_Fn, (1 .. 0 => <>));
         Position_Builder_At_End (Our_BB);
      end if;
   end Save_Stack_Pointer;

   ------------------------
   -- Add_Lifetime_Entry --
   ------------------------

   procedure Add_Lifetime_Entry (Ptr, Size : GL_Value) is
      BI     : Block_Info renames Block_Stack.Table (Block_Stack.Last);
      Our_BB : constant Basic_Block_T := Get_Insert_Block;

   begin
      --  If we don't have a 64-bit type, we can't make lifetime calls, so
      --  do nothing in that case. Likewise if we're generating C.

      if No (Int_64_GL_Type) or else Emit_C then
         return;
      end if;

      --  We need to put the call to start the lifetime at the start of
      --  this block. However, if the start of this block is the entry
      --  BB, we need to put this after the allocas.

      Set_Current_Position
        ((if   BI.At_Entry_Start then Entry_Block_Allocas
          else BI.Starting_Position));
      Create_Lifetime_Start (Ptr, Size);
      Position_Builder_At_End (Our_BB);
      BI.Lifetime_List := new Lifetime_Data'(BI.Lifetime_List, Ptr, Size);

   end Add_Lifetime_Entry;

   ---------------------
   -- Get_Landing_Pad --
   ---------------------

   function Get_Landing_Pad return Basic_Block_T is
      BI : Block_Info;

   begin
      --  If we're in the Statements part of a block that has exceptions,
      --  see if we've made a block for the landing-pad. If not, make one.

      for J in reverse 1 .. Block_Stack.Last loop
         BI := Block_Stack.Table (J);

         if (Present (BI.EH_List)
             or else Present (BI.At_End_Proc)
             or else BI.Catch_Unhandled)
           and then not BI.Unprotected
         then
            if No (BI.Landing_Pad) then
               Block_Stack.Table (J).Landing_Pad :=
                 Create_Basic_Block ("LPAD");
            end if;

            return Block_Stack.Table (J).Landing_Pad;
         end if;
      end loop;

      return No_BB_T;
   end Get_Landing_Pad;

   -----------------
   -- Call_At_End --
   -----------------

   procedure Call_At_End
     (Block : Block_Stack_Level; For_Exception : Boolean := False)
   is
      procedure Push_If_Present (V : GL_Value) with Inline;
      --  Push V onto the Params array below if it's Present

      BI          : Block_Info renames Block_Stack.Table (Block);
      Our_BI      : Block_Info renames Block_Stack.Table (Block_Stack.Last);
      Our_Exc_Ptr : constant GL_Value   :=
        (if    not BI.At_End_Pass_Excptr then No_GL_Value
         elsif For_Exception then Our_BI.Exc_Ptr
         else  Const_Null (A_Char_GL_Type));
      Unprotected : constant Boolean   := Our_BI.Unprotected;
      Params      : GL_Value_Array (1 .. 3);
      Last_Param  : Nat                := 0;

      ---------------------
      -- Push_If_Present --
      ---------------------

      procedure Push_If_Present (V : GL_Value) is
      begin
         if Present (V) then
            Last_Param := Last_Param + 1;
            Params (Last_Param) := V;
         end if;
      end Push_If_Present;

   begin
      if Present (BI.At_End_Proc) then
         Push_If_Present (BI.At_End_Parameter);
         Push_If_Present (BI.At_End_Parameter_2);
         Push_If_Present (Our_Exc_Ptr);
         Our_BI.Unprotected := True;
         Call (BI.At_End_Proc, Params (1 .. Last_Param));
         Our_BI.Unprotected := Unprotected;
      end if;
   end Call_At_End;

   -------------------------
   -- Build_Fixups_From_To --
   -------------------------

   procedure Build_Fixups_From_To (From, To : Block_Stack_Level)
   is
      Stack_Save : GL_Value := No_GL_Value;

   begin
      --  We're going from block From to block To. Run fixups for any blocks
      --  we pass and then restore the outermost stack pointer.

      for J in reverse To + 1 .. From loop

         --  Process any ends of variable lifetimes

         declare
            Lifetimes : A_Lifetime_Data := Block_Stack.Table (J).Lifetime_List;

         begin
            while Lifetimes /= null loop
               Create_Lifetime_End (Lifetimes.Memory, Lifetimes.Size);
               Lifetimes := Lifetimes.Next;
            end loop;
         end;

         --  Now call the at-end handler, if any, and record the stack save
         --  location for this block.

         Call_At_End (J);

         if Present (Block_Stack.Table (J).Stack_Save) then
            Stack_Save := Block_Stack.Table (J).Stack_Save;
         end if;
      end loop;

      --  If we crossed a saved stack pointer and we aren't returning out of
      --  the subprogram, restore the stack pointer.

      if To /= 0 and then Present (Stack_Save) then
         Call (Get_Stack_Restore_Fn, (1 => Stack_Save));
      end if;

   end Build_Fixups_From_To;

   ----------------------------
   -- Emit_Fixups_For_Return --
   ----------------------------

   procedure Emit_Fixups_For_Return is
   begin
      --  We're going from our current position entirely out of the block
      --  stack.

      Build_Fixups_From_To (Block_Stack.Last, 0);
   end Emit_Fixups_For_Return;

   ---------------------------
   -- Initialize_Predefines --
   ---------------------------

   procedure Initialize_Predefines is
   begin
      if Predefines_Set then
         return;
      end if;

      Personality_Fn :=
        Add_Global_Function
          (Get_Personality_Function_Name (Normalized_Target_Triple.all),
           Fn_Ty ((1 .. 0 => <>), Int_32_T, True), Void_GL_Type);

      Begin_Handler_Fn :=
        Add_Global_Function ("__gnat_begin_handler_v1",
                             Fn_Ty ((1 => Void_Ptr_T), Void_Ptr_T),
                             A_Char_GL_Type);

      End_Handler_Fn   :=
        Add_Global_Function ("__gnat_end_handler_v1",
                             Fn_Ty ((1 => Void_Ptr_T, 2 => Void_Ptr_T,
                                     3 => Void_Ptr_T),
                                    Void_Type),
                             Void_GL_Type);

      Unhandler_Fn     :=
        Add_Global_Function ("__gnat_unhandled_except_handler",
                             Fn_Ty ((1 => Void_Ptr_T), Void_Type),
                             Void_GL_Type);

      Reraise_Fn       :=
        Add_Global_Function ("__gnat_reraise_zcx",
                             Fn_Ty ((1 => Void_Ptr_T), Void_Type),
                             Void_GL_Type,
                             Can_Return => False, Can_Throw => True);

      EH_Slot_Id_Fn    :=
        Build_Intrinsic ("llvm.eh.typeid.for", Int_32_GL_Type);
      Set_Does_Not_Throw (EH_Slot_Id_Fn);

      Others_Value           :=
        Add_Global (SSI_GL_Type, "__gnat_others_value");
      All_Others_Value       :=
        Add_Global (SSI_GL_Type, "__gnat_all_others_value");
      Unhandled_Others_Value :=
        Add_Global (SSI_GL_Type, "__gnat_unhandled_others_value");

      Predefines_Set := True;

   end Initialize_Predefines;

   -----------------
   -- Get_LP_Type --
   -----------------

   function Get_LP_Type return Type_T is
   begin
      if No (Global_LP_Type) then
         Global_LP_Type := Build_Struct_Type
           ((1 => Void_Ptr_T, 2 => Int_32_T),
            Name        => Name_Find ("LANDING_PAD"),
            Field_Names => (1 => Name_Find ("EH_PTR"),
                            2 => Name_Find ("EH_SELECT")));
      end if;

      return Global_LP_Type;
   end Get_LP_Type;

   --------------------------
   -- Get_Set_EH_Param_Fn --
   --------------------------

   function Get_Set_EH_Param_Fn (Exc_GT : GL_Type) return GL_Value is
   begin
      --  If we haven't already made the function to set the
      --  choice parameter, make it now that we have the type.

      if No (Set_Exception_Param_Fn) then
         Set_Exception_Param_Fn := Add_Global_Function
           ("__gnat_set_exception_parameter",
            Fn_Ty ((1 => Create_Access_Type_To (Exc_GT), 2 => Void_Ptr_T),
                   Void_Type),
            Void_GL_Type);
      end if;

      return Set_Exception_Param_Fn;
   end Get_Set_EH_Param_Fn;

   ------------------
   -- Get_Raise_Fn --
   ------------------

   function Get_Raise_Fn
     (Kind : RT_Exception_Code; Ext : Boolean := False) return GL_Value
   is
      Int_T    : constant Type_T := Type_Of (Integer_GL_Type);
      Fun_Type : Type_T          :=
        Fn_Ty ((1 => Address_T, 2 => Int_T), Void_Type);

   begin
      --  If we're using a last-chance handler, that's the function we need

      if No_Exception_Handlers_Set then
         if No (LCH_Fn) then
            LCH_Fn := Add_Global_Function
              ("__gnat_last_chance_handler", Fun_Type,
               Void_GL_Type, Can_Throw => True, Can_Return => False);
         end if;

         return LCH_Fn;

      --  Otherwise see if this is an extended exception case

      elsif Ext then
         if No (Rcheck_Ext_FNs (Kind)) then
            case Kind is
               when CE_Access_Check_Failed =>
                  Fun_Type :=
                    Fn_Ty ((1 => Address_T, 2 => Int_T, 3 => Int_T),
                           Void_Type);

               when CE_Index_Check_Failed | CE_Range_Check_Failed
                  | CE_Invalid_Data =>
                  Fun_Type :=
                    Fn_Ty ((1 => Address_T, 2 => Int_T, 3 => Int_T,
                            4 => Int_T, 5 => Int_T, 6 => Int_T),
                           Void_Type);

               when others =>
                  pragma Assert (Standard.False);
            end case;

            Rcheck_Ext_FNs (Kind) := Add_Global_Function
              (Rcheck_Names (Kind).all & "_ext", Fun_Type, Void_GL_Type,
               Can_Throw => True, Can_Return => False);
         end if;

         return Rcheck_Ext_FNs (Kind);

      --  If none of the cases above apply, see if we've already made a
      --  function for this exception code and create one if not.

      elsif No (Rcheck_FNs (Kind)) then
         Rcheck_FNs (Kind) := Add_Global_Function
           (Rcheck_Names (Kind).all, Fun_Type,
            Void_GL_Type, Can_Throw => True, Can_Return => False);
      end if;

      return Rcheck_FNs (Kind);
   end Get_Raise_Fn;

   ------------------------
   -- Emit_Raise_Call_If --
   ------------------------

   procedure Emit_Raise_Call_If
     (V    : GL_Value;
      N    : Node_Id;
      Kind : RT_Exception_Code := CE_Overflow_Check_Failed)
   is
      BB_Then  : constant Basic_Block_T := Create_Basic_Block ("RAISE");
      BB_Next  : constant Basic_Block_T := Create_Basic_Block;

   begin
      Build_Cond_Br (V, BB_Then, BB_Next);
      Position_Builder_At_End (BB_Then);
      Emit_Raise_Call (N, Kind);
      Move_To_BB (BB_Next);
   end Emit_Raise_Call_If;

   ---------------------------
   -- Get_File_Name_Address --
   ---------------------------

   function Get_File_Name_Address
     (Index : Source_File_Index) return GL_Value
   is
      Is_Global : constant Boolean :=
        Emit_C and then Present (Current_Func)
        and then (Has_Inline_Attribute (Current_Func)
                  or else Has_Inline_Always_Attribute (Current_Func));

   begin
      --  If we haven't yet allocated the cache for our filename strings
      --  do it now.

      if File_Name_Strings = null then
         File_Name_Strings :=
           new File_GL_Value_Array'(1 .. Last_Source_File =>
                                      (False .. True => No_GL_Value));
      end if;

      --  If we haven't already computed a string literal for this filename,
      --  do it now. Take into account pragma Suppress_Exception_Location.

      if No (File_Name_Strings (Index, Is_Global)) then
         declare
            File      : constant String  :=
              (if   Debug_Flag_NN or else Exception_Locations_Suppressed
               then ""
               else Get_Name_String (Debug_Source_Name (Index)));
            Elements  : GL_Value_Array (1 .. File'Length + 1);
            V         : GL_Value;
            Str       : GL_Value;

         begin
            --  First build a string literal for FILE

            for J in File'Range loop
               Elements (Nat (J)) :=
                 Const_Int (SSI_GL_Type, ULL (Character'Pos (File (J))));
            end loop;

            --  Append NUL character and create the string for the filename.
            --  If we're generating C and this function is inline, we
            --  need to make this a global.

            Elements (Elements'Last) := Const_Null (SSI_GL_Type);
            Str := Const_Array (Elements, Any_Array_GL_Type);
            V   := G_Ref (Add_Global (Module, Type_Of (Str),
                                      Globalize_Name ("FNAME", Is_Global)),
                          Any_Array_GL_Type);
            Set_Initializer     (V, Str);
            Set_Global_Constant (V);
            File_Name_Strings (Index, Is_Global) :=
              Ptr_To_Int (V, Size_GL_Type);

            if not Is_Global then
               Set_Linkage (V, Private_Linkage);
            end if;
         end;
      end if;

      return File_Name_Strings (Index, Is_Global);
   end Get_File_Name_Address;

   ---------------------
   -- Emit_Raise_Call --
   ---------------------

   procedure Emit_Raise_Call
     (N : Node_Id; Kind : RT_Exception_Code; Column : Boolean := False)
   is
      S    : constant Source_Ptr := Sloc (N);
      File : constant GL_Value   :=
        Get_File_Name_Address (Get_Source_File_Index (S));
      Line : constant GL_Value   :=
        Const_Int (Integer_GL_Type,
                   ULL (if   Debug_Flag_NN
                             or else Exception_Locations_Suppressed
                        then 0 else Get_Logical_Line_Number (S)));
      Col  : constant GL_Value   :=
        Const_Int (Integer_GL_Type, ULL (Get_Column_Number (S)));

   begin
      --  Build a call to __gnat_xx (FILE, LINE)

      if Column then
         Call (Get_Raise_Fn (Kind, Ext => True),
               (1 => File, 2 => Line, 3 => Col));
      else
         Call (Get_Raise_Fn (Kind), (1 => File, 2 => Line));
      end if;
   end Emit_Raise_Call;

   ---------------------
   -- Emit_Raise_Call --
   ---------------------

   function Emit_Raise_Call_With_Extra_Info
     (N    : N_Raise_xxx_Error_Id;
      Kind : RT_Exception_Code;
      Cond : N_Subexpr_Id) return Boolean
   is
      S     : constant Source_Ptr  := Sloc (N);
      File   : constant GL_Value   :=
        Get_File_Name_Address (Get_Source_File_Index (S));
      Line   : constant GL_Value   :=
        Const_Int (Integer_GL_Type,
                   ULL (if   Debug_Flag_NN
                             or else Exception_Locations_Suppressed
                        then 0 else Get_Logical_Line_Number (S)));
      Col    : constant GL_Value   :=
        Const_Int (Integer_GL_Type, ULL (Get_Column_Number (S)));
      LB, HB : N_Subexpr_Id;
      Rng    : N_Subexpr_Id;
      Index  : GL_Value;
      LB_V   : GL_Value;
      HB_V   : GL_Value;

   begin
      --  If this isn't a NOT operation, we can't handle it. If we're only
      --  processing decls, don't try.

      if Nkind (Cond) /= N_Op_Not or else Decls_Only then
         return False;
      end if;

      --  Otherwise, get the range

      case Nkind (Right_Opnd (Cond)) is
         when N_In =>
            Rng := Right_Opnd (Right_Opnd (Cond));

            if Is_Entity_Name (Rng) then
               Rng := Simplify_Range (Scalar_Range (Full_Etype (Rng)));
            end if;

            LB := Low_Bound  (Rng);
            HB := High_Bound (Rng);

         when N_Op_Ge =>
            LB := Right_Opnd (Right_Opnd (Cond));
            HB := Type_High_Bound (Full_Etype (Left_Opnd (Right_Opnd (Cond))));

         when N_Op_Le =>
            LB := Type_Low_Bound (Full_Etype (Left_Opnd (Right_Opnd (Cond))));
            HB := Right_Opnd (Right_Opnd (Cond));

         when others =>
            return False;
      end case;

      --  If we don't know the size of the type or if it's wider than an
      --  Integer, we can't give the extended information.

      if not Known_RM_Size (Full_Etype (LB))
        or else RM_Size (Full_Etype (LB)) > RM_Size (Standard_Integer)
      then
         return False;
      end if;

      --  Now convert everything to Integer and call the raise function

      Index := Emit_Convert_Value (Left_Opnd (Right_Opnd (Cond)),
                                   Integer_GL_Type);
      LB_V  := Emit_Convert_Value (LB, Integer_GL_Type);
      HB_V  := Emit_Convert_Value (HB, Integer_GL_Type);
      Call (Get_Raise_Fn (Kind, Ext => True),
            (1 => File,  2 => Line, 3 => Col,
             4 => Index, 5 => LB_V, 6 => HB_V));
      return True;

   end Emit_Raise_Call_With_Extra_Info;

   -------------------
   -- Emit_Handlers --
   -------------------

   procedure Emit_Handlers (Block : Block_Stack_Level) is
      BI                : Block_Info renames Block_Stack.Table (Block);
      LP_Type           : constant Type_T        := Get_LP_Type;
      Have_Cleanup      : constant Boolean       :=
        (for some J in 1 .. Block =>
           Present (Block_Stack.Table (J).At_End_Proc)
           and then (not Block_Stack.Table (J).Unprotected or else J = Block));
      LP_Inst           : GL_Value               := No_GL_Value;
      N_Dispatch_Froms  : Nat                    :=
        (if Present (BI.Landing_Pad) then 1 else 0);
      Next_BB           : Basic_Block_T;
      DDT               : D_D_Info;
      BB                : Basic_Block_T;
      Handler           : Opt_N_Exception_Handler_Id;
      Choice            : Opt_N_Is_Exception_Choice_Id;
      EH_Data           : GL_Value;
      Selector, Exc_Ptr : GL_Value;
      Exc               : GL_Value;

      function Choice_To_Exc
        (Choice : N_Is_Exception_Choice_Id) return GL_Value
        with Post => Present (Choice_To_Exc'Result);
      --  Given a Choice from an exception alternative, return a GL_Value
      --  corresponding to that choice, taking into account the special
      --  values use for "others".

      -------------------
      -- Choice_To_Exc --
      -------------------

      function Choice_To_Exc
        (Choice : N_Is_Exception_Choice_Id) return GL_Value
      is
      begin
         if Nkind (Choice) = N_Others_Choice then
            return (if   All_Others (Choice) then All_Others_Value
                    else Others_Value);
         else
            return Emit_LValue (Choice);
         end if;
      end Choice_To_Exc;

      type One_Clause is record
         BB    : Basic_Block_T;
         --  Basic block containing the actions for this exception

         Exc   : GL_Value;
         --  The address of the exception caught by this handler

         Param : Opt_E_Variable_Id;
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
      --  block above our own as well as this block. If this or any upper
      --  block has an At_End proc, we must catch all exceptions, so the
      --  landingpad must contain a cleanup and hence need not list any
      --  exceptions other than ones we catch. Otherwise, we need to list
      --  all exceptions that we or any parent blocks catch.
      --
      --  The dispatcher may be entered either from the landingpad or by
      --  falling through from the dispatchers of inner blocks. In the
      --  latter case, we need to generate a Phi to track the location of
      --  the exception data.

      Initialize_Predefines;

      --  If somebody asked for landing pad, make one and set it as
      --  needing cleanup if it does.

      if Present (BI.Landing_Pad) then
         Position_Builder_At_End (BI.Landing_Pad);
         LP_Inst := Landing_Pad (LP_Type, Personality_Fn);

         if Have_Cleanup then
            Set_Cleanup (LP_Inst);
         end if;
      end if;

      --  If we have handlers, we have to record them in the table of
      --  clauses so we can dispatch to them. If we have a landingpad
      --  instruction, add them to it. Also record what we've seen here for
      --  the following code.

      if Present (BI.EH_List) then
         Handler := First_Non_Pragma (BI.EH_List);
         while Present (Handler) loop
            BB     := Create_Basic_Block;
            Choice := First (Exception_Choices (Handler));
            while Present (Choice) loop
               Exc := Choice_To_Exc (Choice);
               Exceptions_Seen.Append (Exc);
               Clauses.Append ((BB    => BB,
                                Exc   => Convert_To_Access (Exc,
                                                            A_Char_GL_Type),
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
      --  cover any uplevel exceptions that we haven't added yet. One might
      --  think this isn't neccessary, but it is in order to get the proper
      --  values for the exception slot. The LLVM exception documentation
      --  is very weak on this point.

      if Present (LP_Inst) then
         for J in reverse 1 .. Block - 1 loop
            if Present (Block_Stack.Table (J).EH_List)
              and then not Block_Stack.Table (J).Unprotected
            then
               Handler   := First_Non_Pragma (Block_Stack.Table (J).EH_List);
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

         if BI.Catch_Unhandled then
            Add_Clause (LP_Inst, Unhandled_Others_Value);
         end if;
      end if;

      --  Now generate the dispatch code to branch to each exception
      --  handler, if we have any. If no inner block set up a BB for us to
      --  use, we can emit this inline and the exception data is in the
      --  landing-pad instruction. Otherwise, we have to branch to the
      --  dispatch code location and add a Phi to collect the values.

      if Present (BI.Dispatch_BB) then
         for J in 1 .. Dispatch_Info.Last loop
            if Dispatch_Info.Table (J).Dispatch_BB = BI.Dispatch_BB then
               N_Dispatch_Froms := N_Dispatch_Froms + 1;
            end if;
         end loop;

         declare
            From_BBs  : Basic_Block_Array (1 .. N_Dispatch_Froms);
            From_Vals : GL_Value_Array    (1 .. N_Dispatch_Froms);
            From_Idx  : Nat := 1;

         begin
            if Present (BI.Landing_Pad) then
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
         BB      := BI.Landing_Pad;
      end if;

      --  Generate handlers and the code to dispatch to them

      Next_BB := Create_Basic_Block;

      if Present (BI.EH_List)
        or else BI.Catch_Unhandled
        or else BI.At_End_Pass_Excptr
      then

         --  Extract the selector and the exception pointer

         Exc_Ptr    := Extract_Value (A_Char_GL_Type,  EH_Data, 0);
         Selector   := Extract_Value (Integer_GL_Type, EH_Data, 1);
         BI.Exc_Ptr := Exc_Ptr;

         --  Generate code for the handlers, taking into account that we
         --  have duplicate BB's in the table. We make a block for the
         --  handler to deal with allocated variables and to establish the
         --  end handler procedure. But that block does not produce an EH
         --  context itself.

         for J in 1 .. Clauses.Last loop
            if No (Get_Last_Instruction (Clauses.Table (J).BB)) then
               Position_Builder_At_End (Clauses.Table (J).BB);
               Push_Block;

               declare
                  BI_Inner : Block_Info
                    renames Block_Stack.Table (Block_Stack.Last);

               begin
                  BI_Inner.At_End_Proc        := End_Handler_Fn;
                  BI_Inner.At_End_Parameter   := Exc_Ptr;
                  BI_Inner.At_End_Parameter_2 :=
                    Call (Begin_Handler_Fn, (1 => Exc_Ptr));
                  BI_Inner.At_End_Pass_Excptr := True;
               end;

               if Present (Clauses.Table (J).Param) then
                  declare
                     Param   : constant E_Variable_Id :=
                       Clauses.Table (J).Param;
                     GT      : constant GL_Type       := Full_GL_Type (Param);
                     V       : constant GL_Value      :=
                       Allocate_For_Type (GT, N => Param, E => Param);
                     Cvt_Ptr : constant GL_Value      :=
                       Convert_To_Access (Exc_Ptr, A_Char_GL_Type);

                  begin
                     Call (Get_Set_EH_Param_Fn (GT), (1 => V, 2 => Cvt_Ptr));
                     Set_Value (Param, V);
                  end;
               end if;

               Emit (Clauses.Table (J). Stmts);
               Pop_Block;
               Maybe_Build_Br (Next_BB);
            end if;
         end loop;

         --  Now generate the dispatch table. We left off in BB.

         for J in 1 .. Clauses.Last loop
            Position_Builder_At_End (BB);
            BB := Create_Basic_Block;
            Build_Cond_Br (I_Cmp (Int_EQ, Selector,
                                  Get_EH_Slot (Clauses.Table (J).Exc)),
                           Clauses.Table (J).BB, BB);
         end loop;

         --  If we're supposed to catch unhandled exceptions (i.e., the
         --  special exception type __gnat_unhandled_others_value), emit a
         --  handler that calls __gnat_unhandled_except_handler.

         if BI.Catch_Unhandled then
            declare
               Handler_BB : constant Basic_Block_T :=
                 Create_Basic_Block ("UNHANDLED_OTHERS");
            begin
               Position_Builder_At_End (Handler_BB);
               Call (Unhandler_Fn, (1 => Exc_Ptr));
               Build_Unreachable;

               Position_Builder_At_End (BB);
               BB := Create_Basic_Block;
               Build_Cond_Br
                 (I_Cmp
                    (Int_EQ, Selector,
                     Call
                       (EH_Slot_Id_Fn,
                       (1 => Unhandled_Others_Value))),
                  Handler_BB, BB);
            end;
         end if;
      end if;

      --  This is the code point where we've fallen through the dispatch
      --  code, so no exception handler in this block is being entered.

      Position_Builder_At_End (BB);
      Call_At_End (Block, For_Exception => True);

      --  Finally, see if there's an outer block that has an "at end" or
      --  exception handlers. Ignore any block that's no longer
      --  "protected", meaning that we're generating code for the handlers.
      --  If code in those handlers gets an exception, that should
      --  propagate to the next outer block, not the one with the handlers.
      --  Find the innermost such. If so, we branch to that dispatch table
      --  (possibly making a BB for it) and indicate the data needed for
      --  that block's Phi. Note that fixups have already been done for
      --  this block, so the only ones we have to do here are if we go more
      --  than one level up.

      for J in reverse 1 .. Block - 1 loop
         declare
            BI : Block_Info renames Block_Stack.Table (J);

         begin
            if (Present (BI.At_End_Proc) or else Present (BI.EH_List))
              and then not BI.Unprotected
            then
               if No (BI.Dispatch_BB) then
                  BI.Dispatch_BB := Create_Basic_Block ("DISPATCH");
               end if;

               Build_Fixups_From_To (Block - 1, J);
               Dispatch_Info.Append ((Dispatch_BB  => BI.Dispatch_BB,
                                      From_BB      => Get_Insert_Block,
                                      From_EH_Data => EH_Data));
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
      Depth     : constant Block_Stack_Level := Block_Stack.Last;
      BI        : Block_Info renames Block_Stack.Table (Depth);
      At_Dead   : constant Boolean           := Are_In_Dead_Code;
      EH_Work   : constant Boolean           :=
        Present (BI.Landing_Pad) or else Present (BI.Dispatch_BB);
      Next_BB   : constant Basic_Block_T     :=
        (if EH_Work and then not At_Dead then Create_Basic_Block else No_BB_T);
      Lifetimes : A_Lifetime_Data            := BI.Lifetime_List;
      Next      : A_Lifetime_Data;

      procedure Free is new Ada.Unchecked_Deallocation (Lifetime_Data,
                                                        A_Lifetime_Data);
   begin
      --  If we're not in dead code, we have to fixup the block and the branch
      --  around any landingpad. But that code is not protected by any
      --  exception handlers in the block and this code isn't protected by
      --  any At_End handler.

      BI.Unprotected := True;

      if not At_Dead then
         Build_Fixups_From_To (Depth, Depth - 1);
         Maybe_Build_Br (Next_BB);
      end if;

      --  Output the landing pad, handlers, and related exception data and
      --  code if we either have exception handlers or an "at end" proc.

      if EH_Work then
         Emit_Handlers (Depth);
      end if;

      --  Look through the Open_Branches table to see if we have any open
      --  branches at our level. For each, add our fixup. Note that the
      --  fixup may have switched to a different block, so update it.

      for J in 1 .. Open_Branches.Last loop
         declare
            Current_BB : constant Basic_Block_T := Get_Insert_Block;
            OB         : Open_Branch renames Open_Branches.Table (J);

         begin
            if OB.From_Block = Depth then
               Position_Builder_At_End (OB.Made_BB);
               Build_Fixups_From_To (Depth, Depth - 1);
               OB.From_Block := Depth - 1;
               OB.Made_BB    := Get_Insert_Block;
               Position_Builder_At_End (Current_BB);
            end if;
         end;
      end loop;

      --  Go through the Exit_Points and Label_Info and clear out any
      --  From_Block that corresponds to our depth since those are no
      --  longer valid: if we push again to that depth, we'll have a
      --  different fixup.

      for J in Exit_Point_Low_Bound .. Exit_Points.Last loop
         Exit_Points.Table (J).From_Block := -1;
      end loop;

      for J in Label_Info_Low_Bound .. Label_Info.Last loop
         Label_Info.Table (J).From_Block := -1;
      end loop;

      --  If we made a label for end-of-block actions, move to it now.

      Move_To_BB (Next_BB);

      --  Free the lifetime data associated with this block

      while Lifetimes /= null loop
         Next := Lifetimes.Next;
         Free (Lifetimes);
         Lifetimes := Next;
      end loop;

      --  And finally pop our stack

      Block_Stack.Decrement_Last;
   end Pop_Block;

   ------------------
   -- Emit_Reraise --
   ------------------

   procedure Emit_Reraise is
   begin
      --  Find the innermost block that has exception data. Call reraise
      --  with that data.

      for J in reverse 1 .. Block_Stack.Last loop
         if Present (Block_Stack.Table (J).Exc_Ptr) then
            Call (Reraise_Fn, (1 => Block_Stack.Table (J).Exc_Ptr));
            Build_Unreachable;
            return;
         end if;
      end loop;

      --  We should have found such a block.

      pragma Assert (Decls_Only);
   end Emit_Reraise;

   --------------------------------------
   -- Process_Push_Pop_xxx_Error_Label --
   --------------------------------------

   procedure Process_Push_Pop_xxx_Error_Label (N : N_Push_Pop_xxx_Label_Id) is
      procedure Maybe_Warn (E : E_Label_Id);
      --  Warn if we haven't generated a branch to E

      ----------------
      -- Maybe_Warn --
      ----------------

      procedure Maybe_Warn (E : E_Label_Id) is
      begin
         if No (Get_Label_Info (E))
           and then No_Exception_Propagation_Active
         then
            Warn_If_No_Local_Raise (E);
         end if;
      end Maybe_Warn;

   begin
      case Nkind (N) is
         when N_Push_Constraint_Error_Label =>
            Constraint_Error_Stack.Append (Exception_Label (N));

         when N_Push_Storage_Error_Label =>
            Storage_Error_Stack.Append (Exception_Label (N));

         when N_Push_Program_Error_Label =>
            Program_Error_Stack.Append (Exception_Label (N));

         when N_Pop_Constraint_Error_Label =>
            Maybe_Warn
              (Constraint_Error_Stack.Table (Constraint_Error_Stack.Last));
            Constraint_Error_Stack.Decrement_Last;

         when N_Pop_Storage_Error_Label =>
            Maybe_Warn
              (Storage_Error_Stack.Table (Storage_Error_Stack.Last));
            Storage_Error_Stack.Decrement_Last;

         when N_Pop_Program_Error_Label =>
            Maybe_Warn
              (Program_Error_Stack.Table (Program_Error_Stack.Last));
            Program_Error_Stack.Decrement_Last;

         when others =>
            pragma Assert (Decls_Only);
      end case;
   end Process_Push_Pop_xxx_Error_Label;

   ------------------------------
   -- Get_Exception_Goto_Entry --
   ------------------------------

   function Get_Exception_Goto_Entry (Kind : Node_Kind) return Opt_E_Label_Id
   is
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

   function Get_Label_BB
     (E : E_Label_Id; For_Address : Boolean := False) return Basic_Block_T
   is
      Depth : constant Block_Stack_Level := Block_Stack.Last;
      L_Idx : Label_Info_Id              := Get_Label_Info (E);

   begin
      --  If we haven't either defined or tried to branch to this label
      --  before, build a new Label_Info entry with just the label.

      if No (L_Idx) then
         Label_Info.Append ((Orig_BB         =>
                               Create_Basic_Block (Get_Name (E)),
                             Fixup_BB        => No_BB_T,
                             Block_Depth     => -1,
                             From_Block      => -1,
                             Has_Open_Branch => False));
         L_Idx := Label_Info.Last;
         Set_Label_Info (E, L_Idx);
      end if;

      declare
         LI         : Label_Data renames Label_Info.Table (L_Idx);
         Current_BB : constant Basic_Block_T := Get_Insert_Block;
         Orig_BB    : constant Basic_Block_T := LI.Orig_BB;

      begin
         --  If we're getting this label to take its address, ignore any
         --  possible fixup issues.

         if For_Address then
            return Orig_BB;

         --  Otherwise see if we know where this label is. If we do, and
         --  the depth of this entry is our depth, we have the label to
         --  branch to which includes the needed fixups. If not, we can
         --  make a block for the fixups.

         elsif LI.Block_Depth >= 0 then
            if LI.From_Block /= Depth then
               LI.From_Block := Depth;
               LI.Fixup_BB   := Create_Basic_Block (Get_Name (E, ".f"));
               Position_Builder_At_End (LI.Fixup_BB);
               Build_Fixups_From_To (Depth, LI.Block_Depth);
               Build_Br (Orig_BB);
               Position_Builder_At_End (Current_BB);
            end if;

            return LI.Fixup_BB;

         --  If we don't know where this label is, we need an entry in the
         --  Open_Branches table for this label. But first see if somebody
         --  already made one. However, if we've already started writing
         --  fixups to it, we need to make sure that we branch to after
         --  those fixup by starting a new block since a branch from an
         --  outer level shouldn't be using those fixups.

         else
            for J in reverse 1 .. Open_Branches.Last loop
               declare
                  OB : Open_Branch renames Open_Branches.Table (J);

               begin
                  if  OB.Orig_BB = Orig_BB and then OB.From_Block = Depth then
                     if Present (Get_Last_Instruction (OB.Made_BB)) then
                        Position_Builder_At_End (OB.Made_BB);
                        Move_To_BB (Create_Basic_Block (Get_Name (E, ".mm")));
                        OB.Made_BB := Get_Insert_Block;
                        Position_Builder_At_End (Current_BB);
                     end if;

                     return OB.Made_BB;
                  end if;
               end;
            end loop;

            Open_Branches.Append ((Orig_BB    => LI.Orig_BB,
                                   Made_BB    =>
                                     Create_Basic_Block (Get_Name (E, ".m")),
                                   From_Block => Depth));
            LI.Has_Open_Branch := True;
            return Open_Branches.Table (Open_Branches.Last).Made_BB;
         end if;
      end;
   end Get_Label_BB;

   ---------------------------
   -- Enter_Block_With_Node --
   ---------------------------

   function Enter_Block_With_Node (Node : Opt_N_Label_Id) return Basic_Block_T
   is
      E         : constant Opt_E_Label_Id  :=
        (if Present (Node) and then Present (Identifier (Node))
         then Entity (Identifier (Node)) else Empty);
      Name      : constant String          :=
        (if Present (E) then Get_Name (E) else "");
      This_BB   : constant Basic_Block_T   := Get_Insert_Block;
      Last_Inst : constant Value_T         := Get_Last_Instruction (This_BB);
      Entry_BB  : constant Basic_Block_T   :=
          Get_Entry_Basic_Block (+Current_Func);
      L_Idx     : constant Label_Info_Id   :=
          (if Present (E) then Get_Label_Info (E) else Empty_Label_Info_Id);
      BB        : constant Basic_Block_T   :=
          (if    Present (L_Idx) then Label_Info.Table (L_Idx).Orig_BB
           elsif No (Last_Inst) and then This_BB /= Entry_BB
           then  This_BB else Create_Basic_Block (Name));
      --  If we have an identifier and it has a basic block already set,
      --  that's the one that we have to use. If we've just started a basic
      --  block with no instructions in it, that basic block will do,
      --  unless it's the entry BB since we're going to branch to it.
      --  Otherwise, get a new one.

   begin
      --  Now, unless this is our basic block, jump to it and position there.
      --  If we take the address of this label, we have to do this with
      --  an indirectbr.

      if BB /= This_BB then
         if Present (E) and then Address_Taken (E) then
            declare
               BB_Val : constant GL_Value := Block_Address (Current_Func, BB);
               Ind_Br : constant Value_T  := Build_Indirect_Br (BB_Val);

            begin
               Add_Destination (Ind_Br, BB);
               Position_Builder_At_End (BB);

               --  The optimizer will undo what we did, so verify that we're
               --  not compiling with optimization.

               if Code_Gen_Level /= Code_Gen_Level_None then
                  Error_Msg_NE
                    ("??optimization breaks taking addresses of labels", E, E);
               end if;
            end;
         else
            Move_To_BB (BB);
         end if;
      end if;

      --  If we don't have an entity to point to the block, we're done

      if No (E) then
         return BB;
      end if;

      --  If we didn't previously have an entry, make one and we're done

      if No (L_Idx) then
         Label_Info.Append ((Orig_BB         => BB,
                             Fixup_BB        => No_BB_T,
                             Block_Depth     => Block_Stack.Last,
                             From_Block      => -1,
                             Has_Open_Branch => False));
         Set_Label_Info (E, Label_Info.Last);
         return BB;
      end if;

      declare
         LI : Label_Data renames Label_Info.Table (L_Idx);

      begin
         LI.Block_Depth := Block_Stack.Last;

         --  If we don't have any open branch entries made for this label,
         --  we have nothing left to do.

         if not LI.Has_Open_Branch then
            return BB;
         end if;

         --  Otherwise, we have to process each open branch for this label
         --  by branching to it and marking the entry inactive.

         for J in reverse 1 .. Open_Branches.Last loop
            declare
               OB : Open_Branch renames Open_Branches.Table (J);

            begin
               if OB.Orig_BB = BB then
                  Position_Builder_At_End (OB.Made_BB);
                  Build_Br (OB.Orig_BB);
                  OB.From_Block := -1;
                  Position_Builder_At_End (BB);
               end if;
            end;
         end loop;
      end;

      return BB;

   end Enter_Block_With_Node;

   ---------------
   -- Push_Loop --
   ---------------

   procedure Push_Loop (LE : E_Loop_Id; Exit_Point : Basic_Block_T) is
   begin
      Exit_Points.Append ((Label_Entity => LE,
                           Block_Depth  => Block_Stack.Last,
                           Orig_BB      => Exit_Point,
                           Exit_BB      => No_BB_T,
                           From_Block   => -1));
   end Push_Loop;

   --------------
   -- Pop_Loop --
   --------------

   procedure Pop_Loop is
   begin
      Exit_Points.Decrement_Last;
   end Pop_Loop;

   ---------------------
   -- Find_Exit_Point --
   ---------------------

   function Find_Exit_Point
     (N : Opt_N_Identifier_Id) return Exit_Point_Level is
   begin
      --  If no exit label was specified, use the last one

      if No (N) then
         return Exit_Points.Last;
      end if;

      --  Otherwise search for a match

      for I in Exit_Point_Low_Bound .. Exit_Points.Last loop
         if Exit_Points.Table (I).Label_Entity = Entity (N) then
            return I;
         end if;
      end loop;

      --  If the loop label isn't registered, then we just met an exit
      --  statement with no corresponding loop: should not happen.

      pragma Assert (Decls_Only);
      return Exit_Points.Last;
   end Find_Exit_Point;

   --------------------
   -- Get_Exit_Point --
   --------------------

   function Get_Exit_Point (N : Opt_N_Has_Entity_Id) return Basic_Block_T is
      Current_BB : constant Basic_Block_T    := Get_Insert_Block;
      EPT_Index  : constant Exit_Point_Level := Find_Exit_Point (N);
      EPT        : Exit_Point renames Exit_Points.Table (EPT_Index);

   begin
      --  If this entry doesn't correspond to a fixup from the current block,
      --  make one.

      if EPT.From_Block /= Block_Stack.Last then
         EPT.From_Block := Block_Stack.Last;
         EPT.Exit_BB    := Create_Basic_Block;
         Position_Builder_At_End (EPT.Exit_BB);
         Build_Fixups_From_To (Block_Stack.Last, EPT.Block_Depth);
         Build_Br (EPT.Orig_BB);
         Position_Builder_At_End (Current_BB);
      end if;

      return EPT.Exit_BB;
   end Get_Exit_Point;

   ------------------------
   -- Reset_Block_Tables --
   ------------------------

   procedure Reset_Block_Tables is
   begin
      Label_Info.Set_Last    (Label_Info_Low_Bound);
      Block_Stack.Set_Last   (0);
      Dispatch_Info.Set_Last (0);
      Open_Branches.Set_Last (0);
   end Reset_Block_Tables;

   ----------------
   -- Emit_Raise --
   ----------------

   procedure Emit_Raise (N : N_Raise_xxx_Error_Id) is
      Label        : constant Opt_E_Label_Id   :=
        Get_Exception_Goto_Entry (Nkind (N));
      Cond         : constant Opt_N_Subexpr_Id := Condition (N);
      BB_Raise     : constant Basic_Block_T    :=
        (if    Present (Label) then Get_Label_BB (Label)
         elsif No (Cond) then No_BB_T else Create_Basic_Block ("RAISE"));
      BB_Next      : constant Basic_Block_T    :=
        (if   Present (Cond) or else Present (Label) then Create_Basic_Block
         else No_BB_T);
      Pos          : constant Position_T       := Get_Current_Position;
      Cannot_Raise : Boolean := False;

   begin
      --  If there's a condition, test it. If we have the label case,
      --  that's all we have to do since it's one of two branches.

      if Present (Cond) then
         Emit_If_Cond (Cond, BB_Raise, BB_Next);
      elsif Present (Label) then
         Build_Br (BB_Raise);
      end if;

      --  If all we've done is an unconditional branch to BB_Next, we know
      --  there can't be an exception. Test by positioning there and seeing
      --  if it's equivalent to our starting position. We've arranged
      --  things so that this change of position will be undone below if
      --  needed as long as this isn't a label case.

      if Present (Cond) and then not Present (Label) then
         Position_Builder_At_End (BB_Next);

         if Is_Equivalent_Position (Pos, Get_Current_Position) then
            Cannot_Raise := True;
            Delete_Basic_Block (BB_Raise);
         end if;
      end if;

      --  If this isn't the branch case and we haven't proved that no raise
      --  can occur, we have to emit code to raise the exception, possibly
      --  only if the condition above failed.

      if No (Label)  and then not Cannot_Raise then
         declare
            Kind : constant RT_Exception_Code :=
              RT_Exception_Code'Val (+Reason (N));

         begin
            if Present (BB_Raise) then
               Position_Builder_At_End (BB_Raise);
            end if;

            --  See if we're asked to provide extended exception information.
            --  If so, there are two cases. For Access checks, we provide
            --  column information. For some other checks, we provide the
            --  actual value and the bounds, if we know them. In the latter
            --  case, we may not be able to find all the information and, if
            --  so, we fall back to the basic case.

            if Exception_Extra_Info and then Kind = CE_Access_Check_Failed then
               Emit_Raise_Call (N, Kind, Column => True);
            elsif Exception_Extra_Info and then Present (Cond)
              and then Kind in
                CE_Index_Check_Failed | CE_Range_Check_Failed | CE_Invalid_Data
              and then Emit_Raise_Call_With_Extra_Info (N, Kind, Cond)
            then
               null;

            --  Otherwise, just call the basic exception raise function

            else
               Emit_Raise_Call (N, Kind);
            end if;
         end;

         Maybe_Build_Br (BB_Next);
      end if;

      --  If we've needed to make one, now switch to the block past
      --  the condition.

      if Present (BB_Next) then
         Position_Builder_At_End (BB_Next);
      end if;
   end Emit_Raise;

   -----------------
   -- Get_EH_Slot --
   -----------------

   function Get_EH_Slot (Exc : GL_Value) return GL_Value is

      --  The LLVM intrinsic llvm.eh.typeid.for is special in that it
      --  always requires its argument to be a pointer in address space 0,
      --  regardless of the module's default address space. We therefore
      --  need to cast the exception pointer first.

      Exc_AS0 : constant GL_Value :=
        G_From
          (Addr_Space_Cast
             (IR_Builder, +Exc, Pointer_Type (Byte_T, 0), ""),
           Exc);
   begin
      return Call (EH_Slot_Id_Fn, (1 => Exc_AS0));
   end Get_EH_Slot;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register_Global_Name ("__gnat_all_others_value");
      Register_Global_Name ("__gnat_begin_handler");
      Register_Global_Name ("__gnat_end_handler");
      Register_Global_Name ("__gnat_others_value");
      Register_Global_Name
        (Get_Personality_Function_Name (Normalized_Target_Triple.all));
      Register_Global_Name ("__gnat_reraise_zcx");
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

begin
   --  Make a dummy entry in the label info table, so the "Empty"
   --  entry is never used.

   Label_Info.Increment_Last;

end GNATLLVM.Blocks;
