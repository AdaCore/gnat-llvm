------------------------------------------------------------------------------
--                              C C G                                       --
--                                                                          --
--                     Copyright (C) 2020-2023, AdaCore                     --
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

with Interfaces.C; use Interfaces.C;

with LLVM.Core; use LLVM.Core;

with Debug;  use Debug;
with Errout; use Errout;

with GNATLLVM.Codegen; use GNATLLVM.Codegen;
with GNATLLVM.Wrapper; use GNATLLVM.Wrapper;

with CCG.Environment;  use CCG.Environment;
with CCG.Utils;        use CCG.Utils;

use CCG.BB_Sets;
use CCG.Value_Sets;

package body CCG.Transform is

   type SC_Kind is (Or_Else, And_Then);
   type FN_Array is array (SC_Kind) of Value_T;
   SC_FNs : FN_Array := (others => No_Value_T);
   --  Builtin functions for "or else" and "and or"

   function Get_Term_True_BB (BB : Basic_Block_T) return Basic_Block_T is
     (Get_True_BB (Get_Basic_Block_Terminator (BB)))
     with Pre => Present (BB), Post => Present (Get_Term_True_BB'Result);

   function Get_Term_False_BB (BB : Basic_Block_T) return Basic_Block_T is
     (Get_False_BB (Get_Basic_Block_Terminator (BB)))
     with Pre => Present (BB), Post => Present (Get_Term_False_BB'Result);

   function Get_Term_BB (BB : Basic_Block_T) return Basic_Block_T is
     (Value_As_Basic_Block (Get_Operand0 (Get_Basic_Block_Terminator (BB))))
     with Pre => Present (BB), Post => Present (Get_Term_BB'Result);
   --  Some shortcuts to simplify code that checks what a terminator of
   --  a basic block does.

   function Has_Cond_Br (BB : Basic_Block_T) return Boolean is
     (Is_Cond_Br (Get_Basic_Block_Terminator (BB)))
     with Pre => Present (BB);

   function Is_Just_Return (BB : Basic_Block_T) return Boolean is
     (Is_A_Return_Inst (Get_First_Instruction (BB)))
     with Pre => Present (BB);

   function Has_Return (BB : Basic_Block_T) return Boolean is
     (Is_A_Return_Inst (Get_Basic_Block_Terminator (BB))
      or else (Is_Unc_Br (Get_Basic_Block_Terminator (BB))
               and then Is_Just_Return (Get_Term_BB (BB))))
     with Pre => Present (BB);

   procedure Remove_Some_Intrinsics (V : Value_T)
     with Pre => Is_A_Function (V);
   --  Remove debgu and lifetime intrinsics from the function V

   procedure Eliminate_Phis (V : Value_T)
     with Pre => Is_A_Function (V);
   --  Eliminate the usage of Phis in V by replacing with loads and
   --  stores (or returns).

   procedure Follow_Jumps (V : Value_T)
     with Pre => Is_A_Function (V);
   --  If a successor is a basic block with just an unconditional branch,
   --  update the successor to the target of that branch.

   function Get_Short_Circuit_FN (Kind : SC_Kind) return Value_T
     with Post => Present (Get_Short_Circuit_FN'Result);
   --  Get or make the appropriate short circuit function for Kind

   function Call_Is_Short_Circuit (V : Value_T) return Boolean is
     (Get_Called_Value (V) = SC_FNs (And_Then)
      or else Get_Called_Value (V) = SC_FNs (Or_Else))
     with Pre => Is_A_Call_Inst (V);
   --  Return True if this is a call to one of the above builtins

   function Negate_Condition
     (V : Value_T; Do_Nothing : Boolean := False) return Boolean
     with Pre => Present (V);
   --  If V is only used once, replace it with a value that represents the
   --  negative of that condition and return True. Otherwise, return False.
   --  If Do_Nothing is True, don't actually make any change, just indicate
   --  whether such a change can be made.

   function Is_Only_Condition (BB : Basic_Block_T) return Boolean
     with Pre => Present (BB);
   --  Returns True if every instruction in the basic block is used
   --  to compute a condition and we'll be able to generate that condition
   --  as a single C expression. This is used to reconstruct "or else" and
   --  "and then".

   procedure Make_Short_Circuit_Op (BB : Basic_Block_T; Kind : SC_Kind)
     with Pre => Present (BB);
   --  BB is a block ending in a conditional branch to a block that
   --  just has a condition computation that corresponds to an "or else"
   --  or "and then" (Kind says which).

   procedure Build_Short_Circuit_Ops (V : Value_T)
     with Pre => Is_A_Function (V);
   --  Look at all the blocks in V and see if we can create "and then" or
   --  "or else" operations.

   procedure Swap_Branches (V : Value_T)
     with Pre => Is_A_Function (V);
   --  See if we can make cleaner code by swapping the operands in a
   --  conditional branch instruction.

   ----------------------------
   -- Remove_Some_Intrinsics --
   ----------------------------

   procedure Remove_Some_Intrinsics (V : Value_T) is
      BB        : Basic_Block_T := Get_First_Basic_Block (V);
      Inst      : Value_T;
      Next_Inst : Value_T;

   begin
      --  Loop through all basic blocks and look for instructions that are
      --  debug or lifetime intrinsics. These can be added by the LLVM
      --  optimizer and can confuse tests for when basic blocks are simple
      --  enough, so remove all of them here.

      while Present (BB) loop
         Inst := Get_First_Instruction (BB);
         while Present (Inst) loop
            Next_Inst := Get_Next_Instruction (Inst);

            if Is_A_Dbg_Info_Intrinsic (Inst)
              or else Is_Lifetime_Intrinsic (Inst)
            then
               Instruction_Erase_From_Parent (Inst);
            end if;

            Inst := Next_Inst;
         end loop;

         BB := Get_Next_Basic_Block (BB);
      end loop;
   end Remove_Some_Intrinsics;

   --------------------
   -- Eliminate_Phis --
   --------------------

   procedure Eliminate_Phis (V : Value_T) is

      function Phi_Alloca (V : Value_T) return Value_T
        with Pre  => Is_APHI_Node (V),
             Post => Is_A_Alloca_Inst (Phi_Alloca'Result);
      --  Return the variable allocated to hold the value of Phi node V

      function Phi_Value (V : Value_T; From_BB : Basic_Block_T) return Value_T
        with Pre  => Is_APHI_Node (V) and then Present (From_BB),
             Post => Type_Of (V) = Type_Of (Phi_Value'Result);
      --  Return the value of V, a Phi node, that's active when branched yto
      --  by From_BB.

      function Is_Return_Phi (V : Value_T) return Boolean
        with Pre => Is_APHI_Node (V);
      --  Return True if V is a Phi instruction that's only used in a
      --  Return instruction and whose basic block contains just Phis and
      --  the Return.

      BB         : Basic_Block_T := Get_First_Basic_Block (V);
      Alloca_Loc : Value_T       :=
        Get_First_Instruction (Get_Entry_Basic_Block (V));
      Phi_Map    : Value_Value_Map_P.Map;
      Ret_Phis   : Value_Sets.Set;
      New_BBs    : BB_Sets.Set;
      Inst       : Value_T;
      Next_BB    : Basic_Block_T;

      ----------------
      -- Phi_Alloca --
      ----------------

      function Phi_Alloca (V : Value_T) return Value_T is
      begin
         --  If we haven't already made an alloca for this phi, make one now

         if not Phi_Map.Contains (V) then
            declare
               Name   : constant String  := Get_Value_Name (V);
               Result : constant Value_T :=
                 Insert_Alloca_Before (Type_Of (V), Alloca_Loc);

            begin
               --  The name of the Phi instruction won't be used, so clear that
               --  name and use it, if any, for the variable we're making.
               --  Also copy any entity from the original value if the entity
               --  isn't already a reference.

               Set_Value_Name_2 (V, "", 0);

               if not Get_Entity_Is_Ref (V) then
                  Set_Entity (Result, Get_Entity (V));
                  Set_Entity_Is_Ref (Result);
               end if;

               Phi_Map.Insert (V, Result);

               if Name'Length /= 0 then
                  Set_Value_Name_2 (Result, Name, Name'Length);
               end if;

               return Result;
            end;
         else
            return Phi_Map.Element (V);
         end if;

      end Phi_Alloca;

      ---------------
      -- Phi_Value --
      ---------------

      function Phi_Value (V : Value_T; From_BB : Basic_Block_T) return Value_T
      is
      begin
         return Val : Value_T := No_Value_T do
            for J in 0 .. Count_Incoming (V) - 1 loop
               if Get_Incoming_Block (V, J) = From_BB then
                  Val := Get_Operand (V, J);
               end if;
            end loop;
         end return;
      end Phi_Value;

      -------------------
      -- Is_Return_Phi --
      -------------------

      function Is_Return_Phi (V : Value_T) return Boolean is
         User : constant Value_T := Single_User (V);
         Inst : Value_T := V;

      begin
         --  The optimizer sometimes creates a Phi just to merge returns.
         --  When generating C, we want to undo that and prefer to generate
         --  the return. So check for a Phi that's just used once and for a
         --  return. For simplicity, do this only if it's the first Phi
         --  (which it should be) and don't do this for array types, since
         --  they can't be directly returned in C. It also must be in the same
         --  basic block as the Phi.

         if No (User) or else Get_Opcode (User) /= Op_Ret
           or else Is_Array_Type (V)
           or else Get_Instruction_Parent (V) /= Get_Instruction_Parent (User)
         then
            return False;
         end if;

         --  Now see if we run into something that's not a Phi before we
         --  hit the return.

         while Present (Inst) and then Inst /= User loop
            if not Is_APHI_Node (Inst) then
               return False;
            end if;

            Inst := Get_Next_Instruction (Inst);
         end loop;

         --  Otherwise, it's a return Phi

         return True;
      end Is_Return_Phi;

   begin -- Start of processing for Eliminate_Phis

      --  We want to put any allocas we create after any that are already
      --  present in the entry block. So skip them. We assume here that
      --  we put them at the start of the block. If that's not the case, the
      --  worst will be having two sequences of allocas in the entry block.

      while Is_A_Alloca_Inst (Alloca_Loc) loop
         Alloca_Loc := Get_Next_Instruction (Alloca_Loc);
      end loop;

      --  Our first pass goes through each basic block looking for branches
      --  to a phi.

      --  First, loop through each basic block in V looking for branches to
      --  blocks that start with one or more Phi nodes. If so, add stores
      --  of the proper value to the Phi alloca unless this a Phi that
      --  handles returns. In the latter case, if the terminator is an
      --  unconditional branch, replace the terminator with a return
      --  instruction and delete the current terminator. For a conditional
      --  branch or a switch, create a new basic block with the return and
      --  branch to it instead.
      --
      --  If we have a unconditional branch, put the store(s) in front of
      --  the unconditional branch. However, if we have a conditional branch,
      --  do the same as we do for a return and create a new block for
      --  the store(s). We do this to make it easier to create an if/elseif
      --  construct for optimized code.

      while Present (BB) loop
         Inst := Get_Basic_Block_Terminator (BB);
         for J in Nat range 0 .. Get_Num_Successors (Inst) - 1 loop
            declare
               Dest_BB     : constant Basic_Block_T := Get_Successor (Inst, J);
               Inst_Erased : Boolean                := False;
               Insert_BB   : Basic_Block_T          := BB;
               Dest_Inst   : Value_T                :=
                 Get_First_Instruction (Dest_BB);
               Insert_Inst : Value_T                := Inst;

            begin
               while Is_APHI_Node (Dest_Inst)
                 and then not Contains (New_BBs, BB)
               loop

                  --  If our terminator isn't an unconditional branch and
                  --  we haven't created a new basic block, do it now.

                  if not Is_Unc_Br (Inst) and then Insert_BB = BB then
                     Insert_BB := Append_Basic_Block (V, "");
                     Insert (New_BBs, Insert_BB);
                  end if;

                  --  Now handle the case of a return Phi and non-return Phi
                  --  separately.

                  if Is_Return_Phi (Dest_Inst) then

                     --  Insert the return instruction into the appropriate
                     --  block and remove our instruction if it's
                     --  an unconditional branch.

                     Insert_At_Block_End
                       (Create_Return (Phi_Value (Dest_Inst, BB)),
                        Insert_BB, Dest_Inst);

                     if Is_Unc_Br (Inst) then
                        Instruction_Erase_From_Parent (Inst);
                        Inst_Erased := True;
                     end if;

                     --  If we have nested phi nodes, it's possible that
                     --  something that wasn't considered a return phi here
                     --  may later if we've replaced a branch with a return.
                     --  That will later cause a crash. So record the Phi's
                     --  that we consider return Phis and use that below.

                     if not Contains (Ret_Phis, Dest_Inst) then
                        Insert (Ret_Phis, Dest_Inst);
                     end if;

                     --  Since, by definition, there's only one return
                     --  Phi per block, we're done.

                     exit;
                  else
                     --  If we have an unconditional branch instruction, we
                     --  can insert the store immediately before it. But if
                     --  not, we've already made a basic block above and we
                     --  need to add a branch to our original successor. By
                     --  updating Insert_Inst each time, we ensure that
                     --  this will only happen for the first Phi in the
                     --  destination block.

                     if not Is_Unc_Br (Insert_Inst) then
                        Insert_Inst := Create_Br (Dest_BB);
                        Insert_At_Block_End (Insert_Inst, Insert_BB, Inst);
                     end if;

                     --  Now add the store, either to the original block or
                     --  to the one we made above.

                     Insert_Store_Before (Phi_Value (Dest_Inst, BB),
                                          Phi_Alloca (Dest_Inst), Insert_Inst);
                  end if;

                  Dest_Inst := Get_Next_Instruction (Dest_Inst);
               end loop;

               --  Now possibly update our successor

               if not Inst_Erased and then not Is_Unc_Br (Inst)
                 and then Insert_BB /= BB
               then
                  Set_Successor (Inst, J, Insert_BB);
               end if;
            end;
         end loop;

         BB := Get_Next_Basic_Block (BB);
      end loop;

      --  In our second pass, we replace each Phi with a load from the
      --  alloca we've made for it unless it's a return Phi, in which case
      --  delete this block since it's dead.

      BB := Get_First_Basic_Block (V);
      while Present (BB) loop
         Next_BB := Get_Next_Basic_Block (BB);
         Inst    := Get_First_Instruction (BB);
         while Is_APHI_Node (Inst) loop
            if Contains (Ret_Phis, Inst) then
               Delete_Basic_Block (BB);
               exit;
            else
               declare
                  Load_Inst : constant Value_T :=
                    Insert_Load_Before (Type_Of (Inst), Phi_Alloca (Inst),
                                        Inst);
                  Next_Inst : constant Value_T := Get_Next_Instruction (Inst);

               begin
                  Replace_All_Uses_With (Inst, Load_Inst);
                  Instruction_Erase_From_Parent (Inst);
                  Inst := Next_Inst;
               end;
            end if;
         end loop;

         BB := Next_BB;
      end loop;
   end Eliminate_Phis;

   ------------------
   -- Follow_Jumps --
   ------------------

   procedure Follow_Jumps (V : Value_T) is
      Entry_BB : constant Basic_Block_T := Get_Entry_Basic_Block (V);
      BB       : Basic_Block_T          := Get_First_Basic_Block (V);
      Changed  : Boolean                := True;
      Next_BB  : Basic_Block_T;
      Term     : Value_T;
      Inst     : Value_T;
      Dest     : Basic_Block_T;
      Count    : Nat;

   begin
      --  Look at the successors of the terminator of each basic block
      --  and replace each with the ultimate destination. Protect against
      --  and infinite loop here if the program has an infinite loop.

      while Present (BB) loop
         Term := Get_Basic_Block_Terminator (BB);
         for J in Nat range 0 .. Get_Num_Successors (Term) - 1 loop
            Dest  := Get_Successor (Term, J);
            Inst  := Get_First_Instruction (Dest);
            Count := 0;
            while Is_A_Terminator_Inst (Inst)
              and then Get_Num_Successors (Inst) = Nat (1) and then Count < 10
            loop
               Dest  := Get_Successor (Inst, Nat (0));
               Inst  := Get_First_Instruction (Dest);
               Count := Count + 1;
            end loop;

            Set_Successor (Term, J, Dest);
         end loop;

         BB := Get_Next_Basic_Block (BB);
      end loop;

      --  Now delete any basic blocks that were just a jump and have
      --  become dead. Note that we can't delete other dead basic blocks
      --  since their results may be used elsewhere.

      while Changed loop
         Changed := False;
         BB      := Get_First_Basic_Block (V);
         while Present (BB) loop
            Next_BB := Get_Next_Basic_Block (BB);

            if BB /= Entry_BB and then Is_Dead_Basic_Block (BB)
              and then Is_A_Terminator_Inst (Get_First_Instruction (BB))
            then
               Delete_Basic_Block (BB);
               Changed := True;
            end if;

            BB := Next_BB;
         end loop;
      end loop;

   end Follow_Jumps;

   ----------------------
   -- Negate_Condition --
   ----------------------

   function Negate_Condition
     (V : Value_T; Do_Nothing : Boolean := False) return Boolean
   is
   begin
      --  If V is used more than once, it's too complicated to do anything

      if Num_Uses (V) > 1 then
         return False;

      --  If it's a NOT instruction (XOR with 1), we can replace V
      --  with the operand of the XOR.

      elsif Is_A_Instruction (V) and then Get_Opcode (V) = Op_Xor
        and then Is_A_Constant_Int (Get_Operand1 (V))
        and then Equals_Int (Get_Operand1 (V), 1)
      then
         if not Do_Nothing then
            Replace_All_Uses_With (V, Get_Operand0 (V));
         end if;

         return True;

      --  If it's an AND or OR instruction, negate both operand and
      --  replace AND with OR and vice versa.

      elsif ((Is_A_Instruction (V) and then Get_Opcode (V) in Op_And | Op_Or)
             or else (Is_A_Call_Inst (V) and then Call_Is_Short_Circuit (V)))
        and then Negate_Condition (Get_Operand0 (V), True)
        and then Negate_Condition (Get_Operand1 (V), Do_Nothing)
      then
         if not Do_Nothing then
            Discard (Negate_Condition (Get_Operand0 (V)));
            Replace_Inst_With_Inst
              (V, (case Get_Opcode (V) is
                   when Op_And =>
                     Create_Or  (Get_Operand0 (V), Get_Operand1 (V)),
                   when Op_Or =>
                     Create_And (Get_Operand0 (V), Get_Operand1 (V)),
                   when others =>
                     Create_Call_2
                       ((if   Get_Called_Value (V) = SC_FNs (Or_Else)
                         then Get_Short_Circuit_FN (And_Then)
                         else Get_Short_Circuit_FN (Or_Else)),
                        Get_Operand0 (V), Get_Operand1 (V))));
         end if;

         return True;

      --  If it's a comparison instruction, we can invert the comparison

      elsif Is_A_Instruction (V)
        and then Get_Opcode (V) in Op_I_Cmp | Op_F_Cmp
      then
         if not Do_Nothing then
            Invert_Predicate (V);
         end if;

         return True;

      --  Otherwise, we can't do anything. We could add an XOR
      --  instruction here, but that case isn't worth dealing with.

      else
         return False;
      end if;

   end Negate_Condition;

   ----------------------------
   -- Has_Unique_Predecessor --
   ----------------------------

   function Has_Unique_Predecessor (BB : Basic_Block_T) return Boolean is
      Pred : constant Basic_Block_T := Get_Unique_Predecessor (BB);
      V    : Value_T;

   begin
      --  If there's no single predecssor, we're done

      if No (Pred) then
         return False;
      end if;

      --  Otherwise, see if the predecessor is just an unconditional brancha

      V := Get_First_Non_Phi_Or_Dbg (Pred);
      return (if Is_Unc_Br (V) then Has_Unique_Predecessor (Pred) else True);
   end Has_Unique_Predecessor;

   -----------------------
   -- Is_Only_Condition --
   -----------------------

   function Is_Only_Condition (BB : Basic_Block_T) return Boolean is
      function Scan_For_Only_Condition (V : Value_T) return Boolean
        with Pre => Present (V);
      --  Scan V, an operand of an instruction, to see if anything in
      --  it prevents this block from being a basic block that only has
      --  a condition. Return False if it means the block doesn't meet the
      --  criteria. Also counts the number of instructions used for the
      --  computation.

      Term              : constant Value_T := Get_Basic_Block_Terminator (BB);
      Num_Inst_In_Block : Nat              := 0;
      Num_Inst_In_Cond  : Nat              := 1;
      Had_Load_Call     : Boolean          := False;
      V                 : Value_T          := Get_First_Instruction (BB);

      -----------------------------
      -- Scan_For_Only_Condition --
      -----------------------------

      function Scan_For_Only_Condition (V : Value_T) return Boolean is
      begin
         --  If V is not an instruction or is located outside of our basic
         --  block, it doesn't cause any issues inside this block.

         if not Is_A_Instruction (V) or else Get_Instruction_Parent (V) /= BB
         then
            return True;

         --  If this is a variable or has multiple uses, we'll be making
         --  a declaration for it. If it's a Phi, it will generate code.

         elsif Is_Variable (V) or else Num_Uses (V) /= 1
           or else Is_APHI_Node (V)
         then
            return False;

         --  If this is call or load, we can support it, but if there's more
         --  than one, we'll be forcing things into variables, so we won't
         --  be making a simple expression for this. We could do better, but
         --  the idea here is just to catch the simple cases.
         --  ??? The problem here is that we consider loads as volatile and
         --  we perhaps should do this only for stores.

         elsif Is_A_Load_Inst (V)
           or else (Is_A_Call_Inst (V) and then not Call_Is_Short_Circuit (V))
         then
            if Had_Load_Call then
               return False;
            else
               Had_Load_Call := True;
            end if;
         end if;

         --  Otherwise, count this instruction. If one of its operands
         --  don't meet our condition, this block doesn't qualify.

         Num_Inst_In_Cond := Num_Inst_In_Cond + 1;
         return (for all J in Nat range 0 .. Get_Num_Operands (V) - 1 =>
                   Scan_For_Only_Condition (Get_Operand (V, J)));

      end Scan_For_Only_Condition;

   begin -- Start of processing for Is_Only_Condition

      --  If the terminator isn't a conditional branch or if the
      --  condition doesn't qualify, this block doesn't qualify.

      if not Is_Cond_Br (Term)
        or else not Scan_For_Only_Condition (Get_Condition (Term))
      then
         return False;
      end if;

      --  Otherwise, count the number of instruction in the block

      while Present (V) loop
         Num_Inst_In_Block := Num_Inst_In_Block + 1;
         V := Get_Next_Instruction (V);
      end loop;

      --  This block qualifies iff all instructions are part of the
      --  condition.

      return Num_Inst_In_Block = Num_Inst_In_Cond;
   end Is_Only_Condition;

   --------------------------
   -- Get_Short_Circuit_FN --
   --------------------------

   function Get_Short_Circuit_FN (Kind : SC_Kind) return Value_T is
      Fn : Value_T := SC_FNs (Kind);

   begin
      --  If we haven't already made the required function, do it now

      if No (Fn) then
         declare
            Param_Types : constant Type_Array := (1 => Bit_T, 2 => Bit_T);
            Name        : constant String     :=
              (if   Kind = Or_Else then "llvm.ccg.orelse"
               else "llvm.ccg.andthen");

         begin
            Fn := Add_Function
              (Module, Name,
               Function_Type (Bit_T, Param_Types'Address, 2, False));
            SC_FNs (Kind) := Fn;
         end;
      end if;

      return Fn;
   end Get_Short_Circuit_FN;

   ---------------------------
   -- Make_Short_Circuit_Op --
   ---------------------------

   procedure Make_Short_Circuit_Op (BB : Basic_Block_T; Kind : SC_Kind) is
      Our_Term    : constant Value_T       := Get_Basic_Block_Terminator (BB);
      Our_Cond    : constant Value_T       := Get_Condition (Our_Term);
      SC_Dest     : constant Value_T       :=
        Get_Operand (Our_Term, (if Kind = Or_Else then Nat (1) else Nat (2)));
      SC_BB       : constant Basic_Block_T := Value_As_Basic_Block (SC_Dest);
      SC_Term     : constant Value_T       :=
        Get_Basic_Block_Terminator (SC_BB);
      Fn          : constant Value_T       := Get_Short_Circuit_FN (Kind);
      Inst        : Value_T                := Get_First_Instruction (SC_BB);
      Next_Inst   : Value_T;
      Call_Inst   : Value_T;

   begin
      --  Remove the present terminator from the current basic block and
      --  move all instructions other than the terminator from our other
      --  basic block.

      Instruction_Remove_From_Parent (Our_Term);
      while Inst /= SC_Term loop
         Next_Inst := Get_Next_Instruction (Inst);
         Instruction_Remove_From_Parent (Inst);
         Insert_At_Block_End (Inst, BB, Inst);
         Inst := Next_Inst;
      end loop;

      --  Now add the or else / and or builtin call, change the condition
      --  of our other block's terminator to it, move that one as well,
      --  and delete our terminator and the other basic block.

      Call_Inst := Create_Call_2 (Fn, Our_Cond, Get_Last_Instruction (BB));
      Insert_At_Block_End (Call_Inst, BB, Our_Term);
      Set_Condition (SC_Term, Call_Inst);
      Instruction_Remove_From_Parent (SC_Term);
      Insert_At_Block_End (SC_Term, BB, SC_Term);
      Delete_Instruction (Our_Term);
      Delete_Basic_Block (SC_BB);

   end Make_Short_Circuit_Op;

   -----------------------------
   -- Build_Short_Circuit_Ops --
   -----------------------------

   procedure Build_Short_Circuit_Ops (V : Value_T) is
      BB      : Basic_Block_T := Get_First_Basic_Block (V);
      Next_BB : Basic_Block_T;
      Term    : Value_T;

   begin
      while Present (BB) loop
         Next_BB := Get_Next_Basic_Block (BB);
         Term    := Get_Basic_Block_Terminator (BB);

         if Is_Cond_Br (Term) then
            declare
               True_BB    : constant Basic_Block_T := Get_True_BB (Term);
               False_BB   : constant Basic_Block_T := Get_False_BB (Term);

            begin
               --  If our False side branches to a block whose terminator
               --  is a conditional branch whose True side is the same as
               --  ours and has only the computation of the condition and
               --  only one predecessor (which must be us), we have an "or
               --  else". But make sure we don't have a loop back to us.

               if False_BB /= BB and then True_BB /= BB
                 and then Has_Cond_Br (False_BB)
                 and then Get_Term_True_BB (False_BB) = True_BB
                 and then Has_Unique_Predecessor (False_BB)
                 and then Is_Only_Condition (False_BB)
               then
                  Make_Short_Circuit_Op (BB, Or_Else);
                  Next_BB := BB;

               --  Similarly, if our True side branches to a block whose
               --  terminator's False side is the same as ours, we have an
               --  "and then".

               elsif False_BB /= BB and then True_BB /= BB
                 and then Has_Cond_Br (True_BB)
                 and then Get_Term_False_BB (True_BB) = False_BB
                 and then Has_Unique_Predecessor (True_BB)
                 and then Is_Only_Condition (True_BB)
               then
                  Make_Short_Circuit_Op (BB, And_Then);
                  Next_BB := BB;
               end if;
            end;
         end if;

         BB := Next_BB;
      end loop;
   end Build_Short_Circuit_Ops;

   -------------------
   -- Swap_Branches --
   -------------------

   procedure Swap_Branches (V : Value_T) is
      BB   : Basic_Block_T := Get_First_Basic_Block (V);
      Term : Value_T;

   begin

      --  For each basic block, we start by looking at the terminator to
      --  see if we'll generate cleaner code by swapping the two operands
      --  and negating the condition.

      while Present (BB) loop
         Term := Get_Basic_Block_Terminator (BB);

         --  One case is if it's a conditional branch where the "true"
         --  block has more than one predecessor but the "false" block
         --  doesn't. But don't do this if the either destination has a
         --  return.

         if Is_Cond_Br (Term)
           and then not Has_Unique_Predecessor (Get_True_BB (Term))
           and then Has_Unique_Predecessor (Get_False_BB (Term))
           and then not Has_Return (Get_True_BB (Term))
           and then not Has_Return (Get_False_BB (Term))
           and then Negate_Condition (Get_Condition (Term))
         then
            Swap_Successors (Term);

         --  Or if the "false" is a return as long as the "true" isn't

         elsif Is_Cond_Br (Term)
           and then Has_Return (Get_False_BB (Term))
           and then not Has_Return (Get_True_BB (Term))
           and then Negate_Condition (Get_Condition (Term))
         then
            Swap_Successors (Term);

         --  Swap if both are a return but the "true" is just a return
         --  and the "false" isn't.

         elsif Is_Cond_Br (Term) and then Has_Return (Get_True_BB (Term))
           and then Has_Return (Get_False_BB (Term))
           and then Is_Just_Return (Get_True_BB (Term))
           and then not Is_Just_Return (Get_False_BB (Term))
           and then Negate_Condition (Get_Condition (Term))
         then
            Swap_Successors (Term);

         --  Another case is where we may be able to merge two tests into an
         --  "else if". We only test for the simple cases.

         elsif Is_Cond_Br (Term) and then not Has_Cond_Br (Get_False_BB (Term))
           and then Has_Cond_Br (Get_True_BB (Term))
           and then Is_Only_Condition (Get_True_BB (Term))
           and then ((Has_Return (Get_False_BB (Term))
                        and then Has_Return (Get_Term_True_BB
                                               (Get_True_BB (Term))))
                     or else Get_False_BB (Term) =
                               Get_Term_True_BB (Get_True_BB (Term)))
           and then Negate_Condition (Get_Condition (Term))
         then
            Swap_Successors (Term);
         end if;

         BB := Get_Next_Basic_Block (BB);
      end loop;
   end Swap_Branches;

   ----------------------
   -- Transform_Blocks --
   ----------------------

   procedure Transform_Blocks (V : Value_T) is
      Dump_Name : constant String := Output_File_Name (".trans.ll");
      Err_Msg   : aliased Ptr_Err_Msg_Type;

   begin
      Remove_Some_Intrinsics (V);
      Eliminate_Phis (V);
      Follow_Jumps (V);
      Build_Short_Circuit_Ops (V);
      Swap_Branches (V);

      --  If -gnatd_t, dump the transformed IR

      if Debug_Flag_Underscore_T then
         if Print_Module_To_File (Module, Dump_Name, Err_Msg'Address) then
            Error_Msg
              ("could not write `" & Dump_Name & "`: " &
               Get_LLVM_Error_Msg (Err_Msg), V);
         end if;
      end if;
   end Transform_Blocks;

end CCG.Transform;
