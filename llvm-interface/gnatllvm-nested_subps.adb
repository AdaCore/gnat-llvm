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

with Ada.Containers.Doubly_Linked_Lists;

with Atree; use Atree;
with Sinfo; use Sinfo;
with Einfo; use Einfo;

with GNATLLVM.Utils; use GNATLLVM.Utils;

package body GNATLLVM.Nested_Subps is

   procedure Get_Access
     (Def_Ident      : Node_Id;
      From_Subp      : Static_Link_Descriptor;
      Acc            : out Access_Record;
      Through_S_Link : out Boolean);
   --  Compute the access path to Def_Ident from From_Subp and store it to Acc,
   --  or set Through_S_Link to False if Def_Ident cannot be accessed through
   --  the local link.
   --  Adjust the corresponding static link descriptor's closure so that
   --  Def_Ident can be accessed from nested subprograms.

   procedure Compute_Static_Link_Descriptors
     (GNAT_Root : Node_Id;
      S_Links   : in out Static_Link_Descriptor_Maps.Map)
   is

      function Process (N : Node_Id) return Traverse_Result;
      procedure Traverse is new Traverse_Proc (Process);
      --  Tree traversal helpers

      type Deferred_Subprogram_Info is record
         Subp              : Node_Id;
         S_Link_Descriptor : Static_Link_Descriptor;
      end record;

      package Deferred_Subprogram_Queues is
        new Ada.Containers.Doubly_Linked_Lists
          (Element_Type => Deferred_Subprogram_Info);

      package Visited_Subprogram_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => Node_Id);

      Visited : Visited_Subprogram_Sets.Set;
      --  We want to process only once each subprogram. Rembember those we
      --  already met.

      Deferred_Subprograms : Deferred_Subprogram_Queues.List;
      --  Subprograms can refer to non-local variables that are declared after
      --  them, so we have to defer the processing of nested subprogram. Each
      --  subprogram is then analyzed only after its parent has been processed.
      --  This queue is used to defer subprograms.

      Current_Root : Node_Id := Empty;
      --  Root node of the current traversal.

      Current_Subp : Static_Link_Descriptor := null;
      --  Current previous static link descriptor. Root subprogram bodies have
      --  no parent, so start with null.

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
      begin
         case Nkind (N) is
            when N_Subprogram_Declaration =>
               --  Skip the defining identifiers in a subprogram declarations:
               --  they are not part of the current subprogram body.

               return Skip;

            when N_Subprogram_Body =>
               --  If we just started the traversal with this node, then
               --  this is a deferred subprogram, so we must no re-defer
               --  it. Likewise for already processed ones.

               if Current_Root = N then
                  return OK;

               elsif Visited.Contains (N) then
                  return Skip;
               end if;

               declare
                  Nested_Subp : constant Static_Link_Descriptor :=
                    new Static_Link_Descriptor_Record'
                      (Parent => Current_Subp,
                       others => <>);
               begin
                  S_Links.Insert
                    (Defining_Unit_Name (Get_Acting_Spec (N)), Nested_Subp);
                  Deferred_Subprograms.Append
                    ((Subp              => N,
                      S_Link_Descriptor => Nested_Subp));
                  Visited.Insert (N);
               end;

               return Skip;

            when N_Parameter_Specification | N_Object_Declaration =>
               --  Register the corresponding defining identifier as defined in
               --  the current subprogram body, if any.

               if Current_Subp /= null then
                  declare
                     Def_Ident : constant Node_Id :=
                       Defining_Identifier (N);
                  begin
                     Current_Subp.Def_Idents.Insert (Def_Ident);
                  end;
               end if;

            when N_Identifier | N_Expanded_Name =>
               --  Register some access to this identifier if and only if it is
               --  defined by a parent function.

               declare
                  Def_Ident      : constant Node_Id := Entity (N);
                  Through_S_Link : Boolean;
                  Acc            : Access_Record;
               begin
                  if Present (Def_Ident) and then Current_Subp /= null then
                     Get_Access (Def_Ident, Current_Subp, Acc, Through_S_Link);
                     if Through_S_Link and then Acc.Depth > 0 then
                        Current_Subp.Accesses.Include (Def_Ident, Acc);
                     end if;
                  end if;
               end;

            when N_Defining_Identifier =>
               --  Freeze points are not syntactic elements, so they are
               --  not reached by the regular traversal. They can contain
               --  subprogram bodies: process them, too.

               Traverse (Freeze_Node (N));
               return Skip;

            when others =>
               null;
         end case;

         --  By default, continue the usual traversal

         return OK;
      end Process;

   begin
      --  Perform a first partial traversal to get top-level subprograms

      Traverse (GNAT_Root);
      if Present (Library_Unit (GNAT_Root))
        and then Library_Unit (GNAT_Root) /= GNAT_Root
      then
         Traverse (Library_Unit (GNAT_Root));
      end if;

      --  Then traverse as many subprograms as possible

      while not Deferred_Subprograms.Is_Empty loop
         declare
            Deferred_Subp : constant Deferred_Subprogram_Info :=
              Deferred_Subprograms.First_Element;
         begin
            Deferred_Subprograms.Delete_First;
            Current_Subp := Deferred_Subp.S_Link_Descriptor;
            Current_Root := Deferred_Subp.Subp;
            Traverse (Deferred_Subp.Subp);
         end;
      end loop;
   end Compute_Static_Link_Descriptors;

   ----------------
   -- Get_Access --
   ----------------

   procedure Get_Access
     (Def_Ident      : Node_Id;
      From_Subp      : Static_Link_Descriptor;
      Acc            : out Access_Record;
      Through_S_Link : out Boolean)
   is
      Parent_Subp : Static_Link_Descriptor := From_Subp.Parent;
      --  Currently analyzed subprogram context. If it does not contain the
      --  information we are looking for, we look for it in its parent.

      Depth_Delta  : Natural := 1;
      --  Distance between From_Subp and Parent_Subp
   begin
      while Parent_Subp /= null loop
         declare
            use Local_Access_Maps;

            Acc_Cur : constant Cursor :=
              Parent_Subp.Accesses.Find (Def_Ident);
            I       : Positive;
         begin
            if Acc_Cur /= Local_Access_Maps.No_Element then
               --  Parent_Subp already references this identifier: compute the
               --  result from this access.

               Through_S_Link := True;
               Acc := Element (Acc_Cur);
               Acc.Depth := Acc.Depth + Depth_Delta;
               return;
            end if;

            if Parent_Subp.Def_Idents.Contains (Def_Ident) then
               --  Parent_Subp defines this identifier: if we are in a parent
               --  subprogram, register this identifier to the closure.

               Through_S_Link := True;
               Acc.Depth := Depth_Delta;

               I := 1;
               for N of Parent_Subp.Closure loop
                  if N = Def_Ident then
                     Acc.Field := I;
                     return;
                  end if;
                  I := I + 1;
               end loop;

               --  Alright, this is the first time Def_Ident is referenced in a
               --  nested function: add it to the closure.

               Parent_Subp.Closure.Append (Def_Ident);
               Acc.Field := Parent_Subp.Closure.Last_Index;

               return;
            end if;
         end;

         Parent_Subp := Parent_Subp.Parent;
         Depth_Delta := Depth_Delta + 1;
      end loop;

      Through_S_Link := False;
   end Get_Access;

end GNATLLVM.Nested_Subps;
