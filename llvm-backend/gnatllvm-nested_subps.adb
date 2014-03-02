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

      package Visited_Subprogram_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => Node_Id);
      Visited : Visited_Subprogram_Sets.Set;
      --  Traversal is stopped and restarted for each subprogram body node, so
      --  we need to remember the ones we have already processed.

      Current_Subp : Static_Link_Descriptor := null;
      --  Current previous static link descriptor. Root subprogram bodies have
      --  no parent, so start with null.

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
      begin
         case Nkind (N) is
            when N_Subprogram_Body =>
               --  If already processed as part of the "parent" traversal, just
               --  skip this node, but continue with its children.

               if Visited.Contains (N) then
                  return OK;
               else
                  Visited.Insert (N);
               end if;

               Current_Subp := new Static_Link_Descriptor_Record'
                 (Parent => Current_Subp,
                  others => <>);
               S_Links.Insert
                 (Defining_Unit_Name (Get_Acting_Spec (N)), Current_Subp);

               Traverse (N);

               --  Restore the current subprogram context and then continue
               --  with the siblings.

               Current_Subp := Current_Subp.Parent;
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

            when N_Identifier =>
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
      Traverse (GNAT_Root);
      if Present (Library_Unit (GNAT_Root))
        and then Library_Unit (GNAT_Root) /= GNAT_Root
      then
         Traverse (Library_Unit (GNAT_Root));
      end if;
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
      Current_Subp : Static_Link_Descriptor := From_Subp.Parent;
      --  Subprogram context that is currently analyzed. If it does not contain
      --  the information we are looking for, we get its parent.

      Depth_Delta  : Natural := 0;
      --  Distance between From_Subp and Current_Subp
   begin
      while Current_Subp /= null loop
         declare
            use Local_Access_Maps;

            Acc_Cur : constant Cursor :=
              Current_Subp.Accesses.Find (Def_Ident);
            I       : Positive;
         begin
            if Acc_Cur /= Local_Access_Maps.No_Element then
               --  Current_Subp already references this identifier: compute the
               --  result from this access.

               Through_S_Link := True;
               Acc := Element (Acc_Cur);
               Acc.Depth := Acc.Depth + Depth_Delta;
               return;
            end if;

            if Current_Subp.Def_Idents.Contains (Def_Ident) then
               --  Current_Subp defines this identifier: if we are in a parent
               --  subprogram, register this identifier to the closure.

               Through_S_Link := True;
               Acc.Depth := Depth_Delta;

               I := 1;
               for N of Current_Subp.Closure loop
                  if N = Def_Ident then
                     Acc.Field := I;
                     return;
                  end if;
                  I := I + 1;
               end loop;

               --  Alright, this is the first time Def_Ident is referenced in a
               --  nested function: add it to the closure.

               Current_Subp.Closure.Append (Def_Ident);
               Acc.Field := Current_Subp.Closure.Last_Index;

               return;
            end if;
         end;

         Current_Subp := Current_Subp.Parent;
         Depth_Delta := Depth_Delta + 1;
      end loop;

      Through_S_Link := False;
   end Get_Access;

end GNATLLVM.Nested_Subps;
