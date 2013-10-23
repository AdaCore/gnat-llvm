with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

with Types; use Types;

package GNATLLVM.Nested_Subps is

   --  Because of subprograms nesting, the access to local variables has to be
   --  done through the "local link": a linked list of "static links" records
   --  that contain escaping local variables.

   --  This package is dedicated to the handling of such local variables.

   package Defining_Identifier_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Entity_Id);

   package Defining_Identifier_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Entity_Id);

   type Access_Record is record
      Depth : Natural;
      --  Number of "parent" static links to skip

      Field : Positive;
      --  Index of the variable inside the static link structure
   end record;
   --  Describes how to access a variable through the static link

   package Local_Access_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Entity_Id,
      Element_Type => Access_Record);

   type Static_Link_Descriptor_Record;
   type Static_Link_Descriptor is access Static_Link_Descriptor_Record;

   type Static_Link_Descriptor_Record is record
      Parent     : Static_Link_Descriptor;
      --  Previous static link descriptor. Null for the first static link
      --  descriptor of a top-level subprogram.

      Def_Idents : Defining_Identifier_Sets.Set;
      --  Set of entities that are declared in the corresponding subprogram,
      --  excluding the ones declared in nested subprograms.

      Closure    : Defining_Identifier_Vectors.Vector;
      --  Sequence containing a subset of Def_Idents that are referenced in
      --  nested subprograms. Defines as-is the structure containing escaping
      --  locals in a subprogram.

      Accesses   : Local_Access_Maps.Map;
      --  For each "foreign" defining identifier referenced in this subprogram,
      --  associate enough information to know how to access it starting from
      --  this subprogram stack frame.
   end record;
   --  Information associated to each subprogram

   package Static_Link_Descriptor_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Entity_Id,
      Element_Type => Static_Link_Descriptor);

   procedure Compute_Static_Link_Descriptors
     (GNAT_Root : Node_Id;
      S_Links   : in out Static_Link_Descriptor_Maps.Map);
   --  Traverse the whole compilation unit tree and build static link
   --  descriptors for each subprogram.

end GNATLLVM.Nested_Subps;
