with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Types; use Types;

with LLVM.Core; use LLVM.Core;
with Ada.Containers.Doubly_Linked_Lists;
with GNATLLVM.Builder;

package GNATLLVM.Environment is

   package Type_Vectors is new Ada.Containers.Vectors (Nat, Type_T);

   type Field_Info is record
      Containing_Struct_Index : Nat;
      Index_In_Struct : Nat;
   end record;

   package Field_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Entity_Id,
      Element_Type => Field_Info);

   type Record_Info is record
      Fields : Field_Maps.Map;
      Structs : Type_Vectors.Vector;
   end record;

   package Record_Info_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Entity_Id,
      Element_Type => Record_Info);

   package Type_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Entity_Id,
      Element_Type => Type_T);

   package Value_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Entity_Id,
      Element_Type => Value_T);

   type Scope_Type is record
      Types         : Type_Maps.Map;
      Values        : Value_Maps.Map;
      --  Types and values registered so far for this scope

      Records_Infos : Record_Info_Maps.Map;
   end record;
   type Scope_Acc is access Scope_Type;

   package Scope_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Scope_Acc);

   type Exit_Point is record
      Label_Entity : Entity_Id;
      Exit_BB      : Basic_Block_T;
   end record;

   package Exit_Point_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Exit_Point);

   --  Expansed Ada-to-LLVM translation context: gathers global information
   type Environ_Record is tagged;
   type Environ is access all Environ_Record;

   type Subp_Env_Record is tagged record
      Env                    : Environ;
      Func                   : Value_T;
      Saved_Builder_Position : Basic_Block_T;
   end record;
   type Subp_Env is access all Subp_Env_Record;

   package Subp_Lists is new Ada.Containers.Doubly_Linked_Lists (Subp_Env);

   type Environ_Record is tagged record
      Ctx                       : LLVM.Core.Context_T;
      Bld                       : GNATLLVM.Builder.Builder;
      Mdl                       : LLVM.Core.Module_T;
      --  Pure-LLVM environment : LLVM context, instruction builder and current
      --  module.

      Scopes                    : Scope_Vectors.Vector;
      --  Stack of scopes, to associate LLVM types/values to expansed tree's
      --  entities.

      Declarations_Level        : Natural;
      --  Keep track of the current nesting level for declarations

      Exit_Points               : Exit_Point_Vectors.Vector;
      --  Stack of scoped loop exit points. Last inserted exit point correspond
      --  to the innermost loop.

      Subprograms               : Subp_Lists.List;
      Current_Subps             : Subp_Lists.List;

      Default_Alloc_Fn          : Value_T;
   end record;

   procedure Push_Scope (Env : access Environ_Record);
   procedure Pop_Scope (Env : access Environ_Record);

   procedure Begin_Declarations (Env : access Environ_Record);
   procedure End_Declarations (Env : access Environ_Record);
   function In_Declarations (Env : access Environ_Record) return Boolean is
     (Env.Declarations_Level > 0);

   No_Such_Type        : exception;
   No_Such_Value       : exception;
   No_Such_Basic_Block : exception;

   function Has_Type
     (Env : access Environ_Record; TE : Entity_Id) return Boolean;
   function Has_Value
     (Env : access Environ_Record; VE : Entity_Id) return Boolean;
   function Get (Env : access Environ_Record; TE : Entity_Id) return Type_T;
   function Get (Env : access Environ_Record; VE : Entity_Id) return Value_T;
   function Get
     (Env : access Environ_Record; BE : Entity_Id) return Basic_Block_T;
   function Get
     (Env : access Environ_Record; RI : Entity_Id) return Record_Info;

   procedure Set
     (Env : access Environ_Record; TE : Entity_Id; RI : Record_Info);
   procedure Set (Env : access Environ_Record; TE : Entity_Id; TL : Type_T);
   procedure Set (Env : access Environ_Record; VE : Entity_Id; VL : Value_T);
   procedure Set
     (Env : access Environ_Record; BE : Entity_Id; BL : Basic_Block_T);

   procedure Push_Loop
     (Env : access Environ_Record; LE : Entity_Id; Exit_Point : Basic_Block_T);
   procedure Pop_Loop (Env : access Environ_Record);
   function Get_Exit_Point
     (Env : access Environ_Record; LE : Entity_Id) return Basic_Block_T;
   function Get_Exit_Point
     (Env : access Environ_Record) return Basic_Block_T;

   function Enter_Subp
     (Env  : access Environ_Record;
      Func : Value_T) return Subp_Env;
   --  Create, push and return a subprogram environment. Also create an entry
   --  basic block for this subprogram and position the builder at its end. To
   --  be used when starting the compilation of a subprogram body.

   procedure Leave_Subp (Env  : access Environ_Record);
   --  Pop and free the most recent subprogram environment. Restore the
   --  previous builder position, if any. To be used when finishing the
   --  compilation of a subprogram body.

   function Create_Basic_Block
     (Env : access Environ_Record; Name : String) return Basic_Block_T;

   function Current_Subp (Env : access Environ_Record) return Subp_Env;

end GNATLLVM.Environment;
