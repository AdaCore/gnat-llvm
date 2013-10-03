with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Types; use Types;

with LLVM.Core; use LLVM.Core;
with Ada.Containers.Doubly_Linked_Lists;

package GNATLLVM.Environment is

   package Type_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Entity_Id,
      Element_Type => Type_T);

   package Value_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Entity_Id,
      Element_Type => Value_T);

   type Scope_Type is record
      Types  : Type_Maps.Map;
      Values : Value_Maps.Map;
   end record;
   type Scope_Acc is access Scope_Type;

   package Scope_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Scope_Acc);

   --  Expansed Ada-to-LLVM translation context: gathers global information
   type Environ_Record is tagged;
   type Environ is access all Environ_Record;

   type Subp_Env_Record is tagged record
      Env           : Environ;
      LLVM_Func     : Value_T;
      Current_Block : Basic_Block_T;
   end record;
   type Subp_Env is access all Subp_Env_Record;

   package Subp_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Subp_Env);

   type Environ_Record is tagged record
      Ctx    : LLVM.Core.Context_T;
      Bld    : LLVM.Core.Builder_T;
      Mdl    : LLVM.Core.Module_T;
      --  Pure-LLVM environment: LLVM context, instruction builder and current
      --  module.

      Scopes : Scope_Vectors.Vector;
      --  Stack of scopes, to associate LLVM types/values to expansed tree's
      --  entities.

      Subprograms : Subp_Lists.List;
   end record;

   procedure Push_Scope (Env : access Environ_Record);
   procedure Pop_Scope (Env : access Environ_Record);

   function Get (Env : access Environ_Record; TE : Entity_Id) return Type_T;
   function Get (Env : access Environ_Record; VE : Entity_Id) return Value_T;

   procedure Set (Env : access Environ_Record; TE : Entity_Id; TL : Type_T);
   procedure Set (Env : access Environ_Record; VE : Entity_Id; VL : Value_T);

   function Create_Subp
     (Env : access Environ_Record;
      Name : String; Typ : Type_T) return Subp_Env;

end GNATLLVM.Environment;
