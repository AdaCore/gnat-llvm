with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Types; use Types;

with LLVM.Core; use LLVM.Core;

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

   type Environ is tagged record
      Ctx    : LLVM.Core.Context_T;
      Bld    : LLVM.Core.Builder_T;
      Mdl    : LLVM.Core.Module_T;
      --  Pure-LLVM environment: LLVM context, instruction builder and current
      --  module.

      Scopes : Scope_Vectors.Vector;
      --  Stack of scopes, to associate LLVM types/values to expansed tree's
      --  entities.
   end record;

   procedure Push_Scope (Env : in out Environ);
   procedure Pop_Scope (Env : in out Environ);

   function Get (Env : Environ; TE : Entity_Id) return Type_T;
   function Get (Env : Environ; VE : Entity_Id) return Value_T;

   procedure Set (Env : Environ; TE : Entity_Id; TL : Type_T);
   procedure Set (Env : Environ; VE : Entity_Id; VL : Value_T);

end GNATLLVM.Environment;
