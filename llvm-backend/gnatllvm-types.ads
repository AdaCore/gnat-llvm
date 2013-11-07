with Types; use Types;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Environment; use GNATLLVM.Environment;
with Atree; use Atree;
with Einfo; use Einfo;

package GNATLLVM.Types is

   type Type_Array is array (Nat range <>) of Type_T;

   procedure Register_Builtin_Types (Env : Environ);

   function Create_Access_Type
     (Env : Environ; Type_Node : Node_Id) return Type_T;
   --  Function that creates the access type for a corresponding type. Since
   --  access types are not just pointers, this is the abstraction bridge
   --  between the two. For the moment, it handles array accesses and thin
   --  (normal) accesses.

   function Array_Bounds_Type return Type_T;
   --  Helper that returns the type of array bounds (always the int type big
   --  enough to address the whole address space atm)

   function Array_Bounds_Array_Type (Nb_Dims : Nat) return Type_T;
   --  Return an array type big enough to contain the bounds for Nb_Dims dims

   function Create_Subprogram_Type
     (Env : Environ; Subp_Spec : Node_Id) return Type_T;

   function Create_Type (Env : Environ; Type_Node : Node_Id) return Type_T;

   procedure Create_Discrete_Type
     (Env       : Environ;
      TE        : Entity_Id;
      TL        : out Type_T;
      Low, High : out Value_T)
     with Pre => Ekind (TE) in Discrete_Kind;

   function Int_Ty (Num_Bits : Natural) return Type_T;
   function Fn_Ty (Param_Ty : Type_Array; Ret_Ty : Type_T) return Type_T;

   function Get_Innermost_Component_Type
     (Env : Environ; N : Entity_Id) return Type_T;

end GNATLLVM.Types;
