with Types; use Types;

with LLVM.Core; use LLVM.Core;

with GNATLLVM.Environment; use GNATLLVM.Environment;

package GNATLLVM.Types is

   procedure Register_Builtin_Types (Env : Environ);

   function Create_Subprogram_Type
     (Env : Environ; Subp_Spec : Node_Id) return Type_T;

   function Create_Type (Env : Environ; Type_Node : Node_Id) return Type_T;

   procedure Create_Discrete_Type
     (Env       : Environ;
      TE        : Entity_Id;
      TL        : out Type_T;
      Low, High : out Value_T);
end GNATLLVM.Types;
