with Types; use Types;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with LLVM.Core; use LLVM.Core;

package GNATLLVM.Compile is

   procedure Compile
     (Env : Environ; Node : Node_Id);

   function Compile_Expression
     (Env : Environ; Node : Node_Id; Is_LValue : Boolean) return Value_T;

end GNATLLVM.Compile;
