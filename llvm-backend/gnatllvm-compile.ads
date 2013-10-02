with Types; use Types;

with GNATLLVM.Environment; use GNATLLVM.Environment;

package GNATLLVM.Compile is

   procedure Compile
     (Env : Environ; Node : Node_Id);

end GNATLLVM.Compile;
