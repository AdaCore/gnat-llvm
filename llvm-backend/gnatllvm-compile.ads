with Types; use Types;
with GNATLLVM.Environment; use GNATLLVM.Environment;
with LLVM.Core; use LLVM.Core;

package GNATLLVM.Compile is

   procedure Compile
     (Env : Environ; Node : Node_Id)
     with Pre => Env /= null;
   --  General compilation routine, called at the top-level.

   function Compile_Expression
     (Env : Environ; Node : Node_Id) return Value_T
     with Pre => Env /= null;
   --  Compile an expression node to an LLVM value.

   function Compile_LValue (Env : Environ; Node : Node_Id) return Value_T
     with Pre => Env /= null;
   --  Compile an expression node to an LLVM value that can be used as an
   --  LValue. This function can be used to get a pointer to a value rather
   --  than the value itself (out parameters, simple accesses, etc.)

end GNATLLVM.Compile;
