with Types; use Types;
with Utils;

package GNATLLVM.Compile is
   procedure Compile
     (C : Utils.Context; Node : Node_Id);

   procedure Compile_Declarations
     (C : Utils.Context; Declarations : List_Id);
end GNATLLVM.Compile;
