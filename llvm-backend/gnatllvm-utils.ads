with Types; use Types;
with LLVM.Core; use LLVM.Core;
with Atree; use Atree;
with Sinfo; use Sinfo;

package GNATLLVM.Utils is

   type Comp_Pred is
     (EQ, NE, LT, LE, GT, GE);

   type Pred_Mapping is record
      Signed : Int_Predicate_T;
      Unsigned : Int_Predicate_T;
      Real : Real_Predicate_T;
   end record;

   function Get_Preds (N : Node_Id) return Pred_Mapping is
     (case Nkind (N) is
         when N_Op_Eq => (Int_EQ, Int_EQ, Real_OEQ),
         when N_Op_Ne => (Int_NE, Int_NE, Real_ONE),
         when N_Op_Lt => (Int_SLT, Int_ULT, Real_OLT),
         when N_Op_Le => (Int_SLE, Int_ULE, Real_OLE),
         when N_Op_Gt => (Int_SGT, Int_UGT, Real_OGT),
         when N_Op_Ge => (Int_SGE, Int_UGE, Real_OGE),
         when others => (others => <>));

   type List_Iterator is array (Nat range <>) of Node_Id;

   function Iterate (L : List_Id) return List_Iterator;
   --  Return an iterator on list L

   function Get_Name (E : Entity_Id) return String;
   --  Return the name of an entity: Get_Name_String (Chars (E))

   procedure Discard (V : Value_T);

   function Is_Binary_Operator (Node : Node_Id) return Boolean;

   procedure Dump_LLVM_Value (V : Value_T);
   --  Simple wrapper around LLVM.Core.Dump_Value. Gives an Ada name to this
   --  function that is usable in debugging sessions.

end GNATLLVM.Utils;
