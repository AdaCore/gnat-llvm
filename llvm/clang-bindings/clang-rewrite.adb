pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body Clang.Rewrite is

   procedure CX_Rewriter_Insert_Text_Before
     (Rew    : Rewriter_T;
      Loc    : Clang.CX_Source_Location.Source_Location_T;
      Insert : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "clang_CXRewriter_insertTextBefore";
   procedure CX_Rewriter_Insert_Text_Before
     (Rew    : Rewriter_T;
      Loc    : Clang.CX_Source_Location.Source_Location_T;
      Insert : String)
   is
      Insert_Array  : aliased char_array := To_C (Insert);
      Insert_String : constant chars_ptr := To_Chars_Ptr (Insert_Array'Unchecked_Access);
   begin
      CX_Rewriter_Insert_Text_Before (Rew, Loc, Insert_String);
   end CX_Rewriter_Insert_Text_Before;

   procedure CX_Rewriter_Replace_Text
     (Rew            : Rewriter_T;
      To_Be_Replaced : Clang.CX_Source_Location.Source_Range_T;
      Replacement    : Interfaces.C.Strings.chars_ptr)
   with Import => True,
        Convention => C,
        External_Name => "clang_CXRewriter_replaceText";
   procedure CX_Rewriter_Replace_Text
     (Rew            : Rewriter_T;
      To_Be_Replaced : Clang.CX_Source_Location.Source_Range_T;
      Replacement    : String)
   is
      Replacement_Array  : aliased char_array := To_C (Replacement);
      Replacement_String : constant chars_ptr := To_Chars_Ptr (Replacement_Array'Unchecked_Access);
   begin
      CX_Rewriter_Replace_Text (Rew, To_Be_Replaced, Replacement_String);
   end CX_Rewriter_Replace_Text;

end Clang.Rewrite;
