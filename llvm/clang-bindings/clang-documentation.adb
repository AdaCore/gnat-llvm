pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body Clang.Documentation is

   function Comment_Is_Whitespace
     (Comment : Comment_T)
      return Boolean
   is
   begin
      return (if Comment_Is_Whitespace_C (Comment) = 0 then False else True);
   end Comment_Is_Whitespace;

   function HTML_Start_Tag_Comment_Is_Self_Closing
     (Comment : Comment_T)
      return Boolean
   is
   begin
      return (if HTML_Start_Tag_Comment_Is_Self_Closing_C (Comment) = 0 then False else True);
   end HTML_Start_Tag_Comment_Is_Self_Closing;

   function Param_Command_Comment_Is_Param_Index_Valid
     (Comment : Comment_T)
      return Boolean
   is
   begin
      return (if Param_Command_Comment_Is_Param_Index_Valid_C (Comment) = 0 then False else True);
   end Param_Command_Comment_Is_Param_Index_Valid;

   function Param_Command_Comment_Is_Direction_Explicit
     (Comment : Comment_T)
      return Boolean
   is
   begin
      return (if Param_Command_Comment_Is_Direction_Explicit_C (Comment) = 0 then False else True);
   end Param_Command_Comment_Is_Direction_Explicit;

   function T_Param_Command_Comment_Is_Param_Position_Valid
     (Comment : Comment_T)
      return Boolean
   is
   begin
      return (if T_Param_Command_Comment_Is_Param_Position_Valid_C (Comment) = 0 then False else True);
   end T_Param_Command_Comment_Is_Param_Position_Valid;

end Clang.Documentation;
