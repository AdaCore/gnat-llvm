pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body Clang.Documentation is

   function Comment_Is_Whitespace
     (Comment : Comment_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_Comment_isWhitespace";
   function Comment_Is_Whitespace
     (Comment : Comment_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Comment_Is_Whitespace (Comment);
      return Return_Value /= 0;
   end Comment_Is_Whitespace;

   function Text_Comment_Get_Text
     (Comment : Comment_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_TextComment_getText";
   function Text_Comment_Get_Text
     (Comment : Comment_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Text_Comment_Get_Text (Comment);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Text_Comment_Get_Text;

   function Inline_Command_Comment_Get_Command_Name
     (Comment : Comment_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_InlineCommandComment_getCommandName";
   function Inline_Command_Comment_Get_Command_Name
     (Comment : Comment_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Inline_Command_Comment_Get_Command_Name (Comment);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Inline_Command_Comment_Get_Command_Name;

   function Inline_Command_Comment_Get_Arg_Text
     (Comment : Comment_T;
      Arg_Idx : unsigned)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_InlineCommandComment_getArgText";
   function Inline_Command_Comment_Get_Arg_Text
     (Comment : Comment_T;
      Arg_Idx : unsigned)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Inline_Command_Comment_Get_Arg_Text (Comment, Arg_Idx);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Inline_Command_Comment_Get_Arg_Text;

   function HTML_Tag_Comment_Get_Tag_Name
     (Comment : Comment_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_HTMLTagComment_getTagName";
   function HTML_Tag_Comment_Get_Tag_Name
     (Comment : Comment_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := HTML_Tag_Comment_Get_Tag_Name (Comment);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end HTML_Tag_Comment_Get_Tag_Name;

   function HTML_Start_Tag_Comment_Is_Self_Closing
     (Comment : Comment_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_HTMLStartTagComment_isSelfClosing";
   function HTML_Start_Tag_Comment_Is_Self_Closing
     (Comment : Comment_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := HTML_Start_Tag_Comment_Is_Self_Closing (Comment);
      return Return_Value /= 0;
   end HTML_Start_Tag_Comment_Is_Self_Closing;

   function HTML_Start_Tag_Get_Attr_Name
     (Comment  : Comment_T;
      Attr_Idx : unsigned)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_HTMLStartTag_getAttrName";
   function HTML_Start_Tag_Get_Attr_Name
     (Comment  : Comment_T;
      Attr_Idx : unsigned)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := HTML_Start_Tag_Get_Attr_Name (Comment, Attr_Idx);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end HTML_Start_Tag_Get_Attr_Name;

   function HTML_Start_Tag_Get_Attr_Value
     (Comment  : Comment_T;
      Attr_Idx : unsigned)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_HTMLStartTag_getAttrValue";
   function HTML_Start_Tag_Get_Attr_Value
     (Comment  : Comment_T;
      Attr_Idx : unsigned)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := HTML_Start_Tag_Get_Attr_Value (Comment, Attr_Idx);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end HTML_Start_Tag_Get_Attr_Value;

   function Block_Command_Comment_Get_Command_Name
     (Comment : Comment_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_BlockCommandComment_getCommandName";
   function Block_Command_Comment_Get_Command_Name
     (Comment : Comment_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Block_Command_Comment_Get_Command_Name (Comment);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Block_Command_Comment_Get_Command_Name;

   function Block_Command_Comment_Get_Arg_Text
     (Comment : Comment_T;
      Arg_Idx : unsigned)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_BlockCommandComment_getArgText";
   function Block_Command_Comment_Get_Arg_Text
     (Comment : Comment_T;
      Arg_Idx : unsigned)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Block_Command_Comment_Get_Arg_Text (Comment, Arg_Idx);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Block_Command_Comment_Get_Arg_Text;

   function Param_Command_Comment_Get_Param_Name
     (Comment : Comment_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_ParamCommandComment_getParamName";
   function Param_Command_Comment_Get_Param_Name
     (Comment : Comment_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Param_Command_Comment_Get_Param_Name (Comment);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Param_Command_Comment_Get_Param_Name;

   function Param_Command_Comment_Is_Param_Index_Valid
     (Comment : Comment_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_ParamCommandComment_isParamIndexValid";
   function Param_Command_Comment_Is_Param_Index_Valid
     (Comment : Comment_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Param_Command_Comment_Is_Param_Index_Valid (Comment);
      return Return_Value /= 0;
   end Param_Command_Comment_Is_Param_Index_Valid;

   function Param_Command_Comment_Is_Direction_Explicit
     (Comment : Comment_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_ParamCommandComment_isDirectionExplicit";
   function Param_Command_Comment_Is_Direction_Explicit
     (Comment : Comment_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := Param_Command_Comment_Is_Direction_Explicit (Comment);
      return Return_Value /= 0;
   end Param_Command_Comment_Is_Direction_Explicit;

   function T_Param_Command_Comment_Get_Param_Name
     (Comment : Comment_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_TParamCommandComment_getParamName";
   function T_Param_Command_Comment_Get_Param_Name
     (Comment : Comment_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := T_Param_Command_Comment_Get_Param_Name (Comment);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end T_Param_Command_Comment_Get_Param_Name;

   function T_Param_Command_Comment_Is_Param_Position_Valid
     (Comment : Comment_T)
      return unsigned
   with Import => True,
        Convention => C,
        External_Name => "clang_TParamCommandComment_isParamPositionValid";
   function T_Param_Command_Comment_Is_Param_Position_Valid
     (Comment : Comment_T)
      return Boolean
   is
      Return_Value : unsigned;
   begin
      Return_Value := T_Param_Command_Comment_Is_Param_Position_Valid (Comment);
      return Return_Value /= 0;
   end T_Param_Command_Comment_Is_Param_Position_Valid;

   function Verbatim_Block_Line_Comment_Get_Text
     (Comment : Comment_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_VerbatimBlockLineComment_getText";
   function Verbatim_Block_Line_Comment_Get_Text
     (Comment : Comment_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Verbatim_Block_Line_Comment_Get_Text (Comment);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Verbatim_Block_Line_Comment_Get_Text;

   function Verbatim_Line_Comment_Get_Text
     (Comment : Comment_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_VerbatimLineComment_getText";
   function Verbatim_Line_Comment_Get_Text
     (Comment : Comment_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Verbatim_Line_Comment_Get_Text (Comment);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Verbatim_Line_Comment_Get_Text;

   function HTML_Tag_Comment_Get_As_String
     (Comment : Comment_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_HTMLTagComment_getAsString";
   function HTML_Tag_Comment_Get_As_String
     (Comment : Comment_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := HTML_Tag_Comment_Get_As_String (Comment);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end HTML_Tag_Comment_Get_As_String;

   function Full_Comment_Get_As_HTML
     (Comment : Comment_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_FullComment_getAsHTML";
   function Full_Comment_Get_As_HTML
     (Comment : Comment_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Full_Comment_Get_As_HTML (Comment);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Full_Comment_Get_As_HTML;

   function Full_Comment_Get_As_XML
     (Comment : Comment_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_FullComment_getAsXML";
   function Full_Comment_Get_As_XML
     (Comment : Comment_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Full_Comment_Get_As_XML (Comment);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Full_Comment_Get_As_XML;

   function Get_Symbol_Graph_For_USR
     (Usr : Interfaces.C.Strings.chars_ptr;
      Api : API_Set_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getSymbolGraphForUSR";
   function Get_Symbol_Graph_For_USR
     (Usr : String;
      Api : API_Set_T)
      return Clang.CX_String.String_T
   is
      Return_Value : Clang.CX_String.String_T;
      Usr_Array    : aliased char_array := To_C (Usr);
      Usr_String   : constant chars_ptr := To_Chars_Ptr (Usr_Array'Unchecked_Access);
   begin
      Return_Value := Get_Symbol_Graph_For_USR (Usr_String, Api);
      return Return_Value;
   end Get_Symbol_Graph_For_USR;

   function Get_Symbol_Graph_For_USR
     (Usr : String;
      Api : API_Set_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Symbol_Graph_For_USR (Usr, Api);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Symbol_Graph_For_USR;

   function Get_Symbol_Graph_For_Cursor
     (Cursor : Clang.Index.Cursor_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_getSymbolGraphForCursor";
   function Get_Symbol_Graph_For_Cursor
     (Cursor : Clang.Index.Cursor_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Get_Symbol_Graph_For_Cursor (Cursor);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Get_Symbol_Graph_For_Cursor;

end Clang.Documentation;
