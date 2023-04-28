pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;
with Clang.Index;
with Clang.CX_String;
with Clang.CX_Error_Code;
with Interfaces.C.Strings;

package Clang.Documentation is

  --==-- clang-c/Documentation.h - Utilities for comment processing -*- C -*-===*|*                                                                            *|
  --|
  --|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
  --|* Exceptions.                                                                *|
  --|* See https://llvm.org/LICENSE.txt for license information.                  *|
  --|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header provides a supplementary interface for inspecting              *|
  --|* documentation comments.                                                    *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------=== 

  --*
  -- * \defgroup CINDEX_COMMENT Comment introspection
  -- *
  -- * The routines in this group provide access to information in documentation
  -- * comments. These facilities are distinct from the core and may be subject to
  -- * their own schedule of stability and deprecation.
  -- *
  -- * @{
  --  

  --*
  -- * A parsed comment.
  --  

   type Comment_T is record
      ASTNode : System.Address;  -- install/include/clang-c/Documentation.h:38
      TranslationUnit : Clang.Index.Translation_Unit_T;  -- install/include/clang-c/Documentation.h:39
   end record
   with Convention => C_Pass_By_Copy;  -- install/include/clang-c/Documentation.h:40

  --*
  -- * Given a cursor that represents a documentable entity (e.g.,
  -- * declaration), return the associated parsed comment as a
  -- * \c CXComment_FullComment AST node.
  --  

   function Cursor_Get_Parsed_Comment (C : Clang.Index.Cursor_T) return Comment_T  -- install/include/clang-c/Documentation.h:47
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getParsedComment";

  --*
  -- * Describes the type of the comment AST node (\c CXComment).  A comment
  -- * node can be considered block content (e. g., paragraph), inline content
  -- * (plain text) or neither (the root AST node).
  --  

   type Comment_Kind_T is 
     (Comment_Null,
      Comment_Text,
      Comment_Inline_Command,
      Comment_HTML_Start_Tag,
      Comment_HTML_End_Tag,
      Comment_Paragraph,
      Comment_Block_Command,
      Comment_Param_Command,
      Comment_T_Param_Command,
      Comment_Verbatim_Block_Command,
      Comment_Verbatim_Block_Line,
      Comment_Verbatim_Line,
      Comment_Full_Comment)
   with Convention => C;  -- install/include/clang-c/Documentation.h:54

  --*
  --   * Null comment.  No AST node is constructed at the requested location
  --   * because there is no text or a syntax error.
  --    

  --*
  --   * Plain text.  Inline content.
  --    

  --*
  --   * A command with word-like arguments that is considered inline content.
  --   *
  --   * For example: \\c command.
  --    

  --*
  --   * HTML start tag with attributes (name-value pairs).  Considered
  --   * inline content.
  --   *
  --   * For example:
  --   * \verbatim
  --   * <br> <br /> <a href="http://example.org/">
  --   * \endverbatim
  --    

  --*
  --   * HTML end tag.  Considered inline content.
  --   *
  --   * For example:
  --   * \verbatim
  --   * </a>
  --   * \endverbatim
  --    

  --*
  --   * A paragraph, contains inline comment.  The paragraph itself is
  --   * block content.
  --    

  --*
  --   * A command that has zero or more word-like arguments (number of
  --   * word-like arguments depends on command name) and a paragraph as an
  --   * argument.  Block command is block content.
  --   *
  --   * Paragraph argument is also a child of the block command.
  --   *
  --   * For example: \has 0 word-like arguments and a paragraph argument.
  --   *
  --   * AST nodes of special kinds that parser knows about (e. g., \\param
  --   * command) have their own node kinds.
  --    

  --*
  --   * A \\param or \\arg command that describes the function parameter
  --   * (name, passing direction, description).
  --   *
  --   * For example: \\param [in] ParamName description.
  --    

  --*
  --   * A \\tparam command that describes a template parameter (name and
  --   * description).
  --   *
  --   * For example: \\tparam T description.
  --    

  --*
  --   * A verbatim block command (e. g., preformatted code).  Verbatim
  --   * block has an opening and a closing command and contains multiple lines of
  --   * text (\c CXComment_VerbatimBlockLine child nodes).
  --   *
  --   * For example:
  --   * \\verbatim
  --   * aaa
  --   * \\endverbatim
  --    

  --*
  --   * A line of text that is contained within a
  --   * CXComment_VerbatimBlockCommand node.
  --    

  --*
  --   * A verbatim line command.  Verbatim line has an opening command,
  --   * a single line of text (up to the newline after the opening command) and
  --   * has no closing command.
  --    

  --*
  --   * A full comment attached to a declaration, contains block content.
  --    

  --*
  -- * The most appropriate rendering mode for an inline command, chosen on
  -- * command semantics in Doxygen.
  --  

   type Comment_Inline_Command_Render_Kind_T is 
     (Comment_Inline_Command_Render_Kind_Normal,
      Comment_Inline_Command_Render_Kind_Bold,
      Comment_Inline_Command_Render_Kind_Monospaced,
      Comment_Inline_Command_Render_Kind_Emphasized,
      Comment_Inline_Command_Render_Kind_Anchor)
   with Convention => C;  -- install/include/clang-c/Documentation.h:165

  --*
  --   * Command argument should be rendered in a normal font.
  --    

  --*
  --   * Command argument should be rendered in a bold font.
  --    

  --*
  --   * Command argument should be rendered in a monospaced font.
  --    

  --*
  --   * Command argument should be rendered emphasized (typically italic
  --   * font).
  --    

  --*
  --   * Command argument should not be rendered (since it only defines an anchor).
  --    

  --*
  -- * Describes parameter passing direction for \\param or \\arg command.
  --  

   type Comment_Param_Pass_Direction_T is 
     (Comment_Param_Pass_Direction_In,
      Comment_Param_Pass_Direction_Out,
      Comment_Param_Pass_Direction_In_Out)
   with Convention => C;  -- install/include/clang-c/Documentation.h:196

  --*
  --   * The parameter is an input parameter.
  --    

  --*
  --   * The parameter is an output parameter.
  --    

  --*
  --   * The parameter is an input and output parameter.
  --    

  --*
  -- * \param Comment AST node of any kind.
  -- *
  -- * \returns the type of the AST node.
  --  

   function Comment_Get_Kind (Comment : Comment_T) return Comment_Kind_T  -- install/include/clang-c/Documentation.h:218
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Comment_getKind";

  --*
  -- * \param Comment AST node of any kind.
  -- *
  -- * \returns number of children of the AST node.
  --  

   function Comment_Get_Num_Children (Comment : Comment_T) return unsigned  -- install/include/clang-c/Documentation.h:225
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Comment_getNumChildren";

  --*
  -- * \param Comment AST node of any kind.
  -- *
  -- * \param ChildIdx child index (zero-based).
  -- *
  -- * \returns the specified child of the AST node.
  --  

   function Comment_Get_Child (Comment : Comment_T; Child_Idx : unsigned) return Comment_T  -- install/include/clang-c/Documentation.h:235
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Comment_getChild";

  --*
  -- * A \c CXComment_Paragraph node is considered whitespace if it contains
  -- * only \c CXComment_Text nodes that are empty or whitespace.
  -- *
  -- * Other AST nodes (except \c CXComment_Paragraph and \c CXComment_Text) are
  -- * never considered whitespace.
  -- *
  -- * \returns non-zero if \c Comment is whitespace.
  --  

function Comment_Is_Whitespace
     (Comment : Comment_T)
      return Boolean;

  --*
  -- * \returns non-zero if \c Comment is inline content and has a newline
  -- * immediately following it in the comment text.  Newlines between paragraphs
  -- * do not count.
  --  

   function Inline_Content_Comment_Has_Trailing_Newline (Comment : Comment_T) return unsigned  -- install/include/clang-c/Documentation.h:254
   with Import => True, 
        Convention => C, 
        External_Name => "clang_InlineContentComment_hasTrailingNewline";

  --*
  -- * \param Comment a \c CXComment_Text AST node.
  -- *
  -- * \returns text contained in the AST node.
  --  

function Text_Comment_Get_Text
     (Comment : Comment_T)
      return String;

  --*
  -- * \param Comment a \c CXComment_InlineCommand AST node.
  -- *
  -- * \returns name of the inline command.
  --  

function Inline_Command_Comment_Get_Command_Name
     (Comment : Comment_T)
      return String;

  --*
  -- * \param Comment a \c CXComment_InlineCommand AST node.
  -- *
  -- * \returns the most appropriate rendering mode, chosen on command
  -- * semantics in Doxygen.
  --  

   function Inline_Command_Comment_Get_Render_Kind (Comment : Comment_T) return Comment_Inline_Command_Render_Kind_T  -- install/include/clang-c/Documentation.h:278
   with Import => True, 
        Convention => C, 
        External_Name => "clang_InlineCommandComment_getRenderKind";

  --*
  -- * \param Comment a \c CXComment_InlineCommand AST node.
  -- *
  -- * \returns number of command arguments.
  --  

   function Inline_Command_Comment_Get_Num_Args (Comment : Comment_T) return unsigned  -- install/include/clang-c/Documentation.h:286
   with Import => True, 
        Convention => C, 
        External_Name => "clang_InlineCommandComment_getNumArgs";

  --*
  -- * \param Comment a \c CXComment_InlineCommand AST node.
  -- *
  -- * \param ArgIdx argument index (zero-based).
  -- *
  -- * \returns text of the specified argument.
  --  

function Inline_Command_Comment_Get_Arg_Text
     (Comment : Comment_T;
      Arg_Idx : unsigned)
      return String;

  --*
  -- * \param Comment a \c CXComment_HTMLStartTag or \c CXComment_HTMLEndTag AST
  -- * node.
  -- *
  -- * \returns HTML tag name.
  --  

function HTML_Tag_Comment_Get_Tag_Name
     (Comment : Comment_T)
      return String;

  --*
  -- * \param Comment a \c CXComment_HTMLStartTag AST node.
  -- *
  -- * \returns non-zero if tag is self-closing (for example, &lt;br /&gt;).
  --  

function HTML_Start_Tag_Comment_Is_Self_Closing
     (Comment : Comment_T)
      return Boolean;

  --*
  -- * \param Comment a \c CXComment_HTMLStartTag AST node.
  -- *
  -- * \returns number of attributes (name-value pairs) attached to the start tag.
  --  

   function HTML_Start_Tag_Get_Num_Attrs (Comment : Comment_T) return unsigned  -- install/include/clang-c/Documentation.h:320
   with Import => True, 
        Convention => C, 
        External_Name => "clang_HTMLStartTag_getNumAttrs";

  --*
  -- * \param Comment a \c CXComment_HTMLStartTag AST node.
  -- *
  -- * \param AttrIdx attribute index (zero-based).
  -- *
  -- * \returns name of the specified attribute.
  --  

function HTML_Start_Tag_Get_Attr_Name
     (Comment  : Comment_T;
      Attr_Idx : unsigned)
      return String;

  --*
  -- * \param Comment a \c CXComment_HTMLStartTag AST node.
  -- *
  -- * \param AttrIdx attribute index (zero-based).
  -- *
  -- * \returns value of the specified attribute.
  --  

function HTML_Start_Tag_Get_Attr_Value
     (Comment  : Comment_T;
      Attr_Idx : unsigned)
      return String;

  --*
  -- * \param Comment a \c CXComment_BlockCommand AST node.
  -- *
  -- * \returns name of the block command.
  --  

function Block_Command_Comment_Get_Command_Name
     (Comment : Comment_T)
      return String;

  --*
  -- * \param Comment a \c CXComment_BlockCommand AST node.
  -- *
  -- * \returns number of word-like arguments.
  --  

   function Block_Command_Comment_Get_Num_Args (Comment : Comment_T) return unsigned  -- install/include/clang-c/Documentation.h:356
   with Import => True, 
        Convention => C, 
        External_Name => "clang_BlockCommandComment_getNumArgs";

  --*
  -- * \param Comment a \c CXComment_BlockCommand AST node.
  -- *
  -- * \param ArgIdx argument index (zero-based).
  -- *
  -- * \returns text of the specified word-like argument.
  --  

function Block_Command_Comment_Get_Arg_Text
     (Comment : Comment_T;
      Arg_Idx : unsigned)
      return String;

  --*
  -- * \param Comment a \c CXComment_BlockCommand or
  -- * \c CXComment_VerbatimBlockCommand AST node.
  -- *
  -- * \returns paragraph argument of the block command.
  --  

   function Block_Command_Comment_Get_Paragraph (Comment : Comment_T) return Comment_T  -- install/include/clang-c/Documentation.h:376
   with Import => True, 
        Convention => C, 
        External_Name => "clang_BlockCommandComment_getParagraph";

  --*
  -- * \param Comment a \c CXComment_ParamCommand AST node.
  -- *
  -- * \returns parameter name.
  --  

function Param_Command_Comment_Get_Param_Name
     (Comment : Comment_T)
      return String;

  --*
  -- * \param Comment a \c CXComment_ParamCommand AST node.
  -- *
  -- * \returns non-zero if the parameter that this AST node represents was found
  -- * in the function prototype and \c clang_ParamCommandComment_getParamIndex
  -- * function will return a meaningful value.
  --  

function Param_Command_Comment_Is_Param_Index_Valid
     (Comment : Comment_T)
      return Boolean;

  --*
  -- * \param Comment a \c CXComment_ParamCommand AST node.
  -- *
  -- * \returns zero-based parameter index in function prototype.
  --  

   function Param_Command_Comment_Get_Param_Index (Comment : Comment_T) return unsigned  -- install/include/clang-c/Documentation.h:402
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ParamCommandComment_getParamIndex";

  --*
  -- * \param Comment a \c CXComment_ParamCommand AST node.
  -- *
  -- * \returns non-zero if parameter passing direction was specified explicitly in
  -- * the comment.
  --  

function Param_Command_Comment_Is_Direction_Explicit
     (Comment : Comment_T)
      return Boolean;

  --*
  -- * \param Comment a \c CXComment_ParamCommand AST node.
  -- *
  -- * \returns parameter passing direction.
  --  

   function Param_Command_Comment_Get_Direction (Comment : Comment_T) return Comment_Param_Pass_Direction_T  -- install/include/clang-c/Documentation.h:419
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ParamCommandComment_getDirection";

  --*
  -- * \param Comment a \c CXComment_TParamCommand AST node.
  -- *
  -- * \returns template parameter name.
  --  

function T_Param_Command_Comment_Get_Param_Name
     (Comment : Comment_T)
      return String;

  --*
  -- * \param Comment a \c CXComment_TParamCommand AST node.
  -- *
  -- * \returns non-zero if the parameter that this AST node represents was found
  -- * in the template parameter list and
  -- * \c clang_TParamCommandComment_getDepth and
  -- * \c clang_TParamCommandComment_getIndex functions will return a meaningful
  -- * value.
  --  

function T_Param_Command_Comment_Is_Param_Position_Valid
     (Comment : Comment_T)
      return Boolean;

  --*
  -- * \param Comment a \c CXComment_TParamCommand AST node.
  -- *
  -- * \returns zero-based nesting depth of this parameter in the template parameter list.
  -- *
  -- * For example,
  -- * \verbatim
  -- *     template<typename C, template<typename T> class TT>
  -- *     void test(TT<int> aaa);
  -- * \endverbatim
  -- * for C and TT nesting depth is 0,
  -- * for T nesting depth is 1.
  --  

   function T_Param_Command_Comment_Get_Depth (Comment : Comment_T) return unsigned  -- install/include/clang-c/Documentation.h:456
   with Import => True, 
        Convention => C, 
        External_Name => "clang_TParamCommandComment_getDepth";

  --*
  -- * \param Comment a \c CXComment_TParamCommand AST node.
  -- *
  -- * \returns zero-based parameter index in the template parameter list at a
  -- * given nesting depth.
  -- *
  -- * For example,
  -- * \verbatim
  -- *     template<typename C, template<typename T> class TT>
  -- *     void test(TT<int> aaa);
  -- * \endverbatim
  -- * for C and TT nesting depth is 0, so we can ask for index at depth 0:
  -- * at depth 0 C's index is 0, TT's index is 1.
  -- *
  -- * For T nesting depth is 1, so we can ask for index at depth 0 and 1:
  -- * at depth 0 T's index is 1 (same as TT's),
  -- * at depth 1 T's index is 0.
  --  

   function T_Param_Command_Comment_Get_Index (Comment : Comment_T; Depth : unsigned) return unsigned  -- install/include/clang-c/Documentation.h:477
   with Import => True, 
        Convention => C, 
        External_Name => "clang_TParamCommandComment_getIndex";

  --*
  -- * \param Comment a \c CXComment_VerbatimBlockLine AST node.
  -- *
  -- * \returns text contained in the AST node.
  --  

function Verbatim_Block_Line_Comment_Get_Text
     (Comment : Comment_T)
      return String;

  --*
  -- * \param Comment a \c CXComment_VerbatimLine AST node.
  -- *
  -- * \returns text contained in the AST node.
  --  

function Verbatim_Line_Comment_Get_Text
     (Comment : Comment_T)
      return String;

  --*
  -- * Convert an HTML tag AST node to string.
  -- *
  -- * \param Comment a \c CXComment_HTMLStartTag or \c CXComment_HTMLEndTag AST
  -- * node.
  -- *
  -- * \returns string containing an HTML tag.
  --  

function HTML_Tag_Comment_Get_As_String
     (Comment : Comment_T)
      return String;

  --*
  -- * Convert a given full parsed comment to an HTML fragment.
  -- *
  -- * Specific details of HTML layout are subject to change.  Don't try to parse
  -- * this HTML back into an AST, use other APIs instead.
  -- *
  -- * Currently the following CSS classes are used:
  -- * \li "para-brief" for \paragraph and equivalent commands;
  -- * \li "para-returns" for \\returns paragraph and equivalent commands;
  -- * \li "word-returns" for the "Returns" word in \\returns paragraph.
  -- *
  -- * Function argument documentation is rendered as a \<dl\> list with arguments
  -- * sorted in function prototype order.  CSS classes used:
  -- * \li "param-name-index-NUMBER" for parameter name (\<dt\>);
  -- * \li "param-descr-index-NUMBER" for parameter description (\<dd\>);
  -- * \li "param-name-index-invalid" and "param-descr-index-invalid" are used if
  -- * parameter index is invalid.
  -- *
  -- * Template parameter documentation is rendered as a \<dl\> list with
  -- * parameters sorted in template parameter list order.  CSS classes used:
  -- * \li "tparam-name-index-NUMBER" for parameter name (\<dt\>);
  -- * \li "tparam-descr-index-NUMBER" for parameter description (\<dd\>);
  -- * \li "tparam-name-index-other" and "tparam-descr-index-other" are used for
  -- * names inside template template parameters;
  -- * \li "tparam-name-index-invalid" and "tparam-descr-index-invalid" are used if
  -- * parameter position is invalid.
  -- *
  -- * \param Comment a \c CXComment_FullComment AST node.
  -- *
  -- * \returns string containing an HTML fragment.
  --  

function Full_Comment_Get_As_HTML
     (Comment : Comment_T)
      return String;

  --*
  -- * Convert a given full parsed comment to an XML document.
  -- *
  -- * A Relax NG schema for the XML can be found in comment-xml-schema.rng file
  -- * inside clang source tree.
  -- *
  -- * \param Comment a \c CXComment_FullComment AST node.
  -- *
  -- * \returns string containing an XML document.
  --  

function Full_Comment_Get_As_XML
     (Comment : Comment_T)
      return String;

  --*
  -- * CXAPISet is an opaque type that represents a data structure containing all
  -- * the API information for a given translation unit. This can be used for a
  -- * single symbol symbol graph for a given symbol.
  --  

   type API_Set_Impl_T is null record;   -- incomplete struct

   type API_Set_T is access all API_Set_Impl_T;  -- install/include/clang-c/Documentation.h:554

  --*
  -- * Traverses the translation unit to create a \c CXAPISet.
  -- *
  -- * \param tu is the \c CXTranslationUnit to build the \c CXAPISet for.
  -- *
  -- * \param out_api is a pointer to the output of this function. It is needs to be
  -- * disposed of by calling clang_disposeAPISet.
  -- *
  -- * \returns Error code indicating success or failure of the APISet creation.
  --  

   function Create_API_Set (Tu : Clang.Index.Translation_Unit_T; Out_Api : System.Address) return Clang.CX_Error_Code.Error_Code_T  -- install/include/clang-c/Documentation.h:566
   with Import => True, 
        Convention => C, 
        External_Name => "clang_createAPISet";

  --*
  -- * Dispose of an APISet.
  -- *
  -- * The provided \c CXAPISet can not be used after this function is called.
  --  

   procedure Dispose_API_Set (Api : API_Set_T)  -- install/include/clang-c/Documentation.h:574
   with Import => True, 
        Convention => C, 
        External_Name => "clang_disposeAPISet";

  --*
  -- * Generate a single symbol symbol graph for the given USR. Returns a null
  -- * string if the associated symbol can not be found in the provided \c CXAPISet.
  -- *
  -- * The output contains the symbol graph as well as some additional information
  -- * about related symbols.
  -- *
  -- * \param usr is a string containing the USR of the symbol to generate the
  -- * symbol graph for.
  -- *
  -- * \param api the \c CXAPISet to look for the symbol in.
  -- *
  -- * \returns a string containing the serialized symbol graph representation for
  -- * the symbol being queried or a null string if it can not be found in the
  -- * APISet.
  --  

function Get_Symbol_Graph_For_USR
     (Usr : String;
      Api : API_Set_T)
      return String;

  --*
  -- * Generate a single symbol symbol graph for the declaration at the given
  -- * cursor. Returns a null string if the AST node for the cursor isn't a
  -- * declaration.
  -- *
  -- * The output contains the symbol graph as well as some additional information
  -- * about related symbols.
  -- *
  -- * \param cursor the declaration for which to generate the single symbol symbol
  -- * graph.
  -- *
  -- * \returns a string containing the serialized symbol graph representation for
  -- * the symbol being queried or a null string if it can not be found in the
  -- * APISet.
  --  

function Get_Symbol_Graph_For_Cursor
     (Cursor : Clang.Index.Cursor_T)
      return String;

  --*
  -- * @}
  --  

end Clang.Documentation;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
