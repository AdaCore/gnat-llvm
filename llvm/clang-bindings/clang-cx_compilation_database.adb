pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body Clang.CX_Compilation_Database is

   function Compilation_Database_From_Directory
     (Build_Dir  : Interfaces.C.Strings.chars_ptr;
      Error_Code : access Compilation_Database_Error_T)
      return Compilation_Database_T
   with Import => True,
        Convention => C,
        External_Name => "clang_CompilationDatabase_fromDirectory";
   function Compilation_Database_From_Directory
     (Build_Dir  : String;
      Error_Code : access Compilation_Database_Error_T)
      return Compilation_Database_T
   is
      Return_Value     : Compilation_Database_T;
      Build_Dir_Array  : aliased char_array := To_C (Build_Dir);
      Build_Dir_String : constant chars_ptr := To_Chars_Ptr (Build_Dir_Array'Unchecked_Access);
   begin
      Return_Value := Compilation_Database_From_Directory (Build_Dir_String, Error_Code);
      return Return_Value;
   end Compilation_Database_From_Directory;

   function Compilation_Database_Get_Compile_Commands
     (Arg_1              : Compilation_Database_T;
      Complete_File_Name : Interfaces.C.Strings.chars_ptr)
      return Compile_Commands_T
   with Import => True,
        Convention => C,
        External_Name => "clang_CompilationDatabase_getCompileCommands";
   function Compilation_Database_Get_Compile_Commands
     (Arg_1              : Compilation_Database_T;
      Complete_File_Name : String)
      return Compile_Commands_T
   is
      Return_Value              : Compile_Commands_T;
      Complete_File_Name_Array  : aliased char_array := To_C (Complete_File_Name);
      Complete_File_Name_String : constant chars_ptr := To_Chars_Ptr (Complete_File_Name_Array'Unchecked_Access);
   begin
      Return_Value := Compilation_Database_Get_Compile_Commands (Arg_1, Complete_File_Name_String);
      return Return_Value;
   end Compilation_Database_Get_Compile_Commands;

   function Compile_Command_Get_Directory
     (Arg_1 : Compile_Command_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_CompileCommand_getDirectory";
   function Compile_Command_Get_Directory
     (Arg_1 : Compile_Command_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Compile_Command_Get_Directory (Arg_1);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Compile_Command_Get_Directory;

   function Compile_Command_Get_Filename
     (Arg_1 : Compile_Command_T)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_CompileCommand_getFilename";
   function Compile_Command_Get_Filename
     (Arg_1 : Compile_Command_T)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Compile_Command_Get_Filename (Arg_1);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Compile_Command_Get_Filename;

   function Compile_Command_Get_Arg
     (Arg_1 : Compile_Command_T;
      I     : unsigned)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_CompileCommand_getArg";
   function Compile_Command_Get_Arg
     (Arg_1 : Compile_Command_T;
      I     : unsigned)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Compile_Command_Get_Arg (Arg_1, I);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Compile_Command_Get_Arg;

   function Compile_Command_Get_Mapped_Source_Path
     (Arg_1 : Compile_Command_T;
      I     : unsigned)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_CompileCommand_getMappedSourcePath";
   function Compile_Command_Get_Mapped_Source_Path
     (Arg_1 : Compile_Command_T;
      I     : unsigned)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Compile_Command_Get_Mapped_Source_Path (Arg_1, I);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Compile_Command_Get_Mapped_Source_Path;

   function Compile_Command_Get_Mapped_Source_Content
     (Arg_1 : Compile_Command_T;
      I     : unsigned)
      return Clang.CX_String.String_T
   with Import => True,
        Convention => C,
        External_Name => "clang_CompileCommand_getMappedSourceContent";
   function Compile_Command_Get_Mapped_Source_Content
     (Arg_1 : Compile_Command_T;
      I     : unsigned)
      return String
   is
      Return_Value : Clang.CX_String.String_T;
   begin
      Return_Value := Compile_Command_Get_Mapped_Source_Content (Arg_1, I);
      declare   Ada_String : String := Clang.CX_String.Get_C_String (Return_Value);
      begin   Clang.CX_String.Dispose_String (Return_Value);
      return Ada_String;
      end;
   end Compile_Command_Get_Mapped_Source_Content;

end Clang.CX_Compilation_Database;
