pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings (Off, "*redundant with clause in body*");

with Interfaces.C;         use Interfaces.C;
pragma Unreferenced (Interfaces.C);
with Interfaces.C.Strings; use Interfaces.C.Strings;pragma Unreferenced (Interfaces.C.Strings);

package body LLVM.Object is

   function Mach_O_Universal_Binary_Copy_Object_For_Arch
     (BR            : LLVM.Types.Binary_T;
      Arch          : Interfaces.C.Strings.chars_ptr;
      Arch_Len      : stddef_h.size_t;
      Error_Message : System.Address)
      return LLVM.Types.Binary_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMMachOUniversalBinaryCopyObjectForArch";
   function Mach_O_Universal_Binary_Copy_Object_For_Arch
     (BR            : LLVM.Types.Binary_T;
      Arch          : String;
      Arch_Len      : stddef_h.size_t;
      Error_Message : System.Address)
      return LLVM.Types.Binary_T
   is
      Return_Value : LLVM.Types.Binary_T;
      Arch_Array   : aliased char_array := To_C (Arch);
      Arch_String  : constant chars_ptr := To_Chars_Ptr (Arch_Array'Unchecked_Access);
   begin
      Return_Value := Mach_O_Universal_Binary_Copy_Object_For_Arch (BR, Arch_String, Arch_Len, Error_Message);
      return Return_Value;
   end Mach_O_Universal_Binary_Copy_Object_For_Arch;

   function Object_File_Is_Section_Iterator_At_End
     (BR : LLVM.Types.Binary_T;
      SI : Section_Iterator_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMObjectFileIsSectionIteratorAtEnd";
   function Object_File_Is_Section_Iterator_At_End
     (BR : LLVM.Types.Binary_T;
      SI : Section_Iterator_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Object_File_Is_Section_Iterator_At_End (BR, SI);
      return Return_Value /= 0;
   end Object_File_Is_Section_Iterator_At_End;

   function Object_File_Is_Symbol_Iterator_At_End
     (BR : LLVM.Types.Binary_T;
      SI : Symbol_Iterator_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMObjectFileIsSymbolIteratorAtEnd";
   function Object_File_Is_Symbol_Iterator_At_End
     (BR : LLVM.Types.Binary_T;
      SI : Symbol_Iterator_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Object_File_Is_Symbol_Iterator_At_End (BR, SI);
      return Return_Value /= 0;
   end Object_File_Is_Symbol_Iterator_At_End;

   function Get_Section_Name
     (SI : Section_Iterator_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetSectionName";
   function Get_Section_Name
     (SI : Section_Iterator_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Section_Name (SI);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Section_Name;

   function Get_Section_Contents
     (SI : Section_Iterator_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetSectionContents";
   function Get_Section_Contents
     (SI : Section_Iterator_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Section_Contents (SI);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Section_Contents;

   function Get_Section_Contains_Symbol
     (SI  : Section_Iterator_T;
      Sym : Symbol_Iterator_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetSectionContainsSymbol";
   function Get_Section_Contains_Symbol
     (SI  : Section_Iterator_T;
      Sym : Symbol_Iterator_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Get_Section_Contains_Symbol (SI, Sym);
      return Return_Value /= 0;
   end Get_Section_Contains_Symbol;

   function Is_Relocation_Iterator_At_End
     (Section : Section_Iterator_T;
      RI      : Relocation_Iterator_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsRelocationIteratorAtEnd";
   function Is_Relocation_Iterator_At_End
     (Section : Section_Iterator_T;
      RI      : Relocation_Iterator_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Relocation_Iterator_At_End (Section, RI);
      return Return_Value /= 0;
   end Is_Relocation_Iterator_At_End;

   function Get_Symbol_Name
     (SI : Symbol_Iterator_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetSymbolName";
   function Get_Symbol_Name
     (SI : Symbol_Iterator_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Symbol_Name (SI);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Symbol_Name;

   function Get_Relocation_Type_Name
     (RI : Relocation_Iterator_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetRelocationTypeName";
   function Get_Relocation_Type_Name
     (RI : Relocation_Iterator_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Relocation_Type_Name (RI);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Relocation_Type_Name;

   function Get_Relocation_Value_String
     (RI : Relocation_Iterator_T)
      return Interfaces.C.Strings.chars_ptr
   with Import => True,
        Convention => C,
        External_Name => "LLVMGetRelocationValueString";
   function Get_Relocation_Value_String
     (RI : Relocation_Iterator_T)
      return String
   is
      Return_Value : Interfaces.C.Strings.chars_ptr;
   begin
      Return_Value := Get_Relocation_Value_String (RI);
      if Return_Value /= Null_Ptr then
         return Value (Return_Value);
      else
         return "";
      end if;
   end Get_Relocation_Value_String;

   function Is_Section_Iterator_At_End
     (Object_File : Object_File_T;
      SI          : Section_Iterator_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsSectionIteratorAtEnd";
   function Is_Section_Iterator_At_End
     (Object_File : Object_File_T;
      SI          : Section_Iterator_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Section_Iterator_At_End (Object_File, SI);
      return Return_Value /= 0;
   end Is_Section_Iterator_At_End;

   function Is_Symbol_Iterator_At_End
     (Object_File : Object_File_T;
      SI          : Symbol_Iterator_T)
      return LLVM.Types.Bool_T
   with Import => True,
        Convention => C,
        External_Name => "LLVMIsSymbolIteratorAtEnd";
   function Is_Symbol_Iterator_At_End
     (Object_File : Object_File_T;
      SI          : Symbol_Iterator_T)
      return Boolean
   is
      Return_Value : LLVM.Types.Bool_T;
   begin
      Return_Value := Is_Symbol_Iterator_At_End (Object_File, SI);
      return Return_Value /= 0;
   end Is_Symbol_Iterator_At_End;

end LLVM.Object;
