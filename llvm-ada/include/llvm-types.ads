pragma Ada_2005;
pragma Style_Checks (Off);

pragma Warnings (Off); with Interfaces.C; use Interfaces.C; pragma Warnings (On);
with System;

package LLVM.Types is

   subtype Bool_T is int;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:29

   False : constant Bool_T := 0;
   True  : constant Bool_T := 1;

   --  skipped empty struct LLVMOpaqueMemoryBuffer

   type Memory_Buffer_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:49

   --  skipped empty struct LLVMOpaqueContext

   type Context_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:54

   --  skipped empty struct LLVMOpaqueModule

   type Module_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:62

   --  skipped empty struct LLVMOpaqueType

   type Type_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:69

   --  skipped empty struct LLVMOpaqueValue

   type Value_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:76

   --  skipped empty struct LLVMOpaqueBasicBlock

   type Basic_Block_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:83

   --  skipped empty struct LLVMOpaqueMetadata

   type Metadata_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:90

   --  skipped empty struct LLVMOpaqueBuilder

   type Builder_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:97

   --  skipped empty struct LLVMOpaqueDIBuilder

   type DI_Builder_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:104

   --  skipped empty struct LLVMOpaqueModuleProvider

   type Module_Provider_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:111

   --  skipped empty struct LLVMOpaquePassManager

   type Pass_Manager_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:114

   --  skipped empty struct LLVMOpaquePassRegistry

   type Pass_Registry_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:117

   --  skipped empty struct LLVMOpaqueUse

   type Use_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:123

   --  skipped empty struct LLVMOpaqueAttributeRef

   type Attribute_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:130

   --  skipped empty struct LLVMOpaqueDiagnosticInfo

   type Diagnostic_Info_T is new System.Address;  -- /chelles.b/users/charlet/git/gnat-llvm/llvm-ada/llvm-5.0.0.src/include/llvm-c/Types.h:135

end LLVM.Types;

