-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------
--

with HAC_Sys.Co_Defs,
     HAC_Sys.Defs,
     HAC_Sys.Files.Default;

with HAT;

with Ada.Containers.Hashed_Maps,
     Ada.Containers.Vectors,
     Ada.Strings.Unbounded.Hash;

package HAC_Sys.Librarian is

  Library_Level : constant := 0;

  type Build_Mode is
    (Read_HCU_Files
        --  ^ Full compilation around main unit is done in memory.
        --    If available and { up-to-date or no source file present },
        --    .hcu files are downloaded to the compilation tables.
     --  Write_HCU_Files
     --    --    If a .hcu file not yet available or out-of-date,
     --    --    the source is compiled and the .hcu file is (re)written.
    );

  --  HAC Compiled Unit files have the .hcu extension. Some may be stored in .zip library files.

  type Compilation_Status is
    (In_Progress,     --  Specification or body-only is in progress.
     Body_Postponed,  --  Specification done, body will be done later.
     Spec_Only,       --  Specification-only is done, but we need to check absence of body.
     Done);           --  Specification done; possible body is done or its absence is checked.

  subtype Spec_Done is Compilation_Status range Body_Postponed .. Spec_Only;

  --  RM 10.1.1

  type Unit_Kind is
    (Package_Declaration, Package_Body,
     Procedure_Unit, Function_Unit);

  subtype Subprogram_Unit is Unit_Kind range Procedure_Unit .. Function_Unit;

  type Library_Unit is record
    full_name     : HAT.VString;  --  Full unit name, like "Ada.Strings.Fixed"
    kind          : Unit_Kind;
    status        : Compilation_Status;
    id_index      : Natural;
    id_body_index : Natural;
    spec_context  : Co_Defs.Id_Maps.Map;  --  WITH & USE's visible to the spec.
  end record;

  package Library_Unit_Vectors is new Ada.Containers.Vectors (Positive, Library_Unit);

  package Library_Name_Mapping is new Ada.Containers.Hashed_Maps
    (Key_Type        => HAT.VString,  --  Upper case of full unit name
     Element_Type    => Positive,     --  Index in the library
     Hash            => Ada.Strings.Unbounded.Hash,
     Equivalent_Keys => Ada.Strings.Unbounded."=");

  --  Global object used as a default for library file management:
  default_file_catalogue : aliased Files.Default.File_Catalogue;

  type Library_Data is tagged record  --  !! details -> private
    library      : Library_Unit_Vectors.Vector;  --  The library itself (= the "books")
    map          : Library_Name_Mapping.Map;     --  Quick access by name to unit number
    cat          : Files.Abstract_File_Catalogue_Reference :=
                     default_file_catalogue'Access;
  end record;

  --  Method used internally by HAC.
  --  Prefer using Build_Data's Set_File_Catalogue method.
  --
  procedure Set_File_Catalogue
    (LD  : in out Library_Data;
     cat : in     Files.Abstract_File_Catalogue_Reference);

  --  Search for file (physical or not, depending on the
  --  LD.cat.Exists function) corresponding to unit name.
  --  First a spec, then a body.
  --  If nothing found, return empty string.
  --
  function Find_Unit_File_Name
    (LD        : Library_Data;
     Unit_Name : String)
  return String;

  -----------------------------------------------------
  --  Apply WITH clause for any unit, including the  --
  --  Standard package and the special HAT package.  --
  -----------------------------------------------------

  procedure Apply_WITH
    (CD         : in out Co_Defs.Compiler_Data;
     LD         : in out Library_Data;
     Upper_Name : in     String);

  ----------------------------------------------------------
  --  Apply the invisible "with Standard; use Standard;"  --
  ----------------------------------------------------------

  procedure Apply_WITH_USE_Standard
    (CD : in out Co_Defs.Compiler_Data;
     LD : in out Library_Data);

  ----------------------------------------------------------------------
  --  Add a new definition to the identifier table, at library level  --
  ----------------------------------------------------------------------

  procedure Enter_Library_Level_Def
    (CD             : in out Co_Defs.Compiler_Data;
     Full_Ident     : in     String;  --  "Main", "Standard.False", ...
     New_Entity     : in     Co_Defs.Entity_Kind;
     Base_Type      : in     Defs.Typen;
     Size           : in     Integer;
     Discrete_First : in     Defs.HAC_Integer := Defs.HAC_Integer'First;
     Discrete_Last  : in     Defs.HAC_Integer := Defs.HAC_Integer'Last;
     is_built_in    : in     Boolean := False);

  Circular_Unit_Dependency : exception;

  procedure Register_Unit
    (LD         : in out Library_Data;
     Descriptor : in     Library_Unit);

  procedure Change_Unit_Details
    (LD         : in out Library_Data;
     Descriptor : in     Library_Unit);
     --  ^ Changes in the library the details for
     --    unit named Descriptor.Full_Name.

  ----------------------------------------------------------------------
  --  GNAT_File_Naming returns the file name that GNAT expects for    --
  --  a unit with the name Unit_Name.                                 --
  ----------------------------------------------------------------------

  function GNAT_File_Naming (Unit_Name : String) return String;

  function Ada_RM_Casing (Identifier : String) return String;

private

end HAC_Sys.Librarian;
