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
     HAC_Sys.Defs;

with HAL;

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

  type Compilation_Status is (
    In_Progress,     --  Specification or body-only is in progress.
    Body_Postponed,  --  Specification done, body will be done later.
    Done
  );

  type Unit_Kind is (Package_Unit, Procedure_Unit, Function_Unit);

  subtype Subprogram_Unit is Unit_Kind range Procedure_Unit .. Function_Unit;

  type Library_Unit is record
    full_name     : HAL.VString;  --  Full unit name, like "Ada.Strings.Fixed"
    kind          : Unit_Kind;
    status        : Compilation_Status;
    needs_body    : Boolean;
    id_index      : Natural;
    id_body_index : Natural;
    spec_context  : Co_Defs.Id_Set.Set;  --  WITH & USE's visible to the spec.
  end record;

  package Library_Unit_Vectors is new Ada.Containers.Vectors (Positive, Library_Unit);

  package Library_Name_Mapping is new Ada.Containers.Hashed_Maps
    (Key_Type        => HAL.VString,  --  Upper case of full unit name
     Element_Type    => Positive,     --  Index in the library
     Hash            => Ada.Strings.Unbounded.Hash,
     Equivalent_Keys => Ada.Strings.Unbounded."=");

  type Library_Data is record
    Library : Library_Unit_Vectors.Vector;  --  The library itself
    Map     : Library_Name_Mapping.Map;     --  Quick access by name to unit number
  end record;

  --  Search for "physical" file corresponding to unit name
  --  First a spec, then a body.
  --  If nothing found, return empty string.
  --
  function Find_Unit_File_Name (
    Unit_Name : String
    --  TBD:
    --  search path for single source files;
    --  search path for zipped files (.har, like .jar ...)
  )
  return String;

  -----------------------------------------------------
  --  Apply WITH clause for any unit, including the  --
  --  Standard package and the special HAL package.  --
  -----------------------------------------------------

  procedure Apply_WITH (
    CD         : in out Co_Defs.Compiler_Data;
    LD         : in out Library_Data;
    Upper_Name : in     String
  );

  -------------------------------------------------
  --  Apply the USE clause at any nesting level  --
  -------------------------------------------------

  procedure Apply_USE_Clause (
    CD       : in out Co_Defs.Compiler_Data;
    Level    : in     Defs.Nesting_level;
    Pkg_Idx  : in     Natural  --  Index in the identifier table for USEd package.
  );

  ----------------------------------------------------------
  --  Apply the invisible "with Standard; use Standard;"  --
  ----------------------------------------------------------

  procedure Apply_WITH_USE_Standard (
    CD         : in out Co_Defs.Compiler_Data;
    LD         : in out Library_Data
  );

  ----------------------------------------------------------------------
  --  Add a new definition to the identifier table, at library level  --
  ----------------------------------------------------------------------

  procedure Enter_Library_Level_Def (
    CD             : in out Co_Defs.Compiler_Data;
    Full_Ident     : in     String;  --  "Main", "Standard.False", ...
    New_Entity     : in     Co_Defs.Entity_Kind;
    Base_Type      : in     Defs.Typen;
    Size           : in     Integer;
    Discrete_First : in     Defs.HAC_Integer := 0;
    Discrete_Last  : in     Defs.HAC_Integer := 0
  );

  Circular_Unit_Dependency : exception;

  procedure Register_Unit (
    LD         : in out Library_Data;
    Descriptor : in     Library_Unit
  );

  procedure Change_Unit_Details (
    LD         : in out Library_Data;
    Descriptor : in     Library_Unit
    --  ^ Changes in the library the details for
    --    unit named Descriptor.Full_Name.
  );

end HAC_Sys.Librarian;
