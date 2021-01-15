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
--  Li_Defs: Librarian Definitions

with HAL;

with Ada.Containers.Hashed_Maps,
     Ada.Containers.Vectors,
     Ada.Strings.Unbounded.Hash;

package HAC_Sys.Li_Defs is

  Library_Level : constant := 0;

  type Build_Mode is
    (All_in_Memory
        --  ^ Full compilation around main unit is done in memory.
        --  Object code is shared, but ther is one ID table per unit.
     --  Use_Library_Files
     --    --  ^ Use .hal files (some stored in .zip files)
    );

  type Compilation_Status is (
    Void,
    Spec_Only,      --  Only specification has been processed so far.
    Live_Compiled   --  Unit (spec & body) is compiled in the main object code.
    --  In a future version we can choose to compile units into files.
  );

  type Unit_Kind is (Package_Unit, Procedure_Unit, Function_Unit);

  type Library_Unit is record
    Kind       : Unit_Kind;
    Full_Name  : HAL.VString;  --  Full unit name, like "Ada.Strings.Fixed"
    Status     : Compilation_Status;
    Needs_Body : Boolean;
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

end HAC_Sys.Li_Defs;
