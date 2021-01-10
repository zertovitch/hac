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

with HAC_Sys.Co_Defs, HAC_Sys.Defs;

package HAC_Sys.Librarian is

  Library_Level : constant := 0;

  type Build_Mode is
    (All_in_Memory
        --  ^ Full compilation around main unit is done in memory.
        --  Object code is shared, but ther is one ID table per unit.
     --  Use_Files
     --    --  ^ Use .hau files (some stored in .zip files)
    );

  ----------------------
  --  Built-in units  --
  ----------------------

  procedure Enter_Built_In (
    CD             : in out Co_Defs.Compiler_Data;
    Full_Ident     : in     String;  --  "Main", "Standard.False", ...
    New_Entity     : in     Co_Defs.Entity_Kind;
    Base_Type      : in     Defs.Typen;
    Size           : in     Integer;
    Discrete_First : in     Defs.HAC_Integer := 0;
    Discrete_Last  : in     Defs.HAC_Integer := 0
  );

  procedure Apply_USE_Clause (
    CD       : in out Co_Defs.Compiler_Data;
    Level    : in     Defs.Nesting_level;
    Pkg_Idx  : in     Natural  --  Index in the current ID table
  );

  --  Import Standard definitions into the current ID table.
  --
  procedure Apply_WITH_Standard (CD : in out Co_Defs.Compiler_Data);

  --  Import HAL definitions into the current ID table.
  --
  procedure Apply_WITH_HAL (CD : in out Co_Defs.Compiler_Data);

end HAC_Sys.Librarian;
