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
     HAC_Sys.Li_Defs;

package HAC_Sys.Librarian is

  -----------------------------------------------------
  --  Apply WITH clause for any unit, including the  --
  --  Standard package and the special HAL package.  --
  -----------------------------------------------------

  procedure Apply_WITH (
    CD         : in out Co_Defs.Compiler_Data;
    LD         : in out Li_Defs.Library_Data;
    Upper_Name : in     String
  );

  -------------------------------------------------
  --  Apply the USE clause at any nesting level  --
  -------------------------------------------------

  procedure Apply_USE_Clause (
    CD       : in out Co_Defs.Compiler_Data;
    Level    : in     Defs.Nesting_level;
    Pkg_Idx  : in     Natural  --  Index in the identifier table
  );

  ----------------------------------------------------------
  --  Apply the invisible "with Standard; use Standard;"  --
  ----------------------------------------------------------

  procedure Apply_WITH_USE_Standard (
    CD         : in out Co_Defs.Compiler_Data;
    LD         : in out Li_Defs.Library_Data
  );

  ----------------------------------------------------------------------
  --  Add a new definition to the identifier table, at library level  --
  ----------------------------------------------------------------------

  procedure Enter_Zero_Level_Def (
    CD             : in out Co_Defs.Compiler_Data;
    Full_Ident     : in     String;  --  "Main", "Standard.False", ...
    New_Entity     : in     Co_Defs.Entity_Kind;
    Base_Type      : in     Defs.Typen;
    Size           : in     Integer;
    Discrete_First : in     Defs.HAC_Integer := 0;
    Discrete_Last  : in     Defs.HAC_Integer := 0
  );

  Circular_Unit_Dependency : exception;

end HAC_Sys.Librarian;
