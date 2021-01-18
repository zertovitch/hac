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

  ----------------------
  --  Built-in units  --
  ----------------------

  procedure Register_Built_In (LD : in out Li_Defs.Library_Data);

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

  --------------------
  --  Custom units  --
  --------------------

  procedure Apply_WITH (
    CD         : in out Co_Defs.Compiler_Data;
    LD         : in out Li_Defs.Library_Data;
    Upper_Name : in     String
  );

end HAC_Sys.Librarian;
