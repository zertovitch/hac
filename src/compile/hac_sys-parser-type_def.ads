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

private package HAC_Sys.Parser.Type_Def is  --  Package around type definitions.

  ------------------------------------------------------------------
  -------------------------------------------------Type_Declaration-
  --
  --  Parses "type T is ..." and "subtype T is ..."
  --
  procedure Type_Declaration (
    CD       : in out Co_Defs.Compiler_Data;
    Level    : in     Defs.Nesting_level;
    FSys_NTD : in     Defs.Symset
  );

  ------------------------------------------------------------------
  -----------------------------------Type_Definition - RM 3.2.1 (4)-
  --
  procedure Type_Definition (
    CD            : in out Co_Defs.Compiler_Data;
    Initial_Level : in     Defs.Nesting_level;
    FSys_TD       : in     Defs.Symset;
    xTP           :    out Co_Defs.Exact_Subtyp;
    Size          :    out Integer
  );

end HAC_Sys.Parser.Type_Def;
