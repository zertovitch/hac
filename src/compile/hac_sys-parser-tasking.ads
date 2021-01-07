with HAC_Sys.Defs;

private package HAC_Sys.Parser.Tasking is

  ------------------------------------------------------------------
  -------------------------------------------------Task_Declaration-
  --  Hathorn
  procedure Task_Declaration (
    CD            : in out Compiler_Data;
    FSys          :        Defs.Symset;
    Initial_Level :        Defs.Nesting_level
  );

end HAC_Sys.Parser.Tasking;
