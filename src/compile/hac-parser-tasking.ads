with HAC.Compiler, HAC.Defs;

private package HAC.Parser.Tasking is

  ------------------------------------------------------------------
  -------------------------------------------------Task_Declaration-
  --  Hathorn
  procedure Task_Declaration (
    CD            : in out Compiler.Compiler_Data;
    FSys          :        Defs.Symset;
    Initial_Level :        PCode.Nesting_level
  );

end HAC.Parser.Tasking;
