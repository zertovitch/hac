with HAC.Compiler, HAC.Data;

package HAC.Parser.Tasking is

  ------------------------------------------------------------------
  -------------------------------------------------Task_Declaration-
  --  Hathorn
  procedure Task_Declaration (
    CD      : in out HAC.Compiler.Compiler_Data;
    FSys    :        HAC.Data.Symset;
    Level_A :        Integer
  );

end HAC.Parser.Tasking;