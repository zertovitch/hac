with HAC.Defs;

private package HAC.Parser.Expressions is

  use Defs;

  procedure Boolean_Expression (
    CD    : in out Compiler_Data;
    Level :        PCode.Nesting_level;
    FSys  :        Symset;
    X     :    out Exact_Typ
  );

  procedure Expression (
    CD    : in out Compiler_Data;
    Level :        PCode.Nesting_level;
    FSys  :        Symset;
    X     :    out Exact_Typ
  );

  procedure Selector (
    CD    : in out Compiler_Data;
    Level :        PCode.Nesting_level;
    FSys  :        Symset;
    V     : in out Exact_Typ
  );

end HAC.Parser.Expressions;
