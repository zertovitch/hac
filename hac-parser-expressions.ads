with HAC.Compiler;           use HAC.Compiler;
with HAC.Data;               use HAC.Data;

package HAC.Parser.Expressions is

  procedure Boolean_Expression (
    CD    : in out Compiler_Data;
    Level :        Integer;
    FSys  :        Symset;
    X     :    out Exact_Type
  );

  procedure Expression (
    CD    : in out Compiler_Data;
    Level :        Integer;
    FSys  :        Symset;
    X     :    out Exact_Type
  );

  procedure Selector (
    CD    : in out Compiler_Data;
    Level :        Integer;
    FSys  :        Symset;
    V     : in out Exact_Type
  );

end HAC.Parser.Expressions;
