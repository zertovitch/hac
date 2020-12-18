with HAC_Sys.Defs;

private package HAC_Sys.Parser.Expressions is

  --  For all expressions:
  --   - the appropriate machine code is emitted;
  --   - in the machine code, the expression is pushed on the stack
  --   - for parsing: the type of the expression is set in X.

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

  --  Conversion, like ` Integer (123.456) ` or
  --  attribute, like  ` Integer'Image (123) `.
  --
  procedure Subtype_Prefixed_Expression (
    CD    : in out Compiler_Data;
    Level : in     PCode.Nesting_level;
    FSys  : in     Defs.Symset;
    X     :    out Exact_Typ
  );

end HAC_Sys.Parser.Expressions;
