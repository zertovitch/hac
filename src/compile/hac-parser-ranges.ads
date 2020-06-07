with HAC.Defs;

private package HAC.Parser.Ranges is

  --  This package has two variants (1: static, 2: dynamic) of
  --  discrete_subtype_definition RM 3.6 (6)
  --
  --      which is either:
  --        a subtype_indication 3.2.2 (3) : name [constraint]
  --        like "Color [range red .. blue]"
  --      or
  --        a range 3.5 (3)
  --        which is either:
  --          simple_expression .. simple_expression : "low .. high"
  --        or
  --          range_attribute_reference 4.1.4 (4): A'Range[(2)]

  --  (1) A range with static bounds is parsed.
  --  The bounds are known at compile-time.
  --  At least, HAC is expecting the bounds to be static...
  --  NB: perhaps there is no case of purely
  --  static discrete_subtype_definition in "full" Ada.
  --
  --    Examples:
  --      type T is range 1 .. 10;
  --      subtype S is T range 2 .. 9;
  --
  --  As long as HAC has static-only arrays, this is
  --  also used for:
  --      type A is array (1 .. 5)
  --
  procedure Static_Range (
    CD         : in out Compiler_Data;
    Level      :        PCode.Nesting_level;
    FSys       :        Defs.Symset;
    Low_Bound  :    out Defs.HAC_Integer;
    High_Bound :    out Defs.HAC_Integer
  );

  --  (2) A range with dynamic bounds is parsed.
  --  The bounds are pushed on the stack.
  --    Example:
  --      --  FOR statement (RM 5.5 (4)).
  --      for I in J .. N loop

  procedure Dynamic_Range (
    CD         : in out Compiler_Data;
    Level      :        PCode.Nesting_level;
    FSys       :        Defs.Symset
  );

end HAC.Parser.Ranges;
