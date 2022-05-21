with HAC_Sys.Defs;

private package HAC_Sys.Parser.Ranges is

  --  This package has two families (Static_Range, Dynamic_Range) of
  --  discrete_subtype_definition RM 3.6 (6). In a distant future we could
  --  have a variant of Dynamic_Range which detects that the bounds are
  --  actually static and optimizes the code accordingly.

  --      which is either:
  --        a subtype_indication 3.2.2 (3) : name [constraint]
  --        like "Color [range red .. blue]"
  --      or
  --        a range 3.5 (3)
  --        which is either:
  --          simple_expression .. simple_expression : "low .. high"
  --        or
  --          range_attribute_reference 4.1.4 (4): A'Range[(2)]

  ---------------------
  --  Static ranges  --
  ---------------------

  --  A range with static bounds is parsed.
  --  The bounds are known at compile-time.
  --  At least, HAC is expecting the bounds to be static...
  --
  --    Examples of static bounds:
  --      type T is range 1 .. 10;              --  Must be static
  --      type T is new Integer range 1 .. 10;  --  Could be dynamic as well
  --      subtype S is T range 2 .. 9;          --  Could be dynamic as well
  --
  --  As long as HAC has static-only arrays, this is also used for:
  --      type A is array (1 .. 5);
  --
  --  Purely static discrete_subtype_definition in "full" Ada seem
  --  to be restricted to:
  --    - range types
  --    - case statements
  --    - record type declarations with variant parts.
  --  CF answer by Niklas Holsti to
  --    "Q: discrete_subtype_definition: static only cases?"
  --    on comp.lang.ada, 2020-06-07.

  ---------------------------
  -- Explicit_Static_Range --
  ---------------------------
  --
  --  `1 .. 3`
  --
  procedure Explicit_Static_Range (
    CD             : in out Co_Defs.Compiler_Data;
    Level          : in     Defs.Nesting_level;
    FSys           : in     Defs.Symset;
    Specific_Error : in     Defs.Compile_Error;
    Lower_Bound    :    out Co_Defs.Constant_Rec;
    Higher_Bound   :    out Co_Defs.Constant_Rec
  );

  ------------------
  -- Static_Range --
  ------------------
  --
  --  So far: either `1 .. 3` (that is, Explicit_Static_Range) or `Character`.
  --
  procedure Static_Range (
    CD             : in out Co_Defs.Compiler_Data;
    Level          : in     Defs.Nesting_level;
    FSys           : in     Defs.Symset;
    Specific_Error : in     Defs.Compile_Error;
    Lower_Bound    :    out Co_Defs.Constant_Rec;
    Higher_Bound   :    out Co_Defs.Constant_Rec
  );

  -------------------
  -- Dynamic_Range --
  -------------------
  --
  --  A range with dynamic bounds is parsed.
  --  The bounds are pushed on the stack.
  --    Example:
  --      --  FOR statement (RM 5.5 (4)).
  --      for I in J .. N loop

  procedure Dynamic_Range (
    CD                 : in out Co_Defs.Compiler_Data;
    Level              : in     Defs.Nesting_level;
    FSys               : in     Defs.Symset;
    Non_Discrete_Error : in     Defs.Compile_Error;
    Range_Typ          :    out Co_Defs.Exact_Subtyp
  );

  procedure Set_Singleton_Range (X : in out Co_Defs.Exact_Subtyp; Value : Defs.HAC_Integer);

  function Is_Singleton_Range (X : Co_Defs.Exact_Subtyp) return Boolean;
  pragma Inline (Is_Singleton_Range);

  function Is_Singleton_Range (X : Co_Defs.Exact_Subtyp; Value : Defs.HAC_Integer) return Boolean;
  pragma Inline (Is_Singleton_Range);

  procedure Negate_Range
    (CD : in out Co_Defs.Compiler_Data;
     X  : in out Co_Defs.Exact_Subtyp);

  --  Check whether ranges [X_min .. X_max] and [Y_min .. Y_max] overlap.
  --
  function Do_Ranges_Overlap (X_min, X_max, Y_min, Y_max : Defs.HAC_Integer) return Boolean;
  pragma Inline (Do_Ranges_Overlap);

  function Do_Ranges_Overlap (X, Y : Co_Defs.Exact_Subtyp) return Boolean;
  pragma Inline (Do_Ranges_Overlap);

end HAC_Sys.Parser.Ranges;
