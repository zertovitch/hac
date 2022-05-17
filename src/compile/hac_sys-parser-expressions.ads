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

private package HAC_Sys.Parser.Expressions is

  procedure Static_Scalar_Expression (
    CD      : in out Co_Defs.Compiler_Data;
    Level   : in     Defs.Nesting_level;
    FSys_ND : in     Defs.Symset;
    C       :    out Co_Defs.Constant_Rec
  );

  --  For all dynamic expressions:
  --   - the appropriate machine code is emitted;
  --   - in the machine code, the expression is pushed on the stack
  --   - for parsing: the type of the expression is set in X.

  procedure Boolean_Expression (
    CD    : in out Co_Defs.Compiler_Data;
    Level :        Defs.Nesting_level;
    FSys  :        Defs.Symset;
    X     :    out Co_Defs.Exact_Subtyp
  );

  procedure Expression (
    CD    : in out Co_Defs.Compiler_Data;
    Level :        Defs.Nesting_level;
    FSys  :        Defs.Symset;
    X     :    out Co_Defs.Exact_Subtyp
  );  --  RM 4.4 (2)

  procedure Simple_Expression (
    CD    : in out Co_Defs.Compiler_Data;
    Level :        Defs.Nesting_level;
    FSys  :        Defs.Symset;
    X     :    out Co_Defs.Exact_Subtyp
  );  --  RM 4.4 (4)

  procedure Selector (
    CD    : in out Co_Defs.Compiler_Data;
    Level :        Defs.Nesting_level;
    FSys  :        Defs.Symset;
    V     : in out Co_Defs.Exact_Subtyp
  );

  --  Conversion, like ` Integer (123.456) `
  --  or attribute, like  ` Integer'Image (123) `.
  --
  procedure Subtype_Prefixed_Expression (
    CD           : in out Co_Defs.Compiler_Data;
    Level        : in     Defs.Nesting_level;
    FSys         : in     Defs.Symset;
    Typ_ID_Index : in     Natural;
    X            : in out Co_Defs.Exact_Subtyp
  );

end HAC_Sys.Parser.Expressions;
