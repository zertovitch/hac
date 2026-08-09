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
--  Record component default values (RM 3.7 / 3.8 default_expression),
--  restricted to *static* values: literals, named numbers, enumeration
--  literals, and aggregates built from these. Because everything here
--  resolves to a compile-time-known value rather than code to replay, a
--  field's default is computed once (when its record type is declared)
--  and stored as plain data (Co_Defs.Default_Value); it is then stamped
--  out as ordinary Push_Address + literal + Store code at every later
--  object declaration that has no explicit initializer -- safe at any
--  lexical level, since nothing here is captured bytecode.
--
--  Not supported (documented v1 restriction): default expressions that
--  reference functions or variables (would require threading flow-analysis
--  context through the whole type-declaration call chain, for a feature
--  whose only real-world need is static values); a composite field's
--  default naming another composite constant instead of an aggregate
--  literal; box notation "<>".

private package HAC_Sys.Parser.Defaults is

  --  Parses one field's default value. Precondition: ":=" already
  --  consumed; CD.Sy is positioned at the start of the value. Consumes
  --  through the end of the value (a static scalar expression, or a
  --  parenthesized static aggregate, positional/named/"others", when
  --  Expected is a record or array type).
  --
  function Parse_Static_Default_Value
    (CD       : in out Co_Defs.Compiler_Data;
     Level    :        Defs.Nesting_Level;
     FSys     :        Defs.Symset;
     Expected :        Co_Defs.Exact_Subtyp) return Co_Defs.Default_Value_Access;

  --  Emits store code for Value (as built by Parse_Static_Default_Value,
  --  or inherited/fanned-out from a nested type's own Default) at
  --  (Dest_Level, Dest_Base), recursively.
  --
  procedure Emit_Default_Value
    (CD         : in out Co_Defs.Compiler_Data;
     Dest_Level :        Defs.Nesting_Level;
     Dest_Base  :        Defs.HAC_Integer;
     Value      :        Co_Defs.Default_Value_Access);

  --  For a field with no explicit default, returns whatever default it
  --  should inherit from its own type, or null if there is none:
  --  a Records type contributes its own stored whole-type Default; an
  --  Arrays type recursively fans its element type's own inherited
  --  default out across every position (handling array-of-array-of-...
  --  -record uniformly, since HAC represents multi-dimensional arrays
  --  as arrays of arrays); any other type contributes null (arrays and
  --  scalars never carry a default of their own -- RM 3.7/3.8 only ever
  --  attaches default_expression to an object declaration or a record
  --  component declaration, never to a type declaration itself).
  --
  function Inherited_Default
    (CD  : in out Co_Defs.Compiler_Data;
     Typ :        Co_Defs.Exact_Subtyp) return Co_Defs.Default_Value_Access;

end HAC_Sys.Parser.Defaults;
