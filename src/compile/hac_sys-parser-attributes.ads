with HAC_Sys.Defs;

private package HAC_Sys.Parser.Attributes is

  use Defs;

  --  Scalar subtype attributes. Ada RM 3.5 (10).
  --
  --  The subtype name has just been parsed and determined to
  --  be corresponding to subtype S. The symbol "'" has been parsed too.
  --  In output, the type of the returned value is set in X.
  --  Example:
  --    S is Boolean, we have the expression "Boolean'Image (Flag)"
  --    From here we parse the attribute ("Image"), then, its
  --    parameter "(Flag)", return a X = (String_Literals, 0) internal type.

  procedure Scalar_Subtype_Attribute (
    CD      : in out Compiler_Data;
    Level   :        PCode.Nesting_level;
    FSys    :        Defs.Symset;
    Type_ID :        String;
    X       :    out Exact_Typ
  );

end HAC_Sys.Parser.Attributes;
