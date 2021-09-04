private package HAC_Sys.Parser.Attributes is

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
    CD      : in out Co_Defs.Compiler_Data;
    Level   :        Defs.Nesting_level;
    FSys    :        Defs.Symset;
    Typ_ID  : in     Co_Defs.IdTabEntry;
    X       : in out Co_Defs.Exact_Typ
  );

end HAC_Sys.Parser.Attributes;
