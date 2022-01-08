private package HAC_Sys.Parser.Attributes is

  --  Scalar subtype attributes. Ada RM 3.5 (10).
  --
  --  The subtype name has just been parsed and determined to
  --  be corresponding to subtype S. The symbol "'" has been parsed too.
  --  In output, the type of the returned value is set in X.
  --  Example:
  --    S is Boolean, we have the expression "Boolean'Image (Flag)"
  --    From here we parse the attribute ("Image"), then, its
  --    parameter "(Flag)" if any.
  --    The return type is X = (Strings_as_VStrings, 0), an internal type in this case.

  procedure Scalar_Subtype_Attribute (
    CD             : in out Co_Defs.Compiler_Data;
    Level          : in     Defs.Nesting_level;
    FSys           : in     Defs.Symset;
    Typ_ID_Index   : in     Natural;
    Type_of_Result :    out Co_Defs.Exact_Subtyp
  );

end HAC_Sys.Parser.Attributes;
