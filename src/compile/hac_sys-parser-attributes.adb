with HAC_Sys.UErrors;

package body HAC_Sys.Parser.Attributes is

  type Attribute is (
    Image,
    Value
  );

  procedure Scalar_Subtype_Attribute (
    CD      : in out Co_Defs.Compiler_Data;
    Level   :        Defs.Nesting_level;
    FSys    :        Defs.Symset;
    Type_ID :        String;
    X       :    out Co_Defs.Exact_Typ
  )
  is
  pragma Unreferenced (Level, FSys, Type_ID, X);
    attr : Attribute;
    pragma Unreferenced (attr);
    use Defs, UErrors;
    attr_ID : constant String := To_String (CD.Id);
  begin
    attr := Attribute'Value (attr_ID);
    Error (CD, err_not_yet_implemented, "attribute " & attr_ID, True);
  exception
    when Constraint_Error =>
      Error (CD, err_syntax_error, "unknown attribute: " & attr_ID, True);
  end Scalar_Subtype_Attribute;

end HAC_Sys.Parser.Attributes;
