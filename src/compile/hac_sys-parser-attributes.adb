with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.UErrors;

package body HAC_Sys.Parser.Attributes is

  type Attribute is (
    First,
    Image,
    Last,
    Value
  );

  procedure Scalar_Subtype_Attribute (
    CD      : in out Co_Defs.Compiler_Data;
    Level   :        Defs.Nesting_level;
    FSys    :        Defs.Symset;
    Typ_ID  : in     Co_Defs.IdTabEntry;
    X       :    out Co_Defs.Exact_Subtyp
  )
  is
  pragma Unreferenced (Level, FSys, X);
    use Defs, UErrors;
    attr : Attribute;
    attr_ID : constant String := To_String (CD.Id);
    discrete_value : HAC_Integer;
  begin
    attr := Attribute'Value (attr_ID);
    Scanner.InSymbol (CD);  --  Consume the attribute name (First, Last, ...)
    case attr is
      when First | Last =>
        case Typ_ID.xTyp.TYP is
          when NOTYP =>
            null;  --  Already in error
          when others =>
            if Discrete_Typ (Typ_ID.xTyp.TYP) then
              if attr = First then
                discrete_value := Typ_ID.xTyp.Discrete_First;
              else
                discrete_value := Typ_ID.xTyp.Discrete_Last;
              end if;
              Compiler.PCode_Emit.Emit_1 (CD, PCode.k_Push_Discrete_Literal, discrete_value);
              X := Typ_ID.xTyp;
            else
              Error (CD, err_not_yet_implemented, "attribute " & attr_ID & " for this subtype", True);
            end if;
        end case;
      when others =>
        Error (CD, err_not_yet_implemented, "attribute " & attr_ID, True);
    end case;
  exception
    when Constraint_Error =>
      Error (CD, err_syntax_error, ": unknown attribute: " & attr_ID, True);
  end Scalar_Subtype_Attribute;

end HAC_Sys.Parser.Attributes;
