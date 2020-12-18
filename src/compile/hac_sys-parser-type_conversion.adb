with HAC_Sys.Compiler.PCode_Emit;
with HAC_Sys.Parser.Expressions;
with HAC_Sys.Parser.Helpers;                use HAC_Sys.Parser.Helpers;
with HAC_Sys.PCode;                         use HAC_Sys.PCode;
with HAC_Sys.UErrors;                       use HAC_Sys.UErrors;

procedure HAC_Sys.Parser.Type_Conversion (  --  Ada RM 4.6
  CD      : in out Compiler_Data;
  Level   :        PCode.Nesting_level;
  FSys    :        Defs.Symset;
  Type_ID :        String;
  X       :    out Exact_Typ
)
is
  use Defs;
  type Type_Conversion_Kind is (To_Float, To_Integer, To_Duration, Unknown);
  kind : Type_Conversion_Kind := Unknown;
begin
  if Type_ID = HAC_Float_Name then
    kind := To_Float;
  elsif Type_ID = HAC_Integer_Name then
    kind := To_Integer;
  elsif Type_ID = "DURATION" then
    kind := To_Duration;
  end if;
  --
  Expressions.Expression (CD, Level, FSys + RParent, X);
  --
  case kind is
    when To_Float =>
      case X.TYP is
        when Floats =>
          null;  --  !!  Emit warning: "already float"
        when Ints =>
          Compiler.PCode_Emit.Emit_1 (CD, k_Integer_to_Float, 0);
        when Durations =>
          Compiler.PCode_Emit.Emit_Std_Funct (CD, SF_Duration_to_Float);
        when others =>
          Argument_Type_Not_Supported (CD);
      end case;
      X.TYP := Floats;
      --
    when To_Integer =>
      case X.TYP is
        when Floats =>  --  Rounding to closest integer (Ada RM 4.6 (33)).
          Compiler.PCode_Emit.Emit_Std_Funct (CD, SF_Round_Float_to_Int);
        when Durations =>
          Compiler.PCode_Emit.Emit_Std_Funct (CD, SF_Duration_to_Int);
        when Ints =>
          null;  --  !!  Emit warning: "already integer"
        when others =>
          Argument_Type_Not_Supported (CD);
      end case;
      X.TYP := Ints;
      --
    when To_Duration =>
      case X.TYP is
        when Floats =>
          Compiler.PCode_Emit.Emit_Std_Funct (CD, SF_Float_to_Duration);
        when Ints =>
          Compiler.PCode_Emit.Emit_Std_Funct (CD, SF_Int_to_Duration);
        when Durations =>
          null;  --  !!  Emit warning: "already duration"
        when others =>
          Argument_Type_Not_Supported (CD);
      end case;
      X.TYP := Durations;
    when Unknown =>
      Error (CD, err_type_conversion_not_supported, "no support for target type " & Type_ID);
  end case;
  Need (CD, RParent, err_closing_parenthesis_missing);
end HAC_Sys.Parser.Type_Conversion;
