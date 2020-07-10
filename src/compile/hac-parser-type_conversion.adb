with HAC.Compiler.PCode_Emit;
with HAC.Parser.Expressions;
with HAC.Parser.Helpers;                use HAC.Parser.Helpers;
with HAC.PCode;                         use HAC.PCode;
with HAC.UErrors;                       use HAC.UErrors;

procedure HAC.Parser.Type_Conversion (  --  Ada RM 4.6
  CD    : in out Compiler_Data;
  Level :        PCode.Nesting_level;
  FSys  :        Defs.Symset;
  X     :    out Exact_Typ
)
is
  use Defs;
  kind    :          Type_Conversion_Kind := Unknown;
  Type_Id : constant String               := To_String (CD.Id);
begin
  Need (CD, LParent, err_missing_an_opening_parenthesis);
  if Type_Id = HAC_Float_Name then
    kind := To_Float;
  elsif Type_Id = HAC_Integer_Name then
    kind := To_Integer;
  elsif Type_Id = "DURATION" then
    kind := To_Duration;
  end if;
  Expressions.Expression (CD, Level, FSys + RParent, X);
  case kind is
    when To_Float =>
      case X.TYP is
        when Floats =>
          null;  --  !!  Emit warning: "already float"
        when Ints =>
          Compiler.PCode_Emit.Emit1 (CD, k_Integer_to_Float, 0);
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
        when Durations =>
          null;  --  !!  Emit warning: "already duration"
        when others =>
          Argument_Type_Not_Supported (CD);
      end case;
      X.TYP := Durations;
    when Unknown =>
      Error (CD, err_type_conversion_not_supported, "no support for target type " & Type_Id);
  end case;
  Need (CD, RParent, err_closing_parenthesis_missing);
end HAC.Parser.Type_Conversion;
