with HAC_Sys.Compiler.PCode_Emit;
with HAC_Sys.Parser.Expressions;
with HAC_Sys.Parser.Helpers;                use HAC_Sys.Parser.Helpers;
with HAC_Sys.PCode;                         use HAC_Sys.PCode;
with HAC_Sys.UErrors;                       use HAC_Sys.UErrors;

procedure HAC_Sys.Parser.Type_Conversion (  --  Ada RM 4.6
  CD      : in out Co_Defs.Compiler_Data;
  Level   :        Defs.Nesting_level;
  FSys    :        Defs.Symset;
  Type_ID :        String;
  X       : in     Co_Defs.Exact_Typ
)
is
  use Defs;
  type Type_Conversion_Kind is (To_Float, To_Integer, To_Duration, Unknown);
  kind : Type_Conversion_Kind;
  T_Expr : Co_Defs.Exact_Typ;
begin
  --  X is the type of the name just parsed, as in the identifier table.
  --  E.g.:  "Natural (12.34)"  ->  X is from Standard.Natural as entered by the librarian.
  case X.TYP is
    when Floats    => kind := To_Float;
    when Ints      => kind := To_Integer;
    when Durations => kind := To_Duration;
    when others    => kind := Unknown;
  end case;
  --
  Expressions.Expression (CD, Level, FSys + RParent, T_Expr);
  --  T_Expr is the type of the expression between the parentheses.
  --  E.g.:  "Natural (12.34)"  ->  X.TYP is Floats because of 12.34.
  --
  case kind is
    when To_Float =>
      case T_Expr.TYP is
        when Floats =>
          null;  --  !! Check range if any !!
        when Ints =>
          Compiler.PCode_Emit.Emit_1 (CD, k_Integer_to_Float, 0);
        when Durations =>
          Compiler.PCode_Emit.Emit_Std_Funct (CD, SF_Duration_to_Float);
        when others =>
          Argument_Type_Not_Supported (CD);
      end case;
      --
    when To_Integer =>
      case T_Expr.TYP is
        when Floats =>  --  Rounding to closest integer (Ada RM 4.6 (33)).
          Compiler.PCode_Emit.Emit_Std_Funct (CD, SF_Round_Float_to_Int);
        when Durations =>
          Compiler.PCode_Emit.Emit_Std_Funct (CD, SF_Duration_to_Int);
        when Ints =>
          null;  --  !! Check range if any !!
        when others =>
          Argument_Type_Not_Supported (CD);
      end case;
      --
    when To_Duration =>
      case T_Expr.TYP is
        when Floats =>
          Compiler.PCode_Emit.Emit_Std_Funct (CD, SF_Float_to_Duration);
        when Ints =>
          Compiler.PCode_Emit.Emit_Std_Funct (CD, SF_Int_to_Duration);
        when Durations =>
          null;  --  !! Check range if any !!
        when others =>
          Argument_Type_Not_Supported (CD);
      end case;
    when Unknown =>
      Error (CD, err_type_conversion_not_supported, "no support for target type " & Type_ID);
  end case;
  Need (CD, RParent, err_closing_parenthesis_missing);
end HAC_Sys.Parser.Type_Conversion;
