with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.PCode,
     HAC_Sys.Errors;

procedure HAC_Sys.Parser.Type_Conversion (  --  Ada RM 4.6
  CD      : in out Co_Defs.Compiler_Data;
  Level   :        Defs.Nesting_level;
  FSys    :        Defs.Symset;
  Typ_ID  : in     Co_Defs.IdTabEntry;
  X       : in     Co_Defs.Exact_Subtyp
)
is
  use Defs, Helpers, PCode, Errors;
  use type HAC_Integer;
  type Type_Conversion_Kind is (To_Float, To_Integer, To_Duration, Unknown);
  kind : Type_Conversion_Kind;
  T_Expr : Co_Defs.Exact_Subtyp;
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
          null;  --  !! Spot useless conversions !!
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
          null;  --  !! Spot useless conversions (identical range) !!
        when others =>
          Argument_Type_Not_Supported (CD);
      end case;
      if X.Discrete_First > HAC_Integer'First then
        Compiler.PCode_Emit.Emit_3
          (CD, k_Check_Lower_Bound, X.Discrete_First, Typen'Pos (X.TYP), Operand_3_Type (X.Ref));
      end if;
      if X.Discrete_Last < HAC_Integer'Last then
        Compiler.PCode_Emit.Emit_3
          (CD, k_Check_Upper_Bound, X.Discrete_Last, Typen'Pos (X.TYP), Operand_3_Type (X.Ref));
      end if;
      --
    when To_Duration =>
      case T_Expr.TYP is
        when Floats =>
          Compiler.PCode_Emit.Emit_Std_Funct (CD, SF_Float_to_Duration);
        when Ints =>
          Compiler.PCode_Emit.Emit_Std_Funct (CD, SF_Int_to_Duration);
        when Durations =>
          null;  --  !! Spot useless conversions !!
        when others =>
          Argument_Type_Not_Supported (CD);
      end case;
    when Unknown =>
      Error (
        CD,
        err_type_conversion_not_supported,
        "no support for target type " & A2S (Typ_ID.name_with_case)
      );
  end case;
  Need (CD, RParent, err_closing_parenthesis_missing);
end HAC_Sys.Parser.Type_Conversion;
