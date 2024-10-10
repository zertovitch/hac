with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.PCode,
     HAC_Sys.Errors;

procedure HAC_Sys.Parser.Type_Conversion  --  Ada RM 4.6
  (CD      : in out Co_Defs.Compiler_Data;
   context : in     Defs.Flow_Context;
   FSys    : in     Defs.Symset;
   Typ_ID  : in     Co_Defs.Identifier_Table_Entry;
   X       : in     Co_Defs.Exact_Subtyp)
is
  use Defs, Helpers, PCode, Errors;
  type Type_Conversion_Kind is
    (To_Float, To_Integer, To_Duration, To_Bools, Unknown);
  kind : Type_Conversion_Kind;
  T_Expr : Co_Defs.Exact_Subtyp;
begin
  --  X is the type of the name just parsed, as in the identifier table.
  --  E.g.:  "Natural (12.34)"  ->  X is from Standard.Natural as entered by the librarian.
  case X.TYP is
    when Floats    => kind := To_Float;
    when Ints      => kind := To_Integer;
    when Durations => kind := To_Duration;
    when Bools     => kind := To_Bools;
    when others    => kind := Unknown;
  end case;
  --
  Expressions.Expression (CD, context, FSys + RParent, T_Expr);
  --  T_Expr is the type of the expression between the parentheses.
  --  E.g.:  "Natural (12.34)"  ->  X.TYP is Floats because of 12.34.
  --
  case kind is
    when To_Float =>
      case T_Expr.TYP is
        when Floats =>
          null;  --  Useless conversion if we have the same named subtype
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
          null;  --  Useless conversion if we have the same named subtype
        when others =>
          Argument_Type_Not_Supported (CD);
      end case;
      Compiler.PCode_Emit.Emit_Lower_Bound_Check (CD, X);
      Compiler.PCode_Emit.Emit_Upper_Bound_Check (CD, X);
      --
    when To_Duration =>
      case T_Expr.TYP is
        when Floats =>
          Compiler.PCode_Emit.Emit_Std_Funct (CD, SF_Float_to_Duration);
        when Ints =>
          Compiler.PCode_Emit.Emit_Std_Funct (CD, SF_Int_to_Duration);
        when Durations =>
          null;  --  Useless conversion if we have the same named subtype
        when others =>
          Argument_Type_Not_Supported (CD);
      end case;
    when To_Bools =>
      if T_Expr.TYP = Bools then
        Remark
          (CD,
           note_redundant_construct,
           "redundant conversion to type ""Boolean""");
      else
        Argument_Type_Not_Supported (CD);
      end if;
    when Unknown =>
      Error (
        CD,
        err_type_conversion_not_supported,
        "no support for target type " & A2S (Typ_ID.name_with_case)
      );
  end case;
  Need (CD, RParent, err_closing_parenthesis_missing);
end HAC_Sys.Parser.Type_Conversion;
