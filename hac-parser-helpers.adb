-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------
--

with HAC.Scanner;

with Ada.Strings.Unbounded;

package body HAC.Parser.Helpers is

  use HAC.Scanner, Ada.Strings.Unbounded;

  procedure Need (
    S       : KeyWSymbol;
    E       : Compile_Error;
    Forgive : KeyWSymbol := Dummy_Symbol
  )
  is
  begin
    if Sy = S then
      InSymbol;
    else
      Error (E);
      if Sy = Forgive then
        InSymbol;
      end if;
    end if;
  end Need;

  procedure Skip (
    FSys : Symset;
    N    : Compile_Error;
    hint : String := ""
  )
  is

    function StopMe return Boolean is
    begin
      return False;
    end StopMe;

  begin
    Error (N, hint);
    --
    SkipFlag := True;
    while not FSys (Sy) loop
      InSymbol;
      if StopMe then
        raise Failure_1_0;
      end if;
    end loop;

    InSymbol;    -- Manuel:  If this InSymbol call is
    -- omitted, the system will get in an
    -- infinite loop on the statement:
    --  put_lin("Typo is on purpose");

    if StopMe then
      raise Failure_1_0;
    end if;
    if SkipFlag then
      EndSkip;
    end if;
  end Skip;

  procedure Skip (
    S    : KeyWSymbol;
    N    : Compile_Error;
    hint : String := ""
  )
  is
  begin
    Skip (Singleton (S), N, hint);
  end Skip;

  procedure Test (
    S1, S2        : Symset;
    N             : Compile_Error;
    stop_on_error : Boolean:= False)
  is
  begin
    if not S1 (Sy) then
      declare
        hint  : Unbounded_String;
        first : Boolean := True;
      begin
        for s in S1'Range loop
          if S1 (s) then
            if not first then
              hint := hint & ", ";
            end if;
            first := False;
            hint := hint & KeyWSymbol'Image (s);
          end if;
        end loop;
        hint := "Found: " & KeyWSymbol'Image (Sy) & "; expected: " & hint;
        if stop_on_error then
          Error (N, stop_on_error => True, hint => To_String (hint));
        end if;
        Skip (S1 + S2, N, To_String (hint));
      end;
    end if;
  end Test;

  After_semicolon : constant Symset :=
    (IDent | TYPE_Symbol | TASK_Symbol => True, others => False) +
    Block_Begin_Symbol;

  Comma_or_colon : constant Symset :=
    Symset'(Comma | Colon => True, others => False);

  procedure Test_Semicolon (FSys : Symset) is
  begin
    if Sy = Semicolon then
      InSymbol;
      Ignore_Extra_Semicolons;
    else
      Error (err_semicolon_missing);
      if Comma_or_colon (Sy) then
        InSymbol;
      end if;
    end if;
    Test (After_semicolon, FSys, err_incorrectly_used_symbol);
  end Test_Semicolon;

  procedure Test_END_Symbol is
  begin
    if Sy = END_Symbol then
      InSymbol;
    else
      Skip (Semicolon, err_END_missing);
    end if;
  end Test_END_Symbol;

  procedure Check_Boolean (T: Types) is
  begin
    --  NB: T = NOTYP was admitted in SmallAda.
    if T /= Bools then
      Error (err_expecting_a_boolean_expression);
    end if;
  end Check_Boolean;

  procedure Ignore_Extra_Semicolons is
  begin
    if Sy = Semicolon then
      Error (err_extra_semicolon_ignored);
      while Sy = Semicolon loop
        InSymbol;
      end loop;
    end if;
  end Ignore_Extra_Semicolons;

  procedure Argument_Type_Not_Supported is
  begin
    Error (err_type_conversion_not_supported, "argument type not supported");
  end Argument_Type_Not_Supported;

  procedure Forbid_Type_Coercion (details: String) is
  begin
    Error (err_int_to_float_coercion, details, stop_on_error => True);
  end Forbid_Type_Coercion;

  function Singleton (s: KeyWSymbol) return Symset is
    res : Symset := Empty_Symset;
  begin
    res (s) := True;
    return res;
  end Singleton;

  procedure Emit_Comparison_Instruction (
    OC        : in out Object_Code_Table;
    LC        : in out Integer;
    Operator  :        Comparison_Operator;
    Base_Type :        Types
  )
  is
  begin
    if Base_Type = Floats then
      case Operator is
        when EQL => Emit (OC, LC, k_EQL_Float);
        when NEQ => Emit (OC, LC, k_NEQ_Float);
        when LSS => Emit (OC, LC, k_LSS_Float);
        when LEQ => Emit (OC, LC, k_LEQ_Float);
        when GTR => Emit (OC, LC, k_GTR_Float);
        when GEQ => Emit (OC, LC, k_GEQ_Float);
      end case;
    elsif Discrete_Typ (Base_Type) then
      case Operator is
        when EQL => Emit (OC, LC, k_EQL_Integer);
        when NEQ => Emit (OC, LC, k_NEQ_Integer);
        when LSS => Emit (OC, LC, k_LSS_Integer);
        when LEQ => Emit (OC, LC, k_LEQ_Integer);
        when GTR => Emit (OC, LC, k_GTR_Integer);
        when GEQ => Emit (OC, LC, k_GEQ_Integer);
      end case;
    else
      raise Internal_error with "Comparison instructions only for atomic types";
    end if;
  end Emit_Comparison_Instruction;

  procedure Emit_Unary_Minus (
    OC        : in out Object_Code_Table;
    LC        : in out Integer;
    Base_Type :        Numeric_Typ
  )
  is
  begin
    case Base_Type is
      when Floats => Emit (OC, LC, k_Unary_MINUS_Float);
      when Ints   => Emit (OC, LC, k_Unary_MINUS_Integer);
    end case;
  end Emit_Unary_Minus;

  procedure Emit_Arithmetic_Binary_Instruction (
    OC        : in out Object_Code_Table;
    LC        : in out Integer;
    Operator  :        Arithmetic_Binary_Operator;
    Base_Type :        Numeric_Typ
  )
  is
  begin
    case Base_Type is
      when Floats =>
        case Operator is
          when Plus    => Emit (OC, LC, k_ADD_Float);
          when Minus   => Emit (OC, LC, k_SUBTRACT_Float);
          when Times   => Emit (OC, LC, k_MULT_Float);
          when Divide  => Emit (OC, LC, k_DIV_Float);
          when Power   => Emit (OC, LC, k_Power_Float);
        end case;
      when Ints   =>
        case Operator is
          when Plus    => Emit (OC, LC, k_ADD_Integer);
          when Minus   => Emit (OC, LC, k_SUBTRACT_Integer);
          when Times   => Emit (OC, LC, k_MULT_Integer);
          when Divide  => Emit (OC, LC, k_DIV_Integer);
          when Power   => Emit (OC, LC, k_Power_Integer);
        end case;
    end case;
  end Emit_Arithmetic_Binary_Instruction;

end HAC.Parser.Helpers;
