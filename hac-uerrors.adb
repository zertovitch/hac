with HAC.Data; use HAC.Data;

with Ada.Text_IO;

package body HAC.UErrors is

  ----------------------------------------------------------------------------

  function ErrorString (Id : Integer) return String is
  begin
    case Id is
    when undefined_identifier =>
      return "undefined identifier";
    when duplicate_identifier =>
      return "multiple definition of an identifier";
    when err_identifier_missing =>
      return "missing an identifier";
    when 3 =>
      return "missing a procedure declaration";
    when err_closing_parenthesis_missing =>
      return "missing closing parenthesis **)""";
    when err_colon_missing =>
      return "missing a colon "":""";
    when err_incorrectly_used_symbol =>
      return "incorrectly used symbol";
    when err_missing_OF =>
      return "missing ""of""";
    when err_missing_an_opening_parenthesis =>
      return "missing an opening parenthesis ""(""";
    when 10 =>
      return "missing identifer; ""array"" or ""record""";
    when 11 =>
      return "-- OPENING BRACKET ""[""";
    when 12 =>
      return "-- CLOSING BRACKET ""]""";
    when err_expecting_dot_dot =>
      return "expecting range symbol: ""..""";
    when err_semicolon_missing =>
      return "missing a semicolon "";""";
    when 15 =>
      return "bad result type for a function";
    when 16 =>
      return "illegal statement start symbol";
    when err_expecting_a_boolean_expression =>
      return "expecting a Boolean expression";
    when 18 =>
      return "control variable of the wrong type";
    when err_first_and_last_must_have_matching_types =>
      return "first and last must have matching types";
    when IS_missing =>
      return "missing ""is""";
    when 21 =>
      return "the number is too large";
    when incorrect_block_name =>
      return "incorrect block name after ""end""";
    when 23 =>
      return "bad type for a case statement";
    when 24 =>
      return "illegal character";
    when illegal_constant_or_constant_identifier =>
      return "illegal constant or constant identifier";
    when err_illegal_array_subscript =>
      return "illegal array subscript (check type)";
    when illegal_array_bounds =>
      return "illegal bounds for an array index";
    when err_indexed_variable_must_be_an_array =>
      return "indexed variable must be an array";
    when err_missing_a_type_identifier =>
      return "missing_a_type_identifier";
    when err_undefined_type =>
      return "undefined type";
    when err_var_with_field_selector_must_be_record =>
      return "var with field selector must be record";
    when err_resulting_type_should_be_Boolean =>
      return "resulting type should be Boolean";
    when err_illegal_type_for_arithmetic_expression =>
      return "illegal type for arithmetic expression";
    when err_mod_requires_integer_arguments =>
      return """mod"" requires integer arguments";
    when err_incompatible_types_for_comparison =>
      return "incompatible types for comparison";
    when err_parameter_types_do_not_match =>
      return "parameter types do not match";
    when variable_missing =>
      return "missing a variable";
    when 38 =>
      return "a string must have one or more char";
    when err_number_of_parameters_do_not_match =>
      return "number of parameters do not match";
    when 40 =>
      return "illegal parameters to ""get""";
    when 41 =>
      return "illegal parameters to ""put""";
    when 42 =>
      return "parameter must be of type Float";
    when parameter_must_be_integer =>
      return "parameter must be of type Integer";
    when err_expected_variable_function_or_constant =>
      return "expected a variable, function or constant";
    when 45 =>
      return "ILLEGAL RETURN STATEMENT FROM MAIN";
    when types_of_assignment_must_match =>
      return "types must match in an assignment";
    when err_case_label_not_same_type_as_case_clause =>
      return "case label not of same type as case clause";
    when err_argument_to_std_function_of_wrong_type =>
      return "argument to std. function of wrong type";
    when 49 =>
      return "the program requires too much storage";
    when 50 =>
      return "illegal symbol for a constant";
    when err_BECOMES_missing =>
      return "missing "":=""";
    when err_THEN_missing =>
      return "missing ""then""";
    when err_IN_missing  =>
      return "missing ""in""";
    when err_closing_LOOP_missing =>
      return "missing closing ""loop""";
    when 56 =>
      return "missing ""begin""";
    when err_END_missing =>
      return "missing ""end""";
    when 58 =>
      return "factor: expecting an id, a constant, ""not"" or ""(""";
    when 59 =>
      return "missing ""return""";
    when 60 =>
      return "control character present in source ";
    when err_missing_record =>
      return "missing ""record""";
    when err_missing_closing_IF =>
      return "missing closing ""if""";
    when 63 =>
      return "missing ""when""";
    when 64 =>
      return "missing the finger ""="">";
    when 65 =>
      return "missing closing ""case""";
    when 66 =>
      return "character delimeter used for string";
    when 67 =>
      return "Ada reserved word; not supported";
    when err_functions_must_return_a_value =>
      return "functions must return a value";
    when 69 =>
      return "must specify ""with small_sp;""";
    when err_use_Small_Sp =>
      return "must specify ""use small_sp;""";
    when 71 =>
      return "expecting an entry";
    when 72 =>
      return "missing expression for delay";
    when 73 =>
      return "delay time must be type float";
    when 74 =>
      return "comma expected";
    when 75 =>
      return "parameter must be of type ""boolean""";
    when 76 =>
      return "expecting ""accept"", ""when"", or entry id";
    when 77 =>
      return "expecting Task.Entry";
    when 78 =>
      return "expecting ""or"" or ""else"" in select";
    when err_expecting_DELAY =>
      return "expecting ""delay""";
    when 80 =>
      return "missing select";
    when 81 =>
      return "program incomplete";
    when OF_instead_of_IS =>
      return "found ""of"", should be ""is""";
    when err_EQUALS_instead_of_BECOMES =>
      return "found ""="", should be "":=""";
    when err_numeric_constant_expected =>
      return "numeric constant expected";
    when others =>
      return "Unknown error Id=" & Integer'Image (Id);
    end case;
  end ErrorString;

  ----------------------------------------------------------------------------

  procedure Error (N : Integer) is  -- Write Error on current line & add To
                                    --TOT ERR
  begin
    cFoundError (N, LineCount, syStart, syEnd, -1);
    Errs (N) := True;
  end Error;

  ----------------------------------------------------------------------------

  procedure EndSkip is -- Skip past part of input
  begin
    SkipFlag := False;
  end EndSkip;

  ----------------------------------------------------------------------------

  procedure Fatal (N : Integer) is   -- internal table overflow
    use Ada.Text_IO;
  begin
    if Errs /= error_free then
      ErrorMsg;
    end if;

    if qDebug then
      Put ("The Compiler TABLE for ");
      case N is
      when IDENTIFIERS_table_overflow =>
        Put ("IDENTIFIERS");
      when PROCEDURES_table_overflow =>
        Put ("PROCEDURES");
      when FLOAT_constants_table_overflow =>
        Put ("FLOAT Constants");
      when 4 =>
        Put ("Arrays");
      when LEVEL_overflow =>
        Put ("LEVELS");
      when OBJECT_overflow =>
        Put ("OBJECT ObjCode");
      when 7 =>
        Put ("Strings");
      when 8 =>
        Put ("TASKS");
      when 9 =>
        Put ("ENTRIES");
      when PATCHING_overflow =>
        Put ("ObjCode PATCHING");
      when others =>
        Put ("N unknown: " & Integer'Image (N));
      end case;
      Put_Line (" is too SMALL");
      New_Line;
      Put_Line (" Please take this output to the maintainers of ");
      Put_Line (" HAC for your installation ");
      New_Line;
      Put_Line (" Fatal termination of HAC");
    end if;
    raise Failure_1_0;
  end Fatal;

  ----------------------------------------------------------------------------

  procedure ErrorMsg is
    use Ada.Text_IO;
    package IIO is new Integer_IO (Integer);
    use IIO;
    K : Integer;
  begin
    K := 0;
    if qDebug then
      New_Line;
      Put_Line (" Error MESSAGE(S)");
    end if;
    if ListingWasRequested then
      New_Line (Listing);
      Put_Line (Listing, " Error MESSAGE(S)");
    end if;
    while Errs /= error_free loop
      while not Errs (K) loop
        K := K + 1;
      end loop;
      if qDebug then
        Put (K, 2);
        Put_Line (":  " & ErrorString (K));
      end if;
      if ListingWasRequested then
        Put (Listing, K, 2);
        Put_Line (Listing, "  " & ErrorString (K));
      end if;
      Errs (K) := False; -- we cancel the K-th sort of error
    end loop;

  end ErrorMsg;

end HAC.UErrors;
