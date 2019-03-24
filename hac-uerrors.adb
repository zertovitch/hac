with HAC.Data; use HAC.Data;

with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;

package body HAC.UErrors is

  ----------------------------------------------------------------------------

  function Error_String (code: Error_code; hint: String:= "") return String is
  begin
    case code is
      when err_undefined_identifier =>
        return "undefined identifier";
      when err_duplicate_identifier =>
        return "multiple definition of an identifier";
      when err_identifier_missing =>
        return "missing an identifier";
      when err_missing_a_procedure_declaration =>
        return "missing a procedure declaration";
      when err_closing_parenthesis_missing =>
        return "missing closing parenthesis "")""";
      when err_colon_missing =>
        return "missing a colon "":""";
      when err_colon_missing_for_named_statement =>
        return
          "undefined identifier;" &
          " if a named statement is meant, a colon "":"" would be expected here";
      when err_incorrectly_used_symbol =>
        return "incorrectly used symbol";
      when err_missing_OF =>
        return "missing ""of""";
      when err_missing_an_opening_parenthesis =>
        return "missing an opening parenthesis ""(""";
      when err_missing_ARRAY_RECORD_or_ident =>
        return "missing identifer; ""array"" or ""record""";
      when err_expecting_dot_dot =>
        return "expecting range symbol: ""..""";
      when err_SEMICOLON_missing =>
        return "missing a semicolon "";""";
      when err_bad_result_type_for_a_function =>
        return "bad result type for a function";
      when err_illegal_statement_start_symbol =>
        return "illegal statement start symbol";
      when err_expecting_a_boolean_expression =>
        return "expecting a Boolean expression";
      when err_control_variable_of_the_wrong_type =>
        return "control variable of the wrong type";
      when err_first_and_last_must_have_matching_types =>
        return "first and last must have matching types";
      when err_IS_missing =>
        return "missing ""is""";
      when err_number_too_large =>
        return "the number is too large";
      when err_incorrect_block_name =>
        return """end " & hint & ";"" expected here";
      when err_bad_type_for_a_case_statement =>
        return "bad type for a case statement";
      when err_illegal_character =>
        return "illegal character";
      when err_illegal_constant_or_constant_identifier =>
        return "illegal constant or constant identifier";
      when err_illegal_array_subscript =>
        return "illegal array subscript (check type)";
      when err_illegal_array_bounds =>
        return "illegal bounds for an array index";
      when err_indexed_variable_must_be_an_array =>
        return "indexed variable must be an array";
      when err_missing_a_type_identifier =>
        return "identifier is not a type";
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
      when err_variable_missing =>
        return "missing a variable";
      when err_character_zero_chars =>
        return "a character literal is of the form 'x'; " &
               "strings are delimited by double quote character";
      when err_number_of_parameters_do_not_match =>
        return "number of parameters do not match";
      when err_illegal_parameters_to_Get =>
        return "illegal parameters to ""Get""";
      when err_illegal_parameters_to_Put =>
        return "illegal parameters to ""Put""";
      when err_parameter_must_be_of_type_Float =>
        return "parameter must be of type Float";
      when err_parameter_must_be_Integer =>
        return "parameter must be of type Integer";
      when err_expected_variable_function_or_constant =>
        return "expected a variable, function or constant";
      when err_illegal_return_statement_from_main =>
        return "ILLEGAL RETURN STATEMENT FROM MAIN";
      when err_types_of_assignment_must_match =>
        return "types must match in an assignment";
      when err_case_label_not_same_type_as_case_clause =>
        return "case label not of same type as case clause";
      when err_duplicate_case_choice_value =>
        return "duplicate choice value in ""case"" instruction";
      when err_argument_to_std_function_of_wrong_type =>
        return "argument to std. function of wrong type";
      when err_stack_size =>
        return "the program requires too much storage";
      when err_illegal_symbol_for_a_constant =>
        return "illegal symbol for a constant";
      when err_BECOMES_missing =>
        return "missing "":=""";
      when err_THEN_missing =>
        return "missing ""then""";
      when err_IN_missing  =>
        return "missing ""in""";
      when err_closing_LOOP_missing =>
        return "missing closing ""loop""";
      when err_BEGIN_missing =>
        return "missing ""begin""";
      when err_END_missing =>
        return "missing ""end""";
      when err_factor_unexpected_symbol =>
        return "factor: expecting an id, a constant, ""not"" or ""(""";
      when err_RETURN_missing =>
        return "missing ""return""";
      when err_control_character =>
        return "control character present in source ";
      when err_RECORD_missing =>
        return "missing ""record""";
      when err_missing_closing_IF =>
        return "missing closing ""if""";
      when err_WHEN_missing =>
        return "missing ""when""";
      when err_FINGER_missing =>
        return "missing the finger ""=>""";
      when err_missing_closing_CASE =>
        return "missing closing ""case""";
      when err_character_delimeter_used_for_string =>
        return "character delimeter used for string; " &
               "strings are delimited by double quote character";
      when err_Ada_reserved_word =>
        return "Ada reserved word; not supported";
      when err_functions_must_return_a_value =>
        return "functions must return a value";
      when err_WITH_Small_Sp =>
        return "must specify ""with hac_pack;"" here";
      when err_use_Small_Sp =>
        return "must specify ""use hac_pack;"" here";
      when err_missing_an_entry =>
        return "expecting an entry";
      when err_missing_expression_for_delay =>
        return "missing expression for ""delay""";
      when err_wrong_type_in_DELAY =>
        return "delay time must be type Float";
      when err_COMMA_missing =>
        return "comma expected";
      when err_parameter_must_be_of_type_Boolean =>
        return "parameter must be of type Boolean";
      when err_expecting_accept_when_or_entry_id =>
        return "expecting ""accept"", ""when"", or entry id";
      when err_expecting_task_entry =>
        return "expecting Task.Entry";
      when err_expecting_OR_or_ELSE_in_SELECT =>
        return "expecting ""or"" or ""else"" in select";
      when err_expecting_DELAY =>
        return "expecting ""delay""";
      when err_SELECT_missing =>
        return "missing ""select""";
      when err_program_incomplete =>
        return "program incomplete";
      when err_OF_instead_of_IS =>
        return "found ""of"", should be ""is""";
      when err_EQUALS_instead_of_BECOMES =>
        return "found ""="", should be "":=""";
      when err_numeric_constant_expected =>
        return "numeric constant expected";
      when err_identifier_too_long =>
        return "identifier """ & hint & "..."" is too long";
      when err_statement_expected =>
        return "statement expected, can be ""null""";
      when err_duplicate_label =>
        return "label already defined: " & hint;
      when err_invalid_power_operands =>
        return "invalid operand types for the ""**"" operator";
      when err_unexpected_end_of_text =>
        return "unexpected end of text";
      when err_not_yet_implemented =>
        return "construct not yet correctly implemented or supported by HAC";
      -- when others =>
      --   return "Unknown error Id=" & Integer'Image (Id);
    end case;
  end Error_String;

  ----------------------------------------------------------------------------

  procedure Error (
    code          : Error_code;
    hint          : String      := "";
    stop_on_error : Boolean     := False
  )
  is
  -- Write Error on current line & add To TOT ERR (?)
    use Ada.Text_IO;
  begin
    cFoundError (code, Line_Count, syStart, syEnd, -1, hint);
    Errs (code) := True;
    Err_Count := Err_Count + 1;
    if HAC.Data.current_error_pipe = null then
      Put_Line(
        Current_Error,
        -- !! File name here
        Trim(Integer'Image(Line_Count),Left) & ':' &
        Trim(Integer'Image(syStart),Left) & '-' &
        Trim(Integer'Image(syEnd),Left) & ": " &
        Error_String (code, hint)
      );
    else
      HAC.Data.current_error_pipe (
        message   => Error_String (code, hint),
        file_name => To_String (current_compiler_file_name),
        line      => Line_Count,
        column_a  => syStart,
        column_z  => syEnd,
        kind      => error
      );
    end if;
    --  Uncomment the next line for getting a nice trace-back of 1st error.
    --  raise Constraint_Error;
    if stop_on_error then
      raise Compilation_abandoned;
    end if;
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
      Put ("The Compiler TABLE for: *");
      case N is
        when IDENTIFIERS_table_overflow =>
          Put ("IDENTIFIERS");
        when PROCEDURES_table_overflow =>
          Put ("PROCEDURES");
        when FLOAT_constants_table_overflow =>
          Put ("FLOAT Constants");
        when ARRAYS_table_overflow =>
          Put ("Arrays");
        when LEVEL_overflow =>
          Put ("LEVELS");
        when OBJECT_overflow =>
          Put ("OBJECT ObjCode");
        when STRING_table_overflow =>
          Put ("Strings");
        when TASKS_table_overflow =>
          Put ("TASKS");
        when ENTRIES_table_overflow =>
          Put ("ENTRIES");
        when PATCHING_overflow =>
          Put ("ObjCode PATCHING");
        when others =>
          Put ("N unknown: " & Integer'Image (N));
      end case;
      Put_Line ("* is too SMALL");
      New_Line;
      Put_Line (" Please take this output to the maintainers of ");
      Put_Line (" HAC for your installation ");
      New_Line;
      Put_Line (" Fatal termination of HAC");
    end if;
    raise Failure_1_0 with
      "HAC.UErrors.Fatal:" &
      " Internal HAC compiler error (a table is too small). Details with qDebug=True";
  end Fatal;

  ----------------------------------------------------------------------------

  procedure ErrorMsg is
    use Ada.Text_IO;
    package IIO is new Integer_IO (Integer);
    use IIO;
    K : Error_code:= Error_code'First;
  begin
    if qDebug then
      New_Line;
      Put_Line ("==== Error MESSAGE(S) ====");
    end if;
    if Listing_Was_Requested then
      New_Line (Listing);
      Put_Line (Listing, "==== Error MESSAGE(S) ====");
    end if;
    while Errs /= error_free loop -- NB: Ouch! A single loop would be sufficient !!
      while not Errs (K) loop
        K := Error_code'Succ(K);
      end loop;
      if qDebug then
        Put_Line (Error_code'Image(K) & ":  " & Error_String (K, ""));
        -- Should be Error_hint(K,n) !!
      end if;
      if Listing_Was_Requested then
        Put_Line (Listing, Error_code'Image(K) & "  " & Error_String (K, ""));
        -- Should be Error_hint(K,n) !!
      end if;
      Errs (K) := False; -- we cancel the K-th sort of error
    end loop;

  end ErrorMsg;

end HAC.UErrors;
