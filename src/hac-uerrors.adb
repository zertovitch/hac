with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Text_IO;

package body HAC.UErrors is

  function Error_String (code: HAC.Data.Compile_Error; hint: String:= "") return String is
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
        return "incorrectly used symbol [" & hint & ']';
      when err_missing_OF =>
        return "missing ""of""";
      when err_missing_an_opening_parenthesis =>
        return "missing an opening parenthesis ""(""";
      when err_left_bracket_instead_of_parenthesis =>
        return "found '[' instead of '('";
      when err_right_bracket_instead_of_parenthesis =>
        return "found ']' instead of ')'";
      when err_missing_ARRAY_RECORD_or_ident =>
        return "missing identifer, ""array"" or ""record""";
      when err_expecting_double_dot =>
        return "expecting double dot symbol: ""..""";
      when err_semicolon_missing =>
        return "missing a semicolon "";""";
      when err_extra_semicolon_ignored =>
        return "extra "";"" ignored";
      when err_bad_result_type_for_a_function =>
        return "bad result type for a function";
      when err_type_of_return_statement_doesnt_match =>
        return "type of expression in return statement doesn't match: " & hint;
      when err_illegal_statement_start_symbol =>
        return "statement cannot start with a " & hint;
      when err_expecting_a_boolean_expression =>
        return "expecting a Boolean expression";
      when err_control_variable_of_the_wrong_type =>
        return "control variable of the wrong type: must be discrete";
      when err_first_and_last_must_have_matching_types =>
        return "first and last must have matching types";
      when err_IS_missing =>
        return "missing ""is""";
      when err_number_too_large =>
        return "number is too large: total actual exponent is " & hint;
      when err_illegal_character_in_number =>
        return "illegal character in number" & hint;
      when err_negative_exponent_for_integer_literal =>
        return "integer literal with negative exponent; suggestion: a float with "".0"" such as" & hint;
      when err_incorrect_block_name =>
        return """end " & hint & ";"" expected here";
      when err_bad_type_for_a_case_statement =>
        return "bad type for a case statement";
      when err_illegal_character =>
        return "illegal character";
      when err_illegal_constant_or_constant_identifier =>
        return "illegal constant or constant identifier";
      when err_illegal_array_subscript =>
        return "type mismatch in array subscript: " & hint;
      when err_illegal_array_bounds =>
        return "illegal bounds for an array index: " & hint;
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
        return "incompatible types for comparison: " & hint;
      when err_parameter_types_do_not_match =>
        return "parameter types do not match: " & hint;
      when err_variable_missing =>
        return "missing a variable";
      when err_character_zero_chars =>
        return "a character literal is of the form 'x'; " &
               "strings are delimited by double quote character";
      when err_number_of_parameters_do_not_match =>
        return "number of parameters do not match" & hint;
      when err_illegal_parameters_to_Get =>
        return "illegal parameters to ""Get""";
      when err_illegal_parameters_to_Put =>
        return "illegal parameters to ""Put""";
      when err_parameter_must_be_of_type_Float =>
        return "parameter must be of type Float";
      when err_parameter_must_be_Integer =>
        return "parameter must be of type Integer";
      when err_expected_constant_function_variable_or_subtype =>
        return "expected a constant, function, variable or subtype name";
      when err_illegal_return_statement_from_main =>
        return "ILLEGAL RETURN STATEMENT FROM MAIN";
      when err_types_of_assignment_must_match =>
        return "types must match in an assignment: " & hint;
      when err_case_label_not_same_type_as_case_clause =>
        return "case label not of same type as case clause";
      when err_duplicate_case_choice_value =>
        return "duplicate choice value in ""case"" instruction";
      when err_argument_to_std_function_of_wrong_type =>
        return "wrong type of argument to standard function: " & hint;
      when err_stack_size =>
        return "the program requires too much storage";
      when err_illegal_symbol_for_a_number_declaration =>
        return "illegal symbol for a number declaration or a literal";
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
        return "missing ""when"" (must have at least one alternative)";
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
      when err_procedures_cannot_return_a_value =>
        return "procedures cannot return a value (use functions instead)";
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
      when err_THEN_instead_of_Arrow =>
        return "found ""then"", should be ""=>""";
      when err_EQUALS_instead_of_BECOMES =>
        return "found ""="", should be "":=""";
      when err_numeric_constant_expected =>
        return "numeric constant expected";
      when err_identifier_too_long =>
        return "identifier """ & hint & "..."" is too long";
      when err_identifier_cannot_end_with_underline =>
        return "identifier cannot end with underline";
      when err_double_underline_not_permitted =>
        return "double underline not permitted";
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
      when err_type_conversion_not_supported =>
        return "this type conversion is not supported: " & hint;
      when err_numeric_type_coercion =>
        return "numeric types don't match: " & hint & " - please use explicit conversion";
      when err_numeric_type_coercion_operator =>
        return "numeric types don't match (" &
               hint (hint'First) & "): " & hint (hint'First + 1 .. hint'Last) &
               " - please use explicit conversion";
      when err_operator_not_defined_for_types =>
        return "operator (" & hint (hint'First) &
               ") is not defined for those operand types: " &
               hint (hint'First + 1 .. hint'Last);
      when err_no_null_functions =>
        return "a function cannot be null; only a procedure can";
      when err_digit_expected =>
        return "digit expected";
      when err_cannot_modify_constant_or_in_parameter =>
        return "cannot modify a constant or a ""in"" parameter" & hint;
      when err_case_others_alone_last =>
        return "the ""others"" choice must appear alone and in the last choice list (RM 5.4 (5))";
      when err_END_LOOP_ident_missing =>
        return """end loop " & hint & ";"" expected (RM 5.5 (5))";
      when err_END_LOOP_ident_wrong =>
        return "wrong loop identifier: ""end loop " & hint & ";"" expected";
      when err_syntax_error =>
        return "syntax error " & hint;
      when err_string_to_vstring_assignment =>
        return "fixed string assigned to a variable string;" &
               " put a ""+"" in front of the fixed string";
    end case;
  end Error_String;

  ----------------------------------------------------------------------------

  function "+" (S : String) return VString renames To_VString;

  repair_table : constant array (Compile_Error) of Repair_kit :=
    (
      err_WITH_Small_Sp               => (insert_line,   +"with HAC_Pack;  use HAC_Pack;"),
      err_use_Small_Sp                => (insert,        +"use HAC_Pack; "),
      err_missing_a_procedure_declaration
                                      => (insert,        +"procedure "),
      err_colon_missing               => (insert,        +": "),
      err_semicolon_missing           => (insert,        +"; "),
      err_RETURN_missing              => (insert,        +"return "),
      err_statement_expected          => (insert,        +"null;"),
      err_IN_missing                  => (insert,        +"in "),
      err_IS_missing                  => (insert,        +"is "),
      err_OF_instead_of_IS            => (replace_token, +"is"),
      err_THEN_instead_of_Arrow       => (replace_token, +"=>"),
      err_FINGER_missing              => (insert,        +" => "),
      err_closing_LOOP_missing        => (insert,        +" loop"),
      err_missing_closing_CASE        => (insert,        +" case"),
      err_missing_closing_IF          => (insert,        +" if"),
      err_closing_parenthesis_missing => (insert,        +")"),
      err_incorrect_block_name        => (replace_token, +"[Error() puts identifier]"),
      err_END_LOOP_ident_missing      => (insert,        +"[Error() puts identifier]"),
      err_END_LOOP_ident_wrong        => (replace_token, +"[Error() puts identifier]"),
      err_EQUALS_instead_of_BECOMES   => (replace_token, +":="),
      others                          => nothing_to_repair
    );

  procedure Error (
    CD   : in out HAC.Compiler.Compiler_Data;
    code :        HAC.Data.Compile_Error;
    hint :        String      := "";
    stop :        Boolean     := False
  )
  is
    use Ada.Text_IO;
    --
    procedure Show_to_comp_dump (
       srcNumber, charStart, charEnd, objNumber : Integer;
       hint_for_dump: String
    )
    is
    begin
      if CD.comp_dump_requested then
        Put_Line
         (CD.comp_dump,
          " Error code = " &
          Compile_Error'Image (code) &
          " (" &
          Error_String (code, hint_for_dump) &
          ") " &
          " srcNumber=" &
          Integer'Image (srcNumber) &
          " charStart=" &
          Integer'Image (charStart) &
          " charEnd=" &
          Integer'Image (charEnd) &
          " objNumber=" &
          Integer'Image (objNumber));
      end if;
    end Show_to_comp_dump;
    --
    updated_repair_kit : Repair_kit := repair_table (code);
    ub_hint : constant VString := To_VString (hint);
    use VStrings_Pkg;
  begin
    Show_to_comp_dump (CD.Line_Count, CD.syStart, CD.syEnd, -1, hint);
    CD.Errs (code) := True;
    CD.Err_Count := CD.Err_Count + 1;
    if CD.error_pipe = null then
      Put_Line(
        Current_Error,
        --  !! Ada "file" name here
        Trim(Integer'Image(CD.Line_Count),Left) & ':' &
        Trim(Integer'Image(CD.syStart),Left) & '-' &
        Trim(Integer'Image(CD.syEnd),Left) & ": " &
        Error_String (code, hint)
      );
    else
      case code is
        when err_incorrect_block_name =>
          if hint = "" then
            updated_repair_kit.kind := none;
          else
            updated_repair_kit.text := ub_hint;
          end if;
        when err_END_LOOP_ident_missing =>
          updated_repair_kit.text := ' ' & ub_hint;
        when err_END_LOOP_ident_wrong =>
          updated_repair_kit.text := ub_hint;
        when others =>
          null;
      end case;
      CD.error_pipe (
        message   => Error_String (code, hint),
        file_name => HAC.Compiler.Get_Current_Source_Name (CD),
        line      => CD.Line_Count,
        column_a  => CD.syStart,
        column_z  => CD.syEnd,
        kind      => error,
        repair    => updated_repair_kit
      );
    end if;
    --  Uncomment the next line for getting a nice trace-back of 1st error.
    --  raise Constraint_Error;
    if stop then
      raise Compilation_abandoned;
    end if;
  end Error;

  ----------------------------------------------------------------------------

  procedure Fatal (N: Table_OverFlow_Error; Current_Error_Output : Boolean := False) is
    use Ada.Text_IO;
  begin
    if Current_Error_Output then
      Put (Current_Error, "The Compiler TABLE for: *");
      case N is
        when FLOAT_CONSTANTS =>
          Put (Current_Error, "FLOAT Constants");
        when OBJECTS =>
          Put (Current_Error, "OBJECT ObjCode");
        when STRING_CONSTANTS =>
          Put (Current_Error, "Strings Constants");
        when PATCHING =>
          Put ("ObjCode PATCHING");
        when others =>
          Put (Current_Error, Table_OverFlow_Error'Image (N));
      end case;
      Put_Line (Current_Error, "* is too SMALL");
      New_Line (Current_Error);
      Put_Line (Current_Error, " Please take this output to the maintainers of ");
      Put_Line (Current_Error, " HAC for your installation ");
      New_Line (Current_Error);
      Put_Line (Current_Error, " Fatal termination of HAC");
    end if;
    --
    raise Failure_1_0 with
      "HAC.UErrors.Fatal:" &
      " internal HAC compiler error." &
      " The table for " &
      Table_OverFlow_Error'Image (N) &
      " is too small. More details with qDebug=True";
  end Fatal;

  ----------------------------------------------------------------------------

  procedure Compilation_Errors_Summary (CD : HAC.Compiler.Compiler_Data) is
    use Ada.Text_IO;
  begin
    if CD.comp_dump_requested then
      New_Line (CD.comp_dump);
      Put_Line (CD.comp_dump, "==== Error MESSAGE(S) ====");
    end if;
    if CD.listing_requested then
      New_Line (CD.listing);
      Put_Line (CD.listing, "==== Error MESSAGE(S) ====");
    end if;
    for K in CD.Errs'Range loop
      if CD.Errs (K) then
        if CD.comp_dump_requested then
          Put_Line (Compile_Error'Image(K) & ":  " & Error_String (K, ""));
          -- Should be Error_hint(K,n) !!
        end if;
        if CD.listing_requested then
          Put_Line (CD.listing, Compile_Error'Image(K) & "  " & Error_String (K, ""));
          -- Should be Error_hint(K,n) !!
        end if;
      end if;
    end loop;
  end Compilation_Errors_Summary;

end HAC.UErrors;
