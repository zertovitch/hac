with HAC_Sys.Librarian;

with Ada.Strings.Fixed,
     Ada.Text_IO;

package body HAC_Sys.Errors is

  use Defs;

  function Diagnostic_String
    (code   : Defs.Compile_Diagnostic;
     hint_1 : String := "";
     hint_2 : String := "") return String
  is
  begin
    case code is
      when err_undefined_identifier =>
        return "undefined identifier" & (if hint_1 = "" then "" else ": " & hint_1);
      when err_duplicate_identifier =>
        return "duplicate identifier: " & hint_1;
      when err_identifier_missing =>
        return "missing an identifier";
      when err_missing_a_procedure_declaration =>
        return "missing a procedure declaration" & hint_1;
      when err_closing_parenthesis_missing =>
        return "missing closing parenthesis "")""";
      when err_colon_missing =>
        return "missing a colon "":""";
      when err_colon_missing_for_named_statement =>
        return
          "undefined identifier (" & hint_1 & ");" &
          " if a named statement is meant, a colon "":"" would be expected here";
      when err_incorrectly_used_symbol =>
        return "incorrectly used symbol [" & hint_1 & ']';
      when err_missing_OF =>
        return "missing ""of""";
      when err_missing_an_opening_parenthesis =>
        return "missing an opening parenthesis ""(""";
      when err_left_bracket_instead_of_parenthesis =>
        return "found '[' instead of '('";
      when err_right_bracket_instead_of_parenthesis =>
        return "found ']' instead of ')'";
      when err_missing_type_begin_symbol =>
        --  We only show the supported symbols for starting a type definition.
        return "missing ""("", ""array"" or ""record""";
      when err_expecting_double_dot =>
        return "expecting double dot symbol: ""..""";
      when err_semicolon_missing =>
        return "missing a semicolon: "";""";
      when err_duplicate_semicolon =>
        return "duplicate semicolon: "";""";
      when err_extra_right_parenthesis =>
        return "extra ')' ignored";
      when err_bad_result_type_for_a_function =>
        return "this type is not supported for a function's result; " &
               "try a procedure with an ""out"" parameter";
      when err_type_of_return_statement_doesnt_match =>
        return "type of expression in return statement doesn't match: " & hint_1;
      when err_illegal_statement_start_symbol =>
        return "statement cannot start with a " & hint_1;
      when err_expecting_a_boolean_expression =>
        return "expecting a Boolean expression";
      when err_control_variable_of_the_wrong_type =>
        return "control variable must be discrete; found: " & hint_1;
      when err_bounds_type_mismatch =>
        return "bounds in range must be of the same type";
      when err_IS_missing =>
        return "missing ""is""";
      when err_scanner_exponent_too_large =>
        return "total actual exponent is too large:" & hint_1;
      when err_scanner_integer_literal_too_large =>
        return "integer part of number literal is too large; max =" & integer_digits_max'Image & " digits";
      when err_scanner_illegal_character_in_number =>
        return "illegal character in number" & hint_1;
      when err_scanner_negative_exponent_for_integer_literal =>
        return "integer literal with negative exponent; possible: a float with "".0"" such as" & hint_1;
      when err_incorrect_name_after_END =>
        return """end " & hint_1 & ";"" expected here";
      when err_bad_type_for_a_case_statement =>
        return "bad type for a case statement";
      when err_scanner_illegal_character =>
        return "illegal character";
      when err_illegal_constant_or_constant_identifier =>
        return "illegal constant or constant identifier";
      when err_wrong_type_for_array_index =>
        return "type mismatch in array index: " & hint_1;
      when err_too_few_array_indices =>
        return "too few indices in array reference: found" & hint_1 & ", needed" & hint_2;
      when err_too_many_array_indices =>
        return "too many indices in array reference: found" & hint_1 & ", needed" & hint_2;
      when err_illegal_array_bounds =>
        return "illegal bounds for an array index: " & hint_1;
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
        return "incompatible types for comparison: " & hint_1;
      when err_parameter_types_do_not_match =>
        return "parameter types do not match: " & hint_1;
      when err_variable_missing =>
        if hint_1 = "" then
          return "variable expected here";
        else
          return "variable expected as actual for """ & hint_1 & '"';
        end if;
      when err_scanner_character_zero_chars =>
        return "a character literal is of the form 'x'; " &
               "strings are delimited by double quote character";
      when err_number_of_parameters_do_not_match =>
        return "number of parameters do not match" & hint_1;
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
      when err_types_of_assignment_must_match =>
        return "types must match in an assignment: " & hint_1;
      when err_case_label_not_same_type_as_case_clause =>
        return "case label not of same type as case clause: " & hint_1;
      when err_duplicate_case_choice_value =>
        return "duplicate choice value in ""case"" statement";
      when err_argument_to_std_function_of_wrong_type =>
        return "wrong type of argument to operator or standard function: " & hint_1;
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
      when err_missing_closing_LOOP | err_missing_closing_LOOP_2 =>
        return "missing closing ""loop""";
      when err_BEGIN_missing =>
        return "missing ""begin""";
      when err_END_missing =>
        return "missing ""end""";
      when err_primary_unexpected_symbol =>
        return "expected ""("", or: name, number, string, ... (RM 4.4 (7), 4.5(8))";
      when err_RETURN_missing =>
        return "missing ""return""";
      when err_scanner_control_character =>
        return "control character present in source ";
      when err_RECORD_missing =>
        return "missing ""record""";
      when err_missing_closing_IF | err_missing_closing_IF_2 =>
        return "missing closing ""if""";
      when err_WHEN_missing =>
        return "missing ""when"" (must have at least one alternative)";
      when err_FINGER_missing =>
        return "missing the finger ""=>""";
      when err_missing_closing_CASE | err_missing_closing_CASE_2 =>
        return "missing closing ""case""";
      when err_functions_must_return_a_value =>
        return "functions must return a value";
      when err_procedures_cannot_return_a_value =>
        return "procedures cannot return a value (use functions instead)";
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
      when err_scanner_identifier_too_long =>
        return
          "identifier is too long; max =" &
          Integer'Image (identifier_length_max) & " characters";
      when err_scanner_identifier_cannot_end_with_underline =>
        return "identifier cannot end with underline";
      when err_scanner_double_underline_not_permitted =>
        return "double underline not permitted";
      when err_statement_expected =>
        return "statement expected, can be ""null""";
      when err_duplicate_loop_identifier =>
        return "loop identifier already defined above: " & hint_1;
      when err_unexpected_end_of_text =>
        return "unexpected end of text";
      when err_not_yet_implemented =>
        return "construct not yet implemented or supported by HAC: " & hint_1;
      when err_type_conversion_not_supported =>
        return "this type conversion is not supported: " & hint_1;
      when err_numeric_type_coercion =>
        return "numeric types don't match: " & hint_1 & " - please use explicit conversion";
      when err_numeric_type_coercion_operator =>
        return "numeric types don't match (" & hint_1 & "): " &
               hint_2 & " - please use explicit conversion";
      when err_operator_not_defined_for_types =>
        return "operator (" & hint_1 & ") is not defined for that type(s) of operand(s): " & hint_2;
      when err_no_null_functions =>
        return "a function cannot be null; only a procedure can";
      when err_scanner_digit_expected =>
        return "digit expected";
      when err_cannot_modify_constant_or_in_parameter =>
        return "cannot modify a constant or a ""in"" parameter" & hint_1;
      when err_case_others_alone_last =>
        return "the ""others"" choice must appear alone and in the last choice list (RM 5.4 (5))";
      when err_no_X_for_END_X =>
        return "no """ & hint_1 & """ for this ""end " & hint_1 & """";
      when err_END_LOOP_ident_missing =>
        return """end loop " & hint_1 & ";"" expected (RM 5.5 (5))";
      when err_END_LOOP_ident_wrong =>
        return "wrong loop identifier: ""end loop " & hint_1 & ";"" expected";
      when err_general_error =>
        if hint_1 = "" then
          return "syntax error";
        else
          return hint_1;  --  The message is the hint.
        end if;
      when err_string_to_vstring_assignment =>
        return "fixed string assigned to a variable string;" &
               " put a ""+"" in front of the fixed string";
      when err_range_constraint_error =>
        return "error in range constraint: " & hint_1;
      when err_discrete_type_expected =>
        return "discrete type expected";
      when err_membership_test_type_mismatch =>
        return "incompatible types in membership test: " & hint_1;
      when err_string_not_supported_as_parameter =>
        return "string not supported as parameter" &
          " - define a constrained ""subtype S2 is String (1..2)"" or use a VString";
      when err_string_lengths_do_not_match =>
        return "fixed-size string lengths do not match: " & hint_1;
      when err_library_error =>
        return "library error: " & hint_1;
      when err_wrong_unit_name =>
        return "unit name """ & hint_1 & """ expected in this file, found: """ & hint_2 & '"';
      when err_obsolete_hat_name =>
        return "the new name of """ & hint_2 & """ is """ & hint_1 & '"';
      when err_object_used_before_end_own_declaration =>
        return "attempt to use object " & hint_1 & "before end of its own declaration";
      when err_attribute_prefix_invalid =>
        return "invalid prefix for """ & hint_1 & """ attribute";
      when err_attribute_prefix_must_be_discrete_type =>
        return "prefix of """ & hint_1 & """ attribute must be discrete type";
      when err_invalid_dimension_number =>
        return "invalid dimension number for array type, " & hint_1;
      when err_spec_body_mismatch =>
        return "specification vs. body mismatch: " & hint_1;
      when err_incomplete_declaration =>
        return "missing body or full declaration for " & hint_1;
      when err_non_public_entity =>
        return '"' & hint_1 & """ is not a public entity of the package in prefix";
      when err_choices_not_covered =>
        return "all case values shall be covered, either explicitly " &
               "or by ""others"" (RM 5.4 (6))" & hint_1;
      when err_choice_out_of_range =>
        return "choice(s) out of range of case expression";
      when err_mixed_logical_operators =>
        return "mixed logical operators in expression (RM 4.4 (2)) - " &
               "clarify by using parentheses";
      when err_scanner_space_missing_after_number =>
        return
          "space missing here; " &
          "if an identifier was meant, it cannot start with a number";
      when err_assignment_not_allowed_declarative =>
        return
          "assignment (variable := ...) not allowed in declarative part";
      when others =>
        return hint_1;
    end case;
  end Diagnostic_String;

  ----------------------------------------------------------------------------

  function "+" (S : String) return HAT.VString renames HAT."+";

  --  The "[...]" are replaced by the correct identifier.

  repair_table : constant array (Compile_Diagnostic) of Repair_Kit :=
    (
      err_missing_a_procedure_declaration
                                             => (insert,        +"procedure "),
      err_colon_missing                      => (insert,        +": "),
      err_semicolon_missing                  => (insert,        +"; "),
      err_RETURN_missing                     => (insert,        +"return "),
      err_statement_expected                 => (insert,        +"null;"),
      err_IN_missing                         => (insert,        +"in "),
      err_IS_missing                         => (insert,        +"is "),
      err_OF_instead_of_IS                   => (replace_token, +"is"),
      err_THEN_instead_of_Arrow              => (replace_token, +"=>"),
      err_FINGER_missing                     => (insert,        +" => "),
      err_missing_closing_LOOP               => (insert,        +" loop"),
      err_missing_closing_LOOP_2             => (replace_token, +"loop"),
      err_missing_closing_CASE               => (insert,        +" case"),
      err_missing_closing_CASE_2             => (replace_token, +"case"),
      err_missing_closing_IF                 => (insert,        +" if"),
      err_missing_closing_IF_2               => (replace_token, +"if"),
      err_RECORD_missing                     => (insert,        +" record"),
      err_closing_parenthesis_missing        => (insert,        +")"),
      err_END_LOOP_ident_missing             => (insert,        +"[ something... ]"),
      err_scanner_space_missing_after_number => (insert,        +" "),
      err_choices_not_covered =>
        (insert,        +"\twhen others => null;  --  Use with caution...\n"),
      err_incorrect_name_after_END |
      err_END_LOOP_ident_wrong     |
      err_wrong_unit_name          |
      err_obsolete_hat_name         => (replace_token, +"[ something... ]"),
      err_EQUALS_instead_of_BECOMES => (replace_token, +":="),
      err_duplicate_semicolon       => (replace_token, +""),
      err_extra_right_parenthesis   => (replace_token, +""),
      others                        => nothing_to_repair
    );

  procedure Diagnostic
    (CD                  : in out Co_Defs.Compiler_Data;
     code                :        Defs.Compile_Diagnostic;
     hint_1              :        String                 := "";
     hint_2              :        String                 := "";
     severity            :        Error_Severity         := medium;
     location_method     :        Symbol_Location_Method := current_symbol;
     explicit_location   :        Defs.Symbol_Location   := (0, 0, 0))
  is
    use Ada.Strings, Ada.Strings.Fixed, Ada.Text_IO;
    to_be_marked : Symbol_Location;
    --
    procedure Show_to_comp_dump (objNumber : Integer) is
    begin
      if CD.comp_dump_requested then
        Put_Line
         (CD.comp_dump,
          " Error code = " &
          Defs.Compile_Diagnostic'Image (code) &
          " (" &
          Diagnostic_String (code, hint_1, hint_2) &
          ") " &
          " srcNumber=" & to_be_marked.line'Image &
          " charStart=" & to_be_marked.column_start'Image &
          " charEnd="   & to_be_marked.column_stop'Image &
          " objNumber=" & objNumber'Image);
      end if;
    end Show_to_comp_dump;
    --
    updated_repair_kit : Repair_Kit := repair_table (code);
    ub_hint : constant HAT.VString := HAT.To_VString (hint_1);
    use HAT.VStr_Pkg;
    kit : Diagnostic_Kit;
    --
    function Diagnostic_Suffix return String is
      (case kit.diagnostic_kind is
         when warning | note => " [-r" & remark_letter (code) & ']',
         when others         => "");
  begin

    kit.diagnostic_kind :=
      (case code is
         when Compile_Error   => error,
         when Compile_Warning => warning,
         when Compile_Note    => note);

    to_be_marked :=
      (case location_method is
         when current_symbol  => CD.CUD.location,
         when previous_symbol => CD.prev_sy_loc,
         when explicit        => explicit_location);

    Show_to_comp_dump (-1);
    CD.diags (code) := True;

    if code in Compile_Error then
      if severity = minor then
        CD.minor_error_count := CD.minor_error_count + 1;
      else
        CD.error_count := CD.error_count + 1;
      end if;
    end if;

    if CD.trace.pipe = null then
      Put_Line
        (Current_Error,
         (if CD.CUD.source_file_name = "" then
            ""
          else
            To_String (CD.CUD.source_file_name) & ": " &
            Trim (to_be_marked.line'Image,         Left) & ':' &
            Trim (to_be_marked.column_start'Image, Left) & '-' &
            Trim (to_be_marked.column_stop'Image,  Left) & ": ")
         &
         Diagnostic_Prefix (kit.diagnostic_kind) &
         Diagnostic_String (code, hint_1, hint_2) &
         Diagnostic_Suffix);
    else

      case code is
        when err_incorrect_name_after_END =>
          if hint_1 = "" then
            updated_repair_kit.repair_kind := none;
          else
            updated_repair_kit.alternative := ub_hint;
          end if;
        when err_END_LOOP_ident_missing =>
          updated_repair_kit.alternative := ' ' & ub_hint;
        when err_END_LOOP_ident_wrong |
             err_obsolete_hat_name
          =>
          updated_repair_kit.alternative := ub_hint;
        when err_wrong_unit_name =>
          updated_repair_kit.alternative :=
            HAT.To_VString (Librarian.Ada_RM_Casing (HAT.To_String (ub_hint)));
        when others =>
          null;
      end case;

      Repair_Kit (kit) := updated_repair_kit;
      kit.message   := To_Unbounded_String (Diagnostic_String (code, hint_1, hint_2));
      kit.file_name := To_Unbounded_String (Co_Defs.Get_Source_Name (CD.CUD));
      kit.location  := to_be_marked;

      CD.trace.pipe (kit);
    end if;
    --  Uncomment the next line for getting a nice trace-back of 1st error.
    --  raise Constraint_Error;
    --
    if code in Compile_Error and then severity = major then
      --  Useless to continue compiling, the source is FUBAR
      --  and the compiler close to total confusion!
      raise Compilation_abandoned;
    end if;
  end Diagnostic;

  procedure Error
    (CD                  : in out Co_Defs.Compiler_Data;
     code                :        Defs.Compile_Error;
     hint_1              :        String                 := "";
     hint_2              :        String                 := "";
     severity            :        Error_Severity         := medium;
     location_method     :        Symbol_Location_Method := current_symbol;
     explicit_location   :        Defs.Symbol_Location   := (0, 0, 0))
  is
  begin
    Diagnostic (CD, code, hint_1, hint_2, severity, location_method, explicit_location);
  end Error;

  procedure Remark
    (CD                  : in out Co_Defs.Compiler_Data;
     code                :        Defs.Compile_Remark;
     hint_1              :        String                 := "";
     hint_2              :        String                 := "";
     location_method     :        Symbol_Location_Method := current_symbol;
     explicit_location   :        Defs.Symbol_Location   := (0, 0, 0);
     remark_made         :    out Boolean)
  is
  begin
    remark_made := False;
    if CD.remarks (code) then
      --  Remark (Note or Warning) is optional.
      --  Severity (minor passed here) does nothing for remarks.
      Diagnostic (CD, code, hint_1, hint_2, minor, location_method, explicit_location);
      remark_made := True;
    end if;
  end Remark;

  procedure Remark
    (CD                  : in out Co_Defs.Compiler_Data;
     code                :        Defs.Compile_Remark;
     hint_1              :        String                 := "";
     hint_2              :        String                 := "";
     location_method     :        Symbol_Location_Method := current_symbol;
     explicit_location   :        Defs.Symbol_Location   := (0, 0, 0))
  is
    dummy : Boolean;
  begin
    Remark (CD, code, hint_1, hint_2, location_method, explicit_location, dummy);
  end Remark;

  ----------------------------------------------------------------------------

  procedure Fatal (N : Table_OverFlow_Error; Current_Error_Output : Boolean := False) is
    use Ada.Text_IO;
  begin
    if Current_Error_Output then
      Put (Current_Error, "The Compiler TABLE for: *");
      case N is
        when FLOAT_CONSTANTS  => Put (Current_Error, "Float Constants");
        when STRING_CONSTANTS => Put (Current_Error, "Strings Constants");
        when Object_Code      => Put (Current_Error, "Object Code");
        when PATCHING         => Put (Current_Error, "ObjCode PATCHING");
        when others           => Put (Current_Error, Table_OverFlow_Error'Image (N));
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

  procedure Compilation_Diagnostics_Summary (CD : Co_Defs.Compiler_Data) is
    use Ada.Text_IO;
    procedure Summary_Line (s : String) is
    begin
      if CD.comp_dump_requested then
        Put_Line (CD.comp_dump, s);
      end if;
      if CD.listing_requested then
        Put_Line (CD.listing, s);
      end if;
    end Summary_Line;
  begin
    Summary_Line ("");
    for K in CD.diags'Range loop
      if K = Compile_Error'First then
        Summary_Line ("==== Error Message(s) ====");
      elsif K = Compile_Warning'First then
        Summary_Line ("==== Warning(s) ==========");
      end if;
      if CD.diags (K) then
        Summary_Line (K'Image & ":  " & Diagnostic_String (K));
      end if;
    end loop;
  end Compilation_Diagnostics_Summary;

end HAC_Sys.Errors;
