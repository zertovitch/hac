-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package HAC.UErrors is

  type Compile_Error is (
    err_undefined_identifier,
    err_duplicate_identifier,
    err_identifier_missing,
    err_missing_a_procedure_declaration,
    err_closing_parenthesis_missing,
    err_colon_missing,
    err_colon_missing_for_named_statement,
    err_incorrectly_used_symbol,
    err_missing_OF,
    err_missing_an_opening_parenthesis,
    err_left_bracket_instead_of_parenthesis,
    err_right_bracket_instead_of_parenthesis,
    err_missing_ARRAY_RECORD_or_ident,
    err_expecting_double_dot,
    err_semicolon_missing,
    err_extra_semicolon_ignored,
    err_bad_result_type_for_a_function,
    err_illegal_statement_start_symbol,
    err_expecting_a_boolean_expression,
    err_control_variable_of_the_wrong_type,
    err_first_and_last_must_have_matching_types,
    err_IS_missing,
    err_number_too_large,
    err_illegal_character_in_number,
    err_negative_exponent_for_integer_literal,
    err_incorrect_block_name,
    err_bad_type_for_a_case_statement,
    err_illegal_character,
    err_illegal_constant_or_constant_identifier,
    err_illegal_array_subscript,
    err_illegal_array_bounds,
    err_indexed_variable_must_be_an_array,
    err_missing_a_type_identifier,
    err_undefined_type,
    err_var_with_field_selector_must_be_record,
    err_resulting_type_should_be_Boolean,
    err_illegal_type_for_arithmetic_expression,
    err_mod_requires_integer_arguments,
    err_incompatible_types_for_comparison,
    err_parameter_types_do_not_match,
    err_variable_missing,
    err_character_zero_chars,
    err_number_of_parameters_do_not_match,
    err_illegal_parameters_to_Get,
    err_illegal_parameters_to_Put,
    err_parameter_must_be_of_type_Float,
    err_parameter_must_be_Integer,
    err_expected_constant_function_variable_or_subtype,
    err_illegal_return_statement_from_main,
    err_types_of_assignment_must_match,
    err_case_label_not_same_type_as_case_clause,
    err_duplicate_case_choice_value,
    err_argument_to_std_function_of_wrong_type,
    err_stack_size,
    err_illegal_symbol_for_a_number_declaration,
    err_BECOMES_missing,
    err_THEN_missing,
    err_IN_missing,
    err_closing_LOOP_missing,
    err_BEGIN_missing,
    err_END_missing,
    err_factor_unexpected_symbol,
    err_RETURN_missing,
    err_control_character,
    err_RECORD_missing,
    err_missing_closing_IF,
    err_WHEN_missing,
    err_FINGER_missing,
    err_missing_closing_CASE,
    err_character_delimeter_used_for_string,
    err_Ada_reserved_word,
    err_functions_must_return_a_value,
    err_procedures_cannot_return_a_value,
    err_WITH_Small_Sp,
    err_use_Small_Sp,
    err_missing_an_entry,
    err_missing_expression_for_delay,
    err_wrong_type_in_DELAY,
    err_COMMA_missing,
    err_expecting_accept_when_or_entry_id,
    err_expecting_task_entry,
    err_expecting_OR_or_ELSE_in_SELECT,
    err_expecting_DELAY,
    err_SELECT_missing,
    err_program_incomplete,
    -- These errors messages are new in HAC and weren't in SmallAda
    err_OF_instead_of_IS,
    err_EQUALS_instead_of_BECOMES,
    err_numeric_constant_expected,
    err_identifier_too_long,
    err_identifier_cannot_end_with_underline,    --  2020-04-09
    err_double_underline_not_permitted,          --  2020-04-09
    err_statement_expected,
    err_duplicate_label,
    err_invalid_power_operands,                  --  2018-03-18
    err_unexpected_end_of_text,                  --  2018-04-01
    err_not_yet_implemented,                     --  2019-03-24
    err_type_conversion_not_supported,           --  2020-03-31
    err_int_to_float_coercion,                   --  2020-04-06
    err_operator_not_defined_for_types,          --  2020-04-06
    err_no_null_functions,                       --  2020-04-10
    err_digit_expected,
    err_cannot_modify_constant_or_in_parameter,
    err_case_others_alone_last,
    err_END_LOOP_ident_missing,
    err_END_LOOP_ident_wrong,
    err_syntax_error                             --  A classic one. "Too long to explain more..."
  );

  type Message_kind is (error, warning, note, style);

  type Repair_kind is (none, insert, insert_line, replace_token);

  has_new_line : array (Repair_kind) of Boolean := (insert_line => True, others => False);

  type Repair_kit is tagged record
    kind : Repair_kind      := none;
    text : Unbounded_String := Null_Unbounded_String;
  end record;

  nothing_to_repair : constant Repair_kit := (none, Null_Unbounded_String);

  --  See current_error_pipe in HAC.Data for main pipe.

  type Smart_error_pipe is access procedure (
    message   : String;
    file_name : String;
    line      : Natural;
    column_a  : Natural;       --  Before first selected character, can be 0.
    column_z  : Natural;
    kind      : Message_kind;  --  Error, or warning, or ? ...
    repair    : Repair_kit     --  Can error be automatically repaired; if so, how ?
  );

  procedure Error (
    code          : Compile_Error;
    hint          : String      := "";
    stop_on_error : Boolean     := False
  );

  procedure EndSkip;

  procedure ErrorMsg;

  type Table_OverFlow_Error is
    (IDENTIFIERS,
     PROCEDURES,
     FLOAT_CONSTANTS,
     ARRAYS,
     LEVELS,
     OBJECTS,
     Case_Labels,
     STRING_CONSTANTS,
     TASKS,
     ENTRIES,
     PATCHING);

  procedure Fatal (N: Table_OverFlow_Error);

  Internal_error: exception;
  Failure_1_0: exception;
  Compilation_abandoned: exception;

  function Error_String (code: Compile_Error; hint: String:= "") return String;

end HAC.UErrors;
