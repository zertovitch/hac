-------------------------------------------------------------------------------------
--
-- HAC - HAC Ada Compiler
--
-- A compiler in Ada for an Ada subset

-- Legal licensing note:

--  Copyright (c) 2013..2014 Gautier de Montmollin
--
--  History and authors list of works HAC was originally derived from
--  can be found in hac.txt.

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

-- NB: this is the MIT License, as found 12-Sep-2013 on the site
-- http://www.opensource.org/licenses/mit-license.php

-------------------------------------------------------------------------------------
--

package HAC.UErrors is

  type Error_code is (
    err_undefined_identifier,
    err_duplicate_identifier,
    err_identifier_missing,
    err_missing_a_procedure_declaration,
    err_closing_parenthesis_missing,
    err_colon_missing,
    err_incorrectly_used_symbol,
    err_missing_OF,
    err_missing_an_opening_parenthesis,
    err_missing_ARRAY_RECORD_or_ident,
    err_expecting_dot_dot,
    err_SEMICOLON_missing,
    err_bad_result_type_for_a_function,
    err_illegal_statement_start_symbol,
    err_expecting_a_boolean_expression,
    err_control_variable_of_the_wrong_type,
    err_first_and_last_must_have_matching_types,
    err_IS_missing,
    err_number_too_large,
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
    err_expected_variable_function_or_constant,
    err_illegal_return_statement_from_main,
    err_types_of_assignment_must_match,
    err_case_label_not_same_type_as_case_clause,
    err_duplicate_case_choice_value,
    err_argument_to_std_function_of_wrong_type,
    err_stack_size,
    err_illegal_symbol_for_a_constant,
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
    err_WITH_Small_Sp,
    err_use_Small_Sp,
    err_missing_an_entry,
    err_missing_expression_for_delay,
    err_wrong_type_in_DELAY,
    err_COMMA_missing,
    err_parameter_must_be_of_type_Boolean,
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
    err_statement_expected,
    err_duplicate_label
  );

  procedure Error(code: Error_code; hint: String:= "");

  procedure EndSkip;

  procedure ErrorMsg;

  procedure Fatal(N: Integer); -- internal table overflow
  --
  IDENTIFIERS_table_overflow    : constant:=  1;
  PROCEDURES_table_overflow     : constant:=  2;
  FLOAT_constants_table_overflow: constant:=  3;
  ARRAYS_table_overflow         : constant:=  4;
  LEVEL_overflow                : constant:=  5;
  OBJECT_overflow               : constant:=  6;
  STRING_table_overflow         : constant:=  7;
  TASKS_table_overflow          : constant:=  8;
  ENTRIES_table_overflow        : constant:=  9;
  PATCHING_overflow             : constant:= 10;

  Internal_error: exception;
  Failure_1_0: exception;

  function ErrorString(code: Error_code; hint: String:= "") return String;

end HAC.UErrors;
