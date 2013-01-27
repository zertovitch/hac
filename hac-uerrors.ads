-------------------------------------------------------------------------------------
--
-- HAC - HAC Ada Compiler
--
-- A compiler in Ada for an Ada subset

-- Legal licensing note:

--  Copyright (c) 2013 Gautier de Montmollin
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

  procedure Error(N: Integer);
  --
  undefined_identifier                       : constant:=  0;
  duplicate_identifier                       : constant:=  1;
  err_identifier_missing                     : constant:=  2; -- also 7
  err_closing_parenthesis_missing            : constant:=  4;
  err_colon_missing                          : constant:=  5;
  err_incorrectly_used_symbol                : constant:=  6;
  err_missing_OF                             : constant:=  8;
  err_missing_an_opening_parenthesis         : constant:=  9;
  err_expecting_dot_dot                      : constant:= 13;
  err_semicolon_missing                      : constant:= 14;
  err_expecting_a_boolean_expression         : constant:= 17;
  IS_missing                                 : constant:= 20;
  incorrect_block_name                       : constant:= 22;
  illegal_constant_or_constant_identifier    : constant:= 25;
  err_illegal_array_subscript                : constant:= 26;
  illegal_array_bounds                       : constant:= 27;
  err_indexed_variable_must_be_an_array      : constant:= 28;
  err_missing_a_type_identifier              : constant:= 29;
  err_undefined_type                         : constant:= 30;
  err_var_with_field_selector_must_be_record : constant:= 31;
  err_resulting_type_should_be_Boolean       : constant:= 32;
  err_illegal_type_for_arithmetic_expression : constant:= 33;
  err_mod_requires_integer_arguments         : constant:= 34;
  err_incompatible_types_for_comparison      : constant:= 35;
  err_parameter_types_do_not_match           : constant:= 36;
  variable_missing                           : constant:= 37;
  err_number_of_parameters_do_not_match      : constant:= 39;
  parameter_must_be_integer                  : constant:= 43;
  err_expected_variable_function_or_constant : constant:= 44;
  types_of_assignment_must_match             : constant:= 46;
  err_case_label_not_same_type_as_case_clause: constant:= 47;
  err_argument_to_std_function_of_wrong_type : constant:= 48;
  err_BECOMES_missing                        : constant:= 51;
  err_THEN_missing                           : constant:= 52;
  err_closing_LOOP_missing                   : constant:= 54;
  err_END_missing                            : constant:= 57;
  err_missing_RECORD                         : constant:= 61;
  err_missing_closing_IF                     : constant:= 62;
  err_functions_must_return_a_value          : constant:= 68;
  err_use_Small_Sp                           : constant:= 70;
  -- These errors messages are new in HAC and weren't in SmallAda
  OF_instead_of_IS                           : constant:= 82;
  err_EQUALS_instead_of_BECOMES              : constant:= 83;
  err_numeric_constant_expected              : constant:= 84;

  procedure EndSkip;

  procedure ErrorMsg;

  procedure Fatal(N: Integer); -- internal table overflow
  --
  IDENTIFIERS_table_overflow    : constant:=  1;
  PROCEDURES_table_overflow     : constant:=  2;
  FLOAT_constants_table_overflow: constant:=  3;
  LEVEL_overflow                : constant:=  5;
  OBJECT_overflow               : constant:=  6;
  PATCHING_overflow             : constant:= 10;

  Failure_1_0: exception;

  function ErrorString(Id: Integer) return String; -- for debug

end HAC.UErrors;
