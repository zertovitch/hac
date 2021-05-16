-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------

--  This package contains constants and types for the
--  compiler and the p-code interpreter.

with HAL;

with Ada.Characters.Handling,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

with Interfaces;

package HAC_Sys.Defs is

  HAL_Name : constant String := "HAL";  --  Stands for:  HAC Ada Library.

  subtype HAC_Integer is Interfaces.Integer_64;
  HAC_Integer_Name       : constant String := "Integer";
  HAC_Integer_Name_Upper : constant String := Ada.Characters.Handling.To_Upper (HAC_Integer_Name);
  function HAC_Image is new HAL.HAC_Generic_Image (Abstract_Integer => HAC_Integer);

  --  HAC's default floating-point type is double-precision
  --  and is called "Real" in HAC's HAL package.
  --  There is *no* Float in HAC's Standard package.
  --  Float is commonly assumed to be single-precision -> no practical use and would
  --  complicate the parsing in HAC by having multiple possible
  --  floating-point type expressions.
  --  On top of that a universal float would be probably needed.
  --
  subtype HAC_Float is HAL.Real;
  HAC_Float_Name       : constant String := "Real";
  HAC_Float_Name_Upper : constant String := Ada.Characters.Handling.To_Upper (HAC_Float_Name);

  --  Max & Min Exponents. IEEE Double Precision.
  --  TBD: find the attribute, applied on HAC_Float, that matches this value.
  EMax : constant :=  308;
  EMin : constant := -308;

  ------------------------
  --  Global constants  --
  ------------------------

  StMax   : constant := 4_000_000;    --  Maximum Stack Size
  STKINCR : constant :=     2_000;    --  Stack Increment allocated per Task

  Header : constant String := "HAC - HAC Ada Compiler";
  --  Alternative name: Hackers' Ada Compiler
  --  Was: "Small-Ada  Macintosh Ver 1.1  Nov 1989  George Washington University"

  MaxINT     : constant Integer := Integer'Last - 1;

  Alng                  : constant := 40;      --  NO. OF SIGNIFICANT CHARS IN IDENTIFIERS
  AMax                  : constant := 30;      --  Size OF ARRAY-TABLE
  BMax                  : constant := 25;      --  Size OF Block-TABLE
  Float_Const_Table_Max : constant := 200;
  Cases_Max             : constant := 30;      --  Max number of cases in a CASE statement
  CDMax                 : constant := 100_000;  --  Size OF ObjCode
  EntryMax              : constant := 30;      --  Maximum Number of Entry Statements
  FMax                  : constant := 20;      --  Maximum Number of files for I/O
  KMax                  : constant := HAC_Float'Digits;

  Nesting_Level_Max     : constant := 20;
  type Nesting_level is range 0 .. Nesting_Level_Max;

  End_Function_without_Return : constant := -1;

  Case_when_something : constant := +1;
  Case_when_others    : constant := -1;

  Normal_Procedure_Call  : constant := 0;
  Normal_Entry_Call      : constant := 1;
  Timed_Entry_Call       : constant := 2;
  Conditional_Entry_Call : constant := 3;

  OrdMinChar : constant := 0;             --  Ord of First Char
  OrdMaxChar : constant := 255;           --  Ord of last Char

  PriMax     : constant := 100;           --  Maximum Task priority
  SMax       : constant := 10_000;        --  Size of String table
  TaskMax    : constant := 12;            --  Max # of concurrent tasks

  Wind_Size    : constant := TaskMax + 2;    --  SnapShot window size
  Id_Table_Max : constant := 10_000;         --  Size of identifier table
  XMax         : constant Integer := MaxINT;

  Patch_Max : constant := 100;

  --------------------
  --  Global types  --
  --------------------

  -----------------------------------------------------------------------
  ---------------------------------------------------------KeyWSymbol----
  -----------------------------------------------------------------------
  type KeyWSymbol is (   --  All keywords or symbols used by the compiler
    IntCon,
    FloatCon,
    CharCon,
    StrCon,
    --
    Plus,     --  +
    Minus,    --  -
    Times,    --  *
    Divide,   --  /
    Power,    --  **
    --
    EQL,      --  =
    NEQ,      --  /=
    GTR,      --  >
    GEQ,      --  >=
    LSS,      --  <
    LEQ,      --  <=
    --
    LParent,
    RParent,
    LBrack,
    RBrack,
    Apostrophe,
    Comma,
    Semicolon,
    Period,
    Range_Double_Dot_Symbol,  --  ".." compound delimiter (RM 2.2)
    Colon,
    Alt,
    Finger,
    Becomes,
    IDent,
    USy,                --  (Apparently) unused symbol
    Dummy_Symbol,       --  Symbol that is never parsed.
    Ampersand_Symbol,
    --                  Ada keywords
    ABORT_Symbol,
    ABS_Symbol,
    ABSTRACT_Symbol,
    ACCEPT_Symbol,
    ACCESS_Symbol,
    ALIASED_Symbol,
    ALL_Symbol,
    AND_Symbol,
    ARRAY_Symbol,
    AT_Symbol,
    BEGIN_Symbol,
    BODY_Symbol,
    CASE_Symbol,
    CONSTANT_Symbol,
    DECLARE_Symbol,
    DELAY_Symbol,
    DELTA_Symbol,
    DIGITS_Symbol,
    DO_Symbol,
    ELSE_Symbol,
    ELSIF_Symbol,
    END_Symbol,
    ENTRY_Symbol,
    EXCEPTION_Symbol,
    EXIT_Symbol,
    FOR_Symbol,
    FUNCTION_Symbol,
    GENERIC_Symbol,
    GOTO_Symbol,
    IF_Symbol,
    IN_Symbol,
    INTERFACE_Symbol,
    IS_Symbol,
    LIMITED_Symbol,
    LOOP_Symbol,
    MOD_Symbol,
    NEW_Symbol,
    NOT_Symbol,
    NULL_Symbol,
    OF_Symbol,
    OR_Symbol,
    OTHERS_Symbol,
    OUT_Symbol,
    OVERRIDING_Symbol,
    PACKAGE_Symbol,
    PRAGMA_Symbol,
    PRIVATE_Symbol,
    PROCEDURE_Symbol,
    PROTECTED_Symbol,
    RAISE_Symbol,
    RANGE_Keyword_Symbol,  --  "range" reserved word (RM 2.9)
    RECORD_Symbol,
    REM_Symbol,
    RENAMES_Symbol,
    REQUEUE_Symbol,
    RETURN_Symbol,
    REVERSE_Symbol,
    SELECT_Symbol,
    SEPARATE_Symbol,
    SOME_Symbol,
    SUBTYPE_Symbol,
    SYNCHRONIZED_Symbol,
    TAGGED_Symbol,
    TASK_Symbol,
    TERMINATE_Symbol,
    THEN_Symbol,
    TYPE_Symbol,
    UNTIL_Symbol,
    USE_Symbol,
    WHEN_Symbol,
    WHILE_Symbol,
    WITH_Symbol,
    XOR_Symbol
  );

  subtype Comparison_Operator is KeyWSymbol range EQL .. LEQ;
  subtype Arithmetic_Binary_Operator is KeyWSymbol range Plus .. Power;

  ---------------------
  -- Sets of symbols --
  ---------------------

  type Symset is array (KeyWSymbol) of Boolean;
  --  The "+" and "-" reproduce the Pascal set operators.
  function "+" (a, b : Symset) return Symset;
  function "+" (a : Symset; b : KeyWSymbol) return Symset;
  function "-" (a, b : Symset) return Symset;
  function "-" (a : Symset; b : KeyWSymbol) return Symset;
  Empty_Symset : constant Symset := (others => False);

  -----------------
  -- Identifiers --
  -----------------

  --  Alfa is a space-padded string
     --  !! Consider replacing by VString or a bounded-length string.
     --     First step for a smooth transition:
     --       type Alfa is array (1 .. Alng) of Character;
     --     detects all type incompatibilities except: slice = literal expressions.
  subtype Alfa is String (1 .. Alng);
  Empty_Alfa : constant Alfa := (others => ' ');
  function Equal (a : Alfa; s : String) return Boolean;
  function Initial (a : Alfa) return Character;
  function To_String (a : Alfa) return String;
  function To_Alfa (s : String) return Alfa;

  --  Data types in HAC. We call them "Typ" (with an Akzent ;-) ) to avoid
  --  confusion with the types of the HAC code itself.
  --
  --  The order of these is significant.
  --
  type Typen is (
    ----------------------
    --  Built-in types  --
    ----------------------
    NOTYP,  --  Appears when the parsing of an expression fails at some point.
    Ints,
    Floats,
    Bools,
    Chars,
    VStrings,
    Times,
    Durations,
    --
    Text_Files,  --  This one is limited (like Ada's File_Type).
    ------------------------------------
    --  Types defined by programmers  --
    ------------------------------------
    Arrays,
    Records,
    Enums,
    --------------------------------------------------
    --  Special types appearing during the parsing  --
    --------------------------------------------------
    String_Literals
  );
  for Typen'Size use 8;

  type Typ_Set is array (Typen) of Boolean;

  subtype Standard_Typ is Typen range NOTYP .. Text_Files;

  subtype Composite_Typ is Typen range Arrays ..  Records;

  Standard_or_Enum_Typ : constant Typ_Set :=
    (Standard_Typ | Enums => True, others => False);

  Discrete_Typ : constant Typ_Set :=  --  RM 3.2 (12)
    (Ints | Bools | Chars | Enums => True, others  => False);

  subtype Numeric_Typ is Typen range Ints .. Floats;  --  RM 3.2 (1)

  Auto_Init_Typ : constant Typ_Set :=
    (VStrings | Text_Files => True, others => False);

  subtype Index is Integer range -XMax .. +XMax;

  type Float_Constants_Table_Type is array (1 .. Float_Const_Table_Max) of HAC_Float;

  ------------------------------
  --  Compilation error type  --
  ------------------------------

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
    err_duplicate_semicolon,
    err_bad_result_type_for_a_function,
    err_type_of_return_statement_doesnt_match,
    err_illegal_statement_start_symbol,
    err_expecting_a_boolean_expression,
    err_control_variable_of_the_wrong_type,
    err_bounds_type_mismatch,
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
    err_Ada_reserved_word,
    err_functions_must_return_a_value,
    err_procedures_cannot_return_a_value,
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
    --  These errors messages are new in HAC and weren't in SmallAda
    err_OF_instead_of_IS,
    err_THEN_instead_of_Arrow,
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
    err_numeric_type_coercion,                   --  2020-04-06
    err_numeric_type_coercion_operator,
    err_operator_not_defined_for_types,          --  2020-04-06
    err_no_null_functions,                       --  2020-04-10
    err_digit_expected,
    err_expected_char_or_string,
    err_cannot_modify_constant_or_in_parameter,
    err_case_others_alone_last,
    err_END_LOOP_ident_missing,
    err_END_LOOP_ident_wrong,
    err_syntax_error,                            --  A classic one. "Too long to explain more..."
    err_string_to_vstring_assignment,
    err_range_constraint_error,
    err_discrete_type_expected,
    err_membership_test_type_mismatch,
    err_string_not_supported_as_parameter,
    err_string_lengths_do_not_match,
    err_library_error
  );

  type Error_set is array (Compile_Error) of Boolean;
  error_free : constant Error_set := (others => False);

  type Repair_kind is (none, insert, insert_line, replace_token);

  use Ada.Strings.Unbounded;

  type Repair_kit is tagged record
    kind : Repair_kind      := none;
    text : Unbounded_String := Null_Unbounded_String;
  end record;

  type Message_kind is (error, warning, note, style);

  type Smart_error_pipe is access procedure (
    message   : String;
    file_name : String;
    line      : Natural;
    column_a  : Natural;       --  Before first selected character, can be 0.
    column_z  : Natural;
    kind      : Message_kind;  --  Error, or warning, or ? ...
    repair    : Repair_kit     --  Can error be automatically repaired; if so, how ?
  );

  package IIO is new Ada.Text_IO.Integer_IO (HAC_Integer);
  package RIO is new Ada.Text_IO.Float_IO (HAC_Float);
  package BIO is new Ada.Text_IO.Enumeration_IO (Boolean);

  --  package REF is new Ada.Numerics.Generic_Elementary_Functions (HAC_Float);

end HAC_Sys.Defs;
