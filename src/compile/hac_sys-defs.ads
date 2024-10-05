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

with HAT;

with Ada.Characters.Handling,
     Ada.Text_IO;

with Interfaces;

package HAC_Sys.Defs is

  HAT_Name : constant String := "HAT";  --  Stands for:  HAC Ada Toolbox (was: Library).

  subtype HAC_Integer is Interfaces.Integer_64;
  HAC_Integer_Name       : constant String := "Integer";
  HAC_Integer_Name_Upper : constant String := Ada.Characters.Handling.To_Upper (HAC_Integer_Name);
  function HAC_Image is new HAT.HAC_Generic_Image (Abstract_Integer => HAC_Integer);

  --  HAC's default floating-point type is double-precision
  --  and is called "Real" in HAC's HAT package.
  --  There is *no* Float in HAC's Standard package.
  --  Float is commonly assumed to be single-precision -> no practical use and would
  --  complicate the parsing in HAC by having multiple possible
  --  floating-point type expressions.
  --  On top of that a universal float would be probably needed.
  --
  subtype HAC_Float is HAT.Real;
  HAC_Float_Name       : constant String := "Real";
  HAC_Float_Name_Upper : constant String := Ada.Characters.Handling.To_Upper (HAC_Float_Name);

  --  Max & Min Exponents. IEEE Double Precision.
  --  TBD: !! find the attribute, applied on HAC_Float, that matches this value.
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

  AMax               : constant := 1_000;    --  Size OF ARRAY-TABLE
  BMax               : constant := 10_000;   --  Size OF Block-TABLE
  float_const_table_max
                     : constant := 200;
  Cases_Max          : constant := 2000;     --  Max number of cases in a CASE statement
  CDMax              : constant := 100_000;  --  Size OF ObjCode
  entry_table_max    : constant := 30;       --  Maximum Number of Entries
  integer_digits_max : constant := 18;       --  Maximum digits for an integer literal, was KMAX:
                                             --  decimal representation of 2**63, minus 1 digit.
  package_table_max  : constant := 10_000;   --  Size of Package table
  loop_nesting_max   : constant := 100;
  nesting_level_max  : constant := 20;       --  Maximum subprogram nesting level, was LMAX.

  nesting_and_descending_max : constant := 40;  --  subprograms, subpackages and child packages

  End_Function_without_Return : constant := -1;

  Normal_Procedure_Call  : constant := 0;
  Normal_Entry_Call      : constant := 1;
  Timed_Entry_Call       : constant := 2;
  Conditional_Entry_Call : constant := 3;

  OrdMinChar : constant := 0;             --  Ord of First Char
  OrdMaxChar : constant := 255;           --  Ord of last Char

  PriMax     : constant := 100;           --  Maximum Task priority
  SMax       : constant := 100_000;       --  Size of String table
  TaskMax    : constant := 12;            --  Max # of concurrent tasks

  string_folding_scan_limit : constant := 1000;

  Wind_Size    : constant := TaskMax + 2;    --  SnapShot window size
  Id_Table_Max : constant := 10_000;         --  Size of identifier table
  XMax         : constant Integer := MaxINT;

  Patch_Max : constant := 100;

  identifier_length_max : constant := 200;

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
    Dummy_Symbol,       --  Symbol that is never scanned.
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
    AND_THEN_Symbol,  --  This symbol is never scanned as such.
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
    OR_ELSE_Symbol,  --  This symbol is never scanned as such.
    OTHERS_Symbol,
    OUT_Symbol,
    OVERRIDING_Symbol,
    PACKAGE_Symbol,
    PARALLEL_Symbol,
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

  subtype Plus_Minus is KeyWSymbol range Plus .. Minus;
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
  empty_symset : constant Symset := (others => False);

  -----------------
  -- Identifiers --
  -----------------

  subtype Alfa is HAT.VString;  --  Originally, Alfa was a space-padded fixed string.
  Empty_Alfa : Alfa renames HAT.Null_VString;
  function A2S (a : Alfa) return String renames HAT.To_String;
  function S2A (s : String) return Alfa renames HAT.To_VString;

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
    -------------------------------------------------------
    --  Special types appearing only during the parsing  --
    --  and unavailable to the programmer.               --
    -------------------------------------------------------
    String_Literals,     --  Strings in the "abcd" form. Takes 2 items on the stack.
    Strings_as_VStrings  --  VString value, but semantically a String. E.g. returned by S'Image
  );
  for Typen'Size use 8;

  type Typ_Set is array (Typen) of Boolean;

  empty_typ_set : constant Typ_Set := (others => False);

  subtype Standard_Typ is Typen range NOTYP .. Text_Files;

  subtype Special_Strings is Typen range String_Literals .. Strings_as_VStrings;

  subtype Composite_Typ is Typen range Arrays .. Records;

  Standard_or_Enum_Typ : constant Typ_Set :=
    (Standard_Typ | Enums => True, others => False);

  Discrete_Typ : constant Typ_Set :=  --  RM 3.2 (12)
    (Ints | Bools | Chars | Enums => True, others  => False);

  subtype Numeric_Typ is Typen range Ints .. Floats;  --  RM 3.2 (1)

  Auto_Init_Typ : constant Typ_Set :=
    (VStrings | Text_Files => True, others => False);

  Typ_with_Variant_Part : constant Typ_Set :=
    (Floats | VStrings | Times | Durations | Text_Files => True, others => False);

  subtype Index is Integer range -XMax .. +XMax;

  type Float_Constants_Table_Type is array (1 .. float_const_table_max) of HAC_Float;

  subtype Nesting_Level is HAC_Integer range 0 .. nesting_level_max;

  type Flow_Context is record
    level                  : Nesting_Level;
    --  This is for rudimentary flow analysis
    --  and the issuance of clever warnings and notes.
    is_within_loop         : Boolean := False;  --  Reversed on leaving top loop
    is_within_condition    : Boolean := False;  --  Reversed on leaving top condition
    is_in_cond_within_loop : Boolean := False;  --  Reversed on leaving this case
  end record;

  ------------------------------
  --  Compilation error type  --
  ------------------------------

  type Compile_Diagnostic is
     --  Errors that may occur during the scanning of symbols (tokens):
    (err_scanner_character_zero_chars,
     err_scanner_control_character,
     err_scanner_digit_expected,
     err_scanner_double_underline_not_permitted,
     err_scanner_identifier_cannot_end_with_underline,
     err_scanner_identifier_too_long,
     err_scanner_illegal_character,
     err_scanner_illegal_character_in_number,
     err_scanner_negative_exponent_for_integer_literal,
     err_scanner_exponent_too_large,
     err_scanner_integer_literal_too_large,
     err_scanner_space_missing_after_number,
     --  Errors that may occur during parsing:
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
     err_missing_type_begin_symbol,
     err_expecting_double_dot,
     err_semicolon_missing,
     err_duplicate_semicolon,
     err_extra_right_parenthesis,                 --  2021-12-29
     err_bad_result_type_for_a_function,
     err_type_of_return_statement_doesnt_match,
     err_illegal_statement_start_symbol,
     err_expecting_a_boolean_expression,
     err_control_variable_of_the_wrong_type,
     err_bounds_type_mismatch,
     err_IS_missing,
     err_incorrect_name_after_END,
     err_bad_type_for_a_case_statement,
     err_illegal_constant_or_constant_identifier,
     err_wrong_type_for_array_index,
     err_too_few_array_indices,
     err_too_many_array_indices,
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
     err_primary_unexpected_symbol,
     err_RETURN_missing,
     err_RECORD_missing,
     err_missing_closing_IF,
     err_WHEN_missing,
     err_FINGER_missing,
     err_missing_closing_CASE,
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
     err_statement_expected,
     err_duplicate_loop_identifier,
     err_unexpected_end_of_text,                  --  2018-04-01
     err_not_yet_implemented,                     --  2019-03-24
     err_type_conversion_not_supported,           --  2020-03-31
     err_numeric_type_coercion,                   --  2020-04-06
     err_numeric_type_coercion_operator,
     err_operator_not_defined_for_types,          --  2020-04-06
     err_no_null_functions,                       --  2020-04-10
     err_cannot_modify_constant_or_in_parameter,
     err_case_others_alone_last,
     err_no_X_for_END_X,
     err_END_LOOP_ident_missing,
     err_END_LOOP_ident_wrong,
     err_general_error,                           --  Default (without hint): the classic "syntax error"
     err_string_to_vstring_assignment,
     err_range_constraint_error,
     err_discrete_type_expected,
     err_membership_test_type_mismatch,
     err_string_not_supported_as_parameter,
     err_string_lengths_do_not_match,
     err_object_used_before_end_own_declaration,  --  2021-12-11
     err_attribute_prefix_invalid,                --  2021-12-26
     err_attribute_prefix_must_be_discrete_type,  --  2021-12-26
     err_invalid_dimension_number,                --  2022-01-09
     err_spec_body_mismatch,                      --  2022-01-22
     err_incomplete_declaration,                  --  2022-01-22
     err_non_public_entity,                       --  2022-04-02
     err_choices_not_covered,
     err_choice_out_of_range,
     err_mixed_logical_operators,
     err_library_error,
     err_wrong_unit_name,
     err_obsolete_hat_name,
     --
     note_redundant_construct,
     note_unused_item,
     note_constant_variable,
     --
     warn_read_but_not_written);

  subtype Compile_Note is Compile_Diagnostic
    range note_redundant_construct .. note_constant_variable;

  subtype Compile_Warning is Compile_Diagnostic
    range warn_read_but_not_written .. warn_read_but_not_written;

  --  A Remark is either a Warning (about something potentially dangerous)
  --  or a Note (about something harmless but typically superfluous).
  subtype Compile_Remark is Compile_Diagnostic
    range Compile_Note'First .. Compile_Warning'Last;

  --  Summary of Compile_Diagnostic:
  --
  --          +------------------------+
  --          |      Diagnostic        |
  --          +---v-----------v--------+
  --          |       |     Remark     |
  --          | Error |--v--------v----+
  --          |       | Note | Warning |
  --          +-------+------+---------+

  type Remark_Set is array (Compile_Remark) of Boolean;

  type Remark_Level is range 0 .. 3;
  default_remark_level : constant Remark_Level := 1;

  --  Level 0 means: no remarks are issued.
  --  Level 1 corresponds roughly the GNAT defaults (when you type "gnatmake"
  --            on a command-line interpreter, it's the warnings marked
  --            with a '*', plus others that are always enabled.
  --  Level 2 corresponds roughly the GNAT's "-gnatwa" option, that is
  --            warnings marked with a '+' in the help.
  --  Level 3 means: all remarkes are enabled. This level corresponds to
  --            the "-gnatw.e" switch.

  preset_remarks : constant array (Remark_Level) of Remark_Set :=
    (0 => (others => False),
     1 => (warn_read_but_not_written => True, others => False),
     2 => (note_redundant_construct |
           note_unused_item |
           note_constant_variable |
           warn_read_but_not_written => True),
     3 => (others => True));

  function Minimum_Level (r : Compile_Remark) return Remark_Level;

  default_remarks : constant Remark_Set :=
    preset_remarks (default_remark_level);

  remark_letter : constant array (Compile_Remark) of Character :=
    (note_redundant_construct => 'r',
     note_unused_item         => 'u',
     note_constant_variable   => 'k',
     --
     warn_read_but_not_written => 'v');

  subtype Compile_Error is Compile_Diagnostic
    range Compile_Diagnostic'First ..
          Compile_Diagnostic'Pred (Compile_Remark'First);

  type Diagnostic_Set is array (Compile_Diagnostic) of Boolean;
  no_diagnostic : constant Diagnostic_Set := (others => False);

  type Repair_Kind_Type is (none, insert, replace_token);

  type Repair_Kit is tagged record
    repair_kind : Repair_Kind_Type := none;
    alternative : HAT.VString      := HAT.Null_VString;
  end record;

  --  The Symbol_Location record unifies the location of symbols
  --  within a source stream, basically for errors & warnings.

  type Symbol_Location is record
    line         : Integer;
    column_start : Integer;
    column_stop  : Integer;
  end record;

  type Diagnostic_Kind_Type is (error, warning, note, style);

  subtype Remark_Type is Diagnostic_Kind_Type range warning .. note;

  type Diagnostic_Kit is new Repair_Kit with record
    diagnostic_kind : Diagnostic_Kind_Type := error;
    message         : HAT.VString          := HAT.Null_VString;
    file_name       : HAT.VString          := HAT.Null_VString;
    location        : Symbol_Location      := (0, 0, 0);
  end record;

  type Smart_Error_Pipe is access procedure (kit : Diagnostic_Kit);

  package IIO is new Ada.Text_IO.Integer_IO (HAC_Integer);
  package RIO is new Ada.Text_IO.Float_IO (HAC_Float);
  package BIO is new Ada.Text_IO.Enumeration_IO (Boolean);

  --  package REF is new Ada.Numerics.Generic_Elementary_Functions (HAC_Float);

  subtype Strings_Constants_Table_Type is String (1 .. SMax);

  ------------------------------------
  --  Standard function operations  --
  ------------------------------------

  type SF_Code is (
    SF_Abs_Int,
    SF_Abs_Float,
    SF_T_Val,                   --  S'Val  : RM 3.5.5 (5)
    SF_T_Pos,                   --  S'Pos  : RM 3.5.5 (2)
    SF_T_Succ,                  --  S'Succ : RM 3.5 (22)
    SF_T_Pred,                  --  S'Pred : RM 3.5 (25)
    SF_in_discrete_Interval,
    SF_not_in_discrete_Interval,
    --  Numerical functions
    SF_Round_Float_to_Int,
    SF_Trunc_Float_to_Int,
    SF_Float_to_Duration,
    SF_Duration_to_Float,
    SF_Int_to_Duration,
    SF_Duration_to_Int,
    SF_Sin,
    SF_Cos,
    SF_Exp,
    SF_Log,
    SF_Sqrt,
    SF_Arctan,
    SF_Sgn_Int,
    SF_Sgn_Float,
    SF_EOF,
    SF_EOLN,
    SF_Is_Open,
    SF_Random_Int,
    SF_Min_Int,
    SF_Max_Int,
    SF_Min_Float,
    SF_Max_Float,
    --
    --  VString functions (Ada.Strings.Unbounded-like)
    --
    SF_String_to_VString,        --  +s        (s is a fixed-size string)
    SF_Literal_to_VString,       --  +"Hello"
    SF_VString_to_String,        --  -v
    SF_Char_to_VString,          --  +'x'
    SF_Two_VStrings_Concat,      --  v1 & v2
    SF_VString_Char_Concat,      --  v & 'x'
    SF_Char_VString_Concat,      --  'x' & v
    SF_LStr_VString_Concat,      --  "Hello " & v
    --
    SF_VString_Int_Concat,       --  v & 123
    SF_Int_VString_Concat,       --  123 & v
    SF_VString_Float_Concat,     --  v & 3.14159
    SF_Float_VString_Concat,     --  3.14159 & v
    SF_VString_Duration_Concat,  --  v & (Time_1 - Time_0)
    SF_Duration_VString_Concat,  --  (Time_1 - Time_0) & v
    SF_VString_Boolean_Concat,   --  v & is_found
    SF_Boolean_VString_Concat,   --  is_found & v
    --
    SF_Element,
    SF_Length,
    SF_Slice,
    --
    SF_To_Lower_Char,
    SF_To_Upper_Char,
    SF_To_Lower_VStr,
    SF_To_Upper_VStr,
    SF_Index,
    SF_Index_Backward,
    SF_Int_Times_Char,
    SF_Int_Times_VStr,
    --
    SF_Trim_Left,
    SF_Trim_Right,
    SF_Trim_Both,
    --
    SF_Head,
    SF_Head_Before_Match,
    SF_Tail,
    SF_Tail_After_Match,
    SF_Starts_With,
    SF_Ends_With,
    --
    --  Ada.Calendar-like functions
    --
    SF_Time_Subtract,    --  T2 - T1 -> Duration
    SF_Duration_Add,
    SF_Duration_Subtract,
    SF_Year,
    SF_Month,
    SF_Day,
    SF_Seconds,
    --
    SF_Image_Ints,              --  HAT.Image without the nasty ' ' before non-negative values
    SF_Image_Floats,            --  HAT.Image with a human-readable formatting whenever possible
    SF_Image_Times,             --  HAT.Image
    SF_Image_Durations,         --  HAT.Image
    --
    SF_Integer_Value,
    SF_Float_Value,
    --  'Image attribute "as is" from Ada:
    SF_Image_Attribute_Ints,
    SF_Image_Attribute_Floats,
    SF_Image_Attribute_Bools,
    SF_Image_Attribute_Chars,
    SF_Image_Attribute_Durs,
    SF_Image_Attribute_Enums,
    --  'Value attribute "as is" from Ada:
    SF_Value_Attribute_Ints,
    SF_Value_Attribute_Floats,
    SF_Value_Attribute_Bools,
    SF_Value_Attribute_Chars,
    SF_Value_Attribute_Durs,
    SF_Value_Attribute_Enums,
    --
    SF_Argument,
    --  Ada.Directories-like
    SF_Directory_Exists,
    SF_Exists,
    SF_File_Exists,
    --  Ada.Environment_Variables-like
    SF_Get_Env,
    SF_Get_VM_Variable,
    --
    --  Niladic functions (they have no arguments).
    --
    SF_Clock,
    SF_Random_Float,
    SF_Null_VString,
    SF_Argument_Count,
    SF_Command_Name,
    SF_Directory_Separator,
    SF_Current_Directory,  --  Ada.Directories-like
    --
    SF_Get_Needs_Skip_Line  --  Informs whether Get from console needs Skip_Line
  );

  subtype SF_Niladic is SF_Code range SF_Clock .. SF_Get_Needs_Skip_Line;

  subtype SF_Min_Max_Int is SF_Code range SF_Min_Int .. SF_Max_Int;

  subtype SF_File_or_Console_Information is SF_Code range SF_EOF .. SF_EOLN;

  subtype SF_Index_Any_Direction is SF_Code range SF_Index .. SF_Index_Backward;

  -------------------------------------
  --  Standard procedure operations  --
  -------------------------------------

  type SP_Code is (
    SP_Create,
    SP_Open,
    SP_Append,
    SP_Close,
    --
    SP_Push_Abstract_Console,
    --
    SP_Get,
    SP_Get_Immediate,
    SP_Get_Line,
    SP_Get_File,
    SP_Get_Line_File,
    SP_Skip_Line,
    --
    SP_Put,
    SP_Put_Line,
    SP_Put_File,
    SP_Put_Line_File,
    SP_New_Line,
    --
    SP_Randomize,
    SP_Random_Seed,
    --
    SP_Wait,
    SP_Signal,
    --
    SP_Quantum,
    SP_Priority,
    SP_InheritP,
    --
    --  Ada.Environment_Variables-like procedures
    --
    SP_Set_Env,
    SP_Set_VM_Variable,
    --
    --  Ada.Directories-like procedures
    --
    SP_Copy_File,
    SP_Create_Directory,
    SP_Create_Path,
    SP_Delete_Directory,
    SP_Delete_File,
    SP_Rename,
    SP_Set_Directory,
    SP_Set_Exit_Status,
    --
    --  VString procedures (Ada.Strings.Unbounded-like)
    --
    SP_Delete,
    --
    --  Other system procedures
    --
    SP_Shell_Execute_without_Result,  --  Result: no,  Output no
    SP_Shell_Execute_with_Result,     --  Result: yes, Output no
    SP_Shell_Execute_Output,          --  Result: no,  Output yes
    SP_Shell_Execute_Result_Output    --  Result: yes, Output yes
  );

  subtype SP_Shell_Execute is SP_Code
    range SP_Shell_Execute_without_Result .. SP_Shell_Execute_Result_Output;

end HAC_Sys.Defs;
