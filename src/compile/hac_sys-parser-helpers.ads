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

package HAC_Sys.Parser.Helpers is

  use Co_Defs, Defs;

  ----------------------
  --  Symbol testing  --
  ----------------------

  --  If needed symbol S is correct, consume it;
  --  otherwise output error code E.
  --
  --  Optionally, we consume a symbol Forgive that
  --  the programmer may have written instead of S.
  --  For instance '[' instead of '('.
  --
  procedure Need (
    CD      : in out Compiler_Data;
    S       : KeyWSymbol;
    E       : Compile_Error;
    Forgive : KeyWSymbol := Dummy_Symbol
  );

  --  Issue error N, then skip all subsequent symbols
  --  that are not in the FSys set.
  --
  procedure Skip (
    CD   : in out Compiler_Data;
    FSys : Symset;
    N    : Compile_Error;
    hint : String := ""
  );

  --  Issue error N, then skip all subsequent symbols
  --  that are not equal to S.
  --
  procedure Skip (
    CD   : in out Compiler_Data;
    S    : KeyWSymbol;
    N    : Compile_Error;
    hint : String := ""
  );

  --  Test if current symbol is in the S1 set, otherwise
  --  issue error N. If stop_on_error = False, we skip
  --  subsequent symbols that are not in the union (S1 + S2).
  --
  procedure Test (
    CD            : in out Compiler_Data;
    S1, S2        : Symset;
    N             : Compile_Error;
    stop_on_error : Boolean := False);

  procedure Need_Semicolon_after_Declaration (CD : in out Compiler_Data; FSys : Symset);

  procedure Need_END_Symbol (CD : in out Compiler_Data);

  procedure Ignore_Extra_Semicolons (CD : in out Compiler_Data);

  procedure Need_Semicolon (CD : in out Compiler_Data);

  -----------------------------------
  --  Data type checks and errors  --
  -----------------------------------

  procedure Check_Boolean (CD : in out Compiler_Data; T : Typen);

  procedure Argument_Type_Not_Supported (CD : in out Compiler_Data);

  function Nice_Exact_Image (CD : Compiler_Data; xT : Exact_Typ'Class) return String;

  procedure Type_Mismatch (
    CD               : in out Compiler_Data;
    Err              :        Compile_Error;
    Found, Expected  :        Exact_Typ'Class
  );

  procedure Type_Mismatch (
    CD       : in out Compiler_Data;
    Err      :        Compile_Error;
    Found    :        Exact_Subtyp;
    Expected :        Typ_Set
  );

  procedure Operator_Undefined (
    CD          : in out Compiler_Data;
    Operator    :        KeyWSymbol;
    Left, Right :        Exact_Subtyp
  );

  --  https://en.wikipedia.org/wiki/Type_conversion#Implicit_type_conversion
  --  One of the most useful feature of Ada is the absence of type coercion.
  --  Note from the Python 2.5 doc:
  --  "In Python 3.0, coercion will not be supported."
  --
  procedure Forbid_Type_Coercion (
    CD          : in out Compiler_Data;
    Operator    :        KeyWSymbol;
    Left, Right :        Exact_Subtyp
  );

  procedure Forbid_Type_Coercion (
    CD              : in out Compiler_Data;
    Found, Expected :        Exact_Subtyp
  );

  ------------------------------------
  --  Symbol sets used for parsing  --
  ------------------------------------

  --  Singletons:

  function Singleton (s : KeyWSymbol) return Symset;
  pragma Inline (Singleton);

  --  Specific singletons:

  Becomes_Set   : constant Symset := (Becomes    => True, others => False);
  Colon_Set     : constant Symset := (Colon      => True, others => False);
  END_Set       : constant Symset := (END_Symbol => True, others => False);
  IDent_Set     : constant Symset := (IDent      => True, others => False);
  RParent_Set   : constant Symset := (RParent    => True, others => False);
  Semicolon_Set : constant Symset := (Semicolon  => True, others => False);

  --  Specific sets:

  Alt_Finger_THEN : constant Symset :=
    --  For "WHEN" in CASE statements ("THEN" is wrong,
    --  but that error is processed specifically).
    (Alt | Finger | THEN_Symbol => True, others => False);

  Becomes_Comma_IDent_Semicolon : constant Symset :=
    (Semicolon | Comma | IDent | Becomes => True, others => False);

  Becomes_EQL_Semicolon : constant Symset :=
    (Becomes | EQL | Semicolon => True, others => False);

  Becomes_EQL : constant Symset :=
    (Becomes | EQL => True, others => False);

  Colon_Comma_LParent_RParent_Semicolon : constant Symset :=
    (Colon | Comma | LParent | RParent | Semicolon => True, others => False);

  Colon_Comma_RParent : constant Symset :=
    (Colon | Comma | RParent => True, others => False);

  Colon_Comma_IS_OF : constant Symset :=
    (Colon | Comma | IS_Symbol | OF_Symbol => True, others => False);

  Comma_END_IDent_Semicolon : constant Symset :=
    (Comma | END_Symbol | IDent | Semicolon => True, others => False);

  Comma_IDent_Semicolon : constant Symset :=
    (Comma | IDent | Semicolon => True, others => False);

  Comma_IDent_RParent_Semicolon : constant Symset :=
    (Comma | IDent | RParent | Semicolon => True, others => False);

  Comma_OF_RParent : constant Symset :=
    (Comma | RParent | OF_Symbol => True, others => False);

  Comma_RParent : constant Symset :=
    (Comma | RParent => True, others => False);

  DO_LOOP : constant Symset :=
    (DO_Symbol | LOOP_Symbol => True, others => False);

  DO_THEN : constant Symset :=
    (DO_Symbol | THEN_Symbol => True, others => False);

  ELSE_ELSIF_END : constant Symset :=
    (ELSE_Symbol | ELSIF_Symbol | END_Symbol => True, others => False);

  ELSE_END_OR : constant Symset :=
    (ELSE_Symbol | END_Symbol | OR_Symbol => True, others => False);

  ELSE_OR : constant Symset :=
    (ELSE_Symbol | OR_Symbol => True, others => False);

  END_IDent_Semicolon : constant Symset :=
    (END_Symbol | IDent | Semicolon => True, others => False);

  END_LOOP_RANGE_Double_Dot : constant Symset :=
    (END_Symbol | LOOP_Symbol | Range_Double_Dot_Symbol => True, others => False);

  END_LOOP_Semicolon : constant Symset :=
    (END_Symbol | LOOP_Symbol | Semicolon => True, others => False);

  END_WHEN : constant Symset :=
    (END_Symbol | WHEN_Symbol => True, others => False);

  OF_RANGE_Double_Dot_RParent : constant Symset :=
    (OF_Symbol | Range_Double_Dot_Symbol | RParent => True, others => False);

  --  Other sets, named by their context:

  After_Subprogram_Parameters : constant Symset :=
    (IS_Symbol | RETURN_Symbol | Semicolon | WITH_Symbol => True, others => False);

  Block_Begin_Symbol : constant Symset :=
   (PROCEDURE_Symbol |
    FUNCTION_Symbol  |
    TASK_Symbol      |
    ENTRY_Symbol     |
    BEGIN_Symbol     |
    DECLARE_Symbol   => True,
    others => False);

  Constant_Definition_Begin_Symbol : constant Symset :=
   (Plus      |
    Minus     |
    IntCon    |
    FloatCon  |
    CharCon   |
    IDent     => True,
    others => False);

  Declaration_X_Subprogram_Symbol : constant Symset :=
    (IDent            |
     PACKAGE_Symbol   |
     SUBTYPE_Symbol   |
     TYPE_Symbol      |
     TASK_Symbol      |
     USE_Symbol       => True,
     others => False);

  Declaration_Symbol : constant Symset :=
    Declaration_X_Subprogram_Symbol +
    (PROCEDURE_Symbol |
     FUNCTION_Symbol  => True,
     others => False);

  Primary_Begin_Symbol : constant Symset :=
   (IntCon     |
    FloatCon   |
    CharCon    |
    IDent      |
    LParent    => True,
    others => False);

  Fail_after_FOR : constant Symset :=
    (IN_Symbol               |
     Range_Double_Dot_Symbol |
     LOOP_Symbol             |
     END_Symbol              => True,
     others => False);

  Plus_Minus : constant Symset :=
    (Plus | Minus => True, others => False);

  Selector_Symbol : constant Symset :=
    (LParent | Period => True, others => False);

  Selector_Symbol_Loose : constant Symset :=
    (LBrack | LParent | Period => True, others => False);

  Statement_Begin_Symbol : constant Symset :=
   (IDent          |
    BEGIN_Symbol   |
    DECLARE_Symbol |
    IF_Symbol      |
    WHILE_Symbol   |
    LOOP_Symbol    |
    FOR_Symbol     |
    CASE_Symbol    |
    EXIT_Symbol    |
    NULL_Symbol    |
    RETURN_Symbol  |
    SELECT_Symbol  |
    ACCEPT_Symbol  |
    DELAY_Symbol   => True,
    others => False);

  Symbols_after_Subprogram_Identifier : constant Symset :=
    (LParent | RETURN_Symbol | IS_Symbol | Semicolon |
     WITH_Symbol  --  Aspect
     => True,
     others => False);

  Type_Begin_Symbol : constant Symset :=
   (IDent                |
    ARRAY_Symbol         |
    RECORD_Symbol        |
    RANGE_Keyword_Symbol |
    LParent              => True,
    others         => False);

  ------------------
  --  Types sets  --
  ------------------

  Numeric_Typ_Set  : constant Typ_Set := (Numeric_Typ         => True, others => False);
  Bools_Set        : constant Typ_Set := (Bools               => True, others => False);
  Chars_Set        : constant Typ_Set := (Chars               => True, others => False);
  Ints_Set         : constant Typ_Set := (Ints                => True, others => False);
  Floats_Set       : constant Typ_Set := (Floats              => True, others => False);
  Arrays_Set       : constant Typ_Set := (Arrays              => True, others => False);
  VStrings_Set     : constant Typ_Set := (VStrings            => True, others => False);
  Times_Set        : constant Typ_Set := (Times               => True, others => False);
  Durations_Set    : constant Typ_Set := (Durations           => True, others => False);
  Txt_Fil_Set      : constant Typ_Set := (Text_Files          => True, others => False);
  Str_Lit_Set      : constant Typ_Set := (String_Literals     => True, others => False);
  Str_as_VStr_Set  : constant Typ_Set := (Strings_as_VStrings => True, others => False);

  PCode_Atomic_Typ         : constant Typ_Set := Discrete_Typ or Numeric_Typ_Set or VStrings_Set;
  Standard_Set             : constant Typ_Set := (Standard_Typ => True, others => False);
  Text_IO_Get_Item_Set     : constant Typ_Set := (Standard_Set and not Bools_Set) or Arrays_Set;

  --  RM 3.2 (3):
  Scalar_Set : constant Typ_Set :=
    (Ints | Floats | Durations | Chars | Bools | Enums => True, others => False);

  ------------------
  --  Base types  --
  ------------------

  Standard_Integer : constant Exact_Subtyp := (Ints, 0, False, HAC_Integer'First, HAC_Integer'Last);

  -------------
  --  Misc.  --
  -------------

  --  Check if we have an "array of Character", for instance a String.
  --
  function Is_Char_Array (CD : Compiler_Data; T : Exact_Subtyp) return Boolean;

  --  Convert a string of any kind sitting on stack top to VString.
  procedure Check_any_String_and_promote_to_VString
    (CD : in out Compiler_Data; X : in out Exact_Subtyp; include_characters : Boolean);

  ------------------------------------------------------------------
  ------------------------------------------------Locate_Identifier-
  --
  --   - Aliases are resolved (True -> Standard.True).
  --   - Parsing continues on each "package_name.": e.g. on "Pkg.",
  --       parser will parse "Pkg.Child_Pkg.Sub_pkg.Var.X" until the
  --       non-package entity: "Var".
  --
  function Locate_Identifier (
    CD               : in out Compiler_Data;
    Id               : in     Alfa;
    Level            : in     Defs.Nesting_level;
    Fail_when_No_Id  : in     Boolean := True;
    Alias_Resolution : in     Boolean := True;
    Level_0_Filter   : in     Boolean := True;
    Public_Filter    : in     Index   := Index'Last
  )
  return Natural;

  procedure Check_Duplicate_Specification
    (CD         : in out Compiler_Data;
     old_id_idx :        Natural;
     id_current :        Alfa);

  procedure Check_Subprogram_Spec_Body_Consistency
    (CD         : in out Compiler_Data;
     old_id_idx :        Natural;
     new_id_idx :        Natural;
     id_current :        Alfa);

  procedure Link_Forward_Declaration
    (CD         : in out Compiler_Data;
     old_id_idx :        Natural;
     new_id_idx :        Natural);

  procedure Check_Incomplete_Definitions
    (CD    : in out Co_Defs.Compiler_Data;
     Level :        Defs.Nesting_level);

end HAC_Sys.Parser.Helpers;
