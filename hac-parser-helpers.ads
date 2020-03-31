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

with HAC.Data;               use HAC.Data;
with HAC.UErrors;            use HAC.UErrors;

package HAC.Parser.Helpers is

  --  If needed symbol S is correct, consume it;
  --  otherwise output error code E.
  --
  procedure Need (
    S       : KeyWSymbol;
    E       : Error_code;
    Forgive : KeyWSymbol := Dummy_Symbol
  );

  --  Issue error N, then skip all subsequent symbols
  --  that are not in the FSys set.
  --
  procedure Skip (FSys : Symset; N : Error_code);

  --  Issue error N, then skip all subsequent symbols
  --  that are not equal to S.
  --
  procedure Skip (S : KeyWSymbol; N : Error_code);

  --  Test if current symbol is in the S1 set, otherwise
  --  issue error N. If stop_on_error = False, we skip
  --  subsequent symbols that are not in the union S1 + S2.
  --
    procedure Test (
      S1, S2        : Symset;
      N             : Error_code;
      stop_on_error : Boolean:= False);

    procedure Test_Semicolon (FSys : Symset);

    procedure Test_END_Symbol;

  ------------------------------------
  --  Symbol sets used for parsing  --
  ------------------------------------

  IDent_set     : constant Symset := (IDent => True, others => False);
  Semicolon_set : constant Symset := (Semicolon => True, others => False);
  END_set       : constant Symset := (END_Symbol => True, others => False);

  Semicolon_Comma_IDent : constant Symset :=
    (Semicolon | Comma | IDent => True, others => False);

  Comma_Colon_RParent : constant Symset :=
    (Comma | Colon | RParent => True, others => False);

  Comma_RParent : constant Symset :=
    (Comma | RParent => True, others => False);

  OF_RANGE_RParent : constant Symset :=
    (OF_Symbol | RANGE_Symbol | RParent => True, others => False);

  Comma_OF_RParent : constant Symset :=
    (Comma | RParent | OF_Symbol => True, others => False);

  After_Subprogram_Parameters : constant Symset :=
    (IS_Symbol | RETURN_Symbol | Semicolon => True, others => False);

  Becomes_EQL_Semicolon : constant Symset :=
    (Becomes | EQL | Semicolon => True, others => False);

  Becomes_Comma_IDent_Semicolon : constant Symset :=
    (Semicolon | Comma | IDent | Becomes => True, others => False);

  --  Constant definition begin symbol(S)

  ConstBegSys : constant Symset :=
   (Plus      |
    MinUS     |
    IntCon    |
    FloatCon  |
    CharCon   |
    IDent     => True,
    others => False);

  Type_Begin_Symbol : constant Symset :=
   (IDent          |
    ARRAY_Symbol   |
    RECORD_Symbol       |
    RANGE_Symbol        |
    LParent        => True,
    others         => False);

  Block_Begin_Symbol : constant Symset :=
   (PROCEDURE_Symbol |
    FUNCTION_Symbol  |
    TASK_Symbol           |
    ENTRY_Symbol          |
    BEGIN_Symbol     |
    DECLARE_Symbol   => True,
    others => False);

  Factor_Begin_Symbol : constant Symset :=
   (IntCon     |
    FloatCon   |
    CharCon    |
    IDent      |
    LParent    |
    NOT_Symbol => True,
    others => False);

  Statement_Begin_Symbol : constant Symset :=
   (IDent         |
    BEGIN_Symbol  |
    DECLARE_Symbol|
    IF_Symbol     |
    WHILE_Symbol  |
    LOOP_Symbol   |
    FOR_Symbol    |
    CASE_Symbol   |
    EXIT_Symbol   |
    NULL_Symbol        |
    RETURN_Symbol |
    SELECT_Symbol      |
    ACCEPT_Symbol      |
    DELAY_Symbol      => True,

    others => False);

  Declaration_Symbol : constant Symset :=
    (IDent | TYPE_Symbol | TASK_Symbol |
     PROCEDURE_Symbol | FUNCTION_Symbol |
     BEGIN_Symbol => True,
     others       => False);

  Symbols_after_Subprogram_Identifier : constant Symset :=
    (LParent | RETURN_Symbol | IS_Symbol | Semicolon => True,
     others => False);

  OR_ELSE_END : constant Symset :=
    (OR_Symbol | ELSE_Symbol | END_Symbol => True, others => False);

  ELSE_ELSIF_END_Symbol : constant Symset :=
    (ELSE_Symbol | ELSIF_Symbol | END_Symbol => True, others => False);

  DO_THEN_Symbol : constant Symset :=
    (DO_Symbol | THEN_Symbol => True, others => False);

  Fail_after_FOR : constant Symset :=
    (IN_Symbol    |
     RANGE_Symbol |
     LOOP_Symbol  |
     END_Symbol   => True,
     others => False);

end HAC.Parser.Helpers;
