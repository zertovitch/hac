with HAC.Data; use HAC.Data;

with Ada.Text_IO;

package body HAC.UErrors is

  ----------------------------------------------------------------------------

  function ErrorString (Id : Integer) return String is
  begin
    case Id is
    when undefined_identifier =>
      return "UNDEFINED IDENTIFIER";
    when duplicate_identifier =>
      return "MULTIPLE DEFINITION OF AN IDENTIFIER";
    when identifier_missing =>
      return "MISSING AN IDENTIFIER";
    when 3 =>
      return "MISSING A PROCEDURE DECLARATION";
    when closing_parenthesis_missing =>
      return "MISSING CLOSING PARENTHESIS ``)''";
    when colon_missing =>
      return "MISSING A COLON ``:''";
    when 6 =>
      return "INCORRECTLY USED SYMBOL";
    when 7 =>
      return "MISSING IDENTIFIER";
    when 8 =>
      return "MISSING ``OF''";
    when 9 =>
      return "MISSING AN OPENING PARENTHESIS ``)''";
    when 10 =>
      return "MISSING IDENTIFER; ``ARRAY'' OR ``RECORD''";
    when 11 =>
      return "-- OPENING BRACKET ``[''";
    when 12 =>
      return "-- CLOSING BRACKET ``]''";
    when 13 =>
      return "EXPECTING ``..''";
    when semicolon_missing =>
      return "MISSING A SEMICOLON ``;''";
    when 15 =>
      return "BAD RESULT TYPE FOR A FUNCTION";
    when 16 =>
      return "ILLEGAL STATEMENT START SYMBOL";
    when 17 =>
      return "EXPECTING A BOOLEAN EXPRESSION ";
    when 18 =>
      return "CONTROL VARIABLE OF THE WRONG TYPE";
    when 19 =>
      return "FIRST/LAST MUST BE MATCHING TYPES";
    when IS_missing =>
      return "missing ""is""";
    when 21 =>
      return "THE NUMBER IS TOO LARGE";
    when incorrect_block_name =>
      return "incorrect block name after ""end""";
    when 23 =>
      return "BAD TYPE FOR A CASE STATEMENT";
    when 24 =>
      return "ILLEGAL CHARACTER";
    when 25 =>
      return "ILLEGAL CONSTANT OR CONSTAT IDENTIFIER";
    when 26 =>
      return "ILLEGAL ARRAY SUBSCRIPT (CHECK TYPE)";
    when illegal_array_bounds =>
      return "ILLEGAL BOUNDS FOR AN ARRAY INDEX";
    when 28 =>
      return "INDEXED VARIABLE MUST BE AN ARRAY";
    when 29 =>
      return "MISSING A TYPE IDENFIFIER";
    when 30 =>
      return "UNDEFINED TYPE";
    when 31 =>
      return "VAR WITH FIELD SELECTOR MUST BE RECORD";
    when 32 =>
      return "RESULTING TYPE IS NOT ``BOOLEAN''";
    when 33 =>
      return "ILLEGAL TYPE FOR ARITHMETIC EXPRESSION";
    when 34 =>
      return "``MOD'' REQUIRES INTEGER ARGUMENTS";
    when 35 =>
      return "INCOMPATIBLE TYPES FOR COMPARISON";
    when 36 =>
      return "PARAMETER TYPES DO NOT MATCH";
    when variable_missing =>
      return "MISSING A VARIABLE";
    when 38 =>
      return "A STRING MUST HAVE ONE OR MORE CHAR";
    when 39 =>
      return "NUMBER OF PARAMETERS DO NOT MATCH";
    when 40 =>
      return "ILLEGAL PARAMETERS TO ``GET''";
    when 41 =>
      return "ILLEGAL PARAMETERS TO ``PUT''";
    when 42 =>
      return "PARAMETER MUST BE OF TYPE ``FLOAT''";
    when parameter_must_be_integer =>
      return "PARAMETER MUST BE OF TYPE ``INTEGER''";
    when 44 =>
      return "EXPECTED A VARIABLE; FUNCTION OR CONST";
    when 45 =>
      return "ILLEGAL RETURN STATEMENT FROM MAIN";
    when types_of_assignment_must_match =>
      return "TYPES MUST MATCH IN AN ASSIGNMENT";
    when 47 =>
      return "CASE LABEL NOT SAME TYPE AS CASE CLAUSE";
    when 48 =>
      return "ARGUMENT TO STD. FUNCTION OF WRONG TYPE";
    when 49 =>
      return "THE PROGRAM REQUIRES TOO MUCH STORAGE";
    when 50 =>
      return "ILLEGAL SYMBOL FOR A CONSTANT";
    when BECOMES_missing =>
      return "missing "":=""";
    when 52 =>
      return "MISSING ``THEN''";
    when 53 =>
      return "MISSING ``IN''";
    when 54 =>
      return "MISSING ``LOOP''";
    when 55 =>
      return "MISSING RANGE ``..''";
    when 56 =>
      return "MISSING ``BEGIN''";
    when END_missing =>
      return "MISSING ``END''";
    when 58 =>
      return "Factor: EXPECTING AN ID; CONST; ``NOT'' OR ``(''";
    when 59 =>
      return "MISSING ``RETURN''";
    when 60 =>
      return "CONTROL CHARACTER PRESENT IN SOURCE ";
    when 61 =>
      return "MISSING ``RECORD''";
    when 62 =>
      return "MISSING CLOSING ``IF''";
    when 63 =>
      return "MISSING ``WHEN''";
    when 64 =>
      return "MISSING the finger ``=''>";
    when 65 =>
      return "MISSING CLOSING ``CASE''";
    when 66 =>
      return "CHARACTER DELIMETER USED FOR STRING";
    when 67 =>
      return "Ada RESERVED WORD; NOT SUPPORTED";
    when 68 =>
      return "FUNCTIONS MUST RETURN A VALUE";
    when 69 =>
      return "MUST SPECIFY ``WITH SMALL_SP;''";
    when 70 =>
      return "MUST SPECIFY ``USE SMALL_SP;''";
    when 71 =>
      return "EXPECTING AN ENTRY";
    when 72 =>
      return "MISSING EXPRESSION FOR DELAY";
    when 73 =>
      return "DELAY TIME MUST BE TYPE FLOAT";
    when 74 =>
      return "COMMA EXPECTED";
    when 75 =>
      return "PARAMETER MUST BE OF TYPE ``BOOLEAN''";
    when 76 =>
      return "EXPECTING ``ACCEPT''; ``WHEN''; OR ENTRY ID";
    when 77 =>
      return "EXPECTING Task.Entry";
    when 78 =>
      return "EXPECTING ``OR'' OR ``ELSE'' IN SELECT";
    when 79 =>
      return "EXPECTING ``DELAY''";
    when 80 =>
      return "MISSING SELECT";
    when 81 =>
      return "PROGRAM INCOMPLETE";
    when OF_instead_of_IS =>
      return "found ""of"", should be ""is""";
    when EQUALS_instead_of_BECOMES =>
      return "found ""="", should be "":=""";
    when others =>
      return "Unknown error Id=" & Integer'Image (Id);
    end case;
  end ErrorString;

  ----------------------------------------------------------------------------

  procedure Error (N : Integer) is  -- Write Error on current line & add To
                                    --TOT ERR
  begin
    cFoundError (N, LineCount, syStart, syEnd, -1);
    Errs (N) := True;
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
      Put ("The Compiler TABLE for ");
      case N is
      when IDENTIFIERS_table_overflow =>
        Put ("IDENTIFIERS");
      when PROCEDURES_table_overflow =>
        Put ("PROCEDURES");
      when FLOAT_constants_table_overflow =>
        Put ("FLOAT Constants");
      when 4 =>
        Put ("Arrays");
      when LEVEL_overflow =>
        Put ("LEVELS");
      when OBJECT_overflow =>
        Put ("OBJECT ObjCode");
      when 7 =>
        Put ("Strings");
      when 8 =>
        Put ("TASKS");
      when 9 =>
        Put ("ENTRIES");
      when PATCHING_overflow =>
        Put ("ObjCode PATCHING");
      when others =>
        Put ("N unknown: " & Integer'Image (N));
      end case;
      Put_Line (" is too SMALL");
      New_Line;
      Put_Line (" Please take this output to the maintainers of ");
      Put_Line (" HAC for your installation ");
      New_Line;
      Put_Line (" Fatal termination of HAC");
    end if;
    raise Failure_1_0;
  end Fatal;

  ----------------------------------------------------------------------------

  procedure ErrorMsg is
    use Ada.Text_IO;
    package IIO is new Integer_IO (Integer);
    use IIO;
    K : Integer;
  begin
    K := 0;
    if qDebug then
      New_Line;
      Put_Line (" Error MESSAGE(S)");
    end if;
    if ListingWasRequested then
      New_Line (Listing);
      Put_Line (Listing, " Error MESSAGE(S)");
    end if;
    while Errs /= error_free loop
      while not Errs (K) loop
        K := K + 1;
      end loop;
      if qDebug then
        Put (K, 2);
        Put_Line (":  " & ErrorString (K));
      end if;
      if ListingWasRequested then
        Put (Listing, K, 2);
        Put_Line (Listing, "  " & ErrorString (K));
      end if;
      Errs (K) := False; -- we cancel the K-th sort of error
    end loop;

  end ErrorMsg;

end HAC.UErrors;
