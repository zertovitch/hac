with HAC_Sys.Defs,
     HAC_Sys.Errors;

with HAT;

with Ada.IO_Exceptions,
     Ada.Streams;

package body HAC_Sys.Scanner is

  use Co_Defs, Defs, Errors, HAT;

  type Special_Symbol_Mapping is array (Character'(' ') .. ']') of Symbol;

  Special_Symbols : constant Special_Symbol_Mapping :=
   ('+'    => Plus,
    '-'    => Minus,
    '*'    => Times,
    '/'    => Divide,
    '('    => LParent,
    ')'    => RParent,
    '['    => LBrack,
    ']'    => RBrack,
    ','    => Comma,
    ';'    => Semicolon,
    '&'    => Ampersand_Symbol,
    others => NULL_Symbol);

  type Character_Category is (Letter, Number, Special, Illegal);

  type Set_of_Character_Category is array (Character_Category) of Boolean;

  special_or_illegal : constant Set_of_Character_Category :=
   (Letter  |  Number  => False,
    Special | Illegal  => True);

  c128 : constant Character := Character'Val (128);

  Character_Types : constant array (Character) of Character_Category :=
       ('A' .. 'Z' | 'a' .. 'z' => Letter,
        '0' .. '9' => Number,
        '#' |
        '+' | '-' | '*' | '/' |
        '(' | ')' |
        '[' | ']' |
        '&' |
        '=' |
        ' ' |
        ',' |
        '.' |
        ''' |
        ':' |
        '_' |
        ';' |
        '|' |
        '<' |
        '>' |
        '"' => Special,
        c128 => Special,
        others => Illegal);

  type Ada_Keyword_Mapping_Pair is record
    st : VString;
    sy : Symbol;
  end record;

  type Ada_Keyword_Mapping_List is
    array (Positive range <>) of Ada_Keyword_Mapping_Pair;

  ada_keyword : constant Ada_Keyword_Mapping_List :=
       ((+"ABORT",        ABORT_Symbol),
        (+"ABS",          ABS_Symbol),
        (+"ABSTRACT",     ABSTRACT_Symbol),     -- [added in] Ada 95
        (+"ACCEPT",       ACCEPT_Symbol),
        (+"ACCESS",       ACCESS_Symbol),
        (+"ALIASED",      ALIASED_Symbol),      -- Ada 95
        (+"ALL",          ALL_Symbol),          -- Ada 95
        (+"AND",          AND_Symbol),
        (+"ARRAY",        ARRAY_Symbol),
        (+"AT",           AT_Symbol),
        (+"BEGIN",        BEGIN_Symbol),
        (+"BODY",         BODY_Symbol),
        (+"CASE",         CASE_Symbol),
        (+"CONSTANT",     CONSTANT_Symbol),
        (+"DECLARE",      DECLARE_Symbol),
        (+"DELAY",        DELAY_Symbol),
        (+"DELTA",        DELTA_Symbol),
        (+"DIGITS",       DIGITS_Symbol),
        (+"DO",           DO_Symbol),
        (+"ELSE",         ELSE_Symbol),
        (+"ELSIF",        ELSIF_Symbol),
        (+"END",          END_Symbol),
        (+"ENTRY",        ENTRY_Symbol),
        (+"EXCEPTION",    EXCEPTION_Symbol),
        (+"EXIT",         EXIT_Symbol),
        (+"FOR",          FOR_Symbol),
        (+"FUNCTION",     FUNCTION_Symbol),
        (+"GENERIC",      GENERIC_Symbol),
        (+"GOTO",         GOTO_Symbol),
        (+"IF",           IF_Symbol),
        (+"IN",           IN_Symbol),
        (+"INTERFACE",    INTERFACE_Symbol),    -- Ada 2005
        (+"IS",           IS_Symbol),
        (+"LIMITED",      LIMITED_Symbol),
        (+"LOOP",         LOOP_Symbol),
        (+"MOD",          MOD_Symbol),
        (+"NEW",          NEW_Symbol),
        (+"NOT",          NOT_Symbol),
        (+"NULL",         NULL_Symbol),
        (+"OF",           OF_Symbol),
        (+"OR",           OR_Symbol),
        (+"OTHERS",       OTHERS_Symbol),
        (+"OUT",          OUT_Symbol),
        (+"OVERRIDING",   OVERRIDING_Symbol),   -- Ada 2005
        (+"PACKAGE",      PACKAGE_Symbol),
        (+"PARALLEL",     PARALLEL_Symbol),     -- Ada 2022
        (+"PRAGMA",       PRAGMA_Symbol),
        (+"PRIVATE",      PRIVATE_Symbol),
        (+"PROCEDURE",    PROCEDURE_Symbol),
        (+"PROTECTED",    PROTECTED_Symbol),    -- Ada 95
        (+"RAISE",        RAISE_Symbol),
        (+"RANGE",        RANGE_Keyword_Symbol),
        (+"RECORD",       RECORD_Symbol),
        (+"REM",          REM_Symbol),
        (+"RENAMES",      RENAMES_Symbol),
        (+"REQUEUE",      REQUEUE_Symbol),      -- Ada 95
        (+"RETURN",       RETURN_Symbol),
        (+"REVERSE",      REVERSE_Symbol),
        (+"SELECT",       SELECT_Symbol),
        (+"SEPARATE",     SEPARATE_Symbol),
        (+"SOME",         SOME_Symbol),         -- Ada 2012
        (+"SUBTYPE",      SUBTYPE_Symbol),
        (+"SYNCHRONIZED", SYNCHRONIZED_Symbol), -- Ada 2005
        (+"TAGGED",       TAGGED_Symbol),       -- Ada 95
        (+"TASK",         TASK_Symbol),
        (+"TERMINATE",    TERMINATE_Symbol),
        (+"THEN",         THEN_Symbol),
        (+"TYPE",         TYPE_Symbol),
        (+"UNTIL",        UNTIL_Symbol),        -- Ada 95
        (+"USE",          USE_Symbol),
        (+"WHEN",         WHEN_Symbol),
        (+"WHILE",        WHILE_Symbol),
        (+"WITH",         WITH_Symbol),
        (+"XOR",          XOR_Symbol)
       );

  subtype Size_test_a is String (1 .. 19);
  subtype Size_test_b is Ada.Streams.Stream_Element_Array (1 .. 19);
  se_equivalent_to_character : constant Boolean :=
    Size_test_a'Size = Size_test_b'Size and
    Size_test_a'Alignment = Size_test_b'Alignment;

  procedure NextCh (CD : in out Compiler_Data) is  --  Read Next Char; process line end

    procedure Get_Next_Line is
      idx : Integer := CD.CUD.input_line'First - 1;
      c   : Character;

      procedure Get_Character_from_Buffer with Inline is

        actually_read : Natural;

        procedure Refill_Buffer is
          use Ada.Streams;
          se_buffer : Stream_Element_Array (1 .. Source_Buffer_String'Length);
          for se_buffer'Address use CD.CUD.buffer'Address;
          pragma Import (Ada, se_buffer);
          last_read : Stream_Element_Offset;
        begin
          if se_equivalent_to_character then
            --  Fast method:
            CD.CUD.compiler_stream.Read (se_buffer, last_read);
            actually_read := Natural (last_read);
          else
            --  Slow method:
            actually_read := 0;
            for cb of CD.CUD.buffer loop
              Character'Read (CD.CUD.compiler_stream, cb);
              actually_read := actually_read + 1;
            end loop;
          end if;
        exception
          when Ada.IO_Exceptions.End_Error =>
            --  Happens with the slow method.
            --  `actually_read` has been incremented correctly.
            null;
        end Refill_Buffer;

      begin
        if CD.CUD.buffer_position > CD.CUD.buffer_length then
          Refill_Buffer;
          if actually_read = 0 then
            --  Refill failed, even a partial one -> the
            --  stream is exhausted.
            raise Ada.IO_Exceptions.End_Error;
          end if;
          CD.CUD.buffer_length := actually_read;
          CD.CUD.buffer_position := 1;
        end if;
        c := CD.CUD.buffer (CD.CUD.buffer_position);
        CD.CUD.buffer_position := CD.CUD.buffer_position + 1;
      end Get_Character_from_Buffer;

    begin
      loop
        Get_Character_from_Buffer;
        --  Fast version of: Character'Read (CD.CUD.compiler_stream, c);
        exit when c = ASCII.LF;
        if c /= ASCII.CR then
          idx := idx + 1;
          CD.CUD.input_line (idx) := c;
        end if;
      end loop;
      CD.CUD.LL := idx;
    exception
      when Ada.IO_Exceptions.End_Error =>
        if idx < CD.CUD.input_line'First then
          --  `idx` was not changed after its initialization.
          raise;
        end if;
        CD.CUD.LL := idx;  --  Avoid trashing a non-empty line ending the stream.
    end Get_Next_Line;

  begin
    if CD.CUD.CC = CD.CUD.LL then
      if CD.listing_requested then
        New_Line (CD.listing);
      end if;
      CD.CUD.location.line := CD.CUD.location.line + 1;
      if CD.listing_requested then
        HAC_Sys.Defs.IIO.Put (CD.listing, HAC_Integer (CD.CUD.location.line), 4);
        Put (CD.listing, "  ");
      end if;
      CD.CUD.LL := 0;
      CD.CUD.CC := 0;
      Get_Next_Line;
      --  Append a space:
      CD.CUD.LL := CD.CUD.LL + 1;
      CD.CUD.input_line (CD.CUD.LL) := ' ';

      if CD.listing_requested then
        New_Line (CD.listing);
        Put_Line (CD.listing, CD.CUD.input_line);
      end if;
    end if;

    CD.CUD.CC     := CD.CUD.CC + 1;
    CD.CUD.prev_c := CD.CUD.c;
    CD.CUD.c      := CD.CUD.input_line (CD.CUD.CC);
    --  Change tabs for spaces:
    if Character'Pos (CD.CUD.c) = 9 then
      CD.CUD.c := ' ';
    end if;
    if CD.CUD.c < ' ' then
      Error (CD, err_scanner_control_character);
    end if;
  end NextCh;

  procedure Skip_Blanks (CD : in out Compiler_Data) is
  begin
    while CD.CUD.c = ' ' loop
      NextCh (CD);
    end loop;
  end Skip_Blanks;

  procedure InSymbol (CD : in out Compiler_Data) is
    I, J, K, e : Integer;

    procedure Read_Scale (allow_minus : Boolean) is
      S, Sign : Integer;
      digit_count : Natural := 0;
    begin
      NextCh (CD);
      Sign := 1;
      S    := 0;
      case CD.CUD.c is
        when '+' =>
          NextCh (CD);
        when '-' =>
          NextCh (CD);
          if allow_minus then
            Sign := -1;
          else
            Error
              (CD, err_scanner_negative_exponent_for_integer_literal,
               CD.INum'Image & ".0e- ...",
               severity => minor);  --  minor -> scanning & parsing go on unhindered.
          end if;
        when others =>
          null;
      end case;
      if CD.CUD.c not in '0' .. '9' then
        Error
          (CD,
           err_scanner_illegal_character_in_number,
           "; expected digit after 'E'");
      else
        loop
          if digit_count = integer_digits_max then
            Error
              (CD,
               err_scanner_integer_literal_too_large,
               severity => minor);  --  minor -> scanning & parsing go on unhindered.
          elsif digit_count > integer_digits_max then
            null;  --  The insult was already issued on digit_count = integer_digits_max...
          else
            S := S * 10 + Character'Pos (CD.CUD.c) - Character'Pos ('0');
          end if;
          digit_count := digit_count + 1;
          NextCh (CD);
          exit when CD.CUD.c not in '0' .. '9';
        end loop;
      end if;
      e := S * Sign + e;
    end Read_Scale;

    procedure Adjust_Scale is
      S    : Integer;
      D, T : HAC_Float;
    begin
      if K + e > EMax then
        Error
          (CD, err_scanner_exponent_too_large,
           Integer'Image (K) & " +" &
           Integer'Image (e) & " =" &
           Integer'Image (K + e) & " > Max =" &
           Integer'Image (EMax),
           severity => minor);  --  minor -> scanning & parsing go on unhindered.
      elsif K + e < EMin then
        CD.RNum := 0.0;
      else
        S := abs e;
        T := 1.0;
        D := 10.0;
        loop
          while S rem 2 = 0 loop
            S := S / 2;
            D := D ** 2;
          end loop;
          S := S - 1;
          T := D * T;
          exit when S = 0;
        end loop;
        CD.RNum := (if e >= 0 then CD.RNum * T else CD.RNum / T);
      end if;
    end Adjust_Scale;

    procedure Read_with_Sharp is
      --  For numbers in bases other than 10, we fall back to Ada
      --  library's parsing, at the price of a less detailed
      --  error diagnostic.
      s : Source_Line_String;
      l : Natural := s'First - 1;
      has_point : Boolean := False;
    begin
      --  Number has been read until the first '#'.
      loop
        NextCh (CD);
        l := l + 1;
        s (l) := CD.CUD.c;
        exit when CD.CUD.c = '#';  --  Second '#'.
        has_point := has_point or CD.CUD.c = '.';
      end loop;
      NextCh (CD);
      if CD.CUD.c in 'E' | 'e' then
        --  Exponent. Special case because of possible '+' or '-' which
        --  are not operators (e.g. 8#123#e+5 vs. 8#123#+5, = 8#123# + 5)...
        --  Otherwise we could have done it all in the previous loop.
        for c in 1 .. 2 loop
          l := l + 1;
          s (l) := CD.CUD.c;  --  We concatenate "e+", "e-", "e5".
          NextCh (CD);
        end loop;
        while CD.CUD.c in '0' .. '9' loop
          l := l + 1;
          s (l) := CD.CUD.c;  --  We concatenate the rest of the exponent.
          NextCh (CD);
        end loop;
      end if;
      declare
        complete_string : constant String :=
          HAC_Integer'Image (CD.INum) & '#' & s (s'First .. l);
      begin
        if has_point then
          CD.Sy   := FloatCon;
          CD.RNum := HAC_Float'Value (complete_string);
        else
          CD.Sy   := IntCon;
          CD.INum := HAC_Integer'Value (complete_string);
        end if;
      exception
        when others =>
          Error (CD, err_scanner_illegal_character_in_number);
      end;
    end Read_with_Sharp;

    procedure Skip_possible_underscore is
    begin
      if CD.CUD.c = '_' then
        NextCh (CD);
        if CD.CUD.c = '_' then
          Error
            (CD,
             err_scanner_double_underline_not_permitted,
             severity => major);
        elsif Character_Types (CD.CUD.c) /= Number then
          Error (CD, err_scanner_digit_expected, severity => major);
        end if;
      end if;
    end Skip_possible_underscore;

    procedure Read_Decimal_Float is
    begin
      --  Floating-point number 123.456
      --  Cursor is here -----------^
      if CD.CUD.c = '.' then
        --  After all, this is not a number with a decimal point,
        --  but a double dot, like 123..456.
        CD.CUD.c := c128;
        return;
      end if;
      --  Read decimal part.
      CD.Sy := FloatCon;
      CD.RNum := HAC_Float (CD.INum);
      e := 0;
      while Character_Types (CD.CUD.c) = Number loop
        e := e - 1;
        CD.RNum :=
          10.0 * CD.RNum +
            HAC_Float (Character'Pos (CD.CUD.c) - Character'Pos ('0'));
        NextCh (CD);
        Skip_possible_underscore;
      end loop;
      if e = 0 then
        Error
          (CD,
           err_scanner_illegal_character_in_number,
           "; expected digit after '.'");
      end if;
      if CD.CUD.c in 'E' | 'e' then
        Read_Scale (allow_minus => True);
      end if;
      if e /= 0 then
        Adjust_Scale;
      end if;
    end Read_Decimal_Float;

    procedure Scan_Number (skip_leading_integer : Boolean) is
      use type HAC_Integer;
    begin
      K       := 0;
      CD.INum := 0;
      CD.Sy   := IntCon;
      if skip_leading_integer then
        --  Example: a naughty person has put ".123" in his/her code.
        --  An error is already emmitted at this point but we continue
        --  the scanning and parsing.
        Read_Decimal_Float;
      else
        --  Scan the integer part of the number.
        loop
          if K = integer_digits_max then
            --  To do: read a multiprecision integer here, so the limitation
            --         is not applied to floating-point numbers.
            --         Challenge: convert accurately the multiprecision integer
            --         to a floating-point number.
            Error
              (CD,
               err_scanner_integer_literal_too_large,
               severity => minor);  --  minor -> scanning & parsing go on unhindered.
          elsif K > integer_digits_max then
            null;  --  The insult was already issued on K = integer_digits_max...
          else
            CD.INum := CD.INum * 10 + (Character'Pos (CD.CUD.c) - Character'Pos ('0'));
          end if;
          K := K + 1;
          NextCh (CD);
          Skip_possible_underscore;
          exit when Character_Types (CD.CUD.c) /= Number;
        end loop;
        --  Integer part is read (CD.INum).
        case CD.CUD.c is
          when '.' =>
            NextCh (CD);
            Read_Decimal_Float;
          when 'E' | 'e' =>
            --  Integer with exponent: 123e4.
            e := 0;
            Read_Scale (allow_minus => False);
            --  NB: a negative exponent issues an error, then e is set to 0.
            if e > 0 then
              if K + e > integer_digits_max then
                Error
                  (CD, err_scanner_exponent_too_large,
                   Integer'Image (K) & " +" &
                   Integer'Image (e) & " =" &
                   Integer'Image (K + e) & " > Max =" &
                   integer_digits_max'Image,
                   severity => minor);  --  minor -> scanning & parsing go on unhindered.
              else
                CD.INum := CD.INum * 10 ** e;
              end if;
            end if;
          when '#' =>
            Read_with_Sharp;
          when others =>
            null;  --  Number was an integer in base 10.
        end case;
      end if;
      if Character_Types (CD.CUD.c) = Letter then
        CD.CUD.location.column_start := CD.CUD.CC;
        CD.CUD.location.column_stop  := CD.CUD.CC;
        Error
          (CD,
           err_scanner_space_missing_after_number,
           severity => minor);  --  scanning & parsing go on unhindered.
      end if;
    end Scan_Number;

    procedure Scan_Apostrophe_or_Character is
      C1, C2 : Character;
    begin
      --  We scan a little bit further: 2 characters. Possible legal cases:
      --      *  1 'c'      : character
      --      *  2 '''      : character
      --      *  3 'Image   : attribute (hope that no-one invents a 1-letter attribute)
      --      *  4 '(...)   : qualified expression
      --      *  5 '(       : (end of a line after last non-blank) start of a qualified expression
      --  NB: all legal cases but the last have two characters
      --      on the same line after the first '
      --  Blatantly illegal cases:
      --      *  6 ''x      where x is not an '
      --      *  7 ''       (end of a line after last non-blank)
      --      *  8 'c       (end of a line after last non-blank)  c being neither '' nor (
      --      *  9 '        (end of a line after last non-blank)
      --
      if CD.CUD.CC = CD.CUD.LL then  --  Case (9) above
        Error (CD, err_scanner_character_zero_chars, severity => major);
      end if;
      NextCh (CD);
      C1 := CD.CUD.c;
      if CD.CUD.CC = CD.CUD.LL then  --  Cases (5), (7), (8)
        if C1 = '(' then
          --  Case (5)
          CD.Sy := Apostrophe;
          return;
        end if;
        --  Case (7), (8)
        Error (CD, err_scanner_character_zero_chars, severity => major);
      end if;
      --  We peek the next character without moving.
      --  Possible since CD.CC < CD.LL .
      C2 := CD.CUD.input_line (CD.CUD.CC + 1);
      if C1 = ''' and C2 /= ''' then  --  Case (6)
        Error (CD, err_scanner_character_zero_chars, severity => major);
      end if;
      --  Until now, case (5) to (9) are treated.
      if C2 = ''' then  --  Cases (1), (2)
        CD.Sy := CharCon;
        CD.INum := Character'Pos (C1);
        NextCh (CD);
        NextCh (CD);
      else  --  Cases (3), (4)
        CD.Sy := Apostrophe;
      end if;
    end Scan_Apostrophe_or_Character;

    procedure Scan_String_Literal is
      ST : String renames CD.Strings_Constants_Table;
      lit_len : Integer;

      procedure Try_String_Folding is
        trace_folding : constant Boolean := False;
        first_char : constant Character := ST (CD.Strings_Table_Top + 1);
        len : Positive;
      begin
        pragma Assert (lit_len > 0);
        for past_start in
          Integer'Max
            (Strings_Constants_Table_Type'First,
             CD.Strings_Table_Top - string_folding_scan_limit)
          ..
          CD.Strings_Table_Top
        loop
          len := Integer'Min (lit_len, CD.Strings_Table_Top - past_start + 1);
          --  NB: from the upper bound of the loop, we know that
          --  CD.Strings_Table_Top >= past_start.
          --  We also have assumed above that lit_len > 0.
          --  Then, len is always > 0 (the substring to match is
          --  at least one character long).
          if ST (past_start) = first_char
            and then
              ST (past_start .. past_start + len - 1) =
              ST (CD.Strings_Table_Top + 1 .. CD.Strings_Table_Top + len)
          then
            CD.INum  := HAC_Integer (past_start);
            if trace_folding then
              Put ("Matched... [" & ST (past_start .. past_start + len - 1) & "] ");
              if len = lit_len then
                Put_Line ("Full match");
                --  Entire new string was found in the existing string table.
                --  CD.Strings_Table_Top won't be changed.
              else
                pragma Assert (len < lit_len);
                Put_Line
                  ("Partial match; rest is: [" &
                   ST (CD.Strings_Table_Top + len + 1 .. CD.Strings_Table_Top + lit_len) &
                   ']');
                 --  A partial match makes sense only from the first characters
                 --  rightwards and if the right limit is on CD.Strings_Table_Top.
                 --  Then the table can be completed to the full string.
              end if;
            end if;
            --  Partial match: append rest to last string in the table.
            for i in 1 .. lit_len - len loop
              ST (CD.Strings_Table_Top + i) :=      --  Extension.
              ST (CD.Strings_Table_Top + i + len);  --  Rest of the newly scanned string
            end loop;
            --  We will extend the known part of the table only for `lit_len - len` characters.
            --  In any case `len` is the "compression" gain.
            lit_len := lit_len - len;
            exit;
          end if;
        end loop;
      end Try_String_Folding;
    begin
      lit_len := 0;
      loop
        NextCh (CD);
        if CD.CUD.c = '"' then
          NextCh (CD);
          if CD.CUD.c /= '"' then  --  The ""x case
            exit;
          end if;
        end if;
        lit_len := lit_len + 1;
        if CD.Strings_Table_Top + lit_len = SMax then
          Fatal (STRING_CONSTANTS);
        end if;
        ST (CD.Strings_Table_Top + lit_len) := CD.CUD.c;
        if CD.CUD.CC = 1 then
          lit_len := 0;  --  END OF InpLine
          CD.CUD.location.column_start := 1;
          CD.CUD.location.column_stop  := 1;
          Error
            (CD,
             err_general_error,
             "missing closing quote on previous line ",
             severity => major);
        else
          null;  --  Continue
        end if;
      end loop;
      CD.Sy    := StrCon;
      CD.SLeng := lit_len;
      CD.INum  := HAC_Integer (CD.Strings_Table_Top + 1);
      --
      if lit_len > 0 then
        if CD.target.Null_Terminated_String_Literals then
          lit_len := lit_len + 1;
          if CD.Strings_Table_Top + lit_len = SMax then
            Fatal (STRING_CONSTANTS);
          end if;
          ST (CD.Strings_Table_Top + lit_len) := Character'Val (0);
        else
          Try_String_Folding;
        end if;
        CD.Strings_Table_Top := CD.Strings_Table_Top + lit_len;
      end if;
    end Scan_String_Literal;

    exit_big_loop : Boolean;

  begin  --  InSymbol
    CD.prev_sy     := CD.Sy;
    CD.prev_sy_loc := CD.CUD.location;

    Big_loop :
    loop
      Small_loop :
      loop
        Skip_Blanks (CD);

        CD.CUD.location.column_start := CD.CUD.CC;
        exit Small_loop when Character_Types (CD.CUD.c) /= Illegal;
        Error (CD, err_scanner_illegal_character);
        if CD.comp_dump_requested then
          Put_Line
           (CD.comp_dump,
            " Char is => " & Integer'Image (Character'Pos (CD.CUD.c)));
        end if;
        if CD.listing_requested then
          Put_Line
           (CD.listing,
            " Char is => " & Integer'Image (Character'Pos (CD.CUD.c)));
        end if;
        NextCh (CD);
      end loop Small_loop;

      exit_big_loop := True;
      case CD.CUD.c is
        when 'A' .. 'Z' |   --  Identifier or keyword
             'a' .. 'z' =>
          K := 0;
          HAT.VStr_Pkg.Set_Unbounded_String (CD.Id_with_case, "");
          loop
            if K < identifier_length_max then
              K := K + 1;
              HAT.VStr_Pkg.Append (CD.Id_with_case, CD.CUD.c);
              if K > 1 and then (CD.CUD.c = '_' and CD.CUD.prev_c = '_') then
                Error
                  (CD,
                   err_scanner_double_underline_not_permitted,
                   severity => major);
              end if;
            else
              Error (CD, err_scanner_identifier_too_long);
            end if;
            NextCh (CD);
            exit when CD.CUD.c /= '_'
                     and then special_or_illegal (Character_Types (CD.CUD.c));
          end loop;
          if K > 0 and then CD.CUD.prev_c = '_' then
            Error
              (CD,
               err_scanner_identifier_cannot_end_with_underline,
               severity => major);
          end if;
          --
          HAT.VStr_Pkg.Set_Unbounded_String
            (CD.Id, HAT.ACH.To_Upper (To_String (CD.Id_with_case)));
          CD.Id_location := No_Id_Cache;
          --
          --  Binary Search
          --
          I := 1;
          J := ada_keyword'Last;
          loop
            K := (I + J) / 2;
            if CD.Id <= ada_keyword (K).st then
              J := K - 1;
            end if;
            if CD.Id >= ada_keyword (K).st then
              I := K + 1;
            end if;
            exit when I > J;
          end loop;
          --
          CD.Sy := (if I - 1 > J then ada_keyword (K).sy else IDent);

        when '0' .. '9' => Scan_Number (skip_leading_integer => False);
        when '"'        => Scan_String_Literal;
        when '''        => Scan_Apostrophe_or_Character;

        when ':' =>
          NextCh (CD);
          if CD.CUD.c = '=' then
            CD.Sy := Becomes;
            NextCh (CD);
          else
            CD.Sy := Colon;
          end if;

        when '<' =>
          NextCh (CD);
          if CD.CUD.c = '=' then
            CD.Sy := LEQ;
            NextCh (CD);
          else
            CD.Sy := LSS;
          end if;

        when '>' =>
          NextCh (CD);
          if CD.CUD.c = '=' then
            CD.Sy := GEQ;
            NextCh (CD);
          else
            CD.Sy := GTR;
          end if;

        when '/' =>
          NextCh (CD);
          if CD.CUD.c = '=' then
            CD.Sy := NEQ;
            NextCh (CD);
          else
            CD.Sy := Divide;
          end if;

        when '.' =>
          NextCh (CD);
          case CD.CUD.c is
            when '.' =>
              CD.Sy := Range_Double_Dot_Symbol;
              NextCh (CD);
            when '0' .. '9' =>
              Error
                (CD, err_general_error,
                 "numeric literal cannot start with point", severity => minor);
              Scan_Number (skip_leading_integer => True);
            when others =>
              CD.Sy := Period;
          end case;

        when c128 =>  --  Hathorn
          CD.Sy := Range_Double_Dot_Symbol;
          NextCh (CD);

        when '-' =>
          NextCh (CD);
          if CD.CUD.c = '-' then     --  Comment
            CD.CUD.CC := CD.CUD.LL;  --  Ignore rest of input line
            NextCh (CD);
            exit_big_loop := False;
          else
            CD.Sy := Minus;
          end if;

        when '=' =>
          NextCh (CD);
          if CD.CUD.c = '>' then
            CD.Sy := Finger;
            NextCh (CD);
          else
            CD.Sy := EQL;
          end if;

        when '|' =>
          CD.Sy := Alt;
          NextCh (CD);

        when '+' | '*' | '(' | ')' | ',' | '[' | ']' | ';' | '&' =>
          CD.Sy := Special_Symbols (CD.CUD.c);
          NextCh (CD);
          if CD.Sy = Times and then CD.CUD.c = '*' then  --  Get the "**" operator symbol
            CD.Sy := Power;
            NextCh (CD);
          end if;

        when '$' | '!' | '@' | '\' | '^' | '_' | '?' | '%' | '#' =>
          Error (CD, err_scanner_illegal_character);
          if CD.comp_dump_requested then
            Put_Line (CD.comp_dump, " [ $!@\^_?%# ]");
          end if;
          if CD.listing_requested then
            Put_Line (CD.listing,   " [ $!@\^_?%# ]");
          end if;
          NextCh (CD);
          exit_big_loop := False;

        when Character'Val (0) .. ' ' =>
          null;
        when others =>
          null;

      end case;  --  CD.SD.CH
      exit Big_loop when exit_big_loop;
    end loop Big_loop;

    CD.CUD.location.column_stop := CD.CUD.CC - 1;

    if CD.comp_dump_requested then
      Put_Line (CD.comp_dump, CD.CUD.input_line (1 .. CD.CUD.LL));
      Put_Line (CD.comp_dump, (CD.CUD.CC - 2) * '.' & '^');  --  Draw: ".......^"
      Put
        (CD.comp_dump,
         '[' & CD.CUD.location.line'Image & ':' & CD.CUD.CC'Image & ":] " &
         CD.Sy'Image);
      case CD.Sy is
        when IDent =>
          Put (CD.comp_dump, ": " & A2S (CD.Id));
        when IntCon =>
          Put (CD.comp_dump, ": " & HAC_Integer'Image (CD.INum));
        when FloatCon =>
          Put (CD.comp_dump, ": " & HAC_Float'Image (CD.RNum));
        when StrCon =>
          Put (CD.comp_dump, ": """);
          for i in Integer (CD.INum) .. Integer (CD.INum) + CD.SLeng - 1 loop
            Put (CD.comp_dump, CD.Strings_Constants_Table (i));
          end loop;
          Put (CD.comp_dump, '"');
        when Becomes =>
          Put (CD.comp_dump, " := ");
        when Colon =>
          Put (CD.comp_dump, " : ");
        when CONSTANT_Symbol =>
          Put (CD.comp_dump, " constant ");
        when others =>
          null;
      end case;
      New_Line (CD.comp_dump, 2);
    end if;
  end InSymbol;

end HAC_Sys.Scanner;
