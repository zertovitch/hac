with HAC_Sys.Defs, HAC_Sys.Errors;

with HAT;

with Ada.Text_IO;

package body HAC_Sys.Scanner is

  use Co_Defs, Defs, Errors, HAT;

  type SSTBzz is array (Character'(' ') .. ']') of KeyWSymbol;

  Special_Symbols : constant SSTBzz :=
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

  type CHTP is (Letter, LowCase, Number, Special, Illegal);

  type Set_of_CHTP is array (CHTP) of Boolean;

  special_or_illegal : constant Set_of_CHTP :=
   (Letter   |
    LowCase  |
    Number   => False,
    Special  |
    Illegal  => True);

  c128 : constant Character := Character'Val (128);

  CharacterTypes : constant array (Character) of CHTP :=
       ('A' .. 'Z' => Letter,
        'a' .. 'z' => LowCase,
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

  type AdaKeyW_Pair is record
    st : VString;
    sy : KeyWSymbol;
  end record;

  type AdaKeyW_List is array (Positive range <>) of AdaKeyW_Pair;

  AdaKeyW : constant AdaKeyW_List :=
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

  procedure NextCh (CD : in out Compiler_Data) is  --  Read Next Char; process line end
    procedure c_Get_Next_Line (InpLine : out String; Last : out Natural) is
      idx : Integer := InpLine'First - 1;
      c   : Character;
    begin
      loop
        Character'Read (CD.CUD.compiler_stream, c);
        --  !! NB: if HAC ever happens to consume large input files,
        --         the one-character-at-a-time stream input could become
        --         a performance bottleneck.  --> buffered input (cf Zip-Ada)
        exit when c = ASCII.LF;
        if c /= ASCII.CR then
          idx           := idx + 1;
          InpLine (idx) := c;
        end if;
      end loop;
      Last := idx;
      --  if qDebug then
      --    Put_Line("[::]" & InpLine(InpLine'First..Last));
      --  end if;
    exception
      when Ada.Text_IO.End_Error =>
        if idx < InpLine'First then
          raise;
        end if;
        Last := idx;  --  Avoid trashing a non-empty line ending the stream.
    end c_Get_Next_Line;

    theLine : Source_Line_String;
  begin
    if CD.CUD.CC = CD.CUD.LL then
      if CD.listing_requested then
        New_Line (CD.listing);
      end if;
      CD.CUD.line_count := CD.CUD.line_count + 1;
      if CD.listing_requested then
        HAC_Sys.Defs.IIO.Put (CD.listing, HAC_Integer (CD.CUD.line_count), 4);
        Put (CD.listing, "  ");
        --  Put (Listing, LC, 5);
        --  Put (Listing, "  ");
      end if;
      CD.CUD.LL := 0;
      CD.CUD.CC := 0;
      c_Get_Next_Line (theLine, CD.CUD.LL);
      CD.CUD.input_line (1 .. CD.CUD.LL + 1) := theLine (1 .. CD.CUD.LL) & ' ';
      CD.CUD.LL := CD.CUD.LL + 1;

      if CD.listing_requested then
        New_Line (CD.listing);
        Put_Line (CD.listing, CD.CUD.input_line);
      end if;
    end if;

    CD.CUD.CC := CD.CUD.CC + 1;
    CD.CUD.c := CD.CUD.input_line (CD.CUD.CC);
    --  Manuel : Change tabs for spaces
    if Character'Pos (CD.CUD.c) = 9 then
      CD.CUD.c := ' '; -- IdTab for space
    end if;
    if Character'Pos (CD.CUD.c) < Character'Pos (' ') then
      Error (CD, err_control_character);
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
    begin
      NextCh (CD);
      Sign := 1;
      S    := 0;
      case CD.CUD.c is
        when '+' =>
          NextCh (CD);
        when '-' =>
          if allow_minus then
            NextCh (CD);
            Sign := -1;
          else
            Error (
              CD, err_negative_exponent_for_integer_literal,
              HAC_Integer'Image (CD.INum) & ".0e- ..."
            );
          end if;
        when others =>
          null;
      end case;
      if CD.CUD.c not in '0' .. '9' then
        Error (CD, err_illegal_character_in_number, "; expected digit after 'E'");
      else
        loop
          S := 10 * S + Character'Pos (CD.CUD.c) - Character'Pos ('0');
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
        Error (
          CD, err_number_too_large,
          Integer'Image (K) & " +" &
          Integer'Image (e) & " =" &
          Integer'Image (K + e) & " > Max =" &
          Integer'Image (EMax)
        );
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
      if CD.CUD.c = 'E' or CD.CUD.c = 'e' then
        --  Exponent. Special case because of eventual '+' or '-' which
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
          Error (CD, err_illegal_character_in_number);
      end;
    end Read_with_Sharp;

    procedure Skip_eventual_underscore is
    begin
      if CD.CUD.c = '_' then
        NextCh (CD);
        if CD.CUD.c = '_' then
          Error (CD, err_double_underline_not_permitted, severity => major);
        elsif CharacterTypes (CD.CUD.c) /= Number then
          Error (CD, err_digit_expected, severity => major);
        end if;
      end if;
    end Skip_eventual_underscore;

    procedure Read_Decimal_Float is
    begin
      --  Floating-point number 123.456
      --  Cursor is here -----------^
      if CD.CUD.c = '.' then  --  Double dot.
        CD.CUD.c := c128;
      else
        --  Read decimal part.
        CD.Sy := FloatCon;
        CD.RNum := HAC_Float (CD.INum);
        e := 0;
        while CharacterTypes (CD.CUD.c) = Number loop
          e := e - 1;
          CD.RNum := 10.0 * CD.RNum +
                  HAC_Float (Character'Pos (CD.CUD.c) - Character'Pos ('0'));
          NextCh (CD);
          Skip_eventual_underscore;
        end loop;
        if e = 0 then
          Error (CD, err_illegal_character_in_number, "; expected digit after '.'");
        end if;
        if CD.CUD.c = 'E' or CD.CUD.c = 'e' then
          Read_Scale (True);
        end if;
        if e /= 0 then
          Adjust_Scale;
        end if;
      end if;
    end Read_Decimal_Float;

    procedure Scan_Number (skip_leading_integer : Boolean) is
      use type HAC_Integer;
    begin
      K       := 0;
      CD.INum := 0;
      CD.Sy   := IntCon;
      if skip_leading_integer then
        --  A naughty person has put ".123" in his/her code.
        --  An error is already emmitted at this point but we continue the scanning and parsing.
        Read_Decimal_Float;
      else
        --  Scan the integer part of the number.
        loop
          CD.INum := CD.INum * 10 + (Character'Pos (CD.CUD.c) - Character'Pos ('0'));
          K := K + 1;
          NextCh (CD);
          Skip_eventual_underscore;
          exit when CharacterTypes (CD.CUD.c) /= Number;
        end loop;
        --  Integer part is read (CD.INum).
        case CD.CUD.c is
          when '.' =>
            NextCh (CD);
            if K > KMax then
              Error (
                CD, err_number_too_large,
                Integer'Image (K) & " > Max =" &
                Integer'Image (KMax)
              );
              CD.INum := 0;
              K       := 0;
            end if;
            Read_Decimal_Float;
          when 'E' | 'e' =>
            --  Integer with exponent: 123e4.
            e := 0;
            Read_Scale (False);
            if e /= 0 then
              CD.INum := CD.INum * 10 ** e;
            end if;
          when '#' =>
            Read_with_Sharp;
          when others =>
            null;  --  Number was an integer in base 10.
        end case;
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
        Error (CD, err_character_zero_chars, severity => major);
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
        Error (CD, err_character_zero_chars, severity => major);
      end if;
      --  We peek the next character without moving.
      --  Possible since CD.CC < CD.LL .
      C2 := CD.CUD.input_line (CD.CUD.CC + 1);
      if C1 = ''' and C2 /= ''' then  --  Case (6)
        Error (CD, err_character_zero_chars, severity => major);
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

    exit_big_loop : Boolean;

  begin  --  InSymbol
    CD.prev_sy       := CD.Sy;
    CD.prev_sy_start := CD.syStart;
    CD.prev_sy_end   := CD.syEnd;
    CD.prev_sy_line  := CD.CUD.line_count;

    Big_loop :
    loop
      Small_loop :
      loop
        Skip_Blanks (CD);

        CD.syStart := CD.CUD.CC - 1;
        exit Small_loop when CharacterTypes (CD.CUD.c) /= Illegal;
        Error (CD, err_illegal_character);
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
            if K < max_identifier_length then
              K := K + 1;
              HAT.VStr_Pkg.Append (CD.Id_with_case, CD.CUD.c);
              if K > 1 and then HAT.VStr_Pkg.Slice (CD.Id_with_case, K - 1, K) = "__" then
                Error
                  (CD, err_double_underline_not_permitted, severity => major);
              end if;
            else
              Error (CD, err_identifier_too_long);
            end if;
            NextCh (CD);
            exit when CD.CUD.c /= '_'
                     and then special_or_illegal (CharacterTypes (CD.CUD.c));
          end loop;
          if K > 0 and then Element (CD.Id_with_case, K) = '_' then
            Error
              (CD, err_identifier_cannot_end_with_underline, severity => major);
          end if;
          --
          HAT.VStr_Pkg.Set_Unbounded_String
            (CD.Id, HAT.ACH.To_Upper (To_String (CD.Id_with_case)));
          --
          I := 1;
          J := AdaKeyW'Last;  --  Binary Search
          loop
            K := (I + J) / 2;
            if CD.Id <= AdaKeyW (K).st then
              J := K - 1;
            end if;
            if CD.Id >= AdaKeyW (K).st then
              I := K + 1;
            end if;
            exit when I > J;
          end loop;
          --
          CD.Sy := (if I - 1 > J then AdaKeyW (K).sy else IDent);

        when '0' .. '9' =>
          Scan_Number (skip_leading_integer => False);

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
              Error (CD, err_syntax_error, ": numeric literal cannot start with point", minor);
              Scan_Number (skip_leading_integer => True);
            when others =>
              CD.Sy := Period;
          end case;

        when c128 =>  --  Hathorn
          CD.Sy := Range_Double_Dot_Symbol;
          NextCh (CD);

        when '"' =>
          K := 0;
          loop
            NextCh (CD);
            if CD.CUD.c = '"' then
              NextCh (CD);
              if CD.CUD.c /= '"' then  --  The ""x case
                exit;
              end if;
            end if;
            if CD.Strings_Table_Top + K = SMax then
              Fatal (STRING_CONSTANTS);
            end if;
            CD.Strings_Constants_Table (CD.Strings_Table_Top + K) := CD.CUD.c;
            K := K + 1;
            if CD.CUD.CC = 1 then
              K := 0;  --  END OF InpLine
              CD.syStart := 1;
              CD.syEnd   := 1;
              Error (
                CD,
                err_syntax_error,
                ": missing closing quote on previous line ",
                major
              );
            else
              null;  --  Continue
            end if;
          end loop;
          CD.Sy    := StrCon;
          CD.INum  := HAC_Integer (CD.Strings_Table_Top);
          CD.SLeng := K;
          CD.Strings_Table_Top := CD.Strings_Table_Top + K;
          --  TBD: we could compress this information by searching already existing strings
          --       in the table! (Quick search as for Lempel-Ziv string matchers - cf LZ77
          --       package in the Zip-Ada project.

        when ''' =>
          Scan_Apostrophe_or_Character;

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
          Error (CD, err_illegal_character);
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

    CD.syEnd := CD.CUD.CC - 1;

    if CD.comp_dump_requested then
      Put_Line (CD.comp_dump, CD.CUD.input_line (1 .. CD.CUD.LL));
      for i in 1 .. CD.CUD.CC - 2 loop
        Put (CD.comp_dump, '.');
      end loop;
      Put_Line (CD.comp_dump, "^");
      Put (CD.comp_dump,
        '[' & Integer'Image (CD.CUD.line_count) & ':' &
              Integer'Image (CD.CUD.CC) & ":] " &
        KeyWSymbol'Image (CD.Sy)
      );
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
